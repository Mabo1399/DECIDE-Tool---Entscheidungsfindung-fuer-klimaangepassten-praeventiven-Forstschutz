# -----------------------------------------------------------------------------
# Created by Jörg Fabian Knufinke, assistance and further development by
# Max Bodanowitz. Last edited 09/2026.
# Assistance of generative AI was used in writing these scripts.

# Recommended Citation: 
# Knufinke, J.F. & Bodanowitz, M. (2026) DECIDE Tool - 
# Entscheidungsfindung für klimaangepassten, präventiven Forstschutz. Code to execute the DECIDE Tool (Version Version1.1) 
# [Computer software]. Zenodo. https://doi.org/10.5281/zenodo.22252774
# -----------------------------------------------------------------------------

# Script to         a) classify tree species distribution -> ba_klasse (toggle),
#                   b) select and display relevant actions (Maßnahmen) for a given stand,
#                   c) calculate PAS values and build ziel_html
#                      (only when 'erhöh_widerstand' is among the selected 'ziel').
#
# Merged from 'Schnittstelle_User_Classification_Tree_styria.R' (measures + PAS)
# and 'eval_treespecies.R' (ba_klasse). The previously present but unused sections
# (Master-Tabelle read, user-data reclassification, stand classification) have
# been removed.


require(tidyverse)   # incl. case_when() in the PAS calculation
require(jsonlite)    # JSON input/output

# Use configuration files so data can be read in flexibly
# config file is located in the same directory
CONFIG_PATH <- "insert path to DECIDE-R-CONFIG.json"

# read JSON config and set variables
CONFIG <- fromJSON(CONFIG_PATH)

# path to the directory that holds the TIF files & example data
DATA_DIRECTORY <- CONFIG$DATA_DIRECTORY

args = commandArgs(trailingOnly=TRUE)

# holds parameters for this script (the data from the user polygon)
INPUT_FILE <- args[1]

# holds the script result (various tree variables for the user polygon)
OUTPUT_FILE <- args[2]

# prevent creation of the rplots.pdf file
pdf(NULL)


# Paths for local testing ------------------------------------------------------
#INPUT_FILE <- "insert path to input JSON"
#OUTPUT_FILE <- "insert path to output JSON"


# Load data, check completeness ------------------------------------------------
# user data from JSON

json_data <- fromJSON(INPUT_FILE)

# areaDetailText -> convert back to areaDetail
# Reconstructs the areaDetail data frame from the embedded JSON, so that
# json_data$areaDetail can be used as before.
if (!is.null(json_data$areaDetailText)) {
  .m <- regmatches(
    json_data$areaDetailText,
    regexec(
      '(?s)<script[^>]*id="areaDetail"[^>]*>(.*?)</script>',
      json_data$areaDetailText,
      perl = TRUE
    )
  )[[1]]

  if (length(.m) < 2 || !nzchar(.m[2])) {
    writeLines("{}", OUTPUT_FILE)   # valid (empty) output -> app does not stay stuck loading
    stop("areaDetail konnte nicht aus areaDetailText extrahiert werden.")
  }

  json_data$areaDetail <- jsonlite::fromJSON(.m[2])
  rm(.m)
}

# stop if data is incomplete
if (length(json_data$areaPolygon) == 0 ||
    length(json_data$ziel) == 0 ||
    length(json_data$areaDetail) == 0) {
  writeLines("{}", OUTPUT_FILE)   # valid (empty) output -> app does not stay stuck loading
  stop("Fehlende Eingaben: areaPolygon, ziel oder areaDetail.")
}

# Determine canopy closure
# an already-set user/input value from one of the
# suffixed fields (Di / BH / StH) takes priority. IMPORTANT: the app returns empty fields
# as "" (not NULL/NA), so "" and pure whitespace count here as
# "not set". Only if none of the fields carries a real value is
# canopy closure is derived from the canopy mean in areaDetail.
# below "räumdig" (clearing/gap) the derived value stays empty (NA).
is_blank <- function(x) {
  is.null(x) || length(x) == 0 || all(is.na(x)) ||
    all(!nzchar(trimws(as.character(x))))
}

pick_first_nonblank <- function(...) {
  for (cand in list(...)) {
    if (!is_blank(cand)) return(as.character(cand)[[1]])
  }
  NA_character_
}

json_data$Kronenschlussgrad <- pick_first_nonblank(
  json_data$Kronenschlussgrad_Di,
  json_data$Kronenschlussgrad_BH,
  json_data$Kronenschlussgrad_StH
)

if (is_blank(json_data$Kronenschlussgrad)) {

  canopy_val <- suppressWarnings(as.numeric(
    json_data$areaDetail$mean[json_data$areaDetail$indikatorVariable == "canopy"]
  ))
  canopy_val <- if (length(canopy_val) >= 1) canopy_val[1] else NA_real_

  json_data$Kronenschlussgrad <- case_when(
    is.na(canopy_val) ~ NA_character_,
    canopy_val >= 95  ~ "gedrängt",
    canopy_val >= 86  ~ "geschlossen",
    canopy_val >= 66  ~ "locker",
    canopy_val >= 46  ~ "licht",
    canopy_val >= 30  ~ "räumdig",
    TRUE              ~ NA_character_
  )
}

# Determine tree-species class (ba_klasse) -------------------------------------
# Taken from eval_treespecies.R. Uses json_data$baumartenVerteilung_Ist.
#
# switch for the ba_klasse section. Set to TRUE for now.
# if FALSE, ba_klasse is neither computed nor written to the output.
RUN_BA_KLASSE <- FALSE

# include both spellings (with/without umlaut) so that the script
# works regardless of whether the aliasCodes in the distribution
# have already been renamed to ASCII or still carry the umlaut form.
if (RUN_BA_KLASSE) {
  nadelholz <- c("Fichte", "Kiefer",
                 "Lärche", "Laerche",
                 "Tanne", "Zirbe", "Douglasie")
  
  laubholz  <- c("Buche",
                 "Hänge_Birke", "Haenge_Birke",
                 "Berg_Ahorn", "Rot_Eiche", "Stiel_Eiche", "Trauben_Eiche",
                 "Esche", "Hainbuche", "Vogel_Kirsche",
                 "Sommer_Linde", "Winter_Linde", "Berg_Ulme")
  
  # read the distribution robustly:
  # baumartenVerteilung_Ist may be null or contain species with 0.
  # missing species -> 0. Convert values safely to numeric.
  verteilung <- json_data$baumartenVerteilung_Ist
  
  get_anteil <- function(namen, verteilung) {
    if (is.null(verteilung) || length(verteilung) == 0) return(numeric(0))
    werte <- suppressWarnings(as.numeric(unlist(verteilung[namen], use.names = FALSE)))
    werte <- werte[!is.na(werte)]
    werte
  }
  
  nadel_werte <- get_anteil(nadelholz, verteilung)
  laub_werte  <- get_anteil(laubholz,  verteilung)
  
  max_nadel <- if (length(nadel_werte) > 0) max(nadel_werte) else 0
  max_laub  <- if (length(laub_werte)  > 0) max(laub_werte)  else 0
  
  # classify:
  # rein     : one conifer species >= 70 %
  # nhmisch  : conifer mixture, no species >= 70 %, no broadleaf
  # misch    : conifer/broadleaf mixture, no species >= 70 %
  # NA/null  : no conifer present (broadleaf-dominated/empty -> outside the 3 cases)
  if (max_nadel >= 70) {
    ba_klasse <- "rein"
  } else if (max_nadel > 0) {
    if (max_laub > 0) {
      ba_klasse <- "misch"
    } else {
      ba_klasse <- "nhmisch"
    }
  } else {
    ba_klasse <- NA_character_
  }
}


# Select measures --------------------------------------------------------------

# create vectors
{
  {
    titel <- c(
      "IST+10/Di_0",
      "IST+10/Di_1",
      "IST+10/Di_2",
      "IST+10/Di_1_M",
      "IST+10/Di_1_M-Ki",
      "IST+10/Di_1_M-Fi-Ki",
      "IST+10/Di_1_M-NH",
      "IST+10/Di_2_M",
      "IST+10/Di_2_M-Lä",
      "IST+10/Di_2_M-Fi-Ki",
      "IST+10/Di_2_M-NH",
      "IST+10/StH_0",
      "IST+10/StH_1",
      "IST+10/StH_2",
      "IST+10/StH_2-Ki",
      "IST+10/StH_2-Lä",
      "IST+10/StH_2-Fi-Ki",
      "IST+10/StH_2-Fi-Ki-Lä",
      "IST+10/StH_2-NH",
      "IST+10/StH_3",
      "IST+10/StH_3-Ki",
      "IST+10/StH_3-Lä",
      "IST+10/StH_3-Fi-Ki",
      "IST+10/StH_3-Fi-Ki-Lä",
      "IST+10/StH_3-NH",
      "IST+10/StH_3_L",
      "IST+10/BH_0",
      "IST+10/BH_1",
      "IST+10/BH_2",
      "IST+10/BH_2_L",
      "IST+10/BH_3",
      "IST+10/BH_3-Fi",
      "IST+10/BH_3-Ki",
      "IST+10/BH_3-Lä",
      "IST+10/BH_3-Fi-Ki",
      "IST+10/BH_3-Fi-Ki-Lä",
      "IST+10/BH_3_L",
      "IST+10/BH_3-NH",
      "IST+10/BH_4",
      "IST+10/BH_4-Fi",
      "IST+10/BH_4-Ki",
      "IST+10/BH_4-Lä",
      "IST+10/BH_4-Fi-Ki",
      "IST+10/BH_4-Fi-Ki-Lä",
      "IST+10/BH_4-NH",
      "IST+10/BH_4_L"
    )
    
    aktion <- c(
      "keinerlei Eingriffe",
      "Schwache Stammzahlreduktion und Protzenaushieb",
      "Starke Stammzahlreduktion und Protzenaushieb",
      "Schwache Stammzahlreduktion und Protzenaushieb, Mischungsregulierung",
      "Schwache Stammzahlreduktion und Protzenaushieb, Mischungsregulierung unter Reduktion des Kiefernanteils um 10%",
      "Schwache Stammzahlreduktion und Protzenaushieb, Mischungsregulierung unter Reduktion des Fichten- und Kiefernanteils um 10%",
      "Schwache Stammzahlreduktion und Protzenaushieb, Mischungsregulierung zugunsten von Laubholz",
      "Starke Stammzahlreduktion und Protzenaushieb, Mischungsregulierung",
      "Starke Stammzahlreduktion und Protzenaushieb, Mischungsregulierung unter Reduktion des Lärchenanteils um 10%",
      "Starke Stammzahlreduktion und Protzenaushieb, Mischungsregulierung unter Reduktion des Fichten- und Kiefernanteils um 10%",
      "Starke Stammzahlreduktion und Protzenaushieb, Mischungsregulierung zugunsten von Laubholz",
      "keinerlei Eingriffe",
      "Niederdurchforstung",
      "Schwache Auslesedurchforstung",
      "Schwache Auslesedurchforstung und Reduktion des Kiefernanteils um 10%",
      "Schwache Auslesedurchforstung und Reduktion des Lärchenanteils um 10%",
      "Schwache Auslesedurchforstung und Reduktion des Fichten- und Kiefernanteils um 10%",
      "Schwache Auslesedurchforstung und Reduktion des Kiefern- und/oder Fichten- und/oder Lärchenanteils um 10%",
      "Schwache Auslesedurchforstung und Reduktion des Nadelholzanteils um 10%",
      "Starke Auslesedurchforstung",
      "Starke Auslesedurchforstung und Reduktion des Kiefernanteils um 10%",
      "Starke Auslesedurchforstung und Reduktion des Lärchenanteils um 10%",
      "Starke Auslesedurchforstung und Reduktion des Fichten- und Kiefernanteils um 10%",
      "Starke Auslesedurchforstung und Reduktion des Kiefern- und/oder Fichten- und/oder Lärchenanteils um 10%",
      "Starke Auslesedurchforstung und Reduktion des Nadelholzanteils um 10%",
      "Starke Auslesedurchforstung und Schaffen von Lücken",
      "keinerlei Eingriffe",
      "Niederdurchforstung",
      "Schwache Auslesedurchforstung",
      "Schwache Auslesedurchforstung und Schaffen von Lücken",
      "Starke Auslesedurchforstung",
      "Starke Auslesedurchforstung und Reduktion des Fichtenanteils um 10%",
      "Starke Auslesedurchforstung und Reduktion des Kiefernanteils um 10%",
      "Starke Auslesedurchforstung und Reduktion des Lärchenanteils um 10%",
      "Starke Auslesedurchforstung und Reduktion des Fichten- und Kiefernanteils um 10%",
      "Starke Auslesedurchforstung und Reduktion des Kiefern- und/oder Fichten- und/oder Lärchenanteils um 10%",
      "Starke Auslesedurchforstung und Schaffen von Lücken",
      "Starke Auslesedurchforstung und Reduktion des Nadelholzanteils um 10%",
      "Sehr starke Auslesedurchforstung",
      "Sehr starke Auslesedurchforstung und Reduktion des Fichtenanteils um 10%",
      "Sehr starke Auslesedurchforstung und Reduktion des Kiefernanteils um 10%",
      "Sehr starke Auslesedurchforstung und Reduktion des Lärchenanteils um 10%",
      "Sehr starke Auslesedurchforstung und Reduktion des Fichten- und Kiefernanteils um 10%",
      "Sehr starke Auslesedurchforstung und Reduktion des Kiefern- und/oder Fichten- und/oder Lärchenanteils um 10%",
      "Sehr starke Auslesedurchforstung und Reduktion des Nadelholzanteils um 10%",
      "Sehr starke Auslesedurchforstung und Schaffen von Lücken"
    )
    
    beschreibung <- c(
      "keinerlei Eingriffe",
      "Entnahme von kranken, geschädigten oder unförmig und v.a. ausladend gewachsenen Individuen (Protzen, Zwiesel). Der Kronenschluss soll nach dem Eingriff locker sein. Press- und Füllholz wird belassen. Die Überschirmung wird auf 70-80% herabgesetzt.",
      "Entnahme von kranken, geschädigten oder unförmig und v.a. ausladend gewachsenen Individuen (Protzen, Zwiesel). Der Kronenschluss soll nach dem Eingriff locker bis licht sein. Press- und Füllholz wird belassen. Die Überschirmung wird auf 60-70% herabgesetzt.",
      "Schaffen einer Dickung mit gewünschter BA-Mischung in bestmöglicher Qualität. Gruppen von einzelnen Baumarten sollen die Schirmfläche eines adulten Baumes der Art einnehmen. Der Kronenschluss soll nach dem Eingriff locker sein. Aus den Gruppen der einzelnen Baumarten werden v.a. geschädigte Individuen und Protzen entnommen. Press- und Füllholz wird belassen. Die Überschirmung wird auf 70-80% herabgesetzt.",
      "Schaffen einer Dickung mit gewünschter BA-Mischung in bestmöglicher Qualität unter Reduktion des Kiefernanteils um 10%. Gruppen von einzelnen Baumarten sollen die Schirmfläche eines adulten Baumes der Art einnehmen. Der Kronenschluss soll nach dem Eingriff locker sein. Aus den Gruppen der einzelnen Baumarten werden v.a. geschädigte Individuen und Protzen entnommen. Press- und Füllholz wird belassen. Die Überschirmung wird auf 70-80% herabgesetzt.",
      "Schaffen einer Dickung mit gewünschter BA-Mischung in bestmöglicher Qualität unter Reduktion des Fichten- und Kiefernanteils um 10%. Gruppen von einzelnen Baumarten sollen die Schirmfläche eines adulten Baumes der Art einnehmen. Der Kronenschluss soll nach dem Eingriff locker sein. Aus den Gruppen der einzelnen Baumarten werden v.a. geschädigte Individuen und Protzen entnommen. Press- und Füllholz wird belassen. Die Überschirmung wird auf 70-80% herabgesetzt.",
      "Schaffen einer Dickung mit gewünschter BA-Mischung in bestmöglicher Qualität unter Reduktion des Nadelholzanteils um 10%. Gruppen von einzelnen Baumarten sollen die Schirmfläche eines adulten Baumes der Art einnehmen. Der Kronenschluss soll nach dem Eingriff locker sein. Aus den Gruppen der einzelnen Baumarten werden v.a. geschädigte Individuen und Protzen entnommen. Press- und Füllholz wird belassen. Die Überschirmung wird auf 70-80% herabgesetzt.",
      "Schaffen einer Dickung mit gewünschter BA-Mischung in bestmöglicher Qualität. Die einzelnen Baumarten sollen die Schirmfläche eines adulten Baumes der Art einnehmen. Der Kronenschluss soll nach dem Eingriff locker bis licht sein; die Abstände zwischen den Gruppen der einzelnen Baumarten sollen größer sein als zwischen den Individuen innerhalb der Gruppen. Press- und Füllholz wird belassen. Die Überschirmung wird auf 60-70% herabgesetzt.",
      "Schaffen einer Dickung mit gewünschter BA-Mischung in bestmöglicher Qualität unter Reduktion des Lärchenanteils um 10%. Die einzelnen Baumarten sollen die Schirmfläche eines adulten Baumes der Art einnehmen. Der Kronenschluss soll nach dem Eingriff locker bis licht sein – die Kronen sollen so viel Abstand haben, dass stellenweise eine weitere Krone dazwischen passt; die Abstände zwischen den Gruppen der einzelnen Baumarten sollen größer sein als zwischen den Individuen innerhalb der Gruppen. Press- und Füllholz wird belassen. Die Überschirmung wird auf 60-70% herabgesetzt.",
      "Schaffen einer Dickung mit gewünschter BA-Mischung in bestmöglicher Qualität unter Reduktion des Fichten- und Kiefernanteils um 10%. Die einzelnen Baumarten sollen die Schirmfläche eines adulten Baumes der Art einnehmen. Der Kronenschluss soll nach dem Eingriff locker bis licht sein – die Kronen sollen so viel Abstand haben, dass stellenweise eine weitere Krone dazwischen passt; die Abstände zwischen den Gruppen der einzelnen Baumarten sollen größer sein als zwischen den Individuen innerhalb der Gruppen. Press- und Füllholz wird belassen. Die Überschirmung wird auf 60-70% herabgesetzt.",
      "Schaffen einer Dickung mit gewünschter BA-Mischung in bestmöglicher Qualität unter Reduktion des Nadelholzanteils um 10%. Die einzelnen Baumarten sollen die Schirmfläche eines adulten Baumes der Art einnehmen. Der Kronenschluss soll nach dem Eingriff locker bis licht sein – die Kronen sollen so viel Abstand haben, dass stellenweise eine weitere Krone dazwischen passt; die Abstände zwischen den Gruppen der einzelnen Baumarten sollen größer sein als zwischen den Individuen innerhalb der Gruppen. Press- und Füllholz wird belassen. Die Überschirmung wird auf 60-70% herabgesetzt.",
      "keinerlei Eingriffe",
      "Entnahme von kranken, instabilen, besonders schlanken oder geschädigten Individuen der Kraft'schen Baumklassen 3 bis 5b (mitherrschend, beherrscht, unterständig), und von unförmig und ausladend gewachsenen Individuen. Je nach Baumart sind entsprechende Abstände zwischen den Individuen zu wählen.",
      "Z-Baumauslese und -Förderung der Z-Baumkandidaten: Entnahme von 1 – 2 Bedrängern; Anlage von Rückegassen; keine Eingriffe im Nebenbestand. (Annahme: Die Überschirmung wird dadurch auf ca. 85% herabgesetzt.)",
      "Z-Baumauslese und -Förderung der Z-Baumkandidaten: Entnahme von 1 - 2 Bedrängern; Anlage von Rückegassen; Reduktion des Kiefernanteils um 10%, ansonsten keine Eingriffe im Nebenbestand. (Annahme: Die Überschirmung wird dadurch auf ca. 85% herabgesetzt.)",
      "Z-Baumauslese und -Förderung der Z-Baumkandidaten: Entnahme von 1 - 2 Bedrängern; Anlage von Rückegassen; Reduktion des Lärchenanteils um 10%, ansonsten keine Eingriffe im Nebenbestand. (Annahme: Die Überschirmung wird dadurch auf ca. 85% herabgesetzt.)",
      "Z-Baumauslese und -Förderung der Z-Baumkandidaten: Entnahme von 1 - 2 Bedrängern; Anlage von Rückegassen; Reduktion des Fichten- und Kiefernanteils um 10%, ansonsten keine Eingriffe im Nebenbestand. (Annahme: Die Überschirmung wird dadurch auf 85% herabgesetzt.)",
      "Z-Baumauslese und -Förderung der Z-Baumkandidaten: Entnahme von 1 - 2 Bedrängern; Anlage von Rückegassen; Reduktion des Kiefern- und/oder Fichten- und/oder Lärchenanteils um 10%, ansonsten keine Eingriffe im Nebenbestand. (Annahme: Die Überschirmung wird dadurch auf ca. 85% herabgesetzt.)",
      "Z-Baumauslese und -Förderung der Z-Baumkandidaten: Entnahme von 1 - 2 Bedrängern; Anlage von Rückegassen; Reduktion des Nadelholzanteils um 10%, ansonsten keine Eingriffe im Nebenbestand. (Annahme: Die Überschirmung wird dadurch auf ca. 85% herabgesetzt.)",
      "Z-Baumauslese und -Förderung der Z-Baumkandidaten: Entnahme von 2 - 3 Bedrängern; Anlage von Rückegassen; Förderung der Mischbaumarten. (Annahme: Die Überschirmung wird dadurch auf ca. 70% herabgesetzt.)",
      "Z-Baumauslese und -Förderung der Z-Baumkandidaten: Entnahme von 2-3 Bedrängern; Reduktion des Kiefernanteils um 10%; Anlage von Rückegassen; Förderung der Mischbaumarten. (Annahme: Die Überschirmung wird dadurch auf ca. 70% herabgesetzt.)",
      "Z-Baumauslese und -Förderung der Z-Baumkandidaten: Entnahme von 2-3 Bedrängern; Reduktion des Lärchenanteils um 10%; Anlage von Rückegassen; Förderung der Mischbaumarten. (Annahme: Die Überschirmung wird dadurch auf ca. 70% herabgesetzt.)",
      "Z-Baumauslese und -Förderung der Z-Baumkandidaten: Entnahme von 2-3 Bedrängern; Reduktion des Fichten- und Kiefernanteils um 10%; Anlage von Rückegassen; Förderung der Mischbaumarten. (Annahme: Die Überschirmung wird dadurch auf ca. 70% herabgesetzt.)",
      "Z-Baumauslese und -Förderung der Z-Baumkandidaten: Entnahme von 2 - 3 Bedrängern; Reduktion des Kiefern- und/oder Fichten- und/oder Lärchenanteils um 10%; Anlage von Rückegassen; Förderung der Mischbaumarten. (Annahme: Die Überschirmung wird dadurch auf ca. 70% herabgesetzt.)",
      "Z-Baumauslese und -Förderung der Z-Baumkandidaten: Entnahme von 2-3 Bedrängern; Reduktion des Nadelholzanteils um 10%; Anlage von Rückegassen; Förderung der Mischbaumarten. (Annahme: Die Überschirmung wird dadurch auf ca. 70% herabgesetzt.)",
      "Z-Baumauslese und -Förderung der Z-Baumkandidaten: Entnahme von 2-3 Bedrängern; Anlage von Rückegassen; Förderung der Mischbaumarten; Schaffung von Lücken >5m Durchmesser und 100m². (Annahme: Die Überschirmung wird dadurch ohne Lücken auf ca. 70% herabgesetzt.)",
      "keinerlei Eingriffe",
      "Entnahme von kranken, instabilen, besonders schlanken oder geschädigten Individuen der Kraft'schen Baumklassen 3 bis 5b (mitherrschend, beherrscht, unterständig).",
      "Förderung der Z-Baumkandidaten: Entnahme von 1 – 2 Bedrängern; Anlage von Rückegassen; keine Eingriffe im Nebenbestand. (Annahme: Die Überschirmung wird dadurch auf ≥ 90% herabgesetzt.)",
      "Förderung der Z-Baumkandidaten: Entnahme von 1 – 2 Bedrängern pro Z-Baumkandidat; Anlage von Rückegassen und Schaffen von Lücken >5m Durchmesser und 100m². (Annahme: Die Überschirmung wird dadurch ohne Lücken auf 90% herabgesetzt.)",
      "Förderung der Z-Baumkandidaten: Entnahme von 2 – 3 Bedrängern; Anlage von Rückegassen; Förderung der Mischbaumarten. (Annahme: Die Überschirmung wird dadurch auf ≥ 80% herabgesetzt.)",
      "Förderung der Z-Baumkandidaten durch Entnahme von 2 – 3 Bedrängern; Anlage von Rückegassen; Förderung der Mischbaumarten; Reduktion des Fichtenanteils um 10%. (Annahme: Die Überschirmung wird dadurch auf ≥ 80% herabgesetzt.)",
      "Förderung der Z-Baumkandidaten durch Entnahme von 2 – 3 Bedrängern; Förderung der Mischbaumarten; Reduktion des Kiefernanteils um 10% in der Oberschicht; Anlage von Rückegassen. (Annahme: Die Überschirmung wird dadurch auf ≥ 80% herabgesetzt.)",
      "Förderung der Z-Baumkandidaten durch Entnahme von 2 – 3 Bedrängern; Anlage von Rückegassen; Förderung der Mischbaumarten; Reduktion des Lärchenanteils um 10%. (Annahme: Die Überschirmung wird dadurch auf ≥ 80% herabgesetzt.)",
      "Förderung der Z-Baumkandidaten durch Entnahme von 2 – 3 Bedrängern; Anlage von Rückegassen; Förderung der Mischbaumarten; Reduktion des Fichten- und Kiefernanteils um 10% (Annahme: Die Überschirmung wird dadurch auf ≥ 80% herabgesetzt.)",
      "Förderung der Z-Baumkandidaten durch Entnahme von 2 – 3 Bedrängern; Förderung der Mischbaumarten; Reduktion des Kiefern- und/oder Fichten- und/oder Lärchenanteils um 10% in der Oberschicht; Anlage von Rückegassen. (Annahme: Die Überschirmung wird dadurch auf ≥ 80% herabgesetzt.)",
      "Förderung der Z-Baumkandidaten: Entnahme von 2 – 3 Bedrängern pro Z-Baumkandidat; Anlage von Rückegassen und Schaffung von Lücken >5m Durchmesser und 100m². (Annahme: Die Überschirmung wird dadurch ohne Lücken auf 80% herabgesetzt.)",
      "Förderung der Z-Baumkandidaten durch Entnahme von 2 – 3 Bedrängern; Anlage von Rückegassen; Förderung der Mischbaumarten; Reduktion des Nadelholzanteils um 10%. (Annahme: Die Überschirmung wird dadurch auf ≥ 80% herabgesetzt.)",
      "Förderung der Z-Baumkandidaten durch Entnahme von 2 – 3 Bedrängern; Anlage von Rückegassen; Förderung der Mischbaumarten. (Annahme: Die Überschirmung wird dadurch auf 70% herabgesetzt.)",
      "Förderung der Z-Baumkandidaten durch Entnahme von 2 – 3 Bedrängern; Anlage von Rückegassen; Förderung der Mischbaumarten; Reduktion des Fichtenanteils um 10%. (Annahme: Die Überschirmung wird dadurch auf 70% herabgesetzt.)",
      "Förderung der Z-Baumkandidaten durch Entnahme von 2 – 3 Bedrängern; Anlage von Rückegassen; Förderung der Mischbaumarten; Reduktion des Kiefernanteils um 10%. (Annahme: Die Überschirmung wird dadurch auf 70% herabgesetzt.)",
      "Förderung der Z-Baumkandidaten durch Entnahme von 2 – 3 Bedrängern; Anlage von Rückegassen; Förderung der Mischbaumarten; Reduktion des Lärchenanteils um 10%. (Annahme: Die Überschirmung wird dadurch auf 70% herabgesetzt.)",
      "Förderung der Z-Baumkandidaten durch Entnahme von 2 – 3 Bedrängern; Anlage von Rückegassen; Förderung der Mischbaumarten; Reduktion des Fichten- und Kiefernanteils um 10%. (Annahme: Die Überschirmung wird dadurch auf 70% herabgesetzt.)",
      "Förderung der Z-Baumkandidaten durch Entnahme von 2 – 3 Bedrängern; Anlage von Rückegassen; Förderung der Mischbaumarten; Reduktion des Kiefern- und/oder Fichten- und/oder Lärchenanteils um 10%. (Annahme: Die Überschirmung wird dadurch auf 70% herabgesetzt.)",
      "Förderung der Z-Baumkandidaten durch Entnahme von 2 – 3 Bedrängern; Anlage von Rückegassen; Förderung der Mischbaumarten; Reduktion des Nadelholzanteils um 10%. (Annahme: Die Überschirmung wird dadurch auf 70% herabgesetzt.)",
      "Förderung der Z-Baumkandidaten durch Entnahme von 2 – 3 Bedrängern; Anlage von Rückegassen; Förderung der Mischbaumarten; Schaffung von Lücken >5m Durchmesser und 100m². (Annahme: Die Überschirmung wird dadurch ohne Lücken auf 70% herabgesetzt.)"
    )
    
    stopifnot(
      length(titel) == length(aktion),
      length(titel) == length(beschreibung)
    )
    
  }
  
  # data frame with all texts
  massnahmen_df <- data.frame(
    original = titel,
    cleaned = gsub("IST\\+10/", "", titel),
    explanation = aktion,
    long_explanation = beschreibung,
    stringsAsFactors = FALSE
  )
  
  # Filter ----------------------------------------------------------------
  
  ## robustly filter measures
  
  ## we work on a copy
  massnahmen_df_refined <- massnahmen_df
  
  
  ## 1) Helper functions
  
  ## robustly retrieve tree-species value:
  ## - if species is missing -> 0
  ## - if value is NA / empty / non-numeric -> 0
  get_species_value <- function(x, species_name) {
    if (is.null(x) || !(species_name %in% names(x))) {
      return(0)
    }
    
    value <- suppressWarnings(as.numeric(x[[species_name]]))
    
    if (length(value) == 0 || is.na(value)) {
      return(0)
    }
    
    value
  }
  
  ## Removes all measures that contain a given token.
  ## examples:
  ## token = "Fi" -> removes -Fi, -Fi-Ki, -Fi-Ki-Lä
  ## token = "NH" -> removes -NH
  ## token = "L"  -> removes _L
  remove_if_contains_token <- function(df, token) {
    if (nrow(df) == 0) {
      return(df)
    }
    
    pattern <- paste0("(^|[-_])", token, "($|[-_])")
    df[!grepl(pattern, df$cleaned), , drop = FALSE]
  }
  
  ## Removes all measures that contain ALL given tokens.
  ## examples:
  ## c("Fi", "Ki") -> removes -Fi-Ki and -Fi-Ki-Lä
  ## c("Fi", "Ki", "Lä") -> removes -Fi-Ki-Lä
  remove_if_contains_all_tokens <- function(df, tokens) {
    if (length(tokens) == 0 || nrow(df) == 0) {
      return(df)
    }
    
    hit_matrix <- sapply(
      tokens,
      function(tok) grepl(paste0("(^|[-_])", tok, "($|[-_])"), df$cleaned)
    )
    
    if (is.null(dim(hit_matrix))) {
      hit_matrix <- matrix(hit_matrix, ncol = 1)
    }
    
    keep <- apply(hit_matrix, 1, function(hits) !all(hits))
    df[keep, , drop = FALSE]
  }
  
  ## Removes measures based on the exact code from cleaned.
  remove_if_code_matches <- function(df, codes) {
    if (length(codes) == 0 || nrow(df) == 0) {
      return(df)
    }
    
    df[!(df$cleaned %in% codes), , drop = FALSE]
  }
  
  
  ## 2) filter forest class
  group_patterns <- c(
    "Dickung" = "^Di_",
    "Stangenholz" = "^StH_",
    "sw Baumholz" = "^BH_"
  )
  
  wuchsklasse_value <- json_data$Wuchsklasse
  selected_pattern <- unname(group_patterns[wuchsklasse_value])
  
  if (is.na(selected_pattern)) {
    stop(sprintf("Unbekannte Wuchsklasse: %s", wuchsklasse_value))
  }
  
  massnahmen_df_refined <- massnahmen_df_refined[
    grepl(selected_pattern, massnahmen_df_refined$cleaned),
    ,
    drop = FALSE
  ]
  
  
  ## 3) robustly prepare tree-species proportions
  baumarten <- json_data$baumartenVerteilung_Ist
  
  prop_fi <- get_species_value(baumarten, "Fichte")
  prop_ki <- get_species_value(baumarten, "Kiefer")
  prop_la <- get_species_value(baumarten, "Lärche")
  prop_ta <- get_species_value(baumarten, "Tanne")
  prop_zi <- get_species_value(baumarten, "Zirbe")
  prop_dg <- get_species_value(baumarten, "Douglasie")
  
  conif_species_sum <- prop_fi + prop_ki + prop_la + prop_ta + prop_zi + prop_dg
  
  sum_fi_ki <- prop_fi + prop_ki
  sum_fi_la <- prop_fi + prop_la
  sum_ki_la <- prop_ki + prop_la
  sum_fi_ki_la <- prop_fi + prop_ki + prop_la
  
  
  ## 4) filter: species proportions / special cases
  
  ## 4a) remove -NH if all conifer species together sum to 100
  if (conif_species_sum == 100) {
    massnahmen_df_refined <- remove_if_contains_token(massnahmen_df_refined, "NH")
  }
  
  ## 4b) remove _L if forest_gaps majority == 1
  forest_gaps_majority <- NA_real_
  
  if (!is.null(json_data$areaDetail) &&
      is.data.frame(json_data$areaDetail) &&
      all(c("indikatorVariable", "majority") %in% names(json_data$areaDetail))) {
    
    forest_gaps_row <- json_data$areaDetail[
      json_data$areaDetail$indikatorVariable == "forest_gaps",
      ,
      drop = FALSE
    ]
    
    if (nrow(forest_gaps_row) > 0) {
      forest_gaps_majority <- suppressWarnings(
        as.numeric(as.character(forest_gaps_row$majority[1]))
      )
    }
  }
  
  if (!is.na(forest_gaps_majority) && forest_gaps_majority == 1) {
    massnahmen_df_refined <- remove_if_contains_token(massnahmen_df_refined, "L")
  }
  
  ## 4c) single species:
  ## if Fi / Ki / Lä equal 0 or 100, remove all measures
  ## that contain this token.
  if (prop_fi %in% c(0, 100)) {
    massnahmen_df_refined <- remove_if_contains_token(massnahmen_df_refined, "Fi")
  }
  
  if (prop_ki %in% c(0, 100)) {
    massnahmen_df_refined <- remove_if_contains_token(massnahmen_df_refined, "Ki")
  }
  
  if (prop_la %in% c(0, 100)) {
    massnahmen_df_refined <- remove_if_contains_token(massnahmen_df_refined, "Lä")
  }
  
  ## 4d) combination Fi_Ki:
  ## if Fi + Ki == 100, remove everything that contains Fi AND Ki.
  ## this removes -Fi-Ki and also -Fi-Ki-Lä.
  if (sum_fi_ki == 100) {
    massnahmen_df_refined <- remove_if_contains_all_tokens(
      massnahmen_df_refined,
      c("Fi", "Ki")
    )
  }
  
  ## 4e) combination Fi_Lä:
  ## in this measure set there is no pure Fi-Lä,
  ## but -Fi-Ki-Lä contains both tokens and must then be removed.
  if (sum_fi_la == 100) {
    massnahmen_df_refined <- remove_if_contains_all_tokens(
      massnahmen_df_refined,
      c("Fi", "Lä")
    )
  }
  
  ## 4f) combination Ki_Lä:
  ## Analogously: effectively removes -Fi-Ki-Lä.
  if (sum_ki_la == 100) {
    massnahmen_df_refined <- remove_if_contains_all_tokens(
      massnahmen_df_refined,
      c("Ki", "Lä")
    )
  }
  
  ## 4g) combination Fi_Ki_Lä:
  ## if Fi + Ki + Lä == 100, remove everything that contains Fi, Ki and Lä.
  if (sum_fi_ki_la == 100) {
    massnahmen_df_refined <- remove_if_contains_all_tokens(
      massnahmen_df_refined,
      c("Fi", "Ki", "Lä")
    )
  }
  
  
  ## 5) existing filter: canopy closure
  remove_codes_map <- list(
    "Dickung" = list(
      "Blöße" = c("Di_1", "Di_2", "Di_1_M", "Di_1_M-Fi-Ki", "Di_1_M-Ki", "Di_1_M-NH", "Di_2_M", "Di_2_M-Fi-Ki", "Di_2_M-Lä", "Di_2_M-NH"),
      "räumdig" = c("Di_1", "Di_2", "Di_1_M", "Di_1_M-Fi-Ki", "Di_1_M-Ki", "Di_1_M-NH", "Di_2_M", "Di_2_M-Fi-Ki", "Di_2_M-Lä", "Di_2_M-NH"),
      "licht" = c("Di_1", "Di_1_M", "Di_1_M-Fi-Ki", "Di_1_M-Ki", "Di_1_M-NH"),
      "locker" = character(0),
      "geschlossen" = character(0),
      "gedrängt" = character(0)
    ),
    "Stangenholz" = list(
      "Blöße" = c("StH_2", "StH_2-Fi-Ki", "StH_2-Fi-Ki-Lä", "StH_2-Ki", "StH_2-Lä", "StH_2-NH", "StH_3", "StH_3-Fi-Ki", "StH_3-Fi-Ki-Lä", "StH_3-Ki", "StH_3_L", "StH_3-Lä", "StH_3-NH"),
      "räumdig" = c("StH_2", "StH_2-Fi-Ki", "StH_2-Fi-Ki-Lä", "StH_2-Ki", "StH_2-Lä", "StH_2-NH", "StH_3", "StH_3-Fi-Ki", "StH_3-Fi-Ki-Lä", "StH_3-Ki", "StH_3_L", "StH_3-Lä", "StH_3-NH"),
      "licht" = c("StH_2", "StH_2-Fi-Ki", "StH_2-Fi-Ki-Lä", "StH_2-Ki", "StH_2-Lä", "StH_2-NH", "StH_3", "StH_3-Fi-Ki", "StH_3-Fi-Ki-Lä", "StH_3-Ki", "StH_3_L", "StH_3-Lä", "StH_3-NH"),
      "locker" = c("StH_2", "StH_2-Fi-Ki", "StH_2-Fi-Ki-Lä", "StH_2-Ki", "StH_2-Lä", "StH_2-NH"),
      "geschlossen" = character(0),
      "gedrängt" = character(0)
    ),
    "sw Baumholz" = list(
      "Blöße" = c("BH_2", "BH_2_L", "BH_3", "BH_3_L", "BH_3-Fi", "BH_3-Fi-Ki", "BH_3-Fi-Ki-Lä", "BH_3-Ki", "BH_3-Lä", "BH_3-NH", "BH_4", "BH_4_L", "BH_4-Fi", "BH_4-Fi-Ki", "BH_4-Fi-Ki-Lä", "BH_4-Ki", "BH_4-Lä", "BH_4-NH"),
      "räumdig" = c("BH_2", "BH_2_L", "BH_3", "BH_3_L", "BH_3-Fi", "BH_3-Fi-Ki", "BH_3-Fi-Ki-Lä", "BH_3-Ki", "BH_3-Lä", "BH_3-NH", "BH_4", "BH_4_L", "BH_4-Fi", "BH_4-Fi-Ki", "BH_4-Fi-Ki-Lä", "BH_4-Ki", "BH_4-Lä", "BH_4-NH"),
      "licht" = c("BH_2", "BH_2_L", "BH_3", "BH_3_L", "BH_3-Fi", "BH_3-Fi-Ki", "BH_3-Fi-Ki-Lä", "BH_3-Ki", "BH_3-Lä", "BH_3-NH", "BH_4", "BH_4_L", "BH_4-Fi", "BH_4-Fi-Ki", "BH_4-Fi-Ki-Lä", "BH_4-Ki", "BH_4-Lä", "BH_4-NH"),
      "locker" = c("BH_2", "BH_2_L", "BH_3", "BH_3_L", "BH_3-Fi", "BH_3-Fi-Ki", "BH_3-Fi-Ki-Lä", "BH_3-Ki", "BH_3-Lä", "BH_3-NH"),
      "geschlossen" = character(0),
      "gedrängt" = character(0)
    )
  )
  
  kronenschluss_value <- json_data$Kronenschlussgrad
  codes_to_remove <- remove_codes_map[[wuchsklasse_value]][[kronenschluss_value]]
  
  if (is.null(codes_to_remove)) {
    stop(sprintf(
      "Keine Filterdefinition für Wuchsklasse '%s' und Kronenschlussgrad '%s' gefunden.",
      wuchsklasse_value,
      kronenschluss_value
    ))
  }
  
  massnahmen_df_refined <- remove_if_code_matches(
    massnahmen_df_refined,
    codes_to_remove
  )
  
  
  ## 6) final filter: objective
  
  ziel_values <- character(0)
  
  if (!is.null(json_data$ziel)) {
    ziel_values <- as.character(unlist(json_data$ziel, use.names = FALSE))
    ziel_values <- trimws(ziel_values)
    ziel_values <- ziel_values[!is.na(ziel_values) & ziel_values != ""]
  }
  
  ## if "erhöh_widerstand" is included:
  ## no additional restriction
  if (!"erhöh_widerstand" %in% ziel_values) {
    
    keep_codes_by_ziel <- list(
      "Sägerundholzqualität" = c(
        "Di_0",
        "Di_1",
        "Di_2",
        "Di_1_M",
        "Di_2_M",
        "StH_0",
        "StH_1",
        "StH_2",
        "StH_3",
        "BH_0",
        "BH_1",
        "BH_2",
        "BH_3"
      ),
      "habverb" = c(
        "Di_0",
        "Di_1",
        "Di_2",
        "Di_1_M",
        "Di_2_M",
        "StH_0",
        "StH_1",
        "StH_2",
        "StH_3",
        "StH_3_L",
        "BH_0",
        "BH_1",
        "BH_2",
        "BH_3",
        "BH_2_L",
        "BH_3_L",
        "BH_4",
        "BH_4_L"
      )
    )
    
    ## only consider known goals
    relevant_ziel_values <- intersect(names(keep_codes_by_ziel), ziel_values)
    
    if (length(relevant_ziel_values) > 0) {
      
      ## always use the largest required set
      ## (= union of all relevant goals)
      allowed_codes <- unique(
        unlist(keep_codes_by_ziel[relevant_ziel_values], use.names = FALSE)
      )
      
      massnahmen_df_refined <- massnahmen_df_refined[
        massnahmen_df_refined$cleaned %in% allowed_codes,
        ,
        drop = FALSE
      ]
    }
  }
  
  # Output: HTML text (list of measures)  -----------------------------------
  
  
  massnahmen_df <- massnahmen_df_refined
  
  # CSS
  css_text <- "<style>
details summary {
  cursor: pointer;
  font-weight: bold;
}
details summary::after {
  content: ' (Beschreibung des Maßnahmenpaketes lesen)';
  font-weight: normal;
  color: #555;
}
details[open] summary::after {
  content: ' (Beschreibung des Maßnahmenpaketes verbergen)';
}
details p {
  margin: 5px 0 0 0;
  color:#bbb; 
  font-size:0.95em; 
  line-height:1.4;
}
</style>"

html_text <- css_text
html_text <- paste0(html_text, "<table style='border-collapse:collapse;'>")

for (i in seq_len(nrow(massnahmen_df))) {
  
  # main row
  html_text <- paste0(
    html_text,
    "<tr style='height:40px;'>",
    "<td style='padding-right:30px; font-weight:bold; vertical-align:top; min-width:180px;'>", 
    massnahmen_df$cleaned[i], ":</td>",
    "<td style='padding-left:20px; vertical-align:top;'>", massnahmen_df$explanation[i], "</td></tr>"
  )
  
  # spacer row
  html_text <- paste0(
    html_text,
    "<tr style='height:10px;'><td colspan='2'></td></tr>"
  )
  
  # only add long explanation if it's not "keinerlei Eingriffe"
  if (massnahmen_df$explanation[i] != "keinerlei Eingriffe") {
    html_text <- paste0(
      html_text,
      "<tr style='height:30px;'>",
      "<td></td>",
      "<td style='padding-left:40px; vertical-align:top;'>",
      "<details>",
      "<summary></summary>",
      "<p>", massnahmen_df$long_explanation[i], "</p>",
      "</details>",
      "</td></tr>"
    )
  } else {
    # keep spacing row even if no details
    html_text <- paste0(
      html_text,
      "<tr style='height:30px;'><td colspan='2'></td></tr>"
    )
  }
  
  # spacer row
  html_text <- paste0(
    html_text,
    "<tr style='height:20px;'><td colspan='2'></td></tr>"
  )
}

html_text <- paste0(html_text, "</table>")

# if only one measure is selected:

if (nrow(massnahmen_df) == 1) {
  html_text <- paste0(
    html_text,
    "<p style='margin-top:20px; color:#b30000; font-weight:bold;'>",
    "Mit den von Ihnen ausgewählten Bestandesparametern ist nur ein Maßnahmenpaket verfügbar. ",
    "Ein Vergleich verschiedener Maßnahmenpakete ist damit nicht möglich. ",
    "Bitte wählen Sie einen anderen Bestand aus.</p>"
  )
}

}

# clean up including the vector itself
objects_to_remove <- c(
  "baumarten", "massnahmen_df", "massnahmen_df_refined", "wuchsklasse_value",
  "titel", "aktion", "beschreibung", "kronenschluss_value",
  "selected_pattern", "codes_to_remove",
  "prop_fi", "prop_ki", "prop_la", "prop_ta", "prop_zi", "prop_dg",
  "conif_species_sum", "sum_fi_ki", "sum_fi_la", "sum_ki_la", "sum_fi_ki_la",
  "forest_gaps_majority", "forest_gaps_row", "canopy_val",
  # helper objects of the tree-species classification (ba_klasse itself is kept)
  "nadelholz", "laubholz", "verteilung", "get_anteil",
  "nadel_werte", "laub_werte", "max_nadel", "max_laub"
)

rm(list = intersect(c(objects_to_remove, "objects_to_remove"), ls()))

# compute current PAS values ---------------------------------------------------
# only needed if 'erhöh_widerstand' was selected as a goal.
# Otherwise neither the PAS values nor ziel_html are computed/output.
#
# ziel_html is (re)computed only when needed.
# The heavy PAS calculation (10 sourced scripts) depends on forest class,
# Kronenschlussgrad and Baumartenverteilung (plus areaDetail). It should
# therefore only run when:
#   1) 'erhöh_widerstand' is a goal,
#   2) all three inputs are present (otherwise the result is based on
#      incomplete input and would be overwritten immediately anyway),
#   3) the input combination has changed relative to the last calculation
#      (fingerprint via <script id=zhsig> in ziel_html).
# If not recomputed, the last computed value is preserved unchanged by the
# app's Object.assign merge (no re-emit needed).
ZIEL_HTML_INITIATOR <- FALSE
# this script is NOT the initiator (forest-class or canopy-closure
# question): the tree-species selection (w_BA) performs the FIRST calculation. If
# an input later, this script still recomputes via the fingerprint.

zh_nz <- function(x) if (is.null(x) || length(x) == 0) "" else as.character(x)[[1]]

compute_ziel_html   <- FALSE
ziel_html_signature <- NA_character_

if ("erhöh_widerstand" %in% json_data$ziel) {

  .zh_ba <- json_data$baumartenVerteilung_Ist
  .zh_ba_present <- !is.null(.zh_ba) && length(.zh_ba) > 0 &&
    sum(suppressWarnings(as.numeric(unlist(.zh_ba))), na.rm = TRUE) > 0
  .zh_all_three <- nzchar(trimws(zh_nz(json_data$Wuchsklasse))) &&
    nzchar(trimws(zh_nz(json_data$Kronenschlussgrad))) && .zh_ba_present

  if (.zh_all_three) {
    # fingerprint of the inputs that determine ziel_html. Instead of the large
    # for areaDetail the polygon is used: it uniquely determines the geodata
    # and is compact.
    .zh_poly <- ""
    if (!is.null(json_data$areaPolygon)) {
      .zh_pdf <- tryCatch(as.data.frame(json_data$areaPolygon), error = function(e) NULL)
      if (!is.null(.zh_pdf) && all(c("lat", "lng") %in% names(.zh_pdf)))
        .zh_poly <- paste0(round(as.numeric(.zh_pdf$lat), 6), ",",
                           round(as.numeric(.zh_pdf$lng), 6), collapse = ";")
    }
    .zh_ba_sig <- paste0(names(unlist(.zh_ba)), "=", unlist(.zh_ba), collapse = ",")
    ziel_html_signature <- paste(zh_nz(json_data$Wuchsklasse),
                                 zh_nz(json_data$Kronenschlussgrad),
                                 .zh_ba_sig, .zh_poly, sep = "||")

    # do NOT read the previous signature from a dedicated field (the app merges only
    # known fields back -> a dedicated ziel_html_sig would be lost),
    # but from the last computed ziel_html, into which the signature was
    # embedded as <script id="zhsig"> (same pattern as areaDetail in
    # areaDetailText -> demonstrably preserved by the app).
    .zh_has_html <- !is.null(json_data$ziel_html) &&
      nzchar(trimws(zh_nz(json_data$ziel_html)))
    .zh_prev_sig <- NA_character_
    if (.zh_has_html) {
      .zh_m <- regmatches(
        zh_nz(json_data$ziel_html),
        regexec('(?s)<script[^>]*id="zhsig"[^>]*>(.*?)</script>',
                zh_nz(json_data$ziel_html), perl = TRUE)
      )[[1]]
      if (length(.zh_m) >= 2 && nzchar(.zh_m[2]))
        .zh_prev_sig <- tryCatch(jsonlite::fromJSON(.zh_m[2]),
                                 error = function(e) NA_character_)
    }

    if (!.zh_has_html) {
      # no ziel_html yet -> first calculation only in the initiator (w_BA)
      compute_ziel_html <- ZIEL_HTML_INITIATOR
    } else if (is.na(.zh_prev_sig)) {
      # ziel_html without a signature (legacy/removed) -> recompute (sets marker)
      compute_ziel_html <- TRUE
    } else {
      # normal: only recompute when the inputs have changed
      compute_ziel_html <- !identical(ziel_html_signature, .zh_prev_sig)
    }
  }
}

if (compute_ziel_html) {
  
  # --- Prepare variables for the PAS calculation ---
  {
    
    # retrieve geodata from JSON
    var_table <- json_data$areaDetail
    
    # convert forest roads from running-meters/ha to running-meters/pixel
    var_table$mean[var_table$indikatorVariable == "roads"] <- as.numeric(var_table$mean[var_table$indikatorVariable == "roads"])/11.11
    
    # canopy closure: text value to numeric value
    Kronenschluss_num <- case_when(
      json_data$Kronenschlussgrad == "gedrängt" ~ 95,
      json_data$Kronenschlussgrad == "geschlossen" ~ 86,
      json_data$Kronenschlussgrad == "locker" ~ 66,
      json_data$Kronenschlussgrad == "licht" ~ 46,
      json_data$Kronenschlussgrad == "räumdig" ~ 30,
      json_data$Kronenschlussgrad == "Blöße" ~ 0,
      TRUE ~ NA_real_
    )
    
    # apply to all relevant variables
    var_table$mean[var_table$indikatorVariable == "canopy"] <- Kronenschluss_num
    var_table$mean[var_table$indikatorVariable == "ips_canopy_cover"] <- Kronenschluss_num
    var_table$mean[var_table$indikatorVariable == "snow_canopy_cover"] <- Kronenschluss_num
    var_table$mean[var_table$indikatorVariable == "storm_canopy_cover"] <- Kronenschluss_num
    
    # forest class: text to category
    forest_class_num <- case_when(
      json_data$Wuchsklasse == "Dickung" ~ 4,
      json_data$Wuchsklasse == "Stangenholz" ~ 5,
      json_data$Wuchsklasse == "sw Baumholz" ~ 6
    )
    
    # apply to the relevant variable
    var_table$majority[var_table$indikatorVariable == "forest_class"] <- forest_class_num
    
    # spruce: numeric value to category
    fichte_val <- if (!is.null(json_data$baumartenVerteilung_Ist$Fichte)) json_data$baumartenVerteilung_Ist$Fichte else NA_real_
    
    fichte_num <- case_when(
      fichte_val >= 90 ~ 4,
      fichte_val >= 61 ~ 3,
      fichte_val >= 40 ~ 2,
      fichte_val >= 11 ~ 1,
      fichte_val >= 0  ~ 0,
      TRUE             ~  0
    )
    
    # apply to the relevant variable
    var_table$majority[var_table$indikatorVariable == "ips_spruce_proportion"] <- fichte_num
    
    # pine: numeric value to category
    kiefer_val <- if (!is.null(json_data$baumartenVerteilung_Ist$Kiefer)) json_data$baumartenVerteilung_Ist$Kiefer else NA_real_
    
    kiefer_num <- case_when(
      is.na(kiefer_val)           ~ -1,
      kiefer_val >= 90            ~ 4,
      kiefer_val >= 60            ~ 3,
      kiefer_val >= 40            ~ 2,
      kiefer_val >= 10            ~ 1,
      kiefer_val >= 1             ~ 0,
      kiefer_val < 1              ~ -1
    )
    
    # apply to the relevant variable
    var_table$majority[var_table$indikatorVariable == "pine"] <- kiefer_num
    
    
    # larch: numeric value to category
    laerche_val <- if (!is.null(json_data$baumartenVerteilung_Ist$Lärche)) json_data$baumartenVerteilung_Ist$Lärche else NA_real_
    
    laerche_num <- case_when(
      is.na(laerche_val)          ~ -1,
      laerche_val >= 90           ~ 4,
      laerche_val >= 60           ~ 3,
      laerche_val >= 40           ~ 2,
      laerche_val >= 10           ~ 1,
      laerche_val >= 1            ~ 0,
      laerche_val < 1             ~ -1
    )
    
    # apply to the relevant variable
    var_table$majority[var_table$indikatorVariable == "larch"] <- laerche_num
    
    # maxproportion
    max_class <- max(c(fichte_num, kiefer_num, laerche_num), na.rm = TRUE)
    
    
    max_class <- if (is.infinite(max_class)) -1 else max_class
    
    var_table$majority[var_table$indikatorVariable == "maxproportion"] <- max_class
    
    
    # conifer
    conif_species <- c("Fichte", "Kiefer", "Lärche", "Tanne", "Zirbe", "Douglasie")
    
    conif_vals <- sapply(conif_species, function(sp) {
      if (!is.null(json_data$baumartenVerteilung_Ist[[sp]])) json_data$baumartenVerteilung_Ist[[sp]] else 0
    })
    
    # sum of all proportions
    conif_sum <- sum(conif_vals, na.rm = TRUE)
    
    # classification
    conif_num <- case_when(
      is.na(conif_sum)        ~ -1,
      conif_sum < 1           ~ -1,
      conif_sum < 10          ~ 0,
      conif_sum < 40          ~ 1,
      conif_sum < 60          ~ 2,
      conif_sum < 90          ~ 3,
      conif_sum <= 100        ~ 4,
      TRUE                    ~ 4 
    )
    
    
    # assign
    var_table$majority[var_table$indikatorVariable == "conif"] <- conif_num
    
    # for Till there is no -1
    snow_conif_num <- ifelse(conif_num == -1, 0, conif_num)
    
    # assign
    var_table$majority[var_table$indikatorVariable == "snow_coniferous_proportion"] <- snow_conif_num
    
    # broadleaf
    broad_species <- c(
      "Berg_Ulme", "Winter_Linde", "Sommer_Linde", "Vogel_Kirsche", 
      "Hainbuche", "Esche", "Trauben_Eiche", "Stiel_Eiche", 
      "Buche", "Hänge_Birke", "Berg_Ahorn", "Rot_Eiche"
    )
    
    broad_vals <- sapply(broad_species, function(sp) {
      if (!is.null(json_data$baumartenVerteilung_Ist[[sp]])) json_data$baumartenVerteilung_Ist[[sp]] else 0
    })
    
    
    broad_sum <- sum(broad_vals, na.rm = TRUE)
    
    broad_num <- case_when(
      is.na(broad_sum)        ~ -1,
      broad_sum < 1           ~ -1,
      broad_sum < 10          ~ 0,
      broad_sum < 40          ~ 1,
      broad_sum < 60          ~ 2,
      broad_sum < 90          ~ 3,
      broad_sum <= 100        ~ 4,
      TRUE                    ~ 4
    )
    
    
    var_table$majority[var_table$indikatorVariable == "broad"] <- broad_num
    
  }
  
  # everything as a value in the environment (mean or majority, as applicable)
  for (i in seq_len(nrow(var_table))) {
    var_name <- var_table$indikatorVariable[i]
    var_value <- var_table$mean[i]
    if (is.na(var_value)) {
      var_value <- var_table$majority[i]
    }
    assign(var_name, var_value, envir = .GlobalEnv)
  }
  
  ziel_html <- NULL  
  
  # run the PAS scripts and overwrite values
  pas_var_map <- list(
    list(script = "pas_1_browsing_backend_SERVER.R", map = c(
      PAS_1_browsing = "res_browsing_combined", PAS_1_browsing_site = "res_browsing_site",
      PAS_1_browsing_stand = "res_browsing_stand", PAS_1_browsing_win = "res_browsing_combined_win",
      PAS_1_browsing_win_site = "res_browsing_site_win")),
    list(script = "pas_2_barkstripping_backend_SERVER.R", map = c(
      PAS_2_barkstripping = "res_barkstripping_combined", PAS_2_barkstripping_site = "res_barkstripping_site",
      PAS_2_barkstripping_stand = "res_barkstripping_stand", pas_stripping = "res_barkstripping_combined",
      stripping_site = "res_barkstripping_site")),
    list(script = "pas_3_cembrae_backend_SERVER.R", map = c(
      PAS_3_cembrae = "res_cembrae_combined", PAS_3_cembrae_site = "res_cembrae_site",
      PAS_3_cembrae_stand = "res_cembrae_stand")),
    list(script = "pas_4_armillaria_backend_SERVER.R", map = c(
      PAS_4_armillaria = "res_armillaria_combined", PAS_4_armillaria_site = "res_armillaria_site",
      PAS_4_armillaria_stand = "res_armillaria_stand")),
    list(script = "pas_5_barkbreeding_backend_SERVER.R", map = c(
      PAS_5_barkbreed = "res_barkbreeding_combined", PAS_5_barkbreed_site = "res_barkbreeding_site",
      PAS_5_barkbreed_stand = "res_barkbreeding_stand")),
    list(script = "pas_6_heterobasidion_backend_SERVER.R", map = c(
      PAS_6_heterobasidion = "res_heterobasidion_combined", PAS_6_heterobasidion_site = "res_heterobasidion_site",
      PAS_6_heterobasidion_stand = "res_heterobasidion_stand")),
    list(script = "pas_7_fire_backend_SERVER.R", map = c(
      PAS_7_fire = "res_fire_combined", PAS_7_fire_site = "res_fire_site",
      PAS_7_fire_stand = "res_fire_stand", pas_fire_stand = "res_fire_stand")),
    list(script = "pas_8_storm_backend_SERVER.R", map = c(
      PAS_8_storm = "res_storm_combined", PAS_8_storm_site = "res_storm_site",
      PAS_8_storm_stand = "res_storm_stand")),
    list(script = "pas_9_snow_backend_SERVER.R", map = c(
      PAS_9_snow = "res_snow_combined", PAS_9_snow_site = "res_snow_site",
      PAS_9_snow_stand = "res_snow_stand", pas_snow_stand = "res_snow_stand")),
    list(script = "pas_10_barkbeetle_backend_SERVER.R", map = c(
      PAS_10_ips = "res_ips_combined", PAS_10_ips_site = "res_ips_site",
      PAS_10_ips_stand = "res_ips_stand"))
  )
  for (pas in pas_var_map) {
    source(paste0("insert path to PAS scripts directory/", pas$script))
    for (ind in names(pas$map)) {
      var_table$mean[var_table$indikatorVariable == ind] <- get(pas$map[[ind]])
    }
  }
  rm(pas_var_map, pas, ind)

  # assemble HTML output:
  
  stoerfaktor_labels <- c(
    PAS_1_browsing = "Verbiss",
    PAS_2_barkstripping = "Schäle",
    PAS_3_cembrae = "Lärchenborkenkäfer",
    PAS_4_armillaria = "Hallimasch",
    PAS_5_barkbreed = "Kiefernrindenbrüter",
    PAS_6_heterobasidion = "Wurzelschwamm",
    PAS_7_fire = "Feuer",
    PAS_8_storm = "Sturm",
    PAS_9_snow = "Schnee",
    PAS_10_ips = "Buchdrucker"
  )
  
  base_codes <- names(stoerfaktor_labels)
  
  has_highlight <- FALSE
  elevated_labels <- c()
  rows <- c()
  
  format_val <- function(val, highlight) {
    if (is.na(val) || val == "") return("nicht verfügbar")
    
    val <- suppressWarnings(as.numeric(val))
    if (is.na(val)) return("nicht verfügbar")
    
    val <- max(0.0, val)  
    val_fmt <- sprintf("%.2f", val)
    
    if (highlight) {
      paste0("<span style='color:#f3e6f9; background-color:#3d0066; padding:2px 4px; border-radius:3px;'>",
             val_fmt, "</span>")
    } else {
      val_fmt
    }
  }
  
  for (code in base_codes) {
    var_comb <- code
    var_stand <- paste0(code, "_stand")
    var_site <- paste0(code, "_site")
    
    val_comb <- var_table$mean[var_table$indikatorVariable == var_comb]
    val_stand <- var_table$mean[var_table$indikatorVariable == var_stand]
    val_site <- var_table$mean[var_table$indikatorVariable == var_site]
    
    val_comb <- ifelse(length(val_comb) == 0, NA, val_comb)
    val_stand <- ifelse(length(val_stand) == 0, NA, val_stand)
    val_site <- ifelse(length(val_site) == 0, NA, val_site)
    
    highlight_comb <- !is.na(val_comb) && val_comb > 0.4
    highlight_stand <- !is.na(val_stand) && val_stand > 0.4
    
    if (highlight_comb || highlight_stand) {
      has_highlight <- TRUE
      elevated_labels <- c(elevated_labels, paste0("<em>", stoerfaktor_labels[code], "</em>"))
    }
    
    
    label_html <- if (highlight_comb || highlight_stand) {
      paste0(
        "<span style='color:#f3e6f9; background-color:#3d0066; padding:2px 4px; border-radius:3px;'>",
        stoerfaktor_labels[code], "</span>"
      )
    } else {
      stoerfaktor_labels[code]
    }
    
    
    row <- paste0(
      "<tr>",
      "<td style='padding:8px 12px; text-align:left;'>", label_html, "</td>",
      "<td style='padding:8px 12px; text-align:left;'>", format_val(val_stand, highlight_stand), "</td>",
      "<td style='padding:8px 12px; text-align:left;'>", format_val(val_site, FALSE), "</td>",
      "<td style='padding:8px 12px; text-align:left;'>", format_val(val_comb, highlight_comb), "</td>",
      "</tr>"
    )
    
    rows <- c(rows, row)
  }
  
  
  ziel_html <- paste0(
    "<div style='font-size: smaller;'>",
    "<table style='border-collapse: collapse;'>",
    "<thead><tr>",
    "<th style='padding:8px 12px; border-bottom: 1px solid #ccc; text-align:left;'>Störfaktor</th>",
    "<th style='padding:8px 12px; border-bottom: 1px solid #ccc; text-align:left;'>Bestandsprädisposition</th>",
    "<th style='padding:8px 12px; border-bottom: 1px solid #ccc; text-align:left;'>Standortprädisposition</th>",
    "<th style='padding:8px 12px; border-bottom: 1px solid #ccc; text-align:left;'>kombinierte Prädisposition</th>",
    "</tr></thead>",
    "<tbody>", paste(rows, collapse = "\n"), "</tbody>",
    "</table>"
  )
  
  
  if (has_highlight) {
    elevated_text <- paste(elevated_labels, collapse = ", ")
    ziel_html <- paste0(
      ziel_html,
      "<p style='margin-top: 20px;'>Die Prädispositionen für die Störfaktoren ", elevated_text,
      " sind erhöht (&gt; 0.4).</p>"
    )
  } else {
    ziel_html <- paste0(
      ziel_html,
      "<p style='font-weight:bold; background-color:#d4edda; color:#155724; padding:10px; border:1px solid #c3e6cb; border-radius:4px; margin-top: 20px;'>
      Herzlichen Glückwunsch! Kein Störfaktor weist eine erhöhte kombinierte oder Bestandsprädisposition (&gt; 0.4) auf. 
      Eine Anwendung des Ziels <em>Erhöhung der Widerstandsfähigkeit</em> ist auf 
      einen Bestand mit so niedrigen Ausgangswerten für die Prädispositionen nicht vorgesehen. 
      Bitte wählen Sie ein anderes Ziel oder einen anderen Bestand aus.
    </p>"
    )
  }
  
  ziel_html <- paste0(ziel_html, "</div>")
  
}


# Export results ---------------------------------------------------------------

# only output the objects that were actually computed.
# ba_klasse and ziel_html are only included if they were computed
# (switch RUN_BA_KLASSE or goal 'erhöh_widerstand').
# 'ba_klasse' is NOT a question alias -> is merged by the app via Object.assign into the
# condition context is merged and is then available in displayConditions as a top-level
# variable available: ba_klasse == 'rein'
# set the name of the measures table depending on the forest class:
# if no (valid) forest class is present, the default name 'Maßnahmen_Tabelle' remains.
massnahmen_key <- "Maßnahmen_Tabelle"
if (!is.null(json_data$Wuchsklasse) && length(json_data$Wuchsklasse) > 0 &&
    !is.na(json_data$Wuchsklasse) && nzchar(json_data$Wuchsklasse)) {
  massnahmen_key <- switch(
    as.character(json_data$Wuchsklasse),
    "Dickung"     = "Maßnahmen_Tabelle_Di",
    "Stangenholz" = "Maßnahmen_Tabelle_StH",
    "sw Baumholz" = "Maßnahmen_Tabelle_BH"
  )
}


# Builds the HTML string 'usereingaben' from json_data. Included below in result_list
# included so that the user-input summary is updated at EVERY step. Missing fields are omitted automatically.
# -> use config-section to set labels (ue_labels etc.),
#    and then apply identically in ALL scripts.

# Prefill (only if not already set), base-R safe (no %||%):
# derive canopy closure from the suffixed field, if not already set
# (in w_KS e.g. it is derived from canopy -> do NOT overwrite here).
ue_is_blank <- function(x) is.null(x) || length(x) == 0 || all(is.na(x)) ||
  all(!nzchar(trimws(as.character(x))))
if (ue_is_blank(json_data[["Kronenschlussgrad"]])) {
  for (.cand in list(json_data[["Kronenschlussgrad_Di"]],
                     json_data[["Kronenschlussgrad_BH"]],
                     json_data[["Kronenschlussgrad_StH"]])) {
    if (!ue_is_blank(.cand)) { json_data[["Kronenschlussgrad"]] <- as.character(.cand)[[1]]; break }
  }
}
# forest class: take from local variable for display if in the JSON
# (still) empty (affects mainly User_Communication, where it is freshly derived).
if (ue_is_blank(json_data[["Wuchsklasse"]]) && exists("Wuchsklasse"))
  json_data[["Wuchsklasse"]] <- Wuchsklasse

# usereingaben: HTML summary of the user inputs --------------------------------
# Builds a formatted HTML string from json_data (variable 'usereingaben').
# usable in every script (after json_data <- fromJSON(...)), so that the field
# is updated at every step.
#
# non-existing fields are omitted automatically (no error): if a
# value is missing (not collected / not required), simply no row appears.
#
# 1) DISPLAY NAMES per field (optionally different per forest class)
# one vector per field:
#   default       = name normally displayed
#   "Dickung"     = alternative name ONLY for Dickung        (optional)
#   "Stangenholz" = alternative name ONLY for Stangenholz    (optional)
#   "sw Baumholz" = alternative name ONLY for sw Baumholz    (optional)
ue_labels <- list(
  Bestandesname       = c(default = "Bestandesname"),
  Ziele               = c(default = "Ziele"),
  Wuchsklasse         = c(default = "Wuchsklasse"),
  Kronenschlussgrad   = c(default = "Kronenschlussgrad"),
  Baumartenverteilung = c(default = "Baumartenverteilung"),

  Kronenlänge = c(default       = "Kronenlänge",
                  "Dickung"     = "",                          # does not exist for Dickung
                  "sw Baumholz" = "Kronenlänge und HD-Wert"),

  Astfreiheit = c(default = "Astfreiheit"),

  Schäden = c(default       = "Schäden",
              "Dickung"     = "Schäden und Wuchs",
              "Stangenholz" = "Schäden und Wuchs"),

  Groberschließung    = c(default = "Groberschließung"),
  Feinerschließung    = c(default = "Feinerschließung"),
  Stabilitätsträger   = c(default = "Stabilitätsträger")
)

# 2) VALUES -> DISPLAY TEXT (same for all forest classes)
# ALWAYS quote the keys (because of 0.3 / 0.7 / 0.9).
ue_label_schaeden <- c("0.3" = "gering", "0.7" = "mittel", "0.9" = "hoch")
ue_label_krone    <- c("0.3" = "kurz",   "0.7" = "mittel", "0.9" = "lang")
ue_label_ast      <- c("0" = "nicht astfrei", "1" = "astfrei")
ue_label_flag     <- c("0" = "nicht vorhanden", "1" = "vorhanden")           # coarse/fine/selection trees

# goals: internal value -> display text (unknown goals are shown unchanged)
ue_label_ziele <- c(
  "Sägerundholzqualität" = "Sägerundholzqualität",
  "erhöh_widerstand"     = "Erhöhung der Widerstandsfähigkeit",
"habverb" = "Habitatverbesserung für das Auerhuhn"
  # ... add further goals here
)

# 3) display ORDER
# = order of the ue_add(...) calls in the section "collect rows" further
# below. To reorder, simply move the calls.


## current forest class
ue_wk <- if (is.null(json_data[["Wuchsklasse"]]) || length(json_data[["Wuchsklasse"]]) == 0)
  NA_character_ else as.character(json_data[["Wuchsklasse"]])[[1]]

## Helpers
ue_empty <- function(v) is.null(v) || length(v) == 0 || all(is.na(v)) ||
  (is.character(v) && !any(nzchar(trimws(v))))

# display name for 'field' at the current forest class (empty text -> hide)
ue_resolve_label <- function(field) {
  spec <- ue_labels[[field]]
  if (is.null(spec)) return(field)                         # fallback: field key
  if (!is.na(ue_wk) && ue_wk %in% names(spec)) unname(spec[[ue_wk]]) else unname(spec[["default"]])
}

# candidates for the pattern search = top-level names + all scalar string values
ue_candidates <- function(jd) {
  vals <- vapply(jd, function(v) if (is.character(v) && length(v) >= 1) v[[1]] else NA_character_,
                 character(1))
  unique(c(names(jd), vals[!is.na(vals)]))
}
# find the first match for 'regex' and return the trailing number (_<number>).
# finds the value regardless of the (forest-class-dependent) property name and regardless of
# whether the coded name appears as a key OR as a value in json_data.
ue_find_num <- function(jd, regex) {
  cand <- ue_candidates(jd)
  hit  <- cand[grepl(regex, cand)]
  if (length(hit) == 0) return(NA_real_)
  suppressWarnings(as.numeric(sub(".*_([0-9.]+)$", "\\1", hit[[1]])))
}
# translate via table, otherwise raw value
ue_tr <- function(x, tbl) {
  k <- as.character(x)
  if (length(k) == 1 && !is.na(k) && k %in% names(tbl)) unname(tbl[[k]]) else k
}
# HTML escape (for user free text, e.g. stand name)
ue_esc <- function(s) {
  s <- as.character(s)
  s <- gsub("&", "&amp;", s, fixed = TRUE)
  s <- gsub("<", "&lt;",  s, fixed = TRUE)
  gsub(">", "&gt;", s, fixed = TRUE)
}

## Add row: only if value present AND field not hidden
ue_rows <- list()
ue_add  <- function(field, value) {
  if (ue_empty(value)) return(invisible())              # variable does not exist -> omit
  label <- ue_resolve_label(field)
  if (is.na(label) || label == "") return(invisible())  # hidden for this forest class
  ue_rows[[length(ue_rows) + 1L]] <<- c(label, as.character(value))
}

## Collect rows (order = display order)
# stand name (only if set)
if (!ue_empty(json_data[["areaName"]]))
  ue_add("Bestandesname", ue_esc(json_data[["areaName"]]))

# goals
if (!ue_empty(json_data[["ziel"]])) {
  ue_z <- vapply(as.character(json_data[["ziel"]]),
                 function(z) ue_tr(z, ue_label_ziele), character(1))
  ue_add("Ziele", paste(ue_z, collapse = ", "))
}

# forest class / canopy closure (already readable)
ue_add("Wuchsklasse",       json_data[["Wuchsklasse"]])
ue_add("Kronenschlussgrad", json_data[["Kronenschlussgrad"]])

# tree-species distribution (only proportions > 0, descending)
if (!ue_empty(json_data[["baumartenVerteilung_Ist"]])) {
  ue_ba <- unlist(json_data[["baumartenVerteilung_Ist"]])
  ue_ba <- ue_ba[!is.na(ue_ba) & ue_ba > 0]
  if (length(ue_ba) > 0) {
    ue_ba <- sort(ue_ba, decreasing = TRUE)
    ue_add("Baumartenverteilung",
           paste(sprintf("%s:&nbsp;%s&nbsp;%%", names(ue_ba), format(ue_ba, trim = TRUE)),
                 collapse = "<br>"))
  }
}

# crown length / branch-free length / damage: pull value by pattern (name irrelevant)
ue_krone    <- ue_find_num(json_data, "_krone_[0-9.]+$")
ue_ast      <- ue_find_num(json_data, "_ast_[0-9.]+$")
ue_schaeden <- ue_find_num(json_data, "(rein|nhmisch|misch)_(sw_)?[0-9.]+$")

if (!is.na(ue_krone))    ue_add("Kronenlänge", ue_tr(ue_krone,    ue_label_krone))
if (!is.na(ue_ast))      ue_add("Astfreiheit", ue_tr(ue_ast,      ue_label_ast))
if (!is.na(ue_schaeden)) ue_add("Schäden",     ue_tr(ue_schaeden, ue_label_schaeden))

# coarse/fine road access, selection trees (0/1)
ue_add("Groberschließung",  ue_tr(json_data[["Groberschließung"]],  ue_label_flag))
ue_add("Feinerschließung",  ue_tr(json_data[["Feinerschließung"]],  ue_label_flag))
ue_add("Stabilitätsträger", ue_tr(json_data[["Stabilitätsträger"]], ue_label_flag))

## Assemble HTML
if (length(ue_rows) == 0) {
  usereingaben <- "<p><em>Keine Eingaben vorhanden.</em></p>"
} else {
  ue_body <- paste(vapply(ue_rows, function(r) paste0(
    "<tr>",
    "<td style='padding:4px 16px 4px 0; font-weight:bold; vertical-align:top; white-space:nowrap;'>",
    r[[1]], "</td>",
    "<td style='padding:4px 0; vertical-align:top;'>", r[[2]], "</td>",
    "</tr>"
  ), character(1)), collapse = "\n")
  usereingaben <- paste0(
    "<div style='font-size:0.95em;'>",
    "<table style='border-collapse:collapse;'>",
    ue_body,
    "</table></div>"
  )
}

result_list <- list()
result_list[[massnahmen_key]] <- html_text

# output canopy closure under a suffixed key:
#   Dickung -> Kronenschlussgrad_Di, Stangenholz -> _StH, sw Baumholz -> _BH
# the value comes either from the existing user input field
# (priority) or was derived from canopy above.
if (!is.null(json_data$Wuchsklasse) && length(json_data$Wuchsklasse) > 0 &&
    !is.na(json_data$Wuchsklasse) && nzchar(json_data$Wuchsklasse)) {
  kronen_suffix <- switch(
    as.character(json_data$Wuchsklasse),
    "Dickung"     = "Di",
    "Stangenholz" = "StH",
    "sw Baumholz" = "BH"
  )
  if (!is.null(kronen_suffix)) {
    result_list[[paste0("Kronenschlussgrad_", kronen_suffix)]] <-
      json_data$Kronenschlussgrad
  }
}

if (RUN_BA_KLASSE) {
  result_list$ba_klasse <- ba_klasse
}

if (compute_ziel_html) {
  # embed the signature invisibly in ziel_html (remains part of the ziel_html
  # strings preserved via the app merge) -> comparable on the next run.
  ziel_html <- paste0(
    ziel_html,
    '<script type="application/json" id="zhsig">',
    jsonlite::toJSON(ziel_html_signature, auto_unbox = TRUE),
    '</script>'
  )
  result_list$ziel_html <- ziel_html
}

# Convert to JSON with formatting
result_list$usereingaben <- usereingaben

result_json <- toJSON(result_list, auto_unbox = TRUE, pretty = TRUE, na = "null")

# Write JSON to output file
writeLines(result_json, OUTPUT_FILE)
