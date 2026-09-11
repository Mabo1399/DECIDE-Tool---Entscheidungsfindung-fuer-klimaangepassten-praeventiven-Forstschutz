# -----------------------------------------------------------------------------
# Created by Jörg Fabian Knufinke, assistance and further development by
# Max Bodanowitz. Last edited 09/2026.
# Assistance of generative AI was used in writing these scripts.

# Recommended Citation: 
# Knufinke, J.F. & Bodanowitz, M. (2026) DECIDE Tool - 
# Entscheidungsfindung für klimaangepassten, präventiven Forstschutz. Code to execute the DECIDE Tool (Version Version1.1) 
# [Computer software]. Zenodo. https://doi.org/10.5281/zenodo.22252774
# -----------------------------------------------------------------------------

# Filters the available silvicultural measures (Maßnahmenpakete) for a
# stand from the user inputs (forest class, canopy closure, tree-species
# distribution, selection trees and goals), builds the HTML measure list
# and the user-input summary, and writes them to OUTPUT_FILE.

# Setup ------------------------------------------------------------------------

require(rpart.plot)
require(rpart)
require(tidyverse)
require(readxl)
require(jsonlite)

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

json_data <- fromJSON(INPUT_FILE)

json_data$Kronenschlussgrad <-
  json_data$Kronenschlussgrad_Di %||%
  json_data$Kronenschlussgrad_BH %||%
  json_data$Kronenschlussgrad_StH

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

# Filter -----------------------------------------------------------------------

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


## 6) additional filter: selection trees
## rule:
## - only relevant for Stangenholz and sw Baumholz
## - if json_data$Stabilitätsträger == "0", only M0 and M1 are allowed
## - missing, empty or 1 -> ignore

stabilitaetstraeger_value <- NULL

if (!is.null(json_data$Stabilitätsträger)) {
  stabilitaetstraeger_value <- as.character(json_data$Stabilitätsträger)
}

if (!is.null(stabilitaetstraeger_value) &&
    length(stabilitaetstraeger_value) > 0 &&
    stabilitaetstraeger_value[1] == "0" &&
    wuchsklasse_value %in% c("Stangenholz", "sw Baumholz")) {

  massnahmen_df_refined <- remove_if_code_matches(
    massnahmen_df_refined,
    c(
      "StH_2", "StH_2-Ki", "StH_2-Lä", "StH_2-Fi-Ki", "StH_2-Fi-Ki-Lä", "StH_2-NH",
      "StH_3", "StH_3-Ki", "StH_3-Lä", "StH_3-Fi-Ki", "StH_3-Fi-Ki-Lä", "StH_3-NH", "StH_3_L",
      "BH_2", "BH_2_L",
      "BH_3", "BH_3-Fi", "BH_3-Ki", "BH_3-Lä", "BH_3-Fi-Ki", "BH_3-Fi-Ki-Lä", "BH_3_L", "BH_3-NH",
      "BH_4", "BH_4-Fi", "BH_4-Ki", "BH_4-Lä", "BH_4-Fi-Ki", "BH_4-Fi-Ki-Lä", "BH_4-NH", "BH_4_L"
    )
  )
}

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

# Output: HTML text (list of measures) -----------------------------------------


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
  "forest_gaps_majority", "forest_gaps_row"
)

rm(list = intersect(c(objects_to_remove, "objects_to_remove"), ls()))

# Export results ---------------------------------------------------------------

# ALWAYS combine the name of the measures table with the forest class:
# a plain 'Maßnahmen_Tabelle' is NEVER output. The forest class is
# guaranteed valid here (otherwise the section "filter forest class" above already
# aborts with stop()), so the switch always matches.
massnahmen_key <- switch(
  as.character(json_data$Wuchsklasse),
  "Dickung"     = "Maßnahmen_Tabelle_Di",
  "Stangenholz" = "Maßnahmen_Tabelle_StH",
  "sw Baumholz" = "Maßnahmen_Tabelle_BH",
  stop(sprintf("Keine Maßnahmen-Tabelle für Wuchsklasse '%s'.", as.character(json_data$Wuchsklasse)))
)


# usereingaben block (identical in all scripts).
# Builds the HTML string 'usereingaben' from json_data. Included below in result_list
# included so that the user-input summary is updated at EVERY step
# is updated. Missing fields are omitted automatically.
# Wording is configured in the config section (ue_labels etc.) below.
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
"habverb"     = "Habitatverbesserung für das Auerhuhn"
 
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

# Convert to JSON with formatting
result_list$usereingaben <- usereingaben

result_json <- toJSON(result_list, auto_unbox = TRUE, pretty = TRUE, na = "null")

# Write JSON to output file
writeLines(result_json, OUTPUT_FILE)
