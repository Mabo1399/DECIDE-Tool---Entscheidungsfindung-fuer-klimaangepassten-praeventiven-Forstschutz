# -----------------------------------------------------------------------------
# Created by Jörg Fabian Knufinke, assistance and further development by
# Max Bodanowitz. Last edited 09/2026.
# Assistance of generative AI was used in writing these scripts.

# Recommended Citation: 
# Knufinke, J.F. & Bodanowitz, M. (2026) DECIDE Tool - 
# Entscheidungsfindung für klimaangepassten, präventiven Forstschutz. Code to execute the DECIDE Tool (Version Version1.1) 
# [Computer software]. Zenodo. https://doi.org/10.5281/zenodo.22252774
# -----------------------------------------------------------------------------

# Script to create results that can be displayed on the DECIDE website.
# Input needed: configuration file and user-input JSON with relevant key
# factors, classification tables, and scripts for predisposition and the
# roundwood-production utility, plus a text/Excel file for formulation.

# Intro and Config -------------------------------------------------------------

require(jsonlite)

# Use configuration files so the data can be read in flexibly
# The config file is in the same directory
CONFIG_PATH <- "insert path to DECIDE-R-CONFIG.json"

# Read the JSON config and set variables
CONFIG <- fromJSON(CONFIG_PATH)

# Path to the directory that holds the TIF files & example data
DATA_DIRECTORY <- CONFIG$DATA_DIRECTORY

args = commandArgs(trailingOnly=TRUE)

# Contains parameters for this script (the data from the user polygon)
INPUT_FILE <- args[1]

# Contains the script result (various tree variables for the user polygon)
OUTPUT_FILE <- args[2]

# Prevent creation of the rplots.pdf file
pdf(NULL)

# Paths for local testing ------------------------------------------------------

#INPUT_FILE <- "insert path to input JSON"
#OUTPUT_FILE <- "insert path to output JSON"

tryCatch( # catch all issues that might come up
  {

# Read in the data -------------------------------------------------------------

json_data <- fromJSON(INPUT_FILE)

# terra/sf/sp removed: nothing in this script (or the scripts it source()s)
# calls into any of the three - they were pure startup-time overhead. Any
# geospatial sampling has already happened upstream by the time areaDetail
# arrives here as plain mean/majority values.
require(tidyverse)
require(readxl)

## Convert areaDetailText back to areaDetail
# Reconstructs the areaDetail data frame from the embedded JSON so that
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
    stop("areaDetail konnte nicht aus areaDetailText extrahiert werden.")
  }

  json_data$areaDetail <- jsonlite::fromJSON(.m[2])
  rm(.m)
}

## Reduce interface variables to canonical names
# Assumes json_data has already been read in.
# The interface delivers ONE variable per indicator, with the value encoded in
# the name (e.g. StH_krone_0.3, kr25_swBH_ast_1, Di_rein_sw_0.9). Here they are
# merged into: Schäden, Kronenlänge, HD_Wert, Astfreiheit.
# IMPORTANT (reason for the earlier "always NA" bug):
# The coded name can appear as a KEY *or* as a VALUE in json_data.
# Currently it is a VALUE, e.g. "Kronenlaenge_swBH": "swBH_krone_0.7".
# Therefore we search by pattern over KEYS AND VALUES (robust against
# the forest-class-dependent property name and against ae/ä spellings).
# ASSUMPTION: exactly one matching variable is set per indicator. If several,
# a warning is issued and the first is used; if none -> NA (with warning).

# Pattern per indicator (matches the coded name, whether key or value):
#   Kronenlänge/HD_Wert : ..._krone_<number>        (e.g. swBH_krone_0.7)
#   Astfreiheit         : ..._ast_<number>          (e.g. kr40_swBH_ast_1)
#   Schäden             : ...(rein|nhmisch|misch)[_sw]_<number>
#                         (BH/StH: ..._rein_0.7 ; Dickung: Di_rein_sw_0.7)
regex_krone    <- "_krone_[0-9.]+$"
regex_ast      <- "_ast_[0-9.]+$"
regex_schaeden <- "(rein|nhmisch|misch)_(sw_)?[0-9.]+$"

# Number from the name suffix (last _<number>)
value_from_name <- function(nm) suppressWarnings(as.numeric(sub(".*_([0-9.]+)$", "\\1", nm)))

# Finds the coded name for 'regex' (from key OR value), returns the value and
# the json_data keys to be removed.
resolve_indicator <- function(jd, regex, label) {
  hits    <- character(0)   # gefundene codierte Namen
  rm_keys <- character(0)   # associated json_data keys (to remove)
  for (k in names(jd)) {
    v      <- jd[[k]]
    v_chr  <- if (is.character(v) && length(v) >= 1 && !is.na(v[[1]])) v[[1]] else NA_character_
    if (grepl(regex, k)) {                       # coded name is in the KEY
      hits <- c(hits, k);      rm_keys <- c(rm_keys, k)
    } else if (!is.na(v_chr) && grepl(regex, v_chr)) {  # ... or in the VALUE
      hits <- c(hits, v_chr);  rm_keys <- c(rm_keys, k)
    }
  }
  if (length(hits) == 0) {
    warning(sprintf("%s: keine gesetzte Schnittstellen-Variable -> NA.", label))
    return(list(value = NA_real_, keys = character(0)))
  }
  if (length(hits) > 1) {
    warning(sprintf("%s: mehrere gesetzte Variablen (%s) -> verwende '%s'.",
                    label, paste(hits, collapse = ", "), hits[1]))
  }
  list(value = value_from_name(hits[1]), keys = unique(rm_keys))
}

res_schaeden <- resolve_indicator(json_data, regex_schaeden, "Schäden")
res_ast      <- resolve_indicator(json_data, regex_ast,      "Astfreiheit")
res_krone    <- resolve_indicator(json_data, regex_krone,    "Kronenlänge/HD_Wert")

# set canonical variables ([[ ]] because of umlauts in the names)
json_data[["Schäden"]]     <- res_schaeden$value
json_data[["Astfreiheit"]] <- res_ast$value
json_data[["Kronenlänge"]] <- res_krone$value   # one source -> both targets
json_data[["HD_Wert"]]     <- res_krone$value

json_data$Kronenschlussgrad <-
  json_data$Kronenschlussgrad_Di %||%
  json_data$Kronenschlussgrad_BH %||%
  json_data$Kronenschlussgrad_StH

# remove the complicated interface names
for (nm in unique(c(res_schaeden$keys, res_ast$keys, res_krone$keys))) {
  json_data[[nm]] <- NULL
}

# Override the tree-species geodata with user input ----------------------------
{
# Fichte: numeric value to category
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
json_data$areaDetail$majority[json_data$areaDetail$indikatorVariable == "ips_spruce_proportion"] <- fichte_num

# Kiefer: numeric value to category
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
json_data$areaDetail$majority[json_data$areaDetail$indikatorVariable == "pine"] <- kiefer_num

# Lärche: numeric value to category
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
json_data$areaDetail$majority[json_data$areaDetail$indikatorVariable == "larch"] <- laerche_num

# maxproportion
max_class <- max(c(fichte_num, kiefer_num, laerche_num), na.rm = TRUE)

max_class <- if (is.infinite(max_class)) -1 else max_class

json_data$areaDetail$majority[json_data$areaDetail$indikatorVariable == "maxproportion"] <- max_class

# conifers
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
json_data$areaDetail$majority[json_data$areaDetail$indikatorVariable == "conif"] <- conif_num

# for Till there is no -1
snow_conif_num <- ifelse(conif_num == -1, 0, conif_num)

# assign
json_data$areaDetail$majority[json_data$areaDetail$indikatorVariable == "snow_coniferous_proportion"] <- snow_conif_num

# broadleaves
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

json_data$areaDetail$majority[json_data$areaDetail$indikatorVariable == "broad"] <- broad_num

  }

# Currently unused block for weighting / interest groups -----------------------
if (FALSE) {

Ziel_DF <- data.frame(ziel = json_data$ziel, # use the goals that the user defined
                      #interessensgruppe = json_data$ziel_Prio$interessensgruppe, # interessensgruppe is right now only "eigene"
                      # but it is inserted her already as if there would be multiple,
                      # to have a placeholder when multiple interest groups are involved
                      # and when it is possible to choose from them
                      value = 0 # set to zero, will be overwritten but is needed for the predisposition
                      #priority = json_data$ziel_Prio$priority
                      ) # priority is scaled from  to 1 to 5)

# Define a vector of prädispo type

Prädispo_Ziele <- c("erhöh_widerstand", # need to add this because of the functionality below
                    "prädi_browsing",
                    "prädi_strip",
                    "prädi_cembrae",
                    "prädi_armillaria",
                    "prädi_barkbreed",
                    "prädi_heterobasidion",
                    "prädi_fire",
                    "prädi_storm",
                    "prädi_snow",
                    "prädi_ips_typ" )

Prädispo_Var_Name <- c("PAS_0_Widerstand", # need to add this because of the functionality below
                       "PAS_1_browsing",
                       "PAS_2_barkstripping",
                       "PAS_3_cembrae",
                       "PAS_4_armillaria",
                       "PAS_5_barkbreed",
                       "PAS_6_heterobasidion",
                       "PAS_7_fire",
                       "PAS_8_storm",
                       "PAS_9_snow",
                       "PAS_10_ips") # note: so far there are only the Stand and Site models for IPS

# bind to df and set colnames to make it accessible
Prädispo_match_table <- bind_cols(Prädispo_Ziele, Prädispo_Var_Name)
colnames(Prädispo_match_table) <- c("Prädispo_Ziele", "Prädispo_Var_Name")

# In case the users select "Reduktion der allgemeinen Störungsanfälligkeit" == "Erhöhung Widerstand",
# the highest predisposition must be extracted
# but this applies only to the current predisposition!
# this step is needed to check whether the goal is even "legitimate"
# if not, the script stops here and the users pick a different goal directly

if (any(json_data$ziel == "erhöh_widerstand")) {
  # check whether one of the predispositions is higher than 0.6
  selected_ziel <- json_data$areaDetail %>% filter(
    indikatorVariable %in% Prädispo_Var_Name )
  # note: the values are stored here in "mean"
  # the values from the current predisposition are needed, so using the JSON values is fine

  selected_ziel$mean <- as.numeric(selected_ziel$mean)

  # first make the df of selected ziel look alike the Ziel_DF
  selected_ziel <- selected_ziel %>% select(c(indikatorVariable, mean )) %>% rename(c( ziel = indikatorVariable, value = mean) )
  # add the interessensgruppe and prio from the original df from "erhöh_widerstand"
  # since this can only be one it does not matter which of the of the prädispositions are inserted => all get the same value
  #selected_ziel <- selected_ziel %>% mutate(
    #interessensgruppe = Ziel_DF %>% filter(ziel == "erhöh_widerstand") %>% select(interessensgruppe),
    #priority = Ziel_DF %>% filter(ziel == "erhöh_widerstand") %>% select(priority) )
  # then bind both
  Ziel_DF <- rbind(Ziel_DF, selected_ziel)

}

}

json_data1 <- json_data

# Prepare the data table (all variables) ---------------------------------------

require(readxl)

# extract the variables from the class tables
clas_table_SRH <- read.csv2("insert path to Zielerreichung_SRH.csv", dec = ",", stringsAsFactors = FALSE)
Vars_SRH <- unique(clas_table_SRH$variable)
Vars_SRH <- Vars_SRH[-1]

Ziele_PAS_Input_Daten <- read_excel("insert path to Variable_Daten_Ziel_styria.xlsx", trim_ws = TRUE)
Vars_Prädi <- unique(Ziele_PAS_Input_Daten$Variable_Name)

# to make sure there is not two that are the same
Vars <- unique(c(Vars_SRH, Vars_Prädi))

Zeitpunkt_Maßnahme <- c(
  "IST",

  ## Dickung
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

  ## Stangenholz
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

  ## Baumholz
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

# Helper functions for filtering the measures ----------------------------------

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

remove_if_contains_token_vec <- function(x, token) {
  pattern <- paste0("(^|[-_])", token, "($|[-_])")
  x[!grepl(pattern, x)]
}

remove_if_contains_all_tokens_vec <- function(x, tokens) {
  if (length(tokens) == 0 || length(x) == 0) {
    return(x)
  }

  hit_matrix <- sapply(
    tokens,
    function(tok) grepl(paste0("(^|[-_])", tok, "($|[-_])"), x)
  )

  if (is.null(dim(hit_matrix))) {
    hit_matrix <- matrix(hit_matrix, ncol = 1)
  }

  x[!apply(hit_matrix, 1, all)]
}

remove_if_code_matches_vec <- function(x, codes) {
  if (length(codes) == 0 || length(x) == 0) {
    return(x)
  }

  pattern_parts <- vapply(
    codes,
    function(code) {
      paste0("(^|IST\\+10/)", code, "($|[-_])")
    },
    character(1)
  )

  pattern <- paste(pattern_parts, collapse = "|")
  x[!grepl(pattern, x)]
}

# Filter the measures ----------------------------------------------------------

# starting vector
Zeitpunkt_Maßnahme_filtered <- Zeitpunkt_Maßnahme

# we always need IST, otherwise only those relevant for the forest class
group_patterns <- c(
  "Dickung" = "^(IST$|IST\\+10/Di_)",
  "Stangenholz" = "^(IST$|IST\\+10/StH_)",
  "sw Baumholz" = "^(IST$|IST\\+10/BH_)"
)

wuchsklasse_value <- json_data$Wuchsklasse
selected_pattern <- unname(group_patterns[wuchsklasse_value])

if (is.na(selected_pattern)) {
  stop(sprintf("Unbekannte Wuchsklasse: %s", wuchsklasse_value))
}

Zeitpunkt_Maßnahme_filtered <- Zeitpunkt_Maßnahme_filtered[
  grepl(selected_pattern, Zeitpunkt_Maßnahme_filtered)
]

# proportions of the relevant tree species (Fi, Ki, Lä + NH)
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

## remove -NH if all conifer species together sum to 100
if (conif_species_sum == 100) {
  Zeitpunkt_Maßnahme_filtered <- remove_if_contains_token_vec(
    Zeitpunkt_Maßnahme_filtered,
    "NH"
  )
}

## remove _L if forest_gaps majority == 1
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
  Zeitpunkt_Maßnahme_filtered <- remove_if_contains_token_vec(
    Zeitpunkt_Maßnahme_filtered,
    "L"
  )
}

## single species: if 0 or 100, remove everything with the token
if (prop_fi %in% c(0, 100)) {
  Zeitpunkt_Maßnahme_filtered <- remove_if_contains_token_vec(
    Zeitpunkt_Maßnahme_filtered,
    "Fi"
  )
}

if (prop_ki %in% c(0, 100)) {
  Zeitpunkt_Maßnahme_filtered <- remove_if_contains_token_vec(
    Zeitpunkt_Maßnahme_filtered,
    "Ki"
  )
}

if (prop_la %in% c(0, 100)) {
  Zeitpunkt_Maßnahme_filtered <- remove_if_contains_token_vec(
    Zeitpunkt_Maßnahme_filtered,
    "Lä"
  )
}

## combination Fi_Ki = 100
if (sum_fi_ki == 100) {
  Zeitpunkt_Maßnahme_filtered <- remove_if_contains_all_tokens_vec(
    Zeitpunkt_Maßnahme_filtered,
    c("Fi", "Ki")
  )
}

## logically complementary: Fi_Lä = 100
if (sum_fi_la == 100) {
  Zeitpunkt_Maßnahme_filtered <- remove_if_contains_all_tokens_vec(
    Zeitpunkt_Maßnahme_filtered,
    c("Fi", "Lä")
  )
}

## logically complementary: Ki_Lä = 100
if (sum_ki_la == 100) {
  Zeitpunkt_Maßnahme_filtered <- remove_if_contains_all_tokens_vec(
    Zeitpunkt_Maßnahme_filtered,
    c("Ki", "Lä")
  )
}

## combination Fi_Ki_Lä = 100
if (sum_fi_ki_la == 100) {
  Zeitpunkt_Maßnahme_filtered <- remove_if_contains_all_tokens_vec(
    Zeitpunkt_Maßnahme_filtered,
    c("Fi", "Ki", "Lä")
  )
}

# filter out measures that would reach an impossible Kronenschlussgrad
remove_codes_map <- list(
  "Dickung" = list(
    "Blöße"       = c("Di_1", "Di_2", "Di_1_M", "Di_2_M"),
    "räumdig"     = c("Di_1", "Di_2", "Di_1_M", "Di_2_M"),
    "licht"       = c("Di_1", "Di_1_M"),
    "locker"      = character(0),
    "geschlossen" = character(0),
    "gedrängt"    = character(0)
  ),
  "Stangenholz" = list(
    "Blöße"       = c("StH_2", "StH_3"),
    "räumdig"     = c("StH_2", "StH_3"),
    "licht"       = c("StH_2", "StH_3"),
    "locker"      = c("StH_2"),
    "geschlossen" = character(0),
    "gedrängt"    = character(0)
  ),
  "sw Baumholz" = list(
    "Blöße"       = c("BH_2", "BH_3", "BH_4"),
    "räumdig"     = c("BH_2", "BH_3", "BH_4"),
    "licht"       = c("BH_2", "BH_3", "BH_4"),
    "locker"      = c("BH_2", "BH_3"),
    "geschlossen" = character(0),
    "gedrängt"    = character(0)
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

Zeitpunkt_Maßnahme_filtered <- remove_if_code_matches_vec(
  Zeitpunkt_Maßnahme_filtered,
  codes_to_remove
)

# if there are no selection trees, only M0 and M1 are possible
stabilitaetstraeger_value <- NULL

if (!is.null(json_data$Stabilitätsträger)) {
  stabilitaetstraeger_value <- trimws(as.character(json_data$Stabilitätsträger))
}

if (!is.null(stabilitaetstraeger_value) &&
    length(stabilitaetstraeger_value) > 0 &&
    !is.na(stabilitaetstraeger_value[1]) &&
    stabilitaetstraeger_value[1] == "0" &&
    wuchsklasse_value %in% c("Stangenholz", "sw Baumholz")) {

  codes_to_remove_stab <- switch(
    wuchsklasse_value,
    "Stangenholz" = c("StH_2", "StH_3"),
    "sw Baumholz" = c("BH_2", "BH_3", "BH_4")
  )

  Zeitpunkt_Maßnahme_filtered <- remove_if_code_matches_vec(
    Zeitpunkt_Maßnahme_filtered,
    codes_to_remove_stab
  )
}

# Final filtering by goal ------------------------------------------------------

ziel_values <- character(0)

if (!is.null(json_data$ziel)) {
  ziel_values <- as.character(unlist(json_data$ziel, use.names = FALSE))
  ziel_values <- trimws(ziel_values)
  ziel_values <- ziel_values[!is.na(ziel_values) & ziel_values != ""]
}

# if "erhöh_widerstand" is among them, no additional restriction
if (!"erhöh_widerstand" %in% ziel_values) {

  keep_codes_by_ziel <- list(
    "Sägerundholzqualität" = c(
      "IST",
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
      "IST",
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

  relevant_ziel_values <- intersect(names(keep_codes_by_ziel), ziel_values)

  if (length(relevant_ziel_values) > 0) {
    allowed_codes <- unique(
      unlist(keep_codes_by_ziel[relevant_ziel_values], use.names = FALSE)
    )

    # extract the actual measure code
    zeitpunkt_codes <- sub("^IST\\+10/", "", Zeitpunkt_Maßnahme_filtered)

    Zeitpunkt_Maßnahme_filtered <- Zeitpunkt_Maßnahme_filtered[
      zeitpunkt_codes %in% allowed_codes
    ]
  }
}

# overwrite
Zeitpunkt_Maßnahme <- Zeitpunkt_Maßnahme_filtered

# Build the data table ---------------------------------------------------------

# build the data frame this way first; transpose later if needed
Zielerreichung_Waldbauliches_Vergleichsbestand <- data.frame(matrix(ncol = length(Vars), nrow = length(Zeitpunkt_Maßnahme)) )
colnames(Zielerreichung_Waldbauliches_Vergleichsbestand) <- Vars
# Add the Zeitpunkt/ Maßnahme as the first column
Zielerreichung_Waldbauliches_Vergleichsbestand$Zeitpunkt_Maßnahme <- Zeitpunkt_Maßnahme

## we need to create one dataframe that includes all vars, all possible actions

# Create the data frame with Zeitpunkt/ Maßnahme as the first column
Zielerreichung_Waldbauliches_Vergleichsbestand <- Zielerreichung_Waldbauliches_Vergleichsbestand %>% mutate(`Zeitpunkt_Maßnahme` = Zeitpunkt_Maßnahme) %>% select(Zeitpunkt_Maßnahme, everything())

# Set default values -----------------------------------------------------------

ct_florian <- read.csv2("insert path to pas_classification_table_Florian.csv", dec=",", stringsAsFactors = FALSE)
ct_till <- read.csv2("insert path to pas_classification_table_Till.csv", dec=",", stringsAsFactors = FALSE)
# Same file, same read parameters as clas_table_SRH above (line ~364) - reuse
# instead of reading insert path to Zielerreichung_SRH.csv a second time.
ct_srh <- clas_table_SRH

unique_vars <- unique(c(ct_florian$variable, ct_srh$variable, ct_till$variable))

  prepare_df <- function(df) {
    df <- df %>%
      mutate(
        variable = as.character(variable),
        from = as.numeric(if ("from" %in% names(.)) from else NA),
        is = as.numeric(if ("is" %in% names(.) ) is else NA),
        value = as.numeric(if ("value" %in% names(.) ) value else NA)
      )

    df %>% select(variable, from, is, value)
  }

# Clean duplicates and apply type enforcement before combining
ct_florian <- ct_florian[!duplicated(ct_florian$variable), ]
ct_till <- ct_till[!duplicated(ct_till$variable), ]
ct_srh <- ct_srh[!duplicated(ct_srh$variable), ]

default_values <- bind_rows(
  prepare_df(ct_florian),
  prepare_df(ct_till),
  prepare_df(ct_srh)
) %>%
  mutate(default = ifelse(!is.na(from), from,
                          ifelse(!is.na(is), is, value))) %>%
  select(variable, default)

# here we set the values of each variable to the minimal value possible.
# the values will be overwritten lateron, but we do this to ensure a working skript wihtout errors due to missing or out of boundary values
for (var in unique_vars) {
  value <- default_values$default[default_values$variable == var]
  Zielerreichung_Waldbauliches_Vergleichsbestand[[var]] <- value
}

# also add the default values for BA_Eignung
# Get the default value for BA_Eignung
ba_eignung_value <- default_values$default[default_values$variable == "BA_Eignung"]

# Find all variables starting with "8585"
vars_to_update <- grep("^8585", names(Zielerreichung_Waldbauliches_Vergleichsbestand), value = TRUE)

# Assign the BA_Eignung default value to those variables
for (var in vars_to_update) {
  Zielerreichung_Waldbauliches_Vergleichsbestand[[var]] <- rep(ba_eignung_value, nrow(Zielerreichung_Waldbauliches_Vergleichsbestand))
}

# clean up
rm(ct_florian, ct_till, ct_srh, default_values, prepare_df)

# Iterate over json_data$areaDetail and update the data frame
for (i in 1:length(json_data$areaDetail$indikatorVariable))  {
  # replace the NA with FALSE
  # for those values from other sources such as from the pictograms
  #json_data$areaDetail$changed <- ifelse(is.na(json_data$areaDetail$changed), "FALSE", json_data$areaChanged)
  ist_row <- which(Zielerreichung_Waldbauliches_Vergleichsbestand$Zeitpunkt_Maßnahme == "IST")

    if (!is.na(json_data$areaDetail$mean[i])) {
      Zielerreichung_Waldbauliches_Vergleichsbestand[ist_row, json_data$areaDetail$indikatorVariable[i]] <- json_data$areaDetail$mean[i]
    } else {
      Zielerreichung_Waldbauliches_Vergleichsbestand[ist_row, json_data$areaDetail$indikatorVariable[i]] <- json_data$areaDetail$majority[i]
    }
  }

# Assign the user input into the data table ------------------------------------
# (library("terra")/library("tidyverse") here removed: terra is unused
# throughout this script, and tidyverse is already loaded at the top.)

# Extract the data from the user and add it to the DF for each timepoint, because we will overwrite it (where necessary) later anyways
# here we name partly by the label and partly by the variable name so the renaming works
Zielerreichung_Waldbauliches_Vergleichsbestand <- Zielerreichung_Waldbauliches_Vergleichsbestand %>% mutate(
  Wuchsklasse = as.character(json_data$Wuchsklasse), # this is named correctly because WB definition name == label
  Kronenschlussgrad = as.character(json_data$Kronenschlussgrad) , # do not rename; this appears twice because of the double predictions,
  # but in the other models it is called Überschirmung or ips_/snow_/storm_überschirmung (as is Wuchsklasse)
  HD_Wert = as.numeric(json_data$HD_Wert), # has the same name in both (label and name)
  Kronenlänge =  as.numeric(json_data$Kronenlänge), # has the same name in both (label and name)
  Groberschließung =  as.numeric(json_data$Groberschließung), # has the same name in both (label and name)
  Feinerschließung =  as.numeric(json_data$Feinerschließung), # has the same name in both (label and name)
  Stabilitätsträger = as.numeric(json_data$Stabilitätsträger), # has the same name in both (label and name)
 # stand_history = as.numeric(json_data$stand_history),
  Schäden = as.numeric(json_data$Schäden),  # has the same name in both (label and name)
  Astfreiheit = as.numeric(json_data$Astfreiheit) # has the same name in both (label and name)
  )

## assign the first variable values for each column, except for the measures
# this prevents errors due to NAs; the values are overwritten later
Zielerreichung_Waldbauliches_Vergleichsbestand <- Zielerreichung_Waldbauliches_Vergleichsbestand %>%
  mutate_at(vars(-Zeitpunkt_Maßnahme), ~first(.))

## match the variable labels to the variable names
## the label is used in the predictions; the variable name comes from the extractions
## but later we need the name again for the predisposition-model predictions

# Work around old naming problems ----------------------------------------------

# Create renaming_template from Ziele_PAS_Input_Daten
renaming_template <- Ziele_PAS_Input_Daten %>%
  select(Variable_Name, indikatorLabel)

# Create named vector for renaming
rename_vector <- setNames(renaming_template$indikatorLabel, renaming_template$Variable_Name)

# Rename columns using rename_with
Zielerreichung_Waldbauliches_Vergleichsbestand <- Zielerreichung_Waldbauliches_Vergleichsbestand %>%
  rename_with(~ rename_vector[.], .cols = any_of(names(rename_vector)))

# Prepare the SRH predictions --------------------------------------------------

## Ensure Schäden_Wuchs
if (json_data$Wuchsklasse %in% c("Stangenholz", "Dickung")) {
  Zielerreichung_Waldbauliches_Vergleichsbestand$Schäden_Wuchs <-
    Zielerreichung_Waldbauliches_Vergleichsbestand$Schäden
}

## Beech
buche_wert <- json_data$baumartenVerteilung_Ist$Buche

buche_null_u30 <- FALSE
buche_zw_30_50 <- FALSE

if (!("Buche" %in% names(json_data$baumartenVerteilung_Ist)) ||
    is.null(buche_wert) ||
    is.na(buche_wert) ||
    buche_wert < 30) {
  buche_null_u30 <- TRUE
} else if (buche_wert >= 30 && buche_wert <= 50) {
  buche_zw_30_50 <- TRUE
}

## Kronenschlussgrad conditions
Kr_lo <- identical(json_data$Kronenschlussgrad, "locker")
Kr_gedr_geschl <- json_data$Kronenschlussgrad %in% c("gedrängt", "geschlossen")
Kr_räum <- json_data$Kronenschlussgrad %in% c("räumdig", "licht")
Kr_li_räum <- json_data$Kronenschlussgrad %in% c("räumdig", "licht")

## Tree species / diversity
kat1 <- c("Fichte", "Lärche", "Tanne", "Douglasie")
kat2 <- c("Kiefer", "Zirbe")
lh_species <- c(
  "Buche", "Rot_Eiche", "Winter_Linde", "Sommer_Linde", "Vogel_Kirsche",
  "Hainbuche", "Berg_Ulme", "Esche", "Trauben_Eiche", "Stiel_Eiche",
  "Berg_Ahorn", "Hänge_Birke"
)

get_species_value <- function(sp) {
  val <- json_data$baumartenVerteilung_Ist[[sp]]
  if (is.null(val) || is.na(val)) return(0)
  as.numeric(val)
}

calculate_total <- function(species_list) {
  sum(vapply(species_list, get_species_value, numeric(1)), na.rm = TRUE)
}

lh_total <- calculate_total(lh_species)

reinbestand <- FALSE
misch_kat1 <- FALSE
misch_kat2 <- FALSE
kat1_kat2_gleich <- FALSE
mehr_als_3 <- FALSE

all_species <- c(kat1, kat2)

valid_species_count <- sum(vapply(all_species, function(sp) {
  val <- get_species_value(sp)
  val > 0 && val <= 70
}, logical(1))) + as.integer(lh_total > 0 && lh_total <= 70)

mehr_als_3 <- valid_species_count > 3

kat1_kat2_gleich <- (
  (any(vapply(kat1, function(sp) get_species_value(sp) == 50, logical(1))) &&
     any(vapply(kat2, function(sp) get_species_value(sp) == 50, logical(1)))) ||
    lh_total == 50
)

reinbestand <- (
  (any(vapply(c(kat1, kat2), function(sp) get_species_value(sp) >= 70, logical(1))) ||
     lh_total >= 70) &&
    !kat1_kat2_gleich &&
    !mehr_als_3
)

misch_kat1 <- (
  any(vapply(kat1, function(sp) {
    val <- get_species_value(sp)
    val >= 50 && val < 70
  }, logical(1))) &&
    !kat1_kat2_gleich &&
    !mehr_als_3
)

misch_kat2 <- (
  (any(vapply(kat2, function(sp) {
    val <- get_species_value(sp)
    val >= 50 && val < 70
  }, logical(1))) ||
    (lh_total > 50 && lh_total < 70)) &&
    !kat1_kat2_gleich &&
    !mehr_als_3
)

## Schäden_Wuchs / sums
kat1_total <- calculate_total(kat1)
kat2_total <- calculate_total(kat2)

kat1_kat2_gleich_sw <- kat1_total == 50 && kat2_total == 50

kat1_ue50 <- kat1_total >= 50 && !kat1_kat2_gleich_sw
kat2_ue50 <- kat2_total >= 50 && !kat1_kat2_gleich_sw

## Assign tree-species diversity baseline
ba_diversität_baseline <- dplyr::case_when(
  mehr_als_3 ~ 0.9,
  reinbestand ~ 0.3,
  misch_kat1 | misch_kat2 | kat1_kat2_gleich ~ 0.7,
  TRUE ~ 0.3
)

Zielerreichung_Waldbauliches_Vergleichsbestand <-
  Zielerreichung_Waldbauliches_Vergleichsbestand %>%
  dplyr::mutate(BA_Diversität = ba_diversität_baseline)

## Load rules
rules <- readxl::read_excel("insert path to prognosen_srh_classtable.xlsx") %>%
  dplyr::mutate(
    dplyr::across(
      where(is.character),
      ~ {
        x <- trimws(.x)
        x[tolower(x) %in% c("", "na")] <- NA_character_
        x
      }
    )
  )

## Check mandatory columns in the rule matrix
required_rule_cols <- c("Indikator", "Wuchsklasse", "Abhängigkeit", "IST")
missing_rule_cols <- setdiff(required_rule_cols, names(rules))

if (length(missing_rule_cols) > 0) {
  stop(
    paste0(
      "Folgende Pflichtspalten fehlen in der Regelmatrix: ",
      paste(missing_rule_cols, collapse = ", ")
    )
  )
}

## Collect conditions centrally
conditions <- c(
  "kat1_ue50" = kat1_ue50,
  "kat2_ue50" = kat2_ue50,
  "kat1_kat2_gleich_sw" = kat1_kat2_gleich_sw,
  "reinbestand" = reinbestand,
  "misch_kat1" = misch_kat1,
  "misch_kat2" = misch_kat2,
  "kat1_kat2_gleich" = kat1_kat2_gleich,
  "mehr_als_3" = mehr_als_3,
  "Kr_lo" = Kr_lo,
  "Kr_gedr_geschl" = Kr_gedr_geschl,
  "Kr_räum" = Kr_räum,
  "Kr_li_räum" = Kr_li_räum,
  "buche_zw_30_50" = buche_zw_30_50,
  "buche_null_u30" = buche_null_u30
)

## Determine the relevant forest class from the IST row
ist_rows_all <- which(
  Zielerreichung_Waldbauliches_Vergleichsbestand$Zeitpunkt_Maßnahme == "IST"
)

if (length(ist_rows_all) != 1) {
  stop("Es muss genau eine IST-Zeile im Vergleichsbestand geben.")
}

target_wuchsklasse <- as.character(
  Zielerreichung_Waldbauliches_Vergleichsbestand$Wuchsklasse[ist_rows_all]
)

if (is.na(target_wuchsklasse) || target_wuchsklasse == "") {
  stop("Die Wuchsklasse der IST-Zeile ist leer oder NA.")
}

## Restrict rules to the relevant forest class
rules <- rules %>%
  dplyr::filter(as.character(Wuchsklasse) == target_wuchsklasse)

if (nrow(rules) == 0) {
  stop(
    paste0(
      "Keine Regeln für Wuchsklasse '", target_wuchsklasse, "' gefunden."
    )
  )
}

## Check that all conditions from the filtered rule matrix are defined
rule_conditions <- unique(rules$Abhängigkeit[!is.na(rules$Abhängigkeit)])
missing_conditions <- setdiff(rule_conditions, names(conditions))

if (length(missing_conditions) > 0) {
  stop(
    paste0(
      "Diese Bedingungen aus rules$Abhängigkeit sind noch nicht definiert: ",
      paste(missing_conditions, collapse = ", ")
    )
  )
}

## Helper functions
coerce_like <- function(x, template) {
  if (length(x) != 1) {
    stop("coerce_like erwartet genau einen Wert.")
  }

  if (is.na(x)) {
    return(NA)
  }

  if (is.integer(template)) {
    out <- suppressWarnings(as.integer(x))
    if (is.na(out)) {
      stop(paste0("Wert kann nicht als integer interpretiert werden: ", x))
    }
    return(out)
  }

  if (is.numeric(template)) {
    out <- suppressWarnings(as.numeric(x))
    if (is.na(out)) {
      stop(paste0("Wert kann nicht als numeric interpretiert werden: ", x))
    }
    return(out)
  }

  if (is.logical(template)) {
    out <- suppressWarnings(as.logical(x))
    if (is.na(out)) {
      stop(paste0("Wert kann nicht als logical interpretiert werden: ", x))
    }
    return(out)
  }

  as.character(x)
}

same_value <- function(x, y, template) {
  if (length(x) != 1) {
    stop("same_value erwartet für x genau einen IST-Wert.")
  }

  if (is.numeric(template) || is.integer(template)) {
    x_num <- suppressWarnings(as.numeric(x))
    y_num <- suppressWarnings(as.numeric(y))
    return(!is.na(x_num) & !is.na(y_num) & x_num == y_num)
  }

  x_chr <- as.character(x)
  y_chr <- as.character(y)

  !is.na(x_chr) & !is.na(y_chr) & x_chr == y_chr
}

matches_dependency <- function(rule_value, conditions) {
  if (length(rule_value) != 1) {
    stop("matches_dependency erwartet genau einen Regelwert.")
  }

  if (is.na(rule_value)) {
    return(FALSE)
  }

  if (!rule_value %in% names(conditions)) {
    stop(paste0("Bedingung nicht definiert: ", rule_value))
  }

  isTRUE(conditions[[rule_value]])
}

apply_rule_table <- function(df, rules, conditions, scenario_col = "Zeitpunkt_Maßnahme") {
  df_out <- df

  required_df_cols <- c("Wuchsklasse", scenario_col)
  missing_df_cols <- setdiff(required_df_cols, names(df_out))

  if (length(missing_df_cols) > 0) {
    stop(
      paste0(
        "Folgende Pflichtspalten fehlen im Data Frame: ",
        paste(missing_df_cols, collapse = ", ")
      )
    )
  }

  ist_rows <- which(df_out[[scenario_col]] == "IST")
  if (length(ist_rows) == 0) {
    stop("Keine IST-Zeile gefunden.")
  }
  if (length(ist_rows) > 1) {
    stop("Mehrere IST-Zeilen gefunden. Es muss genau eine IST-Zeile geben.")
  }

  ist_row <- ist_rows[1]
  target_wuchsklasse <- as.character(df_out$Wuchsklasse[ist_row])

  keep_rows <- which(
    df_out[[scenario_col]] == "IST" |
      as.character(df_out$Wuchsklasse) == target_wuchsklasse
  )

  df_work <- df_out[keep_rows, , drop = FALSE]

  ist_rows_work <- which(df_work[[scenario_col]] == "IST")
  if (length(ist_rows_work) != 1) {
    stop("Nach Filterung muss genau eine IST-Zeile vorhanden sein.")
  }
  ist_row_work <- ist_rows_work[1]

  applicable_indicators <- intersect(unique(rules$Indikator), names(df_work))
  if (length(applicable_indicators) == 0) {
    stop("Keiner der Indikatoren aus der Regelmatrix ist im Data Frame vorhanden.")
  }

  target_cols <- unique(df_work[[scenario_col]][df_work[[scenario_col]] != "IST"])
  missing_rule_target_cols <- setdiff(target_cols, names(rules))

  if (length(missing_rule_target_cols) > 0) {
    stop(
      paste0(
        "Für folgende Zeitpunkte fehlt die entsprechende Spalte in der Regelmatrix: ",
        paste(missing_rule_target_cols, collapse = ", ")
      )
    )
  }

  for (row_idx in seq_len(nrow(df_work))) {
    zielzeitpunkt <- as.character(df_work[[scenario_col]][row_idx])

    if (is.na(zielzeitpunkt) || zielzeitpunkt == "IST") {
      next
    }

    for (indikator in applicable_indicators) {
      template_col <- df_work[[indikator]]
      ist_value <- df_work[[indikator]][ist_row_work]

      rules_for_indicator <- rules[
        as.character(rules$Indikator) == indikator,
        ,
        drop = FALSE
      ]

      if (nrow(rules_for_indicator) == 0) {
        next
      }

      rules_for_ist <- rules_for_indicator[
        same_value(ist_value, rules_for_indicator$IST, template_col),
        ,
        drop = FALSE
      ]

      if (nrow(rules_for_ist) == 0) {
        stop(
          paste0(
            "Keine passende Regel zum IST-Wert gefunden für Indikator='", indikator,
            "', Wuchsklasse='", target_wuchsklasse,
            "', IST='", ist_value, "'."
          )
        )
      }

      dep_match <- vapply(
        rules_for_ist$Abhängigkeit,
        matches_dependency,
        logical(1),
        conditions = conditions
      )

      rules_dep <- rules_for_ist[dep_match, , drop = FALSE]

      if (nrow(rules_dep) == 1) {
        selected_rule <- rules_dep
      } else if (nrow(rules_dep) > 1) {
        stop(
          paste0(
            "Mehrere passende Regeln gefunden für Indikator='", indikator,
            "', Wuchsklasse='", target_wuchsklasse,
            "', Zeitpunkt='", zielzeitpunkt,
            "', IST='", ist_value, "'."
          )
        )
      } else {
        rules_fallback <- rules_for_ist[is.na(rules_for_ist$Abhängigkeit), , drop = FALSE]

        if (nrow(rules_fallback) == 1) {
          selected_rule <- rules_fallback
        } else if (nrow(rules_fallback) > 1) {
          stop(
            paste0(
              "Mehrere Fallback-Regeln ohne Abhängigkeit gefunden für Indikator='", indikator,
              "', Wuchsklasse='", target_wuchsklasse,
              "', Zeitpunkt='", zielzeitpunkt,
              "', IST='", ist_value, "'."
            )
          )
        } else {
          stop(
            paste0(
              "Keine passende Abhängigkeit und keine Fallback-Regel gefunden für Indikator='", indikator,
              "', Wuchsklasse='", target_wuchsklasse,
              "', Zeitpunkt='", zielzeitpunkt,
              "', IST='", ist_value, "'."
            )
          )
        }
      }

      new_value <- selected_rule[[zielzeitpunkt]][1]

      if (!is.na(new_value)) {
        df_work[[indikator]][row_idx] <- coerce_like(new_value, template_col)
      }
    }
  }

  df_out[keep_rows, names(df_work)] <- df_work

  df_out
}

# Run the SRH predictions ------------------------------------------------------

Zielerreichung_Waldbauliches_Vergleichsbestand <-
  apply_rule_table(
    df = Zielerreichung_Waldbauliches_Vergleichsbestand,
    rules = rules,
    conditions = conditions
  )

# PAS predictions --------------------------------------------------------------

tryCatch(
  {

    {

      # Canopy

      # Till, 3 Vars
      Zielerreichung_Waldbauliches_Vergleichsbestand <- Zielerreichung_Waldbauliches_Vergleichsbestand %>%
        mutate(Ips_Überschirmung = case_when(

          Kronenschlussgrad == "gedrängt" ~ 97.5,
          Kronenschlussgrad == "geschlossen" ~ 90.5 ,
          Kronenschlussgrad == "locker" ~ 75.5,
          Kronenschlussgrad == "licht" ~ 55.5,
          Kronenschlussgrad == "räumdig" ~ 37.5,
          # the knockout is never triggered this way, but is otherwise correct (see below)
          Kronenschlussgrad == "Blöße" ~ 30)
        )

      Zielerreichung_Waldbauliches_Vergleichsbestand <- Zielerreichung_Waldbauliches_Vergleichsbestand %>%
        mutate(Sturm_Überschirmung = Ips_Überschirmung) %>%
        mutate(Schnee_Überschirmung = Ips_Überschirmung)

      # Florian, 1 Var

      Zielerreichung_Waldbauliches_Vergleichsbestand <- Zielerreichung_Waldbauliches_Vergleichsbestand %>%
        mutate(Überschirmung = case_when(
          Kronenschlussgrad == "gedrängt" ~ 97.5,
          Kronenschlussgrad == "geschlossen" ~ 90.5 ,

          # you cannot reach either 8 or 12 points per Florian without
          # drastically changing the classes.

          Kronenschlussgrad == "locker" ~ 75.5,

          Kronenschlussgrad == "licht" ~ 55.5,
          # for räumdig we use the more conservative (lower) value instead of the mean, so that
          # the class between 40 and 50 is covered.
          Kronenschlussgrad == "räumdig" ~ 40.5,
          Kronenschlussgrad == "Blöße" ~ 30)
        )

      # safeguard, since Fichte, Kiefer, Lärche are used later
      # user value or 0

      Fichte <- ifelse(
        is.null(json_data$baumartenVerteilung_Ist$Fichte) || is.na(json_data$baumartenVerteilung_Ist$Fichte),
        0,
        json_data$baumartenVerteilung_Ist$Fichte
      )

      Kiefer <- ifelse(
        is.null(json_data$baumartenVerteilung_Ist$Kiefer) || is.na(json_data$baumartenVerteilung_Ist$Kiefer),
        0,
        json_data$baumartenVerteilung_Ist$Kiefer
      )

      Lärche <- ifelse(
        is.null(json_data$baumartenVerteilung_Ist$Lärche) || is.na(json_data$baumartenVerteilung_Ist$Lärche),
        0,
        json_data$baumartenVerteilung_Ist$Lärche
      )

      # central measure groups (only to avoid redundant lists;
      # functionality and results stay unchanged)
      massnahmen_gap <- c(
        "IST+10/BH_2_L",
        "IST+10/BH_3_L",
        "IST+10/BH_4_L",
        "IST+10/StH_3_L"
      )

      massnahmen_lh_plus_10 <- c(
        "IST+10/Di_1_M-Ki",
        "IST+10/Di_1_M-Fi-Ki",
        "IST+10/Di_1_M-NH",
        "IST+10/Di_2_M-Lä",
        "IST+10/Di_2_M-Fi-Ki",
        "IST+10/Di_2_M-NH",
        "IST+10/StH_2-Ki",
        "IST+10/StH_2-Lä",
        "IST+10/StH_2-Fi-Ki",
        "IST+10/StH_2-Fi-Ki-Lä",
        "IST+10/StH_2-NH",
        "IST+10/StH_3-Ki",
        "IST+10/StH_3-Lä",
        "IST+10/StH_3-Fi-Ki",
        "IST+10/StH_3-Fi-Ki-Lä",
        "IST+10/StH_3-NH",
        "IST+10/BH_3-Fi",
        "IST+10/BH_3-Ki",
        "IST+10/BH_3-Lä",
        "IST+10/BH_3-Fi-Ki",
        "IST+10/BH_3-Fi-Ki-Lä",
        "IST+10/BH_3-NH",
        "IST+10/BH_4-Fi",
        "IST+10/BH_4-Ki",
        "IST+10/BH_4-Lä",
        "IST+10/BH_4-Fi-Ki",
        "IST+10/BH_4-Fi-Ki-Lä",
        "IST+10/BH_4-NH"
      )

      massnahmen_nh_minus_10 <- massnahmen_lh_plus_10

      massnahmen_fi <- c(
        "IST+10/BH_3-Fi",
        "IST+10/BH_4-Fi"
      )

      massnahmen_ki <- c(
        "IST+10/Di_1_M-Ki",
        "IST+10/StH_2-Ki",
        "IST+10/StH_3-Ki",
        "IST+10/BH_3-Ki",
        "IST+10/BH_4-Ki"
      )

      massnahmen_lae <- c(
        "IST+10/Di_2_M-Lä",
        "IST+10/StH_2-Lä",
        "IST+10/StH_3-Lä",
        "IST+10/BH_3-Lä",
        "IST+10/BH_4-Lä"
      )

      massnahmen_fi_ki <- c(
        "IST+10/Di_1_M-Fi-Ki",
        "IST+10/Di_2_M-Fi-Ki",
        "IST+10/StH_2-Fi-Ki",
        "IST+10/StH_3-Fi-Ki",
        "IST+10/BH_3-Fi-Ki",
        "IST+10/BH_4-Fi-Ki"
      )

      massnahmen_fi_ki_lae <- c(
        "IST+10/StH_2-Fi-Ki-Lä",
        "IST+10/StH_3-Fi-Ki-Lä",
        "IST+10/BH_3-Fi-Ki-Lä",
        "IST+10/BH_4-Fi-Ki-Lä"
      )

      massnahmen_maxprop_fi <- c(
        massnahmen_fi_ki_lae,
        massnahmen_fi_ki,
        massnahmen_fi
      )

      massnahmen_maxprop_ki <- c(
        massnahmen_fi_ki_lae,
        massnahmen_fi_ki,
        massnahmen_ki
      )

      massnahmen_maxprop_lae <- c(
        massnahmen_fi_ki_lae,
        massnahmen_lae
      )

      massnahmen_nh_only <- c(

        "IST+10/Di_1_M-NH",
        "IST+10/Di_2_M-NH",
        "IST+10/StH_2-NH",
        "IST+10/StH_3-NH",
        "IST+10/BH_3-NH",
        "IST+10/BH_4-NH"
      )

      massnahmen_maxprop_fi_equal_ki <- c(
        massnahmen_fi_ki_lae,
        "IST+10/Di_2_M-Fi-Ki",
        "IST+10/StH_2-Fi-Ki",
        "IST+10/StH_3-Fi-Ki",
        "IST+10/BH_3-Fi-Ki",
        "IST+10/BH_4-Fi-Ki"
      )

      # all independent of the forest class
      # Forest gaps
      Zielerreichung_Waldbauliches_Vergleichsbestand <- Zielerreichung_Waldbauliches_Vergleichsbestand %>%
        mutate(Bestandeslücken = case_when(
          Zeitpunkt_Maßnahme == "IST" ~ as.numeric(first(Zielerreichung_Waldbauliches_Vergleichsbestand$Bestandeslücken)),

          as.numeric(Bestandeslücken) == 1 ~ 1,
          as.numeric(Bestandeslücken) == 0 &  (Zeitpunkt_Maßnahme == "IST+10/BH_2_L"|

                                                 Zeitpunkt_Maßnahme == "IST+10/BH_3_L" |
                                                 Zeitpunkt_Maßnahme == "IST+10/BH_4_L" |
                                                 Zeitpunkt_Maßnahme == "IST+10/StH_3_L" ) ~ 1,

          TRUE ~ as.numeric(Bestandeslücken)
        ))

      # Broadleaf proportion

      lh_species <- c("Buche", "Rot_Eiche", "Winter_Linde", "Sommer_Linde", "Vogel_Kirsche",
                      "Hainbuche", "Berg_Ulme", "Esche", "Trauben_Eiche", "Stiel_Eiche",
                      "Berg_Ahorn", "Hänge_Birke")  # Laubholzarten
      # sum of broadleaves
      lh_percent <- sum(sapply(lh_species, function(sp) ifelse(!is.null(json_data$baumartenVerteilung_Ist[[sp]]), json_data$baumartenVerteilung_Ist[[sp]], 0)), na.rm = TRUE)

      Zielerreichung_Waldbauliches_Vergleichsbestand <- Zielerreichung_Waldbauliches_Vergleichsbestand %>%
        mutate(

          Laubholzanteil = case_when(
            # if classified as "Kein Wald", it must stay that way
            Laubholzanteil == -1 ~ -1,
            # for the relevant measures then +10% to the LH user value
            Zeitpunkt_Maßnahme %in% massnahmen_lh_plus_10 ~ lh_percent + 10,
            # otherwise keep the user value
            TRUE ~ lh_percent
          ),

          # apply classification
          Laubholzanteil = case_when(
            Laubholzanteil == -1 ~ -1,
            Laubholzanteil < 10 ~ 0,
            Laubholzanteil < 40 ~ 1,
            Laubholzanteil < 60 ~ 2,
            Laubholzanteil < 90 ~ 3,
            Laubholzanteil <= 100 ~ 4,
            # limitation: via the user values the approach can reach at most 60% (since stands above 50% LH are not
            # handled, but this is only problematic once we want to extend the tool
          )
        )

      # Conifer proportion

      nh_species <- c("Fichte", "Lärche", "Tanne", "Douglasie", "Kiefer", "Zirbe")

      # sum of broadleaves
      nh_percent <- sum(sapply(nh_species, function(sp) ifelse(!is.null(json_data$baumartenVerteilung_Ist[[sp]]), json_data$baumartenVerteilung_Ist[[sp]], 0)), na.rm = TRUE)

      Zielerreichung_Waldbauliches_Vergleichsbestand <- Zielerreichung_Waldbauliches_Vergleichsbestand %>%
        mutate(

          Nadelholzanteil = case_when(
            # if classified as "Kein Wald", it must stay that way
            Nadelholzanteil == -1 ~ -1,
            # for the relevant measures then -10% off the NH user value

            Zeitpunkt_Maßnahme %in% massnahmen_lh_plus_10 ~ nh_percent - 10,
            # otherwise keep the user value
            TRUE ~ nh_percent
          ),

          # apply classification
          Nadelholzanteil = case_when(
            Nadelholzanteil == -1 ~ -1,
            Nadelholzanteil < 10 ~ 0,
            Nadelholzanteil < 40 ~ 1,
            Nadelholzanteil < 60 ~ 2,
            Nadelholzanteil < 90 ~ 3,
            Nadelholzanteil <= 100 ~ 4)
        )

      # Schnee_Nadelholzanteil, Till

      # needs nh_percent from the previous block

      Zielerreichung_Waldbauliches_Vergleichsbestand <- Zielerreichung_Waldbauliches_Vergleichsbestand %>%
        mutate(

          Schnee_Nadelholzanteil = case_when(
            Schnee_Nadelholzanteil == -1 ~ -1,
            Zeitpunkt_Maßnahme %in% massnahmen_lh_plus_10 ~ nh_percent - 10,
            TRUE ~ nh_percent
          ),

          # classification
          Schnee_Nadelholzanteil = case_when(
            Schnee_Nadelholzanteil >= -10 & Schnee_Nadelholzanteil <= 10 ~ 0,
            Schnee_Nadelholzanteil >= 11 & Schnee_Nadelholzanteil < 40 ~ 1,
            Schnee_Nadelholzanteil >= 40 & Schnee_Nadelholzanteil <= 60 ~ 2,
            Schnee_Nadelholzanteil > 60 & Schnee_Nadelholzanteil < 90 ~ 3,
            Schnee_Nadelholzanteil >= 90 & Schnee_Nadelholzanteil <= 100 ~ 4
          )
        )

      # Ips_Fichtenanteil, Till

      # user value
      spruce_percent <- json_data$baumartenVerteilung_Ist$Fichte

      Zielerreichung_Waldbauliches_Vergleichsbestand <- Zielerreichung_Waldbauliches_Vergleichsbestand %>%
        mutate(

          Ips_Fichtenanteil = case_when(
            # for the relevant measures then -10% off the Fichte user value
            Zeitpunkt_Maßnahme %in% massnahmen_fi ~ spruce_percent - 10,
            # otherwise keep the user value
            TRUE ~ spruce_percent
          ),

          # classification
          Ips_Fichtenanteil = case_when(
            Ips_Fichtenanteil >= -10 &  Ips_Fichtenanteil <= 10 ~ 0,
            Ips_Fichtenanteil >= 11 &  Ips_Fichtenanteil < 40 ~ 1,
            Ips_Fichtenanteil >= 40 &  Ips_Fichtenanteil <= 60 ~ 2,
            Ips_Fichtenanteil > 60 &  Ips_Fichtenanteil < 90 ~ 3,
            Ips_Fichtenanteil >= 90 &  Ips_Fichtenanteil <= 100 ~ 4
          )
        )

      # Larch proportion

      # user value
      larch_percent <- json_data$baumartenVerteilung_Ist$Lärche

      Zielerreichung_Waldbauliches_Vergleichsbestand <- Zielerreichung_Waldbauliches_Vergleichsbestand %>%
        mutate(

          Lärchenanteil = case_when(
            # if classified as "Kein Wald", it must stay that way
            Lärchenanteil == -1 ~ -1,
            # for the relevant measures then -10% off the Lärche user value
            Zeitpunkt_Maßnahme %in% massnahmen_lae ~ larch_percent - 10,
            # otherwise keep the user value
            TRUE ~ larch_percent
          ),

          # apply classification
          Lärchenanteil = case_when(
            Lärchenanteil == -1 ~ -1,
            Lärchenanteil < 10 ~ 0,
            Lärchenanteil < 40 ~ 1,
            Lärchenanteil < 60 ~ 2,
            Lärchenanteil < 90 ~ 3,
            Lärchenanteil <= 100 ~ 4
          )
        )

      # Pine proportion

      # user value
      pine_percent <- json_data$baumartenVerteilung_Ist$Kiefer

      Zielerreichung_Waldbauliches_Vergleichsbestand <- Zielerreichung_Waldbauliches_Vergleichsbestand %>%
        mutate(

          Kiefernanteil = case_when(
            # if classified as "Kein Wald", it must stay that way
            Kiefernanteil == -1 ~ -1,
            # for the relevant measures then -10% off the Kiefer user value
            Zeitpunkt_Maßnahme %in% massnahmen_ki ~ pine_percent - 10,

            # otherwise keep the user value
            TRUE ~ pine_percent
          ),

          # apply classification
          Kiefernanteil = case_when(
            Kiefernanteil == -1 ~ -1,
            Kiefernanteil < 10 ~ 0,
            Kiefernanteil < 40 ~ 1,
            Kiefernanteil < 60 ~ 2,
            Kiefernanteil < 90 ~ 3,
            Kiefernanteil <= 100 ~ 4
          )
        )

      # proportion of Fichte, Kiefer or Lärche (maxproportion)
      # convention when several species occur: Fichte is reduced first
      # if Fichte is not present, Kiefer is reduced

      # Assuming Fichte, Kiefer, and Lärche are numeric values
      if ((Fichte > Kiefer) & (Fichte > Lärche)) {
        # Fichte is greater than both Kiefer and Lärche

        # user value
        maxprop <- json_data$baumartenVerteilung_Ist$Fichte

        Zielerreichung_Waldbauliches_Vergleichsbestand <- Zielerreichung_Waldbauliches_Vergleichsbestand %>%
          mutate(

            `Anteil Fichte, Kiefer oder Lärche` = case_when(
              # if classified as "Kein Wald", the value stays -1
              `Anteil Fichte, Kiefer oder Lärche` == -1 ~ -1,

              # for the relevant measures (list) 10 percentage points are subtracted from the user value (maxprop)
              Zeitpunkt_Maßnahme %in% massnahmen_maxprop_fi ~ maxprop - 10,

              # for all other cases the user value is kept unchanged
              TRUE ~ maxprop
            ),

            # classify the proportion into categories (0–4)
            `Anteil Fichte, Kiefer oder Lärche` = case_when(
              # "Kein Wald" stays -1
              `Anteil Fichte, Kiefer oder Lärche` == -1 ~ -1,

              # proportion below 10% => category 0
              `Anteil Fichte, Kiefer oder Lärche` < 10 ~ 0,

              # proportion below 40% => category 1
              `Anteil Fichte, Kiefer oder Lärche` < 40 ~ 1,

              # proportion below 60% => category 2
              `Anteil Fichte, Kiefer oder Lärche` < 60 ~ 2,

              # proportion below 90% => category 3
              `Anteil Fichte, Kiefer oder Lärche` < 90 ~ 3,

              # proportion up to 100% => category 4
              `Anteil Fichte, Kiefer oder Lärche` <= 100 ~ 4
            )
          )

      }

      if ((Kiefer > Fichte) & (Kiefer > Lärche)) {

        # Kiefer is greater than both Fichte and Lärche

        # user value
        maxprop <- json_data$baumartenVerteilung_Ist$Kiefer

        Zielerreichung_Waldbauliches_Vergleichsbestand <- Zielerreichung_Waldbauliches_Vergleichsbestand %>%
          mutate(

            `Anteil Fichte, Kiefer oder Lärche` = case_when(
              # if classified as "Kein Wald", it stays that way
              `Anteil Fichte, Kiefer oder Lärche` == -1 ~ -1,

              # for the relevant measures -10% off the user value (maxprop)
              Zeitpunkt_Maßnahme %in% massnahmen_maxprop_ki ~ maxprop - 10,

              # otherwise keep the user value
              TRUE ~ maxprop
            ),

            # classify the proportion into categories (0–4)
            `Anteil Fichte, Kiefer oder Lärche` = case_when(
              # "Kein Wald" stays -1
              `Anteil Fichte, Kiefer oder Lärche` == -1 ~ -1,

              # proportion below 10% => category 0
              `Anteil Fichte, Kiefer oder Lärche` < 10 ~ 0,

              # proportion below 40% => category 1
              `Anteil Fichte, Kiefer oder Lärche` < 40 ~ 1,

              # proportion below 60% => category 2
              `Anteil Fichte, Kiefer oder Lärche` < 60 ~ 2,

              # proportion below 90% => category 3
              `Anteil Fichte, Kiefer oder Lärche` < 90 ~ 3,

              # proportion up to 100% => category 4
              `Anteil Fichte, Kiefer oder Lärche` <= 100 ~ 4
            )
          )

      }

      if ((Lärche > Fichte) & (Lärche > Kiefer)) {
        # Lärche is greater than both Fichte and Kiefer

        # user value
        maxprop <- json_data$baumartenVerteilung_Ist$Lärche

        Zielerreichung_Waldbauliches_Vergleichsbestand <- Zielerreichung_Waldbauliches_Vergleichsbestand %>%
          mutate(

            `Anteil Fichte, Kiefer oder Lärche` = case_when(
              # if classified as "Kein Wald", it stays that way
              `Anteil Fichte, Kiefer oder Lärche` == -1 ~ -1,

              # for the relevant measures -10% off the user value (maxprop)
              Zeitpunkt_Maßnahme %in% massnahmen_maxprop_lae ~ maxprop - 10,

              # otherwise keep the user value
              TRUE ~ maxprop
            ),

            # classify the proportion into categories (0–4)
            `Anteil Fichte, Kiefer oder Lärche` = case_when(
              # "Kein Wald" stays -1
              `Anteil Fichte, Kiefer oder Lärche` == -1 ~ -1,

              # proportion below 10% => category 0
              `Anteil Fichte, Kiefer oder Lärche` < 10 ~ 0,

              # proportion below 40% => category 1
              `Anteil Fichte, Kiefer oder Lärche` < 40 ~ 1,

              # proportion below 60% => category 2
              `Anteil Fichte, Kiefer oder Lärche` < 60 ~ 2,

              # proportion below 90% => category 3
              `Anteil Fichte, Kiefer oder Lärche` < 90 ~ 3,

              # proportion up to 100% => category 4
              `Anteil Fichte, Kiefer oder Lärche` <= 100 ~ 4
            )
          )

      }

      if ((Fichte == Kiefer) & (Fichte > Lärche)) {
        # Fichte and Kiefer are equal, and Fichte & Kiefer are greater than Lärche

        # user value
        maxprop <- json_data$baumartenVerteilung_Ist$Fichte

        Zielerreichung_Waldbauliches_Vergleichsbestand <- Zielerreichung_Waldbauliches_Vergleichsbestand %>%
          mutate(
            `Anteil Fichte, Kiefer oder Lärche` = case_when(
              `Anteil Fichte, Kiefer oder Lärche` == -1 ~ -1,
              Zeitpunkt_Maßnahme %in% massnahmen_maxprop_fi_equal_ki ~ maxprop - 10,
              TRUE ~ maxprop),

            # classify the proportion into categories (0–4)
            `Anteil Fichte, Kiefer oder Lärche` = case_when(
              # "Kein Wald" stays -1
              `Anteil Fichte, Kiefer oder Lärche` == -1 ~ -1,

              # proportion below 10% => category 0
              `Anteil Fichte, Kiefer oder Lärche` < 10 ~ 0,

              # proportion below 40% => category 1
              `Anteil Fichte, Kiefer oder Lärche` < 40 ~ 1,

              # proportion below 60% => category 2
              `Anteil Fichte, Kiefer oder Lärche` < 60 ~ 2,

              # proportion below 90% => category 3
              `Anteil Fichte, Kiefer oder Lärche` < 90 ~ 3,

              # proportion up to 100% => category 4
              `Anteil Fichte, Kiefer oder Lärche` <= 100 ~ 4
            )
          )

      }

      if ((Fichte == Lärche) & (Fichte > Kiefer)) {
        # Fichte and Lärche are equal, and Fichte & Lärche are greater than Kiefer

        # user value
        maxprop <- json_data$baumartenVerteilung_Ist$Fichte

        Zielerreichung_Waldbauliches_Vergleichsbestand <- Zielerreichung_Waldbauliches_Vergleichsbestand %>%
          mutate(
            `Anteil Fichte, Kiefer oder Lärche` = case_when(
              `Anteil Fichte, Kiefer oder Lärche` == -1 ~ -1,
              Zeitpunkt_Maßnahme %in% massnahmen_fi_ki_lae ~ maxprop - 10,
              TRUE ~ maxprop
            ),

            # classify the proportion into categories (0–4)
            `Anteil Fichte, Kiefer oder Lärche` = case_when(
              # "Kein Wald" stays -1
              `Anteil Fichte, Kiefer oder Lärche` == -1 ~ -1,

              # proportion below 10% => category 0
              `Anteil Fichte, Kiefer oder Lärche` < 10 ~ 0,

              # proportion below 40% => category 1
              `Anteil Fichte, Kiefer oder Lärche` < 40 ~ 1,

              # proportion below 60% => category 2
              `Anteil Fichte, Kiefer oder Lärche` < 60 ~ 2,

              # proportion below 90% => category 3
              `Anteil Fichte, Kiefer oder Lärche` < 90 ~ 3,

              # proportion up to 100% => category 4
              `Anteil Fichte, Kiefer oder Lärche` <= 100 ~ 4
            )
          )

      }

      if ((Kiefer == Lärche) & (Kiefer > Fichte)) {
        # Kiefer and Lärche are equal, and Kiefer & Lärche are greater than Fichte
        # user value
        maxprop <- json_data$baumartenVerteilung_Ist$Kiefer

        Zielerreichung_Waldbauliches_Vergleichsbestand <- Zielerreichung_Waldbauliches_Vergleichsbestand %>%
          mutate(
            `Anteil Fichte, Kiefer oder Lärche` = case_when(
              `Anteil Fichte, Kiefer oder Lärche` == -1 ~ -1,
              Zeitpunkt_Maßnahme %in% massnahmen_fi_ki_lae ~ maxprop - 10,
              TRUE ~ maxprop
            ),

            # classify the proportion into categories (0–4)
            `Anteil Fichte, Kiefer oder Lärche` = case_when(
              # "Kein Wald" stays -1
              `Anteil Fichte, Kiefer oder Lärche` == -1 ~ -1,

              # proportion below 10% => category 0
              `Anteil Fichte, Kiefer oder Lärche` < 10 ~ 0,

              # proportion below 40% => category 1
              `Anteil Fichte, Kiefer oder Lärche` < 40 ~ 1,

              # proportion below 60% => category 2
              `Anteil Fichte, Kiefer oder Lärche` < 60 ~ 2,

              # proportion below 90% => category 3
              `Anteil Fichte, Kiefer oder Lärche` < 90 ~ 3,

              # proportion up to 100% => category 4
              `Anteil Fichte, Kiefer oder Lärche` <= 100 ~ 4
            )
          )

      }

      if ((Fichte == Kiefer) & (Fichte == Lärche)) {
        # All species are equal

        # user value
        maxprop <- json_data$baumartenVerteilung_Ist$Fichte

        Zielerreichung_Waldbauliches_Vergleichsbestand <- Zielerreichung_Waldbauliches_Vergleichsbestand %>%
          mutate(
            `Anteil Fichte, Kiefer oder Lärche` = case_when(
              # "Kein Wald" stays -1
              `Anteil Fichte, Kiefer oder Lärche` == -1 ~ -1,

              # for the relevant measures: maxprop - 10
              Zeitpunkt_Maßnahme %in% massnahmen_fi_ki_lae ~ maxprop - 10,

              # otherwise keep the original value
              TRUE ~ `Anteil Fichte, Kiefer oder Lärche`
            ),

            # classify the proportion into categories (0–4)
            `Anteil Fichte, Kiefer oder Lärche` = case_when(
              # "Kein Wald" stays -1
              `Anteil Fichte, Kiefer oder Lärche` == -1 ~ -1,

              # proportion below 10% => category 0
              `Anteil Fichte, Kiefer oder Lärche` < 10 ~ 0,

              # proportion below 40% => category 1
              `Anteil Fichte, Kiefer oder Lärche` < 40 ~ 1,

              # proportion below 60% => category 2
              `Anteil Fichte, Kiefer oder Lärche` < 60 ~ 2,

              # proportion below 90% => category 3
              `Anteil Fichte, Kiefer oder Lärche` < 90 ~ 3,

              # proportion up to 100% => category 4
              `Anteil Fichte, Kiefer oder Lärche` <= 100 ~ 4
            )
          )

      }

      # special cases / combined measures

      # _Fi_Ki
      # case 1: Fichte greater/equal Kiefer
      if (Fichte >= Kiefer) {

        # user value
        spruce_percent <- json_data$baumartenVerteilung_Ist$Fichte

        Zielerreichung_Waldbauliches_Vergleichsbestand <- Zielerreichung_Waldbauliches_Vergleichsbestand %>%
          mutate(
            Ips_Fichtenanteil = case_when(
              Zeitpunkt_Maßnahme %in% massnahmen_fi_ki ~ spruce_percent - 10,
              TRUE ~ Ips_Fichtenanteil
            ),

            Ips_Fichtenanteil = case_when(
              Zeitpunkt_Maßnahme %in% massnahmen_fi_ki & Ips_Fichtenanteil >= -10 & Ips_Fichtenanteil <= 10 ~ 0,

              Zeitpunkt_Maßnahme %in% massnahmen_fi_ki & Ips_Fichtenanteil >= 11 & Ips_Fichtenanteil < 40 ~ 1,

              Zeitpunkt_Maßnahme %in% massnahmen_fi_ki & Ips_Fichtenanteil >= 40 & Ips_Fichtenanteil <= 60 ~ 2,

              Zeitpunkt_Maßnahme %in% massnahmen_fi_ki & Ips_Fichtenanteil > 60 & Ips_Fichtenanteil < 90 ~ 3,

              Zeitpunkt_Maßnahme %in% massnahmen_fi_ki & Ips_Fichtenanteil >= 90 & Ips_Fichtenanteil <= 100 ~ 4,

              TRUE ~ Ips_Fichtenanteil
            )
          )

      }

      # case 2: Kiefer greater than Fichte
      if (Kiefer > Fichte) {

        # user value
        pine_percent <- json_data$baumartenVerteilung_Ist$Kiefer

        Zielerreichung_Waldbauliches_Vergleichsbestand <- Zielerreichung_Waldbauliches_Vergleichsbestand %>%
          mutate(
            Kiefernanteil = case_when(
              Zeitpunkt_Maßnahme %in% massnahmen_fi_ki ~ pine_percent - 10,
              TRUE ~ Kiefernanteil
            ),

            Kiefernanteil = case_when(
              Zeitpunkt_Maßnahme %in% massnahmen_fi_ki & Kiefernanteil == -1 ~ -1,

              Zeitpunkt_Maßnahme %in% massnahmen_fi_ki & Kiefernanteil < 10 ~ 0,

              Zeitpunkt_Maßnahme %in% massnahmen_fi_ki & Kiefernanteil < 40 ~ 1,

              Zeitpunkt_Maßnahme %in% massnahmen_fi_ki & Kiefernanteil < 60 ~ 2,

              Zeitpunkt_Maßnahme %in% massnahmen_fi_ki & Kiefernanteil < 90 ~ 3,

              Zeitpunkt_Maßnahme %in% massnahmen_fi_ki & Kiefernanteil <= 100 ~ 4,

              TRUE ~ Kiefernanteil
            )
          )

      }

      # _Fi_Ki_Lä, spruce - 10% if Fi > Ki and Fi > Lä etc., same principle as above. new, +/- logical consequence of point 2.

      if ((Fichte > Kiefer) & (Fichte > Lärche)) {
        # Fichte is greater than both Kiefer and Lärche
        # Fichte

        # user value
        spruce_percent <- json_data$baumartenVerteilung_Ist$Fichte

        Zielerreichung_Waldbauliches_Vergleichsbestand <- Zielerreichung_Waldbauliches_Vergleichsbestand %>%
          mutate(
            Ips_Fichtenanteil = case_when(
              Zeitpunkt_Maßnahme %in% massnahmen_fi_ki_lae ~ spruce_percent - 10,
              TRUE ~ Ips_Fichtenanteil
            ),

            Ips_Fichtenanteil = case_when(
              Zeitpunkt_Maßnahme %in% massnahmen_fi_ki_lae & Ips_Fichtenanteil >= -10 & Ips_Fichtenanteil <= 10 ~ 0,

              Zeitpunkt_Maßnahme %in% massnahmen_fi_ki_lae & Ips_Fichtenanteil >= 11 & Ips_Fichtenanteil < 40 ~ 1,

              Zeitpunkt_Maßnahme %in% massnahmen_fi_ki_lae & Ips_Fichtenanteil >= 40 & Ips_Fichtenanteil <= 60 ~ 2,

              Zeitpunkt_Maßnahme %in% massnahmen_fi_ki_lae & Ips_Fichtenanteil > 60 & Ips_Fichtenanteil < 90 ~ 3,

              Zeitpunkt_Maßnahme %in% massnahmen_fi_ki_lae & Ips_Fichtenanteil >= 90 & Ips_Fichtenanteil <= 100 ~ 4,

              TRUE ~ Ips_Fichtenanteil
            )
          )

      }

      if ((Kiefer > Fichte) & (Kiefer > Lärche)) {
        # Kiefer is greater than both Fichte and Lärche
        # Kiefer

        # user value
        pine_percent <- json_data$baumartenVerteilung_Ist$Kiefer

        Zielerreichung_Waldbauliches_Vergleichsbestand <- Zielerreichung_Waldbauliches_Vergleichsbestand %>%
          mutate(
            Kiefernanteil = case_when(
              Zeitpunkt_Maßnahme %in% massnahmen_fi_ki_lae ~ pine_percent - 10,
              TRUE ~ Kiefernanteil
            ),

            Kiefernanteil = case_when(
              Zeitpunkt_Maßnahme %in% massnahmen_fi_ki_lae & Kiefernanteil == -1 ~ -1,

              Zeitpunkt_Maßnahme %in% massnahmen_fi_ki_lae & Kiefernanteil < 10 ~ 0,

              Zeitpunkt_Maßnahme %in% massnahmen_fi_ki_lae & Kiefernanteil < 40 ~ 1,

              Zeitpunkt_Maßnahme %in% massnahmen_fi_ki_lae & Kiefernanteil < 60 ~ 2,

              Zeitpunkt_Maßnahme %in% massnahmen_fi_ki_lae & Kiefernanteil < 90 ~ 3,

              Zeitpunkt_Maßnahme %in% massnahmen_fi_ki_lae & Kiefernanteil <= 100 ~ 4,

              TRUE ~ Kiefernanteil
            )
          )

      }

      if ((Lärche > Fichte) & (Lärche > Kiefer)) {
        # Lärche is greater than both Fichte and Kiefer
        # Larch

        # user value
        larch_percent <- json_data$baumartenVerteilung_Ist$Lärche

        Zielerreichung_Waldbauliches_Vergleichsbestand <- Zielerreichung_Waldbauliches_Vergleichsbestand %>%
          mutate(
            Lärchenanteil = case_when(
              Zeitpunkt_Maßnahme %in% massnahmen_fi_ki_lae ~ larch_percent - 10,
              TRUE ~ Lärchenanteil
            ),

            Lärchenanteil = case_when(
              Zeitpunkt_Maßnahme %in% massnahmen_fi_ki_lae & Lärchenanteil == -1 ~ -1,

              Zeitpunkt_Maßnahme %in% massnahmen_fi_ki_lae & Lärchenanteil < 10 ~ 0,

              Zeitpunkt_Maßnahme %in% massnahmen_fi_ki_lae & Lärchenanteil < 40 ~ 1,

              Zeitpunkt_Maßnahme %in% massnahmen_fi_ki_lae & Lärchenanteil < 60 ~ 2,

              Zeitpunkt_Maßnahme %in% massnahmen_fi_ki_lae & Lärchenanteil < 90 ~ 3,

              Zeitpunkt_Maßnahme %in% massnahmen_fi_ki_lae & Lärchenanteil <= 100 ~ 4,

              TRUE ~ Lärchenanteil
            )
          )

      }

      if ((Fichte == Kiefer) & (Fichte > Lärche)) {
        # Fichte and Kiefer are equal, and Fichte & Kiefer are greater than Lärche
        # Fichte

        # user value
        spruce_percent <- json_data$baumartenVerteilung_Ist$Fichte

        Zielerreichung_Waldbauliches_Vergleichsbestand <- Zielerreichung_Waldbauliches_Vergleichsbestand %>%
          mutate(
            Ips_Fichtenanteil = case_when(
              Zeitpunkt_Maßnahme %in% massnahmen_fi_ki_lae ~ spruce_percent - 10,
              TRUE ~ Ips_Fichtenanteil
            ),

            Ips_Fichtenanteil = case_when(
              Zeitpunkt_Maßnahme %in% massnahmen_fi_ki_lae & Ips_Fichtenanteil >= -10 & Ips_Fichtenanteil <= 10 ~ 0,

              Zeitpunkt_Maßnahme %in% massnahmen_fi_ki_lae & Ips_Fichtenanteil >= 11 & Ips_Fichtenanteil < 40 ~ 1,

              Zeitpunkt_Maßnahme %in% massnahmen_fi_ki_lae & Ips_Fichtenanteil >= 40 & Ips_Fichtenanteil <= 60 ~ 2,

              Zeitpunkt_Maßnahme %in% massnahmen_fi_ki_lae & Ips_Fichtenanteil > 60 & Ips_Fichtenanteil < 90 ~ 3,

              Zeitpunkt_Maßnahme %in% massnahmen_fi_ki_lae & Ips_Fichtenanteil >= 90 & Ips_Fichtenanteil <= 100 ~ 4,

              TRUE ~ Ips_Fichtenanteil
            )
          )

      }

      if ((Fichte == Lärche) & (Fichte > Kiefer)) {
        # Fichte and Lärche are equal, and Fichte & Lärche are greater than Kiefer
        # Fichte

        # user value
        spruce_percent <- json_data$baumartenVerteilung_Ist$Fichte

        Zielerreichung_Waldbauliches_Vergleichsbestand <- Zielerreichung_Waldbauliches_Vergleichsbestand %>%
          mutate(
            Ips_Fichtenanteil = case_when(
              Zeitpunkt_Maßnahme %in% massnahmen_fi_ki_lae ~ spruce_percent - 10,
              TRUE ~ Ips_Fichtenanteil
            ),

            Ips_Fichtenanteil = case_when(
              Zeitpunkt_Maßnahme %in% massnahmen_fi_ki_lae & Ips_Fichtenanteil >= -10 & Ips_Fichtenanteil <= 10 ~ 0,

              Zeitpunkt_Maßnahme %in% massnahmen_fi_ki_lae & Ips_Fichtenanteil >= 11 & Ips_Fichtenanteil < 40 ~ 1,

              Zeitpunkt_Maßnahme %in% massnahmen_fi_ki_lae & Ips_Fichtenanteil >= 40 & Ips_Fichtenanteil <= 60 ~ 2,

              Zeitpunkt_Maßnahme %in% massnahmen_fi_ki_lae & Ips_Fichtenanteil > 60 & Ips_Fichtenanteil < 90 ~ 3,

              Zeitpunkt_Maßnahme %in% massnahmen_fi_ki_lae & Ips_Fichtenanteil >= 90 & Ips_Fichtenanteil <= 100 ~ 4,

              TRUE ~ Ips_Fichtenanteil
            )
          )

      }

      if ((Kiefer == Lärche) & (Kiefer > Fichte)) {
        # Kiefer and Lärche are equal, and Kiefer & Lärche are greater than Fichte
        # Kiefer

        # user value
        pine_percent <- json_data$baumartenVerteilung_Ist$Kiefer

        Zielerreichung_Waldbauliches_Vergleichsbestand <- Zielerreichung_Waldbauliches_Vergleichsbestand %>%
          mutate(
            Kiefernanteil = case_when(
              Zeitpunkt_Maßnahme %in% massnahmen_fi_ki_lae ~ pine_percent - 10,
              TRUE ~ Kiefernanteil
            ),

            Kiefernanteil = case_when(
              Kiefernanteil == -1 ~ -1,

              Zeitpunkt_Maßnahme %in% massnahmen_fi_ki_lae & Kiefernanteil < 10 ~ 0,

              Zeitpunkt_Maßnahme %in% massnahmen_fi_ki_lae & Kiefernanteil < 40 ~ 1,

              Zeitpunkt_Maßnahme %in% massnahmen_fi_ki_lae & Kiefernanteil < 60 ~ 2,

              Zeitpunkt_Maßnahme %in% massnahmen_fi_ki_lae & Kiefernanteil < 90 ~ 3,

              Zeitpunkt_Maßnahme %in% massnahmen_fi_ki_lae & Kiefernanteil <= 100 ~ 4,

              TRUE ~ Kiefernanteil
            )
          )

      }

      if ((Fichte == Kiefer) & (Fichte == Lärche)) {
        # All species are equal
        # Fichte

        # user value
        spruce_percent <- json_data$baumartenVerteilung_Ist$Fichte

        Zielerreichung_Waldbauliches_Vergleichsbestand <- Zielerreichung_Waldbauliches_Vergleichsbestand %>%
          mutate(
            Ips_Fichtenanteil = case_when(
              Zeitpunkt_Maßnahme %in% massnahmen_fi_ki_lae ~ spruce_percent - 10,
              TRUE ~ Ips_Fichtenanteil
            ),

            Ips_Fichtenanteil = case_when(
              Zeitpunkt_Maßnahme %in% massnahmen_fi_ki_lae & Ips_Fichtenanteil >= -10 & Ips_Fichtenanteil <= 10 ~ 0,

              Zeitpunkt_Maßnahme %in% massnahmen_fi_ki_lae & Ips_Fichtenanteil >= 11 & Ips_Fichtenanteil < 40 ~ 1,

              Zeitpunkt_Maßnahme %in% massnahmen_fi_ki_lae & Ips_Fichtenanteil >= 40 & Ips_Fichtenanteil <= 60 ~ 2,

              Zeitpunkt_Maßnahme %in% massnahmen_fi_ki_lae & Ips_Fichtenanteil > 60 & Ips_Fichtenanteil < 90 ~ 3,

              Zeitpunkt_Maßnahme %in% massnahmen_fi_ki_lae & Ips_Fichtenanteil >= 90 & Ips_Fichtenanteil <= 100 ~ 4,

              TRUE ~ Ips_Fichtenanteil
            )
          )

      }

      # if Fi / Ki / Lä == 100%, or Fi / Ki / Lä is the only conifer species, then apply -10% on the corresponding variable for _NH as well, in addition to the change of the NH proportion, also reducing maxproportion

      if (Fichte == nh_percent) {

        # user value
        spruce_percent <- json_data$baumartenVerteilung_Ist$Fichte

        Zielerreichung_Waldbauliches_Vergleichsbestand <- Zielerreichung_Waldbauliches_Vergleichsbestand %>%
          mutate(
            Ips_Fichtenanteil = case_when(
              Zeitpunkt_Maßnahme %in% massnahmen_nh_only ~ spruce_percent - 10,
              TRUE ~ Ips_Fichtenanteil
            ),

            Ips_Fichtenanteil = case_when(
              Zeitpunkt_Maßnahme %in% massnahmen_nh_only & Ips_Fichtenanteil >= -10 & Ips_Fichtenanteil <= 10 ~ 0,

              Zeitpunkt_Maßnahme %in% massnahmen_nh_only & Ips_Fichtenanteil >= 11 & Ips_Fichtenanteil < 40 ~ 1,

              Zeitpunkt_Maßnahme %in% massnahmen_nh_only & Ips_Fichtenanteil >= 40 & Ips_Fichtenanteil <= 60 ~ 2,

              Zeitpunkt_Maßnahme %in% massnahmen_nh_only & Ips_Fichtenanteil > 60 & Ips_Fichtenanteil < 90 ~ 3,

              Zeitpunkt_Maßnahme %in% massnahmen_nh_only & Ips_Fichtenanteil >= 90 & Ips_Fichtenanteil <= 100 ~ 4,

              TRUE ~ Ips_Fichtenanteil
            )
          )

        # user value
        maxprop <- json_data$baumartenVerteilung_Ist$Fichte

        Zielerreichung_Waldbauliches_Vergleichsbestand <- Zielerreichung_Waldbauliches_Vergleichsbestand %>%
          mutate(
            `Anteil Fichte, Kiefer oder Lärche` = case_when(
              `Anteil Fichte, Kiefer oder Lärche` == -1 ~ -1,
              Zeitpunkt_Maßnahme %in% massnahmen_nh_only ~ maxprop - 10,
              TRUE ~ `Anteil Fichte, Kiefer oder Lärche`
            ),

            `Anteil Fichte, Kiefer oder Lärche` = case_when(
              `Anteil Fichte, Kiefer oder Lärche` == -1 ~ -1,
              Zeitpunkt_Maßnahme %in% massnahmen_nh_only & `Anteil Fichte, Kiefer oder Lärche` < 10 ~ 0,
              Zeitpunkt_Maßnahme %in% massnahmen_nh_only & `Anteil Fichte, Kiefer oder Lärche` < 40 ~ 1,
              Zeitpunkt_Maßnahme %in% massnahmen_nh_only & `Anteil Fichte, Kiefer oder Lärche` < 60 ~ 2,
              Zeitpunkt_Maßnahme %in% massnahmen_nh_only & `Anteil Fichte, Kiefer oder Lärche` < 90 ~ 3,
              Zeitpunkt_Maßnahme %in% massnahmen_nh_only & `Anteil Fichte, Kiefer oder Lärche` <= 100 ~ 4,
              TRUE ~ `Anteil Fichte, Kiefer oder Lärche`
            )
          )
      }

      if (Kiefer == nh_percent) {

        # user value
        spruce_percent <- json_data$baumartenVerteilung_Ist$Kiefer

        Zielerreichung_Waldbauliches_Vergleichsbestand <- Zielerreichung_Waldbauliches_Vergleichsbestand %>%
          mutate(
            Kiefernanteil = case_when(
              Zeitpunkt_Maßnahme %in% massnahmen_nh_only ~ spruce_percent - 10,
              TRUE ~ Kiefernanteil
            ),

            Kiefernanteil = case_when(
              Kiefernanteil == -1 ~ -1,
              Zeitpunkt_Maßnahme %in% massnahmen_nh_only & Kiefernanteil < 10 ~ 0,
              Zeitpunkt_Maßnahme %in% massnahmen_nh_only & Kiefernanteil < 40 ~ 1,
              Zeitpunkt_Maßnahme %in% massnahmen_nh_only & Kiefernanteil < 60 ~ 2,
              Zeitpunkt_Maßnahme %in% massnahmen_nh_only & Kiefernanteil < 90 ~ 3,
              Zeitpunkt_Maßnahme %in% massnahmen_nh_only & Kiefernanteil <= 100 ~ 4,
              TRUE ~ Kiefernanteil
            )
          )

        # user value
        maxprop <- json_data$baumartenVerteilung_Ist$Kiefer

        Zielerreichung_Waldbauliches_Vergleichsbestand <- Zielerreichung_Waldbauliches_Vergleichsbestand %>%
          mutate(
            `Anteil Fichte, Kiefer oder Lärche` = case_when(
              `Anteil Fichte, Kiefer oder Lärche` == -1 ~ -1,
              Zeitpunkt_Maßnahme %in% massnahmen_nh_only ~ maxprop - 10,
              TRUE ~ `Anteil Fichte, Kiefer oder Lärche`
            ),

            `Anteil Fichte, Kiefer oder Lärche` = case_when(
              `Anteil Fichte, Kiefer oder Lärche` == -1 ~ -1,
              Zeitpunkt_Maßnahme %in% massnahmen_nh_only & `Anteil Fichte, Kiefer oder Lärche` < 10 ~ 0,
              Zeitpunkt_Maßnahme %in% massnahmen_nh_only & `Anteil Fichte, Kiefer oder Lärche` < 40 ~ 1,
              Zeitpunkt_Maßnahme %in% massnahmen_nh_only & `Anteil Fichte, Kiefer oder Lärche` < 60 ~ 2,
              Zeitpunkt_Maßnahme %in% massnahmen_nh_only & `Anteil Fichte, Kiefer oder Lärche` < 90 ~ 3,
              Zeitpunkt_Maßnahme %in% massnahmen_nh_only & `Anteil Fichte, Kiefer oder Lärche` <= 100 ~ 4,
              TRUE ~ `Anteil Fichte, Kiefer oder Lärche`
            )
          )
      }

      if (Lärche == nh_percent) {

        # user value
        larch_percent <- json_data$baumartenVerteilung_Ist$Lärche

        Zielerreichung_Waldbauliches_Vergleichsbestand <- Zielerreichung_Waldbauliches_Vergleichsbestand %>%
          mutate(
            Lärchenanteil = case_when(
              Zeitpunkt_Maßnahme %in% massnahmen_nh_only ~ larch_percent - 10,
              TRUE ~ Lärchenanteil
            ),

            Lärchenanteil = case_when(
              Lärchenanteil == -1 ~ -1,
              Zeitpunkt_Maßnahme %in% massnahmen_nh_only & Lärchenanteil < 10 ~ 0,
              Zeitpunkt_Maßnahme %in% massnahmen_nh_only & Lärchenanteil < 40 ~ 1,
              Zeitpunkt_Maßnahme %in% massnahmen_nh_only & Lärchenanteil < 60 ~ 2,
              Zeitpunkt_Maßnahme %in% massnahmen_nh_only & Lärchenanteil < 90 ~ 3,
              Zeitpunkt_Maßnahme %in% massnahmen_nh_only & Lärchenanteil <= 100 ~ 4,
              TRUE ~ Lärchenanteil
            )
          )

        # user value
        maxprop <- json_data$baumartenVerteilung_Ist$Lärche

        Zielerreichung_Waldbauliches_Vergleichsbestand <- Zielerreichung_Waldbauliches_Vergleichsbestand %>%
          mutate(
            `Anteil Fichte, Kiefer oder Lärche` = case_when(
              `Anteil Fichte, Kiefer oder Lärche` == -1 ~ -1,
              Zeitpunkt_Maßnahme %in% massnahmen_nh_only ~ maxprop - 10,
              TRUE ~ `Anteil Fichte, Kiefer oder Lärche`
            ),

            `Anteil Fichte, Kiefer oder Lärche` = case_when(
              `Anteil Fichte, Kiefer oder Lärche` == -1 ~ -1,
              Zeitpunkt_Maßnahme %in% massnahmen_nh_only & `Anteil Fichte, Kiefer oder Lärche` < 10 ~ 0,
              Zeitpunkt_Maßnahme %in% massnahmen_nh_only & `Anteil Fichte, Kiefer oder Lärche` < 40 ~ 1,
              Zeitpunkt_Maßnahme %in% massnahmen_nh_only & `Anteil Fichte, Kiefer oder Lärche` < 60 ~ 2,
              Zeitpunkt_Maßnahme %in% massnahmen_nh_only & `Anteil Fichte, Kiefer oder Lärche` < 90 ~ 3,
              Zeitpunkt_Maßnahme %in% massnahmen_nh_only & `Anteil Fichte, Kiefer oder Lärche` <= 100 ~ 4,
              TRUE ~ `Anteil Fichte, Kiefer oder Lärche`
            )
          )
      }

    }
  }, error = function(e) {
    return(e$message)
  } )

# Tree-species suitability -----------------------------------------------------

require(tidyverse)
# 1. build the data frames for tree-species suitability from the geodata and the species present in the user data

BA_Eignung_Var_Name <- c("Standorteignung Bergahorn",
                         "Standorteignung Birke",
                         "Standorteignung Buche",
                         "Standorteignung Bergulme",
                         "Standorteignung Douglasie",
                         "Standorteignung Esche",
                         "Standorteignung FichteBorkenkäfer",
                         "Standorteignung Fichte",
                         "Standorteignung Hainbuche",
                         "Standorteignung Kiefer",
                         "Standorteignung Kirsche",
                         "Standorteignung Lärche",
                         "Standorteignung Roteiche",
                         "Standorteignung Sommerlinde",
                         "Standorteignung Stieleiche",
                         "Standorteignung Tanne",
                         "Standorteignung Traubeneiche",
                         "Standorteignung Winterlinde",
                         "Standorteignung Zirbe")

# create a new df by filtering the json_data which consists of the tree suitability variables
BA_Eignung_Polygon <- json_data$areaDetail %>%
  filter(indikatorLabel %in% BA_Eignung_Var_Name) %>%
  select(c(indikatorLabel, mean) #%>% rename(Standorteignung = mean)
  )

# remove the word "Standorteignung"
BA_Eignung_Polygon <- BA_Eignung_Polygon %>% sapply(function(text) sub("Standorteignung ", "", text)) %>% as.data.frame()
BA_Eignung_Polygon <- BA_Eignung_Polygon %>% rename(Baumart = indikatorLabel)

# Create the data frame for the existing tree species
BA_Eignung_df <- data.frame(Baumart = names(json_data$baumartenVerteilung_Ist),  # from user data, Baumartenverteilung_IST
                            Anteil = sapply(json_data$baumartenVerteilung_Ist, function(x){as.numeric(x[1])}) )

BA_Eignung_df <- left_join(BA_Eignung_df, BA_Eignung_Polygon, by = "Baumart")
BA_Eignung_df <- BA_Eignung_df %>% rename(Standorteignung = "mean")

# 2.
## Processing for calculation and display

BA_Eignung_df <- BA_Eignung_df %>%
  mutate(
    dominant = ifelse(Anteil > 60, 1, 0),                          # Kontrolle, ob BA dominant
    weight = ifelse(dominant == 1, 2, 1),                          # if dominant, then weighted 2x
    label = paste0("Standorteignung ", Baumart, ""),               # label, for output to the user
    standorteignung_reclassified = case_when(                      # (possibly needed for the traffic-light indicators)
      Standorteignung >= 0 & Standorteignung < 50 ~ 0.3,
      Standorteignung >= 50 & Standorteignung < 80 ~ 0.7,
      Standorteignung >= 80 & Standorteignung <= 100 ~ 1
    )
  ) %>%
  filter(Anteil != 0) %>%                                          # ignore tree species with proportion == 0
  arrange(desc(Anteil)) %>%
  mutate(name = ifelse(dominant == 1, "BA_Eignung_dom", paste0("BA_Eignung_", row_number())))  # name (dom/1, 2, 3, 4, 5 etc.) for later access, e.g. traffic-light script

# 3.
# compute the weighted BA_Eignung
BA_Eignung_df <- BA_Eignung_df %>% filter(is.na(standorteignung_reclassified) == FALSE ) # exclude NA's just in case
BA_Eignung_df$standorteignung_reclassified <- as.numeric(BA_Eignung_df$standorteignung_reclassified)
BA_Eignung_weighted <- BA_Eignung_df %>%
  summarise(BA_Eignung_weighted = sum(standorteignung_reclassified * weight) / sum(weight)) %>%
  pull(BA_Eignung_weighted)
# BA_Eignung_weighted is now the value we continue with, i.e. the one used for the SRH computation

Zielerreichung_Waldbauliches_Vergleichsbestand$BA_Eignung_weighted <- BA_Eignung_weighted

## Rename columns back to original names
rename_back_vector <- setNames(names(rename_vector), rename_vector)

Zielerreichung_Waldbauliches_Vergleichsbestand <- Zielerreichung_Waldbauliches_Vergleichsbestand %>%
  rename_with(~ rename_back_vector[.], .cols = any_of(names(rename_back_vector)))

# Clean up
rm(rename_vector, rename_back_vector, renaming_template)

# Create a DF-List with all actions and Vars
require(tidyverse)
Zielerreichung_list <- Zielerreichung_Waldbauliches_Vergleichsbestand %>% select(!Zeitpunkt_Maßnahme)
names <- Zielerreichung_Waldbauliches_Vergleichsbestand %>% select(Zeitpunkt_Maßnahme)

# Save the actions as new ones in a new List DF
Zielerreichung_list  <- transpose(Zielerreichung_list, .names = names$Zeitpunkt_Maßnahme)

Zielerreichung_Waldbauliches_Vergleichsbestand1 <- Zielerreichung_Waldbauliches_Vergleichsbestand

# Start PAS loops --------------------------------------------------------------

# Runs one PAS backend script across the accepted Wuchsklasse/Maßnahme
# scenarios: for each scenario, sets its variables into .GlobalEnv (as every
# pas_*_backend_SERVER.R script expects), source()s the script, and collects
# the requested res_* outputs into one named list per output - exactly the
# accumulation the 11 hand-duplicated copies of this loop used to do
# (including the append-then-rename step and the trailing keep() filter),
# just parameterized instead of repeated per PAS type.
run_pas_scenario_loop <- function(scenario_range, script_path, outputs) {
  # outputs: named character vector; names become the result list's names,
  # values are the res_* variable names to read after sourcing, e.g.
  # c(site = "res_browsing_site", stand = "res_browsing_stand")
  result <- setNames(vector("list", length(outputs)), names(outputs))
  for (nm in names(result)) result[[nm]] <- list()

  for (i in scenario_range) {
    scenario_name <- names(Zielerreichung_list)[i]
    scenario <- Zielerreichung_list[[scenario_name]]
    for (k in 1:length(scenario)) {
      variable_name <- names(scenario)[k]
      variable_value <- scenario[[k]]
      assign(variable_name, variable_value, envir = .GlobalEnv)
    }
    suppressWarnings(source(script_path))
    # variable names in Zielerreichung_list must match the names in the backend script
    for (nm in names(outputs)) {
      val <- get(outputs[[nm]], envir = .GlobalEnv)
      result[[nm]] <- append(result[[nm]], val, after = length(result[[nm]]))
      names(result[[nm]])[length(result[[nm]])] <- scenario_name
    }
  }

  ## the scenarios contain duplicates; remove the ones that are not needed
  for (nm in names(result)) {
    result[[nm]] <- result[[nm]] %>% keep(names(result[[nm]]) %in% names$Zeitpunkt_Maßnahme)
  }
  result
}

if("erhöh_widerstand" %in% json_data$ziel) {

scenario_range_1_10 <- 1:length(unique(names(Zielerreichung_list)))

# Browsing ---------------------------------------------------------------------

tryCatch({
  res <- run_pas_scenario_loop(
    scenario_range_1_10,
    "insert path to PAS scripts directory/pas_1_browsing_backend_SERVER.R",
    c(site = "res_browsing_site", stand = "res_browsing_stand", combined = "res_browsing_combined",
      site_win = "res_browsing_site_win", combined_win = "res_browsing_combined_win")
  )
  PAS1_list_browsing_site <- res$site
  PAS1_list_browsing_stand <- res$stand
  PAS1_list_browsing_combined <- res$combined
  PAS1_list_browsing_site_win <- res$site_win
  PAS1_list_browsing_combined_win <- res$combined_win
}, error = function(e) {
  message("Error in PAS 1 Browsing loop: ", e$message)
})

# Bark stripping ---------------------------------------------------------------

tryCatch({
  res <- run_pas_scenario_loop(
    scenario_range_1_10,
    "insert path to PAS scripts directory/pas_2_barkstripping_backend_SERVER.R",
    c(site = "res_barkstripping_site", stand = "res_barkstripping_stand", combined = "res_barkstripping_combined")
  )
  PAS2_list_barkstripping_site <- res$site
  PAS2_list_barkstripping_stand <- res$stand
  PAS2_list_barkstripping_combined <- res$combined
}, error = function(e) {
  message("Error in PAS 2 Barkstripping loop: ", e$message)
})

# Cembrae ----------------------------------------------------------------------

tryCatch({
  res <- run_pas_scenario_loop(
    scenario_range_1_10,
    "insert path to PAS scripts directory/pas_3_cembrae_backend_SERVER.R",
    c(site = "res_cembrae_site", stand = "res_cembrae_stand", combined = "res_cembrae_combined")
  )
  PAS3_list_cembrae_site <- res$site
  PAS3_list_cembrae_stand <- res$stand
  PAS3_list_cembrae_combined <- res$combined
}, error = function(e) {
  message("Error in PAS 3 Cembrae loop: ", e$message)
})

# Armillaria -------------------------------------------------------------------

tryCatch({
  res <- run_pas_scenario_loop(
    scenario_range_1_10,
    "insert path to PAS scripts directory/pas_4_armillaria_backend_SERVER.R",
    c(site = "res_armillaria_site", stand = "res_armillaria_stand", combined = "res_armillaria_combined")
  )
  PAS4_list_armillaria_site <- res$site
  PAS4_list_armillaria_stand <- res$stand
  PAS4_list_armillaria_combined <- res$combined
}, error = function(e) {
  message("Error in PAS 4 Armillaria loop: ", e$message)
})

# Bark-breeding beetles --------------------------------------------------------

tryCatch({
  res <- run_pas_scenario_loop(
    scenario_range_1_10,
    "insert path to PAS scripts directory/pas_5_barkbreeding_backend_SERVER.R",
    c(site = "res_barkbreeding_site", stand = "res_barkbreeding_stand", combined = "res_barkbreeding_combined")
  )
  PAS5_list_barkbreeding_site <- res$site
  PAS5_list_barkbreeding_stand <- res$stand
  PAS5_list_barkbreeding_combined <- res$combined
}, error = function(e) {
  message("Error in PAS 5 Barkbr loop: ", e$message)
})

# Heterobasidion ---------------------------------------------------------------

tryCatch({
  res <- run_pas_scenario_loop(
    scenario_range_1_10,
    "insert path to PAS scripts directory/pas_6_heterobasidion_backend_SERVER.R",
    c(site = "res_heterobasidion_site", stand = "res_heterobasidion_stand", combined = "res_heterobasidion_combined")
  )
  PAS6_list_heterobasidion_site <- res$site
  PAS6_list_heterobasidion_stand <- res$stand
  PAS6_list_heterobasidion_combined <- res$combined
}, error = function(e) {
  message("Error in PAS 6 HetBas loop: ", e$message)
})

# Fire -------------------------------------------------------------------------

tryCatch({
  res <- run_pas_scenario_loop(
    scenario_range_1_10,
    "insert path to PAS scripts directory/pas_7_fire_backend_SERVER.R",
    c(site = "res_fire_site", stand = "res_fire_stand", combined = "res_fire_combined")
  )
  PAS7_list_fire_site <- res$site
  PAS7_list_fire_stand <- res$stand
  PAS7_list_fire_combined <- res$combined
}, error = function(e) {
  message("Error in PAS 7 Fire loop: ", e$message)
})

# Storm ------------------------------------------------------------------------

tryCatch({
  res <- run_pas_scenario_loop(
    scenario_range_1_10,
    "insert path to PAS scripts directory/pas_8_storm_backend_SERVER.R",
    c(site = "res_storm_site", stand = "res_storm_stand", combined = "res_storm_combined")
  )
  PAS8_list_storm_site <- res$site
  PAS8_list_storm_stand <- res$stand
  PAS8_list_storm_combined <- res$combined
}, error = function(e) {
  message("Error in PAS 8 Storm loop: ", e$message)
})

# Snow -------------------------------------------------------------------------
tryCatch({
  res <- run_pas_scenario_loop(
    scenario_range_1_10,
    "insert path to PAS scripts directory/pas_9_snow_backend_SERVER.R",
    c(site = "res_snow_site", stand = "res_snow_stand", combined = "res_snow_combined")
  )
  PAS9_list_snow_site <- res$site
  PAS9_list_snow_stand <- res$stand
  PAS9_list_snow_combined <- res$combined
}, error = function(e) {
  message("Error in PAS 9 Snow loop: ", e$message)
})

# Ips --------------------------------------------------------------------------

tryCatch({
  res <- run_pas_scenario_loop(
    scenario_range_1_10,
    "insert path to PAS scripts directory/pas_10_barkbeetle_backend_SERVER.R",
    c(site = "res_ips_site", stand = "res_ips_stand", combined = "res_ips_combined")
  )
  PAS10_list_ips_site <- res$site
  PAS10_list_ips_stand <- res$stand
  PAS10_list_ips_combined <- res$combined
}, error = function(e) {
  message("Error in PAS 10 Ips loop: ", e$message)
})

rm(res, scenario_range_1_10)

}

# Capercaillie calculation -----------------------------------------------------

if("habverb" %in% json_data$ziel){

res <- run_pas_scenario_loop(
  2:length(unique(names(Zielerreichung_list))),
  "insert path to PAS scripts directory/pas_11_capercaillie_backend_SERVER.R",
  c(sum = "res_capercaillie_sum", win = "res_capercaillie_win", combined = "res_capercaillie_combined")
)
PAS11_list_capercaillie_sum <- res$sum
PAS11_list_capercaillie_win <- res$win
PAS11_list_capercaillie_combined <- res$combined
rm(res)

}

# Collect the PAS results ------------------------------------------------------

if("erhöh_widerstand"  %in% json_data$ziel){
# make one list of all
library(purrr)
library(dplyr)

# Define a function to process each list
process_list <- function(PAS_name, list_data) {
  # Convert the list or data frame into a data frame
  df <- as.data.frame(list_data)

  # Add the list name as the first column
  df <- df %>%
    mutate(PAS_name = PAS_name) %>%
    relocate(PAS_name) # Move ListName to the first column

  return(df)
}

# Create a named list of all your lists
PAS_lists <- list(
  PAS1_list_browsing_combined = PAS1_list_browsing_combined,
  PAS1_list_browsing_combined_win = PAS1_list_browsing_combined_win,
  PAS1_list_browsing_site = PAS1_list_browsing_site,
  PAS1_list_browsing_site_win = PAS1_list_browsing_site_win,
  PAS1_list_browsing_stand = PAS1_list_browsing_stand,

  PAS2_list_barkstripping_combined = PAS2_list_barkstripping_combined,
  PAS2_list_barkstripping_site = PAS2_list_barkstripping_site,
  PAS2_list_barkstripping_stand = PAS2_list_barkstripping_stand,

  PAS3_list_cembrae_combined = PAS3_list_cembrae_combined,
  PAS3_list_cembrae_site = PAS3_list_cembrae_site,
  PAS3_list_cembrae_stand = PAS3_list_cembrae_stand,

  PAS4_list_armillaria_combined = PAS4_list_armillaria_combined,
  PAS4_list_armillaria_site = PAS4_list_armillaria_site,
  PAS4_list_armillaria_stand = PAS4_list_armillaria_stand,

  PAS5_list_barkbreeding_combined = PAS5_list_barkbreeding_combined,
  PAS5_list_barkbreeding_site = PAS5_list_barkbreeding_site,
  PAS5_list_barkbreeding_stand = PAS5_list_barkbreeding_stand,

  PAS6_list_heterobasidion_combined = PAS6_list_heterobasidion_combined,
  PAS6_list_heterobasidion_site = PAS6_list_heterobasidion_site,
  PAS6_list_heterobasidion_stand = PAS6_list_heterobasidion_stand,

  PAS7_list_fire_combined = PAS7_list_fire_combined,
  PAS7_list_fire_site = PAS7_list_fire_site,
  PAS7_list_fire_stand = PAS7_list_fire_stand,

  PAS8_list_storm_combined = PAS8_list_storm_combined,
  PAS8_list_storm_site = PAS8_list_storm_site,
  PAS8_list_storm_stand = PAS8_list_storm_stand,

  PAS9_list_snow_combined = PAS9_list_snow_combined,
  PAS9_list_snow_site = PAS9_list_snow_site,
  PAS9_list_snow_stand = PAS9_list_snow_stand,

  PAS10_list_ips_combined = PAS10_list_ips_combined,
  PAS10_list_ips_site = PAS10_list_ips_site,
  PAS10_list_ips_stand = PAS10_list_ips_stand
)

# Apply the function to each list and combine the results
PAS_all_lists <- map2_dfr(names(PAS_lists), PAS_lists, process_list)

# End of predisposition
# List of all variables to check
variables_to_check <- c(
  "PAS1_list_browsing_combined",
  "PAS1_list_browsing_combined_win",
  "PAS1_list_browsing_site",
  "PAS1_list_browsing_site_win",
  "PAS1_list_browsing_stand",

  "PAS2_list_barkstripping_combined",
  "PAS2_list_barkstripping_site",
  "PAS2_list_barkstripping_stand",

  "PAS3_list_cembrae_combined",
  "PAS3_list_cembrae_site",
  "PAS3_list_cembrae_stand",

  "PAS4_list_armillaria_combined",
  "PAS4_list_armillaria_site",
  "PAS4_list_armillaria_stand",

  "PAS5_list_barkbreeding_combined",
  "PAS5_list_barkbreeding_site",
  "PAS5_list_barkbreeding_stand",

  "PAS6_list_heterobasidion_combined",
  "PAS6_list_heterobasidion_site",
  "PAS6_list_heterobasidion_stand",

  "PAS7_list_fire_combined",
  "PAS7_list_fire_site",
  "PAS7_list_fire_stand",

  "PAS8_list_storm_combined",
  "PAS8_list_storm_site",
  "PAS8_list_storm_stand",

  "PAS9_list_snow_combined",
  "PAS9_list_snow_site",
  "PAS9_list_snow_stand",

  "PAS10_list_ips_combined",
  "PAS10_list_ips_site",
  "PAS10_list_ips_stand"
)

# Check which variables exist
missing_variables <- variables_to_check[!sapply(variables_to_check, exists)]

# Notify about missing variables
if (length(missing_variables) == 0) {
  message("All variables exist in the environment.")
} else {
  message("The following variables are missing:")
  print(missing_variables)
}

tryCatch({
  if (!exists("PAS_lists")) {
    stop("PAS_lists does not exist")
  } else {
    message("PAS_lists exists in the environment.")
  }
}, error = function(e) {
  message("An error occurred: ", e$message)
})

}

# Utility of the goals ---------------------------------------------------------

# Read in and weight the goals defined by the users

# read in the goals
# get the utility of the goals
# get the weighting of the goals
# sum up the goal weighting
# divide the weighting through the sum of weighting
# multiply utility and normalized weighting
# do the same for each of the named management action

## Calculate the utilities

Ziel_DF <- data.frame(ziel = json_data$ziel # use the goals that the user defined
                      #interessensgruppe = json_data$ziel_Prio$interessensgruppe, # interessensgruppe is right now only "eigene"
                      # but it is inserted her already as if there would be multiple,
                      # to have a placeholder when multiple interest groups are involved
                      # and when it is possible to choose from them
                      #priority = json_data$ziel_Prio$priority
                      ) # priority is scaled from  to 1 to 5)

Ziel_DF <- crossing(Ziel_DF, Zeitpunkt_Maßnahme)

# Filter the data to only include the relevant age class and therefore only the relevant actions
# do it again on the basis of the data in Vergleichsbestand
Ziel_DF <- Ziel_DF %>%
  filter(
    (Zielerreichung_Waldbauliches_Vergleichsbestand$Wuchsklasse[1]  == "Dickung" & (Zeitpunkt_Maßnahme == "IST" | str_detect(Zeitpunkt_Maßnahme, "^IST\\+10/Di"))) |
      (Zielerreichung_Waldbauliches_Vergleichsbestand$Wuchsklasse[1]  == "Stangenholz" & (Zeitpunkt_Maßnahme == "IST" | str_detect(Zeitpunkt_Maßnahme, "^IST\\+10/StH"))) |
      (Zielerreichung_Waldbauliches_Vergleichsbestand$Wuchsklasse[1]  == "sw Baumholz" & (Zeitpunkt_Maßnahme == "IST" | str_detect(Zeitpunkt_Maßnahme, "^IST\\+10/BH")))
  )

# as a double check, remove possible IST states
Ziel_DF <- Ziel_DF %>%
  filter(Zeitpunkt_Maßnahme != "IST")

## Utility for Capercaillie

# note: the logic is inverted here, because in the future it should be higher than in the current state
# therefore we compute future - IST, and if this is greater than 0.1 it is good
if (any(json_data$ziel == "habverb")) {
  # Filter Ziel_DF once outside the loop
  Ziel_DF_habverb <- Ziel_DF %>%
    filter(ziel == "habverb")

  # Initialize an empty vector to store Utility values
  utilities <- numeric(length(PAS11_list_capercaillie_combined))

  for (i in seq_along(PAS11_list_capercaillie_combined)) {
    # Calculate the Utility value for the current sublist
    utilities[i] <- case_when(
      (PAS11_list_capercaillie_combined[[i]] - PAS11_list_capercaillie_combined[[1]]) >= 0.1 ~ 100,
      (PAS11_list_capercaillie_combined[[i]] - PAS11_list_capercaillie_combined[[1]]) < 0.1 ~ 0
    )
  }

  # Add the Utility values to Ziel_DF_habverb
  Ziel_DF_habverb$Utility <- as.numeric(utilities)
}

## Utility for resistance

if (any(json_data$ziel == "erhöh_widerstand")) {
# then select those predispositions whose value is greater than 0.6

PAS_lists_filtered <-
  PAS_all_lists %>% filter(PAS_name != "PAS11_list_capercaillie_win" &
                             PAS_name != "PAS11_list_capercaillie_combined" &
                             PAS_name != "PAS11_list_capercaillie_sum")

# remove the word list_ to be able to join easier
PAS_lists_filtered <- PAS_lists_filtered %>%
  mutate(PAS_name = gsub("list_", "", PAS_name))
# do the same for the word _combined
PAS_lists_filtered <- PAS_lists_filtered %>%
  mutate(PAS_name = gsub("_combined", "", PAS_name))
PAS_lists_filtered <- PAS_lists_filtered %>%
  mutate(PAS_name = gsub("PAS", "PAS_", PAS_name))

# filter the PAS IST to only include those with a higher value then 0.6, therefore they should be reduced
PAS_lists_merged <- PAS_lists_filtered %>% filter(IST >= 0.4)

## then fix the IST value below and, with a loop, subtract the P values of the measures (writing them into a new column)
## if all these differences are >= 0.1, set value = 100, otherwise 0

# Exclude the irrelevant columns from the vector to only get the relevabt actions
Maßnahme_Vector <- colnames(PAS_lists_merged)
Maßnahme_Vector <- Maßnahme_Vector[!(Maßnahme_Vector == "IST") & !(Maßnahme_Vector == "PAS_name")]

# Initialize an empty data frame to store results
Utility_df <- data.frame(PAS_name = PAS_lists_merged$PAS_name)

# Loop through each Maßnahme column
for (Maßnahme in Maßnahme_Vector) {
  # Calculate the difference
  difference <- as.numeric(PAS_lists_merged$IST) - as.numeric(PAS_lists_merged[[Maßnahme]])

  # Assign 100 if difference >= 0.1, otherwise 0
  Utility_df[[Maßnahme]] <- ifelse(difference >= 0.1, 100, 0)
}

# Calculate the mean for each column, ignoring NA values
column_means <- colMeans(Utility_df[, -1], na.rm = TRUE)
column_means_df <- data.frame(Column = names(column_means), Mean = column_means)

## it messes up the names of the actions, therefore redefine it and match the original ones with the "new" ones
Zeitpunkt_Maßnahme_dots <-
  c(
    ## Dickung
    "IST.10.Di_0",
    "IST.10.Di_1",
    "IST.10.Di_2",

    "IST.10.Di_1_M",
    "IST.10.Di_1_M.Ki",
    "IST.10.Di_1_M.Fi.Ki",
    "IST.10.Di_1_M.NH",

    "IST.10.Di_2_M",
    "IST.10.Di_2_M.Lä",
    "IST.10.Di_2_M.Fi.Ki",
    "IST.10.Di_2_M.NH",

    ## Stangenholz
    "IST.10.StH_0",
    "IST.10.StH_1",

    "IST.10.StH_2",
    "IST.10.StH_2.Ki",
    "IST.10.StH_2.Lä",
    "IST.10.StH_2.Fi.Ki",
    "IST.10.StH_2.Fi.Ki.Lä",
    "IST.10.StH_2.NH",

    "IST.10.StH_3",
    "IST.10.StH_3.Ki",
    "IST.10.StH_3.Lä",
    "IST.10.StH_3.Fi.Ki",
    "IST.10.StH_3.Fi.Ki.Lä",
    "IST.10.StH_3.NH",
    "IST.10.StH_3_L",

    ## Baumholz
    "IST.10.BH_0",
    "IST.10.BH_1",
    "IST.10.BH_2",
    "IST.10.BH_2_L",

    "IST.10.BH_3",
    "IST.10.BH_3.Fi",
    "IST.10.BH_3.Ki",
    "IST.10.BH_3.Lä",
    "IST.10.BH_3.Fi.Ki",
    "IST.10.BH_3.Fi.Ki.Lä",

    "IST.10.BH_3_L",
    "IST.10.BH_3.NH",

    "IST.10.BH_4",
    "IST.10.BH_4.Fi",
    "IST.10.BH_4.Ki",
    "IST.10.BH_4.Lä",
    "IST.10.BH_4.Fi.Ki",
    "IST.10.BH_4.Fi.Ki.Lä",
    "IST.10.BH_4.NH",
    "IST.10.BH_4_L"
  )

Zeitpunkt_Maßnahme <-
  c(
    ## Dickung
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

    ## Stangenholz
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

    ## Baumholz
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

actions <- data.frame(Zeitpunkt_Maßnahme, Zeitpunkt_Maßnahme_dots)

actions <- actions %>%
  filter(
    (Zielerreichung_Waldbauliches_Vergleichsbestand$Wuchsklasse[1] == "Dickung" & (Zeitpunkt_Maßnahme != "IST" | str_detect(Zeitpunkt_Maßnahme, "^IST\\+10/Di"))) |
      (Zielerreichung_Waldbauliches_Vergleichsbestand$Wuchsklasse[1] == "Stangenholz" & (Zeitpunkt_Maßnahme != "IST" | str_detect(Zeitpunkt_Maßnahme, "^IST\\+10/StH"))) |
      (Zielerreichung_Waldbauliches_Vergleichsbestand$Wuchsklasse[1] == "sw Baumholz" & (Zeitpunkt_Maßnahme != "IST" | str_detect(Zeitpunkt_Maßnahme, "^IST\\+10/BH")))
  )

# get the DF with the values and then match it according to the names of the actions to lateron be able to add it to the Ziel_DF
column_means_df <- left_join(column_means_df, actions, by = join_by(Column==Zeitpunkt_Maßnahme_dots) )

# Update Ziel_DF
Ziel_DF_widerstand <- Ziel_DF %>%
  filter(ziel == "erhöh_widerstand") %>% # Filter rows where ziel == "erhöh_widerstand"
  full_join(column_means_df, by = "Zeitpunkt_Maßnahme") %>% # Match Zeitpunkt_Maßnahme with Column
  mutate(Utility = Mean) %>% # Update Utility column with Mean values
  select(-Mean) # Remove the Mean column if no longer needed

Ziel_DF_widerstand$Utility <- as.numeric(round(Ziel_DF_widerstand$Utility, 2))
}

## Utility for sawlog quality

if (any(json_data$ziel == "Sägerundholzqualität")) {

  scenario_names <- names(Zielerreichung_list)[-1]

  Obj_list_roundwood <- vector("list", length(scenario_names))
  names(Obj_list_roundwood) <- scenario_names

  srh_script <- "insert path to PAS scripts directory/obj_SRH_backend_SERVER.R"

  for (i in seq_along(scenario_names)) {
    scenario_name <- scenario_names[i]
    scenario <- as.list(Zielerreichung_list[[scenario_name]])

    scenario_env <- new.env(parent = .GlobalEnv)

    for (nm in names(scenario)) {
      assign(nm, scenario[[nm]], envir = scenario_env)
    }

    source(srh_script, local = scenario_env)

    if (!exists("Obj_roundwood", envir = scenario_env, inherits = FALSE)) {
      stop(
        paste0(
          "Obj_roundwood was not created for scenario '",
          scenario_name,
          "'. Check obj_SRH_backend_SERVER.R."
        )
      )
    }

    Obj_list_roundwood[[scenario_name]] <- get("Obj_roundwood", envir = scenario_env)
  }

  valid_names <- Ziel_DF %>%
    dplyr::filter(ziel == "Sägerundholzqualität") %>%
    dplyr::pull(Zeitpunkt_Maßnahme) %>%
    unique()

  Obj_list_roundwood <- Obj_list_roundwood[names(Obj_list_roundwood) %in% valid_names]

  Ziel_DF_SRH <- Ziel_DF %>%
    dplyr::filter(ziel == "Sägerundholzqualität")

  Obj_list_roundwood <- Obj_list_roundwood[
    match(Ziel_DF_SRH$Zeitpunkt_Maßnahme, names(Obj_list_roundwood))
  ]

  Ziel_DF_SRH$Utility <- vapply(
    Obj_list_roundwood,
    function(x) {
      if (is.null(x) || length(x) < 1) {
        return(NA_real_)
      }
      as.numeric(x[[1]])
    },
    numeric(1)
  )

  Ziel_DF_SRH$Utility <- Ziel_DF_SRH$Utility * 100
}

## Utility summary

utility_sources <- list()

if (exists("Ziel_DF_habverb")) {
  utility_sources[["habverb"]] <- Ziel_DF_habverb
}

if (exists("Ziel_DF_widerstand")) {
  utility_sources[["widerstand"]] <- Ziel_DF_widerstand
}

if (exists("Ziel_DF_SRH")) {
  utility_sources[["SRH"]] <- Ziel_DF_SRH
}

if (length(utility_sources) == 0) {
  stop("No utility tables were created.")
}

result <- bind_rows(utility_sources, .id = "source")
result$source <- NULL

# Group by Zeitpunkt_Maßnahme and calculate the mean Utility for each goal
utility_all <- result %>%
  group_by(Zeitpunkt_Maßnahme, ziel) %>%
  summarise(Zielerreichung = mean(Utility, na.rm = TRUE), .groups = "drop")

# all measures
utility_alle_massnahmen <- utility_all %>%
  group_by(Zeitpunkt_Maßnahme) %>%
  summarise(Zielerreichung = mean(Zielerreichung, na.rm = TRUE), .groups = "drop")

# best measure(s) only for the text
utility_max_maßnahme <- utility_alle_massnahmen %>%
  mutate(Zielerreichung_round = round(Zielerreichung, 2)) %>%
  filter(Zielerreichung_round == max(Zielerreichung_round)) %>%
  select(-Zielerreichung_round)

utility_max_goal <- utility_all %>%
  group_by(ziel) %>%
  summarise(Zielerreichung = mean(Zielerreichung, na.rm = TRUE), .groups = "drop")

print(utility_all)
print(utility_alle_massnahmen)
print(utility_max_maßnahme)
print(utility_max_goal)

tryCatch({
  if (!exists("utility_all")) stop("utility_all does not exist")
  if (!exists("utility_alle_massnahmen")) stop("utility_alle_massnahmen does not exist")
  if (!exists("utility_max_maßnahme")) stop("utility_max_maßnahme does not exist")
  if (!exists("utility_max_goal")) stop("utility_max_goal does not exist")

  message("All utility variables exist in the environment.")
}, error = function(e) {
  message("An error occurred: ", e$message)
})

# Build the traffic-light indicators -------------------------------------------

# Modify PAS_Results so we can pass them into the Ampel code
if (any(json_data$ziel == "erhöh_widerstand")) {

  name_map <- setNames(Zeitpunkt_Maßnahme, Zeitpunkt_Maßnahme_dots)

  PAS_lists_filtered <- PAS_lists_filtered %>%
    rename_with(~ ifelse(.x %in% names(name_map), name_map[.x], .x))

  # Step 1: Store the current column names
  original_colnames <- colnames(PAS_lists_filtered)

  # Step 2: Convert to character to avoid type coercion issues
  PAS_lists_filtered[] <- lapply(PAS_lists_filtered, as.character)

  # Step 3: Transpose (excluding the first column)
  transposed_mat <- t(PAS_lists_filtered[, -1])

  # Step 4: Convert to data frame
  PAS_lists_transposed <- as.data.frame(transposed_mat, stringsAsFactors = FALSE)

  # Step 5: Set the column names to the values from the first column
  colnames(PAS_lists_transposed) <- PAS_lists_filtered[[1]]

  # Step 6: Add the original column names as a new column
  PAS_lists_transposed$Zeitpunkt_Maßnahme <- original_colnames[-1]

  # Step 7: Reorder columns if needed (make the new col first)
  PAS_lists_transposed <- PAS_lists_transposed[, c(ncol(PAS_lists_transposed), 1:(ncol(PAS_lists_transposed) - 1))]

  # IMPORTANT:
  # Keep raw numeric values here. Do NOT classify to 0.3 / 0.7 / 1 yet.
  PAS_lists_transposed <- PAS_lists_transposed %>%
  mutate(across(
    .cols = -Zeitpunkt_Maßnahme,
    .fns = ~ 1 - as.numeric(.)
  ))
# normalize PAS values here. since 1 is bad for PAS but good for everything else.

  cols_to_exclude <- setdiff(
    intersect(colnames(PAS_lists_transposed), colnames(Zielerreichung_Waldbauliches_Vergleichsbestand)),
    "Zeitpunkt_Maßnahme"
  )

  Zielerreichung_Waldbauliches_Vergleichsbestand_Ampeln <- PAS_lists_transposed %>%
    left_join(
      Zielerreichung_Waldbauliches_Vergleichsbestand %>%
        select(-all_of(cols_to_exclude)),
      by = "Zeitpunkt_Maßnahme"
    )

} else {
  Zielerreichung_Waldbauliches_Vergleichsbestand_Ampeln <- Zielerreichung_Waldbauliches_Vergleichsbestand
}

Zielerreichung_Waldbauliches_Vergleichsbestand_Ampeln$forest_gaps_Ampeln <-
  Zielerreichung_Waldbauliches_Vergleichsbestand_Ampeln$forest_gaps

#flip_map <- c(
#  "0" = 1,
#  "0.3" = 0.7,
#  "0.5" = 0.5,
#  "0.7" = 0.3,
#  "0.9" = 0,
#  "1" = 0
#)

#Zielerreichung_Waldbauliches_Vergleichsbestand_Ampeln$Kronenlänge <-
#  flip_map[as.character(Zielerreichung_Waldbauliches_Vergleichsbestand_Ampeln$Kronenlänge)]

#Zielerreichung_Waldbauliches_Vergleichsbestand_Ampeln$HD_Wert <-
#  flip_map[as.character(Zielerreichung_Waldbauliches_Vergleichsbestand_Ampeln$HD_Wert)]

Ampel_ct <- read_excel("insert path to Ampel_classtable.xlsx", trim_ws = TRUE)

# switch off the traffic-light indicators depending on the input
if (json_data$Wuchsklasse %in% c("Dickung")) {
  Ampel_ct$displayAmpel[Ampel_ct$Variable_Name == "Schäden"] <- 0
  Ampel_ct$displayAmpel[Ampel_ct$Variable_Name == "HD_Wert"] <- 0
  Ampel_ct$displayAmpel[Ampel_ct$Variable_Name == "Astfreiheit"] <- 0
  Ampel_ct$displayAmpel[Ampel_ct$Variable_Name == "Kronenlänge"] <- 0
  Ampel_ct$displayAmpel[Ampel_ct$Variable_Name == "Feinerschließung"] <- 0

}

if (json_data$Wuchsklasse %in% c("sw Baumholz")) {
  Ampel_ct$displayAmpel[Ampel_ct$Variable_Name == "Schäden_Wuchs"] <- 0
  Ampel_ct$displayAmpel[Ampel_ct$Variable_Name == "BA_Diversität"] <- 0
}

if (json_data$Wuchsklasse %in% c("Stangenholz")) {
  Ampel_ct$displayAmpel[Ampel_ct$Variable_Name == "Schäden"] <- 0
  Ampel_ct$displayAmpel[Ampel_ct$Variable_Name == "BA_Diversität"] <- 0
  Ampel_ct$displayAmpel[Ampel_ct$Variable_Name == "Kronenlänge"] <- 0
}

# Check if 'erhöh_widerstand' is NOT present in json_data$ziel
if (!any(json_data$ziel == "erhöh_widerstand")) {

  pas_vars <- c(
    "PAS_1_browsing", "PAS_1_browsing_site", "PAS_1_browsing_stand",
    "PAS_1_browsing_win", "PAS_1_browsing_win_site",
    "PAS_2_barkstripping", "PAS_2_barkstripping_site", "PAS_2_barkstripping_stand",
    "PAS_3_cembrae", "PAS_3_cembrae_site", "PAS_3_cembrae_stand",
    "PAS_4_armillaria", "PAS_4_armillaria_site", "PAS_4_armillaria_stand",
    "PAS_5_barkbreeding", "PAS_5_barkbreeding_site", "PAS_5_barkbreeding_stand",
    "PAS_6_heterobasidion", "PAS_6_heterobasidion_site", "PAS_6_heterobasidion_stand",
    "PAS_7_fire", "PAS_7_fire_site", "PAS_7_fire_stand",
    "PAS_8_storm", "PAS_8_storm_site", "PAS_8_storm_stand",
    "PAS_9_snow", "PAS_9_snow_site", "PAS_9_snow_stand",
    "PAS_10_ips", "PAS_10_ips_site", "PAS_10_ips_stand"
  )

  Ampel_ct$displayAmpel[Ampel_ct$Variable_Name %in% pas_vars] <- 0
}

if (!any(json_data$ziel == "Sägerundholzqualität")) {

  SRH_vars <- c(
    "BA_Diversität",
    "Groberschließung",
    "Schäden_Wuchs",
    "Stabilitätsträger",
    "Feinerschließung",
    "Kronenlänge",
    "Astfreiheit",
    "HD_Wert",
    "Schäden"
  )

  Ampel_ct$displayAmpel[Ampel_ct$Variable_Name %in% SRH_vars] <- 0
}

if (!any(json_data$ziel == "habverb")) {
  Ampel_ct$displayAmpel[Ampel_ct$Variable_Name %in% "forest_gaps_Ampeln"] <- 0
}

Zielerreichung_Waldbauliches_Vergleichsbestand_Ampeln$forest_gaps <-
  as.numeric(Zielerreichung_Waldbauliches_Vergleichsbestand_Ampeln$forest_gaps)

# Create vectors of variable names based on Ampel_ct table
Indikatoren_Zweier_Ampeln <- Ampel_ct %>%
  filter(displayAmpel == 1, Ampel_type == "Zweier_Ampel") %>%
  pull(Variable_Name)

Indikatoren_Dreier_Ampeln <- Ampel_ct %>%
  filter(displayAmpel == 1, Ampel_type == "Dreier_Ampel") %>%
  pull(Variable_Name)

Indikatoren_Aktiv <- c(Indikatoren_Zweier_Ampeln, Indikatoren_Dreier_Ampeln)
Indikatoren_Aktiv <- intersect(Indikatoren_Aktiv, colnames(Zielerreichung_Waldbauliches_Vergleichsbestand_Ampeln))
Indikatoren_Zweier_Ampeln <- intersect(Indikatoren_Zweier_Ampeln, Indikatoren_Aktiv)
Indikatoren_Dreier_Ampeln <- intersect(Indikatoren_Dreier_Ampeln, Indikatoren_Aktiv)

# Traffic-light helper functions -----------------------------------------------

# Only Dreier-Ampeln are classified here.
# Border logic:
# < 0.4  -> 0.3 (rot)
# 0.4-0.7 inclusive -> 0.7 (gelb)
# > 0.7  -> 1 (gruen)
classify_dreier_ampel <- function(value) {
  case_when(
    is.na(value)  ~ NA_real_,
    value < 0.4   ~ 0.3,
    value <= 0.7  ~ 0.7,
    value > 0.7   ~ 1,
    TRUE          ~ NA_real_
  )
}

get_ampel_code <- function(value, indikator_name, zweier_indikatoren, dreier_indikatoren) {

  if (is.na(value)) {
    return(NA_character_)
  }

  if (indikator_name %in% zweier_indikatoren) {
    return(case_when(
      value == 0 ~ "Zweier_Ampel_rot",
      value == 1 ~ "Zweier_Ampel_gruen",
      TRUE ~ NA_character_
    ))
  }

  if (indikator_name %in% dreier_indikatoren) {
    value_class <- classify_dreier_ampel(as.numeric(value))

    return(case_when(
      value_class == 0.3 ~ "Dreier_Ampel_rot",
      value_class == 0.7 ~ "Dreier_Ampel_gelb",
      value_class == 1   ~ "Dreier_Ampel_gruen",
      TRUE ~ NA_character_
    ))
  }

  NA_character_
}

get_ampel_color <- function(value, indikator_name, zweier_indikatoren, dreier_indikatoren) {

  if (is.na(value)) {
    return(NA_character_)
  }

  if (indikator_name %in% zweier_indikatoren) {
    return(case_when(
      value == 0 ~ "rot",
      value == 1 ~ "gruen",
      TRUE ~ NA_character_
    ))
  }

  if (indikator_name %in% dreier_indikatoren) {
    value_class <- classify_dreier_ampel(as.numeric(value))

    return(case_when(
      value_class == 0.3 ~ "rot",
      value_class == 0.7 ~ "gelb",
      value_class == 1   ~ "gruen",
      TRUE ~ NA_character_
    ))
  }

  NA_character_
}

get_direction <- function(scenario_value, ist_value) {
  case_when(
    is.na(scenario_value) | is.na(ist_value) ~ NA_character_,
    scenario_value < ist_value ~ "unten",
    scenario_value > ist_value ~ "oben",
    scenario_value == ist_value ~ "waagerecht",
    TRUE ~ NA_character_
  )
}

ampel_source_df <- Zielerreichung_Waldbauliches_Vergleichsbestand_Ampeln %>%
  select(Zeitpunkt_Maßnahme, all_of(Indikatoren_Aktiv))

# Extract IST row
ist_row_raw <- ampel_source_df %>%
  filter(Zeitpunkt_Maßnahme == "IST") %>%
  slice(1)

if (nrow(ist_row_raw) == 0) {
  stop("No IST row found in Zielerreichung_Waldbauliches_Vergleichsbestand_Ampeln.")
}

# Create IST display row
df_IST_mapped <- ist_row_raw

for (col in Indikatoren_Aktiv) {
  df_IST_mapped[[col]] <- get_ampel_code(
    value = as.numeric(ist_row_raw[[col]][[1]]),
    indikator_name = col,
    zweier_indikatoren = Indikatoren_Zweier_Ampeln,
    dreier_indikatoren = Indikatoren_Dreier_Ampeln
  )
}

# Create scenario rows with arrows
scenario_rows <- ampel_source_df %>%
  filter(Zeitpunkt_Maßnahme != "IST")

df_scenarios_mapped <- scenario_rows

for (i in seq_len(nrow(df_scenarios_mapped))) {

  current_massnahme <- df_scenarios_mapped$Zeitpunkt_Maßnahme[i]

  for (col in Indikatoren_Aktiv) {

    ist_value <- as.numeric(ist_row_raw[[col]][[1]])
    scenario_value <- as.numeric(scenario_rows[[col]][[i]])

    direction <- get_direction(
      scenario_value = scenario_value,
      ist_value = ist_value
    )

    color <- get_ampel_color(
      value = scenario_value,
      indikator_name = col,
      zweier_indikatoren = Indikatoren_Zweier_Ampeln,
      dreier_indikatoren = Indikatoren_Dreier_Ampeln
    )

    df_scenarios_mapped[[col]][i] <- ifelse(
      is.na(direction) | is.na(color),
      NA_character_,
      paste0("Pfeil_", color, "_", direction)
    )
  }
}

# Combine IST + scenarios
df_IST_mapped_with_comparisons <- bind_rows(df_IST_mapped, df_scenarios_mapped)

# Keep only relevant columns
cols_to_keep <- c("Zeitpunkt_Maßnahme", Indikatoren_Aktiv)

df_IST_mapped_with_comparisons <- df_IST_mapped_with_comparisons %>%
  select(any_of(cols_to_keep))

print("Ampel output successfully created")

Ergebnis_Sortierung <- read_excel("insert path to Ergebnisse_Variablenselektion_und_Benennung.xlsx")

# for convert_to_json_structure()
Ergebnis_Sortierung_filtered_all <- Ergebnis_Sortierung %>%
  filter(!is.na(ResultsLabel)) %>%
  select(Variable_Name, ResultsLabel)

# for areaDetail, with Excel row order preserved
Ergebnis_Sortierung_filtered_vars <- Ergebnis_Sortierung %>%
  filter(!is.na(ResultsLabel), table == "vars") %>%
  mutate(display_order = row_number()) %>%
  select(Variable_Name, ResultsLabel, display_order)

df_cleaned <- df_IST_mapped_with_comparisons %>%
  mutate(Zeitpunkt_Maßnahme = gsub("^IST\\+10/", "", Zeitpunkt_Maßnahme)) %>%
  group_by(across(-Zeitpunkt_Maßnahme)) %>%
  summarise(
    Zeitpunkt_Maßnahme = paste(sort(unique(Zeitpunkt_Maßnahme)), collapse = ", "),
    .groups = "drop"
  )

## Force sorting by intervention intensity

# Your updated vector (new measure codes)
alle_voll <- c(
  ## Dickung
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

  ## Stangenholz
  "IST+10/StH_0",
  "IST+10/StH_1",

  "IST+10/StH_2",
  "IST+10/StH_2-Fi-Ki",
  "IST+10/StH_2-Fi-Ki-Lä",
  "IST+10/StH_2-Ki",
  "IST+10/StH_2-Lä",
  "IST+10/StH_2-NH",

  "IST+10/StH_3",
  "IST+10/StH_3-Fi-Ki",
  "IST+10/StH_3-Fi-Ki-Lä",
  "IST+10/StH_3-Ki",
  "IST+10/StH_3_L",
  "IST+10/StH_3-Lä",
  "IST+10/StH_3-NH",

  ## Baumholz
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
  "IST+10/BH_3-NH",
  "IST+10/BH_3_L",

  "IST+10/BH_4",
  "IST+10/BH_4-Fi",
  "IST+10/BH_4-Ki",
  "IST+10/BH_4-Lä",
  "IST+10/BH_4-Fi-Ki",
  "IST+10/BH_4-Fi-Ki-Lä",
  "IST+10/BH_4-NH",
  "IST+10/BH_4_L"
)

# Clean alle_voll vector (unchanged logic!)
alle_voll_cleaned <- gsub("IST\\+10/", "", alle_voll)
alle_voll_cleaned <- c("IST", alle_voll_cleaned)

# Create a named lookup for sorting order
order_lookup <- setNames(seq_along(alle_voll_cleaned), alle_voll_cleaned)

# Function to find earliest Maßnahme in the aggregated string
get_sort_key <- function(zeitpunkt_string) {
  # Split by comma and trim whitespace
  parts <- str_trim(unlist(strsplit(zeitpunkt_string, ",")))

  # Get positions in alle_voll_cleaned order vector (NA if not found)
  indices <- order_lookup[parts]

  # Return minimum index found or a large number if none found
  if (all(is.na(indices))) return(Inf)
  min(indices, na.rm = TRUE)
}

# Apply sorting key to dataframe and arrange without altering Zeitpunkt_Maßnahme
df_cleaned <- df_cleaned %>%
  mutate(sort_key = sapply(Zeitpunkt_Maßnahme, get_sort_key)) %>%
  arrange(sort_key) %>%
  select(-sort_key)

# ============================================================================
# Zweistufige tabularResult-Struktur (Oberziele -> Unterindikatoren)
# ============================================================================
# replaces convert_to_json_structure() + the line tabRes <- convert_to_json_structure(...)
# assumes (as before): df_cleaned, Ergebnis_Sortierung_filtered_all,
#   classify_dreier_ampel/get_ampel_code/get_ampel_color/get_direction,
#   Zielerreichung_Waldbauliches_Vergleichsbestand_Ampeln, Zielerreichung_list,
#   Obj_list_roundwood (SRH), PAS11_list_capercaillie_* (Auerhuhn), json_data$areaDetail.
# scale: all top-level goal values HIGHER = BETTER (predisposition columns in the traffic-light DF are
# already flipped). All three top-level goals are rated as a three-level traffic light.

library(tidyr); library(purrr)

GOAL_CONFIG <- tibble::tribble(
  ~goal,          ~parent_label,                            ~ziel_key,
  "Auerhuhn",     "Habitatverbesserung für das Auerhuhn",   "habverb",
  "Sägerundholz", "Holzproduktion in Sägerundholzqualität", "Sägerundholzqualität",
  "Prädispo",     "Erhöhung der Widerstandsfähigkeit",      "erhöh_widerstand"
)
detect_goal <- function(rl) dplyr::case_when(
  stringr::str_starts(rl, "Auerhuhn")       ~ "Auerhuhn",
  stringr::str_starts(rl, "Sägerundholz")   ~ "Sägerundholz",
  stringr::str_starts(rl, "Prädisposition") ~ "Prädispo",
  TRUE ~ NA_character_
)
strip_ist <- function(x) gsub("^IST\\+10/", "", x)

# authoritative scenario columns (incl. combined ones like "BH_3, BH_4")
class_names <- df_cleaned$Zeitpunkt_Maßnahme

# aligns a value vector named by SINGLE measures onto class_names.
# combined columns ("BH_3, BH_4") are resolved via the FIRST measure
# (the combined measures are equal in value by definition).
align_to_classnames <- function(measure_vals, ist_value) {
  out <- setNames(rep(NA_real_, length(class_names)), class_names)
  for (cn in class_names) {
    if (identical(cn, "IST")) { out[[cn]] <- as.numeric(ist_value); next }
    if (cn %in% names(measure_vals)) { out[[cn]] <- as.numeric(measure_vals[[cn]]); next }
    fm <- trimws(strsplit(cn, ",")[[1]][1])
    if (fm %in% names(measure_vals)) out[[cn]] <- as.numeric(measure_vals[[fm]])
  }
  out
}

# numeric top-level goal value per className -> traffic-light symbols (always three-level)
overall_to_symbols <- function(named_vec) {
  ist <- as.numeric(named_vec[["IST"]])
  purrr::map_dfr(names(named_vec), function(cn) {
    v <- as.numeric(named_vec[[cn]])
    if (identical(cn, "IST")) {
      sym <- get_ampel_code(v, "PARENT", zweier_indikatoren = character(0), dreier_indikatoren = "PARENT")
    } else {
      col <- get_ampel_color(v, "PARENT", zweier_indikatoren = character(0), dreier_indikatoren = "PARENT")
      dir <- get_direction(v, ist)
      sym <- if (is.na(col) || is.na(dir)) NA_character_ else paste0("Pfeil_", col, "_", dir)
    }
    tibble::tibble(className = cn, thresholdValue = sym)
  })
}

# mean of an indicator from json_data$areaDetail (for IST values)
ad_mean <- function(indvar) {
  ad <- json_data$areaDetail
  x <- ad$mean[ad$indikatorVariable == indvar]
  if (length(x) >= 1 && !is.na(x[[1]])) as.numeric(x[[1]]) else NA_real_
}
# robust reading of the (doubly appended) PAS lists: only named entries
clean_pas_list <- function(lst) {
  nm <- names(lst)
  if (is.null(nm)) return(setNames(numeric(0), character(0)))
  keep <- nzchar(nm)
  vals <- vapply(lst[keep], function(x) suppressWarnings(as.numeric(x)[1]), numeric(1))
  setNames(vals, strip_ist(nm[keep]))
}
# one-off IST computation of a backend (for SRH), without changing the existing loops
compute_ist_env <- function(script_path, want) {
  ist_name <- names(Zielerreichung_list)[1]
  scen     <- as.list(Zielerreichung_list[[ist_name]])
  env      <- new.env(parent = .GlobalEnv)
  for (nm in names(scen)) assign(nm, scen[[nm]], envir = env)
  suppressWarnings(source(script_path, local = env))
  setNames(lapply(want, function(w) if (exists(w, envir = env, inherits = FALSE)) get(w, envir = env) else NA_real_), want)
}

parent_syms  <- tibble::tibble()
extra_leaves <- tibble::tibble()

# (A) predisposition: mean of the combined (total) PAS models from the traffic-light DF
if ("erhöh_widerstand" %in% json_data$ziel) {
  praedispo_cols <- c("PAS_1_browsing","PAS_2_barkstripping","PAS_3_cembrae","PAS_4_armillaria",
                      "PAS_5_barkbreeding","PAS_6_heterobasidion","PAS_7_fire","PAS_8_storm",
                      "PAS_9_snow","PAS_10_ips")
  praedispo_cols <- intersect(praedispo_cols, names(Zielerreichung_Waldbauliches_Vergleichsbestand_Ampeln))
  pr <- Zielerreichung_Waldbauliches_Vergleichsbestand_Ampeln %>%
    dplyr::transmute(cn = strip_ist(Zeitpunkt_Maßnahme),
                     value = rowMeans(dplyr::across(dplyr::all_of(praedispo_cols), as.numeric), na.rm = TRUE))
  ist_val      <- pr$value[pr$cn == "IST"][1]
  measure_vals <- setNames(pr$value[pr$cn != "IST"], pr$cn[pr$cn != "IST"])
  vec <- align_to_classnames(measure_vals, ist_val)
  parent_syms <- dplyr::bind_rows(parent_syms, overall_to_symbols(vec) %>% dplyr::mutate(goal = "Prädispo"))
}

# (B) sawlog: Obj_list_roundwood (measures) + IST computed separately
if ("Sägerundholzqualität" %in% json_data$ziel) {
  measure_vals <- setNames(as.numeric(unlist(Obj_list_roundwood)), strip_ist(names(Obj_list_roundwood)))
  ist_val <- as.numeric(compute_ist_env(srh_script, "Obj_roundwood")[["Obj_roundwood"]])
  vec <- align_to_classnames(measure_vals, ist_val)
  parent_syms <- dplyr::bind_rows(parent_syms, overall_to_symbols(vec) %>% dplyr::mutate(goal = "Sägerundholz"))
}

# (C) Auerhuhn: capercaillie combined (Parent) + win/sum (Unterindikatoren).
#     IST values come from areaDetail (robust; no re-sourcing of the backend).
if ("habverb" %in% json_data$ziel) {
  mk <- function(lst, ist) align_to_classnames(clean_pas_list(lst), ist)
  comb <- mk(PAS11_list_capercaillie_combined, ad_mean("PAS_11_capercaillie"))
  win  <- mk(PAS11_list_capercaillie_win,      ad_mean("PAS_11_capercaillie_winter"))
  sum_ <- mk(PAS11_list_capercaillie_sum,      ad_mean("PAS_11_capercaillie_summer"))
  parent_syms <- dplyr::bind_rows(parent_syms, overall_to_symbols(comb) %>% dplyr::mutate(goal = "Auerhuhn"))
  extra_leaves <- dplyr::bind_rows(
    extra_leaves,
    overall_to_symbols(win)  %>% dplyr::mutate(goal = "Auerhuhn", indikatorVariable = "Habitateignung Auerhuhn (Winter)"),
    overall_to_symbols(sum_) %>% dplyr::mutate(goal = "Auerhuhn", indikatorVariable = "Habitateignung Auerhuhn (Sommer)")
  )
}

build_two_level <- function(data, label_map, parent_syms, extra_leaves, selected_ziel) {
  ind <- setdiff(names(data), "Zeitpunkt_Maßnahme")
  leaves <- data %>%
    pivot_longer(dplyr::all_of(ind), names_to = "indikatorVariable", values_to = "thresholdValue") %>%
    dplyr::rename(className = Zeitpunkt_Maßnahme) %>%
    dplyr::left_join(label_map, by = c("indikatorVariable" = "Variable_Name")) %>%
    dplyr::filter(!is.na(ResultsLabel)) %>%
    dplyr::mutate(goal = detect_goal(ResultsLabel), indikatorVariable = ResultsLabel) %>%
    dplyr::select(goal, indikatorVariable, className, thresholdValue) %>%
    dplyr::filter(!is.na(goal))
  if (!is.null(extra_leaves) && nrow(extra_leaves) > 0)
    leaves <- dplyr::bind_rows(leaves, extra_leaves %>% dplyr::select(goal, indikatorVariable, className, thresholdValue))

  active <- GOAL_CONFIG$goal[GOAL_CONFIG$ziel_key %in% selected_ziel]
  active <- GOAL_CONFIG$goal[GOAL_CONFIG$goal %in% intersect(active, unique(c(leaves$goal, parent_syms$goal)))]

  out <- lapply(active, function(g) {
    pv <- parent_syms %>% dplyr::filter(goal == g) %>% dplyr::select(className, thresholdValue)
    kids <- leaves %>% dplyr::filter(goal == g) %>%
      dplyr::group_by(indikatorVariable) %>%
      dplyr::summarise(values = list(tibble::tibble(className = className, thresholdValue = thresholdValue)), .groups = "drop")
    list(indikatorVariable = GOAL_CONFIG$parent_label[GOAL_CONFIG$goal == g],
         values = pv,
         untergeordnete = lapply(seq_len(nrow(kids)), function(i)
           list(indikatorVariable = kids$indikatorVariable[i], values = kids$values[[i]])))
  })
  toJSON(out, pretty = TRUE, auto_unbox = TRUE)
}

tabRes <- build_two_level(df_cleaned, Ergebnis_Sortierung_filtered_all, parent_syms, extra_leaves, json_data$ziel)

# this is needed and must still be extended for further vectors, so that there is an output or it aborts beforehand
areaName <- json_data$areaName
if (length(areaName) == 0 || is.null(areaName) || is.na(areaName) || areaName == "") {
  areaName <- " "
}

require(glue)

# User-input table -------------------------------------------------------------

cfg <- read_excel("insert path to user_input_summary_config.xlsx") %>%
  mutate(
    input = as.character(input),
    value = as.character(value)
  )

map_value <- function(var, val, cfg_table) {

  if (length(val) == 0 || is.null(val) || all(is.na(val)) || identical(val, "")) {
    return(NA_character_)
  }

  val_chr <- as.character(val)[1]

  out <- cfg_table %>%
    filter(Variable_Name == var, input == val_chr) %>%
    pull(value)

  if (length(out) == 0 || is.na(out[1]) || out[1] == "") {
    return(val_chr)
  }

  out[1]
}

rows_list <- list()
vars <- unique(cfg$Variable_Name)

for (var in vars) {

  var_cfg <- cfg %>%
    filter(Variable_Name == var) %>%
    slice(1)

  render_type <- var_cfg$render_type
  label <- var_cfg$display_label
  order <- var_cfg$display_order
  show_if_missing <- var_cfg$show_if_missing

  if (render_type == "scalar") {

    raw_val <- json_data[[var]]
    val <- map_value(var, raw_val, cfg)

    rows_list[[length(rows_list) + 1]] <- tibble(
      label = label,
      value = val,
      order = order,
      show_if_missing = show_if_missing
    )
  }

  if (render_type == "vector") {

    raw_vals <- json_data[[var]]

    if (is.null(raw_vals) || length(raw_vals) == 0) {
      val <- NA_character_
    } else {
      pretty_vals <- vapply(
        raw_vals,
        function(x) map_value(var, x, cfg),
        character(1)
      )

      pretty_vals <- pretty_vals[!is.na(pretty_vals) & pretty_vals != ""]

      val <- if (length(pretty_vals) == 0) {
        NA_character_
      } else {
        glue_collapse(pretty_vals, sep = ", ", last = " und ")
      }
    }

    rows_list[[length(rows_list) + 1]] <- tibble(
      label = label,
      value = val,
      order = order,
      show_if_missing = show_if_missing
    )
  }

  if (render_type == "named_vector_percent") {

    raw_vals <- json_data[[var]]

    if (is.null(raw_vals) || length(raw_vals) == 0) {
      val <- NA_character_
    } else {
      ba_df <- tibble(
        input = names(raw_vals),
        Anteil = suppressWarnings(as.numeric(raw_vals))
      ) %>%
        filter(!is.na(Anteil), Anteil > 0) %>%
        mutate(
          Baumart = vapply(input, function(x) map_value(var, x, cfg), character(1))
        )

      val <- if (nrow(ba_df) == 0) {
        NA_character_
      } else {
        paste0(
          "<ul style='margin:0; padding-left:18px;'>",
          paste0(
            "<li>", ba_df$Baumart, ": ", ba_df$Anteil, " %</li>",
            collapse = ""
          ),
          "</ul>"
        )
      }
    }

    rows_list[[length(rows_list) + 1]] <- tibble(
      label = label,
      value = val,
      order = order,
      show_if_missing = show_if_missing
    )
  }
}

user_input_display <- bind_rows(rows_list) %>%
  distinct(order, label, .keep_all = TRUE) %>%
  arrange(order) %>%
  filter(!is.na(value) | show_if_missing == 1) %>%
  mutate(
    value = ifelse(is.na(value), "keine Angabe", value)
  )

table_rows <- paste0(
  "<tr>",
  "<td style='padding:6px; vertical-align:top;'>", user_input_display$label, "</td>",
  "<td style='padding:6px; vertical-align:top;'>", user_input_display$value, "</td>",
  "</tr>",
  collapse = ""
)

user_input_html <- paste0(
  "<details style='margin-top:12px;'>",
  "<summary style='cursor:pointer; font-weight:600;'>Ihre Eingaben einsehen</summary>",
  "<table style='margin-top:10px; border-collapse:collapse; width:100%;'>",
  "<tr>",
  "<th style='text-align:left; padding:6px; border-bottom:1px solid #ccc;'>Eingabe</th>",
  "<th style='text-align:left; padding:6px; border-bottom:1px solid #ccc;'>Wert</th>",
  "</tr>",
  table_rows,
  "</table>",
  "</details>"
)

# Result text (top) ------------------------------------------------------------

areaPolygonText <- glue(paste0(
  "<p>Basierend auf Ihren Eingaben und unseren Geodaten wurde Ihr ",
  "ausgew&auml;hlter Bestand <b>{areaName}</b> analysiert.</p>",
  user_input_html
))

print(123)

# Result text (table with data + PAS) ------------------------------------------

areaDetailText <- glue(paste0(
  "<p>Nachfolgend finden Sie eine Zusammenfassung der wichtigsten ",
  "Bestandesparameter sowie der berechneten Anf&auml;lligkeit gegen&uuml;ber ",
  "St&ouml;rungen (Pr&auml;disposition) in Ihrem Waldbestand. Alle Werte ",
  "beziehen sich auf den aktuellen Zustand des ausgew&auml;hlten Bestandes.<br>",
  "Eine hohe Schadanf&auml;lligkeit bzw. Pr&auml;disposition bedeutet nicht ",
  "zwangsl&auml;ufig, dass Sch&auml;den bereits auftreten, sondern weist auf ",
  "ein erh&ouml;htes Risiko hin. Sch&auml;den entstehen dann, wenn anf&auml;llige ",
  "Best&auml;nde mit einem entsprechenden Schadausl&ouml;ser zusammentreffen. ",
  "Ein hoher Pr&auml;dispositionswert kann daher auf Handlungsbedarf hinweisen.</p>"
))

print(345)

# Goals for the text on the results page ---------------------------------------

# create a df for the matching of name and label of the goals
Prädispo_Ziele <- c(
  "erhöh_widerstand",
  "habverb",
  "prädi_browsing",
  "prädi_strip",
  "prädi_cembrae",
  "prädi_armillaria",
  "prädi_barkbreed",
  "prädi_heterobasidion",
  "prädi_fire",
  "prädi_storm",
  "prädi_snow",
  "prädi_ips_typ",
  "Sägerundholzqualität"
)

Prädispo_Var_Name <- c(
  "Erhöhung der Widerstandsfähigkeit",
  "Habitatverbesserung für das Auerhuhn",
  "Reduktion der Prädisposition gegenüber Verbiss",
  "Reduktion der Prädisposition gegenüber Schäle",
  "Reduktion der Prädisposition gegenüber Lärchenborkenkäfern",
  "Reduktion der Prädisposition gegenüber Hallimasch",
  "Reduktion der Prädisposition gegenüber Rindenbrütern",
  "Reduktion der Prädisposition gegenüber Wurzelschwamm",
  "Reduktion der Prädisposition gegenüber Waldbränden",
  "Reduktion der Prädisposition gegenüber Windwurf",
  "Reduktion der Prädisposition gegenüber Schneebruch",
  "Reduktion der Prädisposition gegenüber Borkenkäferbefall (Buchdrucker)",
  "Holzproduktion in Sägerundholzqualität"
)

Ziele_Namen_DF <- as.data.frame(
  bind_cols("Ziele" = Prädispo_Ziele, "Namen" = Prädispo_Var_Name)
)

# filter the goals and only keep the names
User_Ziele_Namen_DF <- Ziele_Namen_DF %>%
  filter(Ziele %in% json_data$ziel)

User_Ziele_Namen_DF$Ziele <- NULL

ziele <- User_Ziele_Namen_DF$Namen
massnahmen <- utility_max_maßnahme$Zeitpunkt_Maßnahme

massnahmen <- glue_collapse(massnahmen, sep = ", ", last = " und ")
ziele <- glue_collapse(ziele, sep = ", ", last = " und ")

print(678)

# Result text traffic lights ---------------------------------------------------

tabularResultText <- glue(paste0(
  "<p>Nachfolgend werden die prognostizierten Auswirkungen der verschiedenen ",
  "Maßnahmenpakete auf die Erreichung Ihrer ausgewählten Ziele dargestellt. ",
  "Die berechnete Zielerreichung berücksichtigt auch weitere, hier nicht ",
  "angezeigte Indikatoren. Deshalb kann es Unterschiede zwischen den oben ",
  "dargestellten besten Maßnahmen (Kreise) und den unten gezeigten ",
  "Auswirkungen auf einzelne Indikatoren (Tabelle mit Ampeln und Pfeilen) ",
  "geben. Eine Maßnahme kann also insgesamt am besten zur Zielerreichung ",
  "beitragen, auch wenn sie bei einzelnen angezeigten Indikatoren schlechter ",
  "abschneidet. Ebenso führt eine Verbesserung eines einzelnen Indikators ",
  "nicht zwangsläufig zu einer rechnerisch besseren Zielerreichung. Gleich ",
  "bewertete Maßnahmen werden gemeinsam in einer Spalte dargestellt. ",
  "Weiterführende Erklärungen zu den Maßnahmen finden Sie im Tool unter dem ",
  "Reiter \"Erklärungen\".</p>",
  "<p>Bei den Ist-Werten zeigt ein gr&uuml;nes Symbol einen guten Zustand ",
  "an, ein gelbes Symbol einen m&auml;&szlig;igen Zustand und ein rotes ",
  "Symbol einen problematischen Zustand. Bei den Vorhersagen zeigt die ",
  "Richtung des Pfeils an, ob sich ein Wert verbessert, unverändert bleibt ",
  "oder verschlechtert. Die Farbe des Pfeils zeigt, auf welchen Zustand sich ",
  "der Wert entwickelt. Bei einem problematischen Ist-Zustand (rotes ",
  "Kreis-Symbol) würde ein gelber Pfeil nach oben bei Ma&szlig;nahme M1 ",
  "beispielsweise bedeuten, dass sich der Wert durch Durchführung dieser ",
  "Ma&szlig;nahme auf einen m&auml;&szlig;igen Zustand verbessert.</p>"
))

print(308)

# rename variables, keep only the relevant variables

# Load Ergebnisse and filter rows with ResultsLabel and table exactly "vars"
#Ergebnis_Sortierung <- read_excel("insert path to Ergebnisse_Variablenselektion_und_Benennung.xlsx")

#Ergebnis_Sortierung_filtered <- Ergebnis_Sortierung %>%
# filter(!is.na(ResultsLabel) & table == "vars")

# Results data table, incl. tree-species suitability. Renaming, sorting --------

areaDetail_df <- as_tibble(json_data$areaDetail)

# Extract species present in the polygon once
present_species <- names(json_data$baumartenVerteilung_Ist)[
  json_data$baumartenVerteilung_Ist > 0
]

# Helper: classify BA-Eignungen into 3 classes (0-100 scale)
classify_ba_eignung <- function(x) {
  case_when(
    is.na(x) ~ NA_character_,
    x < 50 ~ "gering",
    x < 80 ~ "moderat",
    TRUE ~ "hoch"
  )
}

# BA-Eignung variables
ba_eignung_vars <- c(
  "8585_Eig_BAh",
  "8585_Eig_Bi",
  "8585_Eig_Bu",
  "8585_Eig_BUl",
  "8585_Eig_Doug",
  "8585_Eig_Esch",
  "8585_Eig_Fichte",
  "8585_Eig_HBu",
  "8585_Eig_Ki",
  "8585_Eig_Kir",
  "8585_Eig_Lär",
  "8585_Eig_REi",
  "8585_Eig_SoLi",
  "8585_Eig_StEi",
  "8585_Eig_Tan",
  "8585_Eig_TrEi",
  "8585_Eig_WiLi",
  "8585_Eig_Zir"
)

areaDetail_updated <- areaDetail_df %>%
  left_join(
    Ergebnis_Sortierung_filtered_vars,
    by = c("indikatorVariable" = "Variable_Name")
  ) %>%
  mutate(
    ResultsLabel = case_when(
      ResultsLabel == "Standorteignung Bergahorn"    & !("Bergahorn" %in% present_species) ~ NA_character_,
      ResultsLabel == "Standorteignung Birke"        & !("Birke" %in% present_species) ~ NA_character_,
      ResultsLabel == "Standorteignung Buche"        & !("Buche" %in% present_species) ~ NA_character_,
      ResultsLabel == "Standorteignung Bergulme"     & !("Bergulme" %in% present_species) ~ NA_character_,
      ResultsLabel == "Standorteignung Douglasie"    & !("Douglasie" %in% present_species) ~ NA_character_,
      ResultsLabel == "Standorteignung Esche"        & !("Esche" %in% present_species) ~ NA_character_,
      ResultsLabel == "Standorteignung Fichte"       & !("Fichte" %in% present_species) ~ NA_character_,
      ResultsLabel == "Standorteignung Hainbuche"    & !("Hainbuche" %in% present_species) ~ NA_character_,
      ResultsLabel == "Standorteignung Kiefer"       & !("Kiefer" %in% present_species) ~ NA_character_,
      ResultsLabel == "Standorteignung Kirsche"      & !("Kirsche" %in% present_species) ~ NA_character_,
      ResultsLabel == "Standorteignung Lärche"       & !("Lärche" %in% present_species) ~ NA_character_,
      ResultsLabel == "Standorteignung Roteiche"     & !("Roteiche" %in% present_species) ~ NA_character_,
      ResultsLabel == "Standorteignung Sommerlinde"  & !("Sommerlinde" %in% present_species) ~ NA_character_,
      ResultsLabel == "Standorteignung Stieleiche"   & !("Stieleiche" %in% present_species) ~ NA_character_,
      ResultsLabel == "Standorteignung Tanne"        & !("Tanne" %in% present_species) ~ NA_character_,
      ResultsLabel == "Standorteignung Traubeneiche" & !("Traubeneiche" %in% present_species) ~ NA_character_,
      ResultsLabel == "Standorteignung Winterlinde"  & !("Winterlinde" %in% present_species) ~ NA_character_,
      ResultsLabel == "Standorteignung Zirbe"        & !("Zirbe" %in% present_species) ~ NA_character_,
      TRUE ~ ResultsLabel
    )
  ) %>%
  filter(!is.na(ResultsLabel)) %>%
  mutate(
    indikatorLabel = ResultsLabel,
    majority = as.character(majority)
  ) %>%
  select(-ResultsLabel)

# Load conversion table
user_data_conversion <- read_excel("insert path to user_data_conversion.xlsx") %>%
  mutate(input = as.character(input)) %>%
  distinct(Variable_Name, input, .keep_all = TRUE)

# Join to get converted majority labels
majority_converted <- areaDetail_updated %>%
  left_join(
    user_data_conversion,
    by = c("indikatorVariable" = "Variable_Name", "majority" = "input")
  ) %>%
  select(
    indikatorVariable,
    display_order,
    original_majority = majority,
    converted_majority = value
  ) %>%
  distinct(indikatorVariable, display_order, original_majority, .keep_all = TRUE)

# Merge back and update majority & mean
# - classify BA-Eignungen into gering / moderat / hoch
# - roads handling intentionally disabled
areaDetail_updated <- areaDetail_updated %>%
  left_join(
    majority_converted,
    by = c(
      "indikatorVariable",
      "display_order",
      "majority" = "original_majority"
    )
  ) %>%
  mutate(
    majority = if_else(
      # indikatorVariable == "roads" |   # intentionally disabled
      is.na(converted_majority),
      majority,
      converted_majority
    ),
    mean_num = round(as.numeric(mean), 2),
    mean = if_else(
      indikatorVariable %in% ba_eignung_vars,
      classify_ba_eignung(mean_num),
      as.character(mean_num)
    )
  ) %>%
  arrange(display_order) %>%
  select(-converted_majority, -mean_num, -display_order)

json_data$areaDetail <- areaDetail_updated

# Measures display (results) ---------------------------------------------------

titel <- c(
  # Dickung
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

  # Stangenholz
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

  # Baumholz
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
  "Starke Auslesedurchforstung und Reduktion des Nadelholzanteils um 10%",
  "Starke Auslesedurchforstung und Schaffen von Lücken",
  "Sehr starke Auslesedurchforstung",
  "Sehr starke Auslesedurchforstung und Reduktion des Fichtenanteils um 10%",
  "Sehr starke Auslesedurchforstung und Reduktion des Kiefernanteils um 10%",
  "Sehr starke Auslesedurchforstung und Reduktion des Lärchenanteils um 10%",
  "Sehr starke Auslesedurchforstung und Reduktion des Fichten- und Kiefernanteils um 10%",
  "Sehr starke Auslesedurchforstung und Reduktion des Kiefern- und/oder Fichten- und/oder Lärchenanteils um 10%",
  "Sehr starke Auslesedurchforstung und Reduktion des Nadelholzanteils um 10%",
  "Sehr starke Auslesedurchforstung und Schaffen von Lücken"
)

erklärung <- c(
  # Dickung
  "Es werden keine waldbaulichen Maßnahmen getroffen.",
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

  # Stangenholz
  "Es werden keine waldbaulichen Maßnahmen getroffen.",
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

  # Baumholz
  "Es werden keine waldbaulichen Maßnahmen getroffen.",
  "Entnahme von kranken, instabilen, besonders schlanken oder geschädigten Individuen der Kraft'schen Baumklassen 3 bis 5b (mitherrschend, beherrscht, unterständig).",
  "Förderung der Z-Baumkandidaten: Entnahme von 1 – 2 Bedrängern; Anlage von Rückegassen; keine Eingriffe im Nebenbestand. (Annahme: Die Überschirmung wird dadurch auf ≥ 90% herabgesetzt.)",
  "Förderung der Z-Baumkandidaten: Entnahme von 1 – 2 Bedrängern pro Z-Baumkandidat; Anlage von Rückegassen und Schaffen von Lücken >5m Durchmesser und 100m². (Annahme: Die Überschirmung wird dadurch ohne Lücken auf 90% herabgesetzt.)",
  "Förderung der Z-Baumkandidaten: Entnahme von 2 – 3 Bedrängern; Anlage von Rückegassen; Förderung der Mischbaumarten. (Annahme: Die Überschirmung wird dadurch auf ≥ 80% herabgesetzt.)",
  "Förderung der Z-Baumkandidaten durch Entnahme von 2 – 3 Bedrängern; Anlage von Rückegassen; Förderung der Mischbaumarten; Reduktion des Fichtenanteils um 10%. (Annahme: Die Überschirmung wird dadurch auf ≥ 80% herabgesetzt.)",
  "Förderung der Z-Baumkandidaten durch Entnahme von 2 – 3 Bedrängern; Förderung der Mischbaumarten; Reduktion des Kiefernanteils um 10% in der Oberschicht; Anlage von Rückegassen. (Annahme: Die Überschirmung wird dadurch auf ≥ 80% herabgesetzt.)",
  "Förderung der Z-Baumkandidaten durch Entnahme von 2 – 3 Bedrängern; Anlage von Rückegassen; Förderung der Mischbaumarten; Reduktion des Lärchenanteils um 10%. (Annahme: Die Überschirmung wird dadurch auf ≥ 80% herabgesetzt.)",
  "Förderung der Z-Baumkandidaten durch Entnahme von 2 – 3 Bedrängern; Anlage von Rückegassen; Förderung der Mischbaumarten; Reduktion des Fichten- und Kiefernanteils um 10% (Annahme: Die Überschirmung wird dadurch auf ≥ 80% herabgesetzt.)",
  "Förderung der Z-Baumkandidaten durch Entnahme von 2 – 3 Bedrängern; Förderung der Mischbaumarten; Reduktion des Kiefern- und/oder Fichten- und/oder Lärchenanteils um 10% in der Oberschicht; Anlage von Rückegassen. (Annahme: Die Überschirmung wird dadurch auf ≥ 80% herabgesetzt.)",
  "Förderung der Z-Baumkandidaten durch Entnahme von 2 – 3 Bedrängern; Anlage von Rückegassen; Förderung der Mischbaumarten; Reduktion des Nadelholzanteils um 10%. (Annahme: Die Überschirmung wird dadurch auf ≥ 80% herabgesetzt.)",
  "Förderung der Z-Baumkandidaten: Entnahme von 2 – 3 Bedrängern pro Z-Baumkandidat; Anlage von Rückegassen und Schaffung von Lücken >5m Durchmesser und 100m². (Annahme: Die Überschirmung wird dadurch ohne Lücken auf 80% herabgesetzt.)",
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
  length(alle_voll) == length(titel),
  length(alle_voll) == length(erklärung)
)

# Create data frame with 3 columns
maßnahme_matching <- data.frame(
  alle_voll = alle_voll,
  cleaned = gsub("IST\\+10/", "", alle_voll),
  titel = titel,
  erklärung = erklärung,
  stringsAsFactors = FALSE
)

# Gauge-needle display ---------------------------------------------------------

tabRes <- fromJSON(tabRes)

massEvalAnzeige <- utility_alle_massnahmen %>%
  select(massnahme = Zeitpunkt_Maßnahme, value = Zielerreichung) %>%
  left_join(
    maßnahme_matching %>%
      mutate(sort_order = seq_len(n())),
    by = c("massnahme" = "alle_voll")
  ) %>%
  mutate(
    massnahme = cleaned,
    value = round(value, 2),
    definition = paste0(
      titel,
      " – ", erklärung
    )
  ) %>%
  arrange(desc(value), sort_order) %>%
  select(massnahme, value, definition)

ziele <- User_Ziele_Namen_DF$Namen
massnahmen <- utility_max_maßnahme$Zeitpunkt_Maßnahme

massnahmen <- maßnahme_matching %>%
  filter(alle_voll %in% massnahmen) %>%
  left_join(
    utility_max_maßnahme,
    by = c("alle_voll" = "Zeitpunkt_Maßnahme")
  ) %>%
  arrange(desc(Zielerreichung), alle_voll) %>%
  mutate(
    label = paste0(titel, " (", cleaned, ")")
  ) %>%
  pull(label)

massnahmen <- glue_collapse(massnahmen, sep = ", ", last = " und ")
ziele <- glue_collapse(ziele, sep = ", ", last = " und ")

# Gauge-needle text ------------------------------------------------------------

massnahmeEvaluationText <- glue(paste0(
  "<p>F&uuml;r Ihren gew&auml;hlten Bestand <b>{areaName}</b> ergibt sich, ",
  "dass die Ma&szlig;nahme/n <b>{massnahmen}</b> am besten geeignet ist/sind, ",
  "um Ihr/e Ziel/e <b>{ziele}</b> zu erreichen. Die Bewertung stellt keine ",
  "betriebswirtschaftliche Kalkulation dar, sondern basiert auf ",
  "waldbaulichen Indikatoren unter Ber&uuml;cksichtigung der ",
  "Schadanf&auml;lligkeit. Der angezeigte Wert beschreibt, in welchem ",
  "Ausma&szlig; ein Maßnahmenpaket zur Erreichung des gew&auml;hlten Ziels ",
  "bzw. der gew&auml;hlten Zielkombination beitr&auml;gt.</p>"
))


# Build JSON -------------------------------------------------------------------

# Convert the data to a JSON file
json_data_export <- toJSON(list(
  displayTexts = list(
    areaPolygonText = areaPolygonText,
    areaDetailText = areaDetailText,
    areaDetailPredisposition = json_data$ziel_html,
    tabularResultText = tabularResultText,
    massnahmeEvaluationText = massnahmeEvaluationText
  ),
  areaPolygon = json_data$areaPolygon,
  tabularResult = tabRes,
  massnahmeEvaluation = massEvalAnzeige,

  # Debug data for internal inspection
  `_debug` = list(
    timestamp = as.character(Sys.time()),
    areaName = areaName,
    json_input = json_data1,
    zielerreichung_raw = Zielerreichung_Waldbauliches_Vergleichsbestand1,
    tabularResult = tabRes,
    massnahmeEvaluation = massEvalAnzeige
  )

), auto_unbox = TRUE, na = "null", pretty = TRUE)


writeLines(json_data_export, OUTPUT_FILE)

}, error = function(e) {
    # On error, create a JSON object with the error message
    error_list <- list(error = TRUE, message = e$message)

    # Convert to JSON string
    json_error <- toJSON(error_list, pretty = TRUE, auto_unbox = TRUE)

    # Write JSON error info to file
    writeLines(json_error, OUTPUT_FILE)

    # Optional: print to console or log
    message("An error occurred. Details written to ", OUTPUT_FILE)
  })
