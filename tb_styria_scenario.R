# -----------------------------------------------------------------------------
# Created by Jörg Fabian Knufinke, assistance and further development by
# Max Bodanowitz. Last edited 09/2026.
# Assistance of generative AI was used in writing these scripts.

# Recommended Citation: 
# Knufinke, J.F. & Bodanowitz, M. (2026) DECIDE Tool - 
# Entscheidungsfindung für klimaangepassten, präventiven Forstschutz. Code to execute the DECIDE Tool (Version Version1.1) 
# [Computer software]. Zenodo. https://doi.org/10.5281/zenodo.22252774
# -----------------------------------------------------------------------------

# tb_styria_scenario.R
# Scenario recalculation for the Styria training area
# - reads hidden context from current_tb
# - compares edited stand parameters to geodata defaults
# - computes result_tb only
# - writes only result_tb
# - exits quickly on init-triggered runs
# - suppresses duplicate echo-runs via scenario fingerprint

# Setup ------------------------------------------------------------------------

require(jsonlite)

CONFIG_PATH <- "insert path to DECIDE-R-CONFIG.json"
CONFIG <- fromJSON(CONFIG_PATH)
DATA_DIRECTORY <- CONFIG$DATA_DIRECTORY

args <- commandArgs(trailingOnly = TRUE)
INPUT_FILE <- args[1]
OUTPUT_FILE <- args[2]

pdf(NULL)

source("insert path to r_functions directory/fun.make_donut_auerhuhn.R")
source("insert path to r_functions directory/fun.make_donut.R")

require(tidyverse)

# Helpers ----------------------------------------------------------------------

read_input_json <- function(path) {
  fromJSON(path)
}

write_output_json <- function(x, path) {
  json_output <- toJSON(x, pretty = TRUE, na = "null", auto_unbox = TRUE)
  writeLines(json_output, path)
}

is_blank_value <- function(x) {
  is.null(x) || length(x) == 0 || all(trimws(as.character(x)) == "")
}

same_value <- function(a, b) {
  identical(as.character(a), as.character(b))
}

# tolerate both:
# <!--TB_CTX:{...}-->
# TB_CTX:{...}-->
extract_hidden_context_comment <- function(x) {
  if (is.null(x) || length(x) == 0) return(NULL)

  m1 <- regexpr("<!--TB_CTX:(.*?)-->", x, perl = TRUE)
  if (m1[1] != -1) {
    raw_match <- regmatches(x, m1)
    return(sub("^<!--TB_CTX:", "", sub("-->$", "", raw_match)))
  }

  m2 <- regexpr("TB_CTX:(.*?)-->", x, perl = TRUE)
  if (m2[1] != -1) {
    raw_match <- regmatches(x, m2)
    return(sub("^TB_CTX:", "", sub("-->$", "", raw_match)))
  }

  NULL
}

read_hidden_context <- function(current_tb) {
  ctx_json <- extract_hidden_context_comment(current_tb)
  if (is.null(ctx_json) || identical(ctx_json, "")) return(NULL)

  tryCatch(
    jsonlite::fromJSON(ctx_json, simplifyVector = FALSE),
    error = function(e) NULL
  )
}

restore_geodata_context <- function(ctx) {
  if (is.null(ctx) || length(ctx) == 0) {
    stop("geodata_context missing or empty")
  }
  for (nm in names(ctx)) {
    assign(nm, ctx[[nm]], envir = .GlobalEnv)
  }
}

convert_canopy_to_user_scale <- function(x) {
  dplyr::case_when(
    x >= 95 ~ 97.5,
    x >= 86 ~ 90.5,
    x >= 66 ~ 75.5,
    x >= 46 ~ 55.5,
    x >= 30 ~ 37.5,
    x < 30  ~ 30,
    TRUE    ~ x
  )
}

map_forest_class_back <- function(val) {
  dplyr::case_when(
    val == "Blöße" ~ 1,
    val == "krautiger Bewuchs (> 20 – 50 cm)" ~ 2,
    val == "Jungwuchs (> 50 – 200 cm)" ~ 3,
    val == "Dickung (> 2 – 6 m)" ~ 4,
    val == "Stangenholz (> 6 – 20 m)" ~ 5,
    val == "Baumholz (> 20 m)" ~ 6,
    val == "ungleichaltrig" ~ 7,
    TRUE ~ NA_real_
  )
}

map_canopy_back <- function(val) {
  dplyr::case_when(
    val == "gedrängt"    ~ 97.5,
    val == "geschlossen" ~ 90.5,
    val == "locker"      ~ 75.5,
    val == "licht"       ~ 55.5,
    val == "räumdig"     ~ 40.5,
    val == "Blöße"       ~ 30,
    TRUE ~ NA_real_
  )
}

map_conif_back <- function(val) {
  dplyr::case_when(
    val == "keine Waldfläche oder Baumart nicht im Bestand" ~ -1,
    val == "<10%"    ~ 0,
    val == "10–39%"  ~ 1,
    val == "40–59%"  ~ 2,
    val == "60–89%"  ~ 3,
    val == "90–100%" ~ 4,
    TRUE ~ NA_real_
  )
}

set_not_relevant_if_missing <- function(var1_name, var2_name) {
  var1_exists <- exists(var1_name, envir = .GlobalEnv, inherits = TRUE)
  var2_exists <- exists(var2_name, envir = .GlobalEnv, inherits = TRUE)

  if (!var1_exists || !var2_exists) {
    assign(var1_name, "not relevant", envir = .GlobalEnv)
    assign(var2_name, "not relevant", envir = .GlobalEnv)
  }
}

safe_source <- function(script_path) {
  source(script_path, local = TRUE)
}

# scenario fingerprint helpers -------------------------------------------------

make_scenario_fingerprint <- function(forest_class_tb, canopy_tb, conif_tb) {
  paste(
    as.character(forest_class_tb),
    as.character(canopy_tb),
    as.character(conif_tb),
    sep = "||"
  )
}

make_hidden_scenario_comment <- function(fingerprint) {
  paste0("<!--TB_SCEN:", fingerprint, "-->")
}

extract_hidden_scenario_comment <- function(x) {
  if (is.null(x) || length(x) == 0) return(NULL)

  m1 <- regexpr("<!--TB_SCEN:(.*?)-->", x, perl = TRUE)
  if (m1[1] != -1) {
    raw_match <- regmatches(x, m1)
    return(sub("^<!--TB_SCEN:", "", sub("-->$", "", raw_match)))
  }

  m2 <- regexpr("TB_SCEN:(.*?)-->", x, perl = TRUE)
  if (m2[1] != -1) {
    raw_match <- regmatches(x, m2)
    return(sub("^TB_SCEN:", "", sub("-->$", "", raw_match)))
  }

  NULL
}

strip_hidden_scenario_comment <- function(x) {
  if (is.null(x) || length(x) == 0) return("")
  x <- gsub("<!--TB_SCEN:.*?-->", "", x, perl = TRUE)
  x <- gsub("TB_SCEN:.*?-->", "", x, perl = TRUE)
  x
}

has_init_marker <- function(x) {
  if (is.null(x) || length(x) == 0) return(FALSE)
  grepl("<!--TB_INIT:1-->|TB_INIT:1-->", x, perl = TRUE)
}

strip_init_marker <- function(x) {
  if (is.null(x) || length(x) == 0) return("")
  x <- gsub("<!--TB_INIT:1-->", "", x, perl = TRUE)
  x <- gsub("TB_INIT:1-->", "", x, perl = TRUE)
  x
}

# Main -------------------------------------------------------------------------

json_data <- tryCatch(
  read_input_json(INPUT_FILE),
  error = function(e) {
    write_output_json(
      list(result_tb = paste0(
        "<div style='padding:12px; color:#a1a1a9; text-align:left;'>",
        "Could not read input JSON: ", e$message,
        "</div>"
      )),
      OUTPUT_FILE
    )
    stop(e$message)
  }
)

if (is.null(json_data$current_tb)) json_data$current_tb <- ""
if (is.null(json_data$result_tb)) json_data$result_tb <- ""

# quiet no-op on incomplete input
if (is.null(json_data$polygon_tb) || length(json_data$polygon_tb) == 0 ||
    is.null(json_data$pas_tb) || length(json_data$pas_tb) == 0) {
  quit(save = "no")
}

if (is_blank_value(json_data$forest_class_tb) ||
    is_blank_value(json_data$canopy_tb) ||
    is_blank_value(json_data$conif_tb)) {
  quit(save = "no")
}

tb_ctx <- read_hidden_context(json_data$current_tb)

# quiet no-op if context is missing
if (is.null(tb_ctx)) {
  quit(save = "no")
}

previous_scenario_fingerprint <- extract_hidden_scenario_comment(json_data$result_tb)

current_scenario_fingerprint <- make_scenario_fingerprint(
  json_data$forest_class_tb,
  json_data$canopy_tb,
  json_data$conif_tb
)

# fast exit for init-triggered scenario run:
# same stand values as fingerprint + init marker present
if (has_init_marker(json_data$result_tb) &&
    !is.null(previous_scenario_fingerprint) &&
    identical(previous_scenario_fingerprint, current_scenario_fingerprint)) {
  quit(save = "no")
}

# suppress duplicate echo-runs after a real scenario write
if (!is.null(previous_scenario_fingerprint) &&
    identical(previous_scenario_fingerprint, current_scenario_fingerprint) &&
    !has_init_marker(json_data$result_tb)) {
  quit(save = "no")
}

result <- tryCatch({

  restore_geodata_context(tb_ctx$geodata_context)

  geodata_forest_class <- forest_class
  geodata_canopy <- convert_canopy_to_user_scale(canopy)
  geodata_conif <- conif

  forest_class <- map_forest_class_back(json_data$forest_class_tb)
  ips_stand_class <- forest_class

  canopy <- map_canopy_back(json_data$canopy_tb)
  fallback_canopy <- ifelse(canopy == 40.5, 37.5, canopy)
  ips_canopy_cover <- fallback_canopy
  snow_canopy_cover <- fallback_canopy
  storm_canopy_cover <- fallback_canopy

  conif <- map_conif_back(json_data$conif_tb)
  fallback_conif <- ifelse(conif == -1, 0, conif)
  snow_coniferous_proportion <- fallback_conif

  if (exists("broad")) {
    broad <- dplyr::case_when(
      is.na(conif) ~ NA_real_,
      conif == -1 ~ -1,
      TRUE ~ 4 - conif
    )
  }

  set_not_relevant_if_missing("geodata_conif", "conif")
  set_not_relevant_if_missing("geodata_canopy", "canopy")
  set_not_relevant_if_missing("geodata_forest_class", "forest_class")

  if (!exists("geodata_conif", envir = .GlobalEnv)) geodata_conif <- NA_character_
  if (!exists("geodata_canopy", envir = .GlobalEnv)) geodata_canopy <- NA_character_
  if (!exists("geodata_forest_class", envir = .GlobalEnv)) geodata_forest_class <- NA_character_

  if (!exists("conif", envir = .GlobalEnv)) conif <- NA_character_
  if (!exists("canopy", envir = .GlobalEnv)) canopy <- NA_character_
  if (!exists("forest_class", envir = .GlobalEnv)) forest_class <- NA_character_

  user_changed_parameters <- !(
    same_value(geodata_conif, conif) &&
    same_value(geodata_canopy, canopy) &&
    same_value(geodata_forest_class, forest_class)
  )

  # no actual user change relative to geodata baseline
  if (!user_changed_parameters) {
    quit(save = "no")
  }

  new_result_tb <- ""

  if ("habverb" %in% json_data$pas_tb) {
    safe_source("insert path to PAS scripts directory/pas_11_capercaillie_backend_SERVER.R")
    if (exists("res_capercaillie_combined")) {
      html_output_auerhuhn_p <- make_donut_auerhuhn(
        res_capercaillie_combined, "Habitateignung Auerhuhn (gesamt)",
        res_capercaillie_sum, "Habitateignung Auerhuhn (Sommer)",
        res_capercaillie_win, "Habitateignung Auerhuhn (Winter)"
      )
      new_result_tb <- paste0(new_result_tb, html_output_auerhuhn_p)
    }
  }

  if ("prädi_browsing" %in% json_data$pas_tb) {
    safe_source("insert path to PAS scripts directory/pas_1_browsing_backend_SERVER.R")
    if (exists("res_browsing_combined")) {
      html_circles_1 <- make_two_donuts(
        res_browsing_combined, "Prädisposition Verbiss (gesamt)",
        res_browsing_stand, "Prädisposition Verbiss (Bestand)"
      )
      html_output_browsing_p <- sprintf(
        '<div style="background: transparent; padding: 20px; text-align:center;">%s</div>',
        html_circles_1
      )
      new_result_tb <- paste0(new_result_tb, html_output_browsing_p)
    }
  }

  if ("prädi_strip" %in% json_data$pas_tb) {
    safe_source("insert path to PAS scripts directory/pas_2_barkstripping_backend_SERVER.R")
    if (exists("res_barkstripping_combined")) {
      html_circles_2 <- make_two_donuts(
        res_barkstripping_combined, "Prädisposition Schäle (gesamt)",
        res_barkstripping_stand, "Prädisposition Schäle (Bestand)"
      )
      html_output_strip_p <- sprintf(
        '<div style="background: transparent; padding: 20px; text-align:center;">%s</div>',
        html_circles_2
      )
      new_result_tb <- paste0(new_result_tb, html_output_strip_p)
    }
  }

  if ("prädi_cembrae" %in% json_data$pas_tb) {
    safe_source("insert path to PAS scripts directory/pas_3_cembrae_backend_SERVER.R")
    if (exists("res_cembrae_combined")) {
      html_circles_3 <- make_two_donuts(
        res_cembrae_combined, "Prädisposition Lärchenborkenkäfer (gesamt)",
        res_cembrae_stand, "Prädisposition Lärchenborkenkäfer (Bestand)"
      )
      html_output_cembrae_p <- sprintf(
        '<div style="background: transparent; padding: 20px; text-align:center;">%s</div>',
        html_circles_3
      )
      new_result_tb <- paste0(new_result_tb, html_output_cembrae_p)
    }
  }

  if ("prädi_armillaria" %in% json_data$pas_tb) {
    safe_source("insert path to PAS scripts directory/pas_4_armillaria_backend_SERVER.R")
    if (exists("res_armillaria_combined")) {
      html_circles_4 <- make_two_donuts(
        res_armillaria_combined, "Prädisposition Armillaria (gesamt)",
        res_armillaria_stand, "Prädisposition Armillaria (Bestand)"
      )
      html_output_armillaria_p <- sprintf(
        '<div style="background: transparent; padding: 20px; text-align:center;">%s</div>',
        html_circles_4
      )
      new_result_tb <- paste0(new_result_tb, html_output_armillaria_p)
    }
  }

  if ("prädi_barkbreed" %in% json_data$pas_tb) {
    safe_source("insert path to PAS scripts directory/pas_5_barkbreeding_backend_SERVER.R")
    if (exists("res_barkbreeding_combined")) {
      html_circles_5 <- make_two_donuts(
        res_barkbreeding_combined, "Prädisposition Rindenbrüter (gesamt)",
        res_barkbreeding_stand, "Prädisposition Rindenbrüter (Bestand)"
      )
      html_output_barkbreed_p <- sprintf(
        '<div style="background: transparent; padding: 20px; text-align:center;">%s</div>',
        html_circles_5
      )
      new_result_tb <- paste0(new_result_tb, html_output_barkbreed_p)
    }
  }

  if ("prädi_heterobasidion" %in% json_data$pas_tb) {
    safe_source("insert path to PAS scripts directory/pas_6_heterobasidion_backend_SERVER.R")
    if (exists("res_heterobasidion_combined")) {
      html_circles_6 <- make_two_donuts(
        res_heterobasidion_combined, "Prädisposition Heterobasidion (gesamt)",
        res_heterobasidion_stand, "Prädisposition Heterobasidion (Bestand)"
      )
      html_output_hetbas_p <- sprintf(
        '<div style="background: transparent; padding: 20px; text-align:center;">%s</div>',
        html_circles_6
      )
      new_result_tb <- paste0(new_result_tb, html_output_hetbas_p)
    }
  }

  if ("prädi_fire" %in% json_data$pas_tb) {
    safe_source("insert path to PAS scripts directory/pas_7_fire_backend_SERVER.R")
    if (exists("res_fire_combined")) {
      html_circles_7 <- make_two_donuts(
        res_fire_combined, "Prädisposition Feuer (gesamt)",
        res_fire_stand, "Prädisposition Feuer (Bestand)"
      )
      html_output_fire_p <- sprintf(
        '<div style="background: transparent; padding: 20px; text-align:center;">%s</div>',
        html_circles_7
      )
      new_result_tb <- paste0(new_result_tb, html_output_fire_p)
    }
  }

  if ("prädi_storm" %in% json_data$pas_tb) {
    safe_source("insert path to PAS scripts directory/pas_8_storm_backend_SERVER.R")
    if (exists("res_storm_combined")) {
      html_circles_8 <- make_two_donuts(
        res_storm_combined, "Prädisposition Sturm (gesamt)",
        res_storm_stand, "Prädisposition Sturm (Bestand)"
      )
      html_output_storm_p <- sprintf(
        '<div style="background: transparent; padding: 20px; text-align:center;">%s</div>',
        html_circles_8
      )
      new_result_tb <- paste0(new_result_tb, html_output_storm_p)
    }
  }

  if ("prädi_snow" %in% json_data$pas_tb) {
    safe_source("insert path to PAS scripts directory/pas_9_snow_backend_SERVER.R")
    if (exists("res_snow_combined")) {
      html_circles_9 <- make_two_donuts(
        res_snow_combined, "Prädisposition Schnee (gesamt)",
        res_snow_stand, "Prädisposition Schnee (Bestand)"
      )
      html_output_snow_p <- sprintf(
        '<div style="background: transparent; padding: 20px; text-align:center;">%s</div>',
        html_circles_9
      )
      new_result_tb <- paste0(new_result_tb, html_output_snow_p)
    }
  }

  if ("prädi_ips_typ" %in% json_data$pas_tb) {
    safe_source("insert path to PAS scripts directory/pas_10_barkbeetle_backend_SERVER.R")
    if (exists("res_ips_combined")) {
      html_circles_10 <- make_two_donuts(
        res_ips_combined, "Prädisposition Buchdrucker (gesamt)",
        res_ips_stand, "Prädisposition Buchdrucker (Bestand)"
      )
      html_output_ips_typ_p <- sprintf(
        '<div style="background: transparent; padding: 20px; text-align:center;">%s</div>',
        html_circles_10
      )
      new_result_tb <- paste0(new_result_tb, html_output_ips_typ_p)
    }
  }

  new_result_tb <- paste0(
    strip_init_marker(strip_hidden_scenario_comment(new_result_tb)),
    make_hidden_scenario_comment(current_scenario_fingerprint)
  )

  # no write if result didn't really change
  if (identical(json_data$result_tb, new_result_tb)) {
    quit(save = "no")
  }

  new_result_tb

}, error = function(e) {
  paste0(
    "<div style='padding:12px; color:#a1a1a9; text-align:left;'>",
    "tb_styria_scenario.R failed: ", e$message,
    "</div>"
  )
})

# partial update only
write_output_json(list(result_tb = result), OUTPUT_FILE)
