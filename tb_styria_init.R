# -----------------------------------------------------------------------------
# Created by Jörg Fabian Knufinke, assistance and further development by
# Max Bodanowitz. Last edited 09/2026.
# Assistance of generative AI was used in writing these scripts.

# Recommended Citation: 
# Knufinke, J.F. & Bodanowitz, M. (2026) DECIDE Tool - 
# Entscheidungsfindung für klimaangepassten, präventiven Forstschutz. Code to execute the DECIDE Tool (Version Version1.1) 
# [Computer software]. Zenodo. https://doi.org/10.5281/zenodo.22252774
# -----------------------------------------------------------------------------

# tb_styria_init.R
# Initializes the Styria training area
# - reads polygon and selected goals
# - extracts geodata
# - computes baseline/current PAS values
# - prefills editable stand parameters
# - embeds technical context into current_tb for scenario script
# - writes only owned fields (partial update)

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

require(terra)
require(sf)
require(sp)
require(tidyverse)
require(readxl)

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

make_polygon_signature <- function(polygon_tb) {
  if (is.null(polygon_tb)) return("")
  polygon_df <- as.data.frame(polygon_tb)
  if (nrow(polygon_df) == 0) return("")
  if (!all(c("lat", "lng") %in% names(polygon_df))) {
    stop("polygon_tb must contain columns 'lat' and 'lng'")
  }

  pts <- paste0(
    formatC(as.numeric(polygon_df$lat), digits = 8, format = "f"),
    "_",
    formatC(as.numeric(polygon_df$lng), digits = 8, format = "f")
  )
  paste(pts, collapse = "|")
}

make_goals_signature <- function(pas_tb) {
  if (is.null(pas_tb) || length(pas_tb) == 0) return("")
  paste(sort(as.character(unlist(pas_tb))), collapse = "|")
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

convert_forest_class <- function(val) {
  switch(
    as.character(val),
    "1" = "Blöße",
    "2" = "krautiger Bewuchs (> 20 – 50 cm)",
    "3" = "Jungwuchs (> 50 – 200 cm)",
    "4" = "Dickung (> 2 – 6 m)",
    "5" = "Stangenholz (> 6 – 20 m)",
    "6" = "Baumholz (> 20 m)",
    "7" = "ungleichaltrig",
    as.character(val)
  )
}

convert_canopy_label <- function(val) {
  dplyr::case_when(
    is.na(val)         ~ "nicht verfügbar",
    val >= 95          ~ "gedrängt",
    val >= 86          ~ "geschlossen",
    val >= 66          ~ "locker",
    val >= 46          ~ "licht",
    val >= 30          ~ "räumdig",
    val < 30           ~ "Blöße",
    TRUE               ~ as.character(val)
  )
}

convert_conif <- function(val) {
  switch(
    as.character(val),
    "-1" = "keine Waldfläche oder Baumart nicht im Bestand",
    "0"  = "<10%",
    "1"  = "10–39%",
    "2"  = "40–59%",
    "3"  = "60–89%",
    "4"  = "90–100%",
    as.character(val)
  )
}

store_geodata_context <- function(input_stand_values) {
  ctx <- list()
  for (i in seq_len(nrow(input_stand_values))) {
    var_name <- input_stand_values$indikatorVariable[i]
    val_mean <- input_stand_values$mean[i]
    val_maj <- input_stand_values$majority[i]
    val <- if (!is.na(val_mean)) val_mean else val_maj
    ctx[[var_name]] <- val
  }
  ctx
}

make_hidden_context_comment <- function(ctx_list) {
  ctx_json <- jsonlite::toJSON(ctx_list, auto_unbox = TRUE, null = "null")
  paste0("<!--TB_CTX:", ctx_json, "-->")
}

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

strip_hidden_context_comment <- function(x) {
  if (is.null(x) || length(x) == 0) return("")
  x <- gsub("<!--TB_CTX:.*?-->", "", x, perl = TRUE)
  x <- gsub("TB_CTX:.*?-->", "", x, perl = TRUE)
  x
}

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

fun_read_rename <- function(df, variable_name_column, file_name_column, test_path) {
  df <- df %>% filter(!is.na(.data[[variable_name_column]]) & !is.na(.data[[file_name_column]]))

  if (nrow(df) == 0) {
    stop("No valid rows to process. Check for NA values in the input data.")
  }

  for (i in seq_len(nrow(df))) {
    variable_name <- df[[variable_name_column]][i]
    file_name <- df[[file_name_column]][i]
    file_path <- file.path(test_path, "TIF", file_name)

    if (!exists(variable_name, where = .GlobalEnv)) {
      tryCatch({
        raster_data <- terra::rast(file_path)
        assign(variable_name, raster_data, envir = .GlobalEnv)
      }, error = function(e) {
        warning(paste("Raster for", variable_name, "could not be loaded from file:", file_name, conditionMessage(e)))
      })
    }
  }
}

mean_majority <- function(cropped_raster, Categorical_Vars) {
  all_var_ras_stack_df <- terra::as.data.frame(cropped_raster)

  input_stand_values <- data.frame(
    indikatorVariable = character(),
    mean = character(),
    majority = character(),
    stringsAsFactors = FALSE
  )

  for (var_name in names(all_var_ras_stack_df)) {
    mean_element_name <- NA
    majority_element_name <- NA

    if (tolower(var_name) %in% tolower(Categorical_Vars)) {
      element_table <- table(all_var_ras_stack_df[[var_name]])
      if (length(element_table) > 0) {
        majority_element_name <- names(element_table)[which.max(element_table)]
      } else {
        majority_element_name <- NA_character_
      }
    } else {
      if (!all(is.na(all_var_ras_stack_df[[var_name]]))) {
        mean_element_name <- round(mean(all_var_ras_stack_df[[var_name]], na.rm = TRUE), 2)
      } else {
        mean_element_name <- NA_real_
      }
    }

    temp_df <- data.frame(
      indikatorVariable = var_name,
      mean = mean_element_name,
      majority = majority_element_name,
      stringsAsFactors = FALSE
    )

    input_stand_values <- rbind(input_stand_values, temp_df)
  }

  input_stand_values
}

# Main -------------------------------------------------------------------------

json_data <- read_input_json(INPUT_FILE)

if (is.null(json_data$current_tb)) json_data$current_tb <- ""

if (is.null(json_data$polygon_tb) || length(json_data$polygon_tb) == 0 ||
    is.null(json_data$pas_tb) || length(json_data$pas_tb) == 0) {
  quit(save = "no")
}

old_ctx <- read_hidden_context(json_data$current_tb)

old_polygon_signature <- if (!is.null(old_ctx$polygon_signature)) as.character(old_ctx$polygon_signature) else ""
old_goals_signature <- if (!is.null(old_ctx$goals_signature)) as.character(old_ctx$goals_signature) else ""

current_polygon_signature <- make_polygon_signature(json_data$polygon_tb)
current_goals_signature <- make_goals_signature(json_data$pas_tb)

polygon_changed <- !identical(old_polygon_signature, current_polygon_signature)
goals_changed <- !identical(old_goals_signature, current_goals_signature)
baseline_changed <- polygon_changed || goals_changed || is_blank_value(strip_hidden_context_comment(json_data$current_tb))

if (!baseline_changed) {
  quit(save = "no")
}

result <- tryCatch({

  polygon_df <- as.data.frame(json_data$polygon_tb)

  if (!all(c("lat", "lng") %in% names(polygon_df))) {
    stop("polygon_tb must contain lat and lng")
  }

  user_input_points <- polygon_df[, c("lat", "lng")]
  User_Ziele <- json_data$pas_tb

  Ziele_PAS_Input_Daten <- read_excel(
    "insert path to input data directory/Variable_Daten_Ziel_TB_styria.xlsx",
    trim_ws = TRUE
  )
  Ziele_PAS_Input_Daten <- dplyr::filter(Ziele_PAS_Input_Daten, file_Name != "NA")

  ziel_cols <- User_Ziele

  User_Ziele_Variablen <- Ziele_PAS_Input_Daten %>%
    filter(rowSums(across(all_of(ziel_cols), ~ .x == 1)) > 0) %>%
    select(Variable_Name, indikatorLabel, file_Name)

  fun_read_rename(User_Ziele_Variablen, "Variable_Name", "file_Name", "insert path to data directory/")

  raster_names <- User_Ziele_Variablen$Variable_Name
  all_var_ras_stack <- list()

  for (raster_name in raster_names) {
    if (exists(raster_name, envir = .GlobalEnv)) {
      raster_obj <- get(raster_name, envir = .GlobalEnv)
      all_var_ras_stack[[raster_name]] <- raster_obj
    }
  }

  for (Variable_name in names(all_var_ras_stack)) {
    terra::varnames(all_var_ras_stack[[Variable_name]]) <- Variable_name
  }

  user_input_sf <- st_as_sf(user_input_points, coords = c("lng", "lat"), crs = 4326)
  user_input_transformed <- st_transform(user_input_sf, crs = 3416)
  user_input_transformed <- vect(user_input_transformed)

  user_input_shape <- user_input_transformed %>%
    st_as_sf(coords = c("lng", "lat")) %>%
    st_combine() %>%
    st_cast("POLYGON")

  user_input_shape <- vect(user_input_shape)
  user_input_shape <- rast(user_input_shape)

  cut_to_extent <- function(raster_obj, extent_shape) {
    if (!inherits(raster_obj, "SpatRaster")) {
      stop("Input must be a SpatRaster object")
    }
    terra::crop(raster_obj, extent_shape)
  }

  cropped_raster <- purrr::map(all_var_ras_stack, cut_to_extent, extent_shape = user_input_shape)
  cropped_raster <- rast(cropped_raster)

  Categorical_Vars <- Ziele_PAS_Input_Daten %>%
    filter(Typ == "Kategorisch") %>%
    select(Variable_Name) %>%
    distinct() %>%
    pull(Variable_Name)

  input_stand_values <- mean_majority(cropped_raster, Categorical_Vars)

  for (var in input_stand_values$indikatorVariable) {
    val_mean <- input_stand_values$mean[input_stand_values$indikatorVariable == var]
    val_maj <- input_stand_values$majority[input_stand_values$indikatorVariable == var]
    val <- if (!is.na(val_mean)) val_mean else val_maj
    assign(var, val, envir = .GlobalEnv)
  }

  new_current_tb <- ""

  if ("habverb" %in% json_data$pas_tb) {
    html_output_auerhuhn <- make_donut_auerhuhn(
      PAS_11_capercaillie, "Habitateignung Auerwild (gesamt)",
      PAS_11_capercaillie_summer, "Habitateignung Auerwild (Sommer)",
      PAS_11_capercaillie_winter, "Habitateignung Auerwild (Winter)"
    )
    html_output_auerhuhn <- sprintf(
      '<div style="background: transparent; padding: 20px; text-align:center;">%s</div>',
      html_output_auerhuhn
    )
    new_current_tb <- paste0(new_current_tb, html_output_auerhuhn)
  }

  if ("prädi_browsing" %in% json_data$pas_tb) {
    html_circles_1 <- paste0(
      make_donut(PAS_1_browsing, "Prädisposition Verbiss (gesamt)"),
      make_donut(PAS_1_browsing_stand, "Prädisposition Verbiss (Bestand)")
    )
    html_output_browsing <- sprintf(
      '<div style="background: transparent; padding: 0px; text-align:center;">%s</div>',
      html_circles_1
    )
    new_current_tb <- paste0(new_current_tb, html_output_browsing)
  }

  if ("prädi_strip" %in% json_data$pas_tb) {
    html_circles_2 <- paste0(
      make_donut(PAS_2_barkstripping, "Prädisposition Schäle (gesamt)"),
      make_donut(PAS_2_barkstripping_stand, "Prädisposition Schäle (Bestand)")
    )
    html_output_strip <- sprintf(
      '<div style="background: transparent; padding: 20px; text-align:center;">%s</div>',
      html_circles_2
    )
    new_current_tb <- paste0(new_current_tb, html_output_strip)
  }

  if ("prädi_cembrae" %in% json_data$pas_tb) {
    html_circles_3 <- paste0(
      make_donut(PAS_3_cembrae, "Prädisposition Lärchenborkenkäfer (gesamt)"),
      make_donut(PAS_3_cembrae_stand, "Prädisposition Lärchenborkenkäfer (Bestand)")
    )
    html_output_cembrae <- sprintf(
      '<div style="background: transparent; padding: 20px; text-align:center;">%s</div>',
      html_circles_3
    )
    new_current_tb <- paste0(new_current_tb, html_output_cembrae)
  }

  if ("prädi_armillaria" %in% json_data$pas_tb) {
    html_circles_4 <- paste0(
      make_donut(PAS_4_armillaria, "Prädisposition Armillaria (gesamt)"),
      make_donut(PAS_4_armillaria_stand, "Prädisposition Armillaria (Bestand)")
    )
    html_output_armillaria <- sprintf(
      '<div style="background: transparent; padding: 20px; text-align:center;">%s</div>',
      html_circles_4
    )
    new_current_tb <- paste0(new_current_tb, html_output_armillaria)
  }

  if ("prädi_barkbreed" %in% json_data$pas_tb) {
    html_circles_5 <- paste0(
      make_donut(PAS_5_barkbreed, "Prädisposition Rindenbrüter (gesamt)"),
      make_donut(PAS_5_barkbreed_stand, "Prädisposition Rindenbrüter (Bestand)")
    )
    html_output_barkbreed <- sprintf(
      '<div style="background: transparent; padding: 20px; text-align:center;">%s</div>',
      html_circles_5
    )
    new_current_tb <- paste0(new_current_tb, html_output_barkbreed)
  }

  if ("prädi_heterobasidion" %in% json_data$pas_tb) {
    html_circles_6 <- paste0(
      make_donut(PAS_6_heterobasidion, "Prädisposition Heterobasidion (gesamt)"),
      make_donut(PAS_6_heterobasidion_stand, "Prädisposition Heterobasidion (Bestand)")
    )
    html_output_hetbas <- sprintf(
      '<div style="background: transparent; padding: 20px; text-align:center;">%s</div>',
      html_circles_6
    )
    new_current_tb <- paste0(new_current_tb, html_output_hetbas)
  }

  if ("prädi_fire" %in% json_data$pas_tb) {
    html_circles_7 <- paste0(
      make_donut(PAS_7_fire, "Prädisposition Feuer (gesamt)"),
      make_donut(PAS_7_fire_stand, "Prädisposition Feuer (Bestand)")
    )
    html_output_fire <- sprintf(
      '<div style="background: transparent; padding: 20px; text-align:center;">%s</div>',
      html_circles_7
    )
    new_current_tb <- paste0(new_current_tb, html_output_fire)
  }

  if ("prädi_storm" %in% json_data$pas_tb) {
    html_circles_8 <- paste0(
      make_donut(PAS_8_storm, "Prädisposition Sturm (gesamt)"),
      make_donut(PAS_8_storm_stand, "Prädisposition Sturm (Bestand)")
    )
    html_output_storm <- sprintf(
      '<div style="background: transparent; padding: 20px; text-align:center;">%s</div>',
      html_circles_8
    )
    new_current_tb <- paste0(new_current_tb, html_output_storm)
  }

  if ("prädi_snow" %in% json_data$pas_tb) {
    html_circles_9 <- paste0(
      make_donut(PAS_9_snow, "Prädisposition Schnee (gesamt)"),
      make_donut(PAS_9_snow_stand, "Prädisposition Schnee (Bestand)")
    )
    html_output_snow <- sprintf(
      '<div style="background: transparent; padding: 20px; text-align:center;">%s</div>',
      html_circles_9
    )
    new_current_tb <- paste0(new_current_tb, html_output_snow)
  }

  if ("prädi_ips_typ" %in% json_data$pas_tb) {
    html_circles_10 <- paste0(
      make_donut(PAS_10_ips, "Prädisposition Buchdrucker (gesamt)"),
      make_donut(PAS_10_ips_stand, "Prädisposition Buchdrucker (Bestand)")
    )
    html_output_ips <- sprintf(
      '<div style="background: transparent; padding: 20px; text-align:center;">%s</div>',
      html_circles_10
    )
    new_current_tb <- paste0(new_current_tb, html_output_ips)
  }

  geodata_forest_class <- forest_class
  geodata_canopy <- convert_canopy_to_user_scale(canopy)
  geodata_conif <- conif

  anzeige_forest_class <- convert_forest_class(geodata_forest_class)
  anzeige_canopy <- convert_canopy_label(geodata_canopy)
  anzeige_conif <- convert_conif(geodata_conif)

  new_ausgangswerte <- paste0(
    "<table style='border-collapse:collapse;'>",
    "<tr><td style='padding-right:15px;'><strong>Wuchsklasse:</strong></td><td>", as.character(anzeige_forest_class), "</td></tr>",
    "<tr><td style='padding-right:15px;'><strong>Kronenschlussgrad:</strong></td><td>", as.character(anzeige_canopy), "</td></tr>",
    "<tr><td style='padding-right:15px;'><strong>Nadelholzanteil:</strong></td><td>", as.character(anzeige_conif), "</td></tr>",
    "</table>"
  )

  ausgangswerte_intro <- "<p style='margin-bottom:10px;'>Die untenstehenden Werte sind die Ausgangswerte, auf denen die aktuelle Anfälligkeit bzw. Zielerreichung basiert. Diese sind für den ersten von Ihnen ausgewählten Bestand unten voreingetragen. Wählen Sie andere Werte aus, um den Einfluss der Änderungen auf die Anfälligkeit bzw. Zielerreichung nachzuvollziehen.</p>"

  ausgangswerte_block <- paste0(
    "<div style='text-align:left; padding: 10px;'>",
    ausgangswerte_intro,
    new_ausgangswerte,
    "</div>"
  )

  new_current_tb <- paste0(new_current_tb, ausgangswerte_block)

  new_forest_class_tb <- convert_forest_class(forest_class)
  new_canopy_tb <- convert_canopy_label(canopy)
  new_conif_tb <- convert_conif(conif)

  init_scenario_fingerprint <- make_scenario_fingerprint(
    new_forest_class_tb,
    new_canopy_tb,
    new_conif_tb
  )

  new_result_tb <- paste0(
    "<div style='padding:12px; color:#a1a1a9; text-align:left;'>",
    "Keine Veränderung an Bestandesparametern durch den Benutzer.",
    "</div>",
    make_hidden_scenario_comment(init_scenario_fingerprint),
    "<!--TB_INIT:1-->"
  )

  tb_ctx <- list(
    polygon_signature = current_polygon_signature,
    goals_signature = current_goals_signature,
    geodata_context = store_geodata_context(input_stand_values)
  )

  new_current_tb <- paste0(
    strip_hidden_context_comment(new_current_tb),
    make_hidden_context_comment(tb_ctx)
  )

  list(
    current_tb = new_current_tb,
    forest_class_tb = new_forest_class_tb,
    canopy_tb = new_canopy_tb,
    conif_tb = new_conif_tb,
    result_tb = new_result_tb
  )

}, error = function(e) {
  list(
    result_tb = paste0(
      "<div style='padding:12px; color:#a1a1a9; text-align:left;'>",
      "tb_styria_init.R failed: ", e$message,
      "</div>"
    )
  )
})

write_output_json(result, OUTPUT_FILE)
