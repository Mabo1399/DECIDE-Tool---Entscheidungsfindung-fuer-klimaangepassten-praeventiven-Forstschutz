# -----------------------------------------------------------------------------
# Created by Jörg Fabian Knufinke, assistance and further development by
# Max Bodanowitz. Last edited 09/2026.
# Assistance of generative AI was used in writing these scripts.

# Recommended Citation: 
# Knufinke, J.F. & Bodanowitz, M. (2026) DECIDE Tool - 
# Entscheidungsfindung für klimaangepassten, präventiven Forstschutz. Code to execute the DECIDE Tool (Version Version1.1) 
# [Computer software]. Zenodo. https://doi.org/10.5281/zenodo.22252774
# -----------------------------------------------------------------------------

# Script to communicate between the DECIDE tool interface and the R backend of the tool
# Input needed: configuration file and user-input JSON with relevant key factors

# Setup ------------------------------------------------------------------------

require(jsonlite)

# Use configuration files so data can be read in flexibly
# config file is located in the same directory
CONFIG_PATH <- "insert path to DECIDE-R-CONFIG.json"

# read JSON config and set variables
CONFIG <- fromJSON(CONFIG_PATH)

# path to the directory that holds the TIF files & example data
DATA_DIRECTORY <- CONFIG$DATA_DIRECTORY

args = commandArgs(trailingOnly = TRUE)

# holds parameters for this script (the data from the user polygon)
INPUT_FILE <- args[1]

# holds the script result (various tree variables for the user polygon)
OUTPUT_FILE <- args[2]

# prevent creation of the rplots.pdf file
pdf(NULL)

# 1) Select goals --------------------------------------------------------------

#INPUT_FILE <- "insert path to input JSON"
#OUTPUT_FILE <- "insert path to output JSON"

## convert the point from JSON to polygon
json_data <- fromJSON(INPUT_FILE)

keep_vars <- c("json_data", "INPUT_FILE", "OUTPUT_FILE")
rm(list = setdiff(ls(), keep_vars))

# sp removed: nothing in this script calls into it (terra/sf cover all the
# raster and vector work below) - it was pure startup-time overhead.
require(terra)
require(sf)
require(tidyverse)
require(readxl)

# return an empty json object until we have all required input parameters

if (length(json_data$areaPolygon) == 0) {
  writeLines("{}", OUTPUT_FILE)
  stop()
}

if (length(json_data$ziel) == 0) {
  writeLines("{}", OUTPUT_FILE)
  stop()
}

# extract polygon coordinates from JSON
user_input_points <- as.data.frame(json_data$areaPolygon)[, c("lng", "lat")]

User_Ziele <- json_data$ziel # use the goals that the user defined


Ziele_PAS_Input_Daten <- read_excel(
  "insert path to input data directory/Variable_Daten_Ziel_styria.xlsx",
  trim_ws = TRUE
)
Ziele_PAS_Input_Daten <- dplyr::filter(Ziele_PAS_Input_Daten, file_Name != "NA")

ziel_cols <- User_Ziele

# If "erhöh_widerstand" is included, it must be treated as if all prädi_--- goals were selected
if ("erhöh_widerstand" %in% ziel_cols) {
  praedi_cols <- grep("^prädi_", colnames(Ziele_PAS_Input_Daten), value = TRUE)
  ziel_cols <- unique(c(ziel_cols, praedi_cols))
}

# if col is in the goals and == 1, take name and label
User_Ziele_Variablen <- Ziele_PAS_Input_Daten %>%
  filter(rowSums(across(all_of(ziel_cols), ~ .x == 1)) > 0) %>%
  select(Variable_Name, indikatorLabel, file_Name)

# check for a mismatch between goal names in the tool and the backend
# if yes -> write error file for processing by the web application
if (length(User_Ziele_Variablen$Variable_Name) == 0) {
  error_json <- toJSON(
    list(
      userZiele = User_Ziele,
      errorMessage = "Mindestens eines der gewählten Ziele konnte nicht korrekt verarbeitet werden. Sollte dieser Fehler auch bei der Wahl von anderen Zielen bestehen, kontaktieren Sie uns bitte."
    ),
    auto_unbox = TRUE,
    pretty = TRUE,
    na = "null"
  )
  
  writeLines(error_json, OUTPUT_FILE)
  stop()
}

# retrieve geodata (function)
fun_read_rename <- function(df, variable_name_column, file_name_column, test_path) {
  
  # remove incomplete rows
  df <- df %>% filter(!is.na(.data[[variable_name_column]]) & !is.na(.data[[file_name_column]]))
  
  # check whether complete rows remain
  if (nrow(df) == 0) {
    stop("No valid rows to process. Check for NA values in the input data.")
  }
  
  raster_list <- list()
  
  # loop over all rows
  for (i in seq_len(nrow(df))) {
    
    # extract variables and file names
    variable_name <- df[[variable_name_column]][i]
    file_name <- df[[file_name_column]][i]
    file_path <- file.path(test_path, "TIF", file_name)
    
    # load, error message if the file is missing
    raster_data <- tryCatch({
      terra::rast(file_path)
    }, error = function(e) {
      warning(paste("Raster for", variable_name, "could not be loaded from file:", file_name, "\n", conditionMessage(e)))
      NULL
    })
    
    if (!is.null(raster_data)) {
      raster_list[[variable_name]] <- raster_data
    }
  }
  
  raster_list
}

# retrieve geodata (execution)
all_var_ras_stack <- fun_read_rename(
  User_Ziele_Variablen,
  "Variable_Name",
  "file_Name",
  "insert path to data directory/"
)

if (length(all_var_ras_stack) == 0) {
  writeLines("{}", OUTPUT_FILE)
  stop("No raster layers available after loading.")
}

# 2) Select and check the administrative unit ----------------------------------

# 2.1) user draws a polygon or uploads a polygon

# check polygon
if (nrow(user_input_points) < 3) {
  writeLines("{}", OUTPUT_FILE)
  stop("Polygon must contain at least 3 vertices.")
}

if (any(is.na(user_input_points$lng)) || any(is.na(user_input_points$lat))) {
  writeLines("{}", OUTPUT_FILE)
  stop("Polygon contains missing coordinates.")
}

# close the polygon if necessary
if (!identical(user_input_points[1, ], user_input_points[nrow(user_input_points), ])) {
  user_input_points <- rbind(user_input_points, user_input_points[1, ])
}

# create polygon in WGS84
user_input_shape <- sf::st_sfc(
  sf::st_polygon(list(as.matrix(user_input_points))),
  crs = 4326
)

# check CRS consistency of the rasters
all_crs <- vapply(all_var_ras_stack, terra::crs, character(1))
if (length(unique(all_crs)) > 1) {
  stop("Not all rasters use the same CRS.")
}

# transform the polygon into the raster CRS
target_crs <- all_crs[1]
user_input_shape <- sf::st_transform(user_input_shape, target_crs)

# convert to a terra vector
user_input_shape_vect <- terra::vect(user_input_shape)

# 1.3) cropping all existing variables to the user-input polygon, to save time and space

# first crop to the polygon bounding box, then mask to the polygon
cut_to_polygon <- function(raster_obj, polygon_vect) {
  if (!inherits(raster_obj, "SpatRaster")) {
    stop("Input must be a SpatRaster object")
  }
  
  cropped_raster <- terra::crop(raster_obj, polygon_vect)
  masked_raster <- terra::mask(cropped_raster, polygon_vect)
  
  return(masked_raster)
}

# If errors occur here, clear the environment and it will work again

# Apply the cutting operation to each SpatRaster in all_var_ras_stack
cropped_raster <- purrr::map(
  all_var_ras_stack,
  cut_to_polygon,
  polygon_vect = user_input_shape_vect
)

Categorical_Vars <- Ziele_PAS_Input_Daten %>%
  filter(Typ == "Kategorisch") %>%
  select(Variable_Name) %>%
  distinct() %>%
  pull(Variable_Name)

mean_majority <- function(cropped_raster_list, Categorical_Vars) {
  
  result_rows <- vector("list", length(cropped_raster_list))
  i <- 1
  
  for (var_name in names(cropped_raster_list)) {
    raster_obj <- cropped_raster_list[[var_name]]
    
    if (is.null(raster_obj)) {
      next
    }
    
    vals <- terra::values(raster_obj, mat = FALSE, na.rm = FALSE)
    
    # safety: if several columns are returned, use the first column.
    # the variable name is deliberately taken from the list name,
    # so that it stays exactly as stable as in the old workflow.
    if (is.matrix(vals)) {
      vals <- vals[, 1]
    }
    
    mean_element_name <- NA_real_
    majority_element_name <- NA_character_
    
    if (tolower(var_name) %in% tolower(Categorical_Vars)) {
      vals_no_na <- vals[!is.na(vals)]
      
      if (length(vals_no_na) > 0) {
        element_table <- table(vals_no_na)
        majority_element_name <- names(element_table)[which.max(element_table)]
      } else {
        majority_element_name <- NA_character_
      }
      
    } else {
      if (!all(is.na(vals))) {
        mean_element_name <- round(mean(vals, na.rm = TRUE), 2)
      } else {
        mean_element_name <- NA_real_
      }
    }
    
    result_rows[[i]] <- data.frame(
      indikatorVariable = var_name,
      mean = mean_element_name,
      majority = majority_element_name,
      stringsAsFactors = FALSE
    )
    
    i <- i + 1
  }
  
  result_rows <- result_rows[!vapply(result_rows, is.null, logical(1))]
  
  if (length(result_rows) == 0) {
    return(data.frame(
      indikatorVariable = character(),
      mean = numeric(),
      majority = character(),
      stringsAsFactors = FALSE
    ))
  }
  
  dplyr::bind_rows(result_rows)
}

# this is only the values for the geodata!
input_stand_values <- mean_majority(cropped_raster, Categorical_Vars)


# re-attach indikatorLabel + showInTable to areaDetail:
#   - showInTable   -> for the areaDetail question in the web interface
#   - indikatorLabel -> for the fusion script (BA_Eignung: filter(indikatorLabel %in% ...))
# only this basic functionality (no more majority/value translation).
# NOTE: adjust the path to the actual location of the function if necessary.
source("insert path to r_functions directory/fun.process_input_stand_values.R")
input_stand_values <- process_input_stand_values(input_stand_values, Ziele_PAS_Input_Daten)


# forest class
# this script runs (on the app side) only when the polygon changes
# (the corresponding question triggers the rScript). Therefore the forest class
# is re-derived from the geodata on EVERY run and thereby overwritten:
#   a) polygon set for the first time  -> derive from geodata
#   c) polygon changed                 -> re-derive from geodata (overwrite)
#   b) only forest class changed       -> script does not run -> user values kept
# (canopy closure is deliberately NO longer derived/written here;
# a separate script handles that.)

# first level or NA (geodata lookups can return 0 hits)
first_or_na <- function(x) if (length(x) >= 1) x[[1]] else NA

# forest class: forest_class code -> text (hard-coded). Only Dickung /
# Stangenholz / sw Baumholz are output; everything else (1,2,3,7, missing)
# stays empty (NA -> null).
forest_class_map <- c(
  "1" = "unbewachsen",
  "2" = "krautiger Bewuchs",
  "3" = "Jungwuchs",
  "4" = "Dickung",
  "5" = "Stangenholz",
  "6" = "sw Baumholz",
  "7" = "ungleichaltrig"
)
allowed_wuchsklassen <- c("Dickung", "Stangenholz", "sw Baumholz")

# always derive the forest class from forest_class
fc_code <- suppressWarnings(as.numeric(first_or_na(
  input_stand_values$majority[input_stand_values$indikatorVariable == "forest_class"]
)))
wk <- unname(forest_class_map[as.character(fc_code)])
Wuchsklasse <- if (length(wk) == 1 && !is.na(wk) && wk %in% allowed_wuchsklassen) {
  wk
} else {
  NA_character_
}

# Write result

# serialize the areaDetail data as before (same toJSON options as for
# the overall output) so they can be reconstructed 1:1 later.
areaDetail_json <- toJSON(input_stand_values, auto_unbox = TRUE, pretty = TRUE, na = "null")

# areaDetailText: visible success message (white on green) + invisible
# embedded areaDetail data. The <script type="application/json"> element
# is not rendered in the browser but is kept in the string.
areaDetailText <- paste0(
  '<div style="color:#ffffff;background-color:#758e44;padding:8px 12px;border-radius:4px;">',
  'Die Geodaten zu Ihrem Bestand wurden erfolgreich abgerufen.',
  '</div>',
  '<script type="application/json" id="areaDetail">',
  areaDetail_json,
  '</script>'
)


# Builds the HTML string 'usereingaben' from json_data. Included below in result_list
# included so that the user-input summary is updated at EVERY step
# is updated. Missing fields are omitted automatically.
# -> use config section to change output (ue_labels etc.),
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

result_list <- list(
  areaDetailText = areaDetailText,
  Wuchsklasse    = Wuchsklasse
)

# if "Saegerundholzqualitaet" was not selected as a goal, output a note.
if (!("Sägerundholzqualität" %in% User_Ziele)) {
  result_list$Anzeigetext_keinSRH <-
    '<p>Weitere Angaben zu Ihrem gewählten Bestand sind nicht erforderlich, da Sie "Holzproduktion in Sägerundholzqualität" nicht als Ziel ausgewählt haben.</p>'
}

result_list$usereingaben <- usereingaben

result_json <- toJSON(result_list, auto_unbox = TRUE, pretty = TRUE, na = "null")
writeLines(result_json, OUTPUT_FILE)
