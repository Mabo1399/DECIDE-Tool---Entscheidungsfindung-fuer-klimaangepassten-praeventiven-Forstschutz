# -----------------------------------------------------------------------------
# Created by Jörg Fabian Knufinke, assistance and further development by
# Max Bodanowitz. Last edited 09/2026.
# Assistance of generative AI was used in writing these scripts.

# Recommended Citation: 
# Knufinke, J.F. & Bodanowitz, M. (2026) DECIDE Tool - 
# Entscheidungsfindung für klimaangepassten, präventiven Forstschutz. Code to execute the DECIDE Tool (Version Version1.1) 
# [Computer software]. Zenodo. https://doi.org/10.5281/zenodo.22252774
# -----------------------------------------------------------------------------

# Script to generate the user-input summary [usereingaben: "user inputs"]
# for the DECIDE results page. Builds from the user-input JSON a
# formatted HTML string and writes it to OUTPUT_FILE.
# Input needed: configuration file and user-input JSON with relevant key factors

# Setup ------------------------------------------------------------------------

require(jsonlite)

# fallback for R < 4.4.0: the %||% operator is only in 'base' from R 4.4
# and is NOT provided by 'jsonlite'. Without this fallback
# the script aborts further below at the canopy-closure line with
# 'could not find function "%||%"' is raised (this script runs in the questions
# damage/crown length etc. alone -> otherwise no output would appear there).
if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# Use configuration files so data can be read in flexibly
# config file is located in the same directory
CONFIG_PATH <- "insert path to DECIDE-R-CONFIG.json"

# read JSON config and set variables
CONFIG <- fromJSON(CONFIG_PATH)

# path to the directory that holds the TIF files & example data
DATA_DIRECTORY <- CONFIG$DATA_DIRECTORY

args = commandArgs(trailingOnly = TRUE)

# holds parameters for this script (user-input JSON)
INPUT_FILE <- args[1]

# holds the script result (user-input HTML)
OUTPUT_FILE <- args[2]

# prevent creation of the rplots.pdf file
pdf(NULL)


# Test paths (uncomment if needed)
#INPUT_FILE  <- "insert path to input JSON"
#OUTPUT_FILE <- "insert path to output JSON"

json_data <- fromJSON(INPUT_FILE)

json_data$Kronenschlussgrad <-
  json_data$Kronenschlussgrad_Di %||%
  json_data$Kronenschlussgrad_BH %||%
  json_data$Kronenschlussgrad_StH


# usereingaben ["user inputs"]: HTML summary of the user inputs --------------------------------
# Builds a formatted HTML string from json_data (variable 'usereingaben' ["user inputs"]).
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

## Write result
result_list <- list(usereingaben = usereingaben)
result_json <- toJSON(result_list, auto_unbox = TRUE, pretty = TRUE, na = "null")
writeLines(result_json, OUTPUT_FILE)
