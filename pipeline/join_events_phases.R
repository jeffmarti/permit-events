# ==============================================================================
# join_events_phases.R
# ------------------------------------------------------------------------------
# Joins two WA Ecology WRTS exports on WR Doc ID:
#   1. EVENT export  : General (Status = Active) + Event tab
#                      (Open; Begin Const / Complete Const / Proof of Approp;
#                       Event date type = None)  -> has event details, no TRS
#   2. PHASE export  : General (Status = Active; Phase = Change-ROE, Permit,
#                      Quincy Basin Permit, Superseding Permit, Superseding
#                      Quincy Basin Permit, Temporary Use Permit) -> has TRS
#
# Keeps ONLY phase records whose WR Doc ID also appears in the event export,
# and adds the event fields (Event, Event Status, Due Date, Done Date).
#
# OUTPUT is written in the same layout as Ecology's "Print Version" CSV
# (8 header lines, then column names), so it loads directly into the
# WA Water Rights PLSS Geocoder app with no changes.
#
# FOLDER LAYOUT (run from the permit-events project root, the folder with app.R):
#   pipeline/join_events_phases.R   <- this script
#   pipeline/raw/                   <- put BOTH downloaded .xlsx exports here
#   pipeline/work/                  <- joined CSV is written here (created automatically)
# Run with:  source("pipeline/join_events_phases.R")
# Next step: geocode the joined CSV, then run pipeline/prepare_app_data.R
#
# You do NOT need to rename the downloads. The script opens every .xlsx in
# pipeline/raw/, decides which one is the event export (has an "Event Status"
# column) and which is the phase export (has a "TRS" column), and uses the
# newest file of each kind.
#
# REQUIRED PACKAGES: tidyverse, readxl   (readxl installs with tidyverse)
# ==============================================================================

library(tidyverse)
library(readxl)

# ==============================================================================
# SETTINGS  — the only part you should normally need to touch
# ==============================================================================

RAW_DIR <- "pipeline/raw"
OUT_DIR <- "pipeline/work"

# How to handle a water right that has MORE THAN ONE open event
# (e.g. both Begin Construction and Complete Construction are overdue):
#   "collapse" = one row per phase record; multiple events are combined into
#                one cell separated by "; "  (no duplicate points on the map)
#   "long"     = one row per phase record PER event (duplicate points)
EVENT_LAYOUT <- "collapse"

# Write the 8 header lines so the file drops straight into the geocoder app?
GEOCODER_READY <- TRUE

# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================

# Normalize a column name for matching: "WR Doc ID" -> "wrdocid"
norm <- function(x) str_to_lower(str_remove_all(x, "[^A-Za-z0-9]"))

# Find a column in df whose normalized name matches one of `candidates`.
# Stops with a clear message listing the actual columns if none match.
find_col <- function(df, candidates, label) {
  hit <- names(df)[norm(names(df)) %in% norm(candidates)]
  if (length(hit) == 0) {
    stop("Could not find the ", label, " column. Columns in this file are:\n  ",
         paste(names(df), collapse = " | "), call. = FALSE)
  }
  hit[1]
}

# Read one WRTS export.
#  - Uses the "Print Version" sheet when present (same sheet the app uses),
#    otherwise the first sheet.
#  - Finds the header row automatically (the row containing "WR Doc ID"),
#    so it doesn't matter whether there are 8 header lines or some other number.
#  - Reads every column as text, so IDs keep leading zeros etc.
read_wrts_export <- function(path) {
  sheets <- excel_sheets(path)
  sheet  <- sheets[str_detect(sheets, regex("print", ignore_case = TRUE))][1]
  if (is.na(sheet)) sheet <- sheets[1]

  peek <- read_excel(path, sheet = sheet, col_names = FALSE, n_max = 40,
                     col_types = "text", .name_repair = "minimal")
  hdr_row <- which(apply(peek, 1, function(r) any(norm(r) == "wrdocid")))[1]
  if (is.na(hdr_row)) {
    stop("No 'WR Doc ID' header found in the first 40 rows of ", basename(path),
         " (sheet '", sheet, "').", call. = FALSE)
  }

  df <- read_excel(path, sheet = sheet, skip = hdr_row - 1,
                   col_types = "text", .name_repair = "unique_quiet")

  # Drop completely empty rows, and empty UNNAMED columns ("...12") that Excel
  # exports sometimes carry. Named columns are kept even when empty — e.g.
  # Done Date is blank for every open event but is still a real field.
  df <- df |>
    filter(if_any(everything(), ~ !is.na(.x) & .x != "")) |>
    select(!(starts_with("...") & where(~ all(is.na(.x)))))

  # Ecology appends the search criteria BELOW the data (e.g. "Role | Primary",
  # "Doc Status | Active"). Those footer lines land in the first two columns,
  # so "Primary" / "Active" look like WR Doc IDs and can falsely match between
  # the two files. Real WR Doc IDs are all digits — keep only those rows.
  id_col  <- names(df)[norm(names(df)) == "wrdocid"][1]
  is_real <- str_detect(str_trim(df[[id_col]]), "^\\d+$") %in% TRUE
  if (any(!is_real)) {
    message("  ", basename(path), ": dropped ", sum(!is_real),
            " non-record row(s) (criteria footer / blanks): ",
            paste(head(unique(df[[1]][!is_real]), 6), collapse = ", "))
  }
  df <- df[is_real, ]

  attr(df, "header_lines") <- read_excel(path, sheet = sheet, col_names = FALSE,
                                         n_max = hdr_row - 1, col_types = "text",
                                         .name_repair = "minimal")
  attr(df, "source_file")  <- basename(path)
  attr(df, "sheet")        <- sheet
  df
}

# Excel dates read as text come through as serial numbers ("44561").
# Convert those to the m/d/Y text the geocoder app expects; leave real
# text dates ("12/31/2021") alone.
fix_excel_dates <- function(x) {
  is_serial <- !is.na(x) & str_detect(x, "^\\d{5}(\\.\\d+)?$")
  x[is_serial] <- format(as.Date(as.numeric(x[is_serial]), origin = "1899-12-30"),
                         "%m/%d/%Y")
  x
}

# ==============================================================================
# STEP 1 — Find and classify the two exports
# ==============================================================================

files <- list.files(RAW_DIR, pattern = "\\.xlsx$", full.names = TRUE)
files <- files[!str_detect(basename(files), "^~\\$")]   # skip Excel lock files
if (length(files) == 0) stop("No .xlsx files found in '", RAW_DIR, "/'.")

catalog <- tibble(path = files, mtime = file.mtime(files)) |>
  mutate(
    data = map(path, read_wrts_export),
    cols = map(data, ~ norm(names(.x))),
    kind = map_chr(cols, function(cn) {
      if ("eventstatus" %in% cn) "event"
      else if ("trs" %in% cn)    "phase"
      else                       "unknown"
    })
  )

message("Files found in ", RAW_DIR, "/:")
walk(seq_len(nrow(catalog)), function(i) {
  message("  ", basename(catalog$path[i]), "  ->  ", catalog$kind[i],
          "  (", nrow(catalog$data[[i]]), " rows, modified ",
          format(catalog$mtime[i], "%Y-%m-%d %H:%M"), ")")
})

pick_newest <- function(k) {
  hit <- catalog |> filter(kind == k) |> arrange(desc(mtime))
  if (nrow(hit) == 0) stop("No ", k, " export found in '", RAW_DIR, "/'.",
                           call. = FALSE)
  hit$data[[1]]
}

events_raw <- pick_newest("event")
phases_raw <- pick_newest("phase")

message("\nUsing EVENT file: ", attr(events_raw, "source_file"),
        "  (sheet '", attr(events_raw, "sheet"), "')")
message("Using PHASE file: ", attr(phases_raw, "source_file"),
        "  (sheet '", attr(phases_raw, "sheet"), "')")

# ==============================================================================
# STEP 2 — Prepare the event table (only the ID + 4 event fields)
# ==============================================================================

ev_id   <- find_col(events_raw, c("WR Doc ID", "WR_Doc_ID"),  "WR Doc ID (events)")
ev_evt  <- find_col(events_raw, c("Event"),                   "Event")
ev_stat <- find_col(events_raw, c("Event Status"),            "Event Status")
ev_due  <- find_col(events_raw, c("Due Date"),                "Due Date")
# Done Date is optional: open events never have one, and if Ecology ever
# leaves the column out of the export we just fill it with blanks.
ev_done <- tryCatch(find_col(events_raw, c("Done Date"), "Done Date"),
                    error = function(e) NA_character_)
if (is.na(ev_done)) {
  message("Note: no Done Date column in event file — filling with blanks.")
  events_raw$`Done Date` <- NA_character_
  ev_done <- "Done Date"
}

events <- events_raw |>
  transmute(
    wr_doc_id      = str_trim(.data[[ev_id]]),
    `Event`        = .data[[ev_evt]],
    `Event Status` = .data[[ev_stat]],
    `Due Date`     = fix_excel_dates(.data[[ev_due]]),
    `Done Date`    = fix_excel_dates(.data[[ev_done]])
  ) |>
  filter(!is.na(wr_doc_id), wr_doc_id != "") |>
  distinct()                                   # drop exact duplicate event rows

if (EVENT_LAYOUT == "collapse") {
  events <- events |>
    mutate(due_sort = as.Date(`Due Date`, format = "%m/%d/%Y")) |>
    arrange(wr_doc_id, due_sort) |>
    group_by(wr_doc_id) |>
    summarise(
      `Event`             = paste(Event, collapse = "; "),
      `Event Status`      = paste(unique(`Event Status`), collapse = "; "),
      `Due Date`          = paste(coalesce(`Due Date`, ""), collapse = "; "),
      `Done Date`         = paste(coalesce(`Done Date`, ""), collapse = "; "),
      `Earliest Due Date` = if (all(is.na(due_sort))) NA_character_ else
                              format(min(due_sort, na.rm = TRUE), "%m/%d/%Y"),
      `N Open Events`     = n(),
      .groups = "drop"
    ) |>
    mutate(`Done Date` = na_if(str_remove_all(`Done Date`, "^(; )+$"), ""))
}

# ==============================================================================
# STEP 3 — Prepare the phase table (keep ALL its columns, original names)
# ==============================================================================

ph_id <- find_col(phases_raw, c("WR Doc ID", "WR_Doc_ID"), "WR Doc ID (phases)")
invisible(find_col(phases_raw, c("TRS"), "TRS"))   # sanity check only

phases <- phases_raw |>
  mutate(wr_doc_id = str_trim(.data[[ph_id]])) |>
  mutate(across(matches("date", ignore.case = TRUE), fix_excel_dates))

# Event columns that ALSO exist in the phase file would be duplicates —
# drop them from the event side so each field appears only once.
dup_cols <- intersect(setdiff(names(events), "wr_doc_id"), names(phases))
if (length(dup_cols) > 0) {
  message("Note: phase file already has ", paste(dup_cols, collapse = ", "),
          " — keeping the phase file's version.")
  events <- events |> select(-all_of(dup_cols))
}

# ==============================================================================
# STEP 4 — Join: keep only phase records that have a matching event
# ==============================================================================

joined <- phases |>
  inner_join(events, by = "wr_doc_id", relationship = "many-to-many") |>
  select(-wr_doc_id)

# ==============================================================================
# STEP 5 — Validation summary (check these numbers each run)
# ==============================================================================

ev_ids <- unique(events$wr_doc_id)
ph_ids <- unique(phases$wr_doc_id)

events_not_in_phases <- events |> filter(!wr_doc_id %in% ph_ids)

cat("\n================ JOIN SUMMARY ================\n")
cat("Event file rows            :", nrow(events_raw), "\n")
cat("Event file unique WR Doc ID:", length(ev_ids), "\n")
cat("Phase file rows            :", nrow(phases), "\n")
cat("Phase file unique WR Doc ID:", length(ph_ids), "\n")
cat("-----------------------------------------------\n")
cat("WR Doc IDs in BOTH files   :", length(intersect(ev_ids, ph_ids)), "\n")
cat("Event IDs NOT in phase file:", length(setdiff(ev_ids, ph_ids)),
    " (see pipeline/work/events_not_in_phases_*.csv)\n")
cat("Joined output rows         :", nrow(joined), "\n")
cat("Joined rows with blank TRS :", sum(is.na(joined$TRS) | joined$TRS == ""), "\n")
if (EVENT_LAYOUT == "long") {
  cat("(EVENT_LAYOUT = 'long': a water right with 2 open events appears twice)\n")
}
cat("===============================================\n\n")

# ==============================================================================
# STEP 6 — Write outputs
# ==============================================================================

dir.create(OUT_DIR, showWarnings = FALSE)
stamp    <- format(Sys.Date(), "%Y%m%d")
out_file <- file.path(OUT_DIR, paste0("WaterRightReport_events_joined_", stamp, ".csv"))

if (GEOCODER_READY) {
  # 8 non-blank header lines, matching Ecology's Print Version layout.
  # (The geocoder app does read_csv(skip = 8), so exactly 8 lines matter.)
  header_lines <- c(
    "WA Ecology WRTS export - joined phase + open event records",
    paste("Created", format(Sys.time(), "%m/%d/%Y %H:%M"), "by join_events_phases.R"),
    paste("Phase source file:", attr(phases_raw, "source_file")),
    paste("Event source file:", attr(events_raw, "source_file")),
    "Kept: phase records whose WR Doc ID has an open event",
    "Events: Begin Construction / Complete Construction / Proof of Appropriation",
    paste("Event layout:", EVENT_LAYOUT),
    paste("Records:", nrow(joined))
  )
  writeLines(header_lines, out_file)
  write_csv(joined, out_file, append = TRUE, col_names = TRUE, na = "")
} else {
  write_csv(joined, out_file, na = "")
}

write_csv(events_not_in_phases,
          file.path(OUT_DIR, paste0("events_not_in_phases_", stamp, ".csv")),
          na = "")

message("Wrote: ", out_file)
message("Wrote: ", file.path(OUT_DIR, paste0("events_not_in_phases_", stamp, ".csv")))
