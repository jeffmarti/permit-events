# ==============================================================================
# prepare_app_data.R
# ------------------------------------------------------------------------------
# Run this ONCE each time you refresh the data, BEFORE publishing the app.
# It turns the geocoded CSV into the compact files the app reads, and
# downloads the WRIA and county boundary layers so the published app never
# has to fetch anything at startup.
#
# PUBLICATION WORKFLOW (run from the project root — the folder with app.R):
#   1. Download the two Ecology WRTS exports (.xlsx) into  pipeline/raw/
#   2. source("pipeline/join_events_phases.R")
#        -> pipeline/work/WaterRightReport_events_joined_YYYYMMDD.csv
#   3. Geocode that file in the WA Water Rights PLSS Geocoder app, and save
#      BOTH downloads (geocoded CSV and failed CSV) into  pipeline/work/
#   4. source("pipeline/prepare_app_data.R")          <- this script
#        -> data/permit_events.rds   (records + data-currency info)
#        -> data/boundaries.rds      (WRIA + county polygons)
#   5. Run / publish the app. Deploy ONLY:
#        app.R, data/permit_events.rds, data/boundaries.rds
#
# REQUIRED PACKAGES: dplyr, readr, stringr, sf
# ==============================================================================

library(dplyr)
library(readr)
library(stringr)
library(sf)

# ==============================================================================
# SETTINGS
# ==============================================================================

WORK_DIR <- "pipeline/work"     # where the geocoded + failed CSVs are saved
OUT_DIR  <- "data"              # the app's data folder (deployed)

# "Data current as of" date shown in the app.
#   NULL = take it from the geocoded file name (..._20261011.csv);
#   or set it yourself, e.g.  DATA_DATE <- as.Date("2026-10-11")
DATA_DATE <- NULL

# Boundary layers rarely change. FALSE = reuse data/boundaries.rds if it
# already exists; TRUE = download fresh copies.
REFRESH_BOUNDARIES <- FALSE

# Same sources as the low-flow-wr app
WRIA_GEOJSON_URL <- paste0(
  "https://services.arcgis.com/6lCKYNJLvwTXqrmp/arcgis/rest/services/",
  "ECY/FeatureServer/11/query?outFields=*&where=1%3D1&f=geojson"
)
COUNTY_GEOJSON_URL <- paste0(
  "https://tigerweb.geo.census.gov/arcgis/rest/services/TIGERweb/",
  "State_County/MapServer/11/query?where=STATE%3D'53'",
  "&outFields=NAME&f=geojson"
)

# ==============================================================================
# LOOKUP TABLE — WRIA names (same as the geocoder / low-flow-wr apps)
# ==============================================================================

wria_names <- tribble(
  ~WRIA, ~wria_name,
  "1",  "Nooksack",               "2",  "San Juan",
  "3",  "Lower Skagit - Samish",  "4",  "Upper Skagit",
  "5",  "Stillaguamish",          "6",  "Island",
  "7",  "Snohomish",              "8",  "Cedar - Sammamish",
  "9",  "Duwamish - Green",       "10", "Puyallup - White",
  "11", "Nisqually",              "12", "Chambers - Clover",
  "13", "Deschutes",              "14", "Kennedy - Goldsborough",
  "15", "Kitsap",                 "16", "Skokomish - Dosewallips",
  "17", "Quilcene - Snow",        "18", "Elwha - Dungeness",
  "19", "Lyre - Hoko",            "20", "Soleduc",
  "21", "Queets - Quinault",      "22", "Lower Chehalis",
  "23", "Upper Chehalis",         "24", "Willapa",
  "25", "Grays - Elochoman",      "26", "Cowlitz",
  "27", "Lewis",                  "28", "Salmon - Washougal",
  "29", "Wind - White Salmon",    "30", "Klickitat",
  "31", "Rock - Glade",           "32", "Walla Walla",
  "33", "Lower Snake",            "34", "Palouse",
  "35", "Middle Snake",           "36", "Esquatzel Coulee",
  "37", "Lower Yakima",           "38", "Naches",
  "39", "Upper Yakima",           "40", "Alkali - Squilchuck",
  "41", "Lower Crab",             "42", "Grand Coulee",
  "43", "Upper Crab-Wilson",      "44", "Moses Coulee",
  "45", "Wenatchee",              "46", "Entiat",
  "47", "Chelan",                 "48", "Methow",
  "49", "Okanogan",               "50", "Foster",
  "51", "Nespelem",               "52", "Sanpoil",
  "53", "Lower Lake Roosevelt",   "54", "Lower Spokane",
  "55", "Little Spokane",         "56", "Hangman",
  "57", "Middle Spokane",         "58", "Middle Lake Roosevelt",
  "59", "Colville",               "60", "Kettle",
  "61", "Upper Lake Roosevelt",   "62", "Pend Oreille"
)

# ==============================================================================
# HELPERS
# ==============================================================================

col_or_na <- function(df, name) {
  if (name %in% names(df)) df[[name]] else rep(NA_character_, nrow(df))
}

parse_date_flex <- function(x) {
  x <- na_if(str_trim(x), "")
  d <- as.Date(x, format = "%m/%d/%Y")
  iso <- is.na(d) & !is.na(x)
  d[iso] <- as.Date(x[iso], format = "%Y-%m-%d")
  d
}

newest <- function(pattern, exclude = NULL) {
  f <- list.files(WORK_DIR, pattern = pattern, full.names = TRUE, ignore.case = TRUE)
  if (!is.null(exclude)) f <- f[!str_detect(basename(f), regex(exclude, ignore_case = TRUE))]
  if (length(f) == 0) return(NULL)
  f[which.max(file.mtime(f))]
}

# Points sharing a location are spread around a small ring (radius grows a
# little with stack size, capped ~400 m) so each record can be clicked.
spread_stacked <- function(df) {
  df |>
    group_by(lon, lat) |>
    mutate(n_stack = n(), k = row_number()) |>
    ungroup() |>
    mutate(
      r_m      = if_else(n_stack > 1, pmin(80 + 15 * n_stack, 400), 0),
      ang      = 2 * pi * (k - 1) / n_stack,
      lat_sprd = lat + (r_m * sin(ang)) / 111320,
      lon_sprd = lon + (r_m * cos(ang)) / (111320 * cos(lat * pi / 180))
    ) |>
    select(-k, -r_m, -ang)
}

# Download a GeoJSON layer, simplify it (~50 m) so the app stays light,
# and return it in WGS84. Returns NULL (with a message) on failure.
fetch_boundary <- function(url, label) {
  message("Downloading ", label, " ...")
  tryCatch({
    lyr <- st_read(url, quiet = TRUE) |> st_make_valid()
    n_raw <- nrow(lyr)
    lyr <- lyr |>
      st_transform(32610) |>                       # metres, for simplifying
      st_simplify(dTolerance = 50, preserveTopology = TRUE) |>
      st_transform(4326)
    message("  ", label, ": ", n_raw, " features | columns: ",
            paste(setdiff(names(lyr), attr(lyr, "sf_column")), collapse = ", "))
    lyr
  }, error = function(e) {
    message("  WARNING - could not download ", label, ": ", e$message)
    NULL
  })
}

# ==============================================================================
# STEP 1 — Read the geocoded CSV (and the failed CSV, for the count)
# ==============================================================================

geo_file  <- newest("geocoded.*\\.csv$", exclude = "fail")
fail_file <- newest("fail.*\\.csv$")
if (is.null(geo_file)) {
  stop("No geocoded CSV found in '", WORK_DIR, "/'. ",
       "Save the geocoder's 'Download Geocoded CSV' file there first.")
}
message("Geocoded file: ", basename(geo_file))
message("Failed file  : ", if (is.null(fail_file)) "(none found)" else basename(fail_file))

raw <- read_csv(geo_file, col_types = cols(.default = "c"),
                na = c("", "NA"), show_col_types = FALSE)

needed  <- c("WR Doc ID", "Record Document Number", "Event", "Due Date", "lon", "lat")
missing <- setdiff(needed, names(raw))
if (length(missing) > 0) stop("Geocoded file is missing column(s): ",
                              paste(missing, collapse = ", "))

n_failed <- if (is.null(fail_file)) NA_integer_ else
  nrow(read_csv(fail_file, col_types = cols(.default = "c"), show_col_types = FALSE))

# ==============================================================================
# STEP 2 — Clean into the app's working table
# ==============================================================================

df <- tibble(
  wr_doc_id   = col_or_na(raw, "WR Doc ID"),
  doc_number  = col_or_na(raw, "Record Document Number"),
  person      = col_or_na(raw, "Person or Organization"),
  phase       = col_or_na(raw, "Phase"),
  stage       = col_or_na(raw, "Stage"),
  priority    = col_or_na(raw, "Priority Date Claim First Use"),
  purpose     = col_or_na(raw, "Purpose of Use"),
  qi          = col_or_na(raw, "Qi"),
  uom         = col_or_na(raw, "UOM"),
  qa          = col_or_na(raw, "Qa"),
  WRIA        = col_or_na(raw, "WRIA"),
  county      = col_or_na(raw, "County"),
  trs         = col_or_na(raw, "TRS"),
  q           = col_or_na(raw, "Q"),
  qq          = col_or_na(raw, "QQ"),
  source_name = col_or_na(raw, "First Source Name Listed"),
  event       = col_or_na(raw, "Event"),
  due         = parse_date_flex(col_or_na(raw, "Due Date")),
  n_events    = suppressWarnings(as.integer(col_or_na(raw, "N Open Events"))),
  lon         = suppressWarnings(as.numeric(raw$lon)),
  lat         = suppressWarnings(as.numeric(raw$lat))
) |>
  mutate(
    # Ecology abbreviates "Begin Const"; standardise all three names
    event = case_when(
      str_detect(event, regex("^Begin Const", ignore_case = TRUE))    ~ "Begin Construction",
      str_detect(event, regex("^Complete Const", ignore_case = TRUE)) ~ "Complete Construction",
      str_detect(event, regex("^Proof", ignore_case = TRUE))          ~ "Proof of Appropriation",
      TRUE ~ event
    ),
    WRIA = as.character(suppressWarnings(as.integer(WRIA)))   # "07" -> "7"
  ) |>
  left_join(wria_names, by = "WRIA") |>
  mutate(wria_label = if_else(is.na(WRIA), NA_character_,
                              paste0(WRIA, " - ", coalesce(wria_name, paste0("WRIA ", WRIA)))))

n_in     <- nrow(df)
n_no_xy  <- sum(is.na(df$lon) | is.na(df$lat))
n_no_due <- sum(is.na(df$due))

df <- df |>
  filter(!is.na(lon), !is.na(lat), !is.na(due)) |>
  spread_stacked()

# Sanity check: every point should fall roughly inside Washington
outside <- sum(df$lon < -125 | df$lon > -116.5 | df$lat < 45.4 | df$lat > 49.1)

# ==============================================================================
# STEP 3 — Data-currency date
# ==============================================================================

if (is.null(DATA_DATE)) {
  stamp <- str_extract(basename(geo_file), "20\\d{6}")
  DATA_DATE <- if (!is.na(stamp)) as.Date(stamp, "%Y%m%d") else
    as.Date(file.mtime(geo_file))
}

info <- list(
  data_date   = DATA_DATE,
  prepared_at = Sys.time(),
  source_file = basename(geo_file),
  n_in_file   = n_in,
  n_failed    = n_failed,       # records the geocoder could not map
  n_no_xy     = n_no_xy,
  n_no_due    = n_no_due,
  n_used      = nrow(df),
  n_multi     = sum(df$n_events > 1, na.rm = TRUE)
)

dir.create(OUT_DIR, showWarnings = FALSE)
saveRDS(list(records = df, info = info), file.path(OUT_DIR, "permit_events.rds"))

# ==============================================================================
# STEP 4 — Boundary layers (WRIA + county), downloaded once
# ==============================================================================

bnd_file <- file.path(OUT_DIR, "boundaries.rds")

if (REFRESH_BOUNDARIES || !file.exists(bnd_file)) {
  wria_sf   <- fetch_boundary(WRIA_GEOJSON_URL,   "WRIA boundaries")
  county_sf <- fetch_boundary(COUNTY_GEOJSON_URL, "WA county boundaries")

  # WRIA: auto-detect the number/name columns (same approach as low-flow-wr)
  if (!is.null(wria_sf)) {
    nr_col <- names(wria_sf)[str_detect(names(wria_sf),
                                        regex("NR$|NUM$|NUMBER$", ignore_case = TRUE))][1]
    nm_col <- names(wria_sf)[str_detect(names(wria_sf),
                                        regex("NM$|NAME$", ignore_case = TRUE))][1]
    if (is.na(nr_col)) stop("Could not find the WRIA number column in the WRIA layer.")
    wria_sf <- wria_sf |>
      mutate(WRIA = as.character(as.integer(.data[[nr_col]])),
             wria_display = paste0("WRIA ", WRIA, " — ",
                                   if (!is.na(nm_col)) .data[[nm_col]] else "")) |>
      select(WRIA, wria_display)
  }
  if (!is.null(county_sf)) county_sf <- county_sf |> select(NAME)

  if (is.null(wria_sf) && is.null(county_sf) && file.exists(bnd_file)) {
    message("Both downloads failed - keeping the existing ", bnd_file)
  } else {
    saveRDS(list(wria = wria_sf, county = county_sf), bnd_file)
  }
} else {
  message("Reusing existing ", bnd_file, " (set REFRESH_BOUNDARIES <- TRUE to re-download)")
}

bnd <- readRDS(bnd_file)

# ==============================================================================
# STEP 5 — Validation summary (check these each refresh)
# ==============================================================================

cat("\n================ APP DATA SUMMARY ================\n")
cat("Data current as of          :", format(DATA_DATE, "%m/%d/%Y"), "\n")
cat("Records in geocoded file    :", n_in, "\n")
cat("Records the geocoder failed :", ifelse(is.na(n_failed), "(no failed file)", n_failed), "\n")
cat("Dropped - no lon/lat        :", n_no_xy, "\n")
cat("Dropped - no due date       :", n_no_due, "\n")
cat("Records in the app          :", nrow(df), "\n")
cat("Points outside WA bounds    :", outside, if (outside > 0) "  <-- CHECK" else "", "\n")
cat("Records with >1 open event  :", info$n_multi, "\n")
cat("Due dates range             :", format(min(df$due), "%m/%d/%Y"), "to",
    format(max(df$due), "%m/%d/%Y"), "\n")
cat("Overdue today / pending     :", sum(df$due < Sys.Date()), "/",
    sum(df$due >= Sys.Date()), "\n")
cat("Events:\n"); print(table(df$event, useNA = "ifany"))
cat("WRIA polygons               :", if (is.null(bnd$wria)) "MISSING" else nrow(bnd$wria),
    " (expect 62)\n")
cat("County polygons             :", if (is.null(bnd$county)) "MISSING" else nrow(bnd$county),
    " (expect 39)\n")
cat("Files written               :", file.path(OUT_DIR, "permit_events.rds"),
    "|", bnd_file, "\n")
cat("==================================================\n")
