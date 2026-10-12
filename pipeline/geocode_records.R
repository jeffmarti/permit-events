# ==============================================================================
# geocode_records.R
# ------------------------------------------------------------------------------
# Unattended version of the WA Water Rights PLSS Geocoder app's geocoding:
# same TRS parsing, same BLM CadNSDI lookup (Section, then Protracted Block
# fallback), same quarter / quarter-quarter placement — but no Shiny, so it
# can run inside the automated pipeline.
#
# Input : newest pipeline/work/WaterRightReport_events_joined_*.csv
# Output: pipeline/work/WaterRightReport_geocoded_YYYYMMDD.csv
#         pipeline/work/WaterRightReport_geocode_failed_YYYYMMDD.csv
#         (same names prepare_app_data.R already looks for)
#
# CACHE: pipeline/cache/section_bbox_cache.csv
#   One row per PLSS section: its bounding box (all the placement math uses)
#   — small, plain text, safe to commit to GitHub. Sections BLM has no match
#   for are remembered too ("NONE") and re-checked after RECHECK_NONE_DAYS.
#
# ONE-TIME SEED from the geocoder app's cache (saves ~15,000 lookups):
#   set IMPORT_APP_CACHE to the path of the app's section_cache.rds, run once,
#   then set it back to NULL. (Needs the sf package for that one run.)
#
# Run from the project root:  source("pipeline/geocode_records.R")
# REQUIRED PACKAGES: dplyr, readr, stringr, httr2   (+ sf only for the seed)
# ==============================================================================

library(dplyr)
library(readr)
library(stringr)
library(httr2)

# ==============================================================================
# SETTINGS
# ==============================================================================

WORK_DIR   <- "pipeline/work"
CACHE_FILE <- "pipeline/cache/section_bbox_cache.csv"

IMPORT_APP_CACHE  <- "F:/OneDrive/OneDriveDocuments/R/Apps/WR_geocode/section_cache.rds" 

RECHECK_NONE_DAYS <- 90     # re-ask BLM about "no match" sections after this many days
PAUSE_S           <- 0.5    # polite pause between BLM requests (same as the app)

BLM_URL <- "https://gis.blm.gov/arcgis/rest/services/Cadastral/BLM_Natl_PLSS_CadNSDI/MapServer/2/query"

# ==============================================================================
# HELPERS (ported from WaterRightReportGeocodeAPP.R)
# ==============================================================================

# BLM CadNSDI first-division ID, e.g. WA330210N0050E0SN010 (Willamette Meridian)
build_frstdivid <- function(twp_num, twp_dir, rng_num, rng_dir, sec_num, div_type = "SN") {
  paste0("WA33",
         str_pad(twp_num, 3, "left", "0"), "0", twp_dir,
         str_pad(rng_num, 3, "left", "0"), "0", rng_dir,
         "0", div_type,
         str_pad(sec_num, 2, "left", "0"), "0")
}

# Ask BLM for one FRSTDIVID. Returns:
#   c(xmin, ymin, xmax, ymax)  if found
#   "none"                     if BLM has no such division
#   NULL                       if the request itself failed (not cached)
query_bbox <- function(fid) {
  resp <- tryCatch(
    request(BLM_URL) |>
      req_url_query(where = sprintf("FRSTDIVID = '%s'", fid), outFields = "FRSTDIVID",
                    returnGeometry = "true", outSR = "4326", f = "geojson") |>
      req_timeout(30) |>
      req_retry(max_tries = 3, backoff = ~ 2) |>
      req_perform(),
    error = function(e) { message("  API error for ", fid, ": ", conditionMessage(e)); NULL })
  if (is.null(resp)) return(NULL)
  gj <- tryCatch(resp_body_json(resp, check_type = FALSE, simplifyVector = FALSE),
                 error = function(e) NULL)
  if (is.null(gj) || !is.null(gj$error)) return(NULL)
  if (length(gj$features) == 0) return("none")
  xy <- unlist(lapply(gj$features, function(f) f$geometry$coordinates))
  xs <- xy[c(TRUE, FALSE)]; ys <- xy[c(FALSE, TRUE)]
  c(xmin = min(xs), ymin = min(ys), xmax = max(xs), ymax = max(ys))
}

# Section first ("SN"); if BLM has none, try Protracted Block ("PB")
lookup_section <- function(twp_num, twp_dir, rng_num, rng_dir, sec_num) {
  for (dt in c("SN", "PB")) {
    r <- query_bbox(build_frstdivid(twp_num, twp_dir, rng_num, rng_dir, sec_num, dt))
    if (is.null(r)) return(list(status = "error"))
    if (!identical(r, "none")) return(list(status = "ok", div_type = dt, bbox = r))
    Sys.sleep(0.3)
  }
  list(status = "none")
}

quarter_frac <- function(dir) {
  switch(dir,
         "NE" = c(x = 0.75, y = 0.75), "NW" = c(x = 0.25, y = 0.75),
         "SE" = c(x = 0.75, y = 0.25), "SW" = c(x = 0.25, y = 0.25),
         c(x = 0.50, y = 0.50))
}

# Same placement as the app: section centre, or quarter centre, or QQ centre
qq_point <- function(xmin, ymin, xmax, ymax, q, qq) {
  dx <- xmax - xmin; dy <- ymax - ymin
  has_q  <- !is.na(q)  && q  != ""
  has_qq <- !is.na(qq) && qq != ""
  if (!has_q) return(c((xmin + xmax) / 2, (ymin + ymax) / 2))
  qf <- quarter_frac(q)
  qx0 <- xmin + dx * (qf[["x"]] - 0.25); qx1 <- xmin + dx * (qf[["x"]] + 0.25)
  qy0 <- ymin + dy * (qf[["y"]] - 0.25); qy1 <- ymin + dy * (qf[["y"]] + 0.25)
  if (!has_qq) return(c((qx0 + qx1) / 2, (qy0 + qy1) / 2))
  qqf <- quarter_frac(qq)
  c(qx0 + (qx1 - qx0) * qqf[["x"]], qy0 + (qy1 - qy0) * qqf[["y"]])
}

empty_cache <- function() {
  tibble(plss_key = character(), div_type = character(), xmin = double(),
         ymin = double(), xmax = double(), ymax = double(), fetched = as.Date(character()))
}

# ==============================================================================
# STEP 0 — Load (or seed) the cache
# ==============================================================================

dir.create(dirname(CACHE_FILE), showWarnings = FALSE, recursive = TRUE)
cache <- if (file.exists(CACHE_FILE)) {
  read_csv(CACHE_FILE, col_types = "ccddddD", show_col_types = FALSE)
} else empty_cache()

if (!is.null(IMPORT_APP_CACHE) && file.exists(IMPORT_APP_CACHE)) {
  message("Importing geocoder app cache: ", IMPORT_APP_CACHE)
  app_cache <- readRDS(IMPORT_APP_CACHE)            # named list: key -> sf polygon
  app_cache <- app_cache[!vapply(app_cache, is.null, logical(1))]
  imported <- bind_rows(lapply(names(app_cache), function(k) {
    bb <- sf::st_bbox(app_cache[[k]])
    tibble(plss_key = k, div_type = "SN", xmin = bb[["xmin"]], ymin = bb[["ymin"]],
           xmax = bb[["xmax"]], ymax = bb[["ymax"]], fetched = Sys.Date())
  }))
  cache <- bind_rows(cache, imported) |> distinct(plss_key, .keep_all = TRUE)
  message("  cache now holds ", nrow(cache), " sections")
}

# ==============================================================================
# STEP 1 — Read the joined file and parse TRS (same regex as the app)
# ==============================================================================

in_file <- list.files(WORK_DIR, pattern = "events_joined.*\\.csv$", full.names = TRUE)
if (length(in_file) == 0) stop("No joined CSV in ", WORK_DIR, ". Run join_events_phases.R first.")
in_file <- in_file[which.max(file.mtime(in_file))]
message("Geocoding: ", basename(in_file))

# The joined file has Ecology-style note lines first (one of them mentions
# "WR Doc ID"), so the real header is the line naming BOTH columns
top <- read_lines(in_file, n_max = 30)
hdr_line <- which(str_detect(top, "WR Doc ID,") & str_detect(top, "(^|,)TRS(,|$)"))[1]
if (is.na(hdr_line)) stop("Could not find the column header row in ", basename(in_file))
wr <- read_csv(in_file, skip = hdr_line - 1, col_types = cols(.default = "c"),
               show_col_types = FALSE)

wr <- wr |>
  mutate(
    twp_num  = str_match(TRS, "T([0-9.]+)[NS]")[, 2] |> as.numeric() |> round(),
    twp_dir  = str_match(TRS, "T[0-9.]+([NS])")[, 2],
    rng_num  = str_match(TRS, "R([0-9.]+)[EW]")[, 2] |> as.numeric() |> round(),
    rng_dir  = str_match(TRS, "R[0-9.]+([EW])")[, 2],
    sec_num  = str_extract(TRS, "(?<=/)[0-9]+$") |> as.integer(),
    plss_key = if_else(is.na(twp_num) | is.na(rng_num) | is.na(sec_num) |
                         is.na(twp_dir) | is.na(rng_dir), NA_character_,
                       paste0("T", twp_num, twp_dir, "_R", rng_num, rng_dir, "_S", sec_num))
  )

secs <- wr |> filter(!is.na(plss_key)) |>
  distinct(plss_key, twp_num, twp_dir, rng_num, rng_dir, sec_num)

# ==============================================================================
# STEP 2 — Look up sections not already cached
# ==============================================================================

stale_none <- cache |> filter(div_type == "NONE", fetched < Sys.Date() - RECHECK_NONE_DAYS)
cache  <- cache |> anti_join(stale_none, by = "plss_key")
to_get <- secs |> anti_join(cache, by = "plss_key")

message(nrow(secs), " unique sections | ", nrow(secs) - nrow(to_get), " cached | ",
        nrow(to_get), " to query from BLM")

n_err <- 0
if (nrow(to_get) > 0) {
  for (i in seq_len(nrow(to_get))) {
    s <- to_get[i, ]
    r <- lookup_section(s$twp_num, s$twp_dir, s$rng_num, s$rng_dir, s$sec_num)
    if (r$status == "error") { n_err <- n_err + 1; next }      # not cached; retried next run
    cache <- bind_rows(cache, tibble(
      plss_key = s$plss_key,
      div_type = if (r$status == "ok") r$div_type else "NONE",
      xmin = if (r$status == "ok") r$bbox[["xmin"]] else NA_real_,
      ymin = if (r$status == "ok") r$bbox[["ymin"]] else NA_real_,
      xmax = if (r$status == "ok") r$bbox[["xmax"]] else NA_real_,
      ymax = if (r$status == "ok") r$bbox[["ymax"]] else NA_real_,
      fetched = Sys.Date()))
    if (i %% 25 == 0 || i == nrow(to_get)) {
      message("  ", i, " / ", nrow(to_get), " queried")
      write_csv(cache, CACHE_FILE)                              # save progress
    }
    Sys.sleep(PAUSE_S)
  }
}
write_csv(cache |> arrange(plss_key), CACHE_FILE)

# ==============================================================================
# STEP 3 — Place each record, split into geocoded / failed
# ==============================================================================

wr <- wr |> left_join(cache |> select(-fetched), by = "plss_key")

pts <- mapply(function(x0, y0, x1, y1, q, qq) {
  if (is.na(x0)) return(c(NA_real_, NA_real_))
  qq_point(x0, y0, x1, y1, q, qq)
}, wr$xmin, wr$ymin, wr$xmax, wr$ymax, wr$Q, wr$QQ)

wr <- wr |>
  mutate(lon = pts[1, ], lat = pts[2, ]) |>
  select(-plss_key, -div_type, -xmin, -ymin, -xmax, -ymax) |>
  relocate(lon, lat, .after = TRS)

geocoded <- wr |> filter(!is.na(lat))
failed   <- wr |> filter(is.na(lat))

stamp    <- format(Sys.Date(), "%Y%m%d")
out_ok   <- file.path(WORK_DIR, paste0("WaterRightReport_geocoded_", stamp, ".csv"))
out_fail <- file.path(WORK_DIR, paste0("WaterRightReport_geocode_failed_", stamp, ".csv"))
write_csv(geocoded, out_ok)
write_csv(failed,   out_fail)

# ==============================================================================
# STEP 4 — Summary
# ==============================================================================

cat("\n================ GEOCODING SUMMARY ================\n")
cat("Records in joined file      :", nrow(wr), "\n")
cat("Geocoded                    :", nrow(geocoded),
    sprintf("(%.1f%%)", 100 * nrow(geocoded) / max(nrow(wr), 1)), "\n")
cat("Failed                      :", nrow(failed), "\n")
cat("  - blank / unreadable TRS  :", sum(is.na(failed$twp_num) | is.na(failed$sec_num)), "\n")
cat("  - no BLM match            :", sum(!is.na(failed$twp_num) & !is.na(failed$sec_num)), "\n")
cat("BLM requests that errored   :", n_err, if (n_err > 0) " (will retry next run)" else "", "\n")
cat("Sections in cache           :", nrow(cache), "\n")
cat("Wrote                       :", basename(out_ok), "|", basename(out_fail), "\n")
cat("===================================================\n")
