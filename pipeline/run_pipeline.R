# ==============================================================================
# run_pipeline.R — refresh the permit-events app data in one go
# ------------------------------------------------------------------------------
#   1. fetch_wrts_exports.R   download both WRTS exports      -> pipeline/raw/
#   2. join_events_phases.R   join them on WR Doc ID          -> pipeline/work/
#   3. geocode_records.R      PLSS -> lon/lat (cached)        -> pipeline/work/
#   4. prepare_app_data.R     build the app's data files      -> data/*.rds
#
# Run from the project root (the folder with app.R), either in RStudio:
#   source("pipeline/run_pipeline.R")
# or from a terminal / GitHub Actions:
#   Rscript pipeline/run_pipeline.R
# Stops at the first step that fails, and says which one.
# ==============================================================================

steps <- c("fetch_wrts_exports.R", "join_events_phases.R",
           "geocode_records.R",   "prepare_app_data.R")

t_start <- Sys.time()
for (s in steps) {
  t0 <- Sys.time()
  cat("\n#############################################################\n")
  cat("## STEP:", s, "\n")
  cat("#############################################################\n")
  tryCatch(
    source(file.path("pipeline", s), local = new.env(), echo = FALSE),
    error = function(e) {
      stop("Pipeline stopped in ", s, ": ", conditionMessage(e), call. = FALSE)
    }
  )
  cat("## done in", round(difftime(Sys.time(), t0, units = "mins"), 1), "min\n")
}
cat("\nPipeline finished in", round(difftime(Sys.time(), t_start, units = "mins"), 1),
    "min. App data refreshed in data/.\n")
