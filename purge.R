# purge.R - delete AeroDataBox detail data older than 6 days.
# Strict reading of the terms: age is counted from the FIRST fetch
# (flight_movements.first_seen_utc and the fetch time in each raw file name).
# The terms allow 7 days. This job runs up to 6 hours apart, so a 6-day cutoff keeps every
# row about 18 hours under that limit and tolerates up to three missed runs.
# daily_summary is permanent and is never touched here.
# Usage:
#   Rscript purge.R             # delete
#   Rscript purge.R --dry-run   # only report what would be deleted

setwd(Sys.getenv("MONITOR_VUELOS_HOME", "/opt/monitor-vuelos"))
readRenviron(".Renviron")
suppressPackageStartupMessages(library(DBI))

RETENTION_DAYS <- 6
RAW_DIR <- "raw"

log_msg <- function(...) cat(format(Sys.time(), "%Y-%m-%d %H:%M:%SZ", tz = "UTC"), ..., "\n")

dry_run <- identical(commandArgs(trailingOnly = TRUE), "--dry-run")
cutoff <- Sys.time() - RETENTION_DAYS * 86400
cutoff_sql <- format(cutoff, "%Y-%m-%d %H:%M:%S", tz = "UTC")
log_msg(if (dry_run) "DRY RUN -" else "PURGE -", "cutoff (UTC):", cutoff_sql)

# --- 1. Database rows -----------------------------------------------------------
con <- dbConnect(RMariaDB::MariaDB(),
  host = Sys.getenv("VUELOS_DB_HOST"), dbname = Sys.getenv("VUELOS_DB_NAME"),
  user = Sys.getenv("VUELOS_DB_USER"), password = Sys.getenv("VUELOS_DB_PASSWORD"))
n_old <- as.integer(dbGetQuery(con,
  "SELECT COUNT(*) n FROM flight_movements WHERE first_seen_utc < ?",
  params = list(cutoff_sql))$n)
if (!dry_run && n_old > 0) {
  n_old <- dbExecute(con, "DELETE FROM flight_movements WHERE first_seen_utc < ?",
                     params = list(cutoff_sql))
}
dbDisconnect(con)
log_msg("flight_movements rows", if (dry_run) "to delete:" else "deleted:", n_old)

# --- 2. Raw JSON files ------------------------------------------------------------
# Fetch time comes from the file name (fids_SDQ_YYYYMMDDTHHMMSSZ.json), which is
# not affected by copies or touches; mtime is only a fallback.
files <- list.files(RAW_DIR, pattern = "^fids_.*[.]json$", full.names = TRUE)
fetched <- vapply(files, function(f) {
  ts <- regmatches(basename(f), regexpr("[0-9]{8}T[0-9]{6}", basename(f)))
  t <- if (length(ts)) as.POSIXct(ts, format = "%Y%m%dT%H%M%S", tz = "UTC") else NA
  if (is.na(t)) t <- file.mtime(f)
  as.numeric(t)
}, numeric(1))
old_files <- files[fetched < as.numeric(cutoff)]
if (!dry_run && length(old_files) > 0) {
  ok <- file.remove(old_files)
  if (!all(ok)) stop("Could not delete: ", paste(old_files[!ok], collapse = ", "))
}
log_msg("raw files", if (dry_run) "to delete:" else "deleted:", length(old_files),
        "of", length(files))
