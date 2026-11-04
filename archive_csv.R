# archive_csv.R - keep daily copies of the Power BI CSV files in archivo/.
# Usage:
#   Rscript archive_csv.R
#
# Writes, with dates in SDQ local time (UTC-4 all year):
#   archivo/resumen/daily_summary_YYYY-MM-DD.csv    today's copy of daily_summary.csv, kept forever
#   archivo/detalle/flights_detail_YYYY-MM-DD.csv   yesterday's flights from flights_detail_7d.csv
#
# An existing file for the same date is never overwritten. A source that is missing, empty
# or not exported today stops the job with an error, and run_job.sh sends the alert.
#
# Retention: a detail file for day D is deleted by the run on D+5, at about 11:20 UTC.
# load_fids.R asks for flights 6 hours ahead, but the API has returned flights up to 11.5
# hours ahead (measured 2026-11-04 on two days of data). With that gap the oldest row is
# about 5.8 days old when deleted, about 29 hours under the 7-day AeroDataBox limit. That
# allows one missed run but not two, so old files are deleted before anything else: a bad
# source must not stop the deletion. The limit holds while the gap stays under 40 hours.

setwd(Sys.getenv("MONITOR_VUELOS_HOME", "/opt/monitor-vuelos"))
invisible(Sys.setlocale("LC_CTYPE", "C.UTF-8"))
Sys.umask("077")

UTC_OFFSET_HOURS <- -4
DETAIL_RETENTION_DAYS <- 4
SOURCE_DIR <- "data/powerbi"
ARCHIVE_DIR <- "archivo"
SUMMARY_DIR <- file.path(ARCHIVE_DIR, "resumen")
DETAIL_DIR <- file.path(ARCHIVE_DIR, "detalle")

log_msg <- function(...) cat(format(Sys.time(), "%Y-%m-%d %H:%M:%SZ", tz = "UTC"), ..., "\n")

to_sdq_date <- function(t) as.Date(format(t + UTC_OFFSET_HOURS * 3600, "%Y-%m-%d", tz = "UTC"))

delete_old_details <- function(today) {
  # A .tmp file is left only if a run was killed mid-write, and it holds detail data too.
  unlink(list.files(DETAIL_DIR, pattern = "[.]tmp$", full.names = TRUE))
  files <- list.files(DETAIL_DIR, pattern = "^flights_detail_[0-9]{4}-[0-9]{2}-[0-9]{2}[.]csv$")
  # The date comes from the file name, which copies and touches do not change.
  dates <- as.Date(sub("^flights_detail_(.*)[.]csv$", "\\1", files), format = "%Y-%m-%d")
  if (anyNA(dates)) stop("Invalid date in file name: ", paste(files[is.na(dates)], collapse = ", "))
  old <- file.path(DETAIL_DIR, files[as.integer(today - dates) > DETAIL_RETENTION_DAYS])
  if (length(old) > 0 && !all(file.remove(old))) stop("Could not delete: ", paste(old, collapse = ", "))
  log_msg("old detail files deleted:", length(old))
}

# All columns are read as text so that values such as gate "01" are kept exactly.
read_source <- function(file_name, today) {
  path <- file.path(SOURCE_DIR, file_name)
  if (!file.exists(path)) stop("Source file not found: ", path)
  if (file.size(path) == 0) stop("Source file is empty: ", path)
  if (to_sdq_date(file.mtime(path)) != today) stop("Source file was not exported today: ", path)
  rows <- read.csv(path, colClasses = "character", na.strings = "", check.names = FALSE,
                   fileEncoding = "UTF-8")
  if (nrow(rows) == 0) stop("Source file has no data rows: ", path)
  rows
}

# Writing to a temporary name first means an interrupted run never leaves a partial file,
# which the no-overwrite rule would then keep.
write_new <- function(path, write_to) {
  if (file.exists(path)) {
    log_msg(path, ": already exists, not overwritten")
    return(invisible())
  }
  tmp <- paste0(path, ".tmp")
  on.exit(unlink(tmp))
  write_to(tmp)
  if (!file.rename(tmp, path)) stop("Could not create: ", path)
  log_msg(path, ": created")
}

archive_summary <- function(today) {
  read_source("daily_summary.csv", today)
  source_path <- file.path(SOURCE_DIR, "daily_summary.csv")
  write_new(file.path(SUMMARY_DIR, sprintf("daily_summary_%s.csv", today)), function(tmp) {
    if (!file.copy(source_path, tmp)) stop("Could not copy: ", source_path)
  })
}

archive_yesterday_details <- function(today) {
  rows <- read_source("flights_detail_7d.csv", today)
  if (!"sched_utc" %in% names(rows)) stop("Column sched_utc not found in flights_detail_7d.csv")
  yesterday <- today - 1
  # SDQ midnight in UTC. sched_utc is YYYY-MM-DD HH:MM:SS, so text order is time order.
  midnight <- sprintf("%02d:00:00", -UTC_OFFSET_HOURS)
  start_utc <- paste(yesterday, midnight)
  end_utc <- paste(today, midnight)
  day_rows <- rows[which(rows$sched_utc >= start_utc & rows$sched_utc < end_utc), ]
  if (nrow(day_rows) == 0) stop("No flights between ", start_utc, " and ", end_utc, " UTC; nothing written")
  write_new(file.path(DETAIL_DIR, sprintf("flights_detail_%s.csv", yesterday)), function(tmp) {
    write.csv(day_rows, tmp, row.names = FALSE, na = "", fileEncoding = "UTF-8")
  })
  log_msg("flights for", format(yesterday), ":", nrow(day_rows))
}

# --- main -----------------------------------------------------------------------
today <- to_sdq_date(Sys.time())
dir.create(SUMMARY_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(DETAIL_DIR, recursive = TRUE, showWarnings = FALSE)
if (!Sys.chmod(ARCHIVE_DIR, "700", use_umask = FALSE)) stop("Could not set permissions on ", ARCHIVE_DIR)

delete_old_details(today)
archive_summary(today)
archive_yesterday_details(today)
