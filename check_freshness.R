# check_freshness.R - alert when flight_movements has received nothing new for too long.
# Usage:
#   Rscript check_freshness.R                        # alert if the newest data is over 12 hours old
#   Rscript check_freshness.R --max-age-hours 6      # use another limit
#   Rscript check_freshness.R --dry-run              # print the alert instead of sending it
#
# Age is measured on last_seen_utc, the time of the last fetch that saw a flight. sched_utc
# would not work: it holds future flights, so it looks fresh even when fetching has stopped.
# Stale data exits 0 once the alert is handled: the check itself worked. A failure of the
# check, such as the database being down, exits non-zero and run_job.sh alerts about it.

setwd(Sys.getenv("MONITOR_VUELOS_HOME", "/opt/monitor-vuelos"))
readRenviron(".Renviron")
invisible(Sys.setlocale("LC_CTYPE", "C.UTF-8"))
suppressPackageStartupMessages(library(DBI))

AIRPORT <- "SDQ"
UTC_OFFSET_HOURS <- -4
DEFAULT_MAX_AGE_HOURS <- 12
JOB <- "check_freshness"

log_msg <- function(...) cat(format(Sys.time(), "%Y-%m-%d %H:%M:%SZ", tz = "UTC"), ..., "\n")

args <- commandArgs(trailingOnly = TRUE)
dry_run <- "--dry-run" %in% args
args <- args[args != "--dry-run"]
max_age_hours <- DEFAULT_MAX_AGE_HOURS
if (length(args) == 2 && args[1] == "--max-age-hours") {
  max_age_hours <- suppressWarnings(as.numeric(args[2]))
  if (is.na(max_age_hours) || max_age_hours < 0) stop("Invalid --max-age-hours")
} else if (length(args) > 0) {
  stop("Unknown arguments: ", paste(args, collapse = " "))
}

con <- dbConnect(RMariaDB::MariaDB(),
  host = Sys.getenv("VUELOS_DB_HOST"), dbname = Sys.getenv("VUELOS_DB_NAME"),
  user = Sys.getenv("VUELOS_RO_USER"), password = Sys.getenv("VUELOS_RO_PASSWORD"))
newest <- dbGetQuery(con, "
  SELECT DATE_FORMAT(TIMESTAMPADD(HOUR, ?, MAX(last_seen_utc)), '%Y-%m-%d %H:%i') AS newest_local,
    TIMESTAMPDIFF(MINUTE, MAX(last_seen_utc), UTC_TIMESTAMP()) AS age_minutes
  FROM flight_movements
  WHERE airport_iata = ?",
  params = list(as.integer(UTC_OFFSET_HOURS), AIRPORT))
dbDisconnect(con)

age_minutes <- as.numeric(newest$age_minutes)
if (!is.na(age_minutes) && age_minutes <= max_age_hours * 60) {
  log_msg("fresh: newest data is", round(age_minutes / 60, 1), "hours old")
  quit(save = "no", status = 0)
}

message_text <- if (is.na(age_minutes)) {
  sprintf("flight_movements no tiene datos de %s.", AIRPORT)
} else {
  sprintf("El \u00faltimo dato de flight_movements tiene %.1f horas (%s, hora de RD). L\u00edmite: %s horas.",
          age_minutes / 60, newest$newest_local, format(max_age_hours))
}
log_msg("stale:", message_text)

alert_args <- c("send_alert.R", JOB, shQuote(message_text), "--summary", shQuote("sin datos nuevos"),
                if (dry_run) "--dry-run")
status <- system2("/usr/bin/Rscript", alert_args)
if (status != 0) stop("send_alert.R exited with status ", status)
