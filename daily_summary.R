# daily_summary.R - build permanent daily_summary rows for SDQ from flight_movements.
# Usage:
#   Rscript daily_summary.R                    # default: the two previous SDQ local days
#   Rscript daily_summary.R --date 2026-11-02  # one specific SDQ local date
#
# A "day" is the SDQ local calendar date of the scheduled time at SDQ.
# Dominican Republic is UTC-4 all year (no DST), so a fixed offset is used and
# MariaDB named time zones are not needed.

setwd(Sys.getenv("MONITOR_VUELOS_HOME", "/opt/monitor-vuelos"))
readRenviron(".Renviron")
suppressPackageStartupMessages(library(DBI))

AIRPORT <- "SDQ"
UTC_OFFSET_HOURS <- -4
# Detail rows are purged 6 days after first fetch, and purge.R runs up to 22:00 UTC, while
# that SDQ day is still open. Rows can be fetched hours before the flight, so older days
# may be incomplete, and rebuilding one would overwrite its permanent summary.
MAX_AGE_DAYS <- 4

log_msg <- function(...) cat(format(Sys.time(), "%Y-%m-%d %H:%M:%SZ", tz = "UTC"), ..., "\n")

sdq_today <- function() as.Date(format(Sys.time() + UTC_OFFSET_HOURS * 3600, "%Y-%m-%d", tz = "UTC"))

# SDQ local midnight of date d, expressed in UTC.
local_midnight_utc <- function(d) {
  format(as.POSIXct(paste(d, "00:00:00"), tz = "UTC") - UTC_OFFSET_HOURS * 3600, "%Y-%m-%d %H:%M:%S")
}

# Delay = best known time at SDQ minus scheduled time at SDQ, in minutes.
# runway_utc is used only when runway_utc <= last_seen_utc, i.e. it had already
# happened when the data was fetched (an actual time, not an estimate);
# otherwise revised_utc is used.
# Runway time is wheels time (takeoff / touchdown), not gate time:
#   - arrivals: touchdown comes before gate arrival, so delay is underestimated
#     by the taxi-in minutes;
#   - departures: takeoff comes after gate departure, so delay is overestimated
#     by the taxi-out minutes.
# Cancelled and diverted flights are counted, but excluded from delay metrics.
# Delays over 720 min (12 h) are usually rescheduled flights: they still count in
# flights_delayed_15 and flights_with_delay_data, are counted in
# flights_delayed_720, and are excluded from avg_delay_min and max_delay_min.
SUMMARY_SQL <- "
INSERT INTO daily_summary (
  summary_date, airport_iata, direction, airline_iata,
  flights_total, flights_cancelled, flights_diverted,
  flights_with_delay_data, flights_delayed_15, flights_delayed_720,
  avg_delay_min, max_delay_min,
  computed_utc
)
SELECT ?, airport_iata, direction, airline,
  COUNT(*),
  SUM(is_cancelled),
  SUM(is_diverted),
  SUM(in_metrics AND delay_min IS NOT NULL),
  SUM(in_metrics AND COALESCE(delay_min, 0) > 15),
  SUM(in_metrics AND COALESCE(delay_min, 0) > 720),
  ROUND(AVG(IF(in_metrics AND delay_min <= 720, delay_min, NULL)), 1),
  MAX(IF(in_metrics AND delay_min <= 720, delay_min, NULL)),
  UTC_TIMESTAMP()
FROM (
  SELECT airport_iata, direction,
    COALESCE(airline_iata, '??') AS airline,
    status IN ('Canceled', 'CanceledUncertain') AS is_cancelled,
    status = 'Diverted' AS is_diverted,
    status NOT IN ('Canceled', 'CanceledUncertain', 'Diverted') AS in_metrics,
    TIMESTAMPDIFF(MINUTE, sched_utc,
      CASE WHEN runway_utc IS NOT NULL AND runway_utc <= last_seen_utc THEN runway_utc
           ELSE revised_utc END) AS delay_min
  FROM flight_movements
  WHERE airport_iata = ? AND sched_utc >= ? AND sched_utc < ?
) t
GROUP BY airport_iata, direction, airline
"

summarize_day <- function(con, d, today) {
  if (d >= today) {
    log_msg(format(d), ": day not finished yet in SDQ, skipped")
    return(invisible())
  }
  if (as.integer(today - d) > MAX_AGE_DAYS) {
    log_msg(format(d), ": older than", MAX_AGE_DAYS, "days, detail may be purged; existing summary kept")
    return(invisible())
  }
  start_utc <- local_midnight_utc(d)
  end_utc <- local_midnight_utc(d + 1)
  n <- as.integer(dbGetQuery(con,
    "SELECT COUNT(*) n FROM flight_movements WHERE airport_iata = ? AND sched_utc >= ? AND sched_utc < ?",
    params = list(AIRPORT, start_utc, end_utc))$n)
  # Never wipe a permanent summary when there is no detail to rebuild it from.
  if (n == 0) {
    log_msg(format(d), ": no detail rows; existing summary left untouched")
    return(invisible())
  }
  dbWithTransaction(con, {
    dbExecute(con, "DELETE FROM daily_summary WHERE summary_date = ? AND airport_iata = ?",
              params = list(format(d), AIRPORT))
    dbExecute(con, SUMMARY_SQL, params = list(format(d), AIRPORT, start_utc, end_utc))
  })
  log_msg(format(d), ": summarized", n, "flights (UTC range", start_utc, "->", end_utc, ")")
}

# --- main -----------------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
today <- sdq_today()
dates <- if (length(args) == 2 && args[1] == "--date") as.Date(args[2]) else today - c(2, 1)
if (anyNA(dates)) stop("Invalid date; expected YYYY-MM-DD")

con <- dbConnect(RMariaDB::MariaDB(),
  host = Sys.getenv("VUELOS_DB_HOST"), dbname = Sys.getenv("VUELOS_DB_NAME"),
  user = Sys.getenv("VUELOS_DB_USER"), password = Sys.getenv("VUELOS_DB_PASSWORD"))
for (d in as.list(dates)) summarize_day(con, d, today)
dbDisconnect(con)
