# export_csv.R - export CSV files for the Power BI dashboard, using the read-only user.
# Usage:
#   Rscript export_csv.R
#
# Writes to data/powerbi/, replacing any previous export:
#   daily_summary.csv      every row of daily_summary
#   flights_detail_7d.csv  flight_movements scheduled in the last 7 days, up to now
#                          and first fetched in the last 5 days
#
# Files are UTF-8 and comma-separated, with empty cells for NULL. Dates are YYYY-MM-DD.
# Datetimes are UTC, as YYYY-MM-DD HH:MM:SS.
#
# flights_detail_7d.csv holds AeroDataBox detail data, and purge.R does not reach it.
# Each copy must be replaced or deleted before any row in it is 7 days past its first
# fetch. Exporting rows up to 5 days old leaves about 28 hours of margin at the next daily
# export, so one failed export stays under the limit but two do not.

setwd(Sys.getenv("MONITOR_VUELOS_HOME", "/opt/monitor-vuelos"))
readRenviron(".Renviron")
invisible(Sys.setlocale("LC_CTYPE", "C.UTF-8"))
suppressPackageStartupMessages(library(DBI))

OUTPUT_DIR <- "data/powerbi"

log_msg <- function(...) cat(format(Sys.time(), "%Y-%m-%d %H:%M:%SZ", tz = "UTC"), ..., "\n")

SUMMARY_SQL <- "
SELECT
  DATE_FORMAT(summary_date, '%Y-%m-%d') AS summary_date,
  airport_iata, direction, airline_iata,
  flights_total, flights_cancelled, flights_diverted,
  flights_with_delay_data, flights_delayed_15, flights_delayed_720,
  avg_delay_min, max_delay_min,
  DATE_FORMAT(computed_utc, '%Y-%m-%d %H:%i:%s') AS computed_utc
FROM daily_summary
ORDER BY summary_date, airport_iata, direction, airline_iata
"

# Columns are listed by name so that internal ones (id, first_seen_utc, last_seen_utc)
# and any column added later stay out of the export.
DETAIL_SQL <- "
SELECT
  airport_iata, direction, flight_number, call_sign,
  airline_iata, airline_icao, airline_name,
  other_airport_iata, other_airport_icao, other_airport_name, other_country_code,
  DATE_FORMAT(sched_utc, '%Y-%m-%d %H:%i:%s') AS sched_utc,
  DATE_FORMAT(revised_utc, '%Y-%m-%d %H:%i:%s') AS revised_utc,
  DATE_FORMAT(runway_utc, '%Y-%m-%d %H:%i:%s') AS runway_utc,
  DATE_FORMAT(other_sched_utc, '%Y-%m-%d %H:%i:%s') AS other_sched_utc,
  DATE_FORMAT(other_revised_utc, '%Y-%m-%d %H:%i:%s') AS other_revised_utc,
  DATE_FORMAT(other_runway_utc, '%Y-%m-%d %H:%i:%s') AS other_runway_utc,
  status, codeshare_status, is_cargo,
  aircraft_model, aircraft_reg, terminal, gate, data_quality
FROM flight_movements
WHERE flight_movements.sched_utc >= UTC_TIMESTAMP() - INTERVAL 7 DAY
  AND flight_movements.sched_utc <= UTC_TIMESTAMP()
  AND flight_movements.first_seen_utc >= UTC_TIMESTAMP() - INTERVAL 5 DAY
ORDER BY sched_utc, airport_iata, direction, flight_number
"

con <- dbConnect(RMariaDB::MariaDB(),
  host = Sys.getenv("VUELOS_DB_HOST"), dbname = Sys.getenv("VUELOS_DB_NAME"),
  user = Sys.getenv("VUELOS_RO_USER"), password = Sys.getenv("VUELOS_RO_PASSWORD"))
summary_rows <- dbGetQuery(con, SUMMARY_SQL)
detail_rows <- dbGetQuery(con, DETAIL_SQL)
dbDisconnect(con)

# An empty result means a problem upstream: keep the previous export instead of blanking it.
if (nrow(summary_rows) == 0) stop("daily_summary returned no rows; nothing written")
if (nrow(detail_rows) == 0) stop("flight_movements returned no rows for the last 7 days; nothing written")

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

write_export <- function(rows, file_name) {
  path <- file.path(OUTPUT_DIR, file_name)
  write.csv(rows, path, row.names = FALSE, na = "", fileEncoding = "UTF-8")
  log_msg(path, ":", nrow(rows), "rows,", ncol(rows), "columns")
}

write_export(summary_rows, "daily_summary.csv")
write_export(detail_rows, "flights_detail_7d.csv")
