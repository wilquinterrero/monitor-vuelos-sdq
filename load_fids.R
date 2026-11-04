# load_fids.R - fetch AeroDataBox FIDS for SDQ and upsert into flight_movements.
# Usage:
#   Rscript load_fids.R                 # live: call API (2 units), save raw JSON, load it
#   Rscript load_fids.R --file <path>   # offline: load an already saved raw JSON (0 units)

setwd(Sys.getenv("MONITOR_VUELOS_HOME", "/opt/monitor-vuelos"))
readRenviron(".Renviron")
suppressPackageStartupMessages({ library(httr2); library(DBI) })

AIRPORT <- "SDQ"
RAW_DIR <- "raw"

log_msg <- function(...) cat(format(Sys.time(), "%Y-%m-%d %H:%M:%SZ", tz = "UTC"), ..., "\n")

# --- 1. Get a raw JSON file (from API or from argument) -----------------------
fetch_fids <- function() {
  key <- Sys.getenv("AERODATABOX_KEY")
  if (!nzchar(key)) stop("AERODATABOX_KEY is empty")
  fetched_at <- Sys.time()
  resp <- request("https://aerodatabox.p.rapidapi.com") |>
    req_url_path_append("flights", "airports", "iata", AIRPORT) |>
    req_url_query(
      offsetMinutes = -360, durationMinutes = 720, direction = "Both",
      withLeg = "true", withCancelled = "true", withCodeshared = "false",
      withCargo = "false", withPrivate = "false", withLocation = "false"
    ) |>
    req_headers(
      `X-RapidAPI-Key` = key,
      `X-RapidAPI-Host` = "aerodatabox.p.rapidapi.com",
      .redact = "X-RapidAPI-Key"
    ) |>
    req_timeout(30) |>
    req_error(is_error = \(r) FALSE) |>
    req_perform()
  rm(key)

  status <- resp_status(resp)
  log_msg("API status:", status)
  # Log every rate-limit header (quota, remaining, reset...) for usage tracking.
  # Logged before the status checks so 429 and other errors are covered too.
  hdrs <- resp_headers(resp)
  rl <- hdrs[grepl("^x-ratelimit", names(hdrs), ignore.case = TRUE)]
  if (length(rl) == 0) log_msg("No x-ratelimit headers in response")
  for (h in names(rl)) log_msg("  ", tolower(h), "=", rl[[h]])
  if (status == 204) { log_msg("No flights in range"); quit(status = 0) }
  if (status != 200) stop("API error ", status, ": ", substr(resp_body_string(resp), 1, 300))

  dir.create(RAW_DIR, showWarnings = FALSE, mode = "0750")
  out <- file.path(RAW_DIR, format(fetched_at, paste0("fids_", AIRPORT, "_%Y%m%dT%H%M%SZ.json"), tz = "UTC"))
  writeBin(resp_body_raw(resp), out)
  log_msg("Saved", out)
  out
}

# Fetch time is encoded in the file name; it drives first_seen_utc / last_seen_utc.
fetched_at_from_name <- function(path) {
  ts <- regmatches(basename(path), regexpr("[0-9]{8}T[0-9]{6}", basename(path)))
  if (length(ts) == 0) stop("Cannot read fetch time from file name: ", path)
  format(as.POSIXct(ts, format = "%Y%m%dT%H%M%S", tz = "UTC"), "%Y-%m-%d %H:%M:%S")
}

# --- 2. Parse JSON into flight_movements rows ---------------------------------
pluck <- function(x, ...) {
  for (k in c(...)) { if (is.null(x[[k]])) return(NA); x <- x[[k]] }
  x
}
to_dt <- function(x) {  # "2026-11-03 05:17Z" -> "2026-11-03 05:17:00"
  if (is.na(x)) return(NA_character_)
  paste0(sub("Z$", "", x), ":00")
}

parse_fids <- function(path, fetched_at) {
  j <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  rows <- list()
  for (dir in c("departure", "arrival")) {
    other <- if (dir == "departure") "arrival" else "departure"
    for (f in j[[paste0(dir, "s")]]) {
      here <- f[[dir]]; there <- f[[other]]
      q <- here$quality
      rows[[length(rows) + 1]] <- data.frame(
        airport_iata       = AIRPORT,
        direction          = dir,
        flight_number      = pluck(f, "number"),
        call_sign          = pluck(f, "callSign"),
        airline_iata       = pluck(f, "airline", "iata"),
        airline_icao       = pluck(f, "airline", "icao"),
        airline_name       = pluck(f, "airline", "name"),
        other_airport_iata = pluck(there, "airport", "iata"),
        other_airport_icao = pluck(there, "airport", "icao"),
        other_airport_name = pluck(there, "airport", "name"),
        other_country_code = toupper(pluck(there, "airport", "countryCode")),
        sched_utc          = to_dt(pluck(here, "scheduledTime", "utc")),
        revised_utc        = to_dt(pluck(here, "revisedTime", "utc")),
        runway_utc         = to_dt(pluck(here, "runwayTime", "utc")),
        other_sched_utc    = to_dt(pluck(there, "scheduledTime", "utc")),
        other_revised_utc  = to_dt(pluck(there, "revisedTime", "utc")),
        other_runway_utc   = to_dt(pluck(there, "runwayTime", "utc")),
        status             = if (is.na(pluck(f, "status"))) "Unknown" else f$status,
        codeshare_status   = if (is.na(pluck(f, "codeshareStatus"))) "Unknown" else f$codeshareStatus,
        is_cargo           = isTRUE(f$isCargo),
        aircraft_model     = pluck(f, "aircraft", "model"),
        aircraft_reg       = pluck(f, "aircraft", "reg"),
        terminal           = pluck(here, "terminal"),
        gate               = pluck(here, "gate"),
        data_quality       = if (length(q)) paste(unlist(q), collapse = ",") else NA,
        first_seen_utc     = fetched_at,
        last_seen_utc      = fetched_at,
        stringsAsFactors   = FALSE
      )
    }
  }
  df <- do.call(rbind, rows)
  if (is.null(df)) return(df)
  df[!is.na(df$flight_number) & !is.na(df$sched_utc), ]
}

# --- 3. Upsert ------------------------------------------------------------------
# first_seen_utc is never updated. Mutable fields only change when the incoming
# snapshot is at least as recent as the stored one, so reprocessing an old raw
# file cannot overwrite newer data. Assignments run left to right in MariaDB,
# so last_seen_utc must be updated LAST.
UPSERT_SQL <- "
INSERT INTO flight_movements (
  airport_iata, direction, flight_number, call_sign, airline_iata, airline_icao,
  airline_name, other_airport_iata, other_airport_icao, other_airport_name,
  other_country_code, sched_utc, revised_utc, runway_utc, other_sched_utc,
  other_revised_utc, other_runway_utc, status, codeshare_status, is_cargo,
  aircraft_model, aircraft_reg, terminal, gate, data_quality,
  first_seen_utc, last_seen_utc
) VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)
ON DUPLICATE KEY UPDATE
  status            = IF(VALUES(last_seen_utc) >= last_seen_utc, VALUES(status), status),
  revised_utc       = IF(VALUES(last_seen_utc) >= last_seen_utc, COALESCE(VALUES(revised_utc), revised_utc), revised_utc),
  other_revised_utc = IF(VALUES(last_seen_utc) >= last_seen_utc, COALESCE(VALUES(other_revised_utc), other_revised_utc), other_revised_utc),
  runway_utc        = IF(VALUES(last_seen_utc) >= last_seen_utc, COALESCE(VALUES(runway_utc), runway_utc), runway_utc),
  other_runway_utc  = IF(VALUES(last_seen_utc) >= last_seen_utc, COALESCE(VALUES(other_runway_utc), other_runway_utc), other_runway_utc),
  terminal          = IF(VALUES(last_seen_utc) >= last_seen_utc, COALESCE(VALUES(terminal), terminal), terminal),
  gate              = IF(VALUES(last_seen_utc) >= last_seen_utc, COALESCE(VALUES(gate), gate), gate),
  aircraft_reg      = IF(VALUES(last_seen_utc) >= last_seen_utc, COALESCE(VALUES(aircraft_reg), aircraft_reg), aircraft_reg),
  last_seen_utc     = GREATEST(last_seen_utc, VALUES(last_seen_utc))
"

upsert <- function(df) {
  con <- dbConnect(RMariaDB::MariaDB(),
    host = Sys.getenv("VUELOS_DB_HOST"), dbname = Sys.getenv("VUELOS_DB_NAME"),
    user = Sys.getenv("VUELOS_DB_USER"), password = Sys.getenv("VUELOS_DB_PASSWORD"))
  on.exit(dbDisconnect(con))
  # COUNT(*) comes back as integer64; convert before doing arithmetic.
  count_rows <- function() as.integer(dbGetQuery(con, "SELECT COUNT(*) n FROM flight_movements")$n)
  before <- count_rows()
  dbWithTransaction(con, dbExecute(con, UPSERT_SQL, params = unname(as.list(df))))
  after <- count_rows()
  log_msg("Rows in file:", nrow(df), "| new:", after - before,
          "| existing (updated or unchanged):", nrow(df) - (after - before))
}

# --- main -----------------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
path <- if (length(args) == 2 && args[1] == "--file") args[2] else fetch_fids()
fetched_at <- fetched_at_from_name(path)
df <- parse_fids(path, fetched_at)
if (is.null(df) || nrow(df) == 0) { log_msg("No flights parsed from", path); quit(status = 0) }
log_msg("Parsed", nrow(df), "flights from", path, "(fetched", fetched_at, "UTC)")
upsert(df)
