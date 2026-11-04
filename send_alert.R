# send_alert.R - email a short alert to REPORT_TO with the tail of a job's log.
# Usage:
#   Rscript send_alert.R <job> <message>                      # subject: "<job> fallo"
#   Rscript send_alert.R <job> <message> --summary "<text>"   # subject: "<job> <text>"
#   Rscript send_alert.R <job> <message> --dry-run            # print it; send and record nothing
#
# At most one alert per job per SDQ local day. The day is recorded in logs/alerts/ only
# after the send succeeds, so a failed send can be retried by the next failure.
# The log text is redacted before use; if redaction fails, the log is left out.
# This file stays ASCII (HTML entities and \u escapes) and LC_CTYPE is set below,
# because cron may run R under a non-UTF-8 locale.

setwd(Sys.getenv("MONITOR_VUELOS_HOME", "/opt/monitor-vuelos"))
readRenviron(".Renviron")
invisible(Sys.setlocale("LC_CTYPE", "C.UTF-8"))

UTC_OFFSET_HOURS <- -4
SMTP_HOST <- "smtp.gmail.com"
SMTP_PORT <- 587
ALERT_DIR <- file.path("logs", "alerts")
LOG_TAIL_LINES <- 20
MAX_LINE_CHARS <- 500
MIN_SECRET_CHARS <- 6
REDACTED <- "<redacted>"
DEFAULT_SUMMARY <- "fall\u00f3"

# --- helpers --------------------------------------------------------------------
log_msg <- function(...) cat(format(Sys.time(), "%Y-%m-%d %H:%M:%SZ", tz = "UTC"), ..., "\n")

sdq_now <- function() Sys.time() + UTC_OFFSET_HOURS * 3600

# Values of the variables in an env file, longest first so that a value containing
# another one is replaced whole. Both forms count: the text in the file and the value R
# loaded, which differs when the file uses ${VAR} expansion.
secret_values <- function(path) {
  if (!file.exists(path)) return(character(0))
  lines <- readLines(path, warn = FALSE)
  lines <- lines[grepl("^[[:space:]]*[A-Za-z_][A-Za-z0-9_]*[[:space:]]*=", lines)]
  raw_values <- trimws(sub("^[^=]*=", "", lines))
  raw_values <- gsub("^[\"']|[\"']$", "", raw_values)
  loaded_values <- unname(Sys.getenv(trimws(sub("=.*$", "", lines))))
  values <- c(raw_values, loaded_values)
  values <- unique(values[nchar(values) >= MIN_SECRET_CHARS])
  values[order(-nchar(values))]
}

# Removes the literal secrets first, then anything shaped like a credential.
redact_lines <- function(lines, secrets) {
  lines <- iconv(lines, "UTF-8", "UTF-8", sub = "?")
  for (secret in secrets) lines <- gsub(secret, REDACTED, lines, fixed = TRUE)
  # Bearer first: the labelled rule would take the word "Bearer" as the value and leave the token.
  lines <- gsub("(?i)(bearer\\s+)\\S+", paste0("\\1", REDACTED), lines, perl = TRUE)
  # Same for any other scheme, such as "Authorization: Basic <credential>".
  lines <- gsub("(?i)(authorization[\"']?\\s*[=:]\\s*[\"']?\\S+\\s+)\\S+", paste0("\\1", REDACTED), lines, perl = TRUE)
  labelled <-"(?i)(token|password|passwd|pwd|secret|api[_-]?key|authorization)([\"']?\\s*[=:]\\s*[\"']?)[^\\s&\"']+"
  lines <- gsub(labelled, paste0("\\1\\2", REDACTED), lines, perl = TRUE)
  lines <- gsub("(://[^/:@\\s]+:)[^@\\s]+@", paste0("\\1", REDACTED, "@"), lines, perl = TRUE)
  lines <- gsub("\\b[A-Fa-f0-9]{32,}\\b", REDACTED, lines, perl = TRUE)
  lines <- gsub("\\b[A-Za-z0-9+/_=-]{40,}\\b", REDACTED, lines, perl = TRUE)
  lines <- gsub("[[:alnum:]._%+-]+@[[:alnum:].-]+\\.[[:alpha:]]+", REDACTED, lines, perl = TRUE)
  substr(lines, 1, MAX_LINE_CHARS)
}

# Never fails: an alert without its log is better than no alert.
log_tail <- function(job) {
  path <- file.path("logs", paste0(job, ".log"))
  if (!file.exists(path)) return("(no log file)")
  tryCatch({
    lines <- tail(readLines(path, warn = FALSE, skipNul = TRUE), LOG_TAIL_LINES)
    secrets <- secret_values(".Renviron")
    clean <- redact_lines(lines, secrets)
    for (secret in secrets) if (any(grepl(secret, clean, fixed = TRUE))) stop("secret survived redaction")
    if (length(clean) == 0) "(empty log)" else clean
  }, error = function(e) "(log omitted: it could not be checked safely)")
}

already_alerted <- function(state_path, today) {
  file.exists(state_path) && identical(readLines(state_path, n = 1, warn = FALSE), today)
}

parse_recipients <- function(value) {
  recipients <- trimws(strsplit(value, ",")[[1]])
  unique(recipients[recipients != ""])
}

alert_html <- function(job, summary, message, local_time, lines) {
  paste0(
    '<div style="font-family:Arial,Helvetica,sans-serif;color:#0D2137;">',
    '<p style="font-size:16px;font-weight:bold;margin:0 0 8px;">',
    htmltools::htmlEscape(paste(job, summary)), '</p>',
    '<p style="font-size:14px;margin:0 0 4px;">', htmltools::htmlEscape(message), '</p>',
    '<p style="font-size:13px;color:#5A6776;margin:0 0 12px;">Hora en RD: ', local_time, '</p>',
    '<pre style="font-size:12px;background-color:#F3F6F9;padding:12px;white-space:pre-wrap;">',
    htmltools::htmlEscape(paste(lines, collapse = "\n")), '</pre>',
    '</div>')
}

# Same call and BCC rule as send_report.R: several recipients go only in the SMTP envelope.
send_email <- function(email, recipients, from, subject, credentials) {
  to_header <- if (length(recipients) == 1) recipients else from
  message <- blastula:::generate_rfc2822(email, subject = subject, from = from, to = to_header)
  result <- curl::send_mail(
    mail_from = from, mail_rcpt = recipients, message = message,
    smtp_server = paste0(credentials$host, ":", credentials$port), use_ssl = credentials$use_ssl,
    verbose = FALSE, username = credentials$user, password = credentials$password, login_options = "")
  if (result$status_code != 250) stop("The alert was NOT sent; the error code was ", result$status_code)
}

# --- main -----------------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
dry_run <- "--dry-run" %in% args
args <- args[args != "--dry-run"]
summary_text <- DEFAULT_SUMMARY
summary_at <- match("--summary", args)
if (!is.na(summary_at)) {
  if (summary_at == length(args)) stop("--summary needs a value")
  summary_text <- args[summary_at + 1]
  args <- args[-c(summary_at, summary_at + 1)]
}
if (length(args) != 2) stop("Usage: send_alert.R <job> <message> [--summary <text>] [--dry-run]")
job <- args[1]
message_text <- gsub("[\r\n]+", " ", args[2])
summary_text <- gsub("[\r\n]+", " ", summary_text)
# The job name becomes a file name and part of a mail header.
if (!grepl("^[a-z0-9_]+$", job)) stop("Invalid job name")

today <- format(sdq_now(), "%Y-%m-%d", tz = "UTC")
state_path <- file.path(ALERT_DIR, paste0(job, ".sent"))
if (!dry_run && already_alerted(state_path, today)) {
  log_msg(job, ": alert already sent today, skipped")
  quit(save = "no", status = 0)
}

local_time <- format(sdq_now(), "%Y-%m-%d %H:%M", tz = "UTC")
lines <- log_tail(job)
subject <- paste0("[ALERTA monitor-vuelos] ", job, " ", summary_text)

if (dry_run) {
  cat("Subject:", subject, "\nHora en RD:", local_time, "\nMessage:", message_text, "\n--- log ---\n")
  cat(lines, sep = "\n")
  quit(save = "no", status = 0)
}

required <- c("SMTP_USER", "SMTP_PASSWORD", "REPORT_TO")
missing_vars <- required[Sys.getenv(required) == ""]
if (length(missing_vars) > 0) stop("Missing in .Renviron: ", paste(missing_vars, collapse = ", "))
recipients <- parse_recipients(Sys.getenv("REPORT_TO"))
if (length(recipients) == 0) stop("REPORT_TO has no valid addresses")

email <- blastula::compose_email(
  body = htmltools::HTML(alert_html(job, summary_text, message_text, local_time, lines)),
  title = subject)
send_email(
  email,
  recipients = recipients,
  from = Sys.getenv("SMTP_USER"),
  subject = subject,
  credentials = blastula::creds_envvar(
    user = Sys.getenv("SMTP_USER"), pass_envvar = "SMTP_PASSWORD",
    host = SMTP_HOST, port = SMTP_PORT, use_ssl = TRUE))

dir.create(ALERT_DIR, showWarnings = FALSE, mode = "0700")
writeLines(today, state_path)
log_msg(job, ": alert sent, recipients =", length(recipients))
