#!/bin/sh
# run_job.sh - run <job>.R, append its output to logs/<job>.log, and alert if it fails.
# Usage:
#   run_job.sh <job> [script args...]
#   run_job.sh --timeout <seconds> <job> [script args...]
#
# Exits with the script's own exit code, so "run_job.sh a && run_job.sh b" still skips b
# when a fails. An alert problem is logged to logs/send_alert.log and never changes it.

PROJECT="${MONITOR_VUELOS_HOME:-/opt/monitor-vuelos}"
RSCRIPT=/usr/bin/Rscript
TIMEOUT=/usr/bin/timeout
ALERT_TIMEOUT_SECONDS=120
# A process blocked in a native call may ignore SIGTERM; SIGKILL follows so the lock is freed.
KILL_AFTER_SECONDS=30

timeout_seconds=""
if [ "${1:-}" = "--timeout" ]; then
  timeout_seconds="${2:-}"
  case "$timeout_seconds" in
    ''|*[!0-9]*) echo "run_job.sh: --timeout needs a number of seconds" >&2; exit 64 ;;
  esac
  shift 2
fi

job="${1:-}"
case "$job" in
  ''|*[!a-z0-9_]*) echo "run_job.sh: invalid job name" >&2; exit 64 ;;
esac
shift

cd "$PROJECT" || exit 1

if [ -n "$timeout_seconds" ]; then
  "$TIMEOUT" -k "$KILL_AFTER_SECONDS" "$timeout_seconds" "$RSCRIPT" "$job.R" "$@" >> "logs/$job.log" 2>&1
else
  "$RSCRIPT" "$job.R" "$@" >> "logs/$job.log" 2>&1
fi
status=$?

if [ "$status" -ne 0 ]; then
  # timeout exits 124 after SIGTERM and 137 when it had to use SIGKILL.
  if { [ "$status" -eq 124 ] || [ "$status" -eq 137 ]; } && [ -n "$timeout_seconds" ]; then
    message="Stopped by the ${timeout_seconds} s timeout (exit code $status)"
  else
    message="Exit code $status"
  fi
  "$TIMEOUT" -k "$KILL_AFTER_SECONDS" "$ALERT_TIMEOUT_SECONDS" "$RSCRIPT" send_alert.R "$job" "$message" >> logs/send_alert.log 2>&1 \
    || echo "$(date -u '+%Y-%m-%d %H:%M:%SZ') $job : could not send the alert" >> logs/send_alert.log
fi

exit "$status"
