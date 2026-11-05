# send_report.R - email the daily_summary report for the previous SDQ local day.
# Usage:
#   Rscript send_report.R                      # send the previous SDQ local day to REPORT_TO
#   Rscript send_report.R --date 2026-11-02    # send one specific SDQ local date
#   Rscript send_report.R --dry-run            # write the HTML to logs/ without sending
#   Rscript send_report.R --date 2026-11-02 --dry-run
#
# A "day" is the SDQ local calendar date, as in daily_summary.R.
# Coverage = flights_with_delay_data / (flights_total - flights_cancelled - flights_diverted):
# cancelled and diverted flights never have delay data, so they are left out.
# On time % = flights with delay data and a delay <= 15 min / flights with delay data.
# Comparisons and the daily trend appear on their own once daily_summary has an earlier
# day, counting only days from FIRST_FULL_DAY on.
# The hourly and country charts read flight_movements, which is purged after 6 days:
# older dates show a note in their place. Everything else reads daily_summary.
# This file stays ASCII (HTML entities and \u escapes) and LC_CTYPE is set below,
# because cron may run R under a non-UTF-8 locale.

setwd(Sys.getenv("MONITOR_VUELOS_HOME", "/opt/monitor-vuelos"))
readRenviron(".Renviron")
invisible(Sys.setlocale("LC_CTYPE", "C.UTF-8"))
suppressPackageStartupMessages({
  library(DBI)
  library(ggplot2)
})

AIRPORT <- "SDQ"
UTC_OFFSET_HOURS <- -4
SMTP_HOST <- "smtp.gmail.com"
SMTP_PORT <- 587
FIRST_FULL_DAY <- as.Date("2026-11-03")  # 2026-11-02 was a partial capture
LOGO_PATH <- "assets/logo.png"
COLOR_NAVY <- "#0D2137"
COLOR_GREEN <- "#1D9E75"
COLOR_MUTED <- "#5A6776"
COLOR_GRID <- "#E3E8EE"
COLOR_PAGE <- "#F3F6F9"
CHART_FONT <- "DejaVu Sans"
CHART_CAPTION <- "Datos: AeroDataBox"
MAX_BAR_PX <- 24
TREND_DAYS <- 30
TOP_COUNTRIES <- 7
MONTHS_ES <- c("enero", "febrero", "marzo", "abril", "mayo", "junio", "julio",
               "agosto", "septiembre", "octubre", "noviembre", "diciembre")
DIRECTIONS_ES <- c(departure = "Salida", arrival = "Llegada")
# Countries without an entry here show their ISO code.
COUNTRIES_ES <- c(
  AG = "Antigua y Barbuda", AI = "Anguila", AR = "Argentina", AW = "Aruba", BB = "Barbados",
  BM = "Bermudas", BR = "Brasil", BS = "Bahamas", CA = "Canad\u00e1", CL = "Chile", CO = "Colombia",
  CR = "Costa Rica", CU = "Cuba", CW = "Curazao", DE = "Alemania", DM = "Dominica",
  DO = "Rep. Dominicana", EC = "Ecuador", ES = "Espa\u00f1a", FR = "Francia", GB = "Reino Unido",
  GP = "Guadalupe", GY = "Guyana", HT = "Hait\u00ed", IT = "Italia", JM = "Jamaica", KY = "Islas Caim\u00e1n",
  LC = "Santa Luc\u00eda", MF = "San Mart\u00edn", MQ = "Martinica", MX = "M\u00e9xico", NL = "Pa\u00edses Bajos",
  PA = "Panam\u00e1", PE = "Per\u00fa", PR = "Puerto Rico", PT = "Portugal", SR = "Surinam", SX = "Sint Maarten",
  TC = "Islas Turcas y Caicos", TR = "Turqu\u00eda", TT = "Trinidad y Tobago", US = "Estados Unidos",
  VE = "Venezuela", VG = "Islas V\u00edrgenes Brit\u00e1nicas", VI = "Islas V\u00edrgenes de EE. UU.")

log_msg <- function(...) cat(format(Sys.time(), "%Y-%m-%d %H:%M:%SZ", tz = "UTC"), ..., "\n")

sdq_today <- function() as.Date(format(Sys.time() + UTC_OFFSET_HOURS * 3600, "%Y-%m-%d", tz = "UTC"))

# SDQ local midnight of date d, expressed in UTC.
local_midnight_utc <- function(d) {
  format(as.POSIXct(paste(d, "00:00:00"), tz = "UTC") - UTC_OFFSET_HOURS * 3600, "%Y-%m-%d %H:%M:%S")
}

date_es <- function(d) {
  paste(as.integer(format(d, "%d")), "de", MONTHS_ES[as.integer(format(d, "%m"))], "de", format(d, "%Y"))
}

date_short_es <- function(d) {
  paste(as.integer(format(d, "%d")), substr(MONTHS_ES[as.integer(format(d, "%m"))], 1, 3))
}

fmt_int <- function(x) format(x, big.mark = ",", trim = TRUE)

coverage_pct <- function(with_data, total, cancelled, diverted) {
  eligible <- total - cancelled - diverted
  ifelse(eligible > 0, 100 * with_data / eligible, NA_real_)
}

on_time_pct <- function(with_data, delayed_15) {
  ifelse(with_data > 0, 100 * (with_data - delayed_15) / with_data, NA_real_)
}

fmt_pct <- function(x, digits = 1) ifelse(is.na(x), "n/d", sprintf(paste0("%.", digits, "f%%"), x))

fmt_delay <- function(x) ifelse(is.na(x), "n/d", sprintf("%.0f min", x))

# Airline name for each code; the code itself when flight_movements has no name.
airline_labels <- function(codes, airline_names) {
  found <- airline_names$airline_name[match(codes, airline_names$airline_iata)]
  ifelse(codes == "??", "Sin identificar", ifelse(is.na(found), codes, found))
}

country_labels <- function(codes) {
  ifelse(is.na(codes), "Sin dato", ifelse(codes %in% names(COUNTRIES_ES), COUNTRIES_ES[codes], codes))
}

# Days that comparisons and the trend may use.
comparable_days <- function(history) history[history$summary_date >= FIRST_FULL_DAY, ]

fetch_report_data <- function(d) {
  start_utc <- local_midnight_utc(d)
  end_utc <- local_midnight_utc(d + 1)
  con <- dbConnect(RMariaDB::MariaDB(),
    host = Sys.getenv("VUELOS_DB_HOST"), dbname = Sys.getenv("VUELOS_DB_NAME"),
    user = Sys.getenv("VUELOS_RO_USER"), password = Sys.getenv("VUELOS_RO_PASSWORD"))
  rows <- dbGetQuery(con, "
    SELECT direction, airline_iata, flights_total, flights_cancelled, flights_diverted,
      flights_with_delay_data, flights_delayed_15, flights_delayed_720, avg_delay_min
    FROM daily_summary
    WHERE summary_date = ? AND airport_iata = ?",
    params = list(format(d), AIRPORT))
  history <- dbGetQuery(con, "
    SELECT summary_date, SUM(flights_total) AS flights_total, SUM(flights_cancelled) AS flights_cancelled,
      SUM(flights_diverted) AS flights_diverted, SUM(flights_with_delay_data) AS flights_with_delay_data,
      SUM(flights_delayed_15) AS flights_delayed_15
    FROM daily_summary
    WHERE airport_iata = ?
    GROUP BY summary_date
    ORDER BY summary_date",
    params = list(AIRPORT))
  airline_names <- dbGetQuery(con, "
    SELECT airline_iata, MAX(NULLIF(TRIM(airline_name), '')) AS airline_name
    FROM flight_movements
    WHERE airport_iata = ? AND airline_iata IS NOT NULL
    GROUP BY airline_iata",
    params = list(AIRPORT))
  hours <- dbGetQuery(con, "
    SELECT HOUR(TIMESTAMPADD(HOUR, ?, sched_utc)) AS local_hour, COUNT(*) AS flights
    FROM flight_movements
    WHERE airport_iata = ? AND sched_utc >= ? AND sched_utc < ?
    GROUP BY local_hour",
    params = list(as.integer(UTC_OFFSET_HOURS), AIRPORT, start_utc, end_utc))
  countries <- dbGetQuery(con, "
    SELECT other_country_code, COUNT(*) AS flights
    FROM flight_movements
    WHERE airport_iata = ? AND sched_utc >= ? AND sched_utc < ?
    GROUP BY other_country_code",
    params = list(AIRPORT, start_utc, end_utc))
  dbDisconnect(con)

  counts <- c("flights_total", "flights_cancelled", "flights_diverted", "flights_with_delay_data",
              "flights_delayed_15", "flights_delayed_720", "avg_delay_min")
  rows[counts] <- lapply(rows[counts], as.numeric)
  rows$airline_label <- airline_labels(rows$airline_iata, airline_names)
  history[names(history) != "summary_date"] <- lapply(history[names(history) != "summary_date"], as.numeric)
  history$summary_date <- as.Date(history$summary_date)

  list(rows = rows, history = comparable_days(history),
       hours = data.frame(local_hour = as.numeric(hours$local_hour), flights = as.numeric(hours$flights)),
       countries = data.frame(code = countries$other_country_code, flights = as.numeric(countries$flights)))
}

# One row per airline, best on-time % first; airlines without delay data go last.
# avg_delay_min is weighted by the flights it covers, which excludes delays over 720 min.
airline_ranking <- function(rows) {
  ranking <- do.call(rbind, lapply(split(rows, rows$airline_iata), function(a) {
    with_data <- sum(a$flights_with_delay_data)
    delayed_15 <- sum(a$flights_delayed_15)
    avg_flights <- with_data - sum(a$flights_delayed_720)
    data.frame(
      airline_iata = a$airline_iata[1], airline_label = a$airline_label[1], flights = with_data,
      on_time = on_time_pct(with_data, delayed_15),
      avg_delay = if (avg_flights > 0) sum(a$avg_delay_min * (a$flights_with_delay_data - a$flights_delayed_720),
                                           na.rm = TRUE) / avg_flights else NA_real_)
  }))
  ranking[order(is.na(ranking$on_time), -ranking$on_time, -ranking$flights, ranking$airline_label), ]
}

day_kpis <- function(rows) {
  total <- sum(rows$flights_total)
  list(total = total, cancelled = sum(rows$flights_cancelled), delayed = sum(rows$flights_delayed_15),
       coverage = coverage_pct(sum(rows$flights_with_delay_data), total,
                               sum(rows$flights_cancelled), sum(rows$flights_diverted)))
}

# Text for what has no earlier day to compare with yet.
unavailable_text <- function(report_date, history) {
  if (any(history$summary_date > report_date)) "Sin d&iacute;a anterior" else "Disponible a partir de ma&ntilde;ana"
}

# --- charts ---------------------------------------------------------------------
theme_report <- function() {
  theme_minimal(base_size = 11, base_family = CHART_FONT) +
    theme(
      plot.background = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA),
      plot.caption = element_text(colour = COLOR_MUTED, size = rel(0.75), hjust = 0, margin = margin(t = 8)),
      plot.caption.position = "plot",
      plot.margin = margin(4, 8, 4, 4),
      panel.grid = element_blank(),
      axis.title = element_blank(),
      axis.text = element_text(colour = COLOR_MUTED),
      legend.title = element_blank(),
      legend.text = element_text(colour = COLOR_NAVY))
}

# Rendered at twice the display size so the PNG stays sharp on high-density screens.
save_chart <- function(plot, width_px, height_px) {
  path <- tempfile(fileext = ".png")
  png(path, width = width_px * 2, height = height_px * 2, res = 192, type = "cairo", bg = "white")
  print(plot)
  dev.off()
  path
}

# White glyph on a navy circle; identical icons collapse into one email attachment.
icon_file <- function(glyph) {
  path <- tempfile(fileext = ".png")
  png(path, width = 96, height = 96, res = 96, type = "cairo", bg = "transparent")
  grid::grid.newpage()
  grid::grid.circle(r = 0.5, gp = grid::gpar(fill = COLOR_NAVY, col = NA))
  grid::grid.text(glyph, gp = grid::gpar(col = "white", fontsize = 40, fontfamily = CHART_FONT))
  dev.off()
  path
}

ICON_PLANE <- "\u2708"
ICON_CANCEL <- "\u2715"
ICON_CLOCK <- "\u25f7"
ICON_CHECK <- "\u2713"
ICON_RING <- "\u25d4"
ICON_FLAG <- "\u2691"
ICON_TREND <- "\u2197"
ICON_TABLE <- "\u2630"

icon_img <- function(glyph, size) {
  as.character(blastula::add_image(icon_file(glyph), alt = "", width = size, align = "inline"))
}

# Each bar stays at or under MAX_BAR_PX however few rows the chart has.
capped_bar_width <- function(height_px, n, caption_lines = 1) {
  min(0.6, MAX_BAR_PX / ((height_px - 30 - 14 * caption_lines) / n))
}

ranking_chart <- function(ranking, height_px) {
  df <- ranking[!is.na(ranking$on_time), ]
  if (nrow(df) == 0) return(NULL)
  # Wrapped so a long airline name cannot squeeze out the value labels.
  axis_labels <- vapply(df$airline_label, function(x) paste(strwrap(x, 18), collapse = "\n"), "")
  names(axis_labels) <- df$airline_iata
  # Keyed on the code: two codes can share one airline name.
  df$airline_iata <- factor(df$airline_iata, levels = rev(df$airline_iata))
  df$value_label <-sprintf("%.0f%% \u00b7 prom. %s \u00b7 %.0f %s", df$on_time, fmt_delay(df$avg_delay), df$flights,
                            ifelse(df$flights == 1, "vuelo", "vuelos"))
  no_data <- ranking$airline_label[is.na(ranking$on_time)]
  caption <- CHART_CAPTION
  if (length(no_data) > 0) {
    caption <- paste0(paste(strwrap(paste("Sin dato de retraso:", paste(no_data, collapse = ", ")), 60), collapse = "\n"),
                      "\n", CHART_CAPTION)
  }
  caption_lines <- length(strsplit(caption, "\n")[[1]])
  plot <- ggplot(df, aes(on_time, airline_iata)) +
    geom_col(fill = COLOR_GREEN, width = capped_bar_width(height_px, nrow(df), caption_lines)) +
    geom_vline(xintercept = 0, colour = COLOR_GRID, linewidth = 0.4) +
    geom_text(aes(label = value_label), hjust = -0.04, size = 2.9, colour = COLOR_NAVY, family = CHART_FONT) +
    # The room past 100% is for the value labels.
    scale_x_continuous(limits = c(0, 320), expand = c(0, 0)) +
    scale_y_discrete(labels = axis_labels) +
    labs(caption = caption) +
    theme_report() +
    theme(axis.text.x = element_blank(), axis.text.y = element_text(colour = COLOR_NAVY))
  list(file = save_chart(plot, 406, height_px),
       alt = paste0("Puntualidad por aerol\u00ednea: ",
                    paste(df$airline_label, df$value_label, sep = " ", collapse = "; "), "."))
}

direction_ring <- function(rows) {
  departures <- sum(rows$flights_total[rows$direction == "departure"])
  arrivals <- sum(rows$flights_total[rows$direction == "arrival"])
  total <- departures + arrivals
  legend_labels <- c(
    Salidas = sprintf("Salidas\n%s (%.0f%%)", fmt_int(departures), 100 * departures / total),
    Llegadas = sprintf("Llegadas\n%s (%.0f%%)", fmt_int(arrivals), 100 * arrivals / total))
  df <- data.frame(
    direction = factor(c("Salidas", "Llegadas"), levels = c("Salidas", "Llegadas")),
    ymin = c(0, departures), ymax = c(departures, total))
  plot <- ggplot(df) +
    geom_rect(aes(xmin = 2, xmax = 3, ymin = ymin, ymax = ymax, fill = direction),
              colour = if (departures > 0 && arrivals > 0) "white" else NA, linewidth = 0.8) +
    annotate("text", x = 0, y = 0, label = fmt_int(total), vjust = -0.1, size = 6.5,
             fontface = "bold", colour = COLOR_NAVY, family = CHART_FONT) +
    annotate("text", x = 0, y = 0, label = "vuelos", vjust = 1.8, size = 2.9,
             colour = COLOR_MUTED, family = CHART_FONT) +
    coord_polar(theta = "y") +
    scale_x_continuous(limits = c(0, 3)) +
    scale_fill_manual(values = c(Salidas = COLOR_NAVY, Llegadas = COLOR_GREEN),
                      labels = legend_labels, drop = FALSE) +
    labs(caption = CHART_CAPTION) +
    theme_report() +
    # The ring's fixed aspect ratio centers the whole layout, so the caption is centered too.
    theme(axis.text = element_blank(), legend.position = "right",
          legend.key.spacing.y = unit(6, "pt"), plot.caption = element_text(hjust = 0.5))
  list(file = save_chart(plot, 320, 250),
       alt = gsub("\n", " ", paste0(legend_labels[["Salidas"]], "; ", legend_labels[["Llegadas"]], ".")))
}

hourly_chart <- function(hours) {
  if (nrow(hours) == 0) return(NULL)
  all_hours <- data.frame(local_hour = 0:23)
  all_hours$flights <- hours$flights[match(all_hours$local_hour, hours$local_hour)]
  all_hours$flights[is.na(all_hours$flights)] <- 0
  peak <- all_hours[all_hours$flights == max(all_hours$flights), ]
  y_top <- max(all_hours$flights)
  plot <- ggplot(all_hours, aes(local_hour, flights)) +
    geom_col(data = all_hours[all_hours$flights > 0, ], fill = COLOR_NAVY, width = 0.7) +
    geom_text(data = peak, aes(label = fmt_int(flights)), vjust = -0.5, size = 3,
              fontface = "bold", colour = COLOR_NAVY, family = CHART_FONT) +
    scale_x_continuous(breaks = seq(0, 21, 3), labels = sprintf("%02d:00", seq(0, 21, 3)),
                       limits = c(-0.6, 23.6), expand = c(0, 0)) +
    scale_y_continuous(breaks = unique(floor(pretty(c(0, y_top), n = 4))),
                       limits = c(0, y_top * 1.25), expand = c(0, 0)) +
    labs(caption = CHART_CAPTION) +
    theme_report() +
    theme(panel.grid.major.y = element_line(colour = COLOR_GRID, linewidth = 0.3))
  active <- all_hours[all_hours$flights > 0, ]
  list(file = save_chart(plot, 510, 250),
       alt = paste0("Vuelos por hora local: ",
                    paste(sprintf("%02d:00, %s", active$local_hour, fmt_int(active$flights)), collapse = "; "), "."))
}

country_chart <- function(countries, height_px) {
  if (nrow(countries) == 0) return(NULL)
  df <- countries[order(-countries$flights), ]
  df$label <- country_labels(df$code)
  # The tail folds into "Otros" so the chart stays readable on a phone.
  if (nrow(df) > TOP_COUNTRIES) {
    tail_rows <- df[-seq_len(TOP_COUNTRIES), ]
    df <- rbind(df[seq_len(TOP_COUNTRIES), c("label", "flights")],
                data.frame(label = sprintf("Otros (%d)", nrow(tail_rows)), flights = sum(tail_rows$flights)))
  }
  df$label <- factor(df$label, levels = rev(df$label))
  plot <- ggplot(df, aes(flights, label)) +
    geom_col(fill = COLOR_NAVY, width = capped_bar_width(height_px, nrow(df))) +
    geom_vline(xintercept = 0, colour = COLOR_GRID, linewidth = 0.4) +
    geom_text(aes(label = fmt_int(flights)), hjust = -0.3, size = 2.9, colour = COLOR_NAVY, family = CHART_FONT) +
    scale_x_continuous(limits = c(0, max(df$flights) * 1.15), expand = c(0, 0)) +
    labs(caption = CHART_CAPTION) +
    theme_report() +
    theme(axis.text.x = element_blank(), axis.text.y = element_text(colour = COLOR_NAVY))
  list(file = save_chart(plot, 406, height_px),
       alt = paste0("Vuelos por pa\u00eds: ", paste(df$label, df$flights, sep = " ", collapse = "; "), "."))
}

trend_chart <- function(history, report_date) {
  days <- tail(history[history$summary_date <= report_date, ], TREND_DAYS)
  if (nrow(days) < 2) return(NULL)
  y_top <- max(days$flights_total)
  breaks <- days$summary_date[unique(round(seq(1, nrow(days), length.out = min(nrow(days), 7))))]
  plot <- ggplot(days, aes(summary_date, flights_total)) +
    geom_area(fill = COLOR_NAVY, alpha = 0.1) +
    geom_line(colour = COLOR_NAVY, linewidth = 0.7, lineend = "round", linejoin = "round") +
    geom_point(colour = COLOR_NAVY, size = 1.8) +
    geom_text(data = days[nrow(days), ], aes(label = fmt_int(flights_total)), vjust = -0.9, size = 3,
              fontface = "bold", colour = COLOR_NAVY, family = CHART_FONT) +
    scale_x_date(breaks = breaks, labels = date_short_es, expand = c(0.03, 0)) +
    scale_y_continuous(breaks = unique(floor(pretty(c(0, y_top), n = 4))),
                       limits = c(0, y_top * 1.25), expand = c(0, 0)) +
    labs(caption = CHART_CAPTION) +
    theme_report() +
    theme(panel.grid.major.y = element_line(colour = COLOR_GRID, linewidth = 0.3))
  list(file = save_chart(plot, 640, 240),
       alt = paste0("Vuelos por d\u00eda: ",
                    paste(date_short_es(days$summary_date), days$flights_total, sep = ", ", collapse = "; "), "."))
}

# --- email HTML -----------------------------------------------------------------
# Change against the earlier day, with an arrow; better = TRUE or FALSE says which way
# is good news, NA means neither. Worse shows in navy: the brand has no alarm color.
delta_html <- function(current, previous, previous_date, unit, better, unavailable) {
  style <- function(color, text) {
    paste0('<div style="font-size:11px;color:', color, ';margin-top:4px;">', text, '</div>')
  }
  if (is.null(previous_date)) return(style(COLOR_MUTED, unavailable))
  versus <- paste("vs", date_short_es(previous_date))
  # Rounded as displayed, so a tiny change reads as no change and not as "+0.0 pp".
  change <- round(current - previous, 1)
  if (is.na(change)) return(style(COLOR_MUTED, paste("n/d", versus)))
  if (change == 0) return(style(COLOR_MUTED, paste("Sin cambio", versus)))
  value <- if (unit == "pp") sprintf("%+.1f pp", change) else paste0(if (change > 0) "+", fmt_int(change))
  color <-if (is.na(better)) COLOR_MUTED else if ((change > 0) == better) COLOR_GREEN else COLOR_NAVY
  style(color, paste(if (change > 0) "&#9650;" else "&#9660;", value, versus))
}

kpi_cell <- function(glyph, value, label, delta, value_color = COLOR_NAVY) {
  paste0(
    '<td width="25%" align="center" valign="top" style="padding:14px 4px;background-color:#FFFFFF;',
    'border-left:4px solid ', COLOR_GREEN, ';border-radius:8px;">',
    icon_img(glyph, 32),
    '<div style="font-size:24px;font-weight:bold;color:', value_color, ';margin-top:6px;">', value, '</div>',
    '<div style="font-size:12px;color:', COLOR_MUTED, ';">', label, '</div>',
    delta,
    '</td>')
}

# A row of cards. The "articles" class is blastula's own template rule that stacks the
# cells below 767px; in a table row, cards also share one height.
chart_row <- function(..., stack = TRUE) {
  paste0('<table', if (stack) ' class="articles"', ' width="100%" cellpadding="0" cellspacing="8" style="border-collapse:separate;">',
         '<tr>', ..., '</tr></table>')
}

# image_max_width keeps a chart's text at the same size as the others in a wider card.
chart_card <- function(glyph, title, subtitle, chart, empty_text, width, image_max_width = "none") {
  content <- if (is.null(chart)) {
    paste0('<div style="padding:28px 8px;font-size:13px;color:', COLOR_MUTED, ';text-align:center;">', empty_text, '</div>')
  } else {
    paste0('<div style="max-width:', image_max_width, ';margin:0 auto;">',
           as.character(blastula::add_image(chart$file, alt = chart$alt, width = "100%", align = "inline")),
           '</div>')
  }
  paste0(
    '<td width="', width, '" valign="top" style="box-sizing:border-box;background-color:#FFFFFF;',
    'border-radius:8px;padding:12px;margin-bottom:8px;text-align:left;">',
    # Inline blocks, not a nested table: the "articles" rule would stack its cells too.
    '<div style="margin-bottom:8px;">',
    '<div style="display:inline-block;vertical-align:middle;margin-right:8px;">', icon_img(glyph, 22), '</div>',
    '<div style="display:inline-block;vertical-align:middle;">',
    '<div style="font-size:15px;font-weight:bold;color:', COLOR_NAVY, ';">', title, '</div>',
    if (subtitle != "") paste0('<div style="font-size:12px;color:', COLOR_MUTED, ';">', subtitle, '</div>'),
    '</div></div>',
    content,
    '</td>')
}

airline_table <- function(rows, ranking) {
  rows <- rows[order(match(rows$airline_iata, ranking$airline_iata), rows$direction), ]
  th <- function(label, align = "right") {
    paste0('<th align="', align, '" style="padding:8px 6px;color:#FFFFFF;background-color:', COLOR_NAVY,
           ';font-size:12px;">', label, '</th>')
  }
  td <- function(value, align = "right", weight = "normal") {
    paste0('<td align="', align, '" style="padding:8px 6px;border-bottom:1px solid ', COLOR_GRID,
           ';font-size:13px;font-weight:', weight, ';color:', COLOR_NAVY, ';">', value, '</td>')
  }
  body <- paste0('<tr>',
    td(htmltools::htmlEscape(rows$airline_label), "left"),
    td(unname(DIRECTIONS_ES[as.character(rows$direction)]), "left"),
    td(fmt_pct(on_time_pct(rows$flights_with_delay_data, rows$flights_delayed_15), 0), weight = "bold"),
    td(fmt_delay(rows$avg_delay_min)),
    td(fmt_int(rows$flights_total)),
    td(fmt_int(rows$flights_cancelled)),
    td(fmt_pct(coverage_pct(rows$flights_with_delay_data, rows$flights_total,
                            rows$flights_cancelled, rows$flights_diverted))),
    '</tr>', collapse = "")
  paste0(
    '<table width="100%" cellpadding="0" cellspacing="0" style="border-collapse:collapse;">',
    '<tr>', th("Aerol&iacute;nea", "left"), th("Direcci&oacute;n", "left"), th("A tiempo"),
    th("Retraso prom."), th("Total"), th("Cancel."), th("Con hora real"), '</tr>',
    body, '</table>')
}

report_section <- function(data, report_date) {
  rows <- data$rows
  history <- data$history
  kpis <- day_kpis(rows)
  earlier <- history[history$summary_date < report_date, ]
  previous_date <- if (nrow(earlier) > 0) earlier$summary_date[nrow(earlier)] else NULL
  previous <- if (nrow(earlier) > 0) {
    last <- earlier[nrow(earlier), ]
    list(total = last$flights_total, cancelled = last$flights_cancelled, delayed = last$flights_delayed_15,
         coverage = coverage_pct(last$flights_with_delay_data, last$flights_total,
                                 last$flights_cancelled, last$flights_diverted))
  } else {
    list(total = NA, cancelled = NA, delayed = NA, coverage = NA)
  }
  unavailable <- unavailable_text(report_date, history)
  delta <- function(metric, unit, better) {
    delta_html(kpis[[metric]], previous[[metric]], previous_date, unit, better, unavailable)
  }

  ranking <- airline_ranking(rows)
  overall_on_time <- on_time_pct(sum(rows$flights_with_delay_data), sum(rows$flights_delayed_15))
  bars_height <- max(250, 70 + 36 * max(nrow(ranking), min(nrow(data$countries), TOP_COUNTRIES + 1)))
  purged_text <- "Sin detalle de vuelos disponible para este gr&aacute;fico."

  paste0(
    '<table width="100%" cellpadding="0" cellspacing="8" style="border-collapse:separate;margin-bottom:0;"><tr>',
    kpi_cell(ICON_PLANE, fmt_int(kpis$total), "Total de vuelos", delta("total", "n", NA)),
    kpi_cell(ICON_CANCEL, fmt_int(kpis$cancelled), "Cancelados", delta("cancelled", "n", FALSE)),
    kpi_cell(ICON_CLOCK, fmt_int(kpis$delayed), "Retraso &gt;15 min", delta("delayed", "n", FALSE)),
    kpi_cell(ICON_CHECK, fmt_pct(kpis$coverage), "Con hora real", delta("coverage", "pp", TRUE), COLOR_GREEN),
    '</tr></table>',
    chart_row(
      chart_card(ICON_PLANE, "Vuelos por hora local", "Hora programada en SDQ (UTC-4)",
                 hourly_chart(data$hours), purged_text, "61%"),
      chart_card(ICON_RING, "Salidas vs. llegadas", "Vuelos programados del d&iacute;a",
                 direction_ring(rows), "", "39%", "340px")),
    chart_row(
      chart_card(ICON_CLOCK, "Puntualidad por aerol&iacute;nea",
                 paste0("% a tiempo (retraso &le;15 min). Global: ", fmt_pct(overall_on_time, 0)),
                 ranking_chart(ranking, bars_height), "Sin dato de retraso para este d&iacute;a.", "50%"),
      chart_card(ICON_FLAG, "Vuelos por pa&iacute;s", "Origen o destino del vuelo",
                 country_chart(data$countries, bars_height), purged_text, "50%")),
    chart_row(
      chart_card(ICON_TREND, "Tendencia por d&iacute;a", "Total de vuelos por d&iacute;a",
                 trend_chart(history, report_date), unavailable, "100%", "640px"),
      stack = FALSE),
    '<div style="background-color:#FFFFFF;border-radius:8px;padding:12px;margin:0 8px 8px 8px;">',
    '<table cellpadding="0" cellspacing="0" style="margin-bottom:8px;"><tr>',
    '<td style="padding-right:8px;vertical-align:middle;">', icon_img(ICON_TABLE, 22), '</td>',
    '<td style="vertical-align:middle;font-size:15px;font-weight:bold;color:', COLOR_NAVY,
    ';">Ranking de aerol&iacute;neas</td></tr></table>',
    airline_table(rows, ranking),
    '<p style="font-size:12px;color:', COLOR_MUTED, ';margin:10px 0 0;">',
    'A tiempo: retraso &le;15 min / vuelos con dato de retraso.<br>',
    'Con hora real: vuelos con dato de retraso / (total - cancelados - desviados).<br>',
    'Depende de los datos que AeroDataBox recibe para SDQ; los retrasos se calculan solo sobre esos vuelos.</p>',
    '</div>')
}

no_data_section <- function(d) {
  paste0('<div style="background-color:#FFFFFF;border-radius:8px;padding:20px;margin:8px;">',
         '<p style="font-size:15px;color:', COLOR_NAVY, ';margin:0;">No hay datos de ', AIRPORT,
         ' para el ', date_es(d), '.</p></div>')
}

brand_html <- function() {
  if (file.exists(LOGO_PATH)) {
    as.character(blastula::add_image(LOGO_PATH, alt = "WilData Systems", width = 150, align = "inline"))
  } else {
    '<div style="font-size:18px;font-weight:bold;color:#FFFFFF;">WilData Systems</div>'
  }
}

email_html <- function(d, section) {
  paste0(
    '<div style="font-family:Arial,Helvetica,sans-serif;">',
    '<table width="100%" cellpadding="0" cellspacing="0" style="background-color:', COLOR_NAVY,
    ';border-bottom:4px solid ', COLOR_GREEN, ';"><tr>',
    '<td style="padding:18px 20px;vertical-align:middle;">', brand_html(), '</td>',
    '<td align="right" style="padding:18px 20px;vertical-align:middle;">',
    '<div style="font-size:20px;font-weight:bold;color:#FFFFFF;">Reporte diario de vuelos</div>',
    '<div style="font-size:13px;color:#FFFFFF;">', AIRPORT, ' &middot; ', date_es(d), ' (hora local RD)</div>',
    '</td></tr></table>',
    '<div style="padding:8px 4px;background-color:', COLOR_PAGE, ';">', section, '</div>',
    '<div style="padding:14px 20px;background-color:#FFFFFF;font-size:12px;color:', COLOR_MUTED, ';">',
    '<div>Datos: AeroDataBox</div>',
    '<div style="color:', COLOR_NAVY, ';">WilData Systems &mdash; Transformamos datos en decisiones inteligentes</div>',
    '</div>',
    '</div>')
}

# REPORT_TO holds one address or several separated by commas.
parse_recipients <- function(value) {
  recipients <- trimws(strsplit(value, ",")[[1]])
  unique(recipients[recipients != ""])
}

# blastula writes inline images with no file name; this adds it to each image part.
# Stops if any part does not match, so a changed blastula format cannot send a half-patched message.
add_image_filenames <- function(message, image_count) {
  pattern <- paste0("Content-Type: image/png\r\nContent-Transfer-Encoding: base64\r\n",
                    "Content-Disposition: inline\r\nContent-ID: <(img[0-9]+\\.png)>")
  replacement <- paste0("Content-Type: image/png; name=\"\\1\"\r\nContent-Transfer-Encoding: base64\r\n",
                        "Content-Disposition: inline; filename=\"\\1\"\r\nContent-ID: <\\1>")
  matches <- gregexpr(pattern, message, perl = TRUE)[[1]]
  found <- sum(matches > 0)
  if (found != image_count) stop("Expected ", image_count, " image parts in the message, found ", found)
  gsub(pattern, replacement, message, perl = TRUE)
}

# Same call as blastula::smtp_send, which offers no step to change the message before sending.
# Several recipients go only in the SMTP envelope, as BCC: the To header shows the sender
# and the message has no Bcc header, so no recipient sees the others.
send_email <- function(email, recipients, from, subject, credentials) {
  to_header <- if (length(recipients) == 1) recipients else from
  message <- add_image_filenames(blastula:::generate_rfc2822(email, subject = subject, from = from, to = to_header),
                                 length(email$images))
  result <- curl::send_mail(
    mail_from = from, mail_rcpt = recipients, message = message,
    smtp_server = paste0(credentials$host, ":", credentials$port), use_ssl = credentials$use_ssl,
    verbose = FALSE, username = credentials$user, password = credentials$password, login_options = "")
  if (result$status_code != 250) stop("The email message was NOT sent; the error code was ", result$status_code)
}

# --- main -----------------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
dry_run <- "--dry-run" %in% args
date_args <- args[args != "--dry-run"]
# A mistyped flag must never turn a preview into a real send.
if (length(date_args) > 0 && !(length(date_args) == 2 && date_args[1] == "--date")) {
  stop("Unknown arguments: ", paste(date_args, collapse = " "))
}
report_date <- if (length(date_args) == 2) as.Date(date_args[2], format = "%Y-%m-%d") else sdq_today() - 1
if (is.na(report_date)) stop("Invalid date; expected YYYY-MM-DD")
if (report_date >= sdq_today()) stop("--date must be before today in SDQ local time")

if (!dry_run) {
  required <- c("SMTP_USER", "SMTP_PASSWORD", "REPORT_TO")
  missing_vars <- required[Sys.getenv(required) == ""]
  if (length(missing_vars) > 0) stop("Missing in .Renviron: ", paste(missing_vars, collapse = ", "))
  recipients <- parse_recipients(Sys.getenv("REPORT_TO"))
  if (length(recipients) == 0) stop("REPORT_TO has no valid addresses")
}

data <- fetch_report_data(report_date)
has_data <- nrow(data$rows) > 0
section <- if (has_data) report_section(data, report_date) else no_data_section(report_date)
subject <- paste0("Reporte diario ", AIRPORT, " ", format(report_date), if (!has_data) ": sin datos")

email <- blastula::compose_email(
  body = htmltools::HTML(email_html(report_date, section)),
  title = subject)

if (dry_run) {
  out_path <- file.path("logs", paste0("send_report_", format(report_date), ".html"))
  # html_html keeps the images inline so the file opens in a browser; html_str, which
  # is what gets sent, refers to them as cid: attachments.
  writeLines(as.character(email$html_html), out_path, useBytes = TRUE)
  log_msg(format(report_date), ": dry run, rows =", nrow(data$rows), ", images =", length(email$images),
          ", HTML written to", out_path)
} else {
  send_email(
    email,
    recipients = recipients,
    from = Sys.getenv("SMTP_USER"),
    subject = subject,
    credentials = blastula::creds_envvar(
      user = Sys.getenv("SMTP_USER"), pass_envvar = "SMTP_PASSWORD",
      host = SMTP_HOST, port = SMTP_PORT, use_ssl = TRUE))
  log_msg(format(report_date), ": report sent, rows =", nrow(data$rows), ", recipients =", length(recipients))
}
