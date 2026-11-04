-- 001_create_tables.sql - initial schema for vuelos_rd
USE vuelos_rd;

-- Flight detail. Rows purged 6 days after first fetch, under the 7-day limit (strict reading of AeroDataBox terms).
CREATE TABLE flight_movements (
  id                  BIGINT UNSIGNED NOT NULL AUTO_INCREMENT,
  airport_iata        CHAR(3)      NOT NULL,
  direction           ENUM('departure','arrival') NOT NULL,
  flight_number       VARCHAR(10)  NOT NULL,
  call_sign           VARCHAR(10)  NULL,
  airline_iata        VARCHAR(3)   NULL,
  airline_icao        VARCHAR(4)   NULL,
  airline_name        VARCHAR(100) NULL,
  other_airport_iata  VARCHAR(3)   NULL,
  other_airport_icao  VARCHAR(4)   NULL,
  other_airport_name  VARCHAR(100) NULL,
  other_country_code  CHAR(2)      NULL,
  sched_utc           DATETIME     NOT NULL,
  revised_utc         DATETIME     NULL,
  runway_utc          DATETIME     NULL,
  other_sched_utc     DATETIME     NULL,
  other_revised_utc   DATETIME     NULL,
  other_runway_utc    DATETIME     NULL,
  status              VARCHAR(20)  NOT NULL,
  codeshare_status    VARCHAR(20)  NOT NULL,
  is_cargo            BOOLEAN      NOT NULL,
  aircraft_model      VARCHAR(60)  NULL,
  aircraft_reg        VARCHAR(15)  NULL,
  terminal            VARCHAR(10)  NULL,
  gate                VARCHAR(10)  NULL,
  data_quality        VARCHAR(30)  NULL,
  first_seen_utc      DATETIME     NOT NULL,
  last_seen_utc       DATETIME     NOT NULL,
  PRIMARY KEY (id),
  UNIQUE KEY uq_flight (airport_iata, direction, flight_number, sched_utc),
  KEY ix_sched (sched_utc),
  KEY ix_first_seen (first_seen_utc)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- Permanent daily aggregates (no per-flight data).
CREATE TABLE daily_summary (
  summary_date            DATE         NOT NULL,
  airport_iata            CHAR(3)      NOT NULL,
  direction               ENUM('departure','arrival') NOT NULL,
  airline_iata            VARCHAR(3)   NOT NULL,
  flights_total           INT UNSIGNED NOT NULL,
  flights_cancelled       INT UNSIGNED NOT NULL,
  flights_diverted        INT UNSIGNED NOT NULL,
  flights_with_delay_data INT UNSIGNED NOT NULL,
  flights_delayed_15      INT UNSIGNED NOT NULL,
  avg_delay_min           DECIMAL(6,1) NULL,
  max_delay_min           INT          NULL,
  computed_utc            DATETIME     NOT NULL,
  PRIMARY KEY (summary_date, airport_iata, direction, airline_iata)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
