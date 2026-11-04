-- 002_add_flights_delayed_720.sql - count delays over 720 min (12 h) in daily_summary
USE vuelos_rd;

ALTER TABLE daily_summary
  ADD COLUMN flights_delayed_720 INT UNSIGNED NOT NULL DEFAULT 0 AFTER flights_delayed_15;
