# scripts/panel.R: coverage accounting over the archive span.

# One row per calendar day. `absent` (no snapshot at all) and `anomalous`
# (snapshot present but its row count deviates from the trailing median) are
# deliberately separate: conflating them would make an outage look like a
# disclosure change.
build_coverage <- function(snapshot_dates, row_counts, span_start, span_end) {
  snapshot_dates <- as.Date(snapshot_dates)
  ord <- order(snapshot_dates)
  snapshot_dates <- snapshot_dates[ord]
  row_counts <- as.numeric(row_counts)[ord]

  all_days <- seq(as.Date(span_start), as.Date(span_end), by = "day")
  idx <- match(all_days, snapshot_dates)

  n_rows <- rep(NA_real_, length(all_days))
  n_rows[!is.na(idx)] <- row_counts[idx[!is.na(idx)]]

  # Trailing median over the previous 30 present snapshots, excluding self.
  anomalous <- rep(FALSE, length(all_days))
  for (i in seq_along(snapshot_dates)) {
    if (i <= 1) next
    window <- row_counts[max(1, i - 30):(i - 1)]
    med <- stats::median(window)
    if (is.finite(med) && med > 0 &&
        abs(row_counts[i] - med) / med > COVERAGE_ANOMALY_FRAC) {
      anomalous[all_days == snapshot_dates[i]] <- TRUE
    }
  }

  present_index <- rep(NA_integer_, length(all_days))
  present_index[!is.na(idx)] <- as.integer(idx[!is.na(idx)])

  data.frame(date = all_days, absent = is.na(idx), anomalous = anomalous,
             n_rows = n_rows, present_index = present_index,
             stringsAsFactors = FALSE)
}
