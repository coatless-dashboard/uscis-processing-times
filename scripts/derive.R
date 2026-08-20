# scripts/derive.R: derived tables. Every function is a pure rebuild from the
# raw panel; nothing here patches state incrementally.

.key_of <- function(df) {
  paste(df$form_name, df$office_code, df$form_subtype, sep = KEY_SEP)
}

# Run identity is the raw published bound quartet, compared as strings so that
# a unit re-expression (an upstream edit) starts a new run even when the
# normalized value is identical. NA-safe: two "See notes" bounds compare equal.
.bound_signature <- function(df) {
  paste(ifelse(is.na(df$range_lower), "NA", format(df$range_lower, trim = TRUE)),
        ifelse(is.na(df$range_lower_unit), "NA", df$range_lower_unit),
        ifelse(is.na(df$range_upper), "NA", format(df$range_upper, trim = TRUE)),
        ifelse(is.na(df$range_upper_unit), "NA", df$range_upper_unit),
        sep = KEY_SEP)
}

build_cycles <- function(snapshots, present_dates) {
  present_dates <- sort(unique(as.Date(present_dates)))
  s <- snapshots
  s$.key <- .key_of(s)
  s$.sig <- .bound_signature(s)
  # Rank among PRESENT snapshots, so a calendar gap (nothing observed) does not
  # look like a withdrawal (observed absence).
  s$.rank <- match(as.Date(s$snapshot_date), present_dates)
  s <- s[order(s$.key, s$.rank), ]

  n <- nrow(s)
  new_key     <- c(TRUE, s$.key[-1] != s$.key[-n])
  sig_changed <- c(TRUE, s$.sig[-1] != s$.sig[-n])
  gap_break   <- c(TRUE, diff(s$.rank) > 1)
  starts <- new_key | sig_changed | gap_break
  s$.run <- cumsum(starts)

  pub <- as.Date(s$publication_date)
  pub_changed <- c(TRUE, pub[-1] != pub[-n])
  # An edit is intra-cycle when the bounds moved but USCIS's stamp did not, and
  # it is not merely the first observation of the key or a post-absence restart.
  s$.intra <- sig_changed & !pub_changed & !new_key & !gap_break

  agg <- lapply(split(s, s$.run), function(g) {
    data.frame(
      form_name = g$form_name[1], office_code = g$office_code[1],
      form_subtype = g$form_subtype[1],
      range_lower = g$range_lower[1], range_lower_unit = g$range_lower_unit[1],
      range_upper = g$range_upper[1], range_upper_unit = g$range_upper_unit[1],
      first_seen = min(as.Date(g$snapshot_date)),
      last_seen  = max(as.Date(g$snapshot_date)),
      first_publication_date = min(as.Date(g$publication_date)),
      last_publication_date  = max(as.Date(g$publication_date)),
      n_observations = nrow(g),
      n_publication_cycles = length(unique(stats::na.omit(as.Date(g$publication_date)))),
      intra_cycle_edit = isTRUE(g$.intra[1]),
      stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, agg)
  out[order(out$form_name, out$office_code, out$form_subtype, out$first_seen), ]
}

# One event per transition between consecutive runs of the same key. Direction
# is taken from the upper bound alone: the two bounds are percentile positions
# under an 80% completion method, so their midpoint has no interpretation.
build_events <- function(cycles) {
  empty <- data.frame(form_name = character(0), office_code = character(0),
                      form_subtype = character(0), event_date = as.Date(character(0)),
                      prev_lower_months = numeric(0), new_lower_months = numeric(0),
                      prev_upper_months = numeric(0), new_upper_months = numeric(0),
                      delta_lower_months = numeric(0), delta_upper_months = numeric(0),
                      direction = character(0), intra_cycle_edit = logical(0),
                      stringsAsFactors = FALSE)
  if (nrow(cycles) < 2) return(empty)

  c2 <- cycles
  c2$.key <- .key_of(c2)
  c2 <- c2[order(c2$.key, c2$first_seen), ]
  c2$lower_months <- normalize_bound(c2$range_lower, c2$range_lower_unit)$months
  c2$upper_months <- normalize_bound(c2$range_upper, c2$range_upper_unit)$months

  n <- nrow(c2)
  keep <- c(FALSE, c2$.key[-1] == c2$.key[-n])
  if (!any(keep)) return(empty)

  cur <- which(keep); prev <- cur - 1L
  d_up <- c2$upper_months[cur] - c2$upper_months[prev]

  data.frame(
    form_name = c2$form_name[cur], office_code = c2$office_code[cur],
    form_subtype = c2$form_subtype[cur], event_date = c2$first_seen[cur],
    prev_lower_months = c2$lower_months[prev], new_lower_months = c2$lower_months[cur],
    prev_upper_months = c2$upper_months[prev], new_upper_months = c2$upper_months[cur],
    delta_lower_months = c2$lower_months[cur] - c2$lower_months[prev],
    delta_upper_months = d_up,
    direction = ifelse(is.na(d_up), "unknown",
                ifelse(d_up > 0, "worsened",
                ifelse(d_up < 0, "improved", "unchanged"))),
    intra_cycle_edit = c2$intra_cycle_edit[cur],
    stringsAsFactors = FALSE)
}

# The inquiry date advances one day per calendar day, so the informative signal
# is deviation. Expectation is derived from observed snapshot spacing rather
# than hardcoded to 1, otherwise every outage fires a spurious leap for nearly
# every key. Classes are evaluated in precedence order and are exhaustive.
build_inquiry_events <- function(snapshots) {
  s <- snapshots
  s$.key <- .key_of(s)
  s <- s[order(s$.key, as.Date(s$snapshot_date)), ]
  n <- nrow(s)

  same <- c(FALSE, s$.key[-1] == s$.key[-n])
  prev_snap <- c(as.Date(NA), as.Date(s$snapshot_date)[-n])
  prev_srd  <- c(as.Date(NA), as.Date(s$service_request_date)[-n])
  prev_snap[!same] <- as.Date(NA)
  prev_srd[!same]  <- as.Date(NA)

  expected <- as.numeric(as.Date(s$snapshot_date) - prev_snap)
  observed <- as.numeric(as.Date(s$service_request_date) - prev_srd)
  excess   <- observed - expected

  cls <- rep(NA_character_, n)
  cls[is.na(prev_snap)] <- "first_observation"
  cls[is.na(cls) & !is.na(observed) & observed < 0] <- "backward"
  cls[is.na(cls) & !is.na(observed) & observed == 0 & expected >= 1] <- "stalled"
  cls[is.na(cls) & !is.na(expected) & expected > 1] <- "post_gap"
  cls[is.na(cls) & !is.na(excess) & abs(excess) > INQUIRY_LEAP_TOLERANCE_DAYS] <- "leap"
  cls[is.na(cls)] <- "normal"

  data.frame(
    form_name = s$form_name, office_code = s$office_code,
    form_subtype = s$form_subtype, snapshot_date = as.Date(s$snapshot_date),
    service_request_date = as.Date(s$service_request_date),
    expected_delta_days = expected, observed_delta_days = observed,
    delta_excess_days = excess, anomaly_class = cls, stringsAsFactors = FALSE)
}

# Withdrawal is only meaningful against snapshots we actually have. Operating on
# present snapshots (not calendar days) is what keeps the 38 missing days from
# manufacturing thousands of spurious disappearances.
build_disclosure_events <- function(snapshots, present_dates, rekey = NULL) {
  present_dates <- sort(unique(as.Date(present_dates)))
  empty <- data.frame(form_name = character(0), office_code = character(0),
                      form_subtype = character(0), event_type = character(0),
                      event_date = as.Date(character(0)), status = character(0),
                      date_uncertainty_days = numeric(0),
                      successor_key = character(0), stringsAsFactors = FALSE)
  if (nrow(snapshots) == 0) return(empty)

  s <- snapshots
  s$.key <- .key_of(s)
  # Days not observed immediately before each present snapshot.
  gap_before <- c(0, as.numeric(diff(present_dates)) - 1)

  out <- lapply(split(s, s$.key), function(g) {
    fn <- g$form_name[1]; oc <- g$office_code[1]; fs <- g$form_subtype[1]
    seen <- present_dates %in% as.Date(g$snapshot_date)
    first_i <- which(seen)[1]

    succ <- NA_character_
    if (!is.null(rekey) && nrow(rekey) > 0) {
      hit <- rekey$form_name == fn & rekey$office_code == oc & rekey$form_subtype == fs
      if (any(hit)) succ <- rekey$successor_subtype[which(hit)[1]]
    }

    ev <- list(); i <- first_i
    while (i <= length(present_dates)) {
      if (!seen[i]) {
        run_end <- i
        while (run_end < length(present_dates) && !seen[run_end + 1]) run_end <- run_end + 1
        run_len <- run_end - i + 1
        if (run_len >= DISCLOSURE_MIN_ABSENT) {
          ev[[length(ev) + 1]] <- data.frame(
            form_name = fn, office_code = oc, form_subtype = fs,
            event_type = "disappeared", event_date = present_dates[i],
            status = if (run_len >= DISCLOSURE_CONFIRM_ABSENT) "confirmed" else "provisional",
            date_uncertainty_days = gap_before[i], successor_key = succ,
            stringsAsFactors = FALSE)
          if (run_end < length(present_dates)) {
            ev[[length(ev) + 1]] <- data.frame(
              form_name = fn, office_code = oc, form_subtype = fs,
              event_type = "retracted", event_date = present_dates[run_end + 1],
              status = "retracted", date_uncertainty_days = gap_before[run_end + 1],
              successor_key = succ, stringsAsFactors = FALSE)
          }
        }
        i <- run_end + 1
      } else {
        i <- i + 1
      }
    }
    if (length(ev) == 0) return(NULL)
    do.call(rbind, ev)
  })

  out <- Filter(Negate(is.null), out)
  if (length(out) == 0) return(empty)
  res <- do.call(rbind, out)
  res[order(res$event_date, res$form_name, res$office_code, res$form_subtype), ]
}

# The monthly table is a PROJECTION of pt_cycles, not a re-aggregation of daily
# rows. The month label is derived from the cycle's first observation and is
# not a key: two publications inside one calendar month emit two rows.
project_monthly <- function(cycles) {
  out <- cycles
  out$month <- format(as.Date(out$first_seen), "%Y-%m")
  out[order(out$form_name, out$office_code, out$form_subtype, out$first_seen), ]
}

# Any over-time comparison must run on keys present at BOTH endpoints; the
# published row count fell ~30% across 2025, so an unbalanced comparison
# measures disclosure policy rather than processing time.
balanced_keys <- function(snapshots, date_a, date_b) {
  k <- function(d) {
    g <- snapshots[as.Date(snapshots$snapshot_date) == as.Date(d), ]
    unique(.key_of(g))
  }
  ka <- k(date_a); kb <- k(date_b)
  both <- intersect(ka, kb)
  list(keys = both,
       excluded_a = length(setdiff(ka, both)),
       excluded_b = length(setdiff(kb, both)))
}
