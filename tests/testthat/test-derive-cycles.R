# tests/testthat/test-derive-cycles.R
snap <- function(date, lo, lo_u, up, up_u, pub) {
  data.frame(snapshot_date = as.Date(date), form_name = "I-129",
             office_code = "SCD", form_subtype = "137-E",
             range_lower = lo, range_lower_unit = lo_u,
             range_upper = up, range_upper_unit = up_u,
             publication_date = as.Date(pub), stringsAsFactors = FALSE)
}

test_that("a run persists while raw bounds are unchanged", {
  s <- rbind(snap("2026-01-01", 4, "Months", 7, "Months", "2025-12-18"),
             snap("2026-01-02", 4, "Months", 7, "Months", "2025-12-18"),
             snap("2026-01-03", 4, "Months", 7, "Months", "2025-12-18"))
  out <- build_cycles(s, present_dates = as.Date(c("2026-01-01","2026-01-02","2026-01-03")))
  expect_equal(nrow(out), 1L)
  expect_equal(out$first_seen, as.Date("2026-01-01"))
  expect_equal(out$last_seen,  as.Date("2026-01-03"))
  expect_equal(out$n_observations, 3L)
})

test_that("a bound change starts a new run", {
  s <- rbind(snap("2026-01-01", 4, "Months", 7, "Months", "2025-12-18"),
             snap("2026-01-02", 5, "Months", 7, "Months", "2026-01-14"))
  out <- build_cycles(s, present_dates = as.Date(c("2026-01-01","2026-01-02")))
  expect_equal(nrow(out), 2L)
  expect_equal(out$range_lower, c(4, 5))
})

test_that("a unit re-expression with identical normalized value is a new run", {
  # 0.5 Months and 15 Days are the same duration but a different publication.
  s <- rbind(snap("2026-01-01", 0.5, "Months", 7, "Months", "2025-12-18"),
             snap("2026-01-02", 15,  "Days",   7, "Months", "2025-12-18"))
  out <- build_cycles(s, present_dates = as.Date(c("2026-01-01","2026-01-02")))
  expect_equal(nrow(out), 2L)
})

test_that("two unparseable bounds compare equal rather than always differing", {
  s <- rbind(snap("2026-01-01", NA, "See notes", 7, "Months", "2025-12-18"),
             snap("2026-01-02", NA, "See notes", 7, "Months", "2025-12-18"))
  out <- build_cycles(s, present_dates = as.Date(c("2026-01-01","2026-01-02")))
  expect_equal(nrow(out), 1L)
})

test_that("a run breaks across a withdrawal so first_seen never spans absence", {
  # Key absent on 2026-01-02, which IS a present snapshot.
  s <- rbind(snap("2026-01-01", 4, "Months", 7, "Months", "2025-12-18"),
             snap("2026-01-03", 4, "Months", 7, "Months", "2025-12-18"))
  out <- build_cycles(s, present_dates = as.Date(c("2026-01-01","2026-01-02","2026-01-03")))
  expect_equal(nrow(out), 2L)
})

test_that("a run survives a coverage gap, since absence was not observed", {
  # 2026-01-02 has no snapshot at all, so nothing was observed about the key.
  s <- rbind(snap("2026-01-01", 4, "Months", 7, "Months", "2025-12-18"),
             snap("2026-01-03", 4, "Months", 7, "Months", "2025-12-18"))
  out <- build_cycles(s, present_dates = as.Date(c("2026-01-01","2026-01-03")))
  expect_equal(nrow(out), 1L)
})

test_that("intra_cycle_edit flags a bound change with no publication_date change", {
  s <- rbind(snap("2026-01-01", 4, "Months", 7, "Months", "2025-12-18"),
             snap("2026-01-02", 5, "Months", 7, "Months", "2025-12-18"))
  out <- build_cycles(s, present_dates = as.Date(c("2026-01-01","2026-01-02")))
  expect_equal(out$intra_cycle_edit, c(FALSE, TRUE))
})

test_that("intra_cycle_edit does not fire on a second key's first observation, even when its publication_date coincides with the prior key's last row", {
  # Global publication_date comparison is key-agnostic: key B's first row here
  # shares its neighbor's publication_date only because key A's last row
  # precedes it in the sorted table. Without the !new_key guard this would be
  # misread as an intra-cycle edit on key B, even though it is key B's very
  # first observation.
  a <- snap("2026-01-01", 4, "Months", 7, "Months", "2025-12-18")
  b <- snap("2026-01-02", 5, "Months", 7, "Months", "2025-12-18")
  b$form_name <- "I-130"
  s <- rbind(a, b)
  out <- build_cycles(s, present_dates = as.Date(c("2026-01-01", "2026-01-02")))
  expect_equal(nrow(out), 2L)
  expect_equal(out$intra_cycle_edit, c(FALSE, FALSE))
})

test_that("intra_cycle_edit does not fire on the first run after a withdrawal, even when bounds changed and publication_date did not", {
  # Key absent on 2026-01-02 (a present snapshot), then reappears on
  # 2026-01-03 with different bounds but the same publication_date as its
  # pre-absence row. Without the !gap_break guard this restart would be
  # misread as an intra-cycle edit rather than a post-absence resumption.
  s <- rbind(snap("2026-01-01", 4, "Months", 7, "Months", "2025-12-18"),
             snap("2026-01-03", 6, "Months", 7, "Months", "2025-12-18"))
  out <- build_cycles(s, present_dates = as.Date(c("2026-01-01", "2026-01-02", "2026-01-03")))
  expect_equal(nrow(out), 2L)
  expect_equal(out$intra_cycle_edit, c(FALSE, FALSE))
})
