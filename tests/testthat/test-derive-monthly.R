# tests/testthat/test-derive-monthly.R
mcyc <- function(lo, first, last) {
  data.frame(form_name = "I-129", office_code = "SCD", form_subtype = "137-E",
             range_lower = lo, range_lower_unit = "Months",
             range_upper = lo + 3, range_upper_unit = "Months",
             first_seen = as.Date(first), last_seen = as.Date(last),
             first_publication_date = as.Date(first),
             last_publication_date = as.Date(first),
             n_observations = 5L, n_publication_cycles = 1L,
             intra_cycle_edit = FALSE, stringsAsFactors = FALSE)
}

test_that("each cycle yields one monthly row labelled by its first snapshot month", {
  m <- project_monthly(mcyc(4, "2026-01-14", "2026-02-17"))
  expect_equal(nrow(m), 1L)
  expect_equal(m$month, "2026-01")
  expect_equal(m$range_lower, 4)
  expect_equal(m$range_upper, 7)
})

test_that("two publication cycles in one calendar month emit two rows", {
  cycles <- rbind(mcyc(4, "2026-01-05", "2026-01-14"),
                  mcyc(5, "2026-01-15", "2026-01-31"))
  m <- project_monthly(cycles)
  expect_equal(nrow(m), 2L)
  expect_equal(m$month, c("2026-01", "2026-01"))
})

test_that("balanced_keys returns only keys present at both endpoints and counts exclusions", {
  s <- rbind(
    data.frame(snapshot_date = as.Date("2026-01-01"), form_name = "I-129",
               office_code = "SCD", form_subtype = c("A", "B", "D"), stringsAsFactors = FALSE),
    data.frame(snapshot_date = as.Date("2026-06-01"), form_name = "I-129",
               office_code = "SCD", form_subtype = c("B", "C"), stringsAsFactors = FALSE))
  bk <- balanced_keys(s, as.Date("2026-01-01"), as.Date("2026-06-01"))
  expect_equal(length(bk$keys), 1L)
  expect_equal(bk$excluded_a, 2L)   # A and D dropped out
  expect_equal(bk$excluded_b, 1L)   # C appeared
})
