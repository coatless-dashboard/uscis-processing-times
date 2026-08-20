test_that("build_coverage marks every calendar day in span, absent or present", {
  dates <- as.Date(c("2026-01-01", "2026-01-02", "2026-01-04"))
  cov <- build_coverage(dates, row_counts = c(500, 500, 500),
                        span_start = as.Date("2026-01-01"),
                        span_end   = as.Date("2026-01-04"))
  expect_equal(nrow(cov), 4L)
  expect_equal(cov$absent, c(FALSE, FALSE, TRUE, FALSE))
  expect_true(is.na(cov$n_rows[3]))
})

test_that("absent and anomalous are independent flags", {
  dates <- as.Date("2026-01-01") + 0:39
  counts <- c(rep(500, 39), 100)   # last day collapses
  cov <- build_coverage(dates, counts, span_start = min(dates), span_end = max(dates))
  last <- cov[cov$date == max(dates), ]
  expect_false(last$absent)
  expect_true(last$anomalous)
  expect_false(any(cov$anomalous[cov$date < max(dates)]))
})

test_that("present_index numbers only present snapshots, so gaps never shift it", {
  dates <- as.Date(c("2026-01-01", "2026-01-02", "2026-01-04"))
  cov <- build_coverage(dates, c(500, 500, 500),
                        span_start = as.Date("2026-01-01"),
                        span_end   = as.Date("2026-01-04"))
  expect_equal(cov$present_index, c(1L, 2L, NA_integer_, 3L))
})
