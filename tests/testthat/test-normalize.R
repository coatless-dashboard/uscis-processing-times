test_that("parse_uscis_date handles the published format and rejects junk", {
  expect_equal(parse_uscis_date("August 17, 2026"), as.Date("2026-08-17"))
  expect_equal(parse_uscis_date("March 09, 1995"), as.Date("1995-03-09"))
  expect_true(is.na(parse_uscis_date("")))
  expect_true(is.na(parse_uscis_date("See notes")))
})

test_that("normalize_bound converts singular and plural units", {
  out <- normalize_bound(c(4, 4, 15, 15, 2, 2),
                         c("Months", "Month", "Days", "Day", "Weeks", "Week"))
  expect_equal(out$months[1], 4)
  expect_equal(out$months[2], 4)
  expect_equal(out$months[3], 15 / 30.4375)
  expect_equal(out$months[4], 15 / 30.4375)
  expect_equal(out$months[5], 2 * 7 / 30.4375)
  expect_equal(out$months[6], 2 * 7 / 30.4375)
  expect_true(all(out$reason == "ok"))
})

test_that("unparseable units become NA with a reason, never zero", {
  out <- normalize_bound(c(1, 1, 1), c("See notes", "learn more.", "Fortnights"))
  expect_true(all(is.na(out$months)))
  expect_equal(out$reason, c("see_notes", "learn_more", "unknown_unit"))
})

test_that("mixed pairs keep the parseable bound", {
  # Observed live: range_lower = 15 Days, range_upper = 4 Months
  lo <- normalize_bound(15, "Days")
  up <- normalize_bound(4, "Months")
  expect_false(is.na(lo$months))
  expect_false(is.na(up$months))
  expect_true(range_is_valid(lo$months, up$months))
})

test_that("an empty value with a valid unit is a missing value, not a silent zero", {
  # Observed live: range_lower stored as '' alongside a genuine unit ("Months").
  # This must never be reported as "ok" -- that reads as a successful parse of
  # a genuine 0 -- nor may months resolve to 0.
  out <- normalize_bound("", "Months")
  expect_true(is.na(out$months))
  expect_equal(out$reason, "missing_value")
})

test_that("range validity is checked after normalization and is NA-tolerant", {
  expect_true(range_is_valid(0.49, 4))
  expect_false(range_is_valid(5, 4))
  expect_true(range_is_valid(NA_real_, 4))
})
