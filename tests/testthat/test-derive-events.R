cyc <- function(lo, up, first, last, intra = FALSE, lo_u = "Months", subtype = "137-E") {
  data.frame(form_name = "I-129", office_code = "SCD", form_subtype = subtype,
             range_lower = lo, range_lower_unit = lo_u,
             range_upper = up, range_upper_unit = "Months",
             first_seen = as.Date(first), last_seen = as.Date(last),
             first_publication_date = as.Date(first),
             last_publication_date = as.Date(last),
             n_observations = 1L, n_publication_cycles = 1L,
             intra_cycle_edit = intra, stringsAsFactors = FALSE)
}

test_that("the first run of a key produces no event", {
  expect_equal(nrow(build_events(cyc(4, 7, "2026-01-01", "2026-01-05"))), 0L)
})

test_that("consecutive runs yield one event dated at the new run's first_seen", {
  cycles <- rbind(cyc(4, 7, "2026-01-01", "2026-01-05"),
                  cyc(5, 8, "2026-01-06", "2026-01-10"))
  ev <- build_events(cycles)
  expect_equal(nrow(ev), 1L)
  expect_equal(ev$event_date, as.Date("2026-01-06"))
  expect_equal(ev$delta_lower_months, 1)
  expect_equal(ev$delta_upper_months, 1)
  expect_equal(ev$direction, "worsened")
})

test_that("direction is driven by the upper bound and never by an average", {
  cycles <- rbind(cyc(4, 7, "2026-01-01", "2026-01-05"),
                  cyc(6, 6, "2026-01-06", "2026-01-10"))
  ev <- build_events(cycles)
  expect_equal(ev$direction, "improved")   # upper 7 -> 6 despite lower rising
})

test_that("an event never crosses a key boundary", {
  # A different key's lone (first-run) cycle sits between two runs of the
  # target key once everything is sorted by (key, first_seen). A guard that
  # forgets to check the key match would splice a bogus event across that
  # boundary and report two events instead of one.
  cycles <- rbind(
    cyc(4, 7, "2026-01-01", "2026-01-05", subtype = "999-X"),  # other key, first run only
    cyc(4, 7, "2026-01-01", "2026-01-05"),                      # target key, run 1
    cyc(5, 8, "2026-01-06", "2026-01-10"))                      # target key, run 2
  ev <- build_events(cycles)
  expect_equal(nrow(ev), 1L)
  expect_equal(ev$form_subtype, "137-E")
  expect_equal(ev$direction, "worsened")
})

test_that("an event into an unparseable bound is classed, not dropped", {
  cycles <- rbind(cyc(4, 7, "2026-01-01", "2026-01-05"),
                  cyc(NA, 7, "2026-01-06", "2026-01-10", lo_u = "See notes"))
  ev <- build_events(cycles)
  expect_equal(nrow(ev), 1L)
  expect_true(is.na(ev$delta_lower_months))
  expect_equal(ev$direction, "unchanged")  # upper bound did not move
})
