inq <- function(date, srd, office = "NBC") {
  data.frame(snapshot_date = as.Date(date), form_name = "I-102",
             office_code = office, form_subtype = "142",
             service_request_date = as.Date(srd), stringsAsFactors = FALSE)
}

test_that("the normal daily roll is +1 and classes as normal", {
  s <- rbind(inq("2026-01-01", "2024-09-10"), inq("2026-01-02", "2024-09-11"))
  out <- build_inquiry_events(s)
  expect_equal(out$anomaly_class, c("first_observation", "normal"))
  expect_equal(out$expected_delta_days[2], 1)
  expect_equal(out$delta_excess_days[2], 0)
})

test_that("expected delta follows observed snapshot spacing, so gaps are not leaps", {
  # 17-day outage: the inquiry date legitimately advances 17 days.
  s <- rbind(inq("2026-01-01", "2024-09-10"), inq("2026-01-18", "2024-09-27"))
  out <- build_inquiry_events(s)
  expect_equal(out$expected_delta_days[2], 17)
  expect_equal(out$delta_excess_days[2], 0)
  expect_equal(out$anomaly_class[2], "post_gap")
})

test_that("a backward move is flagged even when it follows a gap", {
  s <- rbind(inq("2026-01-01", "2024-09-10"), inq("2026-01-18", "2024-08-01"))
  out <- build_inquiry_events(s)
  expect_equal(out$anomaly_class[2], "backward")
})

test_that("a stalled date is distinct from a normal roll", {
  s <- rbind(inq("2026-01-01", "2024-09-10"), inq("2026-01-02", "2024-09-10"))
  out <- build_inquiry_events(s)
  expect_equal(out$anomaly_class[2], "stalled")
})

test_that("a leap beyond tolerance on a contiguous day is flagged", {
  s <- rbind(inq("2026-01-01", "2024-09-10"), inq("2026-01-02", "2024-12-01"))
  out <- build_inquiry_events(s)
  expect_equal(out$anomaly_class[2], "leap")
})

test_that("a new key resets prior snapshot/service-request state rather than inheriting it", {
  # Sorted by (key, snapshot_date), "NBC" precedes "SCD". Without resetting
  # prev_snap/prev_srd at the key boundary, SCD's first row would compute its
  # delta against NBC's last row -- a genuinely different key -- rather than
  # being classed as a first observation.
  s <- rbind(inq("2026-01-01", "2024-09-10", office = "NBC"),
            inq("2026-01-02", "2024-09-11", office = "NBC"),
            inq("2026-01-01", "2024-09-10", office = "SCD"))
  out <- build_inquiry_events(s)
  nbc_first <- out[out$office_code == "NBC" & out$snapshot_date == as.Date("2026-01-01"), ]
  scd_first <- out[out$office_code == "SCD" & out$snapshot_date == as.Date("2026-01-01"), ]
  expect_equal(nbc_first$anomaly_class, "first_observation")
  expect_equal(scd_first$anomaly_class, "first_observation")
})

test_that("anomaly classes are exhaustive and mutually exclusive", {
  s <- rbind(inq("2026-01-01", "2024-09-10"), inq("2026-01-02", "2024-09-11"),
             inq("2026-01-03", "2024-09-11"), inq("2026-01-04", "2024-08-01"),
             inq("2026-01-20", "2024-08-17"), inq("2026-01-21", "2025-01-01"))
  out <- build_inquiry_events(s)
  valid <- c("first_observation", "normal", "backward", "stalled", "leap", "post_gap")
  expect_true(all(out$anomaly_class %in% valid))
  expect_false(any(is.na(out$anomaly_class)))
})
