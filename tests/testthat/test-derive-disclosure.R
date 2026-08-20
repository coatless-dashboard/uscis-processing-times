key_rows <- function(dates, office = "SCD", subtype = "137-E") {
  # office_code_stitched mirrors what extract_snapshot actually produces (§6.2
  # stitching), so a fixture using only one raw office per test cannot tell a
  # correct raw-keyed derivation from a wrong stitched-keyed one -- both would
  # look identical on a single key. See the two-raw-office test below.
  stitched <- if (office %in% names(STITCH_MAP)) unname(STITCH_MAP[[office]]) else office
  data.frame(snapshot_date = as.Date(dates), form_name = "I-129",
             office_code = office, office_code_stitched = stitched,
             form_subtype = subtype, stringsAsFactors = FALSE)
}

test_that("a single absent snapshot is not enough to declare a withdrawal", {
  present <- as.Date("2026-01-01") + 0:4
  s <- key_rows(present[-3])   # absent on day 3 only, then returns
  out <- build_disclosure_events(s, present)
  expect_equal(sum(out$event_type == "disappeared"), 0L)
})

test_that("two consecutive absent snapshots emit a provisional disappearance", {
  present <- as.Date("2026-01-01") + 0:4
  s <- key_rows(present[1:2])  # absent days 3,4,5
  out <- build_disclosure_events(s, present)
  d <- out[out$event_type == "disappeared", ]
  expect_equal(nrow(d), 1L)
  expect_equal(d$event_date, as.Date("2026-01-03"))
  expect_equal(d$status, "provisional")
})

test_that("an absence run of exactly DISCLOSURE_MIN_ABSENT triggers a disappearance", {
  run_len <- DISCLOSURE_MIN_ABSENT
  # Present on the first 2 dates only; the sequence ends after exactly run_len
  # absences, so the run cannot be inflated beyond the boundary being tested.
  present <- as.Date("2026-01-01") + 0:(run_len + 1L)
  s <- key_rows(present[1:2])
  out <- build_disclosure_events(s, present)
  d <- out[out$event_type == "disappeared", ]
  expect_equal(nrow(d), 1L)
  expect_equal(d$event_date, present[3])
  expect_equal(d$status, "provisional")
})

test_that("absence beyond the confirmation threshold is confirmed", {
  present <- as.Date("2026-01-01") + 0:40
  s <- key_rows(present[1:2])
  out <- build_disclosure_events(s, present)
  d <- out[out$event_type == "disappeared", ]
  expect_equal(d$status, "confirmed")
})

test_that("an absence run of exactly DISCLOSURE_CONFIRM_ABSENT is confirmed", {
  run_len <- DISCLOSURE_CONFIRM_ABSENT
  # Present on day 1 only; the sequence ends after exactly run_len absences.
  present <- as.Date("2026-01-01") + 0:run_len
  s <- key_rows(present[1])
  out <- build_disclosure_events(s, present)
  d <- out[out$event_type == "disappeared", ]
  expect_equal(nrow(d), 1L)
  expect_equal(d$status, "confirmed")
})

test_that("an absence run one short of DISCLOSURE_CONFIRM_ABSENT remains provisional", {
  run_len <- DISCLOSURE_CONFIRM_ABSENT - 1L
  present <- as.Date("2026-01-01") + 0:run_len
  s <- key_rows(present[1])
  out <- build_disclosure_events(s, present)
  d <- out[out$event_type == "disappeared", ]
  expect_equal(nrow(d), 1L)
  expect_equal(d$status, "provisional")
})

test_that("a re-appearance emits a retraction rather than rewriting history", {
  present <- as.Date("2026-01-01") + 0:9
  s <- key_rows(present[c(1, 2, 8, 9, 10)])   # absent days 3-7, then returns
  out <- build_disclosure_events(s, present)
  expect_equal(sum(out$event_type == "disappeared"), 1L)
  expect_equal(sum(out$event_type == "retracted"), 1L)
  expect_equal(out$event_date[out$event_type == "retracted"], as.Date("2026-01-08"))
})

test_that("date_uncertainty_days reflects unobserved days before the event", {
  # No snapshot on 2026-01-02 or 01-03, so the withdrawal date is uncertain.
  present <- as.Date(c("2026-01-01", "2026-01-04", "2026-01-05", "2026-01-06"))
  s <- key_rows(present[1])
  out <- build_disclosure_events(s, present)
  d <- out[out$event_type == "disappeared", ]
  expect_equal(d$event_date, as.Date("2026-01-04"))
  expect_equal(d$date_uncertainty_days, 2)
})

test_that("disclosure keys on raw office_code so the retired centers are visible", {
  present <- as.Date("2026-01-01") + 0:4
  s <- key_rows(present[1:2], office = "ESC")
  out <- build_disclosure_events(s, present)
  expect_equal(out$office_code[out$event_type == "disappeared"], "ESC")
})

test_that("disclosure keys on raw office_code even when two raw offices stitch to the same code", {
  # ESC and CSC both stitch to SCD (office_code_stitched = "SCD" for both),
  # but disclosure must never key on the stitched column -- that would erase
  # the 2025-10-22 service-center-retirement finding entirely by collapsing
  # every named center's withdrawal into a single SCD row.
  present <- as.Date("2026-01-01") + 0:4
  s <- rbind(key_rows(present[1:2], office = "ESC"),
            key_rows(present[1:2], office = "CSC"))
  out <- build_disclosure_events(s, present)
  d <- out[out$event_type == "disappeared", ]
  expect_equal(nrow(d), 2L)
  expect_equal(sort(d$office_code), c("CSC", "ESC"))
})

test_that("a re-keyed subtype resolves to its successor instead of a bare drop", {
  present <- as.Date("2026-01-01") + 0:4
  s <- rbind(key_rows(present[1:2], subtype = "I-526"),
             key_rows(present[3:5], subtype = "I-526E"))
  rekey <- data.frame(form_name = "I-129", office_code = "SCD",
                      form_subtype = "I-526", successor_subtype = "I-526E",
                      stringsAsFactors = FALSE)
  out <- build_disclosure_events(s, present, rekey = rekey)
  d <- out[out$event_type == "disappeared" & out$form_subtype == "I-526", ]
  expect_equal(d$successor_key, "I-526E")
})
