test_that("detect_era classifies the three upstream eras by table presence", {
  expect_equal(detect_era(c("forms", "offices", "processing_time", "selftest")), "v0.1")
  expect_equal(detect_era(c("forms", "offices", "processing_time", "form_types")), "v0.2")
  expect_equal(detect_era(c("forms", "offices", "processing_time", "form_types",
                            "foia_processing_time")), "v0.5")
})

test_that("an unrecognized table set is rejected, not defaulted", {
  expect_equal(detect_era(c("forms", "offices")), "unknown")
  expect_equal(detect_era(character(0)), "unknown")
})

test_that("extract_snapshot returns one tidy row per key with normalized bounds", {
  path <- withr::local_tempfile(fileext = ".db")
  make_fixture_db(path, era = "v0.5")
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  withr::defer(DBI::dbDisconnect(con))

  out <- extract_snapshot(con, as.Date("2026-08-18"))

  expect_equal(nrow(out), 1L)
  expect_equal(out$snapshot_date, as.Date("2026-08-18"))
  expect_equal(out$publication_date, as.Date("2026-08-17"))
  expect_equal(out$service_request_date, as.Date("2024-09-07"))
  expect_equal(out$range_lower_months, 18.5)
  expect_equal(out$range_upper_months, 23.5)
  expect_equal(out$range_lower_reason, "ok")
  expect_true(out$range_valid)
  # raw values are retained alongside the normalized ones
  expect_equal(out$range_lower, 18.5)
  expect_equal(out$range_lower_unit, "Months")
})

test_that("a genuinely mixed-storage-class range_lower column never coerces '' to zero", {
  # The real archive stores range_lower/range_upper with MIXED SQLite storage
  # classes: a normal numeric row followed by an empty-string '' row where
  # USCIS published no lower bound, while the unit is still a genuine
  # "Months". dbWriteTable()/make_fixture_db() cannot reproduce this -- R
  # vectors are typed, so every row would come out the same storage class.
  # Only raw CREATE TABLE + INSERT can put a real column and a real empty
  # string in the same SQLite column, which is what actually triggers
  # RSQLite's "mixed type ... coercing other values of type string" warning
  # and its 0.0 coercion.
  path <- withr::local_tempfile(fileext = ".db")
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  withr::defer(DBI::dbDisconnect(con))

  DBI::dbExecute(con, "
    CREATE TABLE processing_time (
      form_name TEXT, office_code TEXT, form_subtype TEXT, publication_date TEXT,
      range_lower REAL, range_lower_unit TEXT, range_upper REAL, range_upper_unit TEXT,
      service_request_date TEXT, subtype_info_en TEXT, subtype_info_es TEXT,
      subtype_note_en TEXT, subtype_note_es TEXT, form_note_en TEXT, form_note_es TEXT
    )")
  # First row: a normal numeric bound. This is what makes RSQLite sniff
  # range_lower as a REAL-typed result column.
  DBI::dbExecute(con, "
    INSERT INTO processing_time VALUES (
      'I-102', 'NBC', '142', 'August 17, 2026', 18.5, 'Months', 23.5, 'Months',
      'September 07, 2024', 'Initial issuance', 'Emision inicial', '', '', '', '')")
  # Second row: the real defect. No lower bound was published, so the column
  # holds '' with storage class TEXT, alongside a perfectly valid unit and a
  # genuine upper bound of 39 months -- a multi-year wait, not an instant one.
  DBI::dbExecute(con, "
    INSERT INTO processing_time VALUES (
      'I-130', 'SCD', '130', 'August 17, 2026', '', 'Months', 39.0, 'Months',
      'September 07, 2024', 'Immigrant petition', 'Peticion de inmigrante', '', '', '', '')")

  out <- expect_silent(extract_snapshot(con, as.Date("2026-08-18")))
  expect_equal(nrow(out), 2L)

  row <- out[out$form_name == "I-130", ]
  expect_true(is.na(row$range_lower))               # never 0
  expect_true(is.na(row$range_lower_months))         # never 0
  expect_equal(row$range_lower_reason, "missing_value")
  expect_equal(row$range_upper_months, 39)           # the genuine bound survives

  # The unaffected row is untouched by the fix.
  nbc <- out[out$form_name == "I-102", ]
  expect_equal(nbc$range_lower_months, 18.5)
})

test_that("retired service centers get a stitched code but keep the raw one", {
  path <- withr::local_tempfile(fileext = ".db")
  rows <- data.frame(form_name = "I-129", office_code = "ESC", form_subtype = "137-E",
    publication_date = "August 17, 2026", range_upper = 22, range_upper_unit = "Months",
    range_lower = 19.5, range_lower_unit = "Months",
    service_request_date = "October 20, 2024", subtype_info_en = "E", subtype_info_es = "E",
    subtype_note_en = "", subtype_note_es = "", form_note_en = "", form_note_es = "",
    stringsAsFactors = FALSE)
  make_fixture_db(path, era = "v0.5", rows = rows)
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  withr::defer(DBI::dbDisconnect(con))
  out <- extract_snapshot(con, as.Date("2025-06-01"))
  expect_equal(out$office_code, "ESC")
  expect_equal(out$office_code_stitched, "SCD")
})
