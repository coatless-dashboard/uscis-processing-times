test_that("run_backfill ingests every valid snapshot and quarantines the rest", {
  work <- withr::local_tempdir()
  out  <- withr::local_tempdir()

  good1 <- file.path(work, "2026-01-01.db"); make_fixture_db(good1)
  good2 <- file.path(work, "2026-01-02.db"); make_fixture_db(good2)
  bad   <- file.path(work, "2026-01-03.db"); writeLines("junk", bad)

  assets <- data.frame(
    tag = c("2026-01-01", "2026-01-02", "2026-01-03"),
    asset_name = c("2026-01-01.db", "2026-01-02.db", "2026-01-03.db"),
    size = c(file.size(good1), file.size(good2), file.size(bad)),
    stringsAsFactors = FALSE)

  res <- run_backfill(work_dir = work, assets = assets, out_dir = out,
                      fetch_fn = function(tag, asset_name, dest) {
                        file.copy(file.path(work, asset_name), dest, overwrite = TRUE)
                        dest
                      })

  expect_equal(nrow(res$tables$pt_snapshots), 2L)
  expect_equal(res$manifest$snapshots[["2026-01-03"]]$status, "quarantined")
  expect_false(is.null(res$manifest$freeze))

  # All four published artifacts must land on disk.
  expect_true(file.exists(file.path(out, "manifest.json")))
  expect_true(file.exists(file.path(out, "processing-times.db")))
  expect_true(file.exists(file.path(out, "processing-times-monthly.parquet")))
  expect_true(file.exists(file.path(out, "panel-monthly.arrow")))
})

test_that("the rekey crosswalk resolves a withdrawn key to its successor when wired", {
  work <- withr::local_tempdir(); out <- withr::local_tempdir()

  # Day 1: the key is present. Days 2-3: absent, using an unrelated filler
  # row so the snapshot itself is non-empty. Two consecutive present-but-
  # absent snapshots (DISCLOSURE_MIN_ABSENT = 2) triggers a "disappeared"
  # event for I-102/NBC/142 dated day 2.
  filler <- data.frame(form_name = "I-999", office_code = "XXX", form_subtype = "000",
    publication_date = "August 17, 2026", range_upper = 1, range_upper_unit = "Months",
    range_lower = 1, range_lower_unit = "Months", service_request_date = "September 07, 2024",
    subtype_info_en = "", subtype_info_es = "", subtype_note_en = "", subtype_note_es = "",
    form_note_en = "", form_note_es = "", stringsAsFactors = FALSE)
  d1 <- file.path(work, "2026-01-01.db"); make_fixture_db(d1)
  d2 <- file.path(work, "2026-01-02.db"); make_fixture_db(d2, rows = filler)
  d3 <- file.path(work, "2026-01-03.db"); make_fixture_db(d3, rows = filler)
  assets <- data.frame(tag = c("2026-01-01", "2026-01-02", "2026-01-03"),
                       asset_name = c("2026-01-01.db", "2026-01-02.db", "2026-01-03.db"),
                       size = c(file.size(d1), file.size(d2), file.size(d3)),
                       stringsAsFactors = FALSE)
  fetch_fn <- function(tag, asset_name, dest) {
    file.copy(file.path(work, asset_name), dest, overwrite = TRUE); dest
  }

  # Without a crosswalk (default path points at the committed, header-only
  # file when run from the repo root; here we point at a path that does not
  # exist at all, which must resolve identically to "no crosswalk") the
  # withdrawal is a bare drop -- no successor recorded.
  res_no_rekey <- run_backfill(work, assets, out, fetch_fn = fetch_fn,
                               rekey_path = file.path(work, "no-such-file.csv"))
  d_no <- res_no_rekey$tables$disclosure_events
  row_no <- d_no[d_no$form_name == "I-102" & d_no$office_code == "NBC" &
                 d_no$form_subtype == "142" & d_no$event_type == "disappeared", ]
  expect_equal(nrow(row_no), 1L)
  expect_true(is.na(row_no$successor_key))

  # With a crosswalk row present at rekey_path, the same withdrawal resolves
  # to its successor key.
  out2 <- withr::local_tempdir()
  rekey_csv <- file.path(work, "rekey.csv")
  writeLines(c("form_name,office_code,form_subtype,successor_subtype",
              "I-102,NBC,142,142-SUCC"), rekey_csv)
  res_rekey <- run_backfill(work, assets, out2, fetch_fn = fetch_fn, rekey_path = rekey_csv)
  d_yes <- res_rekey$tables$disclosure_events
  row_yes <- d_yes[d_yes$form_name == "I-102" & d_yes$office_code == "NBC" &
                   d_yes$form_subtype == "142" & d_yes$event_type == "disappeared", ]
  expect_equal(nrow(row_yes), 1L)
  expect_equal(row_yes$successor_key, "142-SUCC")
})

test_that("a quarantined snapshot is recorded as absent, never as an empty snapshot", {
  work <- withr::local_tempdir(); out <- withr::local_tempdir()
  good <- file.path(work, "2026-01-01.db"); make_fixture_db(good)
  bad  <- file.path(work, "2026-01-02.db"); writeLines("junk", bad)
  assets <- data.frame(tag = c("2026-01-01", "2026-01-02"),
                       asset_name = c("2026-01-01.db", "2026-01-02.db"),
                       size = c(file.size(good), file.size(bad)),
                       stringsAsFactors = FALSE)
  res <- run_backfill(work, assets, out, fetch_fn = function(tag, asset_name, dest) {
    file.copy(file.path(work, asset_name), dest, overwrite = TRUE); dest })
  cov <- res$tables$coverage
  expect_true(cov$absent[cov$date == as.Date("2026-01-02")])
})
