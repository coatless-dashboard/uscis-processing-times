test_that("a run with no new releases is a no-op that still reports success", {
  out <- withr::local_tempdir()
  prev <- list(snapshots = list("2026-01-01" = list(tag = "2026-01-01",
                                size_fp = asset_fingerprint(10),
                                era = "v0.5", n_rows = 1L, status = "ok")))
  jsonlite::write_json(prev, file.path(out, "manifest.json"), auto_unbox = TRUE)
  assets <- data.frame(tag = "2026-01-01", asset_name = "2026-01-01.db",
                       size = 10, stringsAsFactors = FALSE)
  res <- run_update(out, assets, out, fetch_fn = function(...) stop("should not fetch"))
  expect_equal(res$status, "no_change")
  expect_true(is.na(res$rebuilt_from))
})

test_that("a new release triggers an update reporting the rebuild anchor", {
  out <- withr::local_tempdir(); work <- withr::local_tempdir()
  good <- file.path(work, "2026-01-01.db"); make_fixture_db(good)
  assets <- data.frame(tag = "2026-01-01", asset_name = "2026-01-01.db",
                       size = file.size(good), stringsAsFactors = FALSE)
  res <- run_update(out, assets, out, fetch_fn = function(tag, asset_name, dest) {
    file.copy(good, dest, overwrite = TRUE); dest })
  expect_equal(res$status, "updated")
  expect_equal(res$rebuilt_from, as.Date("2026-01-01"))
})

test_that("a quarantined snapshot warns without failing the run", {
  out <- withr::local_tempdir(); work <- withr::local_tempdir()
  bad <- file.path(work, "2026-01-01.db"); writeLines("junk", bad)
  good <- file.path(work, "2026-01-02.db"); make_fixture_db(good)
  assets <- data.frame(tag = c("2026-01-01", "2026-01-02"),
                       asset_name = c("2026-01-01.db", "2026-01-02.db"),
                       size = c(file.size(bad), file.size(good)),
                       stringsAsFactors = FALSE)
  res <- run_update(out, assets, out, fetch_fn = function(tag, asset_name, dest) {
    file.copy(file.path(work, asset_name), dest, overwrite = TRUE); dest })
  expect_equal(res$status, "updated")
  expect_true(any(grepl("quarantined 2026-01-01", res$warnings)))
})

test_that("a re-cut upstream asset rebuilds from that date rather than the newest", {
  out <- withr::local_tempdir(); work <- withr::local_tempdir()
  d1 <- file.path(work, "2026-01-01.db"); make_fixture_db(d1)
  d2 <- file.path(work, "2026-01-02.db"); make_fixture_db(d2)
  assets <- data.frame(tag = c("2026-01-01", "2026-01-02"),
                       asset_name = c("2026-01-01.db", "2026-01-02.db"),
                       size = c(file.size(d1), file.size(d2)), stringsAsFactors = FALSE)
  # Seed a real prior panel + manifest covering both tags, matching what an
  # actual completed prior run leaves in state_dir.
  seed <- run_update(out, assets, out, fetch_fn = function(tag, asset_name, dest) {
    file.copy(file.path(work, asset_name), dest, overwrite = TRUE); dest })
  expect_equal(seed$status, "updated")

  # Corrupt only the recorded size for 2026-01-02 in the manifest, simulating
  # an upstream re-cut discovered on the next run. The panel file itself is
  # untouched, exactly as it would be for a real re-cut.
  manifest <- jsonlite::read_json(file.path(out, "manifest.json"), simplifyVector = FALSE)
  manifest$snapshots[["2026-01-02"]]$size_fp <- asset_fingerprint(999999)
  jsonlite::write_json(manifest, file.path(out, "manifest.json"),
                       auto_unbox = TRUE, pretty = TRUE)

  res <- run_update(out, assets, out, fetch_fn = function(tag, asset_name, dest) {
    file.copy(file.path(work, asset_name), dest, overwrite = TRUE); dest })
  expect_equal(res$rebuilt_from, as.Date("2026-01-02"))
})

test_that("successive runs accumulate history in the published panel and manifest", {
  out <- withr::local_tempdir(); work <- withr::local_tempdir()
  d1 <- file.path(work, "2026-01-01.db"); make_fixture_db(d1)
  assets1 <- data.frame(tag = "2026-01-01", asset_name = "2026-01-01.db",
                        size = file.size(d1), stringsAsFactors = FALSE)
  res1 <- run_update(out, assets1, out, fetch_fn = function(tag, asset_name, dest) {
    file.copy(file.path(work, asset_name), dest, overwrite = TRUE); dest })
  expect_equal(res1$status, "updated")

  d2 <- file.path(work, "2026-01-02.db"); make_fixture_db(d2)
  assets2 <- data.frame(tag = c("2026-01-01", "2026-01-02"),
                        asset_name = c("2026-01-01.db", "2026-01-02.db"),
                        size = c(file.size(d1), file.size(d2)), stringsAsFactors = FALSE)
  res2 <- run_update(out, assets2, out, fetch_fn = function(tag, asset_name, dest) {
    file.copy(file.path(work, asset_name), dest, overwrite = TRUE); dest })
  expect_equal(res2$status, "updated")
  expect_equal(res2$rebuilt_from, as.Date("2026-01-02"))

  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, "processing-times.db"))
  panel <- DBI::dbReadTable(con, "pt_snapshots")
  DBI::dbDisconnect(con)
  dates <- as.Date(panel$snapshot_date, origin = "1970-01-01")
  expect_setequal(as.character(dates), c("2026-01-01", "2026-01-02"))

  manifest <- jsonlite::read_json(file.path(out, "manifest.json"), simplifyVector = FALSE)
  expect_setequal(names(manifest$snapshots), c("2026-01-01", "2026-01-02"))

  con2 <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, "processing-times.db"))
  coverage <- DBI::dbReadTable(con2, "coverage")
  DBI::dbDisconnect(con2)
  cov_dates <- as.Date(coverage$date, origin = "1970-01-01")
  expect_setequal(as.character(cov_dates), c("2026-01-01", "2026-01-02"))
})

test_that("the rebuild anchor is the earliest changed tag, not the newest changed or newest overall", {
  out <- withr::local_tempdir(); work <- withr::local_tempdir()
  d1 <- file.path(work, "2026-01-01.db"); make_fixture_db(d1)
  d2 <- file.path(work, "2026-01-02.db"); make_fixture_db(d2)
  d3 <- file.path(work, "2026-01-03.db"); make_fixture_db(d3)
  assets <- data.frame(tag = c("2026-01-01", "2026-01-02", "2026-01-03"),
                       asset_name = c("2026-01-01.db", "2026-01-02.db", "2026-01-03.db"),
                       size = c(file.size(d1), file.size(d2), file.size(d3)),
                       stringsAsFactors = FALSE)
  # Seed a real prior panel + manifest covering all three tags.
  seed <- run_update(out, assets, out, fetch_fn = function(tag, asset_name, dest) {
    file.copy(file.path(work, asset_name), dest, overwrite = TRUE); dest })
  expect_equal(seed$status, "updated")

  # Corrupt the recorded sizes for the two earliest tags only, simulating an
  # upstream re-cut of both discovered on the next run.
  manifest <- jsonlite::read_json(file.path(out, "manifest.json"), simplifyVector = FALSE)
  manifest$snapshots[["2026-01-01"]]$size_fp <- asset_fingerprint(999999)
  manifest$snapshots[["2026-01-02"]]$size_fp <- asset_fingerprint(888888)
  jsonlite::write_json(manifest, file.path(out, "manifest.json"),
                       auto_unbox = TRUE, pretty = TRUE)

  res <- run_update(out, assets, out, fetch_fn = function(tag, asset_name, dest) {
    file.copy(file.path(work, asset_name), dest, overwrite = TRUE); dest })
  expect_equal(res$rebuilt_from, as.Date("2026-01-01"))
})

test_that("history carries forward when state_dir and out_dir are separate directories", {
  # Mirrors the actual nightly topology: prior artifacts are staged into a
  # dedicated state directory (read-only for this run), and output is
  # published to a distinct directory each run -- never the same path.
  state1 <- withr::local_tempdir(); out1 <- withr::local_tempdir()
  work <- withr::local_tempdir()
  d1 <- file.path(work, "2026-01-01.db"); make_fixture_db(d1)
  assets1 <- data.frame(tag = "2026-01-01", asset_name = "2026-01-01.db",
                        size = file.size(d1), stringsAsFactors = FALSE)
  res1 <- run_update(state1, assets1, out1, fetch_fn = function(tag, asset_name, dest) {
    file.copy(file.path(work, asset_name), dest, overwrite = TRUE); dest })
  expect_equal(res1$status, "updated")

  # Stage the published artifacts into a fresh state directory, as CI does
  # between runs, then publish the second run to yet another output directory.
  state2 <- withr::local_tempdir(); out2 <- withr::local_tempdir()
  file.copy(file.path(out1, "processing-times.db"), file.path(state2, "processing-times.db"))
  file.copy(file.path(out1, "manifest.json"), file.path(state2, "manifest.json"))

  d2 <- file.path(work, "2026-01-02.db"); make_fixture_db(d2)
  assets2 <- data.frame(tag = c("2026-01-01", "2026-01-02"),
                        asset_name = c("2026-01-01.db", "2026-01-02.db"),
                        size = c(file.size(d1), file.size(d2)), stringsAsFactors = FALSE)
  res2 <- run_update(state2, assets2, out2, fetch_fn = function(tag, asset_name, dest) {
    file.copy(file.path(work, asset_name), dest, overwrite = TRUE); dest })
  expect_equal(res2$status, "updated")
  expect_equal(res2$rebuilt_from, as.Date("2026-01-02"))

  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out2, "processing-times.db"))
  panel <- DBI::dbReadTable(con, "pt_snapshots")
  DBI::dbDisconnect(con)
  dates <- as.Date(panel$snapshot_date, origin = "1970-01-01")
  expect_setequal(as.character(dates), c("2026-01-01", "2026-01-02"))

  manifest <- jsonlite::read_json(file.path(out2, "manifest.json"), simplifyVector = FALSE)
  expect_setequal(names(manifest$snapshots), c("2026-01-01", "2026-01-02"))
})

test_that("force_full_rebuild anchors at the earliest known tag, not the diff", {
  out <- withr::local_tempdir(); work <- withr::local_tempdir()
  d1 <- file.path(work, "2026-01-01.db"); make_fixture_db(d1)
  d2 <- file.path(work, "2026-01-02.db"); make_fixture_db(d2)
  d3 <- file.path(work, "2026-01-03.db"); make_fixture_db(d3)
  # Seed a real prior panel + manifest covering only 2026-01-01 and
  # 2026-01-02, so those two are already recorded with matching sizes and an
  # ordinary diff would anchor at 2026-01-03, never touching the earlier two.
  assets12 <- data.frame(tag = c("2026-01-01", "2026-01-02"),
                         asset_name = c("2026-01-01.db", "2026-01-02.db"),
                         size = c(file.size(d1), file.size(d2)), stringsAsFactors = FALSE)
  seed <- run_update(out, assets12, out, fetch_fn = function(tag, asset_name, dest) {
    file.copy(file.path(work, asset_name), dest, overwrite = TRUE); dest })
  expect_equal(seed$status, "updated")

  assets <- data.frame(tag = c("2026-01-01", "2026-01-02", "2026-01-03"),
                       asset_name = c("2026-01-01.db", "2026-01-02.db", "2026-01-03.db"),
                       size = c(file.size(d1), file.size(d2), file.size(d3)),
                       stringsAsFactors = FALSE)
  fetched <- character(0)
  res <- run_update(out, assets, out, force_full_rebuild = TRUE,
                    fetch_fn = function(tag, asset_name, dest) {
                      fetched <<- c(fetched, tag)
                      file.copy(file.path(work, asset_name), dest, overwrite = TRUE); dest })
  expect_equal(res$status, "updated")
  expect_equal(res$rebuilt_from, as.Date("2026-01-01"))
  expect_setequal(fetched, c("2026-01-01", "2026-01-02", "2026-01-03"))
})

test_that("a manifest recording prior history without a local panel file is fatal", {
  # Mirrors `gh release download processing-times.db ... || true` swallowing
  # a failed fetch: the manifest says there is history, but no panel file was
  # staged. Proceeding would rebuild only the incremental window and publish
  # it over the full archive. A genuine first-ever run (empty prior
  # manifest) is a separate branch below and must still succeed.
  out <- withr::local_tempdir(); work <- withr::local_tempdir()
  prev <- list(snapshots = list("2026-01-01" = list(tag = "2026-01-01",
                                size_fp = asset_fingerprint(10),
                                era = "v0.5", n_rows = 1L, status = "ok")))
  jsonlite::write_json(prev, file.path(out, "manifest.json"), auto_unbox = TRUE)
  expect_false(file.exists(file.path(out, "processing-times.db")))

  d2 <- file.path(work, "2026-01-02.db"); make_fixture_db(d2)
  assets <- data.frame(tag = c("2026-01-01", "2026-01-02"),
                       asset_name = c("2026-01-01.db", "2026-01-02.db"),
                       size = c(10, file.size(d2)), stringsAsFactors = FALSE)
  expect_error(
    run_update(out, assets, out, fetch_fn = function(tag, asset_name, dest) {
      file.copy(file.path(work, asset_name), dest, overwrite = TRUE); dest }),
    "panel state is missing")
})

test_that("a first-ever run with an empty prior manifest proceeds without a local panel", {
  out <- withr::local_tempdir(); work <- withr::local_tempdir()
  expect_false(file.exists(file.path(out, "manifest.json")))
  expect_false(file.exists(file.path(out, "processing-times.db")))

  d1 <- file.path(work, "2026-01-01.db"); make_fixture_db(d1)
  assets <- data.frame(tag = "2026-01-01", asset_name = "2026-01-01.db",
                       size = file.size(d1), stringsAsFactors = FALSE)
  res <- run_update(out, assets, out, fetch_fn = function(tag, asset_name, dest) {
    file.copy(file.path(work, asset_name), dest, overwrite = TRUE); dest })
  expect_equal(res$status, "updated")
})

test_that("a chunk of older snapshots keeps every prior row instead of dropping them", {
  state <- withr::local_tempdir(); out <- withr::local_tempdir()
  work <- withr::local_tempdir()

  # a panel that already holds a NEWER day
  newer <- file.path(work, "2026-01-05.db"); make_fixture_db(newer)
  a1 <- data.frame(tag = "2026-01-05", asset_name = "2026-01-05.db",
                   size = file.size(newer), stringsAsFactors = FALSE)
  r1 <- run_backfill(file.path(state, "w1"), a1, state,
                     fetch_fn = function(tag, asset_name, dest) {
                       file.copy(newer, dest, overwrite = TRUE); dest })
  expect_equal(nrow(r1$tables$pt_snapshots[
    as.Date(r1$tables$pt_snapshots$snapshot_date) == as.Date("2026-01-05"), ]) > 0, TRUE)

  # now collect an OLDER day as a chunk
  older <- file.path(work, "2025-12-01.db"); make_fixture_db(older)
  a2 <- data.frame(tag = "2025-12-01", asset_name = "2025-12-01.db",
                   size = file.size(older), stringsAsFactors = FALSE)
  res <- run_backfill_chunk(state, a2, out,
                            fetch_fn = function(tag, asset_name, dest) {
                              file.copy(older, dest, overwrite = TRUE); dest })
  expect_equal(res$status, "collected")
  expect_equal(res$added, 1L)

  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, "processing-times.db"))
  days <- sort(unique(as.Date(DBI::dbReadTable(con, "pt_snapshots")$snapshot_date,
                              origin = "1970-01-01")))
  DBI::dbDisconnect(con)
  # both days present: the older chunk did not evict the newer prior rows
  expect_equal(as.character(days), c("2025-12-01", "2026-01-05"))
})

test_that("pending_tags returns the oldest missing tags, oldest first", {
  prev <- list(snapshots = list("2026-01-03" = list(tag = "2026-01-03")))
  all <- c("2026-01-01", "2026-01-02", "2026-01-03", "2026-01-04")
  expect_equal(pending_tags(all, prev, 2), c("2026-01-01", "2026-01-02"))
  expect_equal(pending_tags(all, prev, 10), c("2026-01-01", "2026-01-02", "2026-01-04"))
  expect_equal(length(pending_tags(all, list(snapshots = setNames(
    lapply(all, function(t) list(tag = t)), all)), 5)), 0L)
})
