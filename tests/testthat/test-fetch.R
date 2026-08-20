test_that("list_release_assets flattens the API payload to tag/name/size", {
  fake_gh <- function(endpoint, ...) {
    list(
      list(tag_name = "2026-08-18",
           assets = list(list(name = "2026-08-18.db", size = 364544))),
      list(tag_name = "2026-08-17",
           assets = list(list(name = "2026-08-17.db", size = 364544)))
    )
  }
  out <- list_release_assets("owner/repo", gh_fn = fake_gh)
  expect_equal(nrow(out), 2L)
  expect_equal(out$tag, c("2026-08-18", "2026-08-17"))
  expect_equal(out$size, c(364544, 364544))
})

test_that("a non-date release tag is dropped with a warning rather than reaching as.Date()", {
  fake_gh <- function(endpoint, ...) {
    list(
      list(tag_name = "2026-08-18",
           assets = list(list(name = "2026-08-18.db", size = 364544))),
      list(tag_name = "current",
           assets = list(list(name = "processing-times.db", size = 999))),
      list(tag_name = "2026-08-17",
           assets = list(list(name = "2026-08-17.db", size = 364544)))
    )
  }
  expect_warning(out <- list_release_assets("owner/repo", gh_fn = fake_gh), "current")
  expect_equal(sort(out$tag), c("2026-08-17", "2026-08-18"))
  # The filtered tags must not silently break downstream as.Date() handling.
  expect_silent(as.Date(out$tag))
})

test_that("releases carrying no asset are dropped rather than yielding NA rows", {
  fake_gh <- function(endpoint, ...) list(list(tag_name = "2026-08-18", assets = list()))
  expect_equal(nrow(list_release_assets("owner/repo", gh_fn = fake_gh)), 0L)
})

test_that("validate_snapshot accepts a well-formed file and reports its era", {
  path <- withr::local_tempfile(fileext = ".db")
  make_fixture_db(path, era = "v0.5")
  res <- validate_snapshot(path, expected_size = file.size(path))
  expect_true(res$ok)
  expect_equal(res$era, "v0.5")
  expect_equal(res$n_rows, 1L)
})

test_that("validate_snapshot rejects a size mismatch", {
  path <- withr::local_tempfile(fileext = ".db")
  make_fixture_db(path, era = "v0.5")
  res <- validate_snapshot(path, expected_size = file.size(path) + 1)
  expect_false(res$ok)
  expect_equal(res$reason, "size_mismatch")
})

test_that("validate_snapshot rejects a file that is not a database", {
  path <- withr::local_tempfile(fileext = ".db")
  writeLines("not a database", path)
  res <- expect_silent(validate_snapshot(path, expected_size = file.size(path)))
  expect_false(res$ok)
  expect_equal(res$reason, "not_a_database")
})

test_that("validate_snapshot rejects an unknown era rather than ingesting it", {
  path <- withr::local_tempfile(fileext = ".db")
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  DBI::dbWriteTable(con, "forms", data.frame(name = "I-102", stringsAsFactors = FALSE))
  DBI::dbWriteTable(con, "processing_time", data.frame(x = 1L))
  DBI::dbDisconnect(con)
  res <- validate_snapshot(path, expected_size = file.size(path))
  expect_false(res$ok)
  expect_equal(res$reason, "unknown_era")
})
