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

test_that("listing pages through the API and stops when a page is short", {
  pages <- list()
  fake_gh <- function(endpoint, per_page = NULL, page = NULL, ...) {
    pages[[length(pages) + 1L]] <<- page
    if (page == 1L) {
      lapply(seq_len(100), function(i)
        list(tag_name = sprintf("2026-%02d-%02d", (i %% 12) + 1, (i %% 28) + 1),
             assets = list(list(name = "x.db", size = 1))))
    } else if (page == 2L) {
      list(list(tag_name = "2025-01-01", assets = list(list(name = "x.db", size = 1))))
    } else {
      list()
    }
  }
  out <- list_release_assets("owner/repo", gh_fn = fake_gh)
  expect_equal(unlist(pages), c(1L, 2L))          # stopped after the short page
  expect_true("2025-01-01" %in% out$tag)
})

test_that("listing stops early once a page holds only tags already known", {
  seen <- 0L
  fake_gh <- function(endpoint, per_page = NULL, page = NULL, ...) {
    seen <<- seen + 1L
    lapply(seq_len(100), function(i)
      list(tag_name = sprintf("2026-01-%02d", i %% 28 + 1),
           assets = list(list(name = "x.db", size = 1))))
  }
  known <- sprintf("2026-01-%02d", 1:28)
  out <- list_release_assets("owner/repo", gh_fn = fake_gh, known_tags = known)
  expect_equal(seen, 1L)                          # one page, not ten
  expect_gt(nrow(out), 0L)
})

test_that("an archive deeper than the API ceiling warns instead of erroring", {
  fake_gh <- function(endpoint, per_page = NULL, page = NULL, ...) {
    lapply(seq_len(100), function(i)
      list(tag_name = sprintf("20%02d-01-01", page * 2 + i %% 9),
           assets = list(list(name = "x.db", size = 1))))
  }
  expect_warning(out <- list_release_assets("owner/repo", gh_fn = fake_gh), "ceiling")
  expect_equal(nrow(out), 1000L)                  # capped, not crashed
})

test_that("assets_for_tags derives the asset from the tag without calling the API", {
  out <- assets_for_tags(c("2022-03-21", "2026-08-18"))
  expect_equal(out$asset_name, c("2022-03-21.db", "2026-08-18.db"))
  # size is unknown here on purpose: validate_snapshot() checks the file we
  # actually receive, and NA disables its redundant byte-count comparison
  expect_true(all(is.na(out$size)))
  expect_equal(nrow(assets_for_tags(character(0))), 0L)
})
