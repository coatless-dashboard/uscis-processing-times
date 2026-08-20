test_that("write_panel_sqlite round-trips every supplied table", {
  path <- withr::local_tempfile(fileext = ".db")
  tabs <- list(pt_cycles = data.frame(a = 1:3),
               coverage  = data.frame(date = as.Date("2026-01-01") + 0:2))
  write_panel_sqlite(tabs, path)
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  withr::defer(DBI::dbDisconnect(con))
  expect_setequal(DBI::dbListTables(con), c("pt_cycles", "coverage"))
  expect_equal(nrow(DBI::dbReadTable(con, "pt_cycles")), 3L)
  expect_equal(nrow(DBI::dbReadTable(con, "coverage")), 3L)
})

test_that("the arrow extract and the parquet come from the same frame", {
  monthly <- data.frame(form_name = "I-129", month = "2026-01",
                        range_upper_months = 7, stringsAsFactors = FALSE)
  pq <- withr::local_tempfile(fileext = ".parquet")
  ar <- withr::local_tempfile(fileext = ".arrow")
  write_monthly_parquet(monthly, pq)
  write_monthly_arrow(monthly, ar)
  pq_data <- arrow::read_parquet(pq)
  ar_data <- arrow::read_feather(ar)
  # Same row count
  expect_equal(nrow(pq_data), nrow(ar_data))
  # Same column count and names
  expect_equal(ncol(pq_data), ncol(ar_data))
  expect_equal(names(pq_data), names(ar_data))
  # Same data (convert arrow to data.frame for comparison)
  expect_equal(as.data.frame(pq_data), as.data.frame(ar_data))
})

test_that("needs_sharding trips only above the configured budget", {
  path <- withr::local_tempfile(fileext = ".db")
  writeLines("small", path)
  file_size <- file.size(path)

  # File does not trip when below threshold
  expect_false(needs_sharding(path))

  # File trips when above threshold
  expect_true(needs_sharding(path, trigger_bytes = 1))

  # Boundary condition: file size exactly equals trigger (should not trip with >)
  expect_false(needs_sharding(path, trigger_bytes = file_size))

  # Non-existent file never trips
  expect_false(needs_sharding(withr::local_tempfile(fileext = ".db")))
})

test_that("write_panel_sqlite preserves old file if write fails partway", {
  path <- withr::local_tempfile(fileext = ".db")
  # Write a valid initial panel
  tabs <- list(good_table = data.frame(x = 1:2))
  write_panel_sqlite(tabs, path)

  # Verify it was written and close the connection
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  expect_equal(DBI::dbListTables(con), "good_table")
  expect_equal(nrow(DBI::dbReadTable(con, "good_table")), 2L)
  DBI::dbDisconnect(con)

  # Now attempt to write a list with an invalid second element that will fail
  # dbWriteTable will fail when it tries to coerce a raw vector to a data frame
  bad_tabs <- list(first_table = data.frame(y = 1:3),
                   bad_table = as.raw(c(1, 2, 3)))

  # This should fail
  expect_error(write_panel_sqlite(bad_tabs, path))

  # The original file at path should still exist and be intact.
  # Open a FRESH connection to verify real on-disk state, not a stale descriptor.
  expect_true(file.exists(path))
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  withr::defer(DBI::dbDisconnect(con))
  # Original table should still be there (and corrupted tables should not exist)
  expect_equal(DBI::dbListTables(con), "good_table")
  expect_equal(nrow(DBI::dbReadTable(con, "good_table")), 2L)
})

test_that("write_panel_sqlite uses a fresh, unpredictable temp file on every write", {
  # Regression guard for a specific prior implementation: naming the temp
  # file deterministically from the target path (e.g. paste0(path, ".tmp"))
  # means two runs writing the same `path` -- or a crashed run's leftover
  # temp file -- can collide. This test seeded such a leftover file at
  # `paste0(path, ".tmp")` and merely checked the PUBLISHED file's contents,
  # but write_panel_sqlite never reads from or writes to that fixed name --
  # it always calls tempfile() -- so that assertion could not fail under any
  # implementation. This rewrite instead verifies the actual mechanism: that
  # tempfile() is called at all, and that it is not called with the same
  # name twice.
  path <- withr::local_tempfile(fileext = ".db")

  seen <- character(0)
  real_tempfile <- tempfile
  fake_tempfile <- function(...) {
    p <- real_tempfile(...)
    seen[[length(seen) + 1]] <<- p
    p
  }
  # write_panel_sqlite is sourced into the global environment (this is not a
  # package), so shadowing `tempfile` there for the duration of this test is
  # enough to observe every call it makes.
  assign("tempfile", fake_tempfile, envir = globalenv())
  withr::defer(assign("tempfile", real_tempfile, envir = globalenv()))

  write_panel_sqlite(list(a = data.frame(x = 1)), path)
  write_panel_sqlite(list(a = data.frame(x = 2)), path)

  expect_length(seen, 2)
  expect_false(identical(seen[1], seen[2]))
})
