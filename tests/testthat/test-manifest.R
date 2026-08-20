entry <- function(tag, size, era = "v0.5", n = 500L, status = "ok") {
  list(tag = tag, size_fp = asset_fingerprint(size), era = era, n_rows = n, status = status)
}

test_that("merge_manifest adds new tags and overwrites changed ones", {
  prev <- list(snapshots = list("2026-01-01" = entry("2026-01-01", 100)))
  new  <- list("2026-01-02" = entry("2026-01-02", 100))
  out <- merge_manifest(prev, new)
  expect_setequal(names(out$snapshots), c("2026-01-01", "2026-01-02"))
})

test_that("merge_manifest overwrites existing tags with new values", {
  prev <- list(snapshots = list("2026-01-01" = entry("2026-01-01", 100)))
  new  <- list("2026-01-01" = entry("2026-01-01", 999))
  out <- merge_manifest(prev, new)
  expect_equal(out$snapshots[["2026-01-01"]]$size_fp, asset_fingerprint(999))
})

test_that("rebuild_from_date returns the earliest new tag when nothing was re-cut", {
  prev <- list(snapshots = list("2026-01-01" = entry("2026-01-01", 100)))
  cur  <- list("2026-01-01" = entry("2026-01-01", 100),
               "2026-01-03" = entry("2026-01-03", 100),
               "2026-01-02" = entry("2026-01-02", 100))
  expect_equal(rebuild_from_date(prev, cur), as.Date("2026-01-02"))
})

test_that("a re-cut asset forces a rebuild from that date, not from the newest tag", {
  prev <- list(snapshots = list("2026-01-01" = entry("2026-01-01", 100),
                                "2026-01-02" = entry("2026-01-02", 100)))
  cur  <- list("2026-01-01" = entry("2026-01-01", 100),
               "2026-01-02" = entry("2026-01-02", 999),   # size changed upstream
               "2026-01-03" = entry("2026-01-03", 100))
  expect_equal(rebuild_from_date(prev, cur), as.Date("2026-01-02"))
})

test_that("rebuild_from_date is NA when nothing changed", {
  prev <- list(snapshots = list("2026-01-01" = entry("2026-01-01", 100)))
  expect_true(is.na(rebuild_from_date(prev, list("2026-01-01" = entry("2026-01-01", 100)))))
})

test_that("measure_freeze reports the intra-cycle edit rate that gates monthly grain", {
  cycles <- data.frame(intra_cycle_edit = c(FALSE, TRUE, FALSE, FALSE))
  f <- measure_freeze(cycles)
  expect_equal(f$n_cycles, 4L)
  expect_equal(f$n_intra_cycle_edits, 1L)
  expect_equal(f$intra_cycle_fraction, 0.25)
})
