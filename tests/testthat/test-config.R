test_that("constants have the exact values the pipeline depends on", {
  expect_identical(DAYS_PER_MONTH, 30.4375)
  expect_identical(KEY_SEP, "|")
  expect_identical(STITCH_MAP[["CSC"]], "SCD")
  expect_setequal(names(STITCH_MAP), c("CSC", "NSC", "SSC", "ESC", "YSC"))
  expect_identical(DISCLOSURE_MIN_ABSENT, 2L)
  expect_identical(DISCLOSURE_CONFIRM_ABSENT, 30L)
  expect_identical(names(ERA_MARKERS), c("v0.1", "v0.2", "v0.5"))
})

test_that("the suite runs under testthat 3rd edition", {
  expect_identical(testthat::edition_get(), 3L)
})
