mk <- function(n = 90, es_fill = 1, note_fill = 0) {
  data.frame(
    form_name = "N-400", office_code = sprintf("O%02d", seq_len(n)),
    form_subtype = "160A",
    subtype_info_es = ifelse(seq_len(n) <= n * es_fill, "texto", ""),
    form_note_en = ifelse(seq_len(n) <= n * note_fill, "note", ""),
    stringsAsFactors = FALSE)
}

test_that("standing checks pass on data matching the documented shape", {
  res <- run_standing_checks(mk())
  expect_true(all(res$ok))
  expect_setequal(res$check,
    c("spanish_fill_complete", "form_note_en_empty", "n400_office_count"))
})

test_that("a drop in Spanish coverage fails rather than passing silently", {
  res <- run_standing_checks(mk(es_fill = 0.5))
  expect_false(res$ok[res$check == "spanish_fill_complete"])
})

test_that("form_note_en gaining content fails, since the pipeline ignores it", {
  res <- run_standing_checks(mk(note_fill = 0.1))
  expect_false(res$ok[res$check == "form_note_en_empty"])
})

test_that("a change in the N-400 office count fails", {
  res <- run_standing_checks(mk(n = 88))
  expect_false(res$ok[res$check == "n400_office_count"])
})

test_that("an upward drift in N-400 office count fails", {
  res <- run_standing_checks(mk(n = 92))
  expect_false(res$ok[res$check == "n400_office_count"])
})
