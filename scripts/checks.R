# assertions on properties the design depends on but that were only ever observed,
# never guaranteed. These fail the run so a silent upstream change cannot
# invalidate the site's claims.

run_standing_checks <- function(latest_snapshot) {
  s <- latest_snapshot
  chk <- function(name, ok, detail) {
    data.frame(check = name, ok = ok, detail = detail, stringsAsFactors = FALSE)
  }

  es <- mean(nzchar(ifelse(is.na(s$subtype_info_es), "", s$subtype_info_es)))
  notes <- sum(nzchar(ifelse(is.na(s$form_note_en), "", s$form_note_en)))
  n400_offices <- length(unique(s$office_code[s$form_name == "N-400"]))

  rbind(
    chk("spanish_fill_complete", isTRUE(all.equal(es, 1)),
        sprintf("subtype_info_es fill = %.3f, expected 1.000", es)),
    chk("form_note_en_empty", notes == 0L,
        sprintf("form_note_en non-empty rows = %d, expected 0", notes)),
    chk("n400_office_count", n400_offices == 90L,
        sprintf("N-400 offices = %d, expected 90", n400_offices))
  )
}
