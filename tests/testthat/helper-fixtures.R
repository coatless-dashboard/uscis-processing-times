# Builds a minimal SQLite file shaped like a real snapshot of the given era.
make_fixture_db <- function(path, era = "v0.5", rows = NULL) {
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con))
  if (is.null(rows)) {
    rows <- data.frame(
      form_name = "I-102", office_code = "NBC", form_subtype = "142",
      publication_date = "August 17, 2026",
      range_upper = 23.5, range_upper_unit = "Months",
      range_lower = 18.5, range_lower_unit = "Months",
      service_request_date = "September 07, 2024",
      subtype_info_en = "Initial issuance", subtype_info_es = "Emision inicial",
      subtype_note_en = "", subtype_note_es = "",
      form_note_en = "", form_note_es = "",
      stringsAsFactors = FALSE)
  }
  DBI::dbWriteTable(con, "processing_time", rows)
  DBI::dbWriteTable(con, "forms", data.frame(name = "I-102",
    description_en = "Replacement doc", description_es = "Documento",
    stringsAsFactors = FALSE))
  DBI::dbWriteTable(con, "offices", data.frame(code = "NBC",
    description = "National Benefits Center", stringsAsFactors = FALSE))
  if (era %in% c("v0.2", "v0.5")) {
    DBI::dbWriteTable(con, "form_types", data.frame(form_name = "I-102",
      form_key = "142", form_type = "142", description_en = "Initial issuance",
      description_es = "Emision", offices = '["NBC","SCD"]', stringsAsFactors = FALSE))
  }
  if (era == "v0.5") {
    DBI::dbWriteTable(con, "foia_processing_time", data.frame(id = 1L,
      metricTimestamp = "2026-08-17T06:04:05Z", metricName = "medianDaysToClose",
      metricValue = 13L, trackId = 1L, officeId = NA_integer_, officeCode = "NRC",
      stringsAsFactors = FALSE))
  }
  path
}
