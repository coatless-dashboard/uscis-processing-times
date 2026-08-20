# scripts/extract.R: era detection and snapshot extraction.

# Classify a snapshot by which tables it carries. Date is deliberately not used:
# a backfilled or re-cut release would defeat date-based dispatch.
detect_era <- function(tables) {
  for (era in names(ERA_MARKERS)) {
    m <- ERA_MARKERS[[era]]
    if (all(m$require %in% tables) && !any(m$forbid %in% tables)) return(era)
  }
  "unknown"
}

# Read one snapshot into a tidy frame. Raw published values are kept alongside
# the normalized ones so every derived value stays auditable.
# The office code -> name mapping is published in every snapshot era. Carrying
# it into the panel means the dashboard needs nothing but the panel, and the
# names of retired offices survive because older snapshots still carry them.
extract_offices <- function(con, snapshot_date) {
  off <- tryCatch(
    DBI::dbGetQuery(con, "SELECT code, description FROM offices"),
    error = function(e) data.frame(code = character(), description = character(),
                                   stringsAsFactors = FALSE))
  if (!nrow(off)) return(off)
  data.frame(code = off$code, description = off$description,
             snapshot_date = as.Date(rep(snapshot_date, nrow(off))),
             stringsAsFactors = FALSE)
}

# Fold per-snapshot office rows into one dimension. The newest description for a
# code wins, so a renamed office reads under its current name, while a code that
# stopped appearing keeps the last name it was published under.
fold_offices <- function(rows) {
  if (!length(rows)) return(data.frame(code = character(), description = character(),
                                       first_seen = as.Date(character()),
                                       last_seen = as.Date(character()),
                                       stringsAsFactors = FALSE))
  all <- do.call(rbind, rows)
  all <- all[!is.na(all$code) & nzchar(all$code), , drop = FALSE]
  all <- all[order(all$code, all$snapshot_date), , drop = FALSE]
  split_by <- split(seq_len(nrow(all)), all$code)
  do.call(rbind, lapply(names(split_by), function(k) {
    i <- split_by[[k]]
    data.frame(code = k,
               description = all$description[i[length(i)]],
               first_seen = min(all$snapshot_date[i]),
               last_seen  = max(all$snapshot_date[i]),
               stringsAsFactors = FALSE)
  }))
}

extract_snapshot <- function(con, snapshot_date) {
  # range_lower/range_upper are read as TEXT, not left to their native SQLite
  # storage class. The real archive stores these columns with MIXED storage
  # classes: numeric rows alongside an empty string '' where USCIS published
  # no bound. RSQLite sniffs a REAL column from the first-seen value and then
  # coerces the '' rows through SQLite's numeric coercion, which yields 0.0 --
  # not NA. Casting to TEXT here keeps that coercion from ever happening; the
  # existing suppressWarnings(as.numeric(...)) below turns '' into NA in R,
  # where it belongs. Do NOT change this to CAST(... AS REAL): SQLite itself
  # casts '' to 0.0, reproducing the exact defect this works around.
  pt <- DBI::dbGetQuery(con, "
    SELECT form_name, office_code, form_subtype, publication_date,
           CAST(range_lower AS TEXT) AS range_lower, range_lower_unit,
           CAST(range_upper AS TEXT) AS range_upper, range_upper_unit,
           service_request_date, subtype_info_en, subtype_info_es,
           subtype_note_en, subtype_note_es, form_note_en, form_note_es
    FROM processing_time")

  lo <- normalize_bound(pt$range_lower, pt$range_lower_unit)
  up <- normalize_bound(pt$range_upper, pt$range_upper_unit)

  stitched <- ifelse(pt$office_code %in% names(STITCH_MAP),
                     unname(STITCH_MAP[pt$office_code]), pt$office_code)

  data.frame(
    snapshot_date        = as.Date(rep(snapshot_date, nrow(pt))),
    form_name            = pt$form_name,
    office_code          = pt$office_code,
    form_subtype         = pt$form_subtype,
    office_code_stitched = stitched,
    publication_date     = parse_uscis_date(pt$publication_date),
    service_request_date = parse_uscis_date(pt$service_request_date),
    range_lower          = suppressWarnings(as.numeric(pt$range_lower)),
    range_lower_unit     = pt$range_lower_unit,
    range_upper          = suppressWarnings(as.numeric(pt$range_upper)),
    range_upper_unit     = pt$range_upper_unit,
    range_lower_months   = lo$months,
    range_upper_months   = up$months,
    range_lower_reason   = lo$reason,
    range_upper_reason   = up$reason,
    range_valid          = range_is_valid(lo$months, up$months),
    subtype_info_en      = pt$subtype_info_en,
    subtype_info_es      = pt$subtype_info_es,
    subtype_note_en      = pt$subtype_note_en,
    subtype_note_es      = pt$subtype_note_es,
    form_note_en         = pt$form_note_en,
    form_note_es         = pt$form_note_es,
    stringsAsFactors     = FALSE)
}
