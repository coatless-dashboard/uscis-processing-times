# scripts/fetch.R: release discovery, download, validation.

# gh_fn is injected so the listing logic is testable without network access.
list_release_assets <- function(repo = SOURCE_RELEASES_REPO, gh_fn = NULL) {
  if (is.null(gh_fn)) gh_fn <- function(endpoint, ...) gh::gh(endpoint, ..., .limit = Inf)
  releases <- gh_fn(paste0("GET /repos/", repo, "/releases"))

  rows <- lapply(releases, function(r) {
    if (length(r$assets) == 0) return(NULL)
    a <- r$assets[[1]]
    data.frame(tag = r$tag_name, asset_name = a$name,
               size = as.numeric(a$size), stringsAsFactors = FALSE)
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) {
    return(data.frame(tag = character(0), asset_name = character(0),
                      size = numeric(0), stringsAsFactors = FALSE))
  }
  out <- do.call(rbind, rows)

  # Snapshot tags are ISO dates ("2026-08-18"); as.Date() is applied to `tag`
  # throughout the pipeline. The upstream repo's tag namespace is external and
  # not ours to control -- one `current`, `latest`, or other non-date release
  # tag would otherwise reach as.Date() downstream and abort every run with a
  # cryptic parse error. Such tags are dropped here, with a warning, so the
  # pipeline degrades to "skip this release" instead of "die every night".
  is_date_tag <- grepl("^\\d{4}-\\d{2}-\\d{2}$", out$tag)
  if (!all(is_date_tag)) {
    warning("dropping non-date release tag(s): ",
            paste(out$tag[!is_date_tag], collapse = ", "))
  }
  out[is_date_tag, , drop = FALSE]
}

snapshot_url <- function(tag, asset_name, repo = SOURCE_RELEASES_REPO) {
  sprintf("https://github.com/%s/releases/download/%s/%s", repo, tag, asset_name)
}

# Download with retry. A transient connection failure was observed in practice
# at high parallelism, so a single-shot fetch is not sufficient.
download_snapshot <- function(tag, asset_name, dest, repo = SOURCE_RELEASES_REPO,
                              attempts = 3L) {
  url <- snapshot_url(tag, asset_name, repo)
  for (i in seq_len(attempts)) {
    ok <- tryCatch({ curl::curl_download(url, dest, quiet = TRUE); TRUE },
                   error = function(e) FALSE)
    if (ok) return(dest)
  }
  stop("download failed after ", attempts, " attempts: ", tag)
}

# Cheapest checks first: size before open, open before integrity, integrity
# before era. A failure quarantines the snapshot; it is never ingested partially.
validate_snapshot <- function(path, expected_size) {
  fail <- function(reason) list(ok = FALSE, reason = reason, era = NA_character_,
                                n_rows = NA_integer_)
  if (!file.exists(path)) return(fail("missing_file"))
  if (!is.na(expected_size) && file.size(path) != expected_size) return(fail("size_mismatch"))

  con <- tryCatch(suppressWarnings(DBI::dbConnect(RSQLite::SQLite(), path)), error = function(e) NULL)
  if (is.null(con)) return(fail("not_a_database"))
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  tables <- tryCatch(DBI::dbListTables(con), error = function(e) character(0))
  if (!"processing_time" %in% tables) return(fail("not_a_database"))

  integrity <- tryCatch(DBI::dbGetQuery(con, "PRAGMA integrity_check")[[1]][1],
                        error = function(e) "error")
  if (!identical(integrity, "ok")) return(fail("integrity_check_failed"))

  era <- detect_era(tables)
  if (era == "unknown") return(fail("unknown_era"))

  n <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM processing_time")$n[1]
  list(ok = TRUE, reason = "ok", era = era, n_rows = as.integer(n))
}
