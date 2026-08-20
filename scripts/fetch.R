# scripts/fetch.R: release discovery, download, validation.

# gh_fn is injected so the listing logic is testable without network access.
#
# The releases endpoint serves at most 1000 rows and returns a hard 422 on the
# request that would cross that line, so asking for every release fails outright
# once an archive passes 1000 of them. Pages are requested one at a time,
# newest first, and the walk stops as soon as it reaches a tag already held.
# A steady-state run therefore costs a single page, and a deeper archive
# degrades to "the newest 1000" with a warning rather than an error.
API_RELEASE_PAGE <- 100L
API_RELEASE_MAX_PAGES <- 10L   # 10 * 100 = the 1000-row ceiling

list_release_assets <- function(repo = SOURCE_RELEASES_REPO, gh_fn = NULL,
                                known_tags = character(0)) {
  if (is.null(gh_fn)) gh_fn <- function(endpoint, ...) gh::gh(endpoint, ...)
  releases <- list()
  hit_ceiling <- TRUE
  for (page in seq_len(API_RELEASE_MAX_PAGES)) {
    batch <- gh_fn(paste0("GET /repos/", repo, "/releases"),
                   per_page = API_RELEASE_PAGE, page = page)
    if (length(batch) == 0) { hit_ceiling <- FALSE; break }
    releases <- c(releases, batch)
    if (length(batch) < API_RELEASE_PAGE) { hit_ceiling <- FALSE; break }
    # everything older than this is already accounted for
    if (length(known_tags) &&
        all(vapply(batch, function(r) r$tag_name %in% known_tags, logical(1)))) {
      hit_ceiling <- FALSE; break
    }
  }
  if (hit_ceiling) {
    warning("release listing stopped at the API ceiling of ",
            API_RELEASE_MAX_PAGES * API_RELEASE_PAGE, " releases; anything older ",
            "than that cannot be listed and must already be in the panel")
  }

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

# The releases endpoint stops at 1000 rows, but the tags endpoint does not, and
# a release can be fetched directly by tag. Together those let the archive be
# collected in chunks however deep it goes, without ever listing all releases.
list_all_tags <- function(repo = SOURCE_RELEASES_REPO, gh_fn = NULL,
                          max_pages = 100L) {
  if (is.null(gh_fn)) gh_fn <- function(endpoint, ...) gh::gh(endpoint, ...)
  tags <- character(0)
  for (page in seq_len(max_pages)) {
    batch <- gh_fn(paste0("GET /repos/", repo, "/tags"),
                   per_page = API_RELEASE_PAGE, page = page)
    if (length(batch) == 0) break
    tags <- c(tags, vapply(batch, function(t) t$name, character(1)))
    if (length(batch) < API_RELEASE_PAGE) break
  }
  sort(unique(tags[grepl("^\\d{4}-\\d{2}-\\d{2}$", tags)]))
}

# The asset for a tag, without asking the API. Release assets are named after
# their tag, and validate_snapshot() checks the file we actually receive
# (SQLite integrity, required tables, era) rather than trusting a byte count,
# so no per-tag lookup is needed. A tag whose asset is named otherwise fails
# the download and is quarantined with a reason, which is visible, rather than
# being silently skipped.
assets_for_tags <- function(tags) {
  data.frame(tag = tags, asset_name = paste0(tags, ".db"),
             size = rep(NA_real_, length(tags)), stringsAsFactors = FALSE)
}

# Oldest first, so the archive fills backwards from where it already reaches.
pending_tags <- function(all_tags, prev, chunk_size) {
  have <- if (is.null(prev$snapshots)) character(0) else names(prev$snapshots)
  utils::head(sort(setdiff(all_tags, have)), chunk_size)
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
