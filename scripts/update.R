# scripts/update.R: nightly incremental run.
#
# Failure semantics. Fatal (run fails, nothing published): panel corruption,
# derived rebuild error. Warning (run continues and publishes, recorded in the
# manifest): a missing upstream release, a quarantined snapshot, an unknown era.

run_update <- function(state_dir, assets, out_dir, fetch_fn = download_snapshot,
                       force_full_rebuild = FALSE) {
  manifest_path <- file.path(state_dir, "manifest.json")
  prev <- if (file.exists(manifest_path)) {
    jsonlite::read_json(manifest_path, simplifyVector = FALSE)
  } else list(snapshots = list())

  current <- stats::setNames(lapply(seq_len(nrow(assets)), function(i) {
    list(tag = assets$tag[i], size_fp = asset_fingerprint(assets$size[i]))
  }), assets$tag)

  # A forced rebuild ignores the diff entirely: the anchor becomes the
  # earliest known tag so every snapshot is re-fetched and every derived
  # table is recomputed from scratch, rather than only the tags that
  # actually changed upstream. This is the operator's lever for recovering
  # from a bad panel.
  from <- if (isTRUE(force_full_rebuild) && nrow(assets) > 0) {
    min(as.Date(assets$tag))
  } else {
    rebuild_from_date(prev, current)
  }
  if (is.na(from)) {
    return(list(status = "no_change", rebuilt_from = as.Date(NA),
                warnings = character(0)))
  }

  # Date-anchored rebuild: everything from `from` forward is recomputed, so a
  # withdrawal (a key that is absent, and therefore never "touched") is caught.
  affected <- assets[as.Date(assets$tag) >= from, , drop = FALSE]

  # Rows before `from` are not re-extracted, but the published panel and every
  # derived table must still cover full history: the raw panel is the source
  # of truth, so prior rows are read back from the last published panel and
  # carried forward into the union `run_backfill` rebuilds from. A round trip
  # through SQLite loses R's Date/logical classes (they come back as numeric/
  # integer), so those columns are restored before the union is built.
  panel_path <- file.path(state_dir, "processing-times.db")

  # A manifest recording prior history without a corresponding local panel
  # file means the panel failed to download (or was never staged) -- most
  # concretely, `gh release download ... || true` swallowing a failed fetch.
  # Proceeding here would carry forward `prior_snapshots <- NULL`, rebuild
  # only the incremental window, and publish that truncated panel over the
  # full archive with `--clobber`. Panel corruption is fatal: nothing is
  # published. A genuine first-ever run has an empty prior manifest and is
  # unaffected by this check.
  if (length(prev$snapshots) > 0 && !file.exists(panel_path)) {
    stop("panel state is missing: manifest records ", length(prev$snapshots),
        " prior snapshot(s) but no panel file was found at ", panel_path,
        " -- refusing to rebuild and publish a truncated panel")
  }

  prior_snapshots <- NULL; prior_offices <- NULL
  if (file.exists(panel_path)) {
    con <- DBI::dbConnect(RSQLite::SQLite(), panel_path)
    prior <- DBI::dbReadTable(con, "pt_snapshots")
    # a panel built before the offices dimension existed simply has none yet
    prior_offices <- tryCatch(DBI::dbReadTable(con, "offices"), error = function(e) NULL)
    DBI::dbDisconnect(con)
    if (!is.null(prior_offices) && nrow(prior_offices)) {
      prior_offices$first_seen <- as.Date(prior_offices$first_seen, origin = "1970-01-01")
      prior_offices$last_seen  <- as.Date(prior_offices$last_seen,  origin = "1970-01-01")
    }
    prior$snapshot_date        <- as.Date(prior$snapshot_date,        origin = "1970-01-01")
    prior$publication_date     <- as.Date(prior$publication_date,     origin = "1970-01-01")
    prior$service_request_date <- as.Date(prior$service_request_date, origin = "1970-01-01")
    prior$range_valid          <- as.logical(prior$range_valid)
    prior_snapshots <- prior[prior$snapshot_date < from, , drop = FALSE]
  }

  res <- run_backfill(work_dir = file.path(state_dir, "work"),
                      assets = affected, out_dir = out_dir, fetch_fn = fetch_fn,
                      prior_snapshots = prior_snapshots, prior_offices = prior_offices,
                      prior_manifest = prev)

  warns <- character(0)
  for (tag in names(res$manifest$snapshots)) {
    st <- res$manifest$snapshots[[tag]]
    if (!identical(st$status, "ok")) {
      warns <- c(warns, sprintf("quarantined %s (%s)", tag, st$reason))
    }
  }
  failed_checks <- res$manifest$checks[!res$manifest$checks$ok, ]
  if (nrow(failed_checks) > 0) {
    warns <- c(warns, sprintf("standing check failed: %s", failed_checks$detail))
  }

  list(status = "updated", rebuilt_from = from, warnings = warns)
}

# Collect the archive a chunk at a time.
#
# This is not run_update with a different anchor. The incremental path drops
# every prior row from the rebuild anchor forward and re-fetches it, which is
# correct when upstream re-cuts a day. A chunk is OLDER than everything already
# held, so the opposite applies: every prior row must survive, and the derived
# tables are rebuilt over the union. Dropping by date here would silently
# discard the newer half of the panel on every chunk.
run_backfill_chunk <- function(state_dir, assets, out_dir,
                               fetch_fn = download_snapshot) {
  manifest_path <- file.path(state_dir, "manifest.json")
  prev <- if (file.exists(manifest_path)) {
    jsonlite::read_json(manifest_path, simplifyVector = FALSE)
  } else list(snapshots = list())

  if (nrow(assets) == 0) {
    return(list(status = "complete", added = 0L, warnings = character(0)))
  }

  panel_path <- file.path(state_dir, "processing-times.db")
  prior_snapshots <- NULL; prior_offices <- NULL
  if (file.exists(panel_path)) {
    con <- DBI::dbConnect(RSQLite::SQLite(), panel_path)
    prior_snapshots <- DBI::dbReadTable(con, "pt_snapshots")
    prior_offices <- tryCatch(DBI::dbReadTable(con, "offices"), error = function(e) NULL)
    DBI::dbDisconnect(con)
    prior_snapshots$snapshot_date        <- as.Date(prior_snapshots$snapshot_date,        origin = "1970-01-01")
    prior_snapshots$publication_date     <- as.Date(prior_snapshots$publication_date,     origin = "1970-01-01")
    prior_snapshots$service_request_date <- as.Date(prior_snapshots$service_request_date, origin = "1970-01-01")
    prior_snapshots$range_valid          <- as.logical(prior_snapshots$range_valid)
    if (!is.null(prior_offices) && nrow(prior_offices)) {
      prior_offices$first_seen <- as.Date(prior_offices$first_seen, origin = "1970-01-01")
      prior_offices$last_seen  <- as.Date(prior_offices$last_seen,  origin = "1970-01-01")
    }
  } else if (length(prev$snapshots)) {
    stop("manifest records ", length(prev$snapshots), " prior snapshot(s) but no ",
         "panel file was found at ", panel_path,
         " -- refusing to rebuild and publish a truncated panel", call. = FALSE)
  }

  res <- run_backfill(work_dir = file.path(state_dir, "work"),
                      assets = assets, out_dir = out_dir, fetch_fn = fetch_fn,
                      prior_snapshots = prior_snapshots, prior_offices = prior_offices,
                      prior_manifest = prev)

  warns <- character(0)
  for (tag in names(res$manifest$snapshots)) {
    st <- res$manifest$snapshots[[tag]]
    if (!identical(st$status, "ok")) {
      warns <- c(warns, sprintf("quarantined %s (%s)", tag, st$reason))
    }
  }
  list(status = "collected", added = nrow(assets),
       span = range(as.Date(assets$tag)),
       total = length(res$manifest$snapshots), warnings = warns)
}
