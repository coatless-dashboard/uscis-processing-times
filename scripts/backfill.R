# scripts/backfill.R: one-time local load. A FULL backfill run must happen on
# a workstation, not in CI: 1,574 requests and ~943 MB is a poor CI citizen
# and transient failures were observed at high parallelism. The same
# run_backfill() is also called nightly, in CI, by run_update() (scripts/
# update.R) with only the small incremental asset list for that run.

run_backfill <- function(work_dir, assets, out_dir, fetch_fn = download_snapshot,
                         prior_snapshots = NULL, prior_offices = NULL,
                         prior_manifest = list(),
                         rekey_path = "data/xwalk_key_rekey.csv") {
  dir.create(work_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(out_dir,  showWarnings = FALSE, recursive = TRUE)

  # Downloads land in a dedicated subdirectory of work_dir, never directly in
  # work_dir itself: work_dir may already hold same-named files (a resumed
  # run, or a fetch_fn reading from a fixture placed there), and a download
  # destination equal to its own source is a hard error, not a no-op.
  dl_dir <- file.path(work_dir, "downloads")
  dir.create(dl_dir, showWarnings = FALSE, recursive = TRUE)

  snaps <- list(); manifest_snaps <- list(); offs <- list()
  for (i in seq_len(nrow(assets))) {
    tag  <- assets$tag[i]
    dest <- file.path(dl_dir, assets$asset_name[i])
    ok <- tryCatch({ fetch_fn(tag, assets$asset_name[i], dest); TRUE },
                   error = function(e) FALSE)
    v <- if (ok) validate_snapshot(dest, assets$size[i]) else
                 list(ok = FALSE, reason = "download_failed",
                      era = NA_character_, n_rows = NA_integer_)

    if (!isTRUE(v$ok)) {
      manifest_snaps[[tag]] <- list(tag = tag, size_fp = asset_fingerprint(assets$size[i]),
                                    era = v$era,
                                    n_rows = NA_integer_, status = "quarantined",
                                    reason = v$reason)
      next
    }
    con <- DBI::dbConnect(RSQLite::SQLite(), dest)
    snaps[[tag]] <- extract_snapshot(con, as.Date(tag))
    offs[[tag]]  <- extract_offices(con, as.Date(tag))
    DBI::dbDisconnect(con)
    manifest_snaps[[tag]] <- list(tag = tag, size_fp = asset_fingerprint(assets$size[i]),
                                  era = v$era,
                                  n_rows = v$n_rows, status = "ok", reason = "ok")
  }

  pt <- do.call(rbind, snaps)
  # The raw panel is the source of truth: prior rows persist across runs, and
  # every derived table below is a full rebuild over the union of prior and
  # newly fetched rows, never a patch over only the newly fetched window.
  if (!is.null(prior_snapshots)) {
    pt <- if (is.null(pt)) prior_snapshots else rbind(prior_snapshots, pt)
  }
  present <- sort(unique(as.Date(pt$snapshot_date)))

  # Archive span includes every tag ever known (prior runs plus this one), not
  # just the tags fetched this run, so coverage accounting does not shrink to
  # the incremental window and lose earlier absent/anomalous days.
  known_tags <- c(assets$tag,
                  vapply(prior_manifest$snapshots, function(s) s$tag, character(1)))
  span <- range(as.Date(known_tags))

  counts <- vapply(present, function(d) sum(as.Date(pt$snapshot_date) == d), integer(1))
  coverage <- build_coverage(present, counts, span[1], span[2])

  # Re-keys (e.g. I-526 -> I-526(L)/I-526E) resolve through this crosswalk so
  # a withdrawn key that was actually renamed shows as a re-key rather than a
  # bare drop. The committed crosswalk is header-only today; that must
  # resolve to zero rows, not an error, so adding a row later takes effect
  # without any further wiring.
  rekey <- if (!is.null(rekey_path) && file.exists(rekey_path)) {
    utils::read.csv(rekey_path, stringsAsFactors = FALSE, colClasses = "character")
  } else {
    NULL
  }

  cycles  <- build_cycles(pt, present)
  events  <- build_events(cycles)
  inquiry <- build_inquiry_events(pt)
  disc    <- build_disclosure_events(pt, present, rekey = rekey)
  monthly <- project_monthly(cycles)

  manifest <- merge_manifest(prior_manifest, manifest_snaps)
  manifest$freeze <- measure_freeze(cycles)
  manifest$checks <- run_standing_checks(pt[as.Date(pt$snapshot_date) == max(present), ])
  jsonlite::write_json(manifest, file.path(out_dir, "manifest.json"),
                       auto_unbox = TRUE, pretty = TRUE)

  # prior offices are folded back in so a code seen only in an already-processed
  # window is not lost when a run fetches nothing new
  if (!is.null(prior_offices) && nrow(prior_offices)) {
    offs[["__prior__"]] <- data.frame(
      code = prior_offices$code, description = prior_offices$description,
      snapshot_date = as.Date(prior_offices$last_seen), stringsAsFactors = FALSE)
    offs[["__prior_first__"]] <- data.frame(
      code = prior_offices$code, description = prior_offices$description,
      snapshot_date = as.Date(prior_offices$first_seen), stringsAsFactors = FALSE)
  }
  offices <- fold_offices(offs)

  tables <- list(pt_snapshots = pt, pt_cycles = cycles, pt_events = events,
                 inquiry_events = inquiry, disclosure_events = disc,
                 coverage = coverage, offices = offices)
  write_panel_sqlite(tables, file.path(out_dir, "processing-times.db"))
  write_monthly_parquet(monthly, file.path(out_dir, "processing-times-monthly.parquet"))
  write_monthly_arrow(monthly, file.path(out_dir, "panel-monthly.arrow"))

  list(manifest = manifest, tables = tables, monthly = monthly)
}
