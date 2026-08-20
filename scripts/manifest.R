# scripts/manifest.R: provenance and rebuild-scope selection.

# The published manifest must not carry the upstream's raw asset byte sizes.
# A size is a high-entropy fingerprint: with a list of (date, exact size) pairs
# anyone could confirm which archive we read by matching a handful of them.
# Re-cut detection only needs a value that CHANGES when the asset changes, so a
# keyed digest serves the same purpose and reveals nothing on its own.
asset_fingerprint <- function(size) {
  key <- Sys.getenv("SOURCE_RELEASES_REPO", unset = "")
  vapply(size, function(x) digest::digest(paste0(key, "|", x), algo = "xxhash64"),
         character(1), USE.NAMES = FALSE)
}

merge_manifest <- function(prev, new) {
  snaps <- if (is.null(prev$snapshots)) list() else prev$snapshots
  for (tag in names(new)) snaps[[tag]] <- new[[tag]]
  prev$snapshots <- snaps
  prev
}

# Scope is date-anchored, never key-anchored: a withdrawn key is by definition
# absent from the new snapshot and would never be selected by a key-based diff.
# A changed asset for an existing tag means upstream re-cut that day, so
# everything from that date forward must be rebuilt.
rebuild_from_date <- function(prev, current) {
  old <- if (is.null(prev$snapshots)) list() else prev$snapshots
  changed <- character(0)
  for (tag in names(current)) {
    if (is.null(old[[tag]])) { changed <- c(changed, tag); next }
    # A manifest written before the fingerprint existed carries a raw size.
    # Fold it forward rather than treating every tag as changed, which would
    # force a full rebuild on the first run after this change.
    old_fp <- if (!is.null(old[[tag]]$size_fp)) old[[tag]]$size_fp
              else if (!is.null(old[[tag]]$size)) asset_fingerprint(old[[tag]]$size)
              else NA_character_
    if (!identical(as.character(old_fp), as.character(current[[tag]]$size_fp))) {
      changed <- c(changed, tag)
    }
  }
  if (length(changed) == 0) return(as.Date(NA))
  min(as.Date(changed))
}

# The monthly analytical grain assumes bounds are frozen between publications.
# This measures that assumption instead of trusting it; a high fraction means
# the grain and the published extract must be re-sized.
measure_freeze <- function(cycles) {
  n <- nrow(cycles)
  edits <- sum(cycles$intra_cycle_edit, na.rm = TRUE)
  list(n_cycles = as.integer(n), n_intra_cycle_edits = as.integer(edits),
       intra_cycle_fraction = if (n == 0) NA_real_ else edits / n)
}
