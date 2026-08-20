# tools/collect.R: fill the archive, a chunk at a time, in one pass.
#
# This lives in tools/ rather than scripts/ because it is an entrypoint with
# side effects. The test harness sources every file in scripts/, so a bare
# script there runs on every test invocation.
#
# It loops internally rather than re-queueing itself per chunk: the panel is
# large, and a run-per-chunk would download and upload the whole thing once for
# every hundred snapshots. One pass, one publish.

for (f in list.files("scripts", pattern = "[.]R$", full.names = TRUE)) source(f)

state_dir <- Sys.getenv("STATE_DIR", "state")
out_dir   <- Sys.getenv("OUT_DIR", "out")
chunk     <- as.integer(Sys.getenv("CHUNK_SIZE", "100"))
if (is.na(chunk) || chunk < 1) chunk <- 100L

emit <- function(key, value) {
  path <- Sys.getenv("GITHUB_OUTPUT", "")
  if (nzchar(path)) cat(sprintf("%s=%s\n", key, value), file = path, append = TRUE)
}

deadline <- Sys.time() + as.numeric(Sys.getenv("BUDGET_MINUTES", "90")) * 60

read_manifest <- function() {
  path <- file.path(state_dir, "manifest.json")
  if (file.exists(path)) jsonlite::read_json(path, simplifyVector = FALSE)
  else list(snapshots = list())
}

all_tags <- list_all_tags()
message("archive holds ", length(all_tags), " tags, ",
        all_tags[1], " .. ", all_tags[length(all_tags)])

collected <- 0L
repeat {
  prev <- read_manifest()
  todo <- pending_tags(all_tags, prev, chunk)
  if (length(todo) == 0) {
    message("archive complete")
    break
  }
  if (Sys.time() > deadline) {
    message("time budget reached with ", length(setdiff(all_tags, names(prev$snapshots))),
            " still to collect; run again to continue")
    break
  }

  message("collecting ", todo[1], " .. ", todo[length(todo)],
          "  (", length(todo), " snapshots)")
  assets <- assets_for_tags(todo)
  if (nrow(assets) == 0) stop("no downloadable asset for any tag in this chunk", call. = FALSE)

  res <- run_backfill_chunk(state_dir, assets, out_dir)
  for (w in res$warnings) message("WARNING: ", w)
  collected <- collected + res$added
  message("  panel now holds ", res$total, " snapshots")

  # the next chunk reads state/, so this run's output becomes the next input
  for (f in list.files(out_dir, full.names = TRUE)) {
    file.copy(f, file.path(state_dir, basename(f)), overwrite = TRUE)
  }
}

emit("collected", collected)
emit("remaining", length(setdiff(all_tags, names(read_manifest()$snapshots))))
message("collected ", collected, " snapshot(s) this run")
