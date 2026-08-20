# scripts/artifacts.R: published outputs.

write_panel_sqlite <- function(tables, path) {
  # Write to a temporary file with a unique name in the same directory to ensure
  # atomic rename and avoid stale-file collisions if a prior run crashed.
  temp_path <- tempfile(tmpdir = dirname(path), fileext = ".db")

  con <- DBI::dbConnect(RSQLite::SQLite(), temp_path)
  # Clean up on.exit handlers in reverse order: first remove temp file,
  # then disconnect on any error (exit before rename succeeds)
  on.exit({
    DBI::dbDisconnect(con)
    if (file.exists(temp_path)) unlink(temp_path)
  }, add = TRUE)

  # Write all tables to the temporary database
  for (nm in names(tables)) DBI::dbWriteTable(con, nm, tables[[nm]], overwrite = TRUE)

  # Close the connection before renaming to release all file locks
  DBI::dbDisconnect(con)

  # Clear on.exit now that we've successfully disconnected
  # Atomically move temp file to final path only after successful completion
  on.exit(NULL)
  file.rename(temp_path, path)

  invisible(path)
}

write_monthly_parquet <- function(monthly, path) {
  arrow::write_parquet(monthly, path)
  invisible(path)
}

# The dashboard extract is written from the SAME frame as the published
# parquet so the site and the download cannot drift apart.
write_monthly_arrow <- function(monthly, path) {
  arrow::write_feather(monthly, path)
  invisible(path)
}

needs_sharding <- function(path, trigger_bytes = SHARD_TRIGGER_BYTES) {
  file.exists(path) && file.size(path) > trigger_bytes
}
