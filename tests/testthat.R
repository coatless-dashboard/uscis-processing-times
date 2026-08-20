library(testthat)
for (f in list.files("scripts", pattern = "[.]R$", full.names = TRUE)) source(f)

# There is no DESCRIPTION file (this is not a package), so test_dir()'s own
# internal edition lookup (find_edition()) would otherwise default every
# test file to 2nd edition regardless of what local_edition(3) sets before
# calling it -- test_dir() re-derives and overwrites the edition itself.
# find_edition() checks the TESTTHAT_EDITION environment variable first, so
# that is what actually makes 3rd edition apply throughout; local_edition(3)
# is kept alongside it for a consistent edition_get() from the moment this
# function starts. Both are set from inside a function, never at the top
# level of this script: at top level there is no enclosing frame for their
# withr-deferred reset to unwind against, and R prints
# "Error in deferred_run(env) : could not find function \"deferred_run\""
# on stderr at exit (exit code stays 0, but stderr is no longer clean).
run_tests <- function() {
  withr::local_envvar(TESTTHAT_EDITION = "3")
  local_edition(3)
  test_dir("tests/testthat", stop_on_failure = TRUE)
}
run_tests()
