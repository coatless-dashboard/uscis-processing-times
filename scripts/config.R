# scripts/config.R: pipeline constants. Sourced by every other script.

# Upstream release source. Supplied by the environment so the name is not
# carried in the repository: set SOURCE_RELEASES_REPO as an Actions secret in
# CI, and in a git-ignored .Renviron for local work.
SOURCE_RELEASES_REPO <- Sys.getenv("SOURCE_RELEASES_REPO", unset = "")
if (!nzchar(SOURCE_RELEASES_REPO)) {
  stop("SOURCE_RELEASES_REPO is not set. Put it in .Renviron locally, or in ",
       "repository secrets for CI.", call. = FALSE)
}

# Mean Gregorian month length, used for every unit conversion.
DAYS_PER_MONTH <- 30.4375

# Key identity separator. Form, office, and subtype codes never contain it.
KEY_SEP <- "|"

# Upstream schema eras, detected by table presence rather than by date so that
# a re-cut or backfilled release cannot be misclassified.
ERA_MARKERS <- list(
  "v0.1" = list(require = c("forms", "offices", "processing_time"),
                forbid  = c("form_types", "foia_processing_time")),
  "v0.2" = list(require = c("forms", "offices", "processing_time", "form_types"),
                forbid  = c("foia_processing_time")),
  "v0.5" = list(require = c("forms", "offices", "processing_time", "form_types",
                            "foia_processing_time"),
                forbid  = character(0))
)

# The five named service centers retired 2025-10-22, folded into SCOPS for
# charting only. Never applied to disclosure derivation.
STITCH_MAP <- c(CSC = "SCD", NSC = "SCD", SSC = "SCD", ESC = "SCD", YSC = "SCD")

# A present snapshot whose row count deviates from the trailing 30-day median by
# more than this fraction is flagged anomalous (but still ingested).
COVERAGE_ANOMALY_FRAC <- 0.05

# Inquiry date advances +1 day per calendar day. Excess beyond this is a leap.
INQUIRY_LEAP_TOLERANCE_DAYS <- 1L

# A key must be absent from this many consecutive present snapshots before a
# disappearance is emitted, and this many before it is confirmed.
DISCLOSURE_MIN_ABSENT     <- 2L
DISCLOSURE_CONFIRM_ABSENT <- 30L

# Shard the published SQLite by year once it exceeds this size.
SHARD_TRIGGER_BYTES <- 250e6
