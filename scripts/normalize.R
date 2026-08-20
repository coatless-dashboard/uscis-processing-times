# scripts/normalize.R: date and unit normalization. Pure, vectorized.

# USCIS publishes dates as "August 17, 2026". Parsed in the C locale so the
# result does not depend on the runner's LC_TIME.
parse_uscis_date <- function(x) {
  x <- trimws(as.character(x))
  x[x == ""] <- NA_character_
  old <- Sys.getlocale("LC_TIME")
  on.exit(Sys.setlocale("LC_TIME", old), add = TRUE)
  Sys.setlocale("LC_TIME", "C")
  suppressWarnings(as.Date(x, format = "%B %d, %Y"))
}

# Map a published bound to months. Unparseable units yield NA plus a reason
# code; they are never coerced to zero.
normalize_bound <- function(value, unit) {
  unit_norm <- tolower(trimws(as.character(unit)))
  value <- suppressWarnings(as.numeric(value))

  reason <- rep("unknown_unit", length(unit_norm))
  reason[unit_norm %in% c("month", "months")] <- "ok"
  reason[unit_norm %in% c("day", "days")]     <- "ok"
  reason[unit_norm %in% c("week", "weeks")]   <- "ok"
  reason[unit_norm == "see notes"]            <- "see_notes"
  reason[unit_norm == "learn more."]          <- "learn_more"
  reason[is.na(unit_norm)]                    <- "missing_unit"
  # A valid unit with an unparseable/empty value (the real archive stores ''
  # where USCIS published no bound) is not a successful parse: it must not be
  # reported as "ok" with a silently missing value, and it must never resolve
  # to 0.
  reason[reason == "ok" & is.na(value)]       <- "missing_value"

  months <- rep(NA_real_, length(unit_norm))
  is_month <- unit_norm %in% c("month", "months")
  is_day   <- unit_norm %in% c("day", "days")
  is_week  <- unit_norm %in% c("week", "weeks")
  months[is_month] <- value[is_month]
  months[is_day]   <- value[is_day] / DAYS_PER_MONTH
  months[is_week]  <- value[is_week] * 7 / DAYS_PER_MONTH
  months[reason != "ok"] <- NA_real_

  data.frame(months = months, reason = reason, stringsAsFactors = FALSE)
}

# Validity is only meaningful post-normalization: "15 Days to 4 Months" is a
# valid range even though 15 > 4 in raw units. NA bounds are not violations.
range_is_valid <- function(lower_months, upper_months) {
  ifelse(is.na(lower_months) | is.na(upper_months), TRUE,
         lower_months <= upper_months)
}
