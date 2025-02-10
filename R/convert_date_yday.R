#' From date to year-day
#'
#' Convert date in the form "YYY-MM-DD" to year-day (a.k.a. julian-day) in the form "YYYY-YDAY"
#'
#' @param mydate string in the form "YYY-MM-DD"representing input date
#' @return string in the form "YYYY-YDAY" representing output year-day (a.k.a. julian-day)
#' @export
#' @examples
#' date_to_yday("2022-05-30")

date_to_yday <- function(mydate) {

  # Convert input to Date format
  mydate <- suppressWarnings(lubridate::ymd(mydate))

  # Handle invalid dates (NA values)
  if (any(is.na(mydate))) {
    warning("Invalid date(s) detected. Returning NA for invalid inputs.")
  }

  # Construct YYYY-DDD format
  ifelse(is.na(mydate), NA,
         paste0(lubridate::year(mydate), "-", sprintf("%03d", lubridate::yday(mydate))))

}


#' From year-day to date
#'
#' Convert year-day (a.k.a. julian-day) in the form "YYYY-YDAY" to date in the form "YYY-MM-DD"
#'
#' @param yday string  in the form "YYYY-YDAY" representing input year-day (a.k.a. julian-day)
#' @return string in the form "YYY-MM-DD"representing output date
#' @export
#' @examples
#' yday_to_date("2022-150")

yday_to_date <- function(yday) {

  # Ensure input is character
  if (!is.character(yday)) stop("Input must be a character vector in 'YYYY-DDD' format.")

  # Regex check for valid format
  if (any(!grepl("^[0-9]{4}-[0-9]{3}$", yday))) {
    stop("Invalid format detected. Expected 'YYYY-DDD' (e.g., '2023-034').")
  }

  # Split into Year and Day-of-Year
  m <- stringr::str_split_fixed(yday, "-", 2)
  years <- as.numeric(m[,1])
  days <- as.numeric(m[,2])

  # Validate Day-of-Year range
  max_days <- ifelse(lubridate::leap_year(years), 366, 365)
  if (any(days < 1 | days > max_days)) {
    stop("Invalid day-of-year: Must be between 1 and 365 (or 366 in leap years).")
  }

  # Convert to Date: Start of Year + (Day-of-Year - 1)
  as.Date(paste0(years, "-01-01")) + (days - 1)

}
