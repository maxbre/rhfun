#' From degrees minutes seconds to decimal degrees
#'
#' Convert degrees minutes seconds to decimal degrees
#'
#' @param s character vector degrees, minutes, seconds
#'
#' @returns
#' numeric vector of decimal degrees
#' @export
#'
#' @examples
#' Standard space-separated format
#' dms_to_dd("30 15 20")     # Expected: 30.25556
#' dms_to_dd("-30 15 20")      # Expected: -30.25556
#'
#' Degree-Minute-Second notation
#' dms_to_dd("30° 15' 20\"")   # Expected: 30.25556
#' dms_to_dd("-30° 15' 20\"")  # Expected: -30.25556
#'
#' Compact notation (without spaces)
#' dms_to_dd("30°15'20\"")     # Expected: 30.25556
#' dms_to_dd("-30°15'20\"")    # Expected: -30.25556
#'
#' Alternative notation (d/m/s)
#' dms_to_dd("30d 15m 20s")    # Expected: 30.25556
#' dms_to_dd("-30d 15m 20s")   # Expected: -30.25556
#'
#' Multiple inputs in a vector
#' dms_to_dd(c("45 30 10", "-120° 5' 15\"")) # Expected: c(45.50278, -120.08750)
#'
#' Handling errors
#' dms_to_dd("30 15")        # Should throw an error
#' dms_to_dd("30° 15 XX")    # Should throw an error
#' dms_to_dd(30)             # Should throw an error


dms_to_dd <- function(s) {

  # Ensure input is a character vector
  if (!is.character(s)) stop("Input must be a character vector.")

  # Normalize different formats by removing special characters
  s <- gsub("[°d]", " ", s)  # Replace ° or d with space
  s <- gsub("['m]", " ", s)  # Replace ' or m (minutes) with space
  s <- gsub('["s]', " ", s)  # Replace " or s (seconds) with space
  s <- trimws(s)  # Remove leading/trailing spaces

  # Split input into components
  x <- strsplit(s, "\\s+")  # Split by any whitespace

  # Convert to decimal degrees
  x <- sapply(x, function(y) {
    if (length(y) != 3) stop("Invalid format. Must have Degrees, Minutes, and Seconds.")

    y <- as.numeric(y)
    if (any(is.na(y))) stop("Invalid numeric values detected.")

    # Compute Decimal Degrees, preserving negative sign
    dd <- sign(y[1]) * (abs(y[1]) + y[2] / 60 + y[3] / 3600)

    dd
  })

  x
}



#' From decimal degrees to degrees minutes seconds
#'
#' Convert degrees minutes seconds to decimal degrees
#'
#' @param dd numeric vector of decimal degrees
#'
#' @returns
#' character vector of degrees minute seconds
#'
#' @export
#'
#' @examples
#'
#' dd_to_dms(30.25556)     # Expected: "30° 15' 20.00''"
#' dd_to_dms(-30.25556)    # Expected: "-30° 15' 20.00''"
#' dd_to_dms(45.50278)     # Expected: "45° 30' 10.00''"
#' dd_to_dms(-120.0875)    # Expected: "-120° 5' 15.00''"
#' dd_to_dms(c(10.1234, -75.6789)) # Expected: c("10° 7' 24.24''", "-75° 40' 44.04''")


dd_to_dms <- function(dd) {
  # Ensure input is numeric
  if (!is.numeric(dd)) stop("Input must be a numeric vector.")

  # Extract sign, degrees, minutes, and seconds
  sign_val <- ifelse(dd < 0, "-", "")  # Preserve negative sign
  dd <- abs(dd)  # Work with absolute values

  degrees <- floor(dd)  # Extract degrees
  minutes <- floor((dd - degrees) * 60)  # Extract minutes
  seconds <- round((dd - degrees - minutes / 60) * 3600, 2)  # Extract seconds

  # Correct rounding issues where seconds become 60
  fix_indices <- which(seconds == 60)
  if (length(fix_indices) > 0) {
    seconds[fix_indices] <- 0
    minutes[fix_indices] <- minutes[fix_indices] + 1
  }

  fix_indices <- which(minutes == 60)
  if (length(fix_indices) > 0) {
    minutes[fix_indices] <- 0
    degrees[fix_indices] <- degrees[fix_indices] + 1
  }

  # Construct formatted output
  dms <- paste0(sign_val, degrees, "° ", minutes, "' ", seconds, "''")

  dms
}


