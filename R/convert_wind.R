#' wind_deg_to_sect
#'
#' convert wind degrees to sectors
#'
#' @param wind_deg numeric vector of wind degrees
#' @return character vector of wind sectors
#' @export
#' @examples
#' wind_deg <- c(0, seq(11.25, (360-11.25), by = 22.5), 360)
#' wind_deg_to_sect(wind_deg)
#'

wind_deg_to_sect <- function(wind_deg) {

  # Check if input is numeric
  if (!is.numeric(wind_deg)) {
    stop("Error: wind_deg must be a numeric value (degrees from 0 to 360).")
  }

  # Initialize output vector
  result <- rep(NA_character_, length(wind_deg))

  # Identify non-NA values
  valid_idx <- !is.na(wind_deg)

  # Normalize valid degrees to the range [0, 360)
  wind_deg[valid_idx] <- wind_deg[valid_idx] %% 360

  # Define the breaks
  breaks <- c(0, seq(11.25, (360 - 11.25), by = 22.5), 360)

  # Define the labels
  labels <- c(
    "N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE",
    "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW", "N"
  )

  # Convert valid degrees to wind sector
  result[valid_idx] <- as.character(cut(
    wind_deg[valid_idx],
    breaks = breaks,
    labels = labels,
    include.lowest = TRUE,
    right = FALSE
  ))

  # Return the result vector
  return(result)
}

#' wind_sect_to_deg
#'
#' convert wind sectors to degrees
#'
#' @param wind_sect character vector of wind sectors
#' @return numeric vector of wind degrees
#' @export
#' @examples
#' wind_sect <- c("N","NNE","NE", "E", "ESE", "SE", "SSE", "S", "W", "WNW", "NW", "NNW")
#' wind_sect_to_deg(wind_sect)
#'

wind_sect_to_deg <- function(wind_sect) {

  # Check if input is missing or NULL
  if (missing(wind_sect) || is.null(wind_sect)) {
    stop("Error: You must provide at least one wind sector abbreviation (e.g., 'N', 'SW').")
  }

  # Define degrees (the center of the corresponding sector)
  deg <- seq(0, 337.5, by = 22.5)

  # Define valid wind sector abbreviations
  sect <- c("N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE",
            "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW")

  # Create a lookup table (named vector)
  lkup_sect <- stats::setNames(deg, sect)

  # Initialize output vector with NA
  result <- rep(NA_real_, length(wind_sect))

  # Identify non-NA values
  valid_idx <- !is.na(wind_sect)

  # Check for invalid values only in non-NA entries
  invalid_inputs <- setdiff(wind_sect[valid_idx], sect)

  if (length(invalid_inputs) > 0) {
    stop(paste("Error: Invalid wind sector(s):", paste(invalid_inputs, collapse = ", "),
               "\nValid sectors are:", paste(sect, collapse = ", ")))
  }

  # Perform lookup only for valid (non-NA) values
  result[valid_idx] <- lkup_sect[wind_sect[valid_idx]]

  # Return the result vector
  return(result)
}
