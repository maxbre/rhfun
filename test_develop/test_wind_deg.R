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

wind_deg_to_sect <- function(wind_deg) {

  #define the breaks
  breaks <- c(0, seq(11.25, (360 - 11.25), by = 22.5), 360)

  # define the labels
  labels <- c(
    "N",
    "NNE",
    "NE",
    "ENE",
    "E",
    "ESE",
    "SE",
    "SSE",
    "S",
    "SSW",
    "SW",
    "WSW",
    "W",
    "WNW",
    "NW",
    "NNW",
    "N"
  )

  # cut by predefined breaks and return vector of sectors (labels)
  # pay attention here to the inclusion of interval limits

  sect <- cut(
    wind_deg,
    breaks = breaks,
    labels = labels,
    include.lowest = TRUE,
    right = FALSE
  )

  as.character(sect)
}

# improved version

wind_deg_to_sect <- function(wind_deg) {

  # Check for missing values (NA)
  if (any(is.na(wind_deg))) {
    warning("Warning: Input contains NA values. Returning NA for those cases.")
    return(rep(NA_character_, length(wind_deg)))
  }

  # Check if input is numeric
  if (!is.numeric(wind_deg)) {
    stop("Error: wind_deg must be a numeric value (degrees from 0 to 360).")
  }

  # Normalize degrees to the range [0, 360)
  wind_deg <- wind_deg %% 360

  # Define the breaks
  breaks <- c(0, seq(11.25, (360 - 11.25), by = 22.5), 360)

  # Define the labels
  labels <- c(
    "N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE",
    "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW", "N"
  )

  # Convert degrees to wind sector
  sect <- cut(
    wind_deg,
    breaks = breaks,
    labels = labels,
    include.lowest = TRUE,
    right = FALSE
  )

  # Return as character vector
  return(as.character(sect))
}


# imporved version better

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


################################################

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

wind_sect_to_deg <- function(wind_sect) {

  # degrees (the centre of the corresponding sector)
  deg <- seq(0, 337.5 , by = 22.5)

  # sectors (pay attention here, it is a different vector than in the function deg_to_sect)
  sect <- c(
    "N",
    "NNE",
    "NE",
    "ENE",
    "E",
    "ESE",
    "SE",
    "SSE",
    "S",
    "SSW",
    "SW",
    "WSW",
    "W",
    "WNW",
    "NW",
    "NNW"
  )

  # define the lookup named vector
  lkup_sect <- stats::setNames(deg, sect)

  # lookup at the sectors
  deg_ <- lkup_sect[wind_sect]

  # return vector of degrees
  unname(deg_)

}



# improved version

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

  # Check if input contains invalid values
  invalid_inputs <- setdiff(wind_sect, sect)
  if (length(invalid_inputs) > 0) {
    stop(paste("Error: Invalid wind sector(s):", paste(invalid_inputs, collapse = ", "),
               "\nValid sectors are:", paste(sect, collapse = ", ")))
  }

  # Lookup sector(s) and return the corresponding degrees
  deg_ <- lkup_sect[wind_sect]

  return(unname(deg_))
}


# Valid inputs
wind_sect_to_deg("N")     # Returns: 0
wind_sect_to_deg("SW")    # Returns: 225
wind_sect_to_deg(c("N", "ESE", "WNW"))
# Returns: 0 112.5 292.5

# Missing input
wind_sect_to_deg()
# Error: You must provide at least one wind sector abbreviation (e.g., 'N', 'SW').

# Invalid input
wind_sect_to_deg("XYZ")
# Error: Invalid wind sector(s): XYZ
# Valid sectors are: N, NNE, NE, ENE, E, ESE, SE, SSE, S, SSW, SW, WSW, W, WNW, NW, NNW

# Mixed valid and invalid inputs
wind_sect_to_deg(c("N", "XYZ", "SE"))
# Error: Invalid wind sector(s): XYZ
# Valid sectors are: N, NNE, NE, ENE, E, ESE, SE, SSE, S, SSW, SW, WSW, W, WNW, NW, NNW

wind_sect_to_deg(c("N", "XYZ", "SE", NA))


# imporved version better!

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

wind_sect_to_deg(c("N", "SW", NA, "ENE"))

