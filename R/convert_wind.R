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

#' convert wind direction and wind speed to u and v components
#'
#' @description
#' this function converts wind direction and wind speed to u and v components
#'
#' Some specifications about the function code.
#' Why in the code function there is a negative sign in ws?
#' Because of Wind Direction Convention:
#' Wind direction (wd) is given as the direction from which the wind is coming, measured in degrees clockwise from North (0°).
#' U and V components represent the wind vector pointing in the direction the wind is blowing.
#' Wind direction is measured from where the wind originates, but sine and cosine functions assume an angle that indicates where the wind is going.
#' Without the negative sign, the computed vector would point in the opposite direction, which is incorrect.
#'
#' @param wd a vector of wind directions
#' @param ws a vector of wind speeds
#'
#' @return
#' a matrix
#'
#' @seealso \code{\link{uv_to_wdws}}
#'
#' @author
#' credits to Tim Appelhans
#' https://raw.githubusercontent.com/environmentalinformatics-marburg/Rsenal/master/R/wdws2uv.R
#'
#' @examples
#' set.seed(123)
#' wd <- as.integer(rnorm(10, 180, 90))
#'
#' set.seed(123)
#' ws <- rnorm(10, 4, 1)
#'
#' ## convert to u and v
#' wdws_to_uv(wd, ws)
#'
#' ## convert back
#' uv <- wdws_to_uv(wd, ws)
#' uv_to_wdws(uv[, 1], uv[, 2])
#'
#' @export

wdws_to_uv <- function(wd, ws) {

  radians <- function(degrees) degrees * pi / 180
  u <- -ws * sin(radians(wd))
  v <- -ws * cos(radians(wd))
  return(cbind(u, v))

}

#' convert u and v components to wind direction and wind speed
#'
#' @description
#' this function converts u and v components to wind direction and wind speed
#'
#' @param u a vector of u components
#' @param v a vector of v components
#'
#' @return
#' a matrix
#'
#' @seealso \code{\link{wdws_to_uv}}
#'
#' @author
#' Credits to Tim Appelhans
#' https://raw.githubusercontent.com/environmentalinformatics-marburg/Rsenal/master/R/uv2wdws.R
#'
#' @examples
#' set.seed(123)
#' wd <- as.integer(rnorm(10, 180, 90))
#'
#' set.seed(123)
#' ws <- rnorm(10, 4, 1)
#'
#' ## convert to u and v
#' wdws_to_uv(wd, ws)
#'
#' ## convert back
#' uv <- wdws_to_uv(wd, ws)
#' uv_to_wdws(uv[, 1], uv[, 2])
#'
#' @export

uv_to_wdws <- function(u,v) {

  degrees <- function(radians) 180 * radians / pi

  mathdegs <- degrees(atan2(v, u))
  wdcalc <- ifelse(mathdegs > 0, mathdegs, mathdegs + 360)
  wd <- ifelse(wdcalc < 270, 270 - wdcalc, 270 - wdcalc + 360)
  ws <- sqrt(u^2 + v^2)

  return(cbind(wd, ws))

}
