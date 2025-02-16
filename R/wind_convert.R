#' Convert wind degrees to wind sectors
#'
#' @description
#' Converts wind degrees (from North) to corresponding wind sectors.
#' By default, considers 16 wind sectors, each spanning 22.5 degrees.
#' The North sector is centred on 0/360 degrees and spans from 348.75 to 11.25.
#'
#' @param wind_degrees A numeric vector of wind degrees (0-360).
#' @return A character vector of wind sectors. Out-of-range values become `NA`.
#' @export
#' @examples
#' wind_degr_to_sect(c(0, 45, 90, 180, 270, 360))
#'

wind_degr_to_sect <- function(wind_degrees) {

  # Input validation
  if (!is.vector(wind_degrees) || !is.numeric(wind_degrees)) {
    stop("wind_degrees must be a numeric vector.")
  }

  wind_sect <- rep(NA_character_, length(wind_degrees))

  # Identify valid indices (0-360 inclusive)
  valid_idx <- !is.na(wind_degrees) & wind_degrees >= 0 & wind_degrees <= 360

  # Warn about out-of-range values
  out_of_range <- wind_degrees[!is.na(wind_degrees) & (wind_degrees < 0 | wind_degrees > 360)]
  if (length(out_of_range) > 0) {
    warning(
      "Out-of-range degree(s) replaced with NA: ",
      paste(round(out_of_range, 2), collapse = ", ")
    )
  }

  # Define sector breaks and labels
  breaks <- c(0, seq(11.25, 348.75, by = 22.5), 360)
  labels <- c("N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE",
              "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW", "N")

  # Assign sectors to valid inputs
  wind_sect[valid_idx] <- as.character(
    cut(wind_degrees[valid_idx],
        breaks = breaks,
        labels = labels,
        include.lowest = TRUE,
        right = FALSE)
  )

  wind_sect
}


#' Convert wind sectors to wind degrees
#'
#' @description
#' Converts wind sector names to their corresponding central wind degrees (from North).
#' Uses 16 sectors centred at 0°, 22.5°, 45°, ..., 337.5°.
#'
#' @param wind_sectors A character vector of wind sector names (e.g., "N", "NE").
#' @return A numeric vector of wind degrees. Invalid sectors become `NA`.
#' @export
#' @examples
#' wind_sect_to_degr(c("N", "NE", "E", "S", "W", "NNW"))
#'

wind_sect_to_degr <- function(wind_sectors) {

  # Input validation
  if (!is.vector(wind_sectors) || !is.character(wind_sectors)) {
    stop("wind_sectors must be a character vector.")
  }

  # Valid sector names and their central degrees
  sect <- c("N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE",
            "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW")
  lkup_sect <- stats::setNames(seq(0, 337.5, by = 22.5), sect)

  wind_degr <- rep(NA_real_, length(wind_sectors))

  # Identify invalid sectors
  invalid <- !is.na(wind_sectors) & !(wind_sectors %in% sect)
  if (any(invalid)) {
    warning(
      "Invalid sector(s) replaced with NA: ",
      paste(unique(wind_sectors[invalid]), collapse = ", ")
    )
  }

  # Convert valid sectors to degrees
  valid_entries <- wind_sectors %in% sect & !is.na(wind_sectors)
  wind_degr[valid_entries] <- lkup_sect[wind_sectors[valid_entries]]

  wind_degr
}


#' Convert wind direction and wind speed to u and v components
#'
#' @description
#' This function converts wind direction and wind speed to u and v components
#' Some specifications about the function code.
#' Why in the code function there is a negative sign in wind speed?
#' Because of wind direction convention is given as the direction from which the
#' wind is coming, measured in degrees clockwise from North (0°).
#' u and v components represent the wind vector pointing in the direction the wind is blowing.
#' Wind direction is measured from where the wind originates,
#' but sine and cosine functions assume an angle that indicates where the wind is going.
#' Without the negative sign, the computed vector would point in the opposite direction,
#' which is incorrect.
#'
#' @param d a vector of wind directions
#' @param s a vector of wind speeds
#'
#' @return
#' a matrix
#'
#' @seealso \code{\link{wind_uv_to_ds}}
#'
#' @author
#' credits to Tim Appelhans
#' https://raw.githubusercontent.com/environmentalinformatics-marburg/Rsenal/master/R/wdws2uv.R
#'
#' @examples
#' set.seed(123)
#' d <- as.integer(rnorm(10, 180, 90))
#' set.seed(123)
#' s <- rnorm(10, 4, 1)
#' # convert to u and v
#' wind_ds_to_uv(d, s)
#' # convert back
#' uv <- wind_ds_to_uv(d, s)
#' wind_uv_to_ds(uv[, 1], uv[, 2])
#'
#' @export

wind_ds_to_uv <- function(d, s) {

  # Input validation
  if (!is.vector(d) || !is.numeric(d)) {
    stop("wind direction must be a numeric vector.")
  }

  if (!is.vector(s) || !is.numeric(s)) {
    stop("wind speed must be a numeric vector.")
  }

  radians <- function(degrees) degrees * pi / 180

  u <- -s * sin(radians(d))
  v <- -s * cos(radians(d))

  return(cbind(u, v))

}

#' Convert u and v components to wind direction and wind speed
#'
#' @description
#' This function converts u and v components to wind direction and wind speed
#'
#' @param u a vector of u components
#' @param v a vector of v components
#'
#' @return
#' a matrix
#'
#' @seealso \code{\link{wind_ds_to_uv}}
#'
#' @author
#' Credits to Tim Appelhans
#' https://raw.githubusercontent.com/environmentalinformatics-marburg/Rsenal/master/R/uv2wdws.R
#'
#' @examples
#' set.seed(123)
#' d <- as.integer(rnorm(10, 180, 90))
#' set.seed(123)
#' s <- rnorm(10, 4, 1)
#' ## convert to u and v
#' wind_ds_to_uv(d, s)
#' ## convert back
#' uv <- wind_ds_to_uv(d, s)
#' wind_uv_to_ds(uv[, 1], uv[, 2])
#'
#' @export

wind_uv_to_ds <- function(u, v) {

  if (!is.vector(u) || !is.numeric(u)) {
    stop("u must be a numeric vector.")
  }

  if (!is.vector(v) || !is.numeric(v)) {
    stop("v must be a numeric vector.")
  }

  degrees <- function(radians) 180 * radians / pi

  mathdegs <- degrees(atan2(v, u))
  wdcalc <- ifelse(mathdegs > 0, mathdegs, mathdegs + 360)
  d <- ifelse(wdcalc < 270, 270 - wdcalc, 270 - wdcalc + 360)
  s <- sqrt(u^2 + v^2)

  return(cbind(d, s))

}
