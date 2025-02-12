#' Temperature conversion utility
#'
#' Convert between Celsius, Fahrenheit, and Kelvin seamlessly (hopefully!)
#'
#' @param temp numeric vector of temperature
#' @param from character scale "from"
#' @param to character scale "to
#' @return vector of converted temperature
#' @export
#' @examples
#' temp_from_to(100, "C", "F")
#' temp_from_to(212, "F", "C")
#' temp_from_to(300, "K", "C")
#' temp_from_to(100, "C", "K")
#' temp_from_to(98.6, "F", "K")
#' temp_from_to(273.15, "K", "F")
#' temp_from_to(0, "C", "C")
#'

temp_from_to <- function(temp, from = "C", to = "K") {

  # make sure input is numeric
  if (!is.numeric(temp)) {
    stop("Error: Temperature must be numeric.")
  }

  # Convert input to uppercase
  from <- toupper(from)
  to <- toupper(to)

  # Define valid temperature scales
  valid_scales <- c("C", "F", "K")

  # Check if both 'from' and 'to' are valid
  if (!(from %in% valid_scales) | !(to %in% valid_scales)) {
    stop("Error: Invalid temperature scale. Use 'C' for Celsius, 'F' for Fahrenheit, or 'K' for Kelvin.")
  }

  # No conversion needed
  if (from == to) {
    return(temp)
  }

  # Convert 'from' to Celsius as pivot
  temp_C <- switch(from,
                   "C" = temp,
                   "F" = (temp - 32) * 5 / 9,
                   "K" = temp - 273.15)

  # Convert Celsius to 'to' scale
  result <- switch(to,
                   "C" = temp_C,
                   "F" = (temp_C * 9 / 5) + 32,
                   "K" = temp_C + 273.15)

  return(result)
}
