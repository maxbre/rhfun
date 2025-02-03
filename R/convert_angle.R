#' Angle degrees to radians
#'
#' Convert angles degrees to radians
#'
#' @param deg a numeric vector of angles degrees
#'
#' @return a numeric vector of corresponding radians
#' @export
#'
#' @examples
#' deg <- c(0, 30, 45, 60, 90, 180, 270, 360)
#' deg_to_rad(deg)

deg_to_rad <- function(deg) {deg / 180 * pi}

######################################################

#' Radians to angle degrees
#'
#' Convert radians to angle degrees
#'
#' @param rad a numeric vector of radians
#'
#' @return a numeric vector of corresponding angle degrees
#' @export
#'
#' @examples
#' rad <- c(0, pi/6, pi/4, pi/3, pi/2, pi, 3/2*pi, 2*pi)
#' rad_to_deg(rad)

rad_to_deg <- function(rad) {rad / pi * 180}

######################################################

#' Cartesian x, y to angle degrees
#'
#' Convert Cartesian coordinates x and y to angle degrees
#'
#' By approaching the problem with the use of complex numbers (strange enough but it's easier!)
#' If z = x + i*y with real x and y
#' r = Mod(z) = sqrt(x^2 + y^2)
#' phi = Arg(z)
#' x = r * cos(phi)
#' y = r * sin(phi)
#'
#' @param x a numeric vector of x coordinates in the Cartesian plane
#' @param y a numeric vector of y coordinates in the Cartesian plane
#'
#' @return a numeric vector of corresponding angle degrees (anticlockwise direction)
#' @export
#'
#' @examples
#' x <- c(1, 1, 0, -1, -1, -1,  0,  1)
#' y <- c(0, 1, 1,  1,  0, -1, -1, -1)
#' car_to_deg(x, y)

car_to_deg <- function(x, y) {
  z <- x + 1i * y        # complex representation
  phi <- Arg(z)          # radians
  deg <- rad_to_deg(phi) # convert radians to degrees
  deg %% 360             # %% indicates “modulo”, i.e. integer division
}

######################################################

#' Cartesian x, y to angle degrees from the North (compass direction)
#'
#' Convert Cartesian coordinates x and y to angle degrees from the North direction, i.e. the compass direction
#'
#' By approaching the problem with the use of complex numbers (strange enough but it's easier!)
#' If z = x + i*y with real x and y
#' r = Mod(z) = sqrt(x^2 + y^2)
#' phi = Arg(z)
#' x = r * cos(phi)
#' y = r * sin(phi)
#'
#' @param x a numeric vector of x coordinates in the Cartesian plane
#' @param y a numeric vector of y coordinates in the Cartesian plane
#'
#' @return a numeric vector of corresponding angle degrees direction from the North (compass direction)
#' @export
#'
#' @examples
#' x <- c(1, 1, 0, -1, -1, -1,  0,  1)
#' y <- c(0, 1, 1,  1,  0, -1, -1, -1)
#' car_to_deg_N(x, y)

car_to_deg_N <- function(x,y) {
  z <- x + 1i * y              # complex representation
  phi <- Arg(z)                # radians
  deg <- 450 - rad_to_deg(phi) # shifting by 90 degrees to get the North direction
  deg %% 360                   # %% indicates “modulo”, i.e. integer division
}

######################################################

#' Angle degrees to Cartesian x, y coordinates
#'
#' Convert angle degrees to Cartesian coordinates x and y
#' It is internally also taking care of the conversion from input angles as degrees to radians
#'
#' @param deg a numeric vector of angle degrees
#' @param r numeric, ray of the unitary circle
#' @return a tibble with degrees, phi in radians, x and y
#' @export
#'
#' @examples
#' deg <- c(0, 30, 45, 60, 90, 180, 270, 360)
#' deg_to_car(deg)

deg_to_car <- function(deg, r = 1) {

  phi <- deg_to_rad(deg)
  x <- r * cos(phi)
  y <- r * sin(phi)

  tibble::tibble(deg, phi, x, y)
}
