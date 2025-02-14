#' Angle degrees to radians
#'
#' @description
#' Convert angles degrees to radians
#'
#' @param deg numeric vector of angles degrees
#' @return numeric vector of corresponding radians
#' @export
#' @examples
#' deg <- c(0, 30, 45, 60, 90, 180, 270, 360)
#' deg_to_rad(deg)
#' deg_to_rad(180)   # Expected: π (≈ 3.1416)
#' deg_to_rad(90)    # Expected: π/2 (≈ 1.5708)
#' deg_to_rad(45)    # Expected: π/4 (≈ 0.7854)
#' deg_to_rad(360)   # Expected: 2π (≈ 6.2832)

deg_to_rad <- function(deg) {

  # Ensure deg is numeric
  if (!is.numeric(deg)) {
    stop("Error: Input must be numeric.")
  }

  # Convert degrees to radians
  radians <- deg * (pi / 180)

  return(radians)
}


#' Radians to angle degrees
#'
#' @description
#' Convert radians to angle degrees
#'
#' @param rad numeric vector of radians
#' @return numeric vector of corresponding angle degrees
#' @export
#' @examples
#' rad <- c(0, pi/6, pi/4, pi/3, pi/2, pi, 3/2*pi, 2*pi)
#' rad_to_deg(rad)
#' rad_to_deg(pi)     # Expected: 180
#' rad_to_deg(pi / 2) # Expected: 90
#' rad_to_deg(pi / 4) # Expected: 45
#' rad_to_deg(2 * pi) # Expected: 360

rad_to_deg <- function(rad) {

  # Ensure rad is numeric
  if (!is.numeric(rad)) {
    stop("Error: Input must be numeric.")
  }

  # Convert radians to degrees
  degrees <- rad * (180 / pi)

  return(degrees)
}


#' Cartesian x, y to angle degrees
#'
#'@description
#'Convert cartesian coordinates x and y to angle degrees.
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
#' @return a numeric vector of corresponding angle degrees (anticlockwise direction)
#' @export
#' @examples
#' x <- c(1, 1, 0, -1, -1, -1,  0,  1)
#' y <- c(0, 1, 1,  1,  0, -1, -1, -1)
#' car_to_deg(x, y)
#' car_to_deg(1, 1)   # Expected: 45 degrees
#' car_to_deg(0, 1)   # Expected: 90 degrees
#' car_to_deg(-1, 1)  # Expected: 135 degrees
#' car_to_deg(-1, 0)  # Expected: 180 degrees
#' car_to_deg(-1, -1) # Expected: 225 degrees
#' car_to_deg(0, -1)  # Expected: 270 degrees
#' car_to_deg(1, -1)  # Expected: 315 degrees
#' car_to_deg(1, 0)   # Expected: 0 degrees

car_to_deg <- function(x, y) {

  # Ensure x and y are numeric
  if (!is.numeric(x) | !is.numeric(y)) {
    stop("Error: Both x and y must be numeric.")
  }

  # Convert to complex number
  z <- complex(real = x, imaginary = y)

  # Compute argument (angle in radians)
  phi <- Arg(z)

  # Convert radians to degrees
  deg <- phi * (180 / pi)

  # Ensure the angle is between 0 and 360 degrees
  deg <- (deg + 360) %% 360

  deg
}


#' Angle degrees to Cartesian x, y coordinates
#'
#'@description
#' Convert angle degrees to cartesian coordinates x and y.
#' It is internally also taking care of the conversion from input angles as degrees to radians
#'
#' @param deg a numeric vector of angle degrees
#' @param r numeric, ray of the unitary circle
#' @return a tibble with degrees, phi in radians, x and y
#' @export
#' @examples
#' deg <- c(0, 30, 45, 60, 90, 180, 270, 360)
#' deg_to_car(deg)
#' deg_to_car(0)      # Expected: (1, 0)
#' deg_to_car(90)     # Expected: (0, 1)
#' deg_to_car(180)    # Expected: (-1, 0)
#' deg_to_car(270)    # Expected: (0, -1)
#' deg_to_car(45, 2)  # Expected: (√2, √2)

deg_to_car <- function(deg, r = 1) {

  # Ensure deg and r are numeric
  if (!is.numeric(deg) | !is.numeric(r)) {
    stop("Error: Both deg and r must be numeric.")
  }

  # Convert degrees to radians
  phi <- deg * (pi / 180)

  # Compute Cartesian coordinates
  x <- r * cos(phi)
  y <- r * sin(phi)

  # Return a tibble with results
  tibble::tibble(deg = deg, phi = phi, x = x, y = y)
}


#' Cartesian x, y to angle degrees from the North (compass direction)
#'
#'@description
#'Convert cartesian coordinates x and y to angle degrees from the North direction, i.e. the compass direction.
#' By approaching the problem with the use of complex numbers (strange enough but it's easier!)
#' If z = x + i*y with real x and y
#' r = Mod(z) = sqrt(x^2 + y^2)
#' phi = Arg(z)
#' x = r * cos(phi)
#' y = r * sin(phi)
#'
#' @param x a numeric vector of x coordinates in the Cartesian plane
#' @param y a numeric vector of y coordinates in the Cartesian plane
#' @return a numeric vector of corresponding angle degrees direction from the North (compass direction)
#' @export
#' @examples
#' x <- c(1, 1, 0, -1, -1, -1,  0,  1)
#' y <- c(0, 1, 1,  1,  0, -1, -1, -1)
#' car_to_deg_N(x, y)
#' car_to_deg_N(1, 1)   # Expected: 315 degrees (North = 0, East = 90)
#' car_to_deg_N(0, 1)   # Expected: 0 degrees (North)
#' car_to_deg_N(-1, 1)  # Expected: 45 degrees (North-East)
#' car_to_deg_N(-1, 0)  # Expected: 90 degrees (East)
#' car_to_deg_N(-1, -1) # Expected: 135 degrees (South-East)
#' car_to_deg_N(0, -1)  # Expected: 180 degrees (South)
#' car_to_deg_N(1, -1)  # Expected: 225 degrees (South-West)
#' car_to_deg_N(1, 0)   # Expected: 270 degrees (West)

car_to_deg_N <- function(x, y) {

  # Ensure x and y are numeric
  if (!is.numeric(x) | !is.numeric(y)) {
    stop("Error: Both x and y must be numeric.")
  }

  # Convert to complex number
  z <- complex(real = x, imaginary = y)

  # Compute argument (angle in radians)
  phi <- Arg(z)

  # Convert radians to degrees
  deg <- 450 - (phi * (180 / pi))  # Shift by 90° to set North as 0°

  # Ensure the angle is between 0 and 360 degrees
  deg <- deg %% 360

  deg
}


#' From angles degrees of North (compass direction) to Cartesian x, y
#'
#'@description
#' Convert from the North direction in angles degrees (i.e. the compass direction) to the cartesian coordinates x and y
#' The function is using r as input parameter. What is it? It defines how far the point is from the origin (0,0)
#' If r = 1 (default), the function converts only the direction (angle) into Cartesian coordinates.
#' If r > 1, it scales the coordinates proportionally.
#' If r < 1, it shrinks the coordinates proportionally.
#' If r = 0, it always returns (0,0), regardless of deg.
#' @param deg vector of degrees North
#' @param r the radius or magnitude of the vector in the Cartesian coordinate system.
#' @returns a tibble with degrees, phi in radians, x and y
#' @export
#' @examples
#' deg_N_to_car(0)     # Expected: (0, 1) -> North
#' deg_N_to_car(90)    # Expected: (1, 0) -> East
#' deg_N_to_car(180)   # Expected: (0, -1) -> South
#' deg_N_to_car(270)   # Expected: (-1, 0) -> West
#' deg_N_to_car(45, 2) # Expected: (√2, √2) -> North-East

deg_N_to_car <- function(deg, r = 1) {

  # Ensure deg and r are numeric
  if (!is.numeric(deg) | !is.numeric(r)) {
    stop("Error: Both deg and r must be numeric.")
  }

  # Convert degrees North to radians (shifting by -90°)
  phi <- (deg - 90) * (pi / 180)

  # Compute Cartesian coordinates
  x <- r * cos(phi)
  y <- r * sin(phi)

  # Return a tibble with results
  tibble::tibble(deg_N = deg, phi_rad = phi, x = x, y = y)

}
