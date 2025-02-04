#' ppm to mg/m3
#'
#' convert from ppm to mg/m3
#'
#' @param ppm numeric vector of volume concentrations in parts per billion (10^-6)
#' @param mw molecular weight (g/mol)
#' @param temp temperature in K (default = 293 K)
#' @param press pressure in hPa (default = 1013 hPa)
#'
#' @returns
#' numeric vector of mass concentrations
#' @export
#'
#' @examples
#' NO2 <- 46
#' ppm_to_mgm3(ppm = 100, mw = 46, temp = 293, press = 1013)
#'

ppm_to_mgm3 <- function(ppm, mw, temp = 293.15, press = 1013) {

  # check all inputs are numeric
  if (!is.numeric(ppm) || !is.numeric(mw) || !is.numeric(temp) || !is.numeric(press)) {
    stop("All inputs must be numeric vectors or scalars.")
  }

  # check all values are positive
  if (any(ppm <= 0)) stop("Concentration [ppm] must be greater than zero.")
  if (any(mw <= 0)) stop("Molecular weight must be greater than zero.")
  if (any(temp <= 0)) stop("Temperature must be greater than zero.")
  if (any(press <= 0)) stop("Pressure must be greater than zero.")

  # recycle shorter vectors to match the longest (for fully vectorization)
  max_length <- max(length(ppm), length(mw), length(temp), length(press))
  ppm  <- rep(ppm, length.out = max_length)
  mw    <- rep(mw, length.out = max_length)
  temp  <- rep(temp, length.out = max_length)
  press <- rep(press, length.out = max_length)

  # standard molar volume 22.41 L/mol at 0°C (273.15K) and 1 atm (1013 hPa)
  # adjust the standard molar volume based on temperature and pressure
  Vm <- (22.41 * (temp / 273.15) * (1013 / press))

  # convert ppb to µg/m³
  mgm3 <- ppm * mw / Vm
  return(mgm3)

  }

#' mg/m3 to ppm
#'
#' convert from mg/m3 to ppm
#'
#' @param mgm3 numeric vector of mass concentrations in mg per cubic meter (m^-3)
#' @param mw molecular weight (g/mol)
#' @param temp temperature in K (default = 293 K)
#' @param press pressure in hPa (default = 1013 hPa)
#'
#' @returns
#' numeric vector of volume concentrations in parts per billion (10^-6)
#' @export
#'
#' @examples
#' NO2 <- 46
#' mgm3_to_ppm(mgm3 = 1, mw = 46, temp = 293, press = 1013)


mgm3_to_ppm <- function(mgm3, mw, temp = 293.15, press = 1013) {

  # check all inputs are numeric
  if (!is.numeric(mgm3) || !is.numeric(mw) || !is.numeric(temp) || !is.numeric(press)) {
    stop("All inputs must be numeric vectors or scalars.")
  }

  # check all values are positive
  if (any(mgm3 <= 0)) stop("Concentration [mgm3] must be greater than zero.")
  if (any(mw <= 0)) stop("Molecular weight must be greater than zero.")
  if (any(temp <= 0)) stop("Temperature must be greater than zero.")
  if (any(press <= 0)) stop("Pressure must be greater than zero.")

  # recycle shorter vectors to match the longest (for fully vectorization)
  max_length <- max(length(mgm3), length(mw), length(temp), length(press))
  mgm3  <- rep(mgm3, length.out = max_length)
  mw    <- rep(mw, length.out = max_length)
  temp  <- rep(temp, length.out = max_length)
  press <- rep(press, length.out = max_length)

  # standard molar volume 22.41 L/mol at 0°C (273.15K) and 1 atm (1013 hPa)
  # adjust the standard molar volume based on temperature and pressure
  Vm <- 22.41 * (temp / 273.15) * (1013 / press)

  # convert µg/m³ to ppb
  ppm <- mgm3 * Vm / mw
  return(ppm)

  }

