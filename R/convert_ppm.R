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

# old version, less explicit and no error checking
# ppm_to_mgm3 <- function(ppm, mw, temp = 293.15, press = 1013) {
#   mgm3 <- ppm * mw / (22.41 * temp / 273.15 * 1013 / press)
#   mgm3
# }

ppm_to_mgm3 <- function(ppm, mw, temp = 293.15, press = 1013) {

  # validate inputs
  if (!is.numeric(ppm) || !is.numeric(mw) || !is.numeric(temp) || !is.numeric(press)) {
    stop("All inputs must be numeric.")
  }
  if (mw <= 0 || temp <= 0 || press <= 0) {
    stop("Molecular weight, temperature, and pressure must be greater than zero.")
  }

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

# old version, less explicit and no error checking
# mgm3_to_ppm <- function(mgm3, mw, temp = 293.15, press = 1013) {
#   ppm <- mgm3 * (22.41 * temp / 273.15 * 1013 / press) / mw
#   ppm
# }

mgm3_to_ppm <- function(mgm3, mw, temp = 293.15, press = 1013) {

  # validate inputs
  if (!is.numeric(mgm3) || !is.numeric(mw) || !is.numeric(temp) || !is.numeric(press)) {
    stop("All inputs must be numeric.")
  }
  if (mw <= 0 || temp <= 0 || press <= 0) {
    stop("Molecular weight, temperature, and pressure must be greater than zero.")
  }

  # standard molar volume 22.41 L/mol at 0°C (273.15K) and 1 atm (1013 hPa)
  # adjust the standard molar volume based on temperature and pressure
  Vm <- 22.41 * (temp / 273.15) * (1013 / press)

  # convert µg/m³ to ppb
  ppm <- mgm3 * Vm / mw
  return(ppm)

  }

