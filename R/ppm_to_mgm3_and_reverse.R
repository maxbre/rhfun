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
#' NO2 = 46
#' ppm_to_mgm3(ppm = 100,  mw = 46, temp = 293, press = 1013)

ppm_to_mgm3 <- function(ppm, mw, temp = 293.15, press = 1013){

  mgm3 <- ppm * mw / (22.41 * temp / 273.15 * 1013 / press)
  mgm3

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
#' NO2 = 46
#' mgm3_to_ppm(mgm3 = 1,  mw = 46, temp = 293, press = 1013)


mgm3_to_ppm <- function(mgm3, mw, temp = 293.15, press = 1013){

  ppm <- mgm3  * (22.41 * temp / 273.15 * 1013 / press) / mw
  ppm

}

