#' ppb to µg/m3
#'
#' convert from ppb to µg/m3
#'
#' @param ppb numeric vector of volume concentrations in parts per billion (10^-9)
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
#' ppb_to_ugm3(ppb = 100, mw = 46, temp = 293, press = 1013)

# old version, less explicit and no error checking
# #ppb_to_ugm3 <- function(ppb, mw, temp = 293.15, press = 1013) {
#   ugm3 <- ppb * mw / (22.41 * temp / 273.15 * 1013 / press)
#   ugm3
# }

ppb_to_ugm3 <- function(ppb, mw, temp = 293.15, press = 1013) {

  # validate inputs
  if (!is.numeric(ppb) || !is.numeric(mw) || !is.numeric(temp) || !is.numeric(press)) {
    stop("All inputs must be numeric.")
  }
  if (mw <= 0 || temp <= 0 || press <= 0) {
    stop("Molecular weight, temperature, and pressure must be greater than zero.")
  }

  # calculate molar volume dynamically (Ideal Gas Law at given temp & pressure)
  Vm <- (22.41 * (temp / 273.15) * (1013 / press))

  # convert ppb to µg/m³
  ugm3 <- ppb * mw / Vm
  return(ugm3)

  }


#' µg/m3 to ppb
#'
#' convert from µg/m3 to ppb
#'
#' @param ugm3 numeric vector of mass concentrations in ug per cubic meter (m^-3)
#' @param mw molecular weight (g/mol)
#' @param temp temperature in K (default = 293 K)
#' @param press pressure in hPa (default = 1013 hPa)
#'
#' @returns
#' numeric vector of volume concentrations in parts per billion (10^-9)
#' @export
#'
#' @examples
#' NO2 <- 46
#' ugm3_to_ppb(ugm3 = 1, mw = 46, temp = 293, press = 1013)

# old version, less explicit and no error checking
# ugm3_to_ppb <- function(ugm3, mw, temp = 293.15, press = 1013) {
#   ppb <- ugm3 * (22.41 * temp / 273.15 * 1013 / press) / mw
#   ppb
# }

ugm3_to_ppb <- function(ugm3, mw, temp = 293.15, press = 1013) {

  # validate inputs
  if (!is.numeric(ugm3) || !is.numeric(mw) || !is.numeric(temp) || !is.numeric(press)) {
    stop("All inputs must be numeric.")
  }
  if (mw <= 0 || temp <= 0 || press <= 0) {
    stop("Molecular weight, temperature, and pressure must be greater than zero.")
  }

  # compute molar volume dynamically
  Vm <- 22.41 * (temp / 273.15) * (1013 / press)

  # convert µg/m³ to ppb
  ppb <- ugm3 * Vm / mw
  return(ppb)
}
