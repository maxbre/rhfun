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

ppb_to_ugm3 <- function(ppb, mw, temp = 293.15, press = 1013) {

  # check all inputs are numeric
  if (!is.numeric(ppb) || !is.numeric(mw) || !is.numeric(temp) || !is.numeric(press)) {
    stop("All inputs must be numeric vectors or scalars.")
  }

  # check all values are positive
  if (any(ppb <= 0)) stop("Concentration [ppb] must be greater than zero.")
  if (any(mw <= 0)) stop("Molecular weight must be greater than zero.")
  if (any(temp <= 0)) stop("Temperature must be greater than zero.")
  if (any(press <= 0)) stop("Pressure must be greater than zero.")

  # recycle shorter vectors to match the longest (for fully vectorization)
  max_length <- max(length(ppb), length(mw), length(temp), length(press))
  ppb  <- rep(ppb, length.out = max_length)
  mw    <- rep(mw, length.out = max_length)
  temp  <- rep(temp, length.out = max_length)
  press <- rep(press, length.out = max_length)

  # standard molar volume 22.41 L/mol at 0°C (273.15K) and 1 atm (1013 hPa)
  # adjust the standard molar volume based on temperature and pressure
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

  # check all inputs are numeric
  if (!is.numeric(ugm3) || !is.numeric(mw) || !is.numeric(temp) || !is.numeric(press)) {
    stop("All inputs must be numeric vectors or scalars.")
  }

  # check all values are positive
  if (any(ugm3 <= 0)) stop("Concentration [ugm3] must be greater than zero.")
  if (any(mw <= 0)) stop("Molecular weight must be greater than zero.")
  if (any(temp <= 0)) stop("Temperature must be greater than zero.")
  if (any(press <= 0)) stop("Pressure must be greater than zero.")

  # recycle shorter vectors to match the longest (for fully vectorization)
  max_length <- max(length(ugm3), length(mw), length(temp), length(press))
  ugm3  <- rep(ugm3, length.out = max_length)
  mw    <- rep(mw, length.out = max_length)
  temp  <- rep(temp, length.out = max_length)
  press <- rep(press, length.out = max_length)

  # standard molar volume 22.41 L/mol at 0°C (273.15K) and 1 atm (1013 hPa)
  # adjust the standard molar volume based on temperature and pressure
  Vm <- 22.41 * (temp / 273.15) * (1013 / press)

  # convert µg/m³ to ppb
  ppb <- ugm3 * Vm / mw
  return(ppb)

  }
