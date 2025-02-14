#' Convert concentrations v/v or mass/v
#'
#' @description
#' Convert concentrations volume/volume or mass/volume from one unit to another.
#' Input and output measure units are: ppm, ppb, mg/m3, ug/m3, in any sensible (!) direction.
#'
#' @param value numeric vector, concentration value to be converted from to
#' @param mw numeric vector, molecular weight of chemical substance to be converted
#' @param temp numeric vector, temperature
#' @param press numeric vector, pressure
#' @param from character, from measure unit
#' @param to character, to measure unit
#'
#' @returns vector of converted concentrations
#' @export
#'
#' @examples
#' conc_from_to(10, 44.01, from = "ppm", to = "mgm3")
#' conc_from_to(1000, 44.01, from = "ppb", to = "mgm3")
#' conc_from_to(1, 44.01, from = "mgm3", to = "ppm")
#' conc_from_to(1000, 44.01, from = "ugm3", to = "ppm")
#'

conc_from_to <- function(value, mw, temp = 293.15, press = 1013,
                                  from = c("ppm", "ppb", "mgm3", "ugm3"),
                                  to = c("ppm", "ppb", "mgm3", "ugm3")) {

  # Check inputs
  if (!is.numeric(value) || !is.numeric(mw) || !is.numeric(temp) || !is.numeric(press)) {
    stop("All inputs must be numeric vectors or scalars.")
  }
  if (any(value <= 0)) stop("Concentration must be greater than zero.")
  if (any(mw <= 0)) stop("Molecular weight must be greater than zero.")
  if (any(temp <= 0)) stop("Temperature must be greater than zero.")
  if (any(press <= 0)) stop("Pressure must be greater than zero.")

  if (!all(from %in% c("ppm", "ppb", "mgm3", "ugm3"))) {
    stop("Invalid 'from' unit. Must be one of: 'ppm', 'ppb', 'mgm3', 'ugm3'.")
  }
  if (!all(to %in% c("ppm", "ppb", "mgm3", "ugm3"))) {
    stop("Invalid 'to' unit. Must be one of: 'ppm', 'ppb', 'mgm3', 'ugm3'.")
  }

  # Standard molar volume adjustment
  Vm <- 22.41 * (temp / 273.15) * (1013 / press)

  # Conversion factors to and from mg/m³ as a named vector
  conversion_factors <- c(
    ppm  = mw / Vm,
    ppb  = (mw / Vm) * 1000,
    mgm3 = 1,
    ugm3 = 1000
  )

  # Convert input to mg/m³ as pivot
  value_mgm3 <- value * conversion_factors[from]

  # Convert mg/m³ to target unit
  result <- value_mgm3 / conversion_factors[to]

  result
}
