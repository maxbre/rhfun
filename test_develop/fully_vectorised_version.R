ugm3_to_ppb_new <- function(ugm3, mw, temp = 293.15, press = 1013) {
  # Ensure all inputs are numeric
  if (!is.numeric(ugm3) || !is.numeric(mw) || !is.numeric(temp) || !is.numeric(press)) {
    stop("All inputs must be numeric vectors or scalars.")
  }
  
  # Ensure all values are positive
  if (any(mw <= 0)) stop("Molecular weight must be greater than zero.")
  if (any(temp <= 0)) stop("Temperature must be greater than zero.")
  if (any(press <= 0)) stop("Pressure must be greater than zero.")
  
  # Recycle shorter vectors to match the longest vector length
  max_length <- max(length(ugm3), length(mw), length(temp), length(press))
  ugm3  <- rep(ugm3, length.out = max_length)
  mw    <- rep(mw, length.out = max_length)
  temp  <- rep(temp, length.out = max_length)
  press <- rep(press, length.out = max_length)
  
  # Compute molar volume dynamically for each element
  Vm <- 22.41 * (temp / 273.15) * (1013 / press)
  
  # Convert µg/m³ to ppb (fully vectorized)
  ppb <- ugm3 * Vm / mw
  return(ppb)
}
