#' Convert spatial coordinates
#'
#' Conversion from one CRS to another by setting the EPSG codes
#'
#' @param x vector of x/lon coordinates
#' @param y vector of y/lat coordinates
#' @param epsg_in number corresponding to EPSG code of initial CRS, default to UTM32, no error check implemented
#' @param epsg_out number corresponding to EPSG code of converted CRS, default to WGS84, no error check implemented
#' @return a dataframe of converted x/lon and y/lat coordinates with indication of final EPSG code
#' @export
#' @examples
#' coord_convert(738239.9, 5073656)
#' coord_convert(738239.9, 5073656, epsg_in = 32632, epsg_out = 32633)
#'

coord_convert <- function(x, y, epsg_in = 32632, epsg_out = 4326) {

  # this is using 'sf' package
  # no error check about epsg

  # Ensure inputs are numeric
  if (!is.numeric(x) || !is.numeric(y)) {
    stop("Inputs 'x' and 'y' must be numeric vectors.")
  }

  # Create an sf object with input CRS
  points_sf <- sf::st_as_sf(data.frame(x = x, y = y), coords = c("x", "y"), crs = epsg_in)

  # Transform to target CRS
  transformed_sf <- sf::st_transform(points_sf, epsg_out)

  # Extract transformed coordinates
  coords <- sf::st_coordinates(transformed_sf)

  # Return as a data frame with explicit column names
  data.frame(lon = coords[,1], lat = coords[,2], crs_epsg = epsg_out)

}


# coord_convert <- function(x, y, epsg_in = 32632, epsg_out = 4326) {
#
#   # this is using 'terra' package
#
#   # Validate inputs
#   if (!is.numeric(x) || !is.numeric(y)) {
#     stop("Inputs 'x' and 'y' must be numeric vectors.")
#   }
#
#   # Create SpatVector with numeric EPSG codes
#   p <- terra::vect(cbind(x, y), crs = epsg_in)
#
#   # Transform coordinates to the target CRS
#   vp <- terra::project(p, y = epsg_out)
#
#   # Extract transformed coordinates
#   coords <- terra::crds(vp)
#
#   # Return as a data frame with explicit column names
#   data.frame(lon = coords[,1], lat = coords[,2], crs_epsg = epsg_out)
#
# }
