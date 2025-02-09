#' wind_degr_to_sect
#'
#' convert wind degrees to sectors
#'
#' @param vd a numeric vector of wind dir degrees
#' @return a character vector of wind dir sectors
#' @export
#' @examples
#' vd<-c(0, seq(11.25, (360-11.25), by = 22.5), 360)
#' wind_degr_to_sect(vd)

wind_degr_to_sect<-function(vd){

  # vd as vector of degrees

  #define the breaks
  breaks<-c(0, seq(11.25, (360-11.25), by = 22.5), 360)

  # define the labels
  labels<-c("N",
            "NNE", "NE", "ENE", "E",
            "ESE", "SE", "SSE","S",
            "SSW","SW","WSW","W",
            "WNW","NW", "NNW",
            "N")

  # cut by predefined breaks and return vector of sectors (labels)
  # pay attention here to the inclusion of interval limits

  vd<-cut(vd,
      breaks = breaks,
      labels = labels,
      include.lowest = TRUE,
      right = FALSE)

  as.character(vd)
}

#' wind_sect_to_degr
#'
#' convert wind sectors to degrees
#'
#' @param vs a character vector of wind dir sectors
#' @return a numeric vector of wind dir degrees
#' @export
#' @examples
#' vs<-c("N","NNE","NE", "E", "ESE", "SE", "SSE", "S", "W", "WNW", "NW", "NNW")
#' wind_sect_to_degr(vs)

wind_sect_to_degr<-function(vs){

  # vs vector of sectors

  # degrees (the centre of the corrisponding sector)
  degr<-seq(0, 337.5 , by = 22.5)

  # sectors (pay attention here it is a different vector than in the function degr_to_sect)
  sect<-c("N",
          "NNE","NE", "ENE", "E",
          "ESE", "SE", "SSE", "S",
          "SSW", "SW", "WSW", "W",
          "WNW", "NW", "NNW")

  # define the lookup named vector
  lkup_sect<-stats::setNames(degr, sect)

  # lookup at the sectors
  vs<-lkup_sect[vs]

  # return vector of sectors (characters)
  unname(vs)

}

#' convert wind direction and wind speed to u and v components
#'
#' @description
#' this function converts wind direction and wind speed to u and v components
#'
#' @param wd a vector of wind directions
#' @param ws a vector of wind speeds
#'
#' @return
#' a matrix
#'
#' @seealso \code{\link{uv2wdws}}
#'
#' @author
#' Tim Appelhans
#' https://raw.githubusercontent.com/environmentalinformatics-marburg/Rsenal/master/R/wdws2uv.R
#'
#' @examples
#' set.seed(123)
#' wd <- as.integer(rnorm(10, 180, 90))
#'
#' set.seed(123)
#' ws <- rnorm(10, 4, 1)
#'
#' ## convert to u and v
#' wdws2uv(wd, ws)
#'
#' ## convert back
#' uv <- wdws2uv(wd, ws)
#' uv2wdws(uv[, 1], uv[, 2])
#'
#' @export wdws2uv
#' @aliases wdws2uv

wdws2uv <- function(wd, ws) {

  radians <- function(degrees) degrees * pi / 180
  u <- -ws * sin(radians(wd))
  v <- -ws * cos(radians(wd))
  return(cbind(u, v))

}

#' convert u and v components to wind direction and wind speed
#'
#' @description
#' this function converts u and v components to wind direction and wind speed
#'
#' @param u a vector of u components
#' @param v a vector of v components
#'
#' @return
#' a matrix
#'
#' @seealso \code{\link{wdws2uv}}
#'
#' @author
#' Tim Appelhans
#' https://raw.githubusercontent.com/environmentalinformatics-marburg/Rsenal/master/R/uv2wdws.R
#'
#' @examples
#' set.seed(123)
#' wd <- as.integer(rnorm(10, 180, 90))
#'
#' set.seed(123)
#' ws <- rnorm(10, 4, 1)
#'
#' ## convert to u and v
#' wdws2uv(wd, ws)
#'
#' ## convert back
#' uv <- wdws2uv(wd, ws)
#' uv2wdws(uv[, 1], uv[, 2])
#'
#' @export uv2wdws
#' @aliases uv2wdws

uv2wdws <- function(u,v) {

  degrees <- function(radians) 180 * radians / pi

  mathdegs <- degrees(atan2(v, u))
  wdcalc <- ifelse(mathdegs > 0, mathdegs, mathdegs + 360)
  wd <- ifelse(wdcalc < 270, 270 - wdcalc, 270 - wdcalc + 360)
  ws <- sqrt(u^2 + v^2)

  return(cbind(wd, ws))

}
