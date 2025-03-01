% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wind_convert.R
\name{wind_ds_to_uv}
\alias{wind_ds_to_uv}
\title{Convert wind direction and wind speed to u and v components}
\usage{
wind_ds_to_uv(d, s)
}
\arguments{
\item{d}{a vector of wind directions}

\item{s}{a vector of wind speeds}
}
\value{
a matrix
}
\description{
This function converts wind direction and wind speed to u and v components
Some specifications about the function code.
Why in the code function there is a negative sign in wind speed?
Because of wind direction convention is given as the direction from which the
wind is coming, measured in degrees clockwise from North (0°).
u and v components represent the wind vector pointing in the direction the wind is blowing.
Wind direction is measured from where the wind originates,
but sine and cosine functions assume an angle that indicates where the wind is going.
Without the negative sign, the computed vector would point in the opposite direction,
which is incorrect.
}
\examples{
set.seed(123)
d <- as.integer(rnorm(10, 180, 90))
set.seed(123)
s <- rnorm(10, 4, 1)
# convert to u and v
wind_ds_to_uv(d, s)
# convert back
uv <- wind_ds_to_uv(d, s)
wind_uv_to_ds(uv[, 1], uv[, 2])

}
\seealso{
\code{\link{wind_uv_to_ds}}
}
\author{
credits to Tim Appelhans
https://raw.githubusercontent.com/environmentalinformatics-marburg/Rsenal/master/R/wdws2uv.R
}
