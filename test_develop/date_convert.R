#' From date to year-day
#'
#' Convert date in the form "YYY-MM-DD" to year-day (a.k.a. julian-day) in the form "YYYY-YDAY"
#'
#' @param mydate string in the form "YYY-MM-DD"representing input date
#' @return string in the form "YYYY-YDAY" representing output year-day (a.k.a. julian-day)
#' @export
#' @examples
#' date_to_yday("2022-05-30")

date_to_yday <- function(mydate){

  mydate <- lubridate::ymd(mydate)
  y <- lubridate::year(mydate)
  yd <- sprintf("%03d", lubridate::yday(mydate))
  yyd <- paste0(y, "-", yd)
  yyd

}

#' From year-day to date
#'
#' Convert year-day (a.k.a. julian-day) in the form "YYYY-YDAY" to date in the form "YYY-MM-DD"
#'
#' @param yday string  in the form "YYYY-YDAY" representing input year-day (a.k.a. julian-day)
#' @return string in the form "YYY-MM-DD"representing output date
#' @export
#' @examples
#' yday_to_date("2022-150")

yday_to_date <- function (yday){

  # mydate <- strsplit(yday, "-")
  # y <- mydate[[1]][1]
  # yd <- as.numeric(mydate[[1]][2])
  # mydate <- lubridate::ymd(paste(y, "01", "01", sep="-"))
  # lubridate::yday(mydate) <- yd
  # mydate

  m <- stringr::str_split(yday, "-", simplify = TRUE)
  mydate <- lubridate::ymd(paste(m[,1], "01", "01", sep="-"))
  lubridate::yday(mydate) <- as.numeric(m[,2])
  mydate

}

