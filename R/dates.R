#' Handle and parse a due_date
#' @param date A date that is year-month-day
#' @importFrom lubridate day month year ymd
#' @export
#'
#' NjA0MDQyMzIzMjM3
date_handler <- function(due_date) {

  due_date <- lubridate::ymd(due_date)

  dates <- list(day = lubridate::day(due_date),
                month = lubridate::month(due_date),
                year = lubridate::year(due_date))

  return(dates)
}

time_handler <- function(due_time = lubridate::hms("24:00:00")) {

  due_time <- lubridate::hms(due_time)

  time <- list(hours = lubridate::hour(due_time),
                minutes = lubridate::minute(due_time),
                seconds = lubridate::second(due_time))

  return(time)
}
