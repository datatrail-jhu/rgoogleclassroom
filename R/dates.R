#' Handle and parse a due_date
#' @param date A string that is a date in format of year-month-day
#' @importFrom lubridate day month year ymd
#' @export
#'
#' @examples \dontrun{
#' date_handler("2025-12-1")
#' }
date_handler <- function(due_date) {

  if (is.na(due_date) || is.null(due_date)){
    stop("due_date cannnot be NULL or NA")
  }
  if (is.string(due_date) || lubridate::is.Date(due_date)) {
    due_date <- lubridate::ymd(due_date)
  } else {
    stop("Date is not supplied as string or date, cannot handle it.")
  }

  dates <- list(day = lubridate::day(due_date),
                month = lubridate::month(due_date),
                year = lubridate::year(due_date))

  return(dates)
}

#' Handle and parse a time
#' @param date A string that is a date in format of year-month-day. Default is midnight.
#' @importFrom lubridate hour minute second hms
#' @export
#'
#' @examples \dontrun{
#' time_handler("21:30:59")
#' }
time_handler <- function(due_time = lubridate::hms("23:59:59")) {

  due_time <- lubridate::hms(due_time)

  time <- list(hours = lubridate::hour(due_time),
                minutes = lubridate::minute(due_time),
                seconds = lubridate::second(due_time))

  return(time)
}
