
date_handler <- function(due_date) {

list(day = lubridate::day(due_date), 
     month = lubridate::month(due_date),
     year = lubridate::year(due_date))
  
}