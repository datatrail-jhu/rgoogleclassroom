#' Create a quiz at a course
#' @param course_id Course id of where to make the new quiz. Can find from end of URL e.g. "https://classroom.google.com/c/COURSE_ID_IS_HERE"
#' @param name Name of new coursework
#' @param full_response Parameter to decide whether to return the full response or just the presentation ID
#' @importFrom httr config accept_json content
#' @importFrom jsonlite fromJSON
#' @export
#'
#' NjA0MDQyMzIzMjM3
create_quiz <- function(course_id) {
  
  create_form() 
  
  create_coursework() 
  
}
