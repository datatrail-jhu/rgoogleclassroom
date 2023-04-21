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
  # Get endpoint url
  url <- get_endpoint("form.endpoint")
  
  # Get auth token
  token <- get_token()
  config <- httr::config(token = token)
  
  # Wrapping body parameters in a requests list
  body_params <- list(
    topic_id = topic_id,
    name = name,
    workType = work_type,
    dueDate = due_date,
    description = description
  )
  
  # Modify course
  result <- httr::POST(url, config = config, accept_json(), body = body_params, encode = "json")
  
  if (httr::status_code(result) != 200) {
    message("Cannot create form")
    httr::stop_for_status(result)
  }
  # Process and return results
  result_content <- content(result, "text")
  result_list <- fromJSON(result_content)
  
  message(paste("Form created at", result_list$alternateLink))
  
  # If user request for minimal response
  if (full_response) {
    return(result_list)
  } else {
    return(result_list$Id)
  }
}
