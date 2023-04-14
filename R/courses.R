#' Create a new course
#' @param title Title of the course
#' @param full_response Parameter to decide whether to return the full response or just the presentation ID
#' @importFrom httr config accept_json content
#' @importFrom jsonlite fromJSON
#' @export
create_course <- function(title = NULL, full_response = FALSE){

  # Get endpoint url
  url <- get_endpoint("classroom.endpoint.create")

  # Get auth token
  token <- get_token()
  config <- httr::config(token=token)

  # Wrapping body parameters in a requests list
  body_params <- list(title = title)

  # Modify course
  result <- httr::POST(url, config = config, accept_json(), body = body_params, encode = "json")

  if(httr::status_code(result) != 200){
    message("Cannot create course")
    httr::stop_for_status(result)
  }
  # Process and return results
  result_content <- content(result, "text")
  result_list <- fromJSON(result_content)

  # If user request for minimal response
  if(full_response){
    return(result_list)
  } else {
    return(result_list$Id)
  }
}


#' Get Google Classroom Course Properties
#' @param id ID of the course
#' @importFrom httr config accept_json content
#' @importFrom jsonlite fromJSON
#' @importFrom assertthat assert_that is.string
#' @export
get_course_properties <- function(course_id){
  # Check validity of inputs
  assert_that(is.string(course_id))

  # Get endpoint url
  url <- get_endpoint("classroom.endpoint.get", course_id)

  # Get auth token
  token <- get_token()
  config <- httr::config(token=token)

  # Get course properties
  result <- httr::GET(url, config = config, accept_json())

  if(httr::status_code(result) != 200){
    message("ID provided does not point towards any course")
    httr::stop_for_status(result)
  }

  # Process and return results
  result_content <- content(result, "text")
  result_list <- fromJSON(result_content)
  return(result_list)
}
