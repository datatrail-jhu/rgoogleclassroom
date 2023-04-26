#' Get list of courses
#' @param owner_id owner_id to retrieve course listings from
#' @importFrom httr config accept_json content
#' @importFrom jsonlite fromJSON
#' @importFrom assertthat assert_that is.string
#' @export
get_course_list <- function(owner_id = NULL) {

  if (is.null(owner_id)) {
    stop("Need to provide owner_id from which to retrieve a list of courses from")
  }

  # Get endpoint url
  url <- get_endpoint("classroom.endpoint.course.get")

  # Get auth token
  token <- get_token()
  config <- httr::config(token = token)

  # Get course properties
  result <- httr::GET(url, config = config, accept_json())

  if (httr::status_code(result) != 200) {
    warning("No courses found")
    return(result_list)
  }

  # Process and return results
  result_content <- content(result, "text")
  result_list <- fromJSON(result_content)
  return(result_list)
}


#' Create a new course
#' @param owner_id The ownerId to use to create the course. Will attempt to retrieve ownerId based on credentials with get_owner_id()
#' @param name Name of the new course
#' @param full_response Parameter to decide whether to return the full response or just the presentation ID
#' @importFrom httr config accept_json content
#' @importFrom jsonlite fromJSON
#' @export
create_course <- function(owner_id = get_owner_id()$id, name = NULL, full_response = FALSE) {
  # Get endpoint url
  url <- get_endpoint("classroom.endpoint.course.get")

  # Get auth token
  token <- get_token()
  config <- httr::config(token = token)

  # Wrapping body parameters in a requests list
  body_params <- list(
    ownerId = owner_id,
    name = name
  )

  # Modify course
  result <- httr::POST(url, config = config, accept_json(), body = body_params, encode = "json")

  if (httr::status_code(result) != 200) {
    warning("Cannot create course")
    return(result_list)
  }

  # Process and return results
  result_content <- content(result, "text")
  result_list <- fromJSON(result_content)

  message(paste("Course created at", result_list$alternateLink))

  # If user request for minimal response
  if (full_response) {
    return(result_list)
  } else {
    return(result_list$Id)
  }
}

#' Get Google Classroom Course Properties
#' @param course_id ID of the course you wish to retrieve information from
#' @importFrom httr config accept_json content
#' @importFrom jsonlite fromJSON
#' @importFrom assertthat assert_that is.string
#' @export
get_course_properties <- function(course_id) {
  # Check validity of inputs
  assert_that(is.string(course_id))

  # Get endpoint url
  url <- get_endpoint("classroom.endpoint.course", course_id)

  # Get auth token
  token <- get_token()
  config <- httr::config(token = token)

  # Get course properties
  result <- httr::GET(url, config = config, accept_json())

  if (httr::status_code(result) != 200) {
    message("ID provided does not point towards any course")
    return(result_list)
  }

  # Process and return results
  result_content <- content(result, "text")
  result_list <- fromJSON(result_content)

  return(result_list)
}
