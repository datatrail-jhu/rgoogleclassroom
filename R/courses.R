#' Get list of courses
#' @param owner_id owner_id to retrieve course listings from
#' @importFrom httr config accept_json content
#' @importFrom jsonlite fromJSON
#' @importFrom assertthat assert_that is.string
#' @export
#'
#' @examples \dontrun{
#' owner_id <- get_owner_id()
#' course_df <- get_course_list(owner_id)
#' }
get_course_list <- function(owner_id = get_owner_id()$id) {
  if (is.null(owner_id)) {
    stop("Need to provide owner_id from which to retrieve a list of courses from")
  }
  if (missing(owner_id)) {
    message("Using `owner_id = \"", get_owner_id()$id, "\"`")
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
#' @param name Name of the new course. Required.
#' @importFrom httr config accept_json content
#' @importFrom jsonlite fromJSON
#' @importFrom utils browseURL
#' @export
#'
#' @examples \dontrun{
#'
#' owner_id <- get_owner_id()
#' course_df <- create_course(owner_id, name = "New course")
#' }
create_course <- function(owner_id = get_owner_id()$id, name = NULL) {
  if (missing(owner_id)) {
    message("Using `owner_id = \"", get_owner_id()$id, "\"`")
  }

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
    return(result)
  }

  # Process and return results
  result_content <- content(result, "text")
  result_list <- fromJSON(result_content)

  message(paste("Course created at", result_list$alternateLink))

  browseURL(result_list$alternateLink)

  return(result_list)
}

#' Get Google Classroom Course Properties
#' @param course_id ID of the course you wish to retrieve information from
#' @importFrom httr config accept_json content
#' @importFrom jsonlite fromJSON
#' @importFrom assertthat assert_that is.string
#' @export
get_course_properties <- function(course_id) {
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

#' Archive a Google Classroom Course
#' @param course_id ID of the archived course you wish to delete
#' @importFrom httr config accept_json content
#' @importFrom jsonlite fromJSON
#' @importFrom assertthat assert_that is.string
#' @export
archive_course <- function(course_id) {
  # Get endpoint url
  url <- get_endpoint("classroom.endpoint.course", course_id)

  # Get auth token
  token <- get_token()
  config <- httr::config(token = token)

  # Wrapping body parameters in a requests list
  body_params <- list(
    id = course_id,
    courseState = "ARCHIVED",
    name = get_course_properties(course_id)$name
  )

  # Modify course
  result <- httr::PUT(url, config = config, accept_json(), body = body_params, encode = "json")

  if (httr::status_code(result) != 200) {
    message("Failed to archive course. Are you sure it's not already archived?")
    return(result_list)
  }

  # Process and return results
  result_content <- content(result, "text")
  result_list <- fromJSON(result_content)

  return(result_list)
}

#' Delete a Google Classroom Course
#' @param course_id ID of the archived course you wish to delete
#' @importFrom httr config accept_json content
#' @importFrom jsonlite fromJSON
#' @importFrom assertthat assert_that is.string
#' @export
delete_course <- function(course_id) {
  # Get endpoint url
  url <- get_endpoint("classroom.endpoint.course", course_id)

  # Get auth token
  token <- get_token()
  config <- httr::config(token = token)

  # Get course properties
  result <- httr::DELETE(url, config = config, accept_json())

  if (httr::status_code(result) != 200) {
    message("Failed to delete course - was it archived first?")
    return(result_list)
  }

  # Process and return results
  result_content <- content(result, "text")
  result_list <- fromJSON(result_content)

  return(result_list)
}
