#' Get list of topics for a course
#' @param id ID of the course
#' @importFrom httr config accept_json content
#' @importFrom jsonlite fromJSON
#' @importFrom assertthat assert_that is.string
#' @export
#' @examples \dontrun{
#'
#' course_id <- get_course_list()$courses$id[1]
#'
#' get_topic_list(course_id)
#'}
get_topic_list <- function(course_id) {
  # Get endpoint url
  url <- get_endpoint("classroom.endpoint.topic.get", course_id = course_id)

  # Get auth token
  token <- get_token()
  config <- httr::config(token = token)

  # Get list of topics
  result <- httr::GET(url, config = config, accept_json())

  if (httr::status_code(result) != 200) {
    message("No topics found")
    httr::stop_for_status(result)
  }

  # Process and return results
  result_content <- content(result, "text")
  result_list <- fromJSON(result_content)
  return(result_list)
}


#' Create a new topic
#' @param course_id Course id of where to make the new topic. Can find from end of URL e.g. "https://classroom.google.com/c/COURSE_ID_IS_HERE"
#' @param name Name of new topic. Required.
#' @param full_response Parameter to decide whether to return the full response or just the presentation ID
#' @importFrom httr config accept_json content
#' @importFrom jsonlite fromJSON
#' @export
create_topic <- function(course_id = NULL, name = NULL, full_response = FALSE) {
  # Get endpoint url
  url <- get_endpoint("classroom.endpoint.topic.get", course_id = course_id)

  # Get auth token
  token <- get_token()
  config <- httr::config(token = token)

  # Wrapping body parameters in a requests list
  body_params <- list(
    courseId = course_id,
    name = name
  )

  # Modify course
  result <- httr::POST(url, config = config, accept_json(), body = body_params, encode = "json")

  if (httr::status_code(result) != 200) {
    message("Cannot create topic")
    httr::stop_for_status(result)
  }
  # Process and return results
  result_content <- content(result, "text")
  result_list <- fromJSON(result_content)

  message(paste("Topic created at", result_list$alternateLink))

  # If user request for minimal response
  if (full_response) {
    return(result_list)
  } else {
    return(result_list$Id)
  }
}


#' Get Google Classroom Topic Properties
#' @param course_id ID of the course
#' @param topic_id ID of the course
#' @importFrom httr config accept_json content
#' @importFrom jsonlite fromJSON
#' @importFrom assertthat assert_that is.string
#' @export
get_topic_properties <- function(course_id, topic_id) {
  # Check validity of inputs
  assert_that(is.string(course_id))
  assert_that(is.string(topic_id))

  # Get endpoint url
  url <- get_endpoint("classroom.endpoint.topic", course_id, topic_id)

  # Get auth token
  token <- get_token()
  config <- httr::config(token = token)

  # Get course properties
  result <- httr::GET(url, config = config, accept_json())

  if (httr::status_code(result) != 200) {
    message("ID provided does not point towards any course or topic")
    httr::stop_for_status(result)
  }

  # Process and return results
  result_content <- content(result, "text")
  result_list <- fromJSON(result_content)

  return(result_list)
}
