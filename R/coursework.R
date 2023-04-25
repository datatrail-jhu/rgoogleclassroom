#' Get list of courseworks for a course
#' @param id ID of the course
#' @importFrom httr config accept_json content
#' @importFrom jsonlite fromJSON
#' @importFrom assertthat assert_that is.string
#' @export
#'
#' @examples \dontrun{
#'
#' courses_id <- get_course_list()$courses$id[2]
#'}
get_coursework_list <- function(course_id) {
  # Get endpoint url
  url <- get_endpoint("classroom.endpoint.coursework.get", course_id = course_id)

  # Get auth token
  token <- get_token()
  config <- httr::config(token = token)

  # Get list of courseworks
  result <- httr::GET(url, config = config, accept_json())

  if (httr::status_code(result) != 200) {
    message("No courseworks found")
    httr::stop_for_status(result)
  }

  # Process and return results
  result_content <- content(result, "text")
  result_list <- fromJSON(result_content)
  return(result_list)
}


#' Create a new coursework
#' @param course_id Course id of where to make the new coursework. Can find from end of URL e.g. "https://classroom.google.com/c/COURSE_ID_IS_HERE"
#' @param title Name of new coursework
#' @param due_date Required Due date for new coursework, must be given in year-month-day format.
#' @param link A url to an associated resource for the coursework being made.
#' @param description Description of new coursework. Is a string
#' @param full_response Parameter to decide whether to return the full response
#' @importFrom httr config accept_json content
#' @importFrom jsonlite fromJSON
#' @export
#'
#' @examples \dontrun{
#' topic_id <- get_topic_list("604042323237")$topic$topicId[1]
#' course_id <- get_course_list()$courses$id[1]
#'
#' create_coursework(course_id, topic_id, title = "a new quiz", due_date = "2025-12-1",
#'   description = "blah blah", link = "https://www.datatrail.org/")
#'
#' }
#'
create_coursework <- function(course_id = NULL,
                              topic_id = NULL,
                              title = NULL,
                              work_type = "ASSIGNMENT",
                              due_date = NULL,
                              description = NULL,
                              link = NULL,
                              full_response = FALSE) {

  # Get endpoint url
  url <- get_endpoint("classroom.endpoint.coursework.get", course_id = course_id)

  assert_that(is.string(course_id))
  assert_that(is.string(topic_id))
  if (!is.null(description)) assert_that(is.string(description))

  # Get auth token
  token <- get_token()
  config <- httr::config(token = token)


  # Wrapping body parameters in a requests list
  body_params <- list(
    courseId = course_id,
    topic_id = topic_id,
    title = title,
    workType = work_type,
    dueDate = date_handler(due_date),
    dueTime = time_handler(),
    description = description,
    materials = list("link" = list("url" = link))
  )

  # Modify course
  result <- httr::POST(url, config = config, accept_json(), body = body_params, encode = "json")

  if (httr::status_code(result) != 200) {
    message("Cannot create coursework")
    httr::stop_for_status(result)
  }
  # Process and return results
  result_content <- content(result, "text")
  result_list <- fromJSON(result_content)

  course_work_url <- gsub("/c/", "/w/", get_course_properties(result_list$courseId)$alternateLink)
  message(paste0("Coursework called: ", result_list$title, "\n Created at: ", course_work_url, "/t/all"))

  # If user request for minimal response
  if (full_response) {
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
get_coursework_properties <- function(course_id, coursework_id) {
  # Check validity of inputs
  assert_that(is.string(course_id))
  assert_that(is.string(coursework_id))

  # Get endpoint url
  url <- get_endpoint("classroom.endpoint.get.coursework", course_id, coursework_id)

  # Get auth token
  token <- get_token()
  config <- httr::config(token = token)

  # Get course properties
  result <- httr::GET(url, config = config, accept_json())

  if (httr::status_code(result) != 200) {
    message("ID provided does not point towards any course or coursework")
    httr::stop_for_status(result)
  }

  # Process and return results
  result_content <- content(result, "text")
  result_list <- fromJSON(result_content)

  return(result_list)
}
