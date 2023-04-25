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
#' @param work_type Type of new coursework
#' @param due_date Due date for new coursework
#' @param description Description of new coursework
#' @param full_response Parameter to decide whether to return the full response
#' @importFrom httr config accept_json content
#' @importFrom jsonlite fromJSON
#' @export
#' topic_id <- get_topic_list("604042323237")$topic$topicId[3]
#' course_id <- "604042323237"
#' title = "a new quiz"
#' work_type = "ASSIGNMENT"
#' description = "blah blah"
#' link = "https://www.datatrail.org/"
create_coursework <- function(course_id = NULL,
                              topic_id = NULL,
                              title = NULL,
                              work_type = NULL,
                              due_date = NULL,
                              description = NULL,
                              link = NULL,
                              full_response = FALSE) {
  # Get endpoint url
  url <- get_endpoint("classroom.endpoint.coursework.get", course_id = course_id)

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

  message(paste0("Coursework created at: ", get_course_properties(result_list$courseId)$alternateLink, "/t/all"))

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
