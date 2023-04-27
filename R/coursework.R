#' Get list of courseworks for a course
#' @param course_id ID of the course to retrieve the courseworks from
#' @importFrom httr config accept_json content
#' @importFrom jsonlite fromJSON
#' @importFrom assertthat assert_that is.string
#' @export
#'
#' @examples \dontrun{
#'
#' course_id <- get_course_list()$courses$id[1]
#'
#' get_coursework_list(course_id)
#'}
get_coursework_list <- function(course_id) {
  # Get endpoint url
  url <- get_endpoint("classroom.endpoint.coursework.get", course_id = course_id)

  # Get auth token
  token <- get_token()
  config <- httr::config(token = token)

  # Modify course
  result <- httr::GET(url, config = config, accept_json(),
                      query = list(courseWorkStates = "DRAFT", courseWorkStates = "PUBLISHED"),
                      encode = "json")

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
#' @param title Name of new coursework. Required.
#' @param topic_id topic ID to be looked for.
#' @param publish TRUE/FALSE, automatically publish the coursework upon posting? Default is to be posted as a draft (students will not see it until you click Post).
#' @param due_date Required Due date for new coursework, must be given in year-month-day format.
#' @param link A url to an associated resource for the coursework being made.
#' @param description Description of new coursework. Is a string
#' @param work_type Currently only supported work type is ASSIGNMENT.
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
                              publish = FALSE,
                              title = NULL,
                              work_type = "ASSIGNMENT",
                              due_date = NULL,
                              description = NULL,
                              link = NULL) {

  # Get endpoint url
  url <- get_endpoint("classroom.endpoint.coursework.get", course_id = course_id)

  assert_that(is.string(course_id))
  if (!is.null(description)) assert_that(is.string(description))
  if (is.null(title)) stop("Argument 'title' must be set")

  # Get auth token
  token <- get_token()
  config <- httr::config(token = token)

  # If the link is specified, we need it to be in the list that Google wants
  if (!is.null(link)) {
    link <- list("link" = list("url" = link))
  }

  # Wrapping body parameters in a requests list
  body_params <- list(
    courseId = course_id,
    topic_id = topic_id,
    title = title,
    workType = work_type,
    dueDate = date_handler(due_date),
    dueTime = time_handler(),
    description = description,
    materials = link,
    state = ifelse(publish, "PUBLISHED", "DRAFT")
  )

  # Only keep non NULL items
  body_params <- body_params %>% purrr::keep( ~ !is.null(.))

  # Modify course
  result <- httr::POST(url, config = config, accept_json(), body = body_params, encode = "json")

  if (httr::status_code(result) != 200) {
    warning("Cannot create coursework")
    return(result)
  }
  # Process and return results
  result_content <- content(result, "text")
  result_list <- fromJSON(result_content)

  course_work_url <- gsub("/c/", "/w/", get_course_properties(result_list$courseId)$alternateLink)
  message(paste0("Coursework called: ", result_list$title, "\n Created at: ", course_work_url, "/t/all"))

  return(result_list)
}


#' Get Google Classroom Course Properties
#' @param course_id ID of the course you wish to retrieve information about a particular coursework
#' @param coursework_id ID of the coursework you wish to retrieve information about
#' @importFrom httr config accept_json content
#' @importFrom jsonlite fromJSON
#' @importFrom assertthat assert_that is.string
#' @export
get_coursework_properties <- function(course_id, coursework_id) {
  # Check validity of inputs
  assert_that(is.string(course_id))
  assert_that(is.string(coursework_id))

  # Get endpoint url
  url <- get_endpoint("classroom.endpoint.coursework.get", course_id, coursework_id)

  # Get auth token
  token <- get_token()
  config <- httr::config(token = token)

  # Get course properties
  result <- httr::GET(url, config = config,
                      query = list(courseWorkStates = "DRAFT", courseWorkStates = "PUBLISHED"),
                      accept_json())

  if (httr::status_code(result) != 200) {
    message("ID provided does not point towards any course or coursework")
    httr::stop_for_status(result)
  }

  # Process and return results
  result_content <- content(result, "text")
  result_list <- fromJSON(result_content)

  return(result_list)
}


##### This currently doesn't work! Not sure why.
#' Delete a Google Classroom Coursework
#' @param coursework_id ID of the archived course you wish to delete
#' @param coursework_id ID of the coursework you wish to retrieve information about
#' @importFrom httr config accept_json content
#' @importFrom jsonlite fromJSON
#' @importFrom assertthat assert_that is.string
delete_coursework <- function(course_id, coursework_id) {
  # Check validity of inputs
  assert_that(is.string(coursework_id))

  # Get endpoint url
  url <- get_endpoint("classroom.endpoint.coursework", course_id, coursework_id)

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

##### This currently doesn't work! Not sure why.
#' Publish a Google Classroom CourseWork
#' @param course_id ID of the archived course you wish to delete
#' @param coursework_id coursework ID of the coursework you wish to publish
#' @importFrom httr config accept_json content
#' @importFrom jsonlite fromJSON
#' @importFrom assertthat assert_that is.string
#' @export
publish_coursework <- function(course_id, coursework_id) {

  # Check validity of inputs
  assert_that(is.string(course_id))
  assert_that(is.string(coursework_id))

  # Get endpoint url
  url <- get_endpoint("classroom.endpoint.coursework", course_id, coursework_id = coursework_id)

  # Get auth token
  token <- get_token()
  config <- httr::config(token = token)

  # Wrapping body parameters in a requests list
  body_params <- list(
    courseId = course_id,
    id = coursework_id,
    state = "PUBLISHED",
    updateMask = "state"
  )

  # Modify course
  result <- httr::PUT(url, config = config,
                      accept_json(),
                      body = body_params,
                      query = list(courseWorkStates = "DRAFT"),
                      encode = "json")

  if (httr::status_code(result) != 200) {
    message("Failed to change the coursework")
    return(result)
  }

  # Process and return results
  result_content <- content(result, "text")
  result_list <- fromJSON(result_content)

  return(result_list)
}

