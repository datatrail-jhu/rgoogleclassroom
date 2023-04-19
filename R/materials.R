#' Get list of materials for a course
#' @param id ID of the course
#' @importFrom httr config accept_json content
#' @importFrom jsonlite fromJSON
#' @importFrom assertthat assert_that is.string
#' @export

get_materials_list <- function(course_id) {
  # Get endpoint url
  url <- get_endpoint("classroom.endpoint.materials.get", course_id = course_id)

  # Get auth token
  token <- get_token()
  config <- httr::config(token = token)

  # Get list of courseworks
  result <- httr::GET(url, config = config, accept_json())

  if (httr::status_code(result) != 200) {
    message("No materials found")
    httr::stop_for_status(result)
  }

  # Process and return results
  result_content <- content(result, "text")
  result_list <- fromJSON(result_content)
  return(result_list)
}


#' Create a new material
#' @param course_id Course id of where to make the new materials. Can find from end of URL e.g. "https://classroom.google.com/c/COURSE_ID_IS_HERE"
#' @param title Name of new material
#' @param description A description for the new material
#' @param material_link A URL to go with the associated material
#' @param due_date A due date for the associated material
#' @param full_response Parameter to decide whether to return the full response or just the presentation ID
#' @importFrom httr config accept_json content
#' @importFrom jsonlite fromJSON
#' @export
create_material <- function(course_id = NULL,
                              topic_id = NULL,
                              title = NULL,
                              due_date = NULL,
                              description = NULL,
                              material_link = NULL,
                              full_response = FALSE) {
  # Get endpoint url
  url <- get_endpoint("classroom.endpoint.materials.get", course_id)

  # Get auth token
  token <- get_token()
  config <- httr::config(token = token)

  # Wrapping body parameters in a requests list
  body_params <- list(
    courseId = course_id,
     topic_id = topic_id,
     materials = list("link" = list("url" = material_link)),
     title = title,
     dueDate = due_date,
     description = description
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

  message(paste("Coursework created at", result_list$alternateLink))

  # If user request for minimal response
  if (full_response) {
    return(result_list)
  } else {
    return(result_list$Id)
  }
}


#' Get Google Classroom Materials properties
#' @param id ID of the course
#' @param materials_id The material id you wish to retrieve
#' @importFrom httr config accept_json content
#' @importFrom jsonlite fromJSON
#' @importFrom assertthat assert_that is.string
#' @export
get_materials_properties <- function(course_id, materials_id) {
  # Check validity of inputs
  assert_that(is.string(course_id))
  assert_that(is.string(materials_id))

  # Get endpoint url
  url <- get_endpoint("classroom.endpoint.materials.get", course_id, materials_id)

  # Get auth token
  token <- get_token()
  config <- httr::config(token = token)

  # Get course properties
  result <- httr::GET(url, config = config, accept_json())

  if (httr::status_code(result) != 200) {
    message("ID provided does not point towards any material")
    httr::stop_for_status(result)
  }

  # Process and return results
  result_content <- content(result, "text")
  result_list <- fromJSON(result_content)

  return(result_list)
}
