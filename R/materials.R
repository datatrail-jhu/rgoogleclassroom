#' Get list of materials for a course
#' @param course_id ID of the course
#' @importFrom httr config accept_json content
#' @importFrom jsonlite fromJSON
#' @importFrom assertthat assert_that is.string
#' @export
#'
#' @examples \dontrun{
#' course_id <- get_course_list()$courses$id[1]
#' materials_df <- get_materials_list(course_id)
#' }
get_materials_list <- function(course_id) {
  # Get endpoint url
  url <- get_endpoint("classroom.endpoint.materials.get", course_id = course_id)

  # Get auth token
  token <- get_token()
  config <- httr::config(token = token)

  # Get list of courseworks
  result <- httr::GET(url,
    config = config,
    query = list(courseWorkMaterialStates = "DRAFT", courseWorkMaterialStates = "PUBLISHED"),
    accept_json()
  )

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
#' @param topic_id topic ID to be looked for.
#' @param title Name of new material
#' @param publish TRUE/FALSE, automatically publish the coursework upon posting? Default is to be posted as a draft (students will not see it until you click Post).
#' @param description A description for the new material
#' @param material_link A URL to go with the associated material
#' @importFrom httr config accept_json content
#' @importFrom jsonlite fromJSON
#' @export
#'
#'
#' @examples \dontrun{
#' course_id <- get_course_list()$courses$id[3]
#' topic_id <- get_topic_list(course_id)$topic$topicId[1]
#'
#' create_material(course_id, topic_id, title = "new material")
#' }
create_material <- function(course_id = NULL,
                            topic_id = NULL,
                            publish = FALSE,
                            title = NULL,
                            description = NULL,
                            material_link = NULL) {
  # Get endpoint url
  url <- get_endpoint("classroom.endpoint.materials.get", course_id)

  # Get auth token
  token <- get_token()
  config <- httr::config(token = token)

  # If the link is specified, we need it to be in the list that Google wants
  if (!is.null(material_link)) {
    link <- list("link" = list("url" = material_link))
  }

  # Wrapping body parameters in a requests list
  body_params <- list(
    courseId = course_id,
    topic_id = topic_id,
    title = title,
    description = description,
    materials = link,
    state = ifelse(publish, "PUBLISHED", "DRAFT")
  )

  # Only keep non NULL items
  body_params <- body_params %>% purrr::keep(~ !is.null(.))

  # Modify course
  result <- httr::POST(url, config = config, accept_json(), body = body_params, encode = "json")

  if (httr::status_code(result) != 200) {
    warning("Cannot create coursework material")
    return(result)
  }
  # Process and return results
  result_content <- content(result, "text")
  result_list <- fromJSON(result_content)

  course_work_url <- gsub("/c/", "/w/", get_course_properties(result_list$courseId)$alternateLink)
  message(paste0("Coursework Materials called: ", result_list$title, "\n Created at: ", course_work_url, "/t/all"))

  return(result_list)
}


#' Get Google Classroom Materials properties
#' @param course_id ID of the course
#' @param materials_id The material id you wish to retrieve
#' @importFrom httr config accept_json content
#' @importFrom jsonlite fromJSON
#' @importFrom assertthat assert_that is.string
#' @export
#'
#' @examples \dontrun{
#' course_id <- get_course_list()$courses$id[3]
#' materials_id <- get_materials_list(course_id)$material_id$courseWorkMaterial$id[1]
#'
#' get_materials_properties(course_id, materials_id)
#' }
get_materials_properties <- function(course_id, materials_id) {
  # Get endpoint url
  url <- get_endpoint("classroom.endpoint.materials.get", course_id, materials_id)

  # Get auth token
  token <- get_token()
  config <- httr::config(token = token)

  # Get course properties
  result <- httr::GET(url,
    config = config,
    query = list(courseWorkMaterialStates = "DRAFT", courseWorkMaterialStates = "PUBLISHED"),
    accept_json()
  )

  if (httr::status_code(result) != 200) {
    message("ID provided does not point towards any material")
    httr::stop_for_status(result)
  }

  # Process and return results
  result_content <- content(result, "text")
  result_list <- fromJSON(result_content)

  return(result_list)
}
