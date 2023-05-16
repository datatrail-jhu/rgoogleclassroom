#' Get list of forms used in a course as quizzes
#' @param course_id ID of the course to retrieve the linked quizzes from
#' @importFrom httr config accept_json content
#' @importFrom jsonlite fromJSON
#' @importFrom assertthat assert_that is.string
#' @export
#' @examples \dontrun{
#' course_id <- get_course_list()$courses$id[1]
#' quiz_list <- get_linked_quizzes_list(course_id)
#' }
get_linked_quizzes_list <- function(course_id) {
  # Get all the coursework for a course
  all_material <- get_coursework_list(course_id = course_id)

  # squash it
  unlisted_material <- unlist(all_material)

  form_urls <- unlisted_material[grep("form.responseUrl", names(unlisted_material))]

  form_ids <- stringr::word(form_urls, sep = "#gid=", 2)

  return(form_ids)
}

#' Get Google Form Properties
#' @param form_id Google form Id
#' @param form_url Google form url
#' @importFrom httr config accept_json content
#' @importFrom jsonlite fromJSON
#' @importFrom assertthat assert_that is.string
#' @export
#' @examples \dontrun{
#'
#' # Make the form
#' form_info <- create_form(title = "A great quiz", description = "This quiz is tricky")
#'
#' # Get info about the form
#' form_info <- get_form_properties(form_id = form_info$formId)
#' }
get_form_properties <- function(form_id = NULL, form_url = NULL) {
  # Check validity of inputs
  if (is.null(form_id) && is.null(form_url)) stop("Must supply either a form id or form url")

  if (!is.null(form_id)) assert_that(is.string(form_id))

  if (!is.null(form_url)) {
    assert_that(is.string(form_url))
    form_id <- handle_form_url(form_url)
  }

  # Get endpoint url
  url <- get_endpoint("forms.endpoint", form_id = form_id)

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

#' Create a new form
#' @param title The title for the new form. Required as a string.
#' @param description The description for the new form as a string.
#' @param document_title The title for the form file that will be stored in Google Drive
#' @importFrom httr config accept_json content
#' @importFrom jsonlite fromJSON
#' @export
#'
#' @examples \dontrun{
#' #'
#' # Make the form
#' form_info <- create_form(
#'   title = "A great quiz",
#'   description = "This quiz is tricky"
#' )
#' }
create_form <- function(title = NULL,
                        document_title = "new_form",
                        description = "") {
  # Check validity of inputs
  assert_that(is.string(title))

  # Get endpoint url
  url <- get_endpoint("forms.endpoint.get")

  # Get auth token
  token <- get_token()
  config <- httr::config(token = token)

  body_params <- list(
    "info" = list(
      "title" = title,
      "documentTitle" = document_title
    )
  )
  # Modify course
  result <- httr::POST(url, config = config, accept_json(), body = body_params, encode = "json")

  if (httr::status_code(result) != 200) {
    message("Cannot create form")
    return(result)
  }
  # Process and return results
  result_content <- content(result, "text")
  result_list <- fromJSON(result_content)

  message(paste("Form created at", result_list$responderUri))

  return(result_list)
}

#' Turn a form into a quiz
#' @param form_id The id of the google form to be updated into a Quiz
#' @importFrom assertthat assert_that is.string
#' @importFrom httr config accept_json content
#' @importFrom jsonlite fromJSON
#' @export
#' @examples \dontrun{
#'
#' # Make the form
#' form_info <- create_form(title = quiz_title)
#'
#' # Now make it a quiz
#' make_form_quiz(form_id = form_info$formId)
#' }
make_form_quiz <- function(form_id) {
  # Get endpoint url
  url <- get_endpoint("forms.endpoint.batchUpdate", form_id = form_id)

  # Get token
  token <- get_token()
  config <- httr::config(token = token)

  # Wrapping body parameters in a requests list
  body_params <- list(
    "requests" = list(
      "updateSettings" = list(
        "settings" = list(
          "quizSettings" = list(
            "isQuiz" = "true"
          )
        ), "updateMask" = "quizSettings"
      )
    )
  )

  # Modify slides
  result <- httr::POST(url, config = config, body = body_params, encode = "json")

  if (httr::status_code(result) != 200) {
    message("Cannot create form")
    return(result)
  }
  # Process and return results
  result_content <- content(result, "text")
  result_list <- fromJSON(result_content)

  message(paste("Form created at", result_list$responderUri))

  return(result_list)
}

#' Make a copy of an existing form
#' @param form_id The form_id that is desired to be copied.
#' @param new_name What should the new file name for the copied file be?
#' @param quiet TRUE or FALSE whether messages should be printed out.
#' @importFrom httr config accept_json content
#' @importFrom jsonlite fromJSON
#' @export
#'
#' @examples \dontrun{
#' #'
#' # Make the form
#' form_info <- copy_form(form_id = "https://docs.google.com/forms/d/someformidhere/edit",
#'                        new_name = "copied form")
#' }
copy_form <- function(form_id, new_name = NULL, quiet = FALSE) {
  form_id <- handle_form_url(form_id)

  # Get endpoint url
  url <- get_endpoint("googledrive.endpoint.copy", form_id = form_id)

  # Get auth token
  token <- get_token()
  config <- httr::config(token = token)

  # Wrapping body parameters in a requests list
  body_params <- list(
    "name" = new_name
  )

  # Modify course
  result <- httr::POST(url, config = config, body = body_params, query = list("supportsAllDrives" = TRUE), encode = "json")

  if (httr::status_code(result) != 200) {
    message("Cannot create form")
    return(result)
  }
  # Process and return results
  result_content <- content(result, "text")
  result_list <- fromJSON(result_content)

  if (quiet) {
    message(paste("Form created at", result_list$id))
  }

  return(result_list)
}
