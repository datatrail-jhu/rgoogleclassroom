#' Commit changes to a Google form
#' @param form_id The id of the google form to be updated
#' @param google_forms_request The google slide request to be applied to the slides
#' @importFrom assertthat assert_that is.string
#' @importFrom httr config accept_json content
#' @importFrom jsonlite fromJSON
#' @export
commit_to_form <- function(form_id, google_forms_request) {
  # Validate input
  assert_that(is.string(form_id))
  assert_that(is.google_forms_request(google_forms_request))

  # Get endpoint url
  url <- get_endpoint("forms.endpoint.batchUpdate", form_id = form_id)

  # Get token
  token <- get_token()
  config <- httr::config(token = token)

  # Wrapping body parameters in a requests list
  body_params <- list(requests = google_forms_request$to_list())

  # body_params <- gsub("value\\..\"", "value\"", jsonlite::toJSON(body_params))

  # Modify slides
  result <- httr::POST(url, config = config, body = body_params, encode = "json")

  # Process results
  result_content <- content(result, "text")
  result_list <- fromJSON(result_content)

  # If endpoint return url status other than 200, return error message
  if (httr::status_code(result) != 200) {
    warning("Cannot make form edit")
    return(result_list)
  }

  message(paste0("Check out your new adds to the form by going to this URL: https://docs.google.com/forms/d/", form_id))

  return(result_list)
}
