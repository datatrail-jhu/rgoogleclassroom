#' Get form responses
#' @param form_id The id of the google form to be updated
#' @param form_url Google form url
#' @importFrom assertthat assert_that is.string
#' @importFrom httr config accept_json content
#' @importFrom jsonlite fromJSON
#' @export
#' @examples \dontrun{
#'
#' form_url <- "https://docs.google.com/forms/d/1woWtLVviIhrwb-NYEjVMO_IV2-62vOhaS4N0/edit#responses"
#' get_form_responses(form_url = form_url)
#' }
get_form_responses <- function(form_id = NULL, form_url = NULL) {
  # Check validity of inputs
  if (is.null(form_id) && is.null(form_url)) stop("Must supply either a form id or form url")

  if(!is.null(form_id)) assert_that(is.string(form_id))

  if(!is.null(form_url)) {
    assert_that(is.string(form_url))
    form_id <- handle_form_url(form_url)
  }

  # Get endpoint url
  url <- get_endpoint("forms.endpoint.responses", form_id = form_id)

  # Get auth token
  token <- get_token()
  config <- httr::config(token = token)

  # Modify course
  result <- httr::GET(url, config = config, accept_json(), encode = "json")

  if (httr::status_code(result) != 200) {
    message("Cannot create form")
    return(result)
  }
  # Process and return results
  result_content <- content(result, "text")
  result_list <- fromJSON(result_content)

  return(result_list)
}
