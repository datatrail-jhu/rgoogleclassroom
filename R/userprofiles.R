#' Get ownerId based on credentials
#' @importFrom httr config accept_json content
#' @importFrom jsonlite fromJSON
#' @importFrom assertthat assert_that is.string
#' @export

get_owner_id <- function() {
  # Get endpoint url
  url <- get_endpoint("classroom.endpoint.user")

  # Get auth token
  token <- get_token()
  config <- httr::config(token = token)

  # Get course properties
  result <- httr::GET(url, config = config, accept_json())

  if (httr::status_code(result) != 200) {
    message("No ownerId found")
    httr::stop_for_status(result)
  }

  # Process and return results
  result_content <- content(result, "text")
  result_list <- fromJSON(result_content)
  return(result_list)
}
