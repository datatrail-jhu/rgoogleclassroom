#' Get list of files from a Google Shared Drive
#' @param drive_id ID of the drive to retrieve a list of files from
#' @importFrom httr config accept_json content
#' @importFrom jsonlite fromJSON
#' @importFrom assertthat assert_that is.string
#' @export
#'
get_drive_file_list <- function(drive_id) {
  # Get endpoint url
  url <- get_endpoint("googledrive.endpoint")

  # Get auth token
  token <- get_token()
  config <- httr::config(token = token)

  # Wrapping body parameters in a requests list
  body_params <- list(
    corpora = "drive",
    driveId = drive_id,
    includeItemsFromAllDrives = "true",
    supportsAllDrives = "true"
  )

  # Get list of topics
  result <- httr::GET(url, config = config, body = body_params, accept_json())

  if (httr::status_code(result) != 200) {
    message("No topics found")
    httr::stop_for_status(result)
  }

  # Process and return results
  result_content <- content(result, "text")
  result_list <- fromJSON(result_content)
  return(result_list)
}
