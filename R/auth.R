.classroomEnv <- new.env(parent = emptyenv())
.classroomEnv$Token <- NULL

# Set token to environment
set_token <- function(value) {
  .classroomEnv$Token <- value
  return(value)
}

# Get token from environment
get_token <- function() {
  .classroomEnv$Token
}

#' Authorize R package to access Google classroom API
#' @description This is a function to authorize the R package to access the Googleclassroom API. If no
#' client.id and client.secret is provided, the package would provide predefined values.
#' @importFrom httr oauth_app oauth_endpoints oauth2.0_token
#' @importFrom googledrive drive_auth
#' @param client_id OAuth client ID. This is obtained from Google API Credentials
#' @param client_secret OAuth client secret. This is obtained from Google API Credentials
#' @param token an output from \code{\link{oauth2.0_token}} to set as the
#' authentication token.
#' @param ... additional arguments to send to \code{\link{oauth2.0_token}}
#' @export
google_classroom_authorize <- function(
  client_id = getOption("classroom.client.id"),
  client_secret = getOption("classroom.client.secret"),
  token = NULL,
  ...){
  drive_auth(
    scope = c("https://www.googleapis.com/auth/classroom.courses.readonly",
              "https://www.googleapis.com/auth/classroom.topics",
              "https://www.googleapis.com/auth/classroom.topics.readonly",
              "https://www.googleapis.com/auth/classroom.courses"),
    ...)
}
