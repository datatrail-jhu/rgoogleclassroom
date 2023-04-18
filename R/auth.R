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


### Declare all the scopes
base_uri <- "https://www.googleapis.com/auth/classroom."
classroom_scopes_list <- paste0(
  base_uri,
  c(
    "courses",
    "courses.readonly",
    "topics",
    "topics.readonly",
    "profile.emails",
    "profile.photos",
    "rosters",
    "rosters.readonly",
    "coursework.students",
    "announcements",
    "announcements.readonly",
    "course-work.readonly",
    "student-submissions.students.readonly",
    "student-submissions.me.readonly",
    "coursework.me",
    "courseworkmaterials",
    "addons.student",
    "push-notifications",
    "addons.teacher",
    "rosters",
    "profile.emails"
  )
)

#' Authorize R package to access Google classroom API
#' @description This is a function to authorize the R package to access the Googleclassroom API. If no
#' client.id and client.secret is provided, the package would provide predefined values.
#' @importFrom httr oauth_app oauth_endpoints oauth2.0_token
#' @param client_id OAuth client ID. This is obtained from Google API Credentials
#' @param client_secret OAuth client secret. This is obtained from Google API Credentials
#' @param token an output from \code{\link{oauth2.0_token}} to set as the
#' authentication token.
#' @param ... additional arguments to send to \code{\link{oauth2.0_token}}
#' @export
authorize <- function(
    token = NULL,
    ...) {
  decrypted <- openssl::aes_cbc_decrypt(readRDS(encrypt_creds_path()),
    key = readRDS(key_encrypt_creds_path())
  )

  if (is.null(token)) {
    app <- oauth_app(
      appname = "googleclassroom",
      key = unserialize(decrypted)$classroom_client_id,
      secret = unserialize(decrypted)$classroom_client_secret
    )
    endpoint <- oauth_endpoints("google")
    token <- oauth2.0_token(
      endpoint = endpoint, app = app,
      scope = classroom_scopes_list,
      ...
    )
  }
  set_token(token)
  return(invisible(token))
}
