.onLoad <- function(libname, pkgname) {
  packageStartupMessage("To begin, run authorize() to supply rgoogleclassroom the proper credentials to run.")
}

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
forms_scopes_list <- c(
  "https://www.googleapis.com/auth/drive",
  "https://www.googleapis.com/auth/drive.file",
  "https://www.googleapis.com/auth/drive.readonly",
  "https://www.googleapis.com/auth/drive.metadata.readonly",
  "https://www.googleapis.com/auth/forms.body",
  "https://www.googleapis.com/auth/forms.body.readonly",
  "https://www.googleapis.com/auth/forms.responses.readonly"
)

base_classroom_uri <- "https://www.googleapis.com/auth/classroom."

classroom_scopes_list <- paste0(
  base_classroom_uri,
  c(
    "courses",
    "courses.readonly",
    "topics",
    "topics.readonly",
    "profile.emails",
    "profile.photos",
    "rosters",
    "rosters.readonly",
    "announcements",
    "announcements.readonly",
    "course-work.readonly",
    "student-submissions.students.readonly",
    "student-submissions.me.readonly",
    "coursework.me",
    "courseworkmaterials",
    "coursework.students",
    "courseworkmaterials",
    "coursework.students.readonly",
    "coursework.me.readonly",
    "addons.student",
    "push-notifications",
    "addons.teacher",
    "rosters",
    "profile.emails"
  )
)

#' Authorize R package to access Google classroom API
#' @description This is a function to authorize the R package to access the Googleclassroom API interactively.
#' @param token an output from \code{\link{oauth2.0_token}} to set as the authentication token.
#' @param cache Should the token be cached as an .httr-oauth file?
#' @param ... additional arguments to send to \code{\link{oauth2.0_token}}
#' @return OAuth token saved to the environment so the package can use the users' Google data
#' @importFrom utils menu installed.packages
#' @importFrom httr oauth_app oauth_endpoints oauth2.0_token
#' @export
#' @examples \dontrun{
#'
#' authorize()
#' }
authorize <- function(token = NULL, cache = FALSE, ...) {
  if (!cache) {
    cache_it <- menu(c("Yes store credentials as .httr-oauth file", "No do not store credentials, I will re-run this authorize() in my next R session"))
    if (cache_it == 1) {
      message("You chose to cache your credentials, if you change your mind, just delete the .httr-oauth. Be careful not to push this file to GitHub or share it anywhere.")
    }
  } else {
    cache_it <- 1
  }
  if (is.null(token)) {
    token <- oauth2.0_token(
      endpoint = app_set_up()$endpoint,
      app = app_set_up()$app,
      cache = cache_it == 1,
      scope = c(classroom_scopes_list, forms_scopes_list),
      ...
    )
  }
  set_token(token)
  return(invisible(token))
}

#' Use secrets to Authorize R package to access Google classroom API
#' @description This is a function to authorize the R package to access the Googleclassroom API. If no
#' client.id and client.secret is provided, the package would provide predefined values.
#' @param access_token Access token can be obtained from running authorize interactively: token <-authorize(); token$credentials$access_token
#' @param refresh_token Refresh token can be obtained from running authorize interactively: token <-authorize(); token$credentials$refresh_token
#' @return OAuth token saved to the environment so the package can use the users' Google data
#' @importFrom utils menu installed.packages
#' @importFrom httr oauth_app oauth_endpoints oauth2.0_token
#' @export
#' @examples \dontrun{
#'
#' token <- authorize()
#'
#' auth_from_secret(
#'   token$credentials$access_token,
#'   token$credentials$refresh_token
#' )
#' }
#'
auth_from_secret <- function(access_token, refresh_token) {
  credentials <- list(
    access_token = access_token,
    expires_in = 3599L,
    refresh_token = refresh_token,
    scope = c(classroom_scopes_list, forms_scopes_list),
    token_type = "Bearer"
  )

  token <- httr::oauth2.0_token(
    endpoint = app_set_up()$endpoint,
    app = app_set_up()$app,
    scope = c(classroom_scopes_list, forms_scopes_list),
    credentials = credentials
  )

  set_token(token)
  return(invisible(token))
}

# This sets up the app creds no matter which way authorization is called
app_set_up <- function() {
  decrypted <- openssl::aes_cbc_decrypt(
    readRDS(encrypt_creds_path()),
    key = readRDS(key_encrypt_creds_path())
  )

  app <- oauth_app(
    appname = "googleclassroom",
    key = unserialize(decrypted)$classroom_client_id,
    secret = unserialize(decrypted)$classroom_client_secret
  )
  endpoint <- oauth_endpoints("google")

  return(list(app = app, endpoint = endpoint))
}
