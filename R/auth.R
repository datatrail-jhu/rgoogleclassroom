library(googleAuthR)

googleAuthR::gar_set_client(".secrets/client_secret_399382401954-f98jfqie6uuvlg5pl6td7i5efn2hi8hu.apps.googleusercontent.com.json",
                            scopes = c("https://www.googleapis.com/auth/classroom.courses.readonly",
                                       "https://www.googleapis.com/auth/classroom.topics",
                                       "https://www.googleapis.com/auth/classroom.topics.readonly",
                                       "https://www.googleapis.com/auth/classroom.courses"),
                            )
auth <- gar_auth()

secrets <- fromJSON(".secrets/client_secret_399382401954-f98jfqie6uuvlg5pl6td7i5efn2hi8hu.apps.googleusercontent.com.json")

config <- httr::config(token=auth$credentials$id_token)

authorize_from_secret <- function(access_token) {

  #client_id <- getOption("classroom.client.id")
  #client_secret <- getOption("classroom.client.secret")

  auth <- gar_auth()

  app <- httr::oauth_app(appname = "rgoogleclassroom",
                         key = secrets$installed$client_id,
                         secret = secrets$installed$client_secret)

  endpoint <- httr::oauth_endpoints("google")

  token <- httr::oauth2.0_token(endpoint = endpoint, app = app,
                                scope = c("https://www.googleapis.com/auth/classroom.courses.readonly",
                                          "https://www.googleapis.com/auth/classroom.topics",
                                          "https://www.googleapis.com/auth/classroom.topics.readonly",
                                          "https://www.googleapis.com/auth/classroom.courses"),
                                credentials = auth$credentials)
  return(out)
}

url <- "https://classroom.googleapis.com/v1/course/NTk0NTA2NTU0Njky"

response <- httr::GET(
  url,
  config = token,
  httr::accept_json()
)

response
