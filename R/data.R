## File paths to needed data

markdown_quiz_path <- function() {
  list.files(
    pattern = "quiz.md",
    recursive = TRUE,
    system.file("extdata", package = "rgoogleclassroom"),
    full.names = TRUE
  )
}

default_creds_path <- function() {
  list.files(
    pattern = "default_creds.rds",
    recursive = TRUE,
    system.file("extdata", package = "rgoogleclassroom"),
    full.names = TRUE
  )
}

key_encrypt_creds_path <- function() {
  list.files(
    pattern = "encrypt_pass.rds",
    recursive = TRUE,
    system.file("extdata", package = "rgoogleclassroom"),
    full.names = TRUE
  )
}

encrypt_creds_path <- function() {
  list.files(
    pattern = "encrypt.rds",
    recursive = TRUE,
    system.file("extdata", package = "rgoogleclassroom"),
    full.names = TRUE
  )
}
