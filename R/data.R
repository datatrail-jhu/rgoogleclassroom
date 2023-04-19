## File paths to needed data

#' Get file path to an example quiz
markdown_quiz_path <- function() {
  list.files(
    pattern = "quiz.md",
    recursive = TRUE,
    system.file("extdata", package = "rgoogleclassroom"),
    full.names = TRUE
  )
}
#' Get file path to an default credentials RDS
default_creds_path <- function() {
  list.files(
    pattern = "default_creds.rds",
    recursive = TRUE,
    system.file("extdata", package = "rgoogleclassroom"),
    full.names = TRUE
  )
}
#' Get file path to an key encryption RDS
key_encrypt_creds_path <- function() {
  list.files(
    pattern = "encrypt_pass.rds",
    recursive = TRUE,
    system.file("extdata", package = "rgoogleclassroom"),
    full.names = TRUE
  )
}
#' Get file path to an encrypted credentials RDS
encrypt_creds_path <- function() {
  list.files(
    pattern = "encrypt.rds",
    recursive = TRUE,
    system.file("extdata", package = "rgoogleclassroom"),
    full.names = TRUE
  )
}
