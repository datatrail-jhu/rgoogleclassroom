skip_if_no_auth <- function() {
  testthat::skip_if_not(
    can_decrypt(),
    "Authentication not available"
  )
}
