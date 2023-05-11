test_that("authorization works", {
  skip_if_no_auth()
  token <- authorize(cache = TRUE)

  expect_type(token, "environment")

  auth_from_secret(
    token$credentials$access_token,
    token$credentials$refresh_token
  )
})
