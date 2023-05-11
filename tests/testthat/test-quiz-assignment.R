test_that("Test quiz making function", {
  authorize(cache = TRUE)
  skip_if_no_auth()

  # Create a course we will use for this test
  owner_id <- get_owner_id()
  new_course <- create_course(owner_id$id, name = "New course")

  new_quiz <- create_quiz(new_course$id,
    quiz_title = "new quiz",
    quiz_description = "This is a great quiz",
    due_date = lubridate::today() + lubridate::hours(24)
  )

  expect_type(new_quiz, "list")

  # Clean up and delete the course
  archive_course(new_course$id)
  delete_course(new_course$id)
})
