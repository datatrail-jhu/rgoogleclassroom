test_that("Test ottrpal translate", {
  authorize(cache = TRUE)
  skip_if_no_auth()

  # Create a course we will use for this test
  owner_id <- get_owner_id()
  new_course <- create_course(owner_id$id, name = "New course")

  # Translate a markua quiz to
  ottr_quiz_to_google(
    markdown_quiz_path(),
    course_id = new_course$id,
    make_new_quiz = TRUE,
    due_date = lubridate::today() + lubridate::hours(24),
    output_path = "test.json"
  )

  file.remove("test.json")

  # Clean up and delete the course
  archive_course(new_course$id)
  delete_course(new_course$id)
})
