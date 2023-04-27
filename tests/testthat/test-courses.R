test_that("Test courses functions", {

  skip_if_no_auth()

  owner_id <- get_owner_id()

  # Retrieve a list of courses from this owner
  course_df <- get_course_list(owner_id$id)

  # Create a course
  new_course <- create_course(owner_id$id, name = "New course")

  # Clean up this test course
  archive_course(new_course$id)
  delete_course(new_course$id)
})
