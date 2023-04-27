test_that("Test coursework functions", {
  authorize(cache = TRUE)
  skip_if_no_auth()

  owner_id <- get_owner_id()

  # Create a new course for the purposes of this test
  new_course <- create_course(owner_id$id, name = "New course")

  # Create a new coursework
  new_coursework <- create_coursework(course_id = new_course$id,
                                      title = "New coursework",
                                      due_date = lubridate::today() + lubridate::hours(24))
  expect_type(new_coursework, "list")

  # Get coursework properties
  course_work_info <- get_coursework_properties(course_id = new_course$id, coursework_id = new_coursework$id)
  # This should retrieve length of 1
  expect_length(course_work_info, 1)

  # Retrieve all the courseworks for this course
  coursework_list <- get_coursework_list(course_id = new_course$id)
  # This should retrieve length of 1
  expect_length(coursework_list, 1)

  # Clean up and delete this course
  archive_course(new_course$id)
  delete_course(new_course$id)
})
