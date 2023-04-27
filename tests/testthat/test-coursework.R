test_that("Test coursework functions", {

  skip_if_no_auth()

  owner_id <- get_owner_id()

  new_course <- create_course(owner_id$id, name = "New course")

  new_coursework <- create_coursework(course_id = new_course$id,
                                      title = "New coursework",
                                      due_date = lubridate::today() + lubridate::hours(24))

  course_work_info <- get_coursework_properties(course_id = new_course$id, coursework_id = new_coursework$id)
  expect_length(course_work_info, 1)

  coursework_list <- get_coursework_list(course_id = new_course$id)
  expect_length(coursework_list, 1)

  archive_course(new_course$id)

  delete_course(new_course$id)
})
