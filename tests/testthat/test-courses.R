test_that("Get courses functions", {

  skip_if_no_auth()

  owner_id <- get_owner_id()

  course_df <- get_course_list(owner_id$id)

  new_course <- create_course(owner_id$id, name = "New course")

  archive_course(new_course$id)

  delete_course(new_course$id)
})
