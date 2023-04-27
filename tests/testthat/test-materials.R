test_that("Test coursework materials functions", {

  skip_if_no_auth()

  # Create a course we will use for this test
  owner_id <- get_owner_id()
  new_course <- create_course(owner_id$id, name = "New course")

  # Create material at this course
  new_material <- create_material(course_id = new_course$id,
                                  title = "New material")

  # Retrieve the material info
  materials_info <- get_materials_properties(course_id = new_course$id,
                                             materials_id = new_material$id)
  # This should retrieve length of 1
  expect_length(materials_info, 1)

  # Retrieve the list of all the materials for the course
  materials_list <- get_materials_list(course_id = new_course$id)

  # This should retrieve length of 1
  expect_length(materials_list, 1)

  # Clean up and delete the course
  archive_course(new_course$id)
  delete_course(new_course$id)
})
