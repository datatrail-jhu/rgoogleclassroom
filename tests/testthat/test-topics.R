test_that("Test topic functions", {

  skip_if_no_auth()
  owner_id <- get_owner_id()

  # Make a new course for this test
  new_course <- create_course(owner_id$id, name = "New course")

  # Create a new topic
  new_topic <- create_topic(course_id = new_course$id,
                            name = "New topic")

  # Retrieve list of topics for this course
  topic_list <- get_topic_list(course_id = new_course$id)
  # This should retrieve length of 1
  expect_length(topic_list, 1)

  # Retrieve info about this topic
  topic_info <- get_topic_properties(course_id = new_course$id, topic_id = topic_list$topic$topicId)
  # This should retrieve length of 1
  expect_length(topic_info, 1)

  # Clean up and delete the course
  archive_course(new_course$id)
  delete_course(new_course$id)
})
