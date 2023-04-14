#' Generate endpoint for the Google classroom API
#' @param type_of_endpoint Type of endpoint to convert to url
#' @param course_id (Optional) ID of the google classroom to manipulate. Optional for classroom.endpoint.create
#' @param topic_id (Optional) ID of the topic to be affected
#' @importFrom assertthat is.string
get_endpoint <- function(type_of_endpoint = "classroom.endpoint.get", course_id = NULL, topic_id=NULL){

  # Check if type of endpoint is create a course
  if(type_of_endpoint == "classroom.endpoint.create" || type_of_endpoint == "classroom.endpoint.get" ){
    return(getOption(type_of_endpoint))
  }

  # Check that course_id parameter is a character, if not throw an error
  assert_that(is.string(course_id))

  # Check if type of endpoint is classroom.endpoint.topic.get
  if(type_of_endpoint == "classroom.endpoint.topic.get.course"){

    # Check that topicId parameter is a character, if not throw an error
    assert_that(is.string(topic_id))
    url_temp <- gsub("{courseId}", course_id, getOption(type_of_endpoint), fixed=TRUE)
    url_temp <- gsub("{topicId}", topic_id, url_temp, fixed=TRUE)

    return(url_temp)
  }
  return(gsub("{courseId}", course_id, getOption(type_of_endpoint), fixed=TRUE))
}
