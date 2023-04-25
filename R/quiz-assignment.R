#' Create a quiz at a course
#' @param course_id  A course id where the quiz should be created
#' @param quiz_title A string indicating the title for the quiz
#' @param quiz_description A description for the quiz that will be made
#' @param topic_id Optional - a topic Id where the quiz will be posted
#' @param coursework_title a string that will be what the coursework title
#' @param due_date A due date for this quiz, in year-month-day format
#' @param assignment_description The description that will be given for the assignment
#' @param full_response Parameter to decide whether to return the full response or just the presentation ID
#' @importFrom httr config accept_json content
#' @importFrom jsonlite fromJSON
#' @export
#'
#' @examples \dontrun{
#'
#' course_id <- get_course_list()$courses$id[1]
#' topic_id <- get_topic_list(course_id)$topic$topicId[1]
#'
#' create_quiz(course_id, quiz_title = "new quiz", quiz_description = "This is a great quiz",
#' topic_id = topic_id, due_date = "2025-12-1")
#'}
create_quiz <- function(course_id = NULL,
                        quiz_title = NULL,
                        quiz_description = NULL,
                        topic_id = NULL,
                        coursework_title = "none",
                        work_type = NULL,
                        due_date = NULL,
                        assignment_description = "",
                        full_response = TRUE) {

  # Check validity of inputs
  assert_that(is.string(course_id))
  assert_that(is.string(quiz_title))
  assert_that(is.string(coursework_title))
  assert_that(is.string(quiz_description))
  assert_that(is.string(coursework_title))
  assert_that(is.string(assignment_description))

  # Build the due date as a list
  date_list <- date_handler(due_date)

  # Make the form
  form_info <- create_form(title = quiz_title)

  # Now make it a quiz
  make_form_quiz(form_id = form_info$formId)

  # Now make it an assignment on the course
  coursework <-
    create_coursework(course_id = course_id,
                      topic_id = topic_id,
                      title = coursework_title,
                      work_type = "ASSIGNMENT",
                      due_date = due_date,
                      description =  assignment_description,
                      link = form_info$responderUri)

  return(coursework)
}
