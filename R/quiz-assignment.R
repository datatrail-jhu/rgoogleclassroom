#' Create a quiz at a course
#' @param course_id = NULL,
#' @param quiz_title = NULL,
#' @param quiz_description = NULL,
#' @param topic_id = NULL,
#' @param coursework_title a string that will be what the coursework title
#' @param work_type = NULL,
#' @param due_date A due date for this quiz, in year-month-day format
#' @param assignment_description The description that will be given for the assignment
#' @param full_response Parameter to decide whether to return the full response or just the presentation ID
#' @importFrom httr config accept_json content
#' @importFrom jsonlite fromJSON
#' @export
#'
#' NjA0MDQyMzIzMjM3
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
  assert_that(is.string(title))
  assert_that(is.string(course_id))
  assert_that(is.string(coursework_title))
  assert_that(is.string(assignment_description))

  date_list <- date_handler(due_date)

  # Make the form
  form_info <- create_form(title = title)

  # Now make it a quiz
  make_form_quiz(form_id = form_info$formId)

  # Now make it an assignment on the course
  coursework <-
    create_coursework(title = coursework_title,
                      work_type = "ASSIGNMENT",
                      due_date = date_list,
                      description =  assignment_description,
                      link = form_info$responderUri)

}