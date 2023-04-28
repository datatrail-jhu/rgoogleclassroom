#' Create a multiple choice question
#' @param form_id The id of the google form to be updated
#' @param question_kind Currently only choiceQuestion 's are supported
#' @param google_forms_request A google forms request object. If not supplied, it will be created new.
#' @param required TRUE or FALSE is this a required question? Default is not required.
#' @param question a string that is what the question should say
#' @param choice_vector a character vector of the choices that should be given for this question
#' @param shuffle_opt TRUE or FALSE options should be shuffled? default is FALSE
#' @param correct_answer The index that corresponds to the correct answer in the `choice_vector` supplied
#' @param location Where should the new question be added
#' @param point_value An integer representing how many points
#' @importFrom assertthat assert_that is.string
#' @importFrom httr config accept_json content
#' @importFrom jsonlite fromJSON
#' @export
#' @examples \dontrun{
#'
#' create_multiple_choice_question(
#'   form_id = "12345",
#'   question = "What answer do you want?",
#'   choice_vector = c("A", "B", "C", "D"),
#'   correct_answer = 3,
#'   shuffle_opt = TRUE
#' )
#' }
create_multiple_choice_question <- function(form_id = NULL,
                                            question_kind = "choiceQuestion",
                                            required = FALSE,
                                            question = NULL,
                                            choice_vector = NULL,
                                            shuffle_opt = FALSE,
                                            correct_answer = NULL,
                                            google_forms_request = NULL,
                                            point_value = 1,
                                            location = 0) {
  if (is.null(google_forms_request)) {
    google_forms_request <- google_forms_request_container$new()
  }
  if (is.null(form_id)) {
    stop("form_id must be provided")
  }

  # Input Validation
  assert_that(is.google_forms_request(google_forms_request))
  assert_that(is.string(question_kind))
  assert_that(is.logical(required))
  assert_that(is.string(question))
  assert_that(is.vector(choice_vector))
  assert_that(is.logical(shuffle_opt))

  create_question_request <- list(createItem = list(item = list(questionItem = list())))

  create_question_request[["createItem"]][["location"]][["index"]] <- as.integer(location)

  if (!is.null(question)) {
    create_question_request[["createItem"]][["item"]][["title"]] <- question
  }

  create_question_request[["createItem"]][["item"]][["questionItem"]][["question"]][["choiceQuestion"]][["type"]] <- "RADIO"

  # Define insertion index on where the slides to be appended
  if (!is.null(choice_vector)) {
    create_question_request[["createItem"]][["item"]][["questionItem"]][["question"]][["choiceQuestion"]][["options"]] <- data.frame(value = choice_vector)
  }

  create_question_request[["createItem"]][["item"]][["questionItem"]][["question"]][["choiceQuestion"]][["shuffle"]] <- shuffle_opt

  if (!is.null(correct_answer)) {
    create_question_request[["createItem"]][["item"]][["questionItem"]][["question"]][["grading"]][["correctAnswers"]][["answers"]] <- data.frame(value = choice_vector[correct_answer])
  }

  if (!is.null(point_value)) {
    create_question_request[["createItem"]][["item"]][["questionItem"]][["question"]][["grading"]][["pointValue"]] <- point_value
  }

  google_forms_request$add_request(create_question_request)

  return(google_forms_request)
}
