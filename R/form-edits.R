#' Create a multiple choice question
#' @param form_id The id of the google form to be updated
#' @param title An updated title
#' @param description An updated description
#' @param google_forms_request A google forms request object. If not supplied, it will be created new.
#' @param quiet TRUE/FALSE you'd like a progress message?
#' @importFrom assertthat assert_that is.string
#' @importFrom httr config accept_json content
#' @importFrom jsonlite fromJSON
#' @export
#' @examples \dontrun{
#'
#' update_form_settings(
#'   form_id = "12345",
#'   new_title = NULL,
#'   new_description = NULL
#' )
#' }
update_form_settings <- function(form_id = NULL,
                                 title = NULL,
                                 description = NULL,
                                 google_forms_request = NULL,
                                 quiet = FALSE) {

  result_description <- NULL
  result_title <- NULL

  form_id <- handle_form_url(form_id)

  if (is.null(google_forms_request)) {
    google_forms_request <- google_forms_request_container$new()
  }
  if (is.null(form_id)) {
    stop("form_id must be provided")
  }

  # Input Validation
  assert_that(is.google_forms_request(google_forms_request))

  if (!is.null(title)) {
    update_request <- list(updateFormInfo = list(info = list()))

    update_request[["updateFormInfo"]][["info"]][["title"]] <- title
    update_request[["updateFormInfo"]][["updateMask"]] <- "title"

    google_forms_request$add_request(update_request)

    result_title <- commit_to_form(form_id, google_forms_request, quiet = quiet)
  }

  if (!is.null(description)) {
    update_request <- list(updateFormInfo = list(info = list()))

    update_request[["updateFormInfo"]][["info"]][["description"]] <- description
    update_request[["updateFormInfo"]][["updateMask"]] <- "description"

    google_forms_request$add_request(update_request)

    result_description <- commit_to_form(form_id, google_forms_request, quiet = quiet)
  }

  if (!is.null(result_title) && !is.null(result_description)) {
    result <- list(result_title, result_description)
  } else if (!is.null(result_title)) {
    result <- result_title
  } else if (!is.null(result_description)) {
    result <- result_description
  }

  return(result = list(result_title, result_description))
}

#' Create a multiple choice question
#' @param form_id The id of the google form to be updated
#' @param commit_to_form Whether or not the request should be committed. If need to build the request further, you will want to say FALSE. Default is TRUE
#' @param google_forms_request A google forms request object. If not supplied, it will be created new.
#' @param required TRUE or FALSE is this a required question? Default is not required.
#' @param question a string that is what the question should say
#' @param choice_vector a character vector of the choices that should be given for this question
#' @param shuffle_opt TRUE or FALSE options should be shuffled? default is FALSE
#' @param correct_answer The index that corresponds to the correct answer in the `choice_vector` supplied
#' @param location Where should the new question be added
#' @param point_value An integer representing how many points
#' @param quiet TRUE/FALSE you'd like a progress message?
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
                                            commit_to_form = TRUE,
                                            required = FALSE,
                                            question = NULL,
                                            choice_vector = NULL,
                                            shuffle_opt = FALSE,
                                            correct_answer = NULL,
                                            google_forms_request = NULL,
                                            point_value = 1,
                                            quiet = FALSE,
                                            location = 0) {
  if (is.null(google_forms_request)) {
    google_forms_request <- google_forms_request_container$new()
  }
  if (is.null(form_id)) {
    stop("form_id must be provided")
  }

  # Input Validation
  assert_that(is.google_forms_request(google_forms_request))
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

  if (commit_to_form) {
    result <- commit_to_form(form_id, google_forms_request, quiet = quiet)
  } else {
    result <- google_forms_request
  }
  return(result)
}


#' Create a text question
#' @description This function makes a google request object that will be able to be posted with a batch request and
#' and added to a Google form to edit it.
#' @param form_id The id of the google form to be updated
#' @param commit_to_form Whether or not the request should be committed. If need to build the request further, you will want to say FALSE. Default is TRUE
#' @param google_forms_request A google forms request object. If not supplied, it will be created new.
#' @param required TRUE or FALSE is this a required question? Default is not required.
#' @param question a string that is what the question should say
#' @param location Where should the new question be added
#' @param correct_answer A string that matches exactly what would be considered a correct
#' @param point_value An integer representing how many points
#' @param quiet TRUE/FALSE you'd like a progress message?
#' @importFrom assertthat assert_that is.string
#' @importFrom httr config accept_json content
#' @importFrom jsonlite fromJSON
#' @export
#' @examples \dontrun{
#'
#' create_text_question(
#'   form_id = "12345",
#'   question = "Put text here that is for filling in the blank",
#'   point_value = 1
#' )
#' }
create_text_question <- function(
    form_id = NULL,
    commit_to_form = TRUE,
    required = FALSE,
    question = NULL,
    correct_answer = NULL,
    google_forms_request = NULL,
    point_value = 1,
    location = 0,
    quiet = FALSE) {
  if (is.null(google_forms_request)) {
    google_forms_request <- google_forms_request_container$new()
  }
  if (is.null(form_id)) {
    stop("form_id must be provided")
  }

  # Input Validation
  assert_that(is.google_forms_request(google_forms_request))
  assert_that(is.logical(required))
  assert_that(is.numeric(point_value))

  create_question_request <- list(createItem = list(item = list(questionItem = list())))

  create_question_request[["createItem"]][["location"]][["index"]] <- as.integer(location)

  if (!is.null(question)) {
    create_question_request[["createItem"]][["item"]][["title"]] <- question
  }

  create_question_request[["createItem"]][["item"]][["questionItem"]][["question"]][["textQuestion"]][["paragraph"]] <- TRUE

  if (!is.null(point_value)) {
    create_question_request[["createItem"]][["item"]][["questionItem"]][["question"]][["grading"]][["pointValue"]] <- point_value
  }

  if (!is.null(correct_answer)) {
    create_question_request[["createItem"]][["item"]][["questionItem"]][["question"]][["grading"]][["correctAnswers"]][["answers"]] <- correct_answer
  }

  google_forms_request$add_request(create_question_request)

  if (commit_to_form) {
    result <- commit_to_form(form_id, google_forms_request, quiet = quiet)
  } else {
    result <- google_forms_request
  }

  return(result)
}
