#' Commit changes to a Google form
#' @param form_id The id of the google form to be updated
#' @param question_kind Can be choiceQuestion or textQuestion
#' @param required TRUE or FALSE is this a required question? Default is not required.
#' @param question a string that is what the question should say
#' @param choice_vector a character vector of the choices that should be given for this question
#' @param shuffle_opt TRUE or FALSE options should be shuffled? default is FALSE
#' @importFrom assertthat assert_that is.string
#' @importFrom httr config accept_json content
#' @importFrom jsonlite fromJSON
#' @export
#' 
#' question = "What answer do you want?"
#' choice_vector = c("A", "B", "C", "D")
create_multiple_choice_question <- function(question_kind = "choiceQuestion",
                                            required = FALSE,
                                            question = NULL,
                                            choice_vector = NULL,
                                            shuffle_opt = FALSE) {
  if (is.null(google_forms_request)) {
    google_forms_request <- google_forms_request_container$new()
  }

  # Input Validation
  assert_that(is.google_forms_request(google_forms_request))
  assert_that(is.string(question_kind))
  assert_that(is.logical(required))
  assert_that(is.string(question))
  assert_that(is.string(choice_vector))
  assert_that(is.logical(shuffle_opt))

  create_question_request <- list(createItem = list(item = list(questionItem = list())))
  
  create_question_request[["createItem"]][["location"]][["index"]] <- as.integer(1)
  
  if(!is.null(question)){
  create_question_request[["createItem"]][["item"]][["title"]] <- question 
  }
  
  create_question_request[["createItem"]][["item"]][["questionItem"]][["question"]][["choiceQuestion"]][["type"]] <- "RADIO"
  
  # Define insertion index on where the slides to be appended
  if(!is.null(choice_vector)){
    create_question_request[["createItem"]][["item"]][["questionItem"]][["question"]][["choiceQuestion"]][["options"]] <-  data.frame(value = choice_vector)
  }

  create_question_request[["createItem"]][["item"]][["questionItem"]][["question"]][["choiceQuestion"]][["shuffle"]] <- shuffle_opt

  google_forms_request$add_request(create_question_request)

  return(google_forms_request)
}


