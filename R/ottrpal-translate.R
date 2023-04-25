# ottrpal pals

#' Translate Markua questions for submission to Google API
#'
#' Takes a Markua formatted quiz and translates it to something that can be sent to Google Forms API.
#'
#' @param quiz_path The file path to a Markua formatted quiz to be translated to Google Forms API format
#' @param verbose Whether progress messages should be given.
#' @importFrom magrittr %>%
#' @return A list of the output from [ottrpal::check_question] with messages/warnings regarding each question and each check.
#'#'
#' @examples \dontrun{
#'
#' # Using quiz example
#'
#' quiz_path <- markdown_quiz_path()
#' good_quiz <- readLines(quiz_path)
#' good_quiz_specs <- parse_quiz(good_quiz)
#' good_quiz_checks <- check_all_questions(good_quiz_specs)
#' }
#'
translate_questions_api <- function(quiz_path) {

  quiz_specs <- ottrpal::parse_quiz(readLines(markdown_quiz_path()))

  # Remove header part and split into per question data frames
  question_dfs <- quiz_specs$data %>%
    dplyr::filter(question > 0) %>%
    dplyr::group_split(question)

  # Get prompt names
  question_names <- quiz_specs$data %>%
    dplyr::filter(type == "prompt") %>%
    dplyr::pull(original)

  # Get tags -> shuffle if needed
  shuffle_tag <- quiz_specs$data %>%
    dplyr::filter(type == "tag") %>%
    dplyr::mutate(shuffle_opt = grepl("choose-answers", trimmed)) %>%
    dplyr::filter(shuffle_opt) %>%
    dplyr::pull(question)

  # Get answer sets
  all_answers_df <- quiz_specs$data %>%
    dplyr::filter(grepl("_answer", type)) %>%
    dplyr::mutate(wording = stringr::word(original, sep = "\\) ", 2)) %>%
    dplyr::group_split(question)

  # We only want one correct answer
  choice_vectors <- lapply(all_answers_df, function(question_df) {
    wording <- question_df[c(which(question_df$type == "correct_answer")[1], which(question_df$type == "wrong_answer")), ] %>%
      dplyr::arrange(index) %>%
      dplyr::pull(wording)
    })

  correct_answer_index <- sapply(all_answers_df, function(question_df) {
    which(question_df$type == "correct_answer")[1]
  })

  question_names <- stringr::str_remove(question_names, "^\\? ")

  # Remove beginning format
  question_info_df <- data.frame(
    question = question_names,
    shuffle_opt = 1:length(all_answers_df) %in% shuffle_tag,
    correct_answer = correct_answer_index,
    row.names = question_names
    )

  return(list(question_info_df, choice_vectors))
}

#' Create quiz batch request from Markua quiz
#'
#' Takes output from [ottrpal::parse_quiz] and runs checks on each question in a quiz by calling [ottrpal::check_question] for each question.
#' First splits questions into their own data frame. Returns a list of messages/warnings about each question's set up.
#'
#' @param form_id quiz_specs which is output from [ottrpal::parse_quiz].
#' @param quiz_name The name of the quiz being checked.
#' @param verbose Whether progress messages should be given.
#' @param ignore_coursera Coursera doesn't like `!` or `:` in the quizzes. Do not convert quizzes to coursera and ignore ! and : in question prompts that would not be allowed in Leanpub quizzes when converted to a Coursera quiz. Default is to ignore Coursera compatibility.
#' @importFrom magrittr %>%
#' @return A list of the output from [ottrpal::check_question] with messages/warnings regarding each question and each check.
#'#'
#' @examples \dontrun{
#'
#' # Using quiz example
#'
#' quiz_path <- markdown_quiz_path()
#'
#' ottr_quiz_to_google(quiz_path)
#' }
#'
ottr_quiz_to_google <- function(course_id = NULL,
                                form_id = NULL,
                                make_new_quiz = FALSE) {

  if (is.null(form_id) && make_new_quiz == FALSE) {
    stop("No form ID supplied and make_new_quiz is set to FALSE. Stopping.")
  }

  if (!is.null(form_id) && make_new_quiz == TRUE) {
    stop("Form ID supplied and make_new_quiz is set to TRUE. Unclear if you want to create a new form or use the form_id you supplied. Stopping.")
  }

  if (make_new_quiz) {
    create_quiz()
  }


# For each question, add it to the batch request we are building
for (question_index in 1:nrow(formatted_df)) {
  create_multiple_choice_question(
    form_id = form_id,
    question = formatted_df$question[question_index],
    choice_vector = all_answers[[question_index]],
    correct_answer = formatted_df$correct_answer[question_index],
    shuffle_opt = formatted_df$shuffle_opt[[question_index]]
  )
}
}
