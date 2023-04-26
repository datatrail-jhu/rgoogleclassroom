# ottrpal pals

#' Translate Markua questions for submission to Google API
#'
#' Takes a Markua formatted quiz and translates it to something that can be sent to Google Forms API.
#'
#' @param quiz_path The file path to a Markua formatted quiz to be translated to Google Forms API format
#' @importFrom magrittr %>%
#' @return A list of the output from [ottrpal::check_question] with messages/warnings regarding each question and each check.
#'#'
#' @examples \dontrun{
#'
#' # Using quiz example
#'
#' quiz_path <- markdown_quiz_path()
#' parsed_questions <- translate_questions_api(quiz_path)
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
#' Takes a file path to a Markua formatted quiz and

#' @param quiz_path file path to a markdown Markua quiz
#' @param course_id An id for the course where this is to be published and linked.
#' @param form_id form id where this quiz is to be published. Alternatively, if you want a new quiz to be made, you should set make_new_quiz = TRUE and leave this NULL.
#' @param make_new_quiz This can only be used if form_id is not specified. This will make a new quiz
#' @importFrom magrittr %>%
#' @examples \dontrun{
#'
#' # Using quiz example
#'
#' quiz_path <- markdown_quiz_path()
#'
#' ottr_quiz_to_google(quiz_path)
#' }
#'
ottr_quiz_to_google <- function(quiz_path = NULL,
                                course_id = NULL,
                                form_id = NULL,
                                make_new_quiz = FALSE) {

  if (is.null(form_id) && make_new_quiz == FALSE) {
    stop("No form ID supplied and make_new_quiz is set to FALSE. Stopping.")
  }

  if (!is.null(form_id) && make_new_quiz == TRUE) {
    stop("Form ID supplied and make_new_quiz is set to TRUE. Unclear if you want to create a new form or use the form_id you supplied. Stopping.")
  }

  if (make_new_quiz) {
    extract_title <- grep("^#", readLines(quiz_path), value = TRUE)

    extract_title <- stringr::word(extract_title, sep = "# ", -1)

    quiz_id <- create_quiz(course_id,
                           quiz_title = extract_title,
                           quiz_description = "")

    form_id <- quiz_id$formId
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
