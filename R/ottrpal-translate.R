# ottrpal pals

#' Translate Markua questions for submission to Google API
#'
#' Takes a Markua formatted quiz and translates it to something that can be sent to Google Forms API.
#'
#' @param quiz_path The file path to a Markua formatted quiz to be translated to Google Forms API format
#' @param output_path Optional file path to save the formatted data to a JSON file
#' @importFrom magrittr %>%
#' @return A list of the output from [ottrpal::check_question] with messages/warnings regarding each question and each check.
#' #'
#' @examples \dontrun{
#'
#' # Using quiz example
#'
#' quiz_path <- markdown_quiz_path()
#' parsed_questions <- translate_questions_api(quiz_path)
#' }
#'
translate_questions_api <- function(quiz_path, output_path = NULL) {
  # Make sure the file path provided is useable
  if (!is.null(output_path)) {
    file_path_works <- try(write(jsonlite::toJSON(list()), output_path))

    if (class(file_path_works) == "try-error") {
      stop("output_path designated won't be writeable")
    }
  }

  # Read in the quiz file
  quiz_specs <- ottrpal::parse_quiz(readLines(quiz_path))

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
    row.names = paste0(1:length(question_names), question_names)
  )

  # Store as a list
  question_data <- list(question_info_df = question_info_df, choice_vectors = choice_vectors)

  if (!is.null(output_path)) {
    write(
      jsonlite::toJSON(question_data),
      output_path
    )
  }

  return(question_data)
}

#' Create quiz batch request from Markua quiz
#'
#' Takes a file path to a Markua formatted quiz and

#' @param quiz_path file path to a markdown Markua quiz
#' @param course_id An id for the course where this is to be published and linked.
#' @param topic_id Optional a topic Id to put the quiz underneath
#' @param quiz_title The title for the quiz. If not supplied, it will attempt to be grabbed from the Markua doc
#' @param coursework_title the title for the coursework to be created
#' @param form_id form id where this quiz is to be published. Alternatively, if you want a new quiz to be made, you should set make_new_quiz = TRUE and leave this NULL.
#' @param make_new_quiz This can only be used if form_id is not specified. This will make a new quiz
#' @param due_date A due date for this quiz, in year-month-day format
#' @param quiz_description The description that will be given for the quiz
#' @param assignment_description The description that will be given for the assignment
#' @param output_path Optional file path to save the question formatted data to
#' @importFrom magrittr %>%
#' @examples \dontrun{
#'
#' # Using quiz example
#'
#' quiz_path <- markdown_quiz_path()
#'
#' ottr_quiz_to_google(
#'   markdown_quiz_path(),
#'   course_id = "606463350924",
#'   make_new_quiz = TRUE,
#'   due_date = "2025-12-1"
#' )
#' }
#'
ottr_quiz_to_google <- function(quiz_path = NULL,
                                course_id = NULL,
                                quiz_title = NULL,
                                topic_id = NULL,
                                coursework_title = NULL,
                                form_id = NULL,
                                due_date = NULL,
                                make_new_quiz = FALSE,
                                assignment_description = "",
                                quiz_description = "",
                                output_path = NULL) {
  if (is.null(due_date)) {
    stop("Due date must be set. Use the due_date argument.")
  }
  if (is.null(form_id) && make_new_quiz == FALSE) {
    stop("No form ID supplied and make_new_quiz is set to FALSE. Stopping.")
  }
  if (!is.null(form_id) && make_new_quiz == TRUE) {
    stop("Form ID supplied and make_new_quiz is set to TRUE. Unclear if you want to create a new form or use the form_id you supplied. Stopping.")
  }
  if (!is.null(output_path)) {
    file.access(output_path, mode = 2)
  }

  if (make_new_quiz) {
    if (is.null(title)) {
      extract_title <- grep("^#", readLines(quiz_path), value = TRUE)
      extract_title <- stringr::word(extract_title, sep = "# ", -1)
      title <- extract_title
    }
    new_quiz <- create_quiz(course_id,
      quiz_title = quiz_title,
      coursework_title = coursework_title,
      topic_id = topic_id,
      due_date = due_date,
      assignment_description = assignment_description,
      quiz_description = quiz_description
    )

    form_id <- new_quiz$form_info$formId
  }

  # Format the questions and save to RDS
  formatted_list <- translate_questions_api(
    quiz_path,
    output_path
  )

  # Create a new google form request
  google_forms_request <- google_forms_request_container$new()

  # For each question, add it to the batch request we are building
  for (question_index in 1:nrow(formatted_list$question_info_df)) {
    create_multiple_choice_question(
      form_id = form_id,
      question = formatted_list$question_info_df$question[question_index],
      choice_vector = formatted_list$choice_vectors[[question_index]],
      correct_answer = formatted_list$question_info_df$correct_answer[question_index],
      shuffle_opt = formatted_list$question_info_df$shuffle_opt[[question_index]],
      google_forms_request = google_forms_request,
      location = (question_index - 1)
    )
  }
  # Add to the form
  result <- commit_to_form(form_id = form_id, google_forms_request)

  return(result)
}
