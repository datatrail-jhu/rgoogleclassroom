# ottrpal pals

#' Translate Markua questions for submission to Google API
#'
#' Takes a Markua formatted quiz and translates it to something that can be sent to Google Forms API.
#'
#' @param quiz_path The file path to a Markua formatted quiz to be translated to Google Forms API format
#' @param output_path Optional file path to save the formatted data to a JSON file
#' @importFrom magrittr %>%
#' @return A list of the output from [ottrpal::check_question] with messages/warnings regarding each question and each check.
#' @export
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

    if (inherits(file_path_works, "try-error")) {
      stop("output_path designated won't be writeable")
    }
  }

  # Read in the quiz file
  quiz_specs <- ottrpal::parse_quiz(readLines(quiz_path))

  # Remove header part and split into per question data frames
  question_dfs <- quiz_specs$data %>%
    dplyr::filter(question > 0) %>%
    dplyr::mutate(question_type = dplyr::case_when(
      grepl("fill_in_blank_answer", type) ~ "text",
      grepl("correct_answer|wrong_answer", type) ~ "multiple_choice"
    )) %>%
    dplyr::group_split(question)

  question_type <- sapply(question_dfs, function(question_df) {
    question_type <- question_df %>%
      dplyr::select(question_type) %>%
      dplyr::filter(!is.na(question_type)) %>%
      dplyr::pull(question_type)

    return(unique(question_type))
  })

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
    dplyr::mutate(wording = stringr::word(original, sep = "\\) ", 2))

  all_answers_df <- all_answers_df %>%
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

  unlist(all_answers_df)

  # Remove beginning format
  question_info_df <- data.frame(
    question = question_names,
    question_type,
    shuffle_opt = 1:length(all_answers_df) %in% shuffle_tag,
    correct_answer = correct_answer_index,
    row.names = question_names
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

#' Create Google Form Quiz from Markua quiz
#'
#' @description Takes a file path to a Markua formatted quiz and runs the steps to convert it to a Google Form Request and sends
#' It to be a Google form quiz.

#' @param quiz_path file path to a markdown Markua quiz
#' @param course_id An id for the course where this is to be published and linked.
#' @param topic_id topic ID that the quiz should be added under.
#' @param quiz_title The title for the quiz. If not supplied, it will attempt to be grabbed from the Markua doc
#' @param coursework_title the title for the coursework to be created
#' @param form_id form id where this quiz is to be published. Alternatively, if you want a new quiz to be made, you should set make_new_quiz = TRUE and leave this NULL.
#' @param make_new_quiz This can only be used if form_id is not specified. This will make a new quiz
#' @param copy_from_template_quiz TRUE or FALSE the form supplied should be copied over and used as a template.
#' @param new_name To be passed to `copy_form` if `copy_from_template_quiz` is TRUE. What the new file name should be called
#' @param due_date A due date for this quiz, in year-month-day format
#' @param quiz_description The description that will be given for the quiz
#' @param assignment_description The description that will be given for the assignment
#' @param output_path Optional file path to save the question formatted data to
#' @param quiet TRUE/FALSE you'd like a progress message?
#' @importFrom magrittr %>%
#' @export
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
                                copy_from_template_quiz = TRUE,
                                new_name = NULL,
                                assignment_description = "",
                                quiz_description = "",
                                output_path = NULL,
                                quiet = FALSE) {
  if (is.null(due_date)) {
    stop("Due date must be set. Use the due_date argument.")
  }
  if (is.null(form_id) && make_new_quiz == FALSE) {
    stop("No form ID supplied and make_new_quiz is set to FALSE. Stopping.")
  }
  if (!is.null(form_id) && make_new_quiz == TRUE) {
    stop("Form ID supplied and make_new_quiz is set to TRUE. Unclear if you want to create a new form or use the form_id you supplied. Stopping.")
  }
  if (copy_from_template_quiz && is.null(form_id)) {
    stop("copy_from_template_quiz is set to TRUE but no form_id to identify what to copy has been supplied")
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

  if (copy_from_template_quiz) {
    if (is.null(title)) {
      extract_title <- grep("^#", readLines(quiz_path), value = TRUE)
      extract_title <- stringr::word(extract_title, sep = "# ", -1)
      title <- extract_title
    }
    new_quiz <- copy_form(
      form_id = form_id,
      new_name = new_name
    )

    if (!is.null(quiz_title)) {
      update_form_settings(form_id = new_quiz$id, title = quiz_title)
    }

    if (!is.null(quiz_description)) {
      update_form_settings(form_id = new_quiz$id, description = quiz_description)
    }

    quiz_link <- paste0("https://docs.google.com/forms/d/", new_quiz$id, "/viewform")

    create_coursework(course_id,
      title = coursework_title,
      topic_id = topic_id,
      due_date = due_date,
      description = assignment_description,
      link = quiz_link
    )

    form_id <- new_quiz$id
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
    if (formatted_list$question_info_df$question_type[question_index] == "multiple_choice") {
      create_multiple_choice_question(
        form_id = form_id,
        question = formatted_list$question_info_df$question[question_index],
        choice_vector = formatted_list$choice_vectors[[question_index]],
        correct_answer = formatted_list$question_info_df$correct_answer[question_index],
        shuffle_opt = formatted_list$question_info_df$shuffle_opt[[question_index]],
        google_forms_request = google_forms_request,
        commit_to_form = FALSE,
        location = (question_index - 1)
      )
    } else if (formatted_list$question_info_df$question_type[question_index] == "text") {
      create_text_question(
        form_id = form_id,
        question = formatted_list$question_info_df$question[question_index],
        google_forms_request = google_forms_request,
        commit_to_form = FALSE,
        location = (question_index - 1)
      )
    }
  }

  result <- commit_to_form(form_id, google_forms_request, quiet = quiet)

  return(result)
}
