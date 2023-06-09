% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ottrpal-translate.R
\name{ottr_quiz_to_google}
\alias{ottr_quiz_to_google}
\title{Create Google Form Quiz from Markua quiz}
\usage{
ottr_quiz_to_google(
  quiz_path = NULL,
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
  quiet = FALSE
)
}
\arguments{
\item{quiz_path}{file path to a markdown Markua quiz}

\item{course_id}{An id for the course where this is to be published and linked.}

\item{quiz_title}{The title for the quiz. If not supplied, it will attempt to be grabbed from the Markua doc}

\item{topic_id}{topic ID that the quiz should be added under.}

\item{coursework_title}{the title for the coursework to be created}

\item{form_id}{form id where this quiz is to be published. Alternatively, if you want a new quiz to be made, you should set make_new_quiz = TRUE and leave this NULL.}

\item{due_date}{A due date for this quiz, in year-month-day format}

\item{make_new_quiz}{This can only be used if form_id is not specified. This will make a new quiz}

\item{copy_from_template_quiz}{TRUE or FALSE the form supplied should be copied over and used as a template.}

\item{new_name}{To be passed to `copy_form` if `copy_from_template_quiz` is TRUE. What the new file name should be called}

\item{assignment_description}{The description that will be given for the assignment}

\item{quiz_description}{The description that will be given for the quiz}

\item{output_path}{Optional file path to save the question formatted data to}

\item{quiet}{TRUE/FALSE you'd like a progress message?}
}
\description{
Takes a file path to a Markua formatted quiz and runs the steps to convert it to a Google Form Request and sends
It to be a Google form quiz.
}
\examples{
\dontrun{

# Using quiz example

quiz_path <- markdown_quiz_path()

ottr_quiz_to_google(
  markdown_quiz_path(),
  course_id = "606463350924",
  make_new_quiz = TRUE,
  due_date = "2025-12-1"
)
}

}
