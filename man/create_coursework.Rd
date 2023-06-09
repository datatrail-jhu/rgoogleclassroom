% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coursework.R
\name{create_coursework}
\alias{create_coursework}
\title{Create a new coursework}
\usage{
create_coursework(
  course_id = NULL,
  topic_id = NULL,
  publish = FALSE,
  title = NULL,
  work_type = "ASSIGNMENT",
  due_date = NULL,
  description = NULL,
  link = NULL
)
}
\arguments{
\item{course_id}{Course id of where to make the new coursework. Can find from end of URL e.g. "https://classroom.google.com/c/COURSE_ID_IS_HERE"}

\item{topic_id}{topic ID to be looked for.}

\item{publish}{TRUE/FALSE, automatically publish the coursework upon posting? Default is to be posted as a draft (students will not see it until you click Post).}

\item{title}{Name of new coursework. Required.}

\item{work_type}{Currently only supported work type is ASSIGNMENT.}

\item{due_date}{Required Due date for new coursework, must be given in year-month-day format.}

\item{description}{Description of new coursework. Is a string}

\item{link}{A url to an associated resource for the coursework being made.}
}
\description{
Create a new coursework
}
\examples{
\dontrun{
topic_id <- get_topic_list("604042323237")$topic$topicId[1]
course_id <- get_course_list()$courses$id[1]

create_coursework(course_id, topic_id,
  title = "a new quiz", due_date = "2025-12-1",
  description = "blah blah", link = "https://www.datatrail.org/"
)
}

}
