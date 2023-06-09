% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{get_endpoint}
\alias{get_endpoint}
\title{Generate endpoint for the Google classroom API}
\usage{
get_endpoint(
  type_of_endpoint = "classroom.endpoint.user",
  course_id = NULL,
  topic_id = NULL,
  coursework_id = NULL,
  materials_id = NULL,
  form_id = NULL
)
}
\arguments{
\item{type_of_endpoint}{Type of endpoint to convert to url}

\item{course_id}{(Optional) ID of the google course to be affected/retrieved}

\item{topic_id}{(Optional) ID of the topic to be affected/retrieved}

\item{coursework_id}{(Optional) ID of the coursework to be affected/retrieved}

\item{materials_id}{(Optional) ID of the material to be affected/retrieved}

\item{form_id}{(Optional) ID of the form to be affected/retrieved}
}
\value{
A url that is the endpoint for the API call
}
\description{
Generate endpoint for the Google classroom API
}
