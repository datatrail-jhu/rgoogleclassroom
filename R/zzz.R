.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Please use predefined Credentials only for the testing requests. To obtain your own Credentials see help(authorize).")
}

.onLoad <- function(libname, pkgname) {
  op <- options()
  op.classroom <- list(
    classroom.client.id = "399382401954-f98jfqie6uuvlg5pl6td7i5efn2hi8hu.apps.googleusercontent.com",
    classroom.client.secret = "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",
    classroom.endpoint.create = "https://classroom.googleapis.com/v1/course",
    classroom.endpoint.get = "https://classroom.googleapis.com/v1/course/{courseId}",
    classroom.endpoint.topic.get = "https://classroom.googleapis.com/v1/courses/{courseId}/topics/{topicId}"
  )
  toset <- !(names(op.classroom) %in% names(op))
  if (any(toset)) options(op.classroom[toset])

  invisible()
}
