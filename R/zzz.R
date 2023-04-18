.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Please use predefined Credentials only for the testing requests. To obtain your own Credentials see help(authorize).")
}

.onLoad <- function(libname, pkgname) {
  op <- options()
  op.classroom <- list(
    classroom.endpoint.user = "https://classroom.googleapis.com/v1/userProfiles/me",
    classroom.endpoint.create = "https://classroom.googleapis.com/v1/courses",
    classroom.endpoint.get = "https://classroom.googleapis.com/v1/courses",
    classroom.endpoint.get.course = "https://classroom.googleapis.com/v1/courses/{courseId}",
    classroom.endpoint.topic.get = "https://classroom.googleapis.com/v1/courses/{courseId}/topics/{topicId}"
  )
  toset <- !(names(op.classroom) %in% names(op))
  if (any(toset)) options(op.classroom[toset])

  invisible()
}
