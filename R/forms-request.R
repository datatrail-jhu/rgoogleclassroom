#' @importFrom R6 R6Class
google_forms_request_container <- R6::R6Class("GoogleFormsRequest",
  public = list(
    requests = list(),
    add_request = function(request) {
      self$requests[[length(self$requests) + 1]] <- request
    },
    to_list = function() {
      return(self$requests)
    }
  )
)

#' Check if the object is a google forms request object
#' @param x A google_forms_request object created from the rgoogleclassroom package
#' @export
is.google_forms_request <- function(x) {
  "GoogleFormsRequest" %in% class(x)
}
