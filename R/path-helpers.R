#' Path helpers
#' 
#' Expand path to code file location on a PL grader container.
#' 
#' @param file_name  Name of the file to construct the file path. 
#' @param fixed_path Create an absolute (e.g. root `/` path) or relative. 
#'                   Default is an absolute path. 
#' 
#' @export
#' @rdname path_helper
path_student = function(file_name, fixed_path = TRUE) {
  if(fixed_path) {
    file.path("", "grade", "student", file_name)
  } else {
    # Assumes working directory is /grade/run/testthat
    file.path("..", "..", "student", file_name)
  }
}

#' @export
#' @rdname path_helper
path_answer = function(file_name, fixed_path = TRUE) {
  if(fixed_path) {
    file.path("", "grade", "tests", "testthat", file_name)
  } else {
    # Assumes working directory is /grade/run/testthat
    file.path(file_name)
  }
}
