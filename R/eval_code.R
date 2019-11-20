#' Secured Code Evaluation
#' 
#' Process the code using a different user outside of the root configuration. 
#' 
#' @param file_name,file_path Name of the file or path to the file location. 
#' @param expr                Code to evaluate.
#' @param user_id             Name of the user to run the code under
#' @param fixed_path          Ensure an absolute or relative path to file. 
#' 
#' @details 
#' 
#' Understanding the function wrappers: 
#' 
#' - `eval_code_secure()` is the workhorse engine. 
#' - `eval_student_code()` provides an interface to run **student code**
#'    relative to where it resides on the path. 
#' - `eval_answer_code()` provides an interface to run **answer key** code 
#'   relative to where it resides on the path. 
#' 
#' @section Code Evaluation: 
#' 
#' Place any code that should be evaluated within curly brackets, e.g. `{}`.
#' 
#' @return 
#' Result of executed code. 
#' 
#' @export
#' @rdname eval_code
#' @examples
#' 
#' \dontrun{
#' # Obtain student results
#' eval_student_code("student_code.R", { cat("hello!") })
#' }
#' 
#' \dontrun{
#' # Obtain solution results
#' eval_student_code("student_code.R", { cat("hello!") })
#' }
#' 
#' \dontrun{
#' # Generalize it to work with any user.
#' eval_student_code("student_code.R", { cat("hello!") }, user_id = "anyone")
#' }
eval_student_code = function(file_name, expr, user_id = "ag", fixed_path = FALSE) {
  eval_code_secure(path_student(file_name, fixed_path = fixed_path), expr, user_id = user_id)
}


#' @export
#' @rdname eval_code
eval_answer_code = function(file_name, expr, user_id = "root", fixed_path = FALSE) { 
  eval_code_secure(path_answer(file, fixed_path = fixed_path), expr, user_id = user_id)
}

#' @export
#' @rdname eval_code
eval_code_secure = function(file_path, expr, user_id = unix::getuid()) {

  if (class(user_id) != "integer") { 
    user_id = unix::user_info(user_id)$uid
  }
  
  if (!file.exists(file_path)) {
    stop("Must provide a valid file path in `file_path`. Supplied path was:", file_path, call. = FALSE)
  }
  
  # From here, we drop down from running the process at ROOT level. 
  # In particular, we award:
  # -rw-rw-r--
  # This prevents "other" write, and prohibits execution. 
  # Moreover, it allows for file access but not directory access. 
  init_file_mode = Sys.chmod(file_path, mode = "0664")
  
  # TODO: Include within the expr call for sanity sake. 
  source(file_path)
  secured_eval_results = unix::eval_safe(expr, uid = user_id)
  Sys.chmod(file_path, mode = init_file_mode)
  
  secured_eval_results
}
