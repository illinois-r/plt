#' Retrieve question totals and test name 
#' 
#' Extracts from the test data the point value and title of the test.
#' 
#' @param test_data A `data.frame` containing the test information.
#' 
#' @return 
#' Modified `data.frame` with:
#' 
#' - `max_points`: Total points available for the question.
#' - `earned_points`: Amount of points student earned.
#' - `title`: Name of the test
#' 
#' @details
#' To facilitate this, we assume the format for the question is given as...
#'
#' NumericPoints: title
#'
#' We extract out from the test name both components.
#' 
#' @rdname test-metadata
#' @export
test_info = function(test_data) {
  return(test_points(test_title(test_data)))
}

test_title = function(test_data) {
  
  # Extract the title of the test
  test_data$title = gsub(".*[[:digit:]]+.*:[[:space:]](.*)", "\\1",
                         test_data$test)  
  
  return(invisible(test_data))
}

test_points = function(test_data) {
  
  # Obtain the maximum points
  max_points = as.numeric(gsub(".*([[:digit:]]+).*:.*", "\\1", test_data$test))
  
  verify_no_na = is.na(max_points)
  if (any(verify_no_na)) {
    message("The following tests did not have a numeric point value:\n\n")
    message(paste("  *", names(test_data)[verify_no_na, "title"], collapse = "\n"), "\n\n")
    message("Please fix the error and then submit again.\n")
    
    # Error
    q(save = "no", status = 1, runLast = FALSE)
  }
  
  # Calculate the number of points the student received
  test_data$earned_points = ifelse(test_data$passed == 1, max_points, 0.0)
  
  # Include max points for the question
  test_data$max_points = max_points
  
  return(invisible(test_data))
}