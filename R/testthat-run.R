#' Automatically Test a Set of Unit Tests in a Directory
#' 
#' Parses a set of files found in the `testthat` directory inside a docker
#' container.
#' 
#' @param tests_path     Directory where unit tests are written
#' 
#' @return 
#' 
#' A `list` with a format that matches the external grading specification. 
#' See the details section for more information.
#' 
#' @section Directory Testing:
#' By default, `testthat` has a preference to run tests within an _R_ package.
#' This approach uncouples the need for an _R_ package. Details for testing
#' outside of an _R_ package can be found at:
#' <https://github.com/r-lib/testthat/issues/659>
#' 
#' @details 
#' 
#' TODO: Add details or link out.
#' <https://prairielearn.readthedocs.io/en/latest/externalGrading/#grading-results>
#' 
#' @export
run_testthat = function(tests_path = "testthat") {
  
  # Begin the process of grading each student's code
  # Add a capture statement to avoid allowing students to output
  # content to console.
  log <- utils::capture.output({
    test_data <- as.data.frame(testthat::test_dir(tests_path,
                              reporter = CustomListReporter))
  })
  
  test_results = format_testthat_to_pl(test_data)
  
  # Output is manipulated from the run.sh file into results.json
  jsonlite::write_json(
    test_results,         # Pre-formatted data in a data.frame
    path = stdout(),      # Dump into Standard Out (STDOUT) to avoid tracking files.
    pretty = TRUE,        # Make the JSON legible
    auto_unbox = TRUE,    # Remove `[]` around single element in a vector
    force = TRUE,         # Avoid unclassing any item without a direct map
    always_decimal = TRUE # Required due to 0's not being coded as 0.0
  )
  
  invisible(test_results)
}
