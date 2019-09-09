#' List reporter: gather all test results along with elapsed time and
#' file information.
#'
#' This reporter gathers all results, adding additional information such as
#' test elapsed time, and test filename if available. Very useful for reporting.
#'
#' @export
#' @family reporters
CustomListReporter <- R6::R6Class("CustomListReporter",
                            inherit = testthat::Reporter,
                            public = list(
                              current_start_time = NA,
                              current_expectations = NULL,
                              current_file = NULL,
                              results = NULL,
                              
                              initialize = function() {
                                super$initialize()
                                self$results <- Stack$new()
                              },
                              
                              start_test = function(context, test) {
                                self$current_expectations <- Stack$new()
                                self$current_start_time <- proc.time()
                              },
                              
                              add_result = function(context, test, result) {
                                if (is.null(self$current_expectations)) {
                                  # we received an unexpected result: could be a bare expectation or an exception/error
                                  if (!inherits(result, 'error')) return()
                                  self$current_expectations <- Stack$new()
                                }
                                
                                self$current_expectations$push(result)
                              },
                              
                              end_test = function(context, test) {
                                elapsed <- as.double(proc.time() - self$current_start_time)
                                
                                results <- list()
                                if (!is.null(self$current_expectations))
                                  results <- self$current_expectations$as_list()
                                
                                self$results$push(list(
                                  file =    self$current_file %||% NA_character_,
                                  context = context,
                                  test =    test,
                                  user =    elapsed[1],
                                  system =  elapsed[2],
                                  real =    elapsed[3],
                                  results = results
                                ))
                                
                                self$current_expectations <- NULL
                              },
                              
                              start_file = function(name) {
                                self$current_file <- name
                              },
                              
                              end_context = function(context) {
                                results <- self$current_expectations
                                if (is.null(results)) return()
                                
                                self$current_expectations <- NULL
                                
                                # look for exceptions raised outside of tests
                                # they happened just before end_context since they interrupt the test_file execution
                                results <- results$as_list()
                                if (length(results) == 0) return()
                                
                                self$results$push(list(
                                  file =    self$current_file %||% NA_character_,
                                  context = context,
                                  test =    NA_character_,
                                  user =    NA_real_,
                                  system =  NA_real_,
                                  real =    NA_real_,
                                  results = results
                                ))
                              },
                              
                              get_results = function() {
                                testthat_results(self$results$as_list())
                              }
                              
                            )
)
