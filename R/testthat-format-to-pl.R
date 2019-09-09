#' Construct the PrairieLearn output format JSON
#' 
#' Interprets and builds the appropriate message for each test result in
#' the PrairieLearn format.
#' 
#' @inheritParams test_info
#' 
#' @return 
#' Formats the `data.frame` into a `list` for export into JSON.
format_testthat_to_pl = function(test_data) {
  
  # Construct the overarching test messages
  individual_tests = lapply(seq_len(nrow(test_data)), 
                            FUN = pl_test_list_format,
                            x = test_data)
  
  # Provide the per test message formatting
  merge_per_test_output =
    paste0(
      sapply(seq_len(nrow(test_data)),
             FUN = pl_per_test_output,
             x = test_data),
      collapse = ""
    )
  
  # A list is used instead of a `data.frame`` due to `jsonlite` formatter
  # encasing `data.frame` objects within a JSON array.
  pl_output_format =
    list(
      "succeeded" = TRUE,
      "score" = sum(test_data$earned_points)/sum(test_data$max_points),
      "message" = "Questions were externally graded",
      "output" = paste0(
        "Running tests...\n", merge_per_test_output
      ),
      "tests" = individual_tests
    )
  
  return(pl_output_format)
}

## Format the Results to Match External Grading Format ----

pl_message_statement = function(x) {
  if (isTRUE(x$error)) {
    test_stack = unlist(x$result, recursive = FALSE)
    error_message = if (!is.null(test_stack) && length(test_stack) != 0) {
      format(test_stack[[1]])
    } else {
      "Error message was unrecoverable."
    }
    sprintf("Your code encounted an error.\n %s", error_message)
  } else if (x$failed >= 1) {
    "The code ran but did not produce the correct result."
  } else {
    "No errors!"
  }
}

pl_output_statement = function(x) {
  condition = "matched"
  check_statement = ""
  
  if(x$passed == 0) {
    condition = "did not match"
    test_stack = unlist(x$result, recursive = FALSE)
    
    check_statement = if (!is.null(test_stack) && length(test_stack) != 0) {
      format(x$result[[1]][[1]][["expectation_calls"]][[1]])
    } else {
      "Unable to recover the check statement."
    }
    
  }
  sprintf("Running test...\nYour output %s the expected output. \n%s", 
          condition, check_statement)
}

# Format the results of each inline test.
pl_test_list_format = function(i, x) {
  list("name" = paste("Test", i),
       "description" = x[i, "title"],
       "points" = x[i, "earned_points"],
       "max_points" = x[i, "max_points"],
       "message" =  pl_message_statement(x[i, , drop = FALSE]),
       "output" = pl_output_statement(x[i, , drop = FALSE])
  )
}

# Provide an overall concatentation of test data
pl_per_test_output = function(i, x) {
  condition = if ( x$passed[i] >= 1 && isFALSE(x$error[i]) ) {
    "passed..."
  } else {
    "failed!"
  }
  sprintf("Test %i %s\n", i, condition)
}

