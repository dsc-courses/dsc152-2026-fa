# Preserve Otter's grading; show failures from zero-point feedback checks too.
# Ottr 1.5.3 otherwise bases "All tests passed!" on the points earned.
local({
  result_class <- get("TestFileResult", envir = asNamespace("ottr"))
  result_class$set("public", "repr", function() {
    messages <- character()
    for (result in self$test_case_results) {
      message <- result$get_message()
      if (!is.na(message)) messages <- c(messages, message)
    }
    messages <- paste(messages, collapse = "\n")
    if (messages != "") messages <- paste0(messages, "\n")

    passed <- vapply(self$test_case_results,
                     function(result) isTRUE(result$passed), logical(1))
    if (all(passed)) return(paste0(messages, "All tests passed!"))

    output <- vapply(self$test_case_results,
                     function(result) result$repr(), character(1))
    paste(output, collapse = "\n\n")
  }, overwrite = TRUE)
})
