test = list(
  name = "q2_2",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.5,
      code = {
        local({
            values <- d_result$group_sd
            testthat::expect_true(is.numeric(values) && !is.null(names(values)))
            valid <- is.finite(values)
            expected <- names(values)[valid & values == max(values[valid])]
            testthat::expect_setequal(as.character(largest_spread_species), expected)
        })
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.5,
      code = {
        local({
            testthat::expect_true(is.logical(normality_reject) && length(normality_reject) == 1L && !is.na(normality_reject))
            testthat::expect_equal(normality_reject, d_result$shapiro_p < 0.05)
        })
      }
    )
  )
)