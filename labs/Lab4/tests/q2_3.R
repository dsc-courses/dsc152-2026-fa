test = list(
  name = "q2_3",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.5,
      code = {
        local({
         expected <- as.character(rate_comparison$condition[rate_comparison$difference==max(rate_comparison$difference)])
         testthat::expect_equal(sort(as.character(largest_excess)),sort(expected))
        })
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.5,
      code = {
        testthat::expect_equal(sort(as.numeric(calibration_statements)),c(1,3))
      }
    )
  )
)