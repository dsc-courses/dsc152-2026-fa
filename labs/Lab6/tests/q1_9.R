test = list(
  name = "q1_9",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.5,
      code = {
        testthat::expect_equal(as.numeric(residual_sum_interp), 3)

      }
    )
  )
)