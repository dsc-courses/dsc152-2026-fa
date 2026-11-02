test = list(
  name = "q2_1",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.5,
      code = {
        testthat::expect_true(is.numeric(overall_ftest))
        testthat::expect_equal(sort(as.numeric(overall_ftest)), c(2, 3))

      }
    )
  )
)