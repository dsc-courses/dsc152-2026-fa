test = list(
  name = "q2_1",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.5,
      code = {
        testthat::expect_equal(unname(ttest_conditions), c(1,4))
      }
    )
  )
)