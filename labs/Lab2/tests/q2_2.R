test = list(
  name = "q2_2",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 3.0,
      code = {
        testthat::expect_equal(test_power, 0.2939)
      }
    )
  )
)