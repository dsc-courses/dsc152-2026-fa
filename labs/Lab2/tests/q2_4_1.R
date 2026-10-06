test = list(
  name = "q2_4_1",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 2.0,
      code = {
        testthat::expect_equal(gamma_power, 0.7636)
      }
    )
  )
)