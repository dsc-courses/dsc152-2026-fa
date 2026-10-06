test = list(
  name = "q1_5",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1.0,
      code = {
        testthat::expect_equal(typeI_n, 2)
      }
    )
  )
)