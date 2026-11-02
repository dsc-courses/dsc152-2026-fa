test = list(
  name = "q1_5",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.5,
      code = {
        testthat::expect_equal(as.numeric(beta1_interpretation), 1)

      }
    )
  )
)