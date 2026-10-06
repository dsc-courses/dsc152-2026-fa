test = list(
  name = "q1_3_1",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1.0,
      code = {
        testthat::expect_equal(alpha, pbinom(9, size=30, prob=0.5)*2)
      }
    )
  )
)