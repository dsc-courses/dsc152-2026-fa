test = list(
  name = "q1_7",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.25,
      code = {
        testthat::expect_equal(as.numeric(beta0_interpretation), 2)

      }
    )
  )
)