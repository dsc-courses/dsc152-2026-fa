test = list(
  name = "q1_6",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.75,
      code = {
        testthat::expect_equal(as.numeric(beta3_interpretation), 4)

      }
    )
  )
)