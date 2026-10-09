test = list(
  name = "q1_3",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1.0,
      code = {
        testthat::expect_equal(unname(interp_large_n), 2)
      }
    )
  )
)