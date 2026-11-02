test = list(
  name = "q2_2",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.5,
      code = {
        testthat::expect_equal(as.numeric(filter_contribution_null), 3)

      }
    )
  )
)