test = list(
  name = "q2_1",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      code = {
        testthat::expect_equal(u, 6:105,
                               info = "Check the loop endpoints: i should run from 1 through 100.")
      }
    )
  )
)