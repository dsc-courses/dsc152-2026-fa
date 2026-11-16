test = list(
  name = "q1_10",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.5,
      code = {
        testthat::expect_equal(q1_10_ans,1)

      }
    )
  )
)