test = list(
  name = "q2_3_2",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.5,
      code = {
        testthat::expect_equal(q2_3_2_ans,2)

      }
    )
  )
)