test = list(
  name = "q3_5",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.5,
      code = {
        testthat::expect_equal(q3_5_ans,2)

      }
    )
  )
)