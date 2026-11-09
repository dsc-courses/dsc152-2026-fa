test = list(
  name = "q3_4",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      code = {
        testthat::expect_equal(q3_4_ans,2)

      }
    )
  )
)