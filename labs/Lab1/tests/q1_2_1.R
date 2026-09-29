test = list(
  name = "q1_2_1",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      code = {
        testthat::expect_equal(neg_ind, 2)
      }
    )
  )
)