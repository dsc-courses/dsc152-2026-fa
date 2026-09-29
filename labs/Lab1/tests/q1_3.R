test = list(
  name = "q1_3",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      code = {
        testthat::expect_equal(mat_33, matrix(1:9, nrow=3, byrow=TRUE))
      }
    )
  )
)