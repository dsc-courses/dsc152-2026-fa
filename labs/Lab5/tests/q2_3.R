test = list(
  name = "q2_3",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      code = {
        testthat::expect_true(is.numeric(curvature_statements))
        testthat::expect_equal(sort(as.numeric(curvature_statements)), c(1, 4))

      }
    )
  )
)