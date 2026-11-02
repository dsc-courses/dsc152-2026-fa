test = list(
  name = "q3_8",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.5,
      code = {
        testthat::expect_true(is.numeric(bootstrap_interpretation))
        testthat::expect_equal(sort(as.numeric(bootstrap_interpretation)), c(1, 3, 5))

      }
    )
  )
)