test = list(
  name = "q2_8",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.5,
      code = {
        testthat::expect_true(is.numeric(new_filter_effects))
        testthat::expect_equal(as.numeric(new_filter_effects), c(1, 0))

      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.75,
      code = {
        testthat::expect_true(is.numeric(new_test_types))
        testthat::expect_equal(as.numeric(new_test_types), c(2, 2))

      }
    )
  )
)