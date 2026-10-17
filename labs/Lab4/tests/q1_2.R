test = list(
  name = "q1_2",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.5,
      code = {
        testthat::expect_equal(as.numeric(ozone_change_20),20*as.numeric(solar_fit$beta1),tolerance=1e-6)
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.5,
      code = {
        testthat::expect_equal(sort(as.numeric(initial_statements)),c(1,3,5))
      }
    )
  )
)