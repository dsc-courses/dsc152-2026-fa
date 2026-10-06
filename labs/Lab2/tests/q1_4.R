test = list(
  name = "q1_4",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 3.0,
      code = {
        testthat::expect_equal(length(gam_data), 30)
        testthat::expect_true(length(boot_means) >= 1000)
        testthat::expect_true(TypeI_bootstrap > 0.07)
        testthat::expect_true(TypeI_bootstrap < 0.10)
      }
    )
  )
)