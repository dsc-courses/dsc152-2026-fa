test = list(
  name = "q1_1",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.5,
      code = {
        testthat::expect_equal(unname(sig_eff_size), c(1,4,5))
      }
    )
  )
)