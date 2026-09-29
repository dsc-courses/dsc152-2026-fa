test = list(
  name = "q3_3",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      code = {
        testthat::expect_equal(round(ts, 4), 0.4006,
                               info = "Use the current GPA values after the Part 1.4 examples.")
        testthat::expect_equal(calc_ts(c(1, 2, 3), 2), 0,
                               info = "The statistic should be zero when mean(x) equals mu0.")
        testthat::expect_equal(calc_ts(c(1, 2, 3), 1), sqrt(3),
                               info = "Check that your function uses both x and mu0, with sd(x)/sqrt(length(x)) in the denominator.")
      }
    )
  )
)