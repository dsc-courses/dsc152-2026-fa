test = list(
  name = "q2_4",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.5,
      code = {
        testthat::expect_equal(as.numeric(ftest_vs_ttest), 4)

      }
    )
  )
)