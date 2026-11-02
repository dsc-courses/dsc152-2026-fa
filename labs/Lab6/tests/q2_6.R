test = list(
  name = "q2_6",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.5,
      code = {
        testthat::expect_equal(as.numeric(relevant_pvals_B), 1)

      }
    )
  )
)