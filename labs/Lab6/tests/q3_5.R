test = list(
  name = "q3_5",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.25,
      code = {
        testthat::expect_equal(as.numeric(steps_equivalence_exp), 1)
      }
    )
  )
)