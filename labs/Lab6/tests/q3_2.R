test = list(
  name = "q3_2",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.25,
      code = {
        testthat::expect_equal(as.numeric(no_interaction_assumption), 2)
      }
    )
  )
)