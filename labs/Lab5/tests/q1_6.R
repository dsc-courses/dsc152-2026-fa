test = list(
  name = "q1_6",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      code = {
        testthat::expect_equal(as.numeric(species_conclusion), 1)

      }
    )
  )
)