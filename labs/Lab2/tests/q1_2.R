test = list(
  name = "q1_2",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 2.0,
      code = {
        testthat::expect_equal(TypeI_t, 0.0674)
      }
    )
  )
)