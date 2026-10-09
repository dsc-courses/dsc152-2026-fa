test = list(
  name = "q1_4_2",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.5,
      code = {
        testthat::expect_equal(unname(p_val_comparison), 3)
      }
    )
  )
)