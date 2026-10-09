test = list(
  name = "q2_5",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.5,
      code = {
        testthat::expect_true(abs(p_val_ttest - p_val_perm) < 0.05)
        testthat::expect_equal(unname(p_val_check), 3)
      }
    )
  )
)