test = list(
  name = "q2_9",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.5,
      code = {
        testthat::expect_equal(unname(combined_power_conclusion), 4)
        testthat::expect_equal(nrow(power_curves), 36)
        testthat::expect_equal(sort(unique(power_curves$n)), n_vals)
        testthat::expect_equal(sort(unique(as.character(power_curves$distribution))), sort(c("Normal","Skewed","Heavy_tailed")))
        testthat::expect_equal(sort(unique(as.character(power_curves$test))), sort(c("t_test","permutation")))
        testthat::expect_true(all(is.finite(power_curves$power) & power_curves$power >= 0 & power_curves$power <= 1))

      }
    )
  )
)