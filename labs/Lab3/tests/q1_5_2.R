test = list(
  name = "q1_5_2",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0,
      code = {
        testthat::expect_true(is.function(one_sample_power))
        testthat::expect_equal(unname(power_tiny_conclusion), 3)
        expected_power <- vapply(c(FALSE, TRUE), function(strict) power.t.test(
            n=5000, delta=0.5, sd=45, sig.level=0.05, type="one.sample",
            alternative="two.sided", strict=strict)$power, numeric(1))
        testthat::expect_true(length(power_tiny_effect) == 1 && is.finite(power_tiny_effect))
        testthat::expect_true(any(abs(power_tiny_effect - expected_power) < 1e-6))

      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.75,
      code = {
        local({
        testthat::expect_true(is.function(one_sample_power))
        expected <- vapply(c(FALSE, TRUE), function(strict) power.t.test(n=5000, delta=.5, sd=45,
            sig.level=.05, type="one.sample", alternative="two.sided", strict=strict)$power, numeric(1))
        testthat::expect_true(is.numeric(power_tiny_effect) && length(power_tiny_effect)==1 && is.finite(power_tiny_effect))
        testthat::expect_true(any(abs(power_tiny_effect-expected)<1e-6))
        expected_fresh <- vapply(c(FALSE, TRUE), function(strict) power.t.test(n=6000, delta=.4, sd=40,
            sig.level=.05, type="one.sample", alternative="two.sided", strict=strict)$power, numeric(1))
        result <- one_sample_power(n=6000, delta=.4, sd=40)
        testthat::expect_true(is.numeric(result) && length(result)==1 && is.finite(result))
        testthat::expect_true(any(abs(result-expected_fresh)<1e-7))
        })

      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.75,
      code = {
        local({
        testthat::expect_equal(unname(power_tiny_conclusion), 3)
        })

      }
    )
  )
)