test = list(
  name = "q1_5_1",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0,
      code = {
        testthat::expect_true(is.function(sample_size_for_power))
        testthat::expect_equal(unname(n_required), 1768)
        target_power <- function(n, strict=FALSE) power.t.test(n=n, delta=3, sd=45,
            sig.level=0.05, type="one.sample", alternative="two.sided", strict=strict)$power
        testthat::expect_true(any(abs(power_at_required - c(target_power(n_required), target_power(n_required, TRUE))) < 1e-6))
        testthat::expect_true(any(abs(power_one_fewer - c(target_power(n_required-1), target_power(n_required-1, TRUE))) < 1e-6))
        testthat::expect_gte(power_at_required, 0.8)
        testthat::expect_lt(power_one_fewer, 0.8)

      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.5,
      code = {
        local({
        testthat::expect_true(is.function(one_sample_power))
        set.seed(39023)
        for (i in seq_len(7)) {
          n <- sample(10:70, 1); delta <- runif(1, .1, 2); scale <- runif(1, 1, 5)
          expected <- vapply(c(FALSE, TRUE), function(strict) power.t.test(n=n, delta=delta, sd=scale,
              sig.level=.05, type="one.sample", alternative="two.sided", strict=strict)$power, numeric(1))
          result <- one_sample_power(n, delta, scale)
          testthat::expect_true(is.numeric(result) && length(result)==1 && is.finite(result))
          testthat::expect_true(any(abs(result-expected)<1e-7))
        }
        })

      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.25,
      code = {
        local({
        testthat::expect_true(is.function(one_sample_power))
        for (alpha in c(.01, .10)) {
          expected <- vapply(c(FALSE, TRUE), function(strict) power.t.test(n=37, delta=.8, sd=2.7,
              sig.level=alpha, type="one.sample", alternative="two.sided", strict=strict)$power, numeric(1))
          result <- one_sample_power(n=37, delta=.8, sd=2.7, alpha=alpha)
          testthat::expect_true(is.numeric(result) && length(result)==1 && is.finite(result))
          testthat::expect_true(any(abs(result-expected)<1e-7))
          testthat::expect_equal(one_sample_power(n=37, delta=3.2, sd=10.8, alpha=alpha), result, tolerance=1e-7)
        }
        })

      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.5,
      code = {
        local({
        testthat::expect_true(is.function(sample_size_for_power))
        set.seed(39024)
        for (i in seq_len(6)) {
          delta <- runif(1, .3, 2); scale <- runif(1, 1, 5); target <- sample(c(.70,.80,.90), 1)
          expected <- vapply(c(FALSE, TRUE), function(strict) ceiling(power.t.test(delta=delta, sd=scale,
              sig.level=.05, power=target, type="one.sample", alternative="two.sided", strict=strict)$n), numeric(1))
          result <- sample_size_for_power(delta, scale, target_power=target)
          testthat::expect_true(is.numeric(result) && length(result)==1 && is.finite(result))
          testthat::expect_true(result %in% expected)
        }
        })

      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.25,
      code = {
        local({
        testthat::expect_true(is.function(sample_size_for_power))
        for (alpha in c(.01, .10)) {
          expected <- vapply(c(FALSE, TRUE), function(strict) ceiling(power.t.test(delta=.7, sd=3.1,
              sig.level=alpha, power=.85, type="one.sample", alternative="two.sided", strict=strict)$n), numeric(1))
          result <- sample_size_for_power(delta=.7, sd=3.1, target_power=.85, alpha=alpha)
          testthat::expect_true(is.numeric(result) && length(result)==1 && is.finite(result))
          testthat::expect_true(result %in% expected)
          testthat::expect_equal(sample_size_for_power(delta=1.4, sd=6.2, target_power=.85, alpha=alpha), result)
        }
        })

      }
    )
  )
)