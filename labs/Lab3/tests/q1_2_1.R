test = list(
  name = "q1_2_1",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0,
      code = {
        testthat::expect_true(is.function(one_sample_test))
        testthat::expect_equal(unname(p_val_large_n), t.test(app_large_sample, mu=0)$p.value, tolerance=1e-8)
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.5,
      code = {
        local({
        testthat::expect_true(is.function(one_sample_test))
        set.seed(39021)
        for (i in seq_len(6)) {
          x <- rnorm(sample(9:35, 1), runif(1, -2, 2), runif(1, 0.7, 3))
          expected <- t.test(x, mu=0, alternative="two.sided")$p.value
          testthat::expect_equal(unname(one_sample_test(x)$p.value), expected, tolerance=1e-8)
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
        testthat::expect_true(is.function(one_sample_test))
        set.seed(39031)
        for (i in seq_len(6)) {
          mu0 <- runif(1, -8, 8)
          x <- rnorm(sample(9:35, 1), mu0 + runif(1, -2, 2), runif(1, 0.7, 3))
          expected <- t.test(x, mu=mu0, alternative="two.sided")$p.value
          testthat::expect_equal(unname(one_sample_test(x, mu_0=mu0)$p.value), expected, tolerance=1e-8)
          testthat::expect_equal(unname(one_sample_test(3.5*x+17, mu_0=3.5*mu0+17)$p.value), expected, tolerance=1e-8)
        }
        })

      }
    )
  )
)