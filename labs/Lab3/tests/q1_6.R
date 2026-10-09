test = list(
  name = "q1_6",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0,
      code = {
        testthat::expect_true(is.function(cohens_d_func))
        testthat::expect_equal(length(cohens_d_result), 2)
        testthat::expect_equal(as.numeric(cohens_d_func(c(-1, 0, 1, 0), mu_0=0)[1]), 0)
        testthat::expect_true(is.na(cohens_d_func(c(1,1), mu_0=0)))
        expected_d <- abs(mean(app_large_sample))/sd(app_large_sample)
        expected_class <- if (expected_d < .2) "very small" else if (expected_d < .5) "small" else if (expected_d < .8) "medium" else "large"
        testthat::expect_equal(as.numeric(cohens_d_result[1]), expected_d, tolerance=1e-8)
        testthat::expect_equal(unname(cohens_d_result[2]), expected_class)

      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      code = {
        local({
        testthat::expect_true(is.function(cohens_d_func))
        set.seed(39025)
        for (offset in c(-1.1, -.3, 0, .1, .3, .6, 1.1)) {
          mu0 <- runif(1,-20,20); scale <- runif(1,.5,4)
          x <- mu0 + scale*(c(-1,0,1)+offset)
          expected <- abs(mean(x)-mu0)/sd(x)
          result <- cohens_d_func(x, mu0)
          testthat::expect_length(result, 2)
          testthat::expect_equal(as.numeric(result[1]), expected, tolerance=1e-8)
          transformed <- cohens_d_func(5*x-3, 5*mu0-3)
          testthat::expect_equal(as.numeric(transformed[1]), expected, tolerance=1e-8)
        }
        })

      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.75,
      code = {
        local({
        testthat::expect_true(is.function(cohens_d_func))
        for (d in c(0,.19,.2,.49,.5,.79,.8,1.1)) {
          x <- c(-1,0,1)
          expected <- if (d<.2) "very small" else if (d<.5) "small" else if (d<.8) "medium" else "large"
          result <- cohens_d_func(x, mu_0=-d)
          testthat::expect_length(result, 2)
          testthat::expect_equal(unname(result[2]), expected)
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
        testthat::expect_true(is.function(cohens_d_func))
        for (x in list(c(4,4,4), c(-3,-3), 4, numeric(0))) {
          result <- cohens_d_func(x, mu_0=0)
          testthat::expect_length(result, 1)
          testthat::expect_true(is.na(result))
        }
        })

      }
    )
  )
)