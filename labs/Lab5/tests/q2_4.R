test = list(
  name = "q2_4",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1.5,
      code = {
        local({

            reference_results <- function(n, beta1, beta2 = 0, sigma = 5,
                                          unequal = FALSE, reps = 20000L) {
                x <- matrix(runif(n * reps, -5, 5), nrow = n)
                error_sd <- if (unequal) 0.5 + 2 * abs(x) else sigma
                y <- beta1 * x + beta2 * x^2 + matrix(rnorm(n * reps), nrow = n) * error_sd
                xc <- sweep(x, 2L, colMeans(x), "-")
                yc <- sweep(y, 2L, colMeans(y), "-")
                xx <- colSums(xc^2)
                xy <- colSums(xc * yc)
                yy <- colSums(yc^2)
                slopes <- xy / xx
                linear_ss <- xy^2 / xx
                linear_rss <- pmax(0, yy - linear_ss)
                t_squared <- linear_ss / (linear_rss / (n - 2))
                x2c <- sweep(x^2, 2L, colMeans(x^2), "-")
                x2orth <- x2c - sweep(xc, 2L, colSums(xc * x2c) / xx, "*")
                quadratic_ss <- linear_ss + colSums(x2orth * yc)^2 / colSums(x2orth^2)
                quadratic_rss <- pmax(0, yy - quadratic_ss)
                f <- (quadratic_ss / 2) / (quadratic_rss / (n - 3))
                list(linear_rate = mean(t_squared > qf(0.95, 1, n - 2)),
                     quadratic_rate = mean(f > qf(0.95, 2, n - 3)),
                     mean_slope = mean(slopes), sd_slope = sd(slopes))
            }

            testthat::expect_true(is.function(sim_mean_tests))
            settings <- list(
                list(n = 12, beta1 = 0, beta2 = 0, sigma = 2, reps = 401),
                list(n = 24, beta1 = -0.8, beta2 = 0, sigma = 2, reps = 503)
            )
            for (i in seq_along(settings)) {
                setting <- settings[[i]]
                set.seed(78143 + i)
                expected <- do.call(reference_results, setting[c("n", "beta1", "beta2", "sigma")])
                set.seed(98117 + i)
                result <- do.call(sim_mean_tests, setting)
                testthat::expect_true(is.list(result))
                testthat::expect_true(all(c("linear_rate", "quadratic_rate") %in% names(result)))
                for (component in c("linear_rate", "quadratic_rate")) {
                    value <- result[[component]]
                    testthat::expect_true(is.numeric(value) && length(value) == 1L && is.finite(value))
                    testthat::expect_true(value >= 0 && value <= 1)
                    p <- expected[[component]]
                    margin <- 6 * sqrt(p * (1 - p) / setting$reps + 0.25 / 20000) + 0.005
                    testthat::expect_lte(abs(as.numeric(value) - p), margin)
                }
            }
        })

      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1.5,
      code = {
        local({

            reference_results <- function(n, beta1, beta2 = 0, sigma = 5,
                                          unequal = FALSE, reps = 20000L) {
                x <- matrix(runif(n * reps, -5, 5), nrow = n)
                error_sd <- if (unequal) 0.5 + 2 * abs(x) else sigma
                y <- beta1 * x + beta2 * x^2 + matrix(rnorm(n * reps), nrow = n) * error_sd
                xc <- sweep(x, 2L, colMeans(x), "-")
                yc <- sweep(y, 2L, colMeans(y), "-")
                xx <- colSums(xc^2)
                xy <- colSums(xc * yc)
                yy <- colSums(yc^2)
                slopes <- xy / xx
                linear_ss <- xy^2 / xx
                linear_rss <- pmax(0, yy - linear_ss)
                t_squared <- linear_ss / (linear_rss / (n - 2))
                x2c <- sweep(x^2, 2L, colMeans(x^2), "-")
                x2orth <- x2c - sweep(xc, 2L, colSums(xc * x2c) / xx, "*")
                quadratic_ss <- linear_ss + colSums(x2orth * yc)^2 / colSums(x2orth^2)
                quadratic_rss <- pmax(0, yy - quadratic_ss)
                f <- (quadratic_ss / 2) / (quadratic_rss / (n - 3))
                list(linear_rate = mean(t_squared > qf(0.95, 1, n - 2)),
                     quadratic_rate = mean(f > qf(0.95, 2, n - 3)),
                     mean_slope = mean(slopes), sd_slope = sd(slopes))
            }

            testthat::expect_true(is.function(sim_mean_tests))
            settings <- list(
                list(n = 38, beta1 = 0, beta2 = -0.25, sigma = 5, reps = 607),
                list(n = 38, beta1 = 0, beta2 = -0.25, sigma = 20, reps = 701),
                list(n = 12, beta1 = 0, beta2 = -0.25, sigma = 5, reps = 809)
            )
            for (i in seq_along(settings)) {
                setting <- settings[[i]]
                set.seed(78143 + i)
                expected <- do.call(reference_results, setting[c("n", "beta1", "beta2", "sigma")])
                set.seed(98117 + i)
                result <- do.call(sim_mean_tests, setting)
                testthat::expect_true(is.list(result))
                testthat::expect_true(all(c("linear_rate", "quadratic_rate") %in% names(result)))
                for (component in c("linear_rate", "quadratic_rate")) {
                    value <- result[[component]]
                    testthat::expect_true(is.numeric(value) && length(value) == 1L && is.finite(value))
                    testthat::expect_true(value >= 0 && value <= 1)
                    p <- expected[[component]]
                    margin <- 6 * sqrt(p * (1 - p) / setting$reps + 0.25 / 20000) + 0.005
                    testthat::expect_lte(abs(as.numeric(value) - p), margin)
                }
            }
        })

      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.5,
      code = {
        testthat::expect_true(is.numeric(mean_test_statements))
        testthat::expect_equal(sort(as.numeric(mean_test_statements)), c(2, 4))

      }
    )
  )
)