test = list(
  name = "q2_5",
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

            testthat::expect_true(is.function(sim_variance_tests))
            settings <- list(
                list(n = 50, reps = 1801, beta1 = 0),
                list(n = 21, reps = 607, beta1 = -2.5),
                list(n = 7, reps = 701, beta1 = 1)
            )
            for (i in seq_along(settings)) {
                setting <- settings[[i]]
                set.seed(71651 + i)
                expected <- reference_results(n = setting$n, beta1 = setting$beta1,
                                              unequal = FALSE)
                set.seed(93811 + i)
                result <- do.call(sim_variance_tests, c(setting, list(unequal = FALSE)))
                testthat::expect_true(is.list(result))
                testthat::expect_true(all(c("mean_slope", "rejection_rate") %in% names(result)))
                for (component in c("mean_slope", "rejection_rate")) {
                    value <- result[[component]]
                    testthat::expect_true(is.numeric(value) && length(value) == 1L && is.finite(value))
                }
                p <- expected$linear_rate
                rate_margin <- 6 * sqrt(p * (1 - p) / setting$reps + 0.25 / 20000) + 0.005
                testthat::expect_true(result$rejection_rate >= 0 && result$rejection_rate <= 1)
                testthat::expect_lte(abs(as.numeric(result$rejection_rate) - p), rate_margin)
                slope_margin <- 6 * expected$sd_slope / sqrt(setting$reps) + 0.011
                testthat::expect_lte(abs(as.numeric(result$mean_slope) - setting$beta1), slope_margin)
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

            testthat::expect_true(is.function(sim_variance_tests))
            settings <- list(
                list(n = 50, reps = 1801, beta1 = 0),
                list(n = 24, reps = 607, beta1 = -2.5),
                list(n = 8, reps = 701, beta1 = 1.2)
            )
            for (i in seq_along(settings)) {
                setting <- settings[[i]]
                set.seed(71651 + i)
                expected <- reference_results(n = setting$n, beta1 = setting$beta1,
                                              unequal = TRUE)
                set.seed(93811 + i)
                result <- do.call(sim_variance_tests, c(setting, list(unequal = TRUE)))
                testthat::expect_true(is.list(result))
                testthat::expect_true(all(c("mean_slope", "rejection_rate") %in% names(result)))
                for (component in c("mean_slope", "rejection_rate")) {
                    value <- result[[component]]
                    testthat::expect_true(is.numeric(value) && length(value) == 1L && is.finite(value))
                }
                p <- expected$linear_rate
                rate_margin <- 6 * sqrt(p * (1 - p) / setting$reps + 0.25 / 20000) + 0.005
                testthat::expect_true(result$rejection_rate >= 0 && result$rejection_rate <= 1)
                testthat::expect_lte(abs(as.numeric(result$rejection_rate) - p), rate_margin)
                slope_margin <- 6 * expected$sd_slope / sqrt(setting$reps) + 0.011
                testthat::expect_lte(abs(as.numeric(result$mean_slope) - setting$beta1), slope_margin)
            }
        })

      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.5,
      code = {
        testthat::expect_true(is.numeric(variance_statements))
        testthat::expect_equal(sort(as.numeric(variance_statements)), c(1, 4))

      }
    )
  )
)