test = list(
  name = "q3_5",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      code = {
        local({
            prescribed_sizes <- c(30, 60, 120, 240)
            reference_power <- function(n, beta1, beta2, rho, sigma, adjusted) {
                if (adjusted) {
                    effect <- beta1
                    design_scale <- 1 - rho^2
                    error_sd <- sigma
                    design_df <- n - 2
                    test_df <- n - 3
                } else {
                    effect <- beta1 + beta2 * rho
                    design_scale <- 1
                    scale <- max(abs(sigma), abs(beta2))
                    error_sd <- scale * sqrt((sigma / scale)^2 +
                        (beta2 / scale)^2 * (1 - rho^2))
                    design_df <- n - 1
                    test_df <- n - 2
                }
                if (effect == 0) return(0.05)
                cutoff <- qt(0.975, test_df)
                probability <- integrate(function(u) {
                    z <- qchisq(u, design_df)
                    ncp <- (effect / error_sd) * sqrt(design_scale * z)
                    pt(-cutoff, test_df, ncp=ncp) +
                        pt(cutoff, test_df, ncp=ncp, lower.tail=FALSE)
                }, lower=0, upper=1, subdivisions=200L, rel.tol=1e-7)$value
                max(0, min(1, probability))
            }
            check_curve <- function(values, adjusted) {
                testthat::expect_true(is.numeric(values))
                testthat::expect_null(dim(values))
                testthat::expect_length(values, length(prescribed_sizes))
                testthat::expect_true(all(is.finite(values)))
                testthat::expect_true(all(values >= 0 & values <= 1))
                for (j in seq_along(prescribed_sizes)) {
                    target <- reference_power(prescribed_sizes[j],
                        beta1=0.3, beta2=1, rho=0.4, sigma=1, adjusted=adjusted)
                    margin <- 6 * sqrt(target * (1 - target) / 600) + 1 / 600 + 0.005
                    testthat::expect_gte(as.numeric(values[j]), max(0, target - margin))
                    testthat::expect_lte(as.numeric(values[j]), min(1, target + margin))
                }
            }
            check_curve(unadjusted_rates, FALSE)
            check_curve(adjusted_power, TRUE)
        })
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.5,
      code = {
        local({
            prescribed_sizes <- c(30, 60, 120, 240)
            qualifying <- which(as.numeric(adjusted_power) >= 0.8)
            expected <- if (length(qualifying)) min(prescribed_sizes[qualifying]) else NA_real_
            testthat::expect_length(selected_n, 1)
            if (is.na(expected)) {
                testthat::expect_true(is.na(selected_n))
            } else {
                testthat::expect_true(is.numeric(selected_n))
                testthat::expect_equal(as.numeric(selected_n), as.numeric(expected), tolerance=0)
            }
        })
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.5,
      code = {
        testthat::expect_equal(sort(as.numeric(planning_statements)), c(1, 3))
      }
    )
  )
)