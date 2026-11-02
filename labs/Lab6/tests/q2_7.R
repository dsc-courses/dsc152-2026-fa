test = list(
  name = "q2_7",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1.25,
      code = {
        local({

            theoretical_rates <- function(n_per_group, beta1, beta2, beta3, sigma, n_sim, alpha) {
                df <- 4 * n_per_group - 4
                noncentral_t <- beta1 / (sigma * sqrt(2 / n_per_group))
                critical_t <- qt(1 - alpha / 2, df)
                noncentral_f <- n_per_group * (beta1^2 + (beta1 + beta3)^2) / (2 * sigma^2)
                c(ttest_rej = pt(-critical_t, df, ncp = noncentral_t) +
                      pt(critical_t, df, ncp = noncentral_t, lower.tail = FALSE),
                  ftest_rej = pf(qf(1 - alpha, 2, df), 2, df,
                      ncp = noncentral_f, lower.tail = FALSE))
            }
            check_settings <- function(settings) {
                testthat::expect_true(is.function(sim_test_rates))
                for (j in seq_along(settings)) {
                    s <- settings[[j]]
                    target <- do.call(theoretical_rates, s)
                    set.seed(80211 + j)
                    actual <- do.call(sim_test_rates, s)
                    testthat::expect_true(is.numeric(actual))
                    testthat::expect_true(all(c("ttest_rej", "ftest_rej") %in% names(actual)))
                    for (name in c("ttest_rej", "ftest_rej")) {
                        value <- actual[[name]]
                        testthat::expect_true(length(value) == 1L && is.finite(value))
                        testthat::expect_true(value >= 0 && value <= 1)
                        p <- target[[name]]
                        allowance <- 6 * sqrt(p * (1 - p) / s$n_sim) + 0.011
                        testthat::expect_lte(abs(value - p), allowance)
                    }
                }
            }

            check_settings(list(
                list(n_per_group=12, beta1=0, beta2=-1.7, beta3=0, sigma=2.5, n_sim=601, alpha=0.05),
                list(n_per_group=8, beta1=1.3, beta2=0.2, beta3=0, sigma=2.5, n_sim=701, alpha=0.05),
                list(n_per_group=20, beta1=-1.5, beta2=2, beta3=0.5, sigma=2.5, n_sim=809, alpha=0.025)))
        })

      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1.25,
      code = {
        local({

            theoretical_rates <- function(n_per_group, beta1, beta2, beta3, sigma, n_sim, alpha) {
                df <- 4 * n_per_group - 4
                noncentral_t <- beta1 / (sigma * sqrt(2 / n_per_group))
                critical_t <- qt(1 - alpha / 2, df)
                noncentral_f <- n_per_group * (beta1^2 + (beta1 + beta3)^2) / (2 * sigma^2)
                c(ttest_rej = pt(-critical_t, df, ncp = noncentral_t) +
                      pt(critical_t, df, ncp = noncentral_t, lower.tail = FALSE),
                  ftest_rej = pf(qf(1 - alpha, 2, df), 2, df,
                      ncp = noncentral_f, lower.tail = FALSE))
            }
            check_settings <- function(settings) {
                testthat::expect_true(is.function(sim_test_rates))
                for (j in seq_along(settings)) {
                    s <- settings[[j]]
                    target <- do.call(theoretical_rates, s)
                    set.seed(80211 + j)
                    actual <- do.call(sim_test_rates, s)
                    testthat::expect_true(is.numeric(actual))
                    testthat::expect_true(all(c("ttest_rej", "ftest_rej") %in% names(actual)))
                    for (name in c("ttest_rej", "ftest_rej")) {
                        value <- actual[[name]]
                        testthat::expect_true(length(value) == 1L && is.finite(value))
                        testthat::expect_true(value >= 0 && value <= 1)
                        p <- target[[name]]
                        allowance <- 6 * sqrt(p * (1 - p) / s$n_sim) + 0.011
                        testthat::expect_lte(abs(value - p), allowance)
                    }
                }
            }

            check_settings(list(
                list(n_per_group=12, beta1=0, beta2=0.4, beta3=2.5, sigma=2.5, n_sim=809, alpha=0.05),
                list(n_per_group=16, beta1=1, beta2=-2, beta3=-2, sigma=2, n_sim=701, alpha=0.1),
                list(n_per_group=8, beta1=1.3, beta2=0.2, beta3=0, sigma=7, n_sim=607, alpha=0.05),
                list(n_per_group=5, beta1=0, beta2=1, beta3=0, sigma=2.5, n_sim=503, alpha=0.2)))
        })

      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.5,
      code = {
        local({
            for (reps in c(1L, 3L)) {
                set.seed(81971 + reps)
                actual <- sim_test_rates(n_per_group=7, beta1=0, beta2=1,
                    beta3=1.5, sigma=2.5, n_sim=reps, alpha=0.05)
                testthat::expect_true(is.numeric(actual))
                for (name in c("ttest_rej", "ftest_rej")) {
                    value <- actual[[name]]
                    testthat::expect_true(length(value) == 1L && is.finite(value))
                    testthat::expect_true(value >= 0 && value <= 1)
                    testthat::expect_lte(min(abs(value - (0:reps)/reps)), 0.011)
                }
            }
        })

      }
    )
  )
)