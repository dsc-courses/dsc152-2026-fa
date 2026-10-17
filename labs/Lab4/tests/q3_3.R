test = list(
  name = "q3_3",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.5,
      code = {
        local({
            reference_power <- function(n, beta1, beta2, rho, sigma, adjusted) {
                if (adjusted) {
                    effect <- beta1
                    design_scale <- 1 - rho^2
                    error_sd <- sigma
                    design_df <- n - 2
                    test_df <- n - 3
                } else {
                    effect <- beta1 + beta2*rho
                    design_scale <- 1
                    scale <- max(abs(sigma),abs(beta2))
                    error_sd <- scale*sqrt((sigma/scale)^2+(beta2/scale)^2*(1-rho^2))
                    design_df <- n - 1
                    test_df <- n - 2
                }
                if (effect == 0) return(0.05)
                cutoff <- qt(0.975, test_df)
                probability <- integrate(function(u) {
                    z <- qchisq(u, design_df)
                    ncp <- (effect/error_sd)*sqrt(design_scale*z)
                    pt(-cutoff, test_df, ncp=ncp) +
                    pt(cutoff, test_df, ncp=ncp, lower.tail=FALSE)
                }, lower=0, upper=1, subdivisions=200L, rel.tol=1e-7)$value
                max(0, min(1, probability))
            }
            check_rate <- function(value, reps, target) {
                testthat::expect_true(is.numeric(value))
                testthat::expect_length(value, 1)
                testthat::expect_true(all(is.finite(value)))
                testthat::expect_gte(as.numeric(value), 0)
                testthat::expect_lte(as.numeric(value), 1)
                margin <- 6*sqrt(target*(1-target)/reps) + 1/reps + 0.005
                testthat::expect_gte(as.numeric(value), max(0, target-margin))
                testthat::expect_lte(as.numeric(value), min(1, target+margin))
            }
            check_scenario <- function(args, component) {
                result <- do.call(sim_mlr, args)
                testthat::expect_true(is.list(result))
                testthat::expect_true(all(c("unadj", "adj") %in% names(result)))
                check_rate(result[[component]], args$reps,
                           reference_power(args$n, args$beta1, args$beta2, args$rho,
                                           args$sigma, component=="adj"))
            }

            testthat::expect_true(is.function(sim_mlr))
            testthat::expect_true(all(c("n", "beta1", "beta2", "rho", "sigma", "reps") %in%
                                     names(formals(sim_mlr))))
            set.seed(44301)
            check_scenario(list(n=12, beta1=0, beta2=1, rho=0, sigma=1, reps=307), "unadj")
            check_scenario(list(n=80, beta1=0.8, beta2=1, rho=-0.8, sigma=1, reps=347), "unadj")
            null_results <- numeric(16)
            for (j in seq_len(16)) {
                result <- sim_mlr(n=12+2*j, beta1=0.4, beta2=1, rho=-0.4, sigma=1+0.03*j, reps=37+2*j)
                null_results[j] <- result$unadj
            }
            testthat::expect_gt(length(unique(round(null_results, 8))),1)
        })
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.5,
      code = {
        local({
            reference_power <- function(n, beta1, beta2, rho, sigma, adjusted) {
                if (adjusted) {
                    effect <- beta1
                    design_scale <- 1 - rho^2
                    error_sd <- sigma
                    design_df <- n - 2
                    test_df <- n - 3
                } else {
                    effect <- beta1 + beta2*rho
                    design_scale <- 1
                    scale <- max(abs(sigma),abs(beta2))
                    error_sd <- scale*sqrt((sigma/scale)^2+(beta2/scale)^2*(1-rho^2))
                    design_df <- n - 1
                    test_df <- n - 2
                }
                if (effect == 0) return(0.05)
                cutoff <- qt(0.975, test_df)
                probability <- integrate(function(u) {
                    z <- qchisq(u, design_df)
                    ncp <- (effect/error_sd)*sqrt(design_scale*z)
                    pt(-cutoff, test_df, ncp=ncp) +
                    pt(cutoff, test_df, ncp=ncp, lower.tail=FALSE)
                }, lower=0, upper=1, subdivisions=200L, rel.tol=1e-7)$value
                max(0, min(1, probability))
            }
            check_rate <- function(value, reps, target) {
                testthat::expect_true(is.numeric(value))
                testthat::expect_length(value, 1)
                testthat::expect_true(all(is.finite(value)))
                testthat::expect_gte(as.numeric(value), 0)
                testthat::expect_lte(as.numeric(value), 1)
                margin <- 6*sqrt(target*(1-target)/reps) + 1/reps + 0.005
                testthat::expect_gte(as.numeric(value), max(0, target-margin))
                testthat::expect_lte(as.numeric(value), min(1, target+margin))
            }
            check_scenario <- function(args, component) {
                result <- do.call(sim_mlr, args)
                testthat::expect_true(is.list(result))
                testthat::expect_true(all(c("unadj", "adj") %in% names(result)))
                check_rate(result[[component]], args$reps,
                           reference_power(args$n, args$beta1, args$beta2, args$rho,
                                           args$sigma, component=="adj"))
            }

            testthat::expect_true(is.function(sim_mlr))
            testthat::expect_true(all(c("n", "beta1", "beta2", "rho", "sigma", "reps") %in%
                                     names(formals(sim_mlr))))
            set.seed(44302)
            check_scenario(list(n=50, beta1=0, beta2=2, rho=0.7, sigma=1, reps=307), "unadj")
            check_scenario(list(n=50, beta1=0, beta2=2, rho=-0.7, sigma=1, reps=307), "unadj")
            check_scenario(list(n=45, beta1=0.4, beta2=1, rho=0, sigma=1, reps=353), "unadj")
        })
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.5,
      code = {
        local({
            reference_power <- function(n, beta1, beta2, rho, sigma, adjusted) {
                if (adjusted) {
                    effect <- beta1
                    design_scale <- 1 - rho^2
                    error_sd <- sigma
                    design_df <- n - 2
                    test_df <- n - 3
                } else {
                    effect <- beta1 + beta2*rho
                    design_scale <- 1
                    scale <- max(abs(sigma),abs(beta2))
                    error_sd <- scale*sqrt((sigma/scale)^2+(beta2/scale)^2*(1-rho^2))
                    design_df <- n - 1
                    test_df <- n - 2
                }
                if (effect == 0) return(0.05)
                cutoff <- qt(0.975, test_df)
                probability <- integrate(function(u) {
                    z <- qchisq(u, design_df)
                    ncp <- (effect/error_sd)*sqrt(design_scale*z)
                    pt(-cutoff, test_df, ncp=ncp) +
                    pt(cutoff, test_df, ncp=ncp, lower.tail=FALSE)
                }, lower=0, upper=1, subdivisions=200L, rel.tol=1e-7)$value
                max(0, min(1, probability))
            }
            check_rate <- function(value, reps, target) {
                testthat::expect_true(is.numeric(value))
                testthat::expect_length(value, 1)
                testthat::expect_true(all(is.finite(value)))
                testthat::expect_gte(as.numeric(value), 0)
                testthat::expect_lte(as.numeric(value), 1)
                margin <- 6*sqrt(target*(1-target)/reps) + 1/reps + 0.005
                testthat::expect_gte(as.numeric(value), max(0, target-margin))
                testthat::expect_lte(as.numeric(value), min(1, target+margin))
            }
            check_scenario <- function(args, component) {
                result <- do.call(sim_mlr, args)
                testthat::expect_true(is.list(result))
                testthat::expect_true(all(c("unadj", "adj") %in% names(result)))
                check_rate(result[[component]], args$reps,
                           reference_power(args$n, args$beta1, args$beta2, args$rho,
                                           args$sigma, component=="adj"))
            }

            testthat::expect_true(is.function(sim_mlr))
            testthat::expect_true(all(c("n", "beta1", "beta2", "rho", "sigma", "reps") %in%
                                     names(formals(sim_mlr))))
            set.seed(44303)
            check_scenario(list(n=9, beta1=0, beta2=2, rho=0.85, sigma=1, reps=251), "adj")
            check_scenario(list(n=50, beta1=0, beta2=2, rho=-0.7, sigma=1, reps=307), "adj")
            check_scenario(list(n=12, beta1=0, beta2=0, rho=0.7, sigma=2, reps=347), "adj")
            null_results <- numeric(16)
            for (j in seq_len(16)) {
                result <- sim_mlr(n=12+2*j, beta1=0, beta2=2, rho=(-1)^j*(0.2+0.03*j), sigma=1+0.03*j, reps=37+2*j)
                null_results[j] <- result$adj
            }
            testthat::expect_gt(length(unique(round(null_results, 8))),1)
        })
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.5,
      code = {
        local({
            reference_power <- function(n, beta1, beta2, rho, sigma, adjusted) {
                if (adjusted) {
                    effect <- beta1
                    design_scale <- 1 - rho^2
                    error_sd <- sigma
                    design_df <- n - 2
                    test_df <- n - 3
                } else {
                    effect <- beta1 + beta2*rho
                    design_scale <- 1
                    scale <- max(abs(sigma),abs(beta2))
                    error_sd <- scale*sqrt((sigma/scale)^2+(beta2/scale)^2*(1-rho^2))
                    design_df <- n - 1
                    test_df <- n - 2
                }
                if (effect == 0) return(0.05)
                cutoff <- qt(0.975, test_df)
                probability <- integrate(function(u) {
                    z <- qchisq(u, design_df)
                    ncp <- (effect/error_sd)*sqrt(design_scale*z)
                    pt(-cutoff, test_df, ncp=ncp) +
                    pt(cutoff, test_df, ncp=ncp, lower.tail=FALSE)
                }, lower=0, upper=1, subdivisions=200L, rel.tol=1e-7)$value
                max(0, min(1, probability))
            }
            check_rate <- function(value, reps, target) {
                testthat::expect_true(is.numeric(value))
                testthat::expect_length(value, 1)
                testthat::expect_true(all(is.finite(value)))
                testthat::expect_gte(as.numeric(value), 0)
                testthat::expect_lte(as.numeric(value), 1)
                margin <- 6*sqrt(target*(1-target)/reps) + 1/reps + 0.005
                testthat::expect_gte(as.numeric(value), max(0, target-margin))
                testthat::expect_lte(as.numeric(value), min(1, target+margin))
            }
            check_scenario <- function(args, component) {
                result <- do.call(sim_mlr, args)
                testthat::expect_true(is.list(result))
                testthat::expect_true(all(c("unadj", "adj") %in% names(result)))
                check_rate(result[[component]], args$reps,
                           reference_power(args$n, args$beta1, args$beta2, args$rho,
                                           args$sigma, component=="adj"))
            }

            testthat::expect_true(is.function(sim_mlr))
            testthat::expect_true(all(c("n", "beta1", "beta2", "rho", "sigma", "reps") %in%
                                     names(formals(sim_mlr))))
            set.seed(44304)
            check_scenario(list(n=80, beta1=0.8, beta2=1, rho=-0.8, sigma=1, reps=347), "adj")
            check_scenario(list(n=18, beta1=0.25, beta2=0, rho=0, sigma=3, reps=353), "adj")
            check_scenario(list(n=100, beta1=0.3, beta2=1, rho=0, sigma=1, reps=401), "adj")
            check_scenario(list(n=100, beta1=0.3, beta2=1, rho=0.8, sigma=1, reps=401), "adj")
            check_scenario(list(n=65, beta1=0.4, beta2=1, rho=0.8, sigma=2, reps=419), "adj")
            check_scenario(list(n=80, beta1=-0.5, beta2=0, rho=0, sigma=0.2, reps=101), "adj")
        })
      }
    )
  )
)