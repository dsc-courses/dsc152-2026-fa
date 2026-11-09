test = list(
  name = "q1_2",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1.5,
      code = {
        local({
            make_design <- function(n, rho) {
                u <- seq(-1, 1, length.out=n)
                u <- u / sqrt(sum(u^2))
                v <- sin(seq_len(n) * 0.73)
                v <- v - mean(v)
                v <- v - sum(u * v) * u
                v <- v / sqrt(sum(v^2))
                list(x1=12 + 7*u, x2=-5 + 3*(rho*u + sqrt(1-rho^2)*v))
            }
            oracle <- function(n, rho, draws=160000L) {
                had_seed <- exists(".Random.seed", envir=.GlobalEnv, inherits=FALSE)
                if (had_seed) old_seed <- get(".Random.seed", envir=.GlobalEnv)
                on.exit({
                    if (had_seed) assign(".Random.seed", old_seed, envir=.GlobalEnv)
                    else if (exists(".Random.seed", envir=.GlobalEnv, inherits=FALSE))
                        rm(".Random.seed", envir=.GlobalEnv)
                })
                set.seed(85191)
                z1 <- rnorm(draws)
                z2 <- rnorm(draws)
                rss <- rchisq(draws, df=n-3)
                scale_full <- sqrt(rss/(n-3))
                full <- 2*pt(-abs((sqrt(1-rho^2)*z1-rho*z2)/scale_full), df=n-3)
                second <- 2*pt(-abs(z2/scale_full), df=n-3)
                reduced <- 2*pt(-abs(z1/sqrt((rss+z2^2)/(n-2))), df=n-2)
                selected <- ifelse(second > .05, reduced, full)
                delta <- abs(full-selected)
                gap <- (selected < .05) - (full < .05)
                list(cdf=vapply(c(.05,.25,.5,.75), function(a) mean(selected<a), numeric(1)),
                     delta=mean(delta), delta_sd=sd(delta),gap=mean(gap),gap_sd=sd(gap))
            }
            check_simulation <- function(n, rho, reps, seed) {
                design <- make_design(n,rho)
                set.seed(seed)
                result <- sim_select_then_infer(reps, design$x1, design$x2)
                testthat::expect_true(is.list(result))
                testthat::expect_true(all(c("pval_full","pval_selected") %in% names(result)))
                for (name in c("pval_full","pval_selected")) {
                    testthat::expect_true(is.numeric(result[[name]]))
                    testthat::expect_equal(length(result[[name]]),reps)
                    testthat::expect_true(all(is.finite(result[[name]])))
                    testthat::expect_true(all(result[[name]]>=0 & result[[name]]<=1))
                }
                expected <- oracle(n,cor(design$x1,design$x2))
                cuts <- c(.05,.25,.5,.75)
                observed_full <- vapply(cuts,function(a)mean(result$pval_full<a),numeric(1))
                observed_selected <- vapply(cuts,function(a)mean(result$pval_selected<a),numeric(1))
                testthat::expect_true(all(abs(observed_full-cuts) <= 6*sqrt(cuts*(1-cuts)/reps)+.006))
                testthat::expect_true(all(abs(observed_selected-expected$cdf) <=
                    6*sqrt(expected$cdf*(1-expected$cdf)/reps)+.009))
                same <- mean(abs(result$pval_full-result$pval_selected)<1e-7)
                testthat::expect_true(abs(same-.05) <= 6*sqrt(.05*.95/reps)+.003)
                observed_delta <- mean(abs(result$pval_full-result$pval_selected))
                testthat::expect_true(abs(observed_delta-expected$delta) <=
                    6*expected$delta_sd/sqrt(reps)+.012)
                observed_gap <- mean((result$pval_selected < .05)-(result$pval_full < .05))
                testthat::expect_true(abs(observed_gap-expected$gap) <=
                    6*expected$gap_sd/sqrt(reps)+.009)
            }
            testthat::expect_true(is.function(sim_select_then_infer))
            check_simulation(n=31,rho=.02,reps=1801L,seed=812)
            check_simulation(n=83,rho=.985,reps=2003L,seed=913)
        })

      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1.5,
      code = {
        local({
            make_design <- function(n, rho) {
                u <- seq(-1, 1, length.out=n)
                u <- u / sqrt(sum(u^2))
                v <- sin(seq_len(n) * 0.73)
                v <- v - mean(v)
                v <- v - sum(u * v) * u
                v <- v / sqrt(sum(v^2))
                list(x1=12 + 7*u, x2=-5 + 3*(rho*u + sqrt(1-rho^2)*v))
            }
            oracle <- function(n, rho, draws=160000L) {
                had_seed <- exists(".Random.seed", envir=.GlobalEnv, inherits=FALSE)
                if (had_seed) old_seed <- get(".Random.seed", envir=.GlobalEnv)
                on.exit({
                    if (had_seed) assign(".Random.seed", old_seed, envir=.GlobalEnv)
                    else if (exists(".Random.seed", envir=.GlobalEnv, inherits=FALSE))
                        rm(".Random.seed", envir=.GlobalEnv)
                })
                set.seed(85191)
                z1 <- rnorm(draws)
                z2 <- rnorm(draws)
                rss <- rchisq(draws, df=n-3)
                scale_full <- sqrt(rss/(n-3))
                full <- 2*pt(-abs((sqrt(1-rho^2)*z1-rho*z2)/scale_full), df=n-3)
                second <- 2*pt(-abs(z2/scale_full), df=n-3)
                reduced <- 2*pt(-abs(z1/sqrt((rss+z2^2)/(n-2))), df=n-2)
                selected <- ifelse(second > .05, reduced, full)
                delta <- abs(full-selected)
                gap <- (selected < .05) - (full < .05)
                list(cdf=vapply(c(.05,.25,.5,.75), function(a) mean(selected<a), numeric(1)),
                     delta=mean(delta), delta_sd=sd(delta),gap=mean(gap),gap_sd=sd(gap))
            }
            check_simulation <- function(n, rho, reps, seed) {
                design <- make_design(n,rho)
                set.seed(seed)
                result <- sim_select_then_infer(reps, design$x1, design$x2)
                testthat::expect_true(is.list(result))
                testthat::expect_true(all(c("pval_full","pval_selected") %in% names(result)))
                for (name in c("pval_full","pval_selected")) {
                    testthat::expect_true(is.numeric(result[[name]]))
                    testthat::expect_equal(length(result[[name]]),reps)
                    testthat::expect_true(all(is.finite(result[[name]])))
                    testthat::expect_true(all(result[[name]]>=0 & result[[name]]<=1))
                }
                expected <- oracle(n,cor(design$x1,design$x2))
                cuts <- c(.05,.25,.5,.75)
                observed_full <- vapply(cuts,function(a)mean(result$pval_full<a),numeric(1))
                observed_selected <- vapply(cuts,function(a)mean(result$pval_selected<a),numeric(1))
                testthat::expect_true(all(abs(observed_full-cuts) <= 6*sqrt(cuts*(1-cuts)/reps)+.006))
                testthat::expect_true(all(abs(observed_selected-expected$cdf) <=
                    6*sqrt(expected$cdf*(1-expected$cdf)/reps)+.009))
                same <- mean(abs(result$pval_full-result$pval_selected)<1e-7)
                testthat::expect_true(abs(same-.05) <= 6*sqrt(.05*.95/reps)+.003)
                observed_delta <- mean(abs(result$pval_full-result$pval_selected))
                testthat::expect_true(abs(observed_delta-expected$delta) <=
                    6*expected$delta_sd/sqrt(reps)+.012)
                observed_gap <- mean((result$pval_selected < .05)-(result$pval_full < .05))
                testthat::expect_true(abs(observed_gap-expected$gap) <=
                    6*expected$gap_sd/sqrt(reps)+.009)
            }
            check_simulation(n=58,rho=-.88,reps=1807L,seed=715)
            check_simulation(n=6,rho=.97,reps=1601L,seed=642)
            design <- make_design(17,.4)
            for (count in c(1L,3L)) {
                value <- sim_select_then_infer(count,design$x1,design$x2)
                testthat::expect_equal(length(value$pval_full),count)
                testthat::expect_equal(length(value$pval_selected),count)
            }
        })

      }
    )
  )
)