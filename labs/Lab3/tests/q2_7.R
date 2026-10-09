test = list(
  name = "q2_7",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0,
      code = {
        local({
        testthat::expect_true(is.function(sim_power_perm))
        set.seed(617)
        draw_constant <- function(n, mean, sd) rep(mean, n)
        testthat::expect_equal(unname(sim_power_perm(n=1, delta=3, sd=1, reps=3, n_perms=5,
                                             draw_group=draw_constant)), 0)
        })

      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.5,
      code = {
        local({
        testthat::expect_true(is.function(sim_power_perm))
        set.seed(39040)
        for (scenario in list(c(n=2, delta=7, sd=2, reps=3, B=7),
                               c(n=4, delta=-11, sd=0.7, reps=5, B=3))) {
          calls <- list()
          draw_spy <- function(n, mean, sd) {
            calls[[length(calls) + 1L]] <<- c(n=n, mean=mean, sd=sd)
            rep(mean, n)
          }
          invisible(sim_power_perm(n=scenario[["n"]], delta=scenario[["delta"]],
            sd=scenario[["sd"]], reps=scenario[["reps"]], n_perms=scenario[["B"]],
            draw_group=draw_spy))
          testthat::expect_length(calls, 2L * scenario[["reps"]])
          testthat::expect_true(all(vapply(calls, function(a) a[["n"]] == scenario[["n"]], logical(1))))
          testthat::expect_true(all(vapply(calls, function(a) a[["sd"]] == scenario[["sd"]], logical(1))))
          means <- vapply(calls, function(a) a[["mean"]], numeric(1))
          testthat::expect_equal(sort(means), sort(rep(c(0, scenario[["delta"]]), scenario[["reps"]])))
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
        testthat::expect_true(is.function(sim_power_perm))
        draw_constant <- function(n, mean, sd) rep(mean, n)
        # With one observation per group every absolute difference is tied, so p=1.
        set.seed(9810)
        testthat::expect_equal(unname(sim_power_perm(n=1, delta=7, sd=2, alpha=1,
          reps=3, n_perms=7, draw_group=draw_constant)), 0)
        testthat::expect_equal(unname(sim_power_perm(n=4, delta=0, sd=1, alpha=0.98,
          reps=4, n_perms=17, draw_group=draw_constant)), 0)
        # Even a zero Monte Carlo p-value is not below alpha=0.
        set.seed(9811)
        testthat::expect_equal(unname(sim_power_perm(n=15, delta=1e6, sd=1, alpha=0,
          reps=3, n_perms=99)), 0)
        set.seed(9812)
        testthat::expect_gte(unname(sim_power_perm(n=15, delta=1e6, sd=1, alpha=0.05,
          reps=5, n_perms=99)), 0.8)
        })

      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.5,
      code = {
        local({
        testthat::expect_true(is.function(sim_power_perm))
        draw_constant <- function(n, mean, sd) rep(mean, n)
        # Exactly two of six allocations are extreme. With one shuffle, b/B is zero
        # in about two-thirds of studies; with 99 shuffles it virtually never falls below .05.
        set.seed(9813)
        one_shuffle <- sim_power_perm(n=2, delta=10, sd=1, alpha=0.05,
          reps=40, n_perms=1, draw_group=draw_constant)
        testthat::expect_true(is.numeric(one_shuffle) && length(one_shuffle) == 1 && is.finite(one_shuffle))
        testthat::expect_gt(one_shuffle, 0.20)
        testthat::expect_lte(one_shuffle, 1)
        testthat::expect_equal(one_shuffle * 40, round(one_shuffle * 40), tolerance=1e-9)
        set.seed(9814)
        many_shuffles <- sim_power_perm(n=2, delta=10, sd=1, alpha=0.05,
          reps=40, n_perms=99, draw_group=draw_constant)
        testthat::expect_true(is.numeric(many_shuffles) && length(many_shuffles) == 1L &&
          is.finite(many_shuffles) && many_shuffles >= 0 && many_shuffles <= 1)
        testthat::expect_lte(many_shuffles, 1/40)
        })

      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.5,
      code = {
        local({
        testthat::expect_true(is.function(sim_power_perm))
        allocation_tail <- function(x, y) {
          n <- length(x)
          pooled <- c(x,y)
          observed <- abs(mean(x)-mean(y))
          values <- apply(combn(seq_along(pooled), n), 2L, function(index)
            abs(mean(pooled[index])-mean(pooled[-index])))
          mean(values >= observed - 1e-12)
        }
        make_fixture <- function(n, lower, upper) {
          for (attempt in seq_len(2000L)) {
            # Multiples of n give exactly representable integer group means for ties.
            pooled <- n * sample(-40:40, 2L*n, replace=FALSE)
            x <- pooled[seq_len(n)]
            y <- pooled[n+seq_len(n)]
            q <- allocation_tail(x,y)
            if (q >= lower && q <= upper) return(list(x=x,y=y,q=q))
          }
          stopifnot(FALSE)
        }
        set.seed(381719)
        fixtures <- list(make_fixture(sample(3:5,1),0.15,0.35),
                         make_fixture(sample(3:5,1),0.45,0.70))
        scenarios <- list(c(f=1,B=1,alpha=0.05,reps=160),
                          c(f=1,B=83,alpha=0.05,reps=80),
                          c(f=1,B=11,alpha=0.35,reps=180),
                          c(f=2,B=7,alpha=0.45,reps=180),
                          c(f=2,B=29,alpha=0.78,reps=180))
        for (scenario in scenarios) {
          fixture <- fixtures[[scenario[["f"]]]]
          n <- length(fixture$x)
          delta <- sample(c(-103,-31,19,107),1)
          sigma <- sample(c(0.7,2.3,5.1),1)
          # Supplied fixture values, rather than a theoretical formula in n/delta/sd,
          # determine the correct permutation rejection probability.
          draw_fixture <- function(n, mean, sd) if (mean == 0) fixture$x else fixture$y
          actual <- sim_power_perm(n=n,delta=delta,sd=sigma,alpha=scenario[["alpha"]],
            reps=scenario[["reps"]],n_perms=scenario[["B"]],draw_group=draw_fixture)
          testthat::expect_true(is.numeric(actual) && length(actual) == 1L &&
            is.finite(actual) && actual >= 0 && actual <= 1)
          testthat::expect_equal(actual*scenario[["reps"]],
            round(actual*scenario[["reps"]]), tolerance=1e-9)
          expected <- pbinom(ceiling(scenario[["B"]]*scenario[["alpha"]])-1,
            size=scenario[["B"]],prob=fixture$q)
          allowance <- 6*sqrt(expected*(1-expected)/scenario[["reps"]]) + 1/scenario[["reps"]]
          testthat::expect_lte(abs(actual-expected),allowance)
        }
        })

      }
    )
  )
)