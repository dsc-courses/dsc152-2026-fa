test = list(
  name = "q2_1",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      code = {
        local({
          testthat::expect_true(is.function(sim_slope))
          for (alpha in c(.01,.35,.8)) {
            calls <- 0L
            x <- seq(-4,4,length.out=9)
            datasets <- lapply(c(0,.3,2,-2,.1), function(slope) list(x=x,y=slope*x+x^2))
            expected <- vapply(datasets,function(d) summary(lm(d$y~d$x))$coefficients[2,4],numeric(1))
            generator <- function(n,offset) {
              calls <<- calls+1L
              testthat::expect_equal(n,9)
              testthat::expect_equal(offset,2.5)
              d <- datasets[[1L+(calls-1L) %% length(datasets)]]
              d$y <- d$y+offset
              d
            }
            result <- sim_slope(generator,reps=13,alpha=alpha,n=9,offset=2.5)
            testthat::expect_equal(calls,13L)
            testthat::expect_lte(abs(as.numeric(result)-mean(rep(expected,length.out=13)<alpha)),.001)
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
        set.seed(41411)
        for (alpha in c(.02,.20)) {
        rate <- sim_slope(gen_normal,reps=601,alpha=alpha,n=28,sigma=1.7,beta1=0)
        testthat::expect_true(is.numeric(rate))
        testthat::expect_length(rate, 1)
        testthat::expect_true(all(is.finite(rate)))
        testthat::expect_gte(as.numeric(rate), max(0, alpha-6*sqrt(alpha*(1-alpha)/601)-1/601-.005))
        testthat::expect_lte(as.numeric(rate), min(1, alpha+6*sqrt(alpha*(1-alpha)/601)+1/601+.005))
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
        set.seed(41412)
        for (beta1 in c(-3,3)) {
            rate <- sim_slope(gen_normal,reps=151,n=35,beta1=beta1,sigma=.5)
            testthat::expect_gte(as.numeric(rate),.97)
            testthat::expect_lte(as.numeric(rate),1)
        }
        })
      }
    )
  )
)