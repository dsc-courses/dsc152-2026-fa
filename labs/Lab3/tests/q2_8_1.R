test = list(
  name = "q2_8_1",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0,
      code = {
        local({
        testthat::expect_true(is.function(draw_skew))
        testthat::expect_length(draw_skew(13, mean=2, sd=3), 13)
        testthat::expect_true(all(is.finite(draw_skew(15, mean=-1, sd=2))))
        for (value in list(t_power_skew, perm_power_skew)) {
          testthat::expect_true(is.numeric(value) && length(value) == 1L &&
            is.finite(value) && value >= 0 && value <= 1)
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
        testthat::expect_true(is.function(draw_skew))
        set.seed(39041)
        for (n in c(1L, 13L, 29L)) {
          x <- draw_skew(n, mean=-2.7, sd=1.3)
          testthat::expect_true(is.numeric(x))
          testthat::expect_length(x,n)
          testthat::expect_true(all(is.finite(x)))
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
        testthat::expect_true(is.function(draw_skew))
        set.seed(572311)
        probs <- c(0.1,0.25,0.5,0.75,0.9)
        for (i in seq_len(3L)) {
          n <- sample(9000:13000,1)
          mu <- sample(-90:90,1)/7
          sigma <- sample(2:45,1)/11
          x <- draw_skew(n,mean=mu,sd=sigma)
          testthat::expect_true(is.numeric(x) && length(x) == n && all(is.finite(x)))
          testthat::expect_true(all(x >= mu-sigma-1e-12))
          standardized <- (x-mu)/sigma
          observed <- vapply(qexp(probs)-1,function(point) mean(standardized <= point),numeric(1))
          testthat::expect_lte(max(abs(observed-probs)),0.035)
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
        testthat::expect_true(is.function(draw_skew))
        set.seed(573311)
        n <- sample(17:37,1)
        first <- draw_skew(n,mean=1.7,sd=2.9)
        second <- draw_skew(n,mean=1.7,sd=2.9)
        testthat::expect_false(identical(first,second))
        # Population parameters do not fix a random group's sample mean and SD.
        # Independent group means have variance sigma^2/n; broad bounds tolerate sampling noise.
        group_n <- sample(13:23,1)
        means <- replicate(180L,mean(draw_skew(group_n,mean=-2.8,sd=1.9)))
        relative_var <- var(means)*group_n/1.9^2
        testthat::expect_true(is.finite(relative_var))
        testthat::expect_gt(relative_var,0.45)
        testthat::expect_lt(relative_var,1.85)
        })

      }
    )
  )
)