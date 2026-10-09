test = list(
  name = "q2_8_2",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0,
      code = {
        local({
        testthat::expect_true(is.function(draw_heavy))
        testthat::expect_length(draw_heavy(13, mean=2, sd=3, df_t=5), 13)
        testthat::expect_true(all(is.finite(draw_heavy(15, mean=-1, sd=2))))
        for (value in list(t_power_heavy, perm_power_heavy)) {
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
        testthat::expect_true(is.function(draw_heavy))
        set.seed(39042)
        for (n in c(1L, 13L, 29L)) {
          x <- draw_heavy(n, mean=-2.7, sd=1.3, df_t=5)
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
        testthat::expect_true(is.function(draw_heavy))
        set.seed(672817)
        probs <- c(0.05,0.1,0.25,0.5,0.75,0.9,0.95)
        for (df in c(3,3.7,sample(5:8,1),sample(12:18,1))) {
          n <- sample(10000:14000,1)
          mu <- sample(-90:90,1)/9
          sigma <- sample(2:45,1)/13
          # The first setting also checks the requested default degrees of freedom.
          x <- if (df == 3) draw_heavy(n,mean=mu,sd=sigma) else
            draw_heavy(n,mean=mu,sd=sigma,df_t=df)
          testthat::expect_true(is.numeric(x) && length(x) == n && all(is.finite(x)))
          standardized <- (x-mu)/sigma
          cutpoints <- qt(probs,df)/sqrt(df/(df-2))
          observed <- vapply(cutpoints,function(point) mean(standardized <= point),numeric(1))
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
        testthat::expect_true(is.function(draw_heavy))
        set.seed(673817)
        n <- sample(17:37,1)
        first <- draw_heavy(n,mean=1.7,sd=2.9)
        second <- draw_heavy(n,mean=1.7,sd=2.9)
        testthat::expect_false(identical(first,second))
        # Population parameters do not fix a random group's sample mean and SD.
        # Independent group means have variance sigma^2/n; broad bounds tolerate sampling noise.
        group_n <- sample(13:23,1)
        means <- replicate(180L,mean(draw_heavy(group_n,mean=-2.8,sd=1.9,df_t=11)))
        relative_var <- var(means)*group_n/1.9^2
        testthat::expect_true(is.finite(relative_var))
        testthat::expect_gt(relative_var,0.45)
        testthat::expect_lt(relative_var,1.85)
        })

      }
    )
  )
)