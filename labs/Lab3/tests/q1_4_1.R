test = list(
  name = "q1_4_1",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0,
      code = {
        testthat::expect_true(is.function(draw_app_sample))
        testthat::expect_true(is.function(one_sample_test))
        testthat::expect_length(app_small_sample, 30)
        testthat::expect_equal(unname(p_val_small_n), t.test(app_small_sample, mu=0, alternative="two.sided")$p.value, tolerance=1e-8)

      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.5,
      code = {
        local({
        testthat::expect_true(is.function(draw_app_sample))
        set.seed(39032)
        settings <- list(c(n=12000, mu=2.4, sigma=4.1), c(n=17000, mu=-5.7, sigma=.8))
        for (setting in settings) {
          x <- draw_app_sample(n=setting["n"], mean=setting["mu"], sd=setting["sigma"])
          testthat::expect_true(is.numeric(x))
          testthat::expect_length(x, as.integer(setting["n"]))
          testthat::expect_true(all(is.finite(x)))
          testthat::expect_lt(abs(mean(x)-setting["mu"]), 6*setting["sigma"]/sqrt(setting["n"]))
          testthat::expect_lt(abs(sd(x)/setting["sigma"]-1), .05)
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
        testthat::expect_true(is.function(draw_app_sample))
        set.seed(39033)
        x <- draw_app_sample(n=16000, mean=-1.3, sd=2.2)
        y <- draw_app_sample(n=16000, mean=-1.3, sd=2.2)
        testthat::expect_length(x, 16000)
        testthat::expect_length(y, 16000)
        testthat::expect_false(identical(x,y))
        # Distributional checks allow equivalent ways of drawing a normal sample.
        for (sample_data in list(x,y)) {
          standardized <- (sample_data+1.3)/2.2
          testthat::expect_true(all(is.finite(standardized)))
          observed <- as.numeric(quantile(standardized, probs=c(.025,.25,.5,.75,.975)))
          expected <- qnorm(c(.025,.25,.5,.75,.975))
          testthat::expect_true(all(abs(observed-expected)<.12))
        }
        })

      }
    )
  )
)