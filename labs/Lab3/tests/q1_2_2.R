test = list(
  name = "q1_2_2",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0,
      code = {
        testthat::expect_true(is.function(one_sample_test))
        testthat::expect_equal(length(ci_large_n), 2)
        testthat::expect_true(ci_large_n[1] < ci_large_n[2])
        testthat::expect_equal(as.numeric(ci_large_n), as.numeric(t.test(app_large_sample, conf.level=.95)$conf.int), tolerance=1e-8)
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.5,
      code = {
        local({
        testthat::expect_true(is.function(one_sample_test))
        set.seed(39022)
        for (i in seq_len(5)) {
          x <- rnorm(sample(12:31, 1), runif(1, -5, 5), runif(1, 0.5, 4))
          expected <- as.numeric(t.test(x, conf.level=.95)$conf.int)
          testthat::expect_equal(as.numeric(one_sample_test(x)$conf.int), expected, tolerance=1e-8)
          testthat::expect_equal(as.numeric(one_sample_test(2*x-11)$conf.int), 2*expected-11, tolerance=1e-8)
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
        testthat::expect_true(is.function(one_sample_test))
        x <- c(-5.4, -2.7, -1.1, 0.3, 1.8, 2.0, 2.4, 3.5, 4.1, 8.2, 9.7, 12.3)
        for (level in c(.80, .90, .99)) {
          expected <- as.numeric(t.test(x, conf.level=level)$conf.int)
          testthat::expect_equal(as.numeric(one_sample_test(x, conf_level=level)$conf.int), expected, tolerance=1e-8)
        }
        })

      }
    )
  )
)