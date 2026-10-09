test = list(
  name = "q2_3",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0,
      code = {
        testthat::expect_true(is.function(diff_in_means))
        testthat::expect_true(is.function(diff_in_means))
        testthat::expect_equal(unname(obs_test_stat), 4.6)
        testthat::expect_equal(unname(diff_in_means(data.frame(score=c(1,3,6,8), group=c("a","a","b","b")))), 5)
        testthat::expect_equal(unname(diff_in_means(data.frame(score=c(0,0,6,2,2,2), group=rep(c("a","b"),each=3)))), 0)

      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.5,
      code = {
        local({
        testthat::expect_true(is.function(diff_in_means))
        set.seed(39026)
        for (i in seq_len(7)) {
          sizes <- sample(2:6, 2, replace=TRUE)
          df <- data.frame(score=rnorm(sum(sizes), runif(1, -4, 4), runif(1, 1, 3)),
                           group=rep(c("left", "right"), sizes))
          expected <- unname(abs(diff(tapply(df[[1]], df[[2]], mean))))
          testthat::expect_equal(unname(diff_in_means(df)), expected, tolerance=1e-8)
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
        testthat::expect_true(is.function(diff_in_means))
        set.seed(39033)
        for (i in seq_len(4)) {
          df <- data.frame(observation=c(rnorm(4, -1, 2), rnorm(7, 3, 1)),
                           condition=rep(c("control", "treatment"), c(4, 7)))
          expected <- unname(abs(diff(tapply(df[[1]], df[[2]], mean))))
          df <- df[sample(nrow(df)), ]
          df[[2]] <- factor(ifelse(df[[2]] == "control", "zeta", "alpha"))
          testthat::expect_equal(unname(diff_in_means(df)), expected, tolerance=1e-8)
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
        testthat::expect_true(is.function(diff_in_means))
        set.seed(39034)
        for (i in seq_len(4)) {
          df <- data.frame(score=c(rnorm(3, 8, .5), rnorm(5, -3, .5)),
                           group=rep(c("A", "B"), c(3, 5)))
          expected <- unname(abs(diff(tapply(df[[1]], df[[2]], mean))))
          testthat::expect_equal(unname(diff_in_means(df)), expected, tolerance=1e-8)
          multiplier <- -runif(1, .7, 4)
          df[[1]] <- multiplier * df[[1]] + runif(1, -12, 12)
          testthat::expect_equal(unname(diff_in_means(df)), abs(multiplier) * expected,
                                 tolerance=1e-8)
        }
        })

      }
    )
  )
)