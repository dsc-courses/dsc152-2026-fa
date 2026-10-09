test = list(
  name = "q2_6",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0,
      code = {
        testthat::expect_true(is.function(diff_in_medians))
        testthat::expect_equal(unname(obs_median_stat), 6)
        testthat::expect_equal(unname(diff_in_medians(data.frame(score=c(1,3,6,8), group=c("a","a","b","b")))), 5)
        testthat::expect_equal(unname(diff_in_medians(data.frame(score=c(0,0,6,2,2,2), group=rep(c("a","b"),each=3)))), 2)
        testthat::expect_equal(length(perm_median_stats), 5000)
        testthat::expect_true(all(is.finite(perm_median_stats) & perm_median_stats >= 0))
        # Accept lecture b/B counting and a tiny tolerance for mathematical ties.
        observed_for_tail <- unname(obs_median_stat)
        tail_tolerance <- 1e-8 * max(1, abs(observed_for_tail))
        expected_tail <- c(mean(perm_median_stats >= observed_for_tail),
                           mean(perm_median_stats >= observed_for_tail - tail_tolerance))
        testthat::expect_true(is.numeric(p_val_medians) && length(p_val_medians) == 1L && is.finite(p_val_medians))
        testthat::expect_true(any(abs(unname(p_val_medians) - expected_tail) < 1e-10))
        splits <- combn(nrow(hand_code_df), 5)
        exact_stats <- apply(splits, 2, function(idx) abs(median(hand_code_df$score[idx]) - median(hand_code_df$score[-idx])))
        testthat::expect_true(all(vapply(perm_median_stats, function(x) any(abs(x - exact_stats) < 1e-8), logical(1))))
        testthat::expect_lt(abs(p_val_medians - mean(exact_stats >= obs_median_stat)), 0.035)

      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.75,
      code = {
        local({
        testthat::expect_true(is.function(diff_in_medians))
        set.seed(39036)
        for (sizes in list(c(3, 4), c(2, 5), c(5, 6), c(4, 2), c(1, 3))) {
          values <- rnorm(sum(sizes), runif(1, -4, 4), runif(1, 1, 3))
          values[1] <- values[1] + runif(1, 12, 25)
          df <- data.frame(score=values, group=rep(c("left", "right"), sizes))
          expected <- unname(abs(diff(tapply(df[[1]], df[[2]], median))))
          testthat::expect_equal(unname(diff_in_medians(df)), expected, tolerance=1e-8)
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
        testthat::expect_true(is.function(diff_in_medians))
        set.seed(39037)
        for (i in seq_len(4)) {
          df <- data.frame(observation=c(rnorm(4, -1, 2), rnorm(7, 3, 1)),
                           condition=rep(c("control", "treatment"), c(4, 7)))
          expected <- unname(abs(diff(tapply(df[[1]], df[[2]], median))))
          df <- df[sample(nrow(df)), ]
          df[[2]] <- factor(ifelse(df[[2]] == "control", "zeta", "alpha"))
          testthat::expect_equal(unname(diff_in_medians(df)), expected, tolerance=1e-8)
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
        testthat::expect_true(is.function(diff_in_medians))
        set.seed(39038)
        for (i in seq_len(4)) {
          df <- data.frame(score=c(rnorm(3, 8, .5), rnorm(5, -3, .5)),
                           group=rep(c("A", "B"), c(3, 5)))
          expected <- unname(abs(diff(tapply(df[[1]], df[[2]], median))))
          testthat::expect_equal(unname(diff_in_medians(df)), expected, tolerance=1e-8)
          df[[1]] <- -df[[1]]
          testthat::expect_equal(unname(diff_in_medians(df)), expected, tolerance=1e-8)
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
        testthat::expect_true(is.function(diff_in_medians))
        set.seed(39039)
        for (i in seq_len(4)) {
          df <- data.frame(score=c(rnorm(3, 1, 2), rnorm(6, 4, 1)),
                           group=rep(c("A", "B"), c(3, 6)))
          expected <- unname(abs(diff(tapply(df[[1]], df[[2]], median))))
          multiplier <- -runif(1, .7, 4)
          df[[1]] <- multiplier * df[[1]] + runif(1, -12, 12)
          testthat::expect_equal(unname(diff_in_medians(df)), abs(multiplier) * expected,
                                 tolerance=1e-8)
        }
        })

      }
    )
  )
)