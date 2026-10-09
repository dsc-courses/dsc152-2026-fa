test = list(
  name = "q2_2",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0,
      code = {
        testthat::expect_true(is.function(two_sample_p))
        testthat::expect_equal(unname(p_val_ttest), t.test(hand_code_df[[1]] ~ hand_code_df[[2]])$p.value, tolerance=1e-8)
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.25,
      code = {
        local({
        testthat::expect_true(is.function(two_sample_p))
        set.seed(39030)
        for (i in seq_len(5)) {
          sizes <- c(sample(7:11, 1), sample(15:20, 1))
          df <- data.frame(score=c(rnorm(sizes[1], -1, .5), rnorm(sizes[2], .4, 4)),
                           group=rep(c("left", "right"), sizes))
          testthat::expect_equal(unname(two_sample_p(df)), t.test(df[[1]] ~ df[[2]])$p.value,
                                 tolerance=1e-8)
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
        testthat::expect_true(is.function(two_sample_p))
        set.seed(39032)
        for (i in seq_len(4)) {
          df <- data.frame(value=c(rnorm(8, -.6, 1), rnorm(13, .8, 2)),
                           label=rep(c("oak", "pine"), c(8, 13)))
          expected <- t.test(df[[1]] ~ df[[2]])$p.value
          df <- df[sample(nrow(df)), ]
          df[[2]] <- factor(ifelse(df[[2]] == "oak", "zeta", "alpha"))
          df[[1]] <- runif(1, .6, 4) * df[[1]] + runif(1, -15, 15)
          testthat::expect_equal(unname(two_sample_p(df)), expected, tolerance=1e-8)
        }
        })

      }
    )
  )
)