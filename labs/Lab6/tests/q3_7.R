test = list(
  name = "q3_7",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      code = {
        local({

        make_heart_cases <- function() {
            lapply(seq_len(3), function(j) {
                n <- c(64, 90, 120)[j]
                i <- seq_len(n)
                x <- 1800 + ((i * 19) %% (n + 1)) / (n + 1) * 13500
                g <- rep(c("no", "yes"), length.out = n)
                data.frame(daily_steps = x, on_diet = factor(g, levels = c("no", "yes")),
                    heart_rate = 80 + c(-0.0007, -0.001, 0.0003)[j] * x +
                        c(0, 4, -7)[j] * (g == "yes") +
                        c(0, -0.0004, 0.0006)[j] * x * (g == "yes") +
                        (1 + j) * (sin(i * 1.31) + 0.4 * cos(i * 0.73)))
            })
        }

        bootstrap_reference <- function(data, reps = 6000) {
            n <- nrow(data)
            indices <- matrix(sample.int(n, n * reps, replace = TRUE), nrow = n)
            x <- matrix(data$daily_steps[indices], nrow = n)
            y <- matrix(data$heart_rate[indices], nrow = n)
            group <- matrix(as.numeric(data$on_diet[indices] == "yes"), nrow = n)
            slope <- function(w) {
                count <- colSums(w)
                (colSums(w * x * y) - colSums(w * x) * colSums(w * y) / count) /
                    (colSums(w * x^2) - colSums(w * x)^2 / count)
            }
            slope(group) - slope(1 - group)
        }
        settings <- list(list(data = transform(make_heart_cases()[[2]], on_diet = relevel(on_diet, "yes")), reps = 1001, level = 0.8),
                         list(data = make_heart_cases()[[3]], reps = 801, level = 0.95))
        for (j in seq_along(settings)) {
            setting <- settings[[j]]
            set.seed(61820 + j)
            expected <- bootstrap_reference(setting$data)
            set.seed(81930 + j)
            actual <- bootstrap_interaction(setting$data, reps = setting$reps, level = setting$level)
            testthat::expect_true(is.list(actual) && all(c("estimates", "ci") %in% names(actual)))
            values <- actual$estimates
            testthat::expect_true(is.numeric(values) && all(is.finite(values)))
            testthat::expect_length(values, setting$reps)
            spread <- sd(expected)
            testthat::expect_gt(sd(values), 0.5 * spread)
            testthat::expect_lt(sd(values), 1.7 * spread)
            testthat::expect_lte(abs(mean(values) - mean(expected)), 6 * spread * sqrt(1 / setting$reps + 1 / length(expected)) + 0.08 * spread)
        }
        })

      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1,
      code = {
        local({

        make_heart_cases <- function() {
            lapply(seq_len(3), function(j) {
                n <- c(64, 90, 120)[j]
                i <- seq_len(n)
                x <- 1800 + ((i * 19) %% (n + 1)) / (n + 1) * 13500
                g <- rep(c("no", "yes"), length.out = n)
                data.frame(daily_steps = x, on_diet = factor(g, levels = c("no", "yes")),
                    heart_rate = 80 + c(-0.0007, -0.001, 0.0003)[j] * x +
                        c(0, 4, -7)[j] * (g == "yes") +
                        c(0, -0.0004, 0.0006)[j] * x * (g == "yes") +
                        (1 + j) * (sin(i * 1.31) + 0.4 * cos(i * 0.73)))
            })
        }

        bootstrap_reference <- function(data, reps = 6000) {
            n <- nrow(data)
            indices <- matrix(sample.int(n, n * reps, replace = TRUE), nrow = n)
            x <- matrix(data$daily_steps[indices], nrow = n)
            y <- matrix(data$heart_rate[indices], nrow = n)
            group <- matrix(as.numeric(data$on_diet[indices] == "yes"), nrow = n)
            slope <- function(w) {
                count <- colSums(w)
                (colSums(w * x * y) - colSums(w * x) * colSums(w * y) / count) /
                    (colSums(w * x^2) - colSums(w * x)^2 / count)
            }
            slope(group) - slope(1 - group)
        }
        settings <- list(list(data = transform(make_heart_cases()[[2]], on_diet = relevel(on_diet, "yes")), reps = 1001, level = 0.8),
                         list(data = make_heart_cases()[[3]], reps = 801, level = 0.95))
        for (j in seq_along(settings)) {
            setting <- settings[[j]]
            set.seed(61820 + j)
            expected <- bootstrap_reference(setting$data)
            set.seed(81930 + j)
            actual <- bootstrap_interaction(setting$data, reps = setting$reps, level = setting$level)
            testthat::expect_true(is.list(actual) && all(c("estimates", "ci") %in% names(actual)))
            values <- actual$estimates
            testthat::expect_true(is.numeric(values) && all(is.finite(values)))
            testthat::expect_length(values, setting$reps)
            spread <- sd(expected)
            probs <- c((1 - setting$level) / 2, (1 + setting$level) / 2)
            testthat::expect_true(is.numeric(actual$ci) && length(actual$ci) == 2 && all(is.finite(actual$ci)))
            testthat::expect_lt(actual$ci[1], actual$ci[2])
            own_quantiles <- quantile(values, probs)
            testthat::expect_lte(max(abs(as.numeric(actual$ci) - own_quantiles)), 0.15 * spread)
            margin <- 6 * spread * sqrt(probs * (1 - probs) * (1 / setting$reps + 1 / length(expected))) / dnorm(qnorm(probs)) + 0.08 * spread
            testthat::expect_true(all(abs(as.numeric(actual$ci) - quantile(expected, probs)) <= margin))
        }
        })

      }
    )
  )
)