test = list(
  name = "q3_1",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.5,
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

        for (data in make_heart_cases()) for (interaction in c(FALSE, TRUE)) {
            data$on_diet <- relevel(data$on_diet, "yes")
            actual <- fit_heart_model(data, interaction = interaction)
            expected <- if (interaction) lm(heart_rate ~ daily_steps * on_diet, data) else
                lm(heart_rate ~ daily_steps + on_diet, data)
            testthat::expect_s3_class(actual, "lm")
            testthat::expect_true("on_dietyes" %in% names(coef(actual)))
            testthat::expect_equal(as.numeric(fitted(actual)), as.numeric(fitted(expected)), tolerance = 1e-7)
            testthat::expect_equal(df.residual(actual), df.residual(expected))
            probe <- data.frame(daily_steps = rep(c(3000, 8000, 13000), each = 2), on_diet = rep(c("no", "yes"), 3))
            testthat::expect_equal(as.numeric(predict(actual, probe)), as.numeric(predict(expected, probe)), tolerance = 1e-7)
        }

        })

      }
    )
  )
)