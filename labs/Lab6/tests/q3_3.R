test = list(
  name = "q3_3",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.75,
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

        for (data in make_heart_cases()) for (reference in c("no", "yes")) {
            data$on_diet <- relevel(data$on_diet, reference)
            model <- lm(heart_rate ~ on_diet * daily_steps, data)
            for (change in c(250, 1000, 1800)) {
                actual <- step_effects(model, step_change = change)
                p <- predict(model, data.frame(daily_steps = c(5000, 5000 + change, 5000, 5000 + change),
                                              on_diet = c("no", "no", "yes", "yes")))
                expected <- c(no_diet = unname(p[2] - p[1]), on_diet = unname(p[4] - p[3]))
                testthat::expect_true(is.numeric(actual))
                testthat::expect_equal(names(actual), names(expected))
                testthat::expect_equal(as.numeric(actual), as.numeric(expected), tolerance = 1e-7)
            }
        }

        })

      }
    )
  )
)