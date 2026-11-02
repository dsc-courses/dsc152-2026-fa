test = list(
  name = "q3_4",
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

        for (data in make_heart_cases()) {
            actual <- joint_diet_test(data)
            full <- lm(heart_rate ~ daily_steps * on_diet, data)
            null <- lm(heart_rate ~ daily_steps, data)
            testthat::expect_true(is.list(actual) && all(c("full", "null", "p_value") %in% names(actual)))
            for (name in c("full", "null")) {
                target <- if (name == "full") full else null
                testthat::expect_s3_class(actual[[name]], "lm")
                testthat::expect_equal(as.numeric(fitted(actual[[name]])), as.numeric(fitted(target)), tolerance = 1e-7)
            }
            expected <- anova(null, full)[2, "Pr(>F)"]
            testthat::expect_true(is.numeric(actual$p_value) && length(actual$p_value) == 1 && is.finite(actual$p_value))
            testthat::expect_equal(as.numeric(actual$p_value), as.numeric(expected), tolerance = 1e-7)
        }

        })

      }
    )
  )
)