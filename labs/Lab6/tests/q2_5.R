test = list(
  name = "q2_5",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.75,
      code = {
        local({

            make_scenarios <- function() {
                lapply(seq_len(5), function(j) {
                    m <- c(4, 7, 10, 13, 16)[j]
                    d <- expand.grid(x1 = 0:1, x2 = 0:1, participant = seq_len(m))
                    i <- seq_len(nrow(d))
                    d$y <- 2 + c(0, 0.25, -0.4, 1.1, -0.2)[j] * d$x1 +
                        c(1, -0.7, 0.2, 0.8, 1.5)[j] * d$x2 +
                        c(0.1, 0, 0.8, -0.6, 0)[j] * d$x1 * d$x2 +
                        sin(i * 1.31 + j / 5) + 0.6 * cos(i * 0.73)
                    d
                })
            }

            testthat::expect_true(is.function(fit_factorial))
            for (d in make_scenarios()) {
                result <- fit_factorial(d)
                testthat::expect_true(is.list(result))
                testthat::expect_true(all(c("model", "p_value") %in% names(result)))
                actual <- result$model
                expected <- lm(y ~ x1 * x2, data = d)
                testthat::expect_true(is.numeric(result$p_value) && length(result$p_value)==1L && is.finite(result$p_value))
                testthat::expect_true(result$p_value>=0 && result$p_value<=1)
                testthat::expect_equal(as.numeric(result$p_value), as.numeric(summary(expected)$coefficients["x1", "Pr(>|t|)"]), tolerance=1e-8)
                testthat::expect_s3_class(actual, "lm")
                testthat::expect_equal(nobs(actual), nrow(d))
                testthat::expect_equal(df.residual(actual), df.residual(expected))
                testthat::expect_equal(as.numeric(model.response(model.frame(actual))), d$y, tolerance = 1e-8)
                testthat::expect_equal(as.numeric(fitted(actual)), as.numeric(fitted(expected)), tolerance = 1e-8)
                probe <- expand.grid(x1 = 0:1, x2 = 0:1)
                testthat::expect_equal(as.numeric(predict(actual, newdata = probe)),
                    as.numeric(predict(expected, newdata = probe)), tolerance = 1e-8)
            }
        })

      }
    )
  )
)