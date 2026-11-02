test = list(
  name = "q2_3",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.75,
      code = {
        local({

            make_sleep_cases <- function() {
                lapply(seq_len(4), function(j) {
                    d <- expand.grid(blue_light = c("no", "yes"),
                        caffeine_cutoff = c("no", "yes"), participant = seq_len(4 + 3 * j))
                    d$blue_light <- factor(d$blue_light, levels = c("no", "yes"))
                    d$caffeine_cutoff <- factor(d$caffeine_cutoff, levels = c("no", "yes"))
                    i <- seq_len(nrow(d))
                    a <- as.numeric(d$blue_light == "yes")
                    b <- as.numeric(d$caffeine_cutoff == "yes")
                    d$sleep_score <- 4 + c(0, 0.3, -0.6, 1)[j] * a +
                        c(0.2, -0.7, 0.8, 0)[j] * b +
                        c(0, 0.4, -1.1, 0.2)[j] * a * b +
                        sin(i * 1.17) + 0.7 * cos(i * 0.47 + j)
                    d
                })
            }

            testthat::expect_true(is.function(sleep_partial_test))
            for (d in make_sleep_cases()) {
                actual <- sleep_partial_test(d, drop = "blue_light")
                full <- lm(sleep_score ~ blue_light * caffeine_cutoff, data = d)
                reduced <- lm(sleep_score ~ caffeine_cutoff, data = d)

                comparison <- anova(reduced, full)
                testthat::expect_true(is.list(actual))
                testthat::expect_true(all(c("full", "reduced", "df_num", "p_value") %in% names(actual)))
                for (name in c("full", "reduced")) {
                    target <- if (name == "full") full else reduced
                    model <- actual[[name]]
                    testthat::expect_s3_class(model, "lm")
                    testthat::expect_equal(nobs(model), nrow(d))
                    testthat::expect_equal(df.residual(model), df.residual(target))
                    testthat::expect_equal(as.numeric(fitted(model)), as.numeric(fitted(target)), tolerance = 1e-8)
                }
                testthat::expect_equal(as.numeric(actual$df_num), as.numeric(comparison[2, "Df"]))
                testthat::expect_true(is.numeric(actual$p_value) && length(actual$p_value) == 1L && is.finite(actual$p_value))
                testthat::expect_equal(as.numeric(actual$p_value), as.numeric(comparison[2, "Pr(>F)"]), tolerance = 1e-8)
            }

        })

      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.75,
      code = {
        local({

            make_sleep_cases <- function() {
                lapply(seq_len(4), function(j) {
                    d <- expand.grid(blue_light = c("no", "yes"),
                        caffeine_cutoff = c("no", "yes"), participant = seq_len(4 + 3 * j))
                    d$blue_light <- factor(d$blue_light, levels = c("no", "yes"))
                    d$caffeine_cutoff <- factor(d$caffeine_cutoff, levels = c("no", "yes"))
                    i <- seq_len(nrow(d))
                    a <- as.numeric(d$blue_light == "yes")
                    b <- as.numeric(d$caffeine_cutoff == "yes")
                    d$sleep_score <- 4 + c(0, 0.3, -0.6, 1)[j] * a +
                        c(0.2, -0.7, 0.8, 0)[j] * b +
                        c(0, 0.4, -1.1, 0.2)[j] * a * b +
                        sin(i * 1.17) + 0.7 * cos(i * 0.47 + j)
                    d
                })
            }

            testthat::expect_true(is.function(sleep_partial_test))
            for (d in make_sleep_cases()) {
                actual <- sleep_partial_test(d, drop = "caffeine_cutoff")
                full <- lm(sleep_score ~ blue_light * caffeine_cutoff, data = d)
                reduced <- lm(sleep_score ~ blue_light, data = d)

                comparison <- anova(reduced, full)
                testthat::expect_true(is.list(actual))
                testthat::expect_true(all(c("full", "reduced", "df_num", "p_value") %in% names(actual)))
                for (name in c("full", "reduced")) {
                    target <- if (name == "full") full else reduced
                    model <- actual[[name]]
                    testthat::expect_s3_class(model, "lm")
                    testthat::expect_equal(nobs(model), nrow(d))
                    testthat::expect_equal(df.residual(model), df.residual(target))
                    testthat::expect_equal(as.numeric(fitted(model)), as.numeric(fitted(target)), tolerance = 1e-8)
                }
                testthat::expect_equal(as.numeric(actual$df_num), as.numeric(comparison[2, "Df"]))
                testthat::expect_true(is.numeric(actual$p_value) && length(actual$p_value) == 1L && is.finite(actual$p_value))
                testthat::expect_equal(as.numeric(actual$p_value), as.numeric(comparison[2, "Pr(>F)"]), tolerance = 1e-8)
            }

            expected <- sleep_partial_test(sleep_df, drop = "caffeine_cutoff")
            testthat::expect_equal(as.numeric(caffeine_test$p_value), as.numeric(expected$p_value), tolerance = 1e-8)
            testthat::expect_equal(as.numeric(fitted(caffeine_test$reduced)), as.numeric(fitted(expected$reduced)), tolerance = 1e-8)

        })

      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.5,
      code = {
        local({

            make_sleep_cases <- function() {
                lapply(seq_len(4), function(j) {
                    d <- expand.grid(blue_light = c("no", "yes"),
                        caffeine_cutoff = c("no", "yes"), participant = seq_len(4 + 3 * j))
                    d$blue_light <- factor(d$blue_light, levels = c("no", "yes"))
                    d$caffeine_cutoff <- factor(d$caffeine_cutoff, levels = c("no", "yes"))
                    i <- seq_len(nrow(d))
                    a <- as.numeric(d$blue_light == "yes")
                    b <- as.numeric(d$caffeine_cutoff == "yes")
                    d$sleep_score <- 4 + c(0, 0.3, -0.6, 1)[j] * a +
                        c(0.2, -0.7, 0.8, 0)[j] * b +
                        c(0, 0.4, -1.1, 0.2)[j] * a * b +
                        sin(i * 1.17) + 0.7 * cos(i * 0.47 + j)
                    d
                })
            }

            testthat::expect_true(is.function(sleep_partial_test))
            for (d in make_sleep_cases()) {
                actual <- sleep_partial_test(d, drop = "interaction")
                full <- lm(sleep_score ~ blue_light * caffeine_cutoff, data = d)
                reduced <- lm(sleep_score ~ blue_light + caffeine_cutoff, data = d)

                comparison <- anova(reduced, full)
                testthat::expect_true(is.list(actual))
                testthat::expect_true(all(c("full", "reduced", "df_num", "p_value") %in% names(actual)))
                for (name in c("full", "reduced")) {
                    target <- if (name == "full") full else reduced
                    model <- actual[[name]]
                    testthat::expect_s3_class(model, "lm")
                    testthat::expect_equal(nobs(model), nrow(d))
                    testthat::expect_equal(df.residual(model), df.residual(target))
                    testthat::expect_equal(as.numeric(fitted(model)), as.numeric(fitted(target)), tolerance = 1e-8)
                }
                testthat::expect_equal(as.numeric(actual$df_num), as.numeric(comparison[2, "Df"]))
                testthat::expect_true(is.numeric(actual$p_value) && length(actual$p_value) == 1L && is.finite(actual$p_value))
                testthat::expect_equal(as.numeric(actual$p_value), as.numeric(comparison[2, "Pr(>F)"]), tolerance = 1e-8)
            }

            expected <- sleep_partial_test(sleep_df, drop = "interaction")
            testthat::expect_equal(as.numeric(interaction_test$p_value), as.numeric(expected$p_value), tolerance = 1e-8)
            testthat::expect_equal(as.numeric(fitted(interaction_test$reduced)), as.numeric(fitted(expected$reduced)), tolerance = 1e-8)

        })

      }
    )
  )
)