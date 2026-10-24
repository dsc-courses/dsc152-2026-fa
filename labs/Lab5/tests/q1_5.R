test = list(
  name = "q1_5",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.5,
      code = {
        local({
            testthat::expect_true(is.function(partial_f_manual))

            pairs <- list(list(full = lm(Petal.Length ~ Species + Petal.Width, iris),
                               reduced = lm(Petal.Length ~ Petal.Width, iris)))
            for (j in 1:4) {
                n <- c(39, 66, 95, 127)[j]
                i <- seq_len(n)
                data <- data.frame(x = seq(-1, 1, length.out = n), z = cos(i * 0.7), w = sin(i * 1.9))
                data$y <- c(0, 0.15, 1.2, -0.6)[j] * data$x +
                    c(0, -0.3, 0.8, 0)[j] * data$z + 0.21 * data$w + sin(i * 1.17)
                pairs[[length(pairs) + 1]] <- switch(as.character(j),
                    "1" = list(full = lm(y ~ x, data), reduced = lm(y ~ 1, data)),
                    "2" = list(full = lm(y ~ x + z, data), reduced = lm(y ~ 1, data)),
                    "3" = list(full = lm(y ~ x + z + w, data), reduced = lm(y ~ w, data)),
                    "4" = list(full = lm(y ~ x + z + w + I(x^2), data), reduced = lm(y ~ x + z, data)))
            }

            for (pair in pairs) {
                actual <- partial_f_manual(pair$full, pair$reduced)
                expected_table <- anova(pair$reduced, pair$full)
                testthat::expect_true(is.list(actual))
                testthat::expect_true(all(c("n", "k", "p", "F_stat", "p_value") %in% names(actual)))
                targets <- list(n = nobs(pair$full), k = pair$full$rank,
                    p = pair$full$rank - pair$reduced$rank,
                    F_stat = expected_table[2, "F"], p_value = expected_table[2, "Pr(>F)"])
                for (name in c("n", "k", "p")) {
                    testthat::expect_true(is.numeric(actual[[name]]))
                    testthat::expect_length(actual[[name]], 1)
                    testthat::expect_equal(as.numeric(actual[[name]]), as.numeric(targets[[name]]), tolerance = 1e-7)
                }
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
            testthat::expect_true(is.function(partial_f_manual))

            pairs <- list(list(full = lm(Petal.Length ~ Species + Petal.Width, iris),
                               reduced = lm(Petal.Length ~ Petal.Width, iris)))
            for (j in 1:4) {
                n <- c(39, 66, 95, 127)[j]
                i <- seq_len(n)
                data <- data.frame(x = seq(-1, 1, length.out = n), z = cos(i * 0.7), w = sin(i * 1.9))
                data$y <- c(0, 0.15, 1.2, -0.6)[j] * data$x +
                    c(0, -0.3, 0.8, 0)[j] * data$z + 0.21 * data$w + sin(i * 1.17)
                pairs[[length(pairs) + 1]] <- switch(as.character(j),
                    "1" = list(full = lm(y ~ x, data), reduced = lm(y ~ 1, data)),
                    "2" = list(full = lm(y ~ x + z, data), reduced = lm(y ~ 1, data)),
                    "3" = list(full = lm(y ~ x + z + w, data), reduced = lm(y ~ w, data)),
                    "4" = list(full = lm(y ~ x + z + w + I(x^2), data), reduced = lm(y ~ x + z, data)))
            }

            for (pair in pairs) {
                actual <- partial_f_manual(pair$full, pair$reduced)
                expected_table <- anova(pair$reduced, pair$full)
                testthat::expect_true(is.list(actual))
                testthat::expect_true(all(c("n", "k", "p", "F_stat", "p_value") %in% names(actual)))
                targets <- list(n = nobs(pair$full), k = pair$full$rank,
                    p = pair$full$rank - pair$reduced$rank,
                    F_stat = expected_table[2, "F"], p_value = expected_table[2, "Pr(>F)"])
                for (name in c("F_stat")) {
                    testthat::expect_true(is.numeric(actual[[name]]))
                    testthat::expect_length(actual[[name]], 1)
                    testthat::expect_equal(as.numeric(actual[[name]]), as.numeric(targets[[name]]), tolerance = 1e-7)
                }
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
            testthat::expect_true(is.function(partial_f_manual))

            pairs <- list(list(full = lm(Petal.Length ~ Species + Petal.Width, iris),
                               reduced = lm(Petal.Length ~ Petal.Width, iris)))
            for (j in 1:4) {
                n <- c(39, 66, 95, 127)[j]
                i <- seq_len(n)
                data <- data.frame(x = seq(-1, 1, length.out = n), z = cos(i * 0.7), w = sin(i * 1.9))
                data$y <- c(0, 0.15, 1.2, -0.6)[j] * data$x +
                    c(0, -0.3, 0.8, 0)[j] * data$z + 0.21 * data$w + sin(i * 1.17)
                pairs[[length(pairs) + 1]] <- switch(as.character(j),
                    "1" = list(full = lm(y ~ x, data), reduced = lm(y ~ 1, data)),
                    "2" = list(full = lm(y ~ x + z, data), reduced = lm(y ~ 1, data)),
                    "3" = list(full = lm(y ~ x + z + w, data), reduced = lm(y ~ w, data)),
                    "4" = list(full = lm(y ~ x + z + w + I(x^2), data), reduced = lm(y ~ x + z, data)))
            }

            for (pair in pairs) {
                actual <- partial_f_manual(pair$full, pair$reduced)
                expected_table <- anova(pair$reduced, pair$full)
                testthat::expect_true(is.list(actual))
                testthat::expect_true(all(c("n", "k", "p", "F_stat", "p_value") %in% names(actual)))
                targets <- list(n = nobs(pair$full), k = pair$full$rank,
                    p = pair$full$rank - pair$reduced$rank,
                    F_stat = expected_table[2, "F"], p_value = expected_table[2, "Pr(>F)"])
                for (name in c("p_value")) {
                    testthat::expect_true(is.numeric(actual[[name]]))
                    testthat::expect_length(actual[[name]], 1)
                    testthat::expect_equal(as.numeric(actual[[name]]), as.numeric(targets[[name]]), tolerance = 1e-7)
                }
            }
        })

      }
    )
  )
)