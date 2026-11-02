test = list(
  name = "q1_1",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.5,
      code = {
        local({

        make_sleep_cases <- function() {
            cases <- list(sleep_df)
            for (j in 1:3) {
                data <- expand.grid(blue_light = c("no", "yes"),
                                    caffeine_cutoff = c("no", "yes"),
                                    replicate = seq_len(5 + j), stringsAsFactors = FALSE)
                i <- seq_len(nrow(data))
                b <- as.numeric(data$blue_light == "yes")
                c <- as.numeric(data$caffeine_cutoff == "yes")
                data$sleep_score <- 4.4 + c(-0.7, 0.8, 0)[j] * b +
                    c(0.2, -0.6, 0.4)[j] * c + c(1.1, -0.9, 0.3)[j] * b * c +
                    0.19 * sin(i * (1.2 + j / 9))
                data$blue_light <- factor(data$blue_light, levels = c("no", "yes"))
                data$caffeine_cutoff <- factor(data$caffeine_cutoff, levels = c("no", "yes"))
                data <- data[-c(2 + j, 9 + j), c("sleep_score", "blue_light", "caffeine_cutoff")]
                data <- data[rev(seq_len(nrow(data))), ]
                if (j == 3) {
                    data$blue_light <- relevel(data$blue_light, "yes")
                    data$caffeine_cutoff <- relevel(data$caffeine_cutoff, "yes")
                }
                cases[[length(cases) + 1]] <- data
            }
            cases
        }

            testthat::expect_true(is.function(summarize_sleep))
            for (data in make_sleep_cases()) {
                actual <- summarize_sleep(data)
                expected <- aggregate(sleep_score ~ blue_light + caffeine_cutoff, data, mean)
                expected <- expected[order(as.character(expected$blue_light), as.character(expected$caffeine_cutoff)), ]
                testthat::expect_s3_class(actual, "data.frame")
                testthat::expect_equal(names(actual), c("blue_light", "caffeine_cutoff", "mean_sleep_score"))
                testthat::expect_equal(nrow(actual), 4L)
                testthat::expect_equal(as.character(actual$blue_light), as.character(expected$blue_light))
                testthat::expect_equal(as.character(actual$caffeine_cutoff), as.character(expected$caffeine_cutoff))
                testthat::expect_true(is.numeric(actual$mean_sleep_score))
                testthat::expect_equal(as.numeric(actual$mean_sleep_score), as.numeric(expected$sleep_score), tolerance = 1e-7)
            }
        })

      }
    )
  )
)