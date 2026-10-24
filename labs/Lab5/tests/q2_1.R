test = list(
  name = "q2_1",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.75,
      code = {
        local({
        make_cases <- function() {
            i <- seq_len(84)
            group <- factor(rep(c("amber", "violet", "blue"), each = 28),
                            levels = c("violet", "amber", "blue", "unused"))
            x <- ((i * 17) %% 83) / 11 - 4
            z <- sin(i / 3)
            noise <- qnorm((((i * 29) %% 83) + 0.5) / 84)
            data <- data.frame(x = x, z = z, group = group,
                y = 2 + 0.8 * x + c(0.7, -1.1, 1.8)[as.integer(group)] +
                    (0.3 + as.integer(group) / 2) * noise)
            first <- lm(y ~ x + group, data = data)
            reversed <- data[rev(seq_len(nrow(data))), ]
            reversed$y <- 3 * reversed$y - 2 + 0.4 * reversed$z
            second <- lm(y ~ x + z + group, data = reversed,
                         contrasts = list(group = "contr.sum"))
            third <- lm(y ~ x + z, data = data)
            list(list(model = first, groups = data$group),
                 list(model = second, groups = reversed$group),
                 list(model = third, groups = as.character(data$group)))
        }
            testthat::expect_true(is.function(diagnostic_summary))
            for (setting in make_cases()) {
                actual <- diagnostic_summary(setting$model, setting$groups)
                testthat::expect_true(is.list(actual))
                testthat::expect_true(all(c("residuals", "fitted", "shapiro_p", "group_sd") %in% names(actual)))
                for (name in c("residuals", "fitted")) {
                    target <- if (name == "residuals") residuals(setting$model) else fitted(setting$model)
                    testthat::expect_true(is.numeric(actual[[name]]))
                    testthat::expect_equal(as.numeric(actual[[name]]), as.numeric(target), tolerance = 1e-7)
                }
            }
        })
      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.5,
      code = {
        local({
        make_cases <- function() {
            i <- seq_len(84)
            group <- factor(rep(c("amber", "violet", "blue"), each = 28),
                            levels = c("violet", "amber", "blue", "unused"))
            x <- ((i * 17) %% 83) / 11 - 4
            z <- sin(i / 3)
            noise <- qnorm((((i * 29) %% 83) + 0.5) / 84)
            data <- data.frame(x = x, z = z, group = group,
                y = 2 + 0.8 * x + c(0.7, -1.1, 1.8)[as.integer(group)] +
                    (0.3 + as.integer(group) / 2) * noise)
            first <- lm(y ~ x + group, data = data)
            reversed <- data[rev(seq_len(nrow(data))), ]
            reversed$y <- 3 * reversed$y - 2 + 0.4 * reversed$z
            second <- lm(y ~ x + z + group, data = reversed,
                         contrasts = list(group = "contr.sum"))
            third <- lm(y ~ x + z, data = data)
            list(list(model = first, groups = data$group),
                 list(model = second, groups = reversed$group),
                 list(model = third, groups = as.character(data$group)))
        }
            testthat::expect_true(is.function(diagnostic_summary))
            for (setting in make_cases()) {
                actual <- diagnostic_summary(setting$model, setting$groups)$shapiro_p
                target <- shapiro.test(residuals(setting$model))$p.value
                testthat::expect_true(is.numeric(actual) && length(actual) == 1L && is.finite(actual))
                testthat::expect_true(actual > 0 && actual <= 1)
                testthat::expect_equal(log(as.numeric(actual)), log(target), tolerance = 1e-6)
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
        make_cases <- function() {
            i <- seq_len(84)
            group <- factor(rep(c("amber", "violet", "blue"), each = 28),
                            levels = c("violet", "amber", "blue", "unused"))
            x <- ((i * 17) %% 83) / 11 - 4
            z <- sin(i / 3)
            noise <- qnorm((((i * 29) %% 83) + 0.5) / 84)
            data <- data.frame(x = x, z = z, group = group,
                y = 2 + 0.8 * x + c(0.7, -1.1, 1.8)[as.integer(group)] +
                    (0.3 + as.integer(group) / 2) * noise)
            first <- lm(y ~ x + group, data = data)
            reversed <- data[rev(seq_len(nrow(data))), ]
            reversed$y <- 3 * reversed$y - 2 + 0.4 * reversed$z
            second <- lm(y ~ x + z + group, data = reversed,
                         contrasts = list(group = "contr.sum"))
            third <- lm(y ~ x + z, data = data)
            list(list(model = first, groups = data$group),
                 list(model = second, groups = reversed$group),
                 list(model = third, groups = as.character(data$group)))
        }
            testthat::expect_true(is.function(diagnostic_summary))
            for (setting in make_cases()) {
                actual <- diagnostic_summary(setting$model, setting$groups)$group_sd
                target <- vapply(split(residuals(setting$model), setting$groups, drop = TRUE), sd, numeric(1))
                testthat::expect_true(is.numeric(actual))
                testthat::expect_true(!is.null(names(actual)))
                testthat::expect_true(!anyDuplicated(names(actual)))
                testthat::expect_true(all(names(target) %in% names(actual)))
                testthat::expect_equal(as.numeric(actual[names(target)]), as.numeric(target), tolerance = 1e-7)
                extras <- setdiff(names(actual), names(target))
                if (length(extras)) {
                    allowed <- if (is.factor(setting$groups)) setdiff(levels(setting$groups), names(target)) else character()
                    testthat::expect_true(all(extras %in% allowed))
                    testthat::expect_true(all(is.na(actual[extras])))
                }
            }
        })
      }
    )
  )
)