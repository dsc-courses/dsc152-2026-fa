test = list(
  name = "q2_1",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.75,
      code = {
        local({

            variables <- c("Years", "Hits", "HmRun", "Runs", "RBI", "Walks", "PutOuts", "League", "Division")
            primary_formula <- 'Salary ~ Years + Hits + HmRun + Runs + RBI + Walks + PutOuts + League + Division'
            make_fixture <- function(which = 1L) {
                if (which == 1L) return(hitters)
                count <- if (which == 2L) 117L else 143L
                rows <- if (which == 2L) seq_len(count) else rev(seq_len(nrow(hitters)))[seq_len(count)]
                d <- hitters[rows, , drop = FALSE]
                d$League <- factor(d$League, levels = rev(levels(hitters$League)))
                d$Division <- factor(d$Division, levels = rev(levels(hitters$Division)))
                design <- model.matrix(as.formula(primary_formula), d)
                beta <- rep(0, ncol(design))
                names(beta) <- colnames(design)
                beta["(Intercept)"] <- 700
                beta["Years"] <- if (which == 2L) 0.35 else -1.1
                beta["Hits"] <- if (which == 2L) 1.9 else -0.8
                beta["Walks"] <- if (which == 2L) 3.1 else 0.6
                beta[grep("Division", names(beta))] <- if (which == 2L) 110 else -50
                beta[grep("League", names(beta))] <- if (which == 2L) 0 else 80
                raw_error <- sin(seq_len(nrow(d)) * sqrt(2) + which) +
                    cos(seq_len(nrow(d)) * sqrt(3) - which)
                error <- qr.resid(qr(design), raw_error)
                error <- error / sqrt(mean(error^2)) * if (which == 2L) 140 else 90
                d$Salary <- unname(drop(design %*% beta) + error)
                d
            }
            expected_primary <- function(data, alpha = 0.05) {
                model <- lm(as.formula(primary_formula), data = data)
                p_value <- unname(summary(model)$coefficients["Years", "Pr(>|t|)"])
                list(model = model, p_value = p_value, reject = p_value < alpha)
            }
            # This oracle uses sequential one-term F comparisons. With these numeric
            # predictors and two-level factors each candidate removes one coefficient.
            expected_backward <- function(data, alpha = 0.05) {
                model <- lm(as.formula(primary_formula), data = data)
                repeat {
                    comparison <- drop1(model, test = "F")
                    candidates <- setdiff(rownames(comparison), c("<none>", "Years"))
                    if (length(candidates) == 0L) break
                    p <- comparison[candidates, "Pr(>F)"]
                    names(p) <- candidates
                    if (max(p) <= alpha) break
                    remove <- names(which.max(p))
                    model <- update(model, as.formula(paste(". ~ . -", remove)))
                }
                list(model = model,
                     p_value = unname(summary(model)$coefficients["Years", "Pr(>|t|)"]),
                     variables = attr(terms(model), "term.labels"))
            }
            check_model <- function(actual, expected, data) {
                testthat::expect_true(inherits(actual, "lm"))
                testthat::expect_equal(nobs(actual), nrow(data))
                testthat::expect_equal(actual$rank, expected$rank)
                testthat::expect_equal(unname(as.numeric(predict(actual, newdata = data))),
                                       unname(as.numeric(predict(expected, newdata = data))), tolerance = 1e-6)
                probe <- data[seq_len(7L), , drop = FALSE]
                probe$Years <- probe$Years + 0.4
                probe$Hits <- probe$Hits + 3.7
                probe$Walks <- probe$Walks - 1.9
                testthat::expect_equal(unname(as.numeric(predict(actual, newdata = probe))),
                                       unname(as.numeric(predict(expected, newdata = probe))), tolerance = 1e-6)
            }
            check_p <- function(actual, expected) {
                testthat::expect_true(is.numeric(actual) && length(actual) == 1L && is.finite(actual))
                testthat::expect_true(actual >= 0 && actual <= 1)
                testthat::expect_lte(abs(unname(actual) - unname(expected)), 5.1e-5)
            }

            testthat::expect_true(is.function(fit_primary))
            for (i in 1:3) {
                d <- make_fixture(i)
                result <- fit_primary(d)
                expected <- expected_primary(d)
                testthat::expect_true(is.list(result))
                check_model(result$model, expected$model, d)
                check_p(result$p_value, expected$p_value)
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

            variables <- c("Years", "Hits", "HmRun", "Runs", "RBI", "Walks", "PutOuts", "League", "Division")
            primary_formula <- 'Salary ~ Years + Hits + HmRun + Runs + RBI + Walks + PutOuts + League + Division'
            make_fixture <- function(which = 1L) {
                if (which == 1L) return(hitters)
                count <- if (which == 2L) 117L else 143L
                rows <- if (which == 2L) seq_len(count) else rev(seq_len(nrow(hitters)))[seq_len(count)]
                d <- hitters[rows, , drop = FALSE]
                d$League <- factor(d$League, levels = rev(levels(hitters$League)))
                d$Division <- factor(d$Division, levels = rev(levels(hitters$Division)))
                design <- model.matrix(as.formula(primary_formula), d)
                beta <- rep(0, ncol(design))
                names(beta) <- colnames(design)
                beta["(Intercept)"] <- 700
                beta["Years"] <- if (which == 2L) 0.35 else -1.1
                beta["Hits"] <- if (which == 2L) 1.9 else -0.8
                beta["Walks"] <- if (which == 2L) 3.1 else 0.6
                beta[grep("Division", names(beta))] <- if (which == 2L) 110 else -50
                beta[grep("League", names(beta))] <- if (which == 2L) 0 else 80
                raw_error <- sin(seq_len(nrow(d)) * sqrt(2) + which) +
                    cos(seq_len(nrow(d)) * sqrt(3) - which)
                error <- qr.resid(qr(design), raw_error)
                error <- error / sqrt(mean(error^2)) * if (which == 2L) 140 else 90
                d$Salary <- unname(drop(design %*% beta) + error)
                d
            }
            expected_primary <- function(data, alpha = 0.05) {
                model <- lm(as.formula(primary_formula), data = data)
                p_value <- unname(summary(model)$coefficients["Years", "Pr(>|t|)"])
                list(model = model, p_value = p_value, reject = p_value < alpha)
            }
            # This oracle uses sequential one-term F comparisons. With these numeric
            # predictors and two-level factors each candidate removes one coefficient.
            expected_backward <- function(data, alpha = 0.05) {
                model <- lm(as.formula(primary_formula), data = data)
                repeat {
                    comparison <- drop1(model, test = "F")
                    candidates <- setdiff(rownames(comparison), c("<none>", "Years"))
                    if (length(candidates) == 0L) break
                    p <- comparison[candidates, "Pr(>F)"]
                    names(p) <- candidates
                    if (max(p) <= alpha) break
                    remove <- names(which.max(p))
                    model <- update(model, as.formula(paste(". ~ . -", remove)))
                }
                list(model = model,
                     p_value = unname(summary(model)$coefficients["Years", "Pr(>|t|)"]),
                     variables = attr(terms(model), "term.labels"))
            }
            check_model <- function(actual, expected, data) {
                testthat::expect_true(inherits(actual, "lm"))
                testthat::expect_equal(nobs(actual), nrow(data))
                testthat::expect_equal(actual$rank, expected$rank)
                testthat::expect_equal(unname(as.numeric(predict(actual, newdata = data))),
                                       unname(as.numeric(predict(expected, newdata = data))), tolerance = 1e-6)
                probe <- data[seq_len(7L), , drop = FALSE]
                probe$Years <- probe$Years + 0.4
                probe$Hits <- probe$Hits + 3.7
                probe$Walks <- probe$Walks - 1.9
                testthat::expect_equal(unname(as.numeric(predict(actual, newdata = probe))),
                                       unname(as.numeric(predict(expected, newdata = probe))), tolerance = 1e-6)
            }
            check_p <- function(actual, expected) {
                testthat::expect_true(is.numeric(actual) && length(actual) == 1L && is.finite(actual))
                testthat::expect_true(actual >= 0 && actual <= 1)
                testthat::expect_lte(abs(unname(actual) - unname(expected)), 5.1e-5)
            }

            d <- make_fixture(2L)
            p <- expected_primary(d)$p_value
            for (alpha in c(p / 2, (1 + p) / 2)) {
                result <- fit_primary(d, alpha = alpha)
                expected <- expected_primary(d, alpha)
                check_p(result$p_value, expected$p_value)
                testthat::expect_true(is.logical(result$reject) && length(result$reject) == 1L && !is.na(result$reject))
                testthat::expect_equal(unname(result$reject), expected$reject)
            }
        })

      }
    )
  )
)