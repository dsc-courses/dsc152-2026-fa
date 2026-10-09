test = list(
  name = "q2_4",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0,
      code = {
        testthat::expect_true(is.function(permute_stats))
        testthat::expect_equal(length(perm_stats), 5000)
        testthat::expect_true(all(is.finite(perm_stats) & perm_stats >= 0))
        # Accept lecture b/B counting and a tiny tolerance for mathematical ties.
        observed_for_tail <- unname(obs_test_stat)
        tail_tolerance <- 1e-8 * max(1, abs(observed_for_tail))
        expected_tail <- c(mean(perm_stats >= observed_for_tail),
                           mean(perm_stats >= observed_for_tail - tail_tolerance))
        testthat::expect_true(is.numeric(p_val_perm) && length(p_val_perm) == 1L && is.finite(p_val_perm))
        testthat::expect_true(any(abs(unname(p_val_perm) - expected_tail) < 1e-10))
        # An exact enumeration supplies a reference independent of the shuffle implementation.
        splits <- combn(nrow(hand_code_df), 5)
        exact_stats <- apply(splits, 2, function(idx) abs(mean(hand_code_df$score[idx]) - mean(hand_code_df$score[-idx])))
        testthat::expect_true(all(vapply(perm_stats, function(x) any(abs(x - exact_stats) < 1e-8), logical(1))))
        exact_p <- mean(exact_stats >= obs_test_stat - 1e-10)
        testthat::expect_lt(abs(p_val_perm - exact_p), 0.035)

      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.5,
      code = {
        local({
        testthat::expect_true(is.function(permute_stats))
        set.seed(39031)
        for (n_perms in sample(9:35, 3)) {
          df <- data.frame(score=rnorm(7), group=rep(c("A", "B"), c(2, 5)))
          marker <- runif(1, 3, 9)
          calls <- 0L
          stat_fn <- function(shuffled) { calls <<- calls + 1L; marker }
          result <- permute_stats(df, stat_fn, n_perms)
          testthat::expect_true(is.numeric(result))
          testthat::expect_equal(length(result), n_perms)
          testthat::expect_equal(as.numeric(result), rep(marker, n_perms))
          testthat::expect_gte(calls, 1L)
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
        testthat::expect_true(is.function(permute_stats))
        set.seed(39035)
        for (sizes in list(c(2, 5), c(4, 3), c(1, 1))) {
          df <- data.frame(observation=rnorm(sum(sizes)),
                           condition=rep(c("cedar", "elm"), sizes))
          valid <- TRUE
          inspect_shuffle <- function(shuffled) {
            valid <<- valid && is.data.frame(shuffled) &&
              isTRUE(all.equal(sort(shuffled[[1]]), sort(df[[1]]))) &&
              identical(sort(as.character(shuffled[[2]])), sort(as.character(df[[2]])))
            unname(abs(diff(tapply(shuffled[[1]], shuffled[[2]], mean))))
          }
          result <- permute_stats(df, inspect_shuffle, 47)
          testthat::expect_true(valid)
          testthat::expect_true(is.numeric(result) && length(result) == 47 && all(is.finite(result)))
          allocations <- combn(nrow(df), sizes[1])
          possible <- apply(allocations, 2, function(idx) abs(mean(df[[1]][idx]) - mean(df[[1]][-idx])))
          testthat::expect_true(all(vapply(result, function(x) any(abs(x - possible) < 1e-8), logical(1))))
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
        testthat::expect_true(is.function(permute_stats))
        set.seed(39027)
        for (summary_fn in list(mean, median)) {
          df <- data.frame(value=runif(1, 1.1, 3) * c(-3, 0, 4, 7, 11, 18) + runif(1, -10, 10),
                           condition=rep(c("north", "south"), c(2, 4)))
          stat_fn <- function(shuffled) unname(abs(diff(tapply(shuffled[[1]], shuffled[[2]], summary_fn))))
          allocations <- combn(nrow(df), 2)
          possible <- apply(allocations, 2, function(idx) abs(summary_fn(df[[1]][idx]) - summary_fn(df[[1]][-idx])))
          set.seed(39028)
          result <- permute_stats(df, stat_fn, 1200)
          testthat::expect_true(is.numeric(result) && length(result) == 1200 && all(is.finite(result)))
          support <- unique(round(possible, 8))
          # Compare distances; rounding can put equivalent values on opposite sides of a boundary.
          testthat::expect_true(all(vapply(result, function(x) any(abs(x - possible) <= 1e-8), logical(1))))
          exact_frequencies <- vapply(support, function(v) mean(abs(possible - v) < 1e-7), numeric(1))
          observed_frequencies <- vapply(support, function(v) mean(abs(result - v) < 1e-7), numeric(1))
          testthat::expect_lt(max(abs(exact_frequencies - observed_frequencies)), .08)
          set.seed(39029)
          second_result <- permute_stats(df, stat_fn, 1200)
          testthat::expect_false(identical(result, second_result))
        }
        })

      }
    )
  )
)