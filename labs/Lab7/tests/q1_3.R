test = list(
  name = "q1_3",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.75,
      code = {
        local({
            check_summary <- function(results, correlations, alpha) {
                answer <- selection_rates(results, correlations, alpha)
                testthat::expect_true(is.data.frame(answer))
                columns <- c("pair","cor_x1_x2","rate_full","rate_selected","inflation")
                testthat::expect_equal(names(answer),columns)
                testthat::expect_equal(nrow(answer),length(results))
                keys <- names(results)[order(abs(correlations[names(results)]))]
                testthat::expect_equal(as.character(answer$pair),keys)
                testthat::expect_equal(as.numeric(answer$cor_x1_x2),unname(correlations[keys]),tolerance=.0051)
                full <- vapply(results[keys],function(x)sum(x$pval_full<alpha)/length(x$pval_full),numeric(1))
                selected <- vapply(results[keys],function(x)sum(x$pval_selected<alpha)/length(x$pval_selected),numeric(1))
                testthat::expect_equal(as.numeric(answer$rate_full),unname(full),tolerance=.0051)
                testthat::expect_equal(as.numeric(answer$rate_selected),unname(selected),tolerance=.0051)
                testthat::expect_equal(as.numeric(answer$inflation),unname(selected/alpha),
                                      tolerance=.0051/alpha+.0051)
            }
            inputs <- list(high=list(pval_full=c(.01,.02,.12,.4,.9),pval_selected=c(.01,.03,.04,.3,.9)),
                           low=list(pval_full=c(.03,.07,.3,.8),pval_selected=c(.01,.09,.5,.6)))
            check_summary(inputs,c(low=.08,high=.91),.05)
            check_summary(inputs,c(low=.08,high=.91),.1)
        })

      }
    ),
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.75,
      code = {
        local({
            check_summary <- function(results, correlations, alpha) {
                answer <- selection_rates(results, correlations, alpha)
                testthat::expect_true(is.data.frame(answer))
                columns <- c("pair","cor_x1_x2","rate_full","rate_selected","inflation")
                testthat::expect_equal(names(answer),columns)
                testthat::expect_equal(nrow(answer),length(results))
                keys <- names(results)[order(abs(correlations[names(results)]))]
                testthat::expect_equal(as.character(answer$pair),keys)
                testthat::expect_equal(as.numeric(answer$cor_x1_x2),unname(correlations[keys]),tolerance=.0051)
                full <- vapply(results[keys],function(x)sum(x$pval_full<alpha)/length(x$pval_full),numeric(1))
                selected <- vapply(results[keys],function(x)sum(x$pval_selected<alpha)/length(x$pval_selected),numeric(1))
                testthat::expect_equal(as.numeric(answer$rate_full),unname(full),tolerance=.0051)
                testthat::expect_equal(as.numeric(answer$rate_selected),unname(selected),tolerance=.0051)
                testthat::expect_equal(as.numeric(answer$inflation),unname(selected/alpha),
                                      tolerance=.0051/alpha+.0051)
            }
            inputs <- list(A=list(pval_full=c(.02,.15,.31,.42,.71,.83,.97),pval_selected=c(.01,.03,.16,.3,.4,.5,.9)),
                           B=list(pval_full=c(.04,.21,.6,.95),pval_selected=c(.02,.11,.5,.8)),
                           C=list(pval_full=c(.03,.05,.1,.3,.7,.8),pval_selected=c(.01,.02,.1,.19,.3,.4)),
                           D=list(pval_full=c(.15,.25,.35),pval_selected=c(.07,.08,.9)))
            check_summary(inputs,c(D=-.45,B=.07,A=-.94,C=.33),.2)
            check_summary(inputs[c("D","C","A","B")],c(D=-.45,B=.07,A=-.94,C=.33),.025)
        })

      }
    )
  )
)