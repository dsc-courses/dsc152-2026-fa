test = list(
  name = "q3_1",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1.5,
      code = {
        local({
            fixtures <- list(hitters,
                data.frame(Years=seq(1,24,length.out=37), Salary=exp(4+.012*seq(1,24,length.out=37)+.8*sin(seq_len(37)))),
                transform(hitters[seq(3,240,by=3),], Salary=2+sqrt(Salary)*7))
            for (data in fixtures) {
                answer <- fit_salary_models(data)
                testthat::expect_true(is.list(answer))
                testthat::expect_true(all(c("untransformed","logged","pval_untransformed","pval_logged") %in% names(answer)))
                expected <- list(untransformed=lm(Salary~Years,data),logged=lm(log(Salary)~Years,data))
                probe <- data.frame(Years=c(2,9,19))
                for(name in names(expected)) {
                    model <- answer[[name]]
                    testthat::expect_true(inherits(model,"lm"))
                    testthat::expect_equal(unname(predict(model,probe)),unname(predict(expected[[name]],probe)),tolerance=1e-6)
                    testthat::expect_equal(unname(predict(model,newdata=data)),unname(predict(expected[[name]],newdata=data)),tolerance=1e-6)
                    p <- answer[[paste0("pval_",name)]]
                    testthat::expect_true(is.numeric(p)&&length(p)==1L&&is.finite(p)&&p>=0&&p<=1)
                    testthat::expect_lte(abs(unname(p)-unname(summary(expected[[name]])$coefficients["Years","Pr(>|t|)"])),5.1e-5)
                }
            }
        })

      }
    )
  )
)