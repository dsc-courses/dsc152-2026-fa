test = list(
  name = "q2_6",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 1.5,
      code = {
        local({
            bmi <- seq(17,48,length.out=120)
            u <- ((seq_len(120)*37)%%127+.5)/127
            fixtures <- list(diabetes_lab8,
                data.frame(BMI=bmi,Outcome=as.integer(u<plogis(-2+.06*bmi))),
                data.frame(BMI=rev(bmi),Outcome=as.integer(u<plogis(2-.065*rev(bmi)))))
            for(data in fixtures) {
                model <- glm(Outcome~BMI,data=data,family=binomial())
                for(p in c(.2,.5,.75)) {
                    value <- bmi_at_probability(model,p)
                    expected <- (log(p/(1-p))-coef(model)["(Intercept)"])/coef(model)["BMI"]
                    testthat::expect_true(is.numeric(value)&&length(value)==1L&&is.finite(value))
                    testthat::expect_lte(abs(unname(value)-unname(expected)),5.1e-3)
                    testthat::expect_lte(abs(unname(predict(model,data.frame(BMI=unname(value)),type="response"))-p),2e-4)
                }
            }
        })

      }
    )
  )
)