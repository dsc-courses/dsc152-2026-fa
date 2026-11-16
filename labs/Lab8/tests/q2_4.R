test = list(
  name = "q2_4",
  cases = list(
    ottr::TestCase$new(
      hidden = FALSE,
      name = NA,
      points = 0.75,
      code = {
        local({
            bmi <- seq(17,48,length.out=120)
            u <- ((seq_len(120)*37)%%127+.5)/127
            fixtures <- list(diabetes_lab8,
                data.frame(BMI=bmi,Outcome=as.integer(u<plogis(-2+.06*bmi))),
                data.frame(BMI=rev(bmi),Outcome=as.integer(u<plogis(2-.065*rev(bmi)))))
            for(data in fixtures) {
                model <- glm(Outcome~BMI,data=data,family=binomial())
                for(change in c(1,4,10)) {
                    value <- odds_multiplier(model,change)
                    expected <- exp(diff(predict(model,data.frame(BMI=c(25,25+change)),type="link")))
                    testthat::expect_true(is.numeric(value)&&length(value)==1L&&is.finite(value)&&value>0)
                    testthat::expect_lte(abs(unname(value)-unname(expected)),5.1e-3)
                }
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
            bmi <- seq(17,48,length.out=120)
            u <- ((seq_len(120)*37)%%127+.5)/127
            fixtures <- list(diabetes_lab8,
                data.frame(BMI=bmi,Outcome=as.integer(u<plogis(-2+.06*bmi))),
                data.frame(BMI=rev(bmi),Outcome=as.integer(u<plogis(2-.065*rev(bmi)))))
            for(data in fixtures) {
                model <- glm(Outcome~BMI,data=data,family=binomial())
                for(change in c(-7,0,2.5)) {
                    value <- odds_multiplier(model,change)
                    expected <- exp(diff(predict(model,data.frame(BMI=c(31,31+change)),type="link")))
                    testthat::expect_true(is.numeric(value)&&length(value)==1L&&is.finite(value)&&value>0)
                    testthat::expect_lte(abs(unname(value)-unname(expected)),5.1e-3)
                }
            }
        })

      }
    )
  )
)