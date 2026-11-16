test = list(
  name = "q2_5",
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
                for(bmi in list(c(25,35),c(42,20,31,31),c(18.5))) {
                    value <- predict_probability(model,bmi)
                    expected <- plogis(predict(model,newdata=data.frame(BMI=bmi),type="link"))
                    testthat::expect_true(is.numeric(value)&&length(value)==length(bmi)&&all(is.finite(value))&&all(value>=0&value<=1))
                    testthat::expect_true(all(abs(unname(value)-unname(expected))<=5.1e-3))
                }
            }
        })

      }
    )
  )
)