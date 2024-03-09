

library(tidyverse)
library(data.table)
library(lubridate)
library(survival)
library(nlme)
library(JM)
#library(JMbayes)
#library(JMbayes2)

data("aids") ; aids

data("aids.id") ; aids.id

lmeFit <- lme(CD4 ~ obstime + obstime:drug,
              random = ~ obstime | patient, data = aids)

summary(lmeFit)


coxFit <- coxph(Surv(Time, death) ~ drug, data = aids.id, x = TRUE)

summary(coxFit)

jointFit <- jointModel(lmeFit, coxFit, timeVar = "obstime",
                       method = "piecewise-PH-aGH")

summary(jointFit)

#jointFitBayes <- jm(Surv_object = coxFit, Mixed_objects = lmeFit, time_var = "obstime")
#summary(jointFitBayes)
#pred <- JMbayes2::jm_fit(jointFitBayes, aids[aids$patient==2,])
#pred$preds
#preds <- predict(jointFitBayes, aids)
#aids %>% select(CD4) %>%
#   bind_cols(
#     data.frame(preds$preds)
#   ) %>%
#   ggplot(aes(`CD4...1`, `CD4...2`)) +
#   geom_point()
# 
# multMixedFit <- mvglmer(list(log(serBilir) ~ year + (year | id),
#                              spiders ~ year + (1 | id)), data = pbc2,
#                         families = list(gaussian, binomial))
# 
# CoxFit <- coxph(Surv(years, status2) ~ drug + age, data = pbc2.id, model = TRUE)
# 
# multJMFit <- mvJointModelBayes(multMixedFit, CoxFit, timeVar = "year")
# 
# summary(multJMFit)