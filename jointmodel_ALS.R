library (JM); library (openxlsx)

## Load dataset


A <- read.xlsx("mmc2.xlsx") # ALSFRS-R data

B <- read.xlsx("mmc3.xlsx") # Survival data

#### 1. Model fit ####

## Cox model

m <- coxph(Surv(STIME, STATUS) ~ TRT, data = B, x = T)

## Linear mixed model

lme <- lme(TOTAL ~ AVISIT + AVISIT:TRT,
           random = ~ AVISIT + I(AVISIT^2)|ID, data = A, method = "ML")

## Joint model

JM <- jointModel(lme, m, timeVar = "AVISIT", method = "weibull-PH-aGH")

summary(JM) # Model output Table 1 [manuscript]



#### 2. Question 1 ####

## 'Survivor analysis': excluding all with less than 20 months follow-up

lme.surv <- lme(TOTAL ~ AVISIT + AVISIT:TRT, random = ~ AVISIT + I(AVISIT^2)|ID,
                 data = A[A$MAXTIME == 20, ], method = "ML")


summary(lme.surv) # Model output 'survivor analysis'; reverses effect

## Comparison with linear mixed model:

summary(lme) # smaller effect size compared to JM


#### 3a. Question 2 ####

## Textual examples:

exp(0.59 + ((-0.11*0)*-0.08)) # HR on month 0

exp(0.59 + ((-0.11*20)*-0.08)) # HR on month 20

## Effect attributable to ALSFRS-R on month 20:

((-0.11*20)*-0.08) / (0.59 + ((-0.11*20)*-0.08)) # 23%

## HR cox model:

summary(m) # 1.68

## Function to estimate sample size for time-to-event [Schoenfeld, 1981, 1983]

EvSchoenfeld <- function (HR, alpha, power, p = .5){
  
  ((qnorm (1 - (alpha/2)) + qnorm (power))^2) / ((log (HR)^2) * (p * (1 - p)))
  
}

## Required sample size, assuming event rate of 30%

EvSchoenfeld(HR = 1.68, alpha = 0.05, power = 0.8)/0.3

EvSchoenfeld(HR = 1.80, alpha = 0.05, power = 0.8)/0.3

EvSchoenfeld(HR = 2.15, alpha = 0.05, power = 0.8)/0.3








#### 3b. Figure of overall effect on survival [NOT IN MS] ####

## Estimate treatment effect [ALSFRS-R] & [Overall survival]

t <- 0:20 # time horizon

## Longitudinal effect per treatment arm (pl = placebo; tr = treated)

pl <- JM$coefficients$betas["(Intercept)"] + JM$coefficients$betas["AVISIT"]*t

tr <- JM$coefficients$betas["(Intercept)"] + (JM$coefficients$betas["AVISIT"] + JM$coefficients$betas["AVISIT:TRT"])*t

## Estimate confidence interval using delta method

E <- vcov(JM)[c("Y.(Intercept)", "Y.AVISIT", "Y.AVISIT:TRT"),
               c("Y.(Intercept)", "Y.AVISIT", "Y.AVISIT:TRT")]

var.tr <- sapply(t, function (ii){
  
  # Jacobian with first derivates
  
  J <- matrix(c(1,
                 JM$coefficients$betas["AVISIT:TRT"]*ii,
                 JM$coefficients$betas["AVISIT"]*ii), nrow = 1)
  
  # Delta method:
  
  J%*%E%*%t(J)
  
})




## Overall treatment effect on survival

hr <- JM$coefficients$gammas["TRT"] + JM$coefficients$alpha * JM$coefficients$betas["AVISIT:TRT"] * t

## Estimate confidence interval using delta method

E <- vcov(JM)[c("Y.AVISIT:TRT", "T.alpha", "T.TRT"),
               c("Y.AVISIT:TRT", "T.alpha", "T.TRT")]


var.hr <- sapply(t, function (ii){
  
  # Jacobian with first derivates
  J <- matrix(c(JM$coefficients$alpha*ii,
                  JM$coefficients$betas["AVISIT:TRT"]*ii,
                  1), nrow = 1)
  
  # Delta method:
  J%*%E%*%t(J)
  
})


## Confidence intervals:

hr.lb <- hr + qnorm(0.025) * sqrt(var.hr)
hr.ub <- hr + qnorm(0.975) * sqrt(var.hr)

tr.lb <- tr + qnorm(0.025) * sqrt(var.tr)
tr.ub <- tr + qnorm(0.975) * sqrt(var.tr)

## P-value at month 20 [Overall effect]:

zstat <- hr[t == 20]/sqrt(var.hr[t == 20])

2*(1 - pnorm(abs(zstat)))

## Figure:
col1 <- "#619CFF"
col2 <- "#F8766D"

# pdf(file = "FIG1.pdf", width = 8, height = 4.25)

par(mfrow = c(1,2), 
    mar = c(3,3,2,1), 
    mgp = c(1.9, 0.75, 0))

## Panel A
plot(t, tr, type = "l", ylim = c(0, 48),
      ylab = "ALSFRS-R total score", bty = "n", axes = F,
      xlab = "Time since randomization (months)",
      col = col1, lwd = 1.5)

axis(1, at = seq(-6, 24, 6)); axis(2, at = seq(-12, 48, 12))

polygon(c(t, rev(t)),
         c(tr.lb, rev(tr.ub)),
         border = NA, col = adjustcolor (col1, .15))

lines(t, pl, col = col2, lwd = 1.5)

legend("topleft", legend = c("Placebo", "Valproic Acid"),
        pch = 15, col = c(col2, col1), bty = "n")

mtext(expression(bold ("A.")~"Treatment effect ALSFRS-R (" * beta * ")"), adj = 0)

## Panel B
plot(t, exp(hr), type = "l", ylim = c(0.75, 5),
      bty = "n", axes = F, lwd = 1.5,
      xlab = "Time since randomization (months)",
      ylab = "Hazard ratio Placebo vs. Valproic Acid")

polygon(c(t, rev(t)),
         c(exp(hr.lb), rev(exp(hr.ub))),
         border = NA, col = adjustcolor(1, .1))
axis(1, at = seq(-6, 24, 6)); axis(2, at = seq(-1, 5, 1))

abline(h = 1, lty = 1)

segments(x0 = 0, x1 = 20,
          y0 = exp(coef(m)), y1 = exp(coef(m)),
          lty = 4, lwd = 1.5)

legend("topleft", legend = c("Joint model",
                                  "Cox model"),
        lty = c (1,4), lwd = 2, bty = "n")

mtext(expression (bold ("B.")~"Treatment effect survival (" * italic ("T")* ")"), adj = 0)

# dev.off ()

#### 4. Question 3 ####

## ANOVA-like test: does treatment effect either ALSFRS-R and/or Survival?

## Create model without treatment terms:

m0 <- coxph(Surv (STIME, STATUS) ~ 1, data = B, x = T)

lme0 <- lme(TOTAL ~ AVISIT, random = ~ AVISIT + I(AVISIT^2)|ID, data = A, method = "ML")

JM0 <- jointModel(lme0, m0, timeVar = "AVISIT", method = "weibull-PH-aGH")

## Compare empty model with full model using a likelihood ratio test:

anova(JM0, JM) # p = 0.15

