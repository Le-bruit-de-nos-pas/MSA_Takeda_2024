library(tidyr)
library(dplyr)
library(ggplot2)
theme_set(theme_bw())
library(survival)
library(mgcv)
library(pammtools)
Set1 <- RColorBrewer::brewer.pal(9, "Set1")


# raw data
# https://socserv.mcmaster.ca/jfox/Books/Companion/scripts/appendix-cox.R

recidivism <- read.table( file   = "https://math.unm.edu/~james/Rossi.txt", header = TRUE) %>%
  mutate(subject=row_number())


# transform into long format
recidivism_long <- recidivism %>%
  gather(calendar.week, employed, emp1:emp52) %>%
  filter(!is.na(employed)) %>% # employed unequal to NA only for intervals under risk
  group_by(subject) %>%
  mutate(
    start  = row_number()-1,
    stop   = row_number(),
    arrest = ifelse(stop == last(stop) & arrest == 1, 1, 0),
    offset = log(stop - start)) %>%
  select(subject, start, stop, offset, arrest, employed, fin:educ) %>%
  arrange(subject, stop)

recidivism_long <- recidivism_long %>%
  mutate(employed.lag1 = lag(employed, default=0)) %>%
  slice(-1) %>% # exclusion of first week, as lagged information is missing
  ungroup()


## Fit PAM (smooth effects of age and prio, using P-Splines)
pam <- gam(arrest ~ s(stop) + fin + s(age, bs="ps") + race + wexp + mar + paro +
             s(prio, bs="ps") + employed.lag1,
           data=recidivism_long, family=poisson(), offset=offset)

tidy_fixed(pam)


# respective extended cox model
cph <- coxph(
  formula = Surv(start, stop, arrest)~ fin + pspline(age) + race + wexp + mar +
    paro + pspline(prio) + employed.lag1,
  data=recidivism_long)

# extract information on fixed coefficients

tidy_fixed(cph)[c(1, 4:7, 10), ]





all_eff <- purrr::map_df(
  list(
    tidy_fixed(pam),
    tidy_fixed(cph)[-c(2:3, 8:9), ]),
  bind_rows, .id="Method") %>%
  mutate(Method = factor(Method, levels=2:1, labels=c("Cox-PH", "PAM")))

## plot of fixed coefficients
coef_gg <- ggplot(all_eff, aes(x=variable, y=coef, ymin=ci_lower, ymax=ci_upper)) +
  geom_hline(yintercept = 0, lty=3) +
  geom_pointrange(aes(col=Method, shape=Method),
                  position=position_dodge(width=0.5)) +
  scale_colour_manual(
    values = c("black", Set1[1]),
    limits = rev(levels(all_eff$Method))) +
  scale_shape_manual(
    values = c(19, 15),
    limits = rev(levels(all_eff$Method))) +
  coord_flip(ylim=range(-1.5, 1)) +
  ylab(expression(hat(beta)%+-% 1.96 %.% SE)) +
  xlab("")

## to visualize smooth effect of age, create data set where all covariates are
## fixed to mean values except for age, which varies between min and max
## (n = 100)
age_df <- recidivism_long %>% make_newdata(age = seq_range(age, n=100))

## add information on contribution of age to linear predictor (partial effect of age)
age_df <- age_df %>%
  add_term(pam, term="age") %>%
  mutate(cphfit = predict(object=cph, ., type="terms")[,"pspline(age)"])

## prep plot object for smooth effects
smooth_gg <- ggplot(age_df, aes(y=fit)) +
  geom_line(aes(col="PAM")) +
  geom_ribbon(aes(ymin=ci_lower, ymax=ci_upper), alpha=0.3) +
  geom_line(aes(y=cphfit, col="Cox-PH")) +
  scale_colour_manual(name="Method", values=c("#E41A1C", "#000000")) +
  ylab(expression(hat(f)(x))) + theme(legend.position="none")

## plot of the age effect
age_gg <- smooth_gg + aes(x=age) + xlab("Age")


## same as "age"" for "prio" variable
prio_df <- recidivism_long %>% make_newdata(prio = seq_range(prio, n = 100))
prio_df <- prio_df %>%
  add_term(pam, term="prio") %>%
  mutate(cphfit = predict(object=cph, ., type="terms")[,7])

## plot of the prio effect
prio_gg <- smooth_gg %+% prio_df + aes(x=prio) +
  xlab("Number of prior convictions")

## put all plots together
gridExtra::grid.arrange(
  coef_gg +theme(legend.position="bottom"),
  age_gg,
  prio_gg,
  layout_matrix=matrix(c(1, 1, 2, 3), ncol=2))




data("pbc", package = "survival")
head(pbc)[, c(1:5, 11, 12)]

head(pbcseq)[, c(1, 4:5, 7, 12, 13)]

pbc <- pbc %>% mutate(bili = log(bili), protime = log(protime))

pbcseq <- pbcseq %>% mutate(bili = log(bili), protime = log(protime))


# below code copied from survival vignette "timedep"
temp <- subset(pbc, id <= 312, select = c(id:sex)) # baseline
head(temp)
pbc2 <- tmerge(temp, temp, id = id, death = event(time, status)) #set range

pbc2 <- tmerge(pbc2, pbcseq, id = id, bili = tdc(day, bili),
               protime = tdc(day, protime))

sum(is.na(pbc2))

fit1 <- coxph(Surv(time, status == 2) ~ bili + protime, pbc)
fit2 <- coxph(Surv(tstart, tstop, death == 2) ~ bili + protime, pbc2)
rbind("baseline fit" = coef(fit1), "time dependent" = coef(fit2))



pbc <- pbc %>% filter(id <= 312) %>%
  select(id:sex, bili, protime) %>%
  mutate(status = 1L * (status == 2))

pbc_ped <- as_ped(
  data = list(pbc, pbcseq),
  formula = Surv(time, status) ~ . + concurrent(bili, protime, tz_var = "day"),
  id = "id")

pbc_pam <- gam(ped_status ~ s(tend) + bili + protime, data = pbc_ped,
               family = poisson(), offset = offset)
cbind(pam = coef(pbc_pam)[2:3], cox = coef(fit2))


## Effect of bilirubin
# note that we use the reference argument to calculate
# the relative risk change (x - \bar{x})'\beta for comparison with predict.coxph
# (see also Details section in ?predict.coxph)
reference = sample_info(pbc_ped)

bili_df <- pbc_ped %>% ungroup() %>%
  make_newdata(bili = seq_range(bili, n = 100)) %>%
  add_term(pbc_pam, term = "bili", reference = reference) %>%
  mutate(cox = predict(fit2, ., type = "term")[, "bili"])

## Effect of protime
protime_df <- pbc_ped %>% ungroup() %>%
  make_newdata(protime = seq_range(protime, n=100)) %>%
  add_term(pbc_pam, term = "protime", reference = reference) %>%
  mutate(cox = predict(fit2, ., type = "term")[, "protime"])

# visualization
# remember that bili and protime are log transformed
p_term <- ggplot(data = NULL, aes(y = fit)) + geom_line(aes(col = "PAM")) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2) +
  geom_line(aes(y = cox, col = "Cox")) +
  scale_colour_manual(name = "Method", values = c("#E41A1C", "#000000"))
gridExtra::grid.arrange(
  p_term %+% bili_df + aes(x = exp(bili)),
  p_term %+% protime_df + aes(x = exp(protime)),
  nrow = 1L)