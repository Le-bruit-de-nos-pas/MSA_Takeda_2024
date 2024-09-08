library("survival")
library("survminer")
library(tidyverse)
library(data.table)

# PART 1 Kaplan-Meier curves  -------------------



AllMSA_Pop_Baseline_671 <- fread("Source/AllMSA_Pop_Baseline_671.txt")
EarlyCT_Pop_Baseline_319 <- fread("Source/EarlyCT_Pop_Baseline_319.txt")


# MORTALITY STATUS
dataCohorteManaged <- readRDS("Source/dataCohorteManaged.rds")
dataCohorteManaged <- dataCohorteManaged %>% select(NUM, TIME_STUDY, DATE_VISITE0, DATECONSULT, DATE_DERVISITE, DATEDC, DC)
dataCohorteManaged <- dataCohorteManaged %>% group_by(NUM) %>% filter(DATECONSULT==DATE_DERVISITE)

Mortalities <- dataCohorteManaged %>% filter(!is.na(DATEDC)) %>% select(-c(DATE_DERVISITE, DATECONSULT)) %>% mutate(DC=1) %>%
  bind_rows(
    dataCohorteManaged %>% filter(is.na(DATEDC)) %>% select(-c(DATECONSULT, DATEDC)) %>%
      rename("DATEDC"="DATE_DERVISITE") %>%
      anti_join(
        dataCohorteManaged %>% filter(!is.na(DATEDC)) %>% select(NUM) %>% distinct()
      )
  )

elapsed_months <- function(end_date, start_date) {
  ed <- as.POSIXlt(end_date)
  sd <- as.POSIXlt(start_date)
  12 * (ed$year - sd$year) + (ed$mon - sd$mon)
}

Mortalities <- Mortalities %>% mutate(elapsed=elapsed_months(DATEDC, DATE_VISITE0))
Mortalities <- Mortalities %>% select(NUM, DC, elapsed)
Mortalities$DC <- Mortalities$DC + 1
unique(Mortalities$DC)


# GASTRO
dataCohorteManaged <- readRDS("Source/dataCohorteManaged.rds")
dataCohorteManaged <- dataCohorteManaged %>% select(NUM, DATE_VISITE0, DATE_GASTRO, DATE_DERVISITE) %>% distinct()


Gastros <- dataCohorteManaged %>% filter(!is.na(DATE_GASTRO)) %>% select(-c(DATE_DERVISITE)) %>% mutate(GASTRO=1) %>%
  bind_rows(
    dataCohorteManaged %>% filter(is.na(DATE_GASTRO)) %>% select(-c(DATE_GASTRO)) %>%
      rename("DATE_GASTRO"="DATE_DERVISITE") %>%
      anti_join(
        dataCohorteManaged %>% filter(!is.na(DATE_GASTRO)) %>% select(NUM) %>% distinct() 
      ) %>% mutate(GASTRO=0)
  )


Gastros %>% group_by(GASTRO) %>% count()

elapsed_months <- function(end_date, start_date) {
  ed <- as.POSIXlt(end_date)
  sd <- as.POSIXlt(start_date)
  12 * (ed$year - sd$year) + (ed$mon - sd$mon)
}

Gastros <- Gastros %>% mutate(elapsed=elapsed_months(DATE_GASTRO, DATE_VISITE0))
Gastros <- Gastros %>% select(NUM, GASTRO, elapsed)
Gastros$GASTRO <- Gastros$GASTRO + 1
unique(Gastros$GASTRO)


# UMSARS 1 Item 1	Unintelligible speech
dataCohorteManaged <- readRDS("Source/dataCohorteManaged.rds")
UMSARS1_Item_1 <- dataCohorteManaged %>%  select(NUM, DATE_VISITE0, DATECONSULT, DATE_DERVISITE, UMSARS1_1)
Speech <- UMSARS1_Item_1 %>% filter(UMSARS1_1>=3) %>% select(NUM) %>% distinct() %>% mutate(Speech=1)

Speech <- Speech %>% left_join(UMSARS1_Item_1) %>% filter(UMSARS1_1>=3) %>% group_by(NUM) %>% filter(DATECONSULT==min(DATECONSULT)) %>%
  select(NUM, Speech, DATE_VISITE0, DATECONSULT) 

No <- UMSARS1_Item_1 %>% anti_join(Speech %>% select(NUM)) %>% 
  mutate(Speech=0) %>% filter(DATECONSULT==DATE_DERVISITE) %>%
  select(NUM, Speech, DATE_VISITE0, DATECONSULT)

Speech <- Speech %>% bind_rows(No)

Speech %>% group_by(Speech) %>% count()

elapsed_months <- function(end_date, start_date) {
  ed <- as.POSIXlt(end_date)
  sd <- as.POSIXlt(start_date)
  12 * (ed$year - sd$year) + (ed$mon - sd$mon)
}

Speech <- Speech %>% mutate(elapsed=elapsed_months(DATECONSULT, DATE_VISITE0))
Speech <- Speech %>% select(NUM, Speech, elapsed)
Speech$Speech <- Speech$Speech + 1
unique(Speech$Speech)



# UMSARS 1 Item 7	Inability to walk
dataCohorteManaged <- readRDS("Source/dataCohorteManaged.rds")
UMSARS1_Item_7 <- dataCohorteManaged %>%  select(NUM, DATE_VISITE0, DATECONSULT, DATE_DERVISITE, UMSARS1_7)
Walk <- UMSARS1_Item_7 %>% filter(UMSARS1_7>=3) %>% select(NUM) %>% distinct() %>% mutate(Walk=1)

Walk <- Walk %>% left_join(UMSARS1_Item_7) %>% filter(UMSARS1_7>=3) %>% group_by(NUM) %>% filter(DATECONSULT==min(DATECONSULT)) %>%
  select(NUM, Walk, DATE_VISITE0, DATECONSULT) 

No <- UMSARS1_Item_7 %>% anti_join(Walk %>% select(NUM)) %>% 
  mutate(Walk=0) %>% filter(DATECONSULT==DATE_DERVISITE) %>%
  select(NUM, Walk, DATE_VISITE0, DATECONSULT)

Walk <- Walk %>% bind_rows(No)

Walk %>% group_by(Walk) %>% count()

elapsed_months <- function(end_date, start_date) {
  ed <- as.POSIXlt(end_date)
  sd <- as.POSIXlt(start_date)
  12 * (ed$year - sd$year) + (ed$mon - sd$mon)
}

Walk <- Walk %>% mutate(elapsed=elapsed_months(DATECONSULT, DATE_VISITE0))
Walk <- Walk %>% select(NUM, Walk, elapsed)
Walk$Walk <- Walk$Walk + 1
unique(Walk$Walk)




# UMSARS 1 Item 8	Falls
dataCohorteManaged <- readRDS("Source/dataCohorteManaged.rds")
UMSARS1_Item_8 <- dataCohorteManaged %>%  select(NUM, DATE_VISITE0, DATECONSULT, DATE_DERVISITE, UMSARS1_8)
Falls <- UMSARS1_Item_8 %>% filter(UMSARS1_8>=4) %>% select(NUM) %>% distinct() %>% mutate(Falls=1)

Falls <- Falls %>% left_join(UMSARS1_Item_8) %>% filter(UMSARS1_8>=4) %>% group_by(NUM) %>% filter(DATECONSULT==min(DATECONSULT)) %>%
  select(NUM, Falls, DATE_VISITE0, DATECONSULT) 

No <- UMSARS1_Item_8 %>% anti_join(Falls %>% select(NUM)) %>% 
  mutate(Falls=0) %>% filter(DATECONSULT==DATE_DERVISITE) %>%
  select(NUM, Falls, DATE_VISITE0, DATECONSULT)

Falls <- Falls %>% bind_rows(No)

Falls %>% group_by(Falls) %>% count()

elapsed_months <- function(end_date, start_date) {
  ed <- as.POSIXlt(end_date)
  sd <- as.POSIXlt(start_date)
  12 * (ed$year - sd$year) + (ed$mon - sd$mon)
}

Falls <- Falls %>% mutate(elapsed=elapsed_months(DATECONSULT, DATE_VISITE0))
Falls <- Falls %>% select(NUM, Falls, elapsed)
Falls$Falls <- Falls$Falls + 1
unique(Falls$Falls)


Mortalities <- Mortalities %>% inner_join(AllMSA_Pop_Baseline_671 %>% select(NUM))
Gastros <- Gastros %>% inner_join(AllMSA_Pop_Baseline_671 %>% select(NUM))
Speech <- Speech %>% inner_join(AllMSA_Pop_Baseline_671 %>% select(NUM))
Walk <- Walk %>% inner_join(AllMSA_Pop_Baseline_671 %>% select(NUM))
Falls <- Falls %>% inner_join(AllMSA_Pop_Baseline_671 %>% select(NUM))

fit <- survfit(Surv(elapsed, DC) ~ 1, data = Mortalities)
print(fit)
# 800 vs 1200
ggsurvplot(fit, conf.int = TRUE,
           linetype = 1, 
           surv.median.line = "hv", 
           cumevents = TRUE,
           cumcensor = TRUE,
           break.time.by = 12,
           xlab = "\n Elapsed Time (months)",
           risk.table = TRUE,  
           risk.table.y.text.col = TRUE,
           ggtheme = theme_minimal(), 
           tables.theme = theme_minimal(),
           tables.height = 0.1 ,
           title = "Mortality: Overall MSA",
           palette = c("#00468B", "#D45769"))




fit <- survfit(Surv(elapsed, GASTRO) ~ 1, data = Gastros)
print(fit)
# 800 vs 1200
ggsurvplot(fit, conf.int = TRUE,
           linetype = 1, 
           surv.median.line = "hv", 
           cumevents = TRUE,
           cumcensor = TRUE,
           break.time.by = 12,
           xlab = "\n Elapsed Time (months)",
           risk.table = TRUE,  
           risk.table.y.text.col = TRUE,
           ggtheme = theme_minimal(), 
           tables.theme = theme_minimal(),
           tables.height = 0.1 ,
           title = "Gastrostomy: Overall MSA",
           palette = c("#00468B", "#D45769"))




fit <- survfit(Surv(elapsed, Speech) ~ 1, data = Speech)
print(fit)
# 800 vs 1200
ggsurvplot(fit, conf.int = TRUE,
           linetype = 1, 
           surv.median.line = "hv", 
           cumevents = TRUE,
           cumcensor = TRUE,
           break.time.by = 12,
           xlab = "\n Elapsed Time (months)",
           risk.table = TRUE,  
           risk.table.y.text.col = TRUE,
           ggtheme = theme_minimal(), 
           tables.theme = theme_minimal(),
           tables.height = 0.1 ,
           title = "Unintelligible speech: Overall MSA",
           palette = c("#00468B", "#D45769"))


fit <- survfit(Surv(elapsed, Walk) ~ 1, data = Walk)
print(fit)
# 800 vs 1200
ggsurvplot(fit, conf.int = TRUE,
           linetype = 1, 
           surv.median.line = "hv", 
           cumevents = TRUE,
           cumcensor = TRUE,
           break.time.by = 12,
           xlab = "\n Elapsed Time (months)",
           risk.table = TRUE,  
           risk.table.y.text.col = TRUE,
           ggtheme = theme_minimal(), 
           tables.theme = theme_minimal(),
           tables.height = 0.1 ,
           title = "Inability to walk: Overall MSA",
           palette = c("#00468B", "#D45769"))


fit <- survfit(Surv(elapsed, Falls) ~ 1, data = Falls)
summary(fit)
# 800 vs 1200
ggsurvplot(fit, conf.int = TRUE,
           linetype = 1, 
           surv.median.line = "hv", 
           cumevents = TRUE,
           cumcensor = TRUE,
           break.time.by = 12,
           xlab = "\n Elapsed Time (months)",
           risk.table = TRUE,  
           risk.table.y.text.col = TRUE,
           ggtheme = theme_minimal(), 
           tables.theme = theme_minimal(),
           tables.height = 0.1 ,
           title = "Falls: Overall MSA",
           palette = c("#00468B", "#D45769"))










Mortalities <- Mortalities %>% inner_join(EarlyCT_Pop_Baseline_319 %>% select(NUM))
Gastros <- Gastros %>% inner_join(EarlyCT_Pop_Baseline_319 %>% select(NUM))
Speech <- Speech %>% inner_join(EarlyCT_Pop_Baseline_319 %>% select(NUM))
Walk <- Walk %>% inner_join(EarlyCT_Pop_Baseline_319 %>% select(NUM))
Falls <- Falls %>% inner_join(EarlyCT_Pop_Baseline_319 %>% select(NUM))

fit <- survfit(Surv(elapsed, DC) ~ 1, data = Mortalities)
print(fit)
# 800 vs 1200
ggsurvplot(fit, conf.int = TRUE,
           linetype = 1, 
           surv.median.line = "hv", 
           cumevents = TRUE,
           cumcensor = TRUE,
           break.time.by = 12,
           xlab = "\n Elapsed Time (months)",
           risk.table = TRUE,  
           risk.table.y.text.col = TRUE,
           ggtheme = theme_minimal(), 
           tables.theme = theme_minimal(),
           tables.height = 0.1 ,
           title = "Mortality: Early CT MSA",
           palette = c( "#D45769"))




fit <- survfit(Surv(elapsed, GASTRO) ~ 1, data = Gastros)
print(fit)
# 800 vs 1200
ggsurvplot(fit, conf.int = TRUE,
           linetype = 1, 
           surv.median.line = "hv", 
           cumevents = TRUE,
           cumcensor = TRUE,
           break.time.by = 12,
           xlab = "\n Elapsed Time (months)",
           risk.table = TRUE,  
           risk.table.y.text.col = TRUE,
           ggtheme = theme_minimal(), 
           tables.theme = theme_minimal(),
           tables.height = 0.1 ,
           title = "Gastrostomy: Early CT MSA",
           palette = c( "#D45769"))




fit <- survfit(Surv(elapsed, Speech) ~ 1, data = Speech)
print(fit)
# 800 vs 1200
ggsurvplot(fit, conf.int = TRUE,
           linetype = 1, 
           surv.median.line = "hv", 
           cumevents = TRUE,
           cumcensor = TRUE,
           break.time.by = 12,
           xlab = "\n Elapsed Time (months)",
           risk.table = TRUE,  
           risk.table.y.text.col = TRUE,
           ggtheme = theme_minimal(), 
           tables.theme = theme_minimal(),
           tables.height = 0.1 ,
           title = "Unintelligible speech: Early CT MSA",
           palette = c( "#D45769"))


fit <- survfit(Surv(elapsed, Walk) ~ 1, data = Walk)
print(fit)
# 800 vs 1200
ggsurvplot(fit, conf.int = TRUE,
           linetype = 1, 
           surv.median.line = "hv", 
           cumevents = TRUE,
           cumcensor = TRUE,
           break.time.by = 12,
           xlab = "\n Elapsed Time (months)",
           risk.table = TRUE,  
           risk.table.y.text.col = TRUE,
           ggtheme = theme_minimal(), 
           tables.theme = theme_minimal(),
           tables.height = 0.1 ,
           title = "Inability to walk: Early CT MSA",
           palette = c( "#D45769"))


fit <- survfit(Surv(elapsed, Falls) ~ 1, data = Falls)
summary(fit)
# 800 vs 1200
ggsurvplot(fit, conf.int = TRUE,
           linetype = 1, 
           surv.median.line = "hv", 
           cumevents = TRUE,
           cumcensor = TRUE,
           break.time.by = 12,
           xlab = "\n Elapsed Time (months)",
           risk.table = TRUE,  
           risk.table.y.text.col = TRUE,
           ggtheme = theme_minimal(), 
           tables.theme = theme_minimal(),
           tables.height = 0.1 ,
           title = "Falls: Early CT MSA",
           palette = c("#D45769"))



# --------------------

# PART 2 Cox proportional model for the time to death -------------------


# BASELINE UMSARS 1
dataCohorteManaged <- readRDS("Source/dataCohorteManaged.rds")
dataCohorteManaged <- dataCohorteManaged %>% group_by(NUM) %>% mutate(TIME_STUDY = ifelse( is.na(TIME_STUDY), 0, TIME_STUDY)) %>%
  mutate(Year= ifelse(TIME_STUDY==0, 0,
                      ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                             ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                    ifelse(TIME_STUDY>=2.5 ,3, NA))))) 
UMSARS1 <- dataCohorteManaged %>% 
  select(NUM, DATECONSULT, TIME_STUDY, Year, UMSARS1_1:UMSARS1_TOT)
UMSARS1$UMSARS1_TOT_v2 <- rowSums(UMSARS1[, 5:16], na.rm = TRUE)
UMSARS1$missing_na <- rowSums(is.na(UMSARS1[, 5:16]))
UMSARS1[, 5:16] <- UMSARS1[, 5:16] -1
for (i in 5:16) {
  UMSARS1[, i][UMSARS1[, i] < 0] <- 0
}
UMSARS1$UMSARS1_TOT_v2 <- rowSums(UMSARS1[, 5:16], na.rm = TRUE)
UMSARS1 <-  UMSARS1 %>%  select(NUM,DATECONSULT,  TIME_STUDY, Year, UMSARS1_TOT_v2, UMSARS1_11)
UMSARS1_FDA <- UMSARS1 %>% mutate(UMSARS1_TOT_FDA=UMSARS1_TOT_v2-UMSARS1_11)
Baseline_UMSARS_1 <- UMSARS1_FDA %>% group_by(NUM) %>% filter(DATECONSULT==min(DATECONSULT)) %>%
  select(NUM, UMSARS1_TOT_v2 , UMSARS1_TOT_FDA )


# BASELINE UMSARS 2
dataCohorteManaged <- readRDS("Source/dataCohorteManaged.rds")
dataCohorteManaged <- dataCohorteManaged %>% group_by(NUM) %>% mutate(TIME_STUDY = ifelse( is.na(TIME_STUDY), 0, TIME_STUDY)) %>%
  mutate(Year= ifelse(TIME_STUDY==0, 0,
                      ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                             ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                    ifelse(TIME_STUDY>=2.5 ,3, NA))))) 
UMSARS2 <- dataCohorteManaged %>% 
  select(NUM, DATECONSULT, TIME_STUDY, Year, UMSARS2_1:UMSARS2_TOT)
UMSARS2$UMSARS2_TOT_v2 <- rowSums(UMSARS2[, 5:19], na.rm = TRUE)
UMSARS2$missing_na <- rowSums(is.na(UMSARS2[, 5:19]))
UMSARS2 <-  UMSARS2 %>%  select(NUM, DATECONSULT,  TIME_STUDY, Year, UMSARS2_TOT)
Baseline_UMSARS2 <- UMSARS2 %>% group_by(NUM) %>% filter(DATECONSULT==min(DATECONSULT)) %>%
  select(NUM, UMSARS2_TOT )



# BASELINE UMSARS 9-item 11-item
dataCohorteManaged <- readRDS("Source/dataCohorteManaged.rds")
dataCohorteManaged <- dataCohorteManaged %>% group_by(NUM) %>% mutate(TIME_STUDY = ifelse( is.na(TIME_STUDY), 0, TIME_STUDY)) %>%
  mutate(Year= ifelse(TIME_STUDY==0, 0,
                      ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                             ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                    ifelse(TIME_STUDY>=2.5 ,3, NA))))) 

UMSARS_9it_11it <- dataCohorteManaged %>% 
  mutate(UMSARS_9item = UMSARS1_1 + UMSARS1_4 + UMSARS1_5 + UMSARS1_6 + UMSARS1_7 + UMSARS1_10 + UMSARS2_11 + UMSARS2_12 + UMSARS2_14) %>%
  mutate(UMSARS_11item = UMSARS1_2 + UMSARS1_3 + UMSARS1_6 + UMSARS1_7 + UMSARS1_11 + UMSARS2_1 + UMSARS2_2 + UMSARS2_9 + UMSARS2_11 + UMSARS2_12 + UMSARS2_14 ) %>% 
  group_by(NUM) %>% filter(DATECONSULT==min(DATECONSULT)) %>% select(NUM, UMSARS_9item, UMSARS_11item)


# AGE GENDER SUBGROUPS
AllMSA_Pop_Baseline_671 <- fread("Source/AllMSA_Pop_Baseline_671.txt")
dataCohorteManaged <- readRDS("Source/dataCohorteManaged.rds")
dataCohorteManaged <- dataCohorteManaged %>% select(NUM, AGE_VISITE0, SEXE) %>% distinct() %>% drop_na()
Baselines <- dataCohorteManaged %>% inner_join(AllMSA_Pop_Baseline_671) %>% inner_join(UMSARS_9it_11it) %>%
  inner_join(Baseline_UMSARS_1) %>% inner_join(Baseline_UMSARS2)


# TIME SINCE 1st Symptoms
dataCohorteManaged <- readRDS("Source/dataCohorteManaged.rds")
dataCohorteManaged <- dataCohorteManaged %>% select(NUM, DELAI_sympt_vis0)
Delay_First_Symptoms <- dataCohorteManaged %>% group_by(NUM) %>% filter(DELAI_sympt_vis0==min(DELAI_sympt_vis0)) %>% distinct()
Baselines <- Baselines %>% inner_join(Delay_First_Symptoms)
Baselines <- Baselines %>% drop_na()

Baselines


# MORTALITY STATUS
dataCohorteManaged <- readRDS("Source/dataCohorteManaged.rds")
dataCohorteManaged <- dataCohorteManaged %>% select(NUM, TIME_STUDY, DATE_VISITE0, DATECONSULT, DATE_DERVISITE, DATEDC, DC)
dataCohorteManaged <- dataCohorteManaged %>% group_by(NUM) %>% filter(DATECONSULT==DATE_DERVISITE)

Mortalities <- dataCohorteManaged %>% filter(!is.na(DATEDC)) %>% select(-c(DATE_DERVISITE, DATECONSULT)) %>% mutate(DC=1) %>%
  bind_rows(
    dataCohorteManaged %>% filter(is.na(DATEDC)) %>% select(-c(DATECONSULT, DATEDC)) %>%
      rename("DATEDC"="DATE_DERVISITE") %>%
      anti_join(
        dataCohorteManaged %>% filter(!is.na(DATEDC)) %>% select(NUM) %>% distinct()
      )
  )

elapsed_months <- function(end_date, start_date) {
  ed <- as.POSIXlt(end_date)
  sd <- as.POSIXlt(start_date)
  12 * (ed$year - sd$year) + (ed$mon - sd$mon)
}


Mortalities <- Mortalities %>% mutate(elapsed=elapsed_months(DATEDC, DATE_VISITE0))
Mortalities <- Mortalities %>% select(NUM, DC, elapsed)

Mortalities <- Mortalities %>% inner_join(Baselines)

Mortalities <- Mortalities %>% 
  mutate(DIAG=ifelse(DIAG=="CB", 1, 2)) %>%
  mutate(DIAGNIV=ifelse(DIAGNIV=="POS", 1, 2)) 


covariates <- c("AGE_VISITE0", "SEXE",  "DIAG", "DIAGNIV", "DELAI_sympt_vis0", "UMSARS1_TOT_v2")

univ_formulas <- sapply(covariates,
                        function(x) as.formula(paste('Surv(elapsed, DC)~', x)))

univ_models <- lapply( univ_formulas, function(x){coxph(x, data = Mortalities)})

univ_results <- lapply(univ_models,
                       function(x){ 
                         x <- summary(x)
                         p.value<-signif(x$wald["pvalue"], digits=2)
                         wald.test<-signif(x$wald["test"], digits=2)
                         beta<-signif(x$coef[1], digits=2);#coeficient beta
                         HR <-signif(x$coef[2], digits=2);#exp(beta)
                         HR.confint.lower <- signif(x$conf.int[,"lower .95"], 2)
                         HR.confint.upper <- signif(x$conf.int[,"upper .95"],2)
                         HR <- paste0(HR, " (", 
                                      HR.confint.lower, "-", HR.confint.upper, ")")
                         res<-c(beta, HR, wald.test, p.value)
                         names(res)<-c("beta", "HR (95% CI for HR)", "wald.test", 
                                       "p.value")
                         return(res)
                         #return(exp(cbind(coef(x),confint(x))))
                       })
res <- t(as.data.frame(univ_results, check.names = FALSE))

as.data.frame(res)

AllMSA_Pop_Baseline_671 <- fread("Source/AllMSA_Pop_Baseline_671.txt")
EarlyCT_Pop_Baseline_319 <- fread("Source/EarlyCT_Pop_Baseline_319.txt")


Mortalities$DC <- Mortalities$DC + 1

Mortalities_all <- Mortalities %>% inner_join(AllMSA_Pop_Baseline_671 %>% select(NUM))
Mortalities_early <- Mortalities %>% inner_join(EarlyCT_Pop_Baseline_319 %>% select(NUM))



res.cox <- coxph(Surv(elapsed, DC) ~ AGE_VISITE0 + SEXE + DIAG + DIAGNIV + DELAI_sympt_vis0 + UMSARS1_TOT_v2, data =  Mortalities)
res.cox <- coxph(Surv(elapsed, DC) ~ AGE_VISITE0 + SEXE + DIAG + DIAGNIV + DELAI_sympt_vis0 + UMSARS1_TOT_FDA , data =  Mortalities)
res.cox <- coxph(Surv(elapsed, DC) ~ AGE_VISITE0 + SEXE + DIAG + DIAGNIV + DELAI_sympt_vis0 + UMSARS2_TOT  , data =  Mortalities)
res.cox <- coxph(Surv(elapsed, DC) ~ AGE_VISITE0 + SEXE + DIAG + DIAGNIV + DELAI_sympt_vis0 + UMSARS_11item   , data =  Mortalities)
res.cox <- coxph(Surv(elapsed, DC) ~ AGE_VISITE0 + SEXE + DIAG + DIAGNIV + DELAI_sympt_vis0 + UMSARS_9item   , data =  Mortalities)

summary(res.cox)



res.cox <- coxph(Surv(elapsed, DC) ~ AGE_VISITE0 + SEXE + DIAG + DIAGNIV + DELAI_sympt_vis0 + UMSARS1_TOT_v2, data =  Mortalities_early)
res.cox <- coxph(Surv(elapsed, DC) ~ AGE_VISITE0 + SEXE + DIAG + DIAGNIV + DELAI_sympt_vis0 + UMSARS1_TOT_FDA , data =  Mortalities_early)
res.cox <- coxph(Surv(elapsed, DC) ~ AGE_VISITE0 + SEXE + DIAG + DIAGNIV + DELAI_sympt_vis0 + UMSARS2_TOT  , data =  Mortalities_early)
res.cox <- coxph(Surv(elapsed, DC) ~ AGE_VISITE0 + SEXE + DIAG + DIAGNIV + DELAI_sympt_vis0 + UMSARS_11item   , data =  Mortalities_early)
res.cox <- coxph(Surv(elapsed, DC) ~ AGE_VISITE0 + SEXE + DIAG + DIAGNIV + DELAI_sympt_vis0 + UMSARS_9item   , data =  Mortalities_early)

summary(res.cox)


# -------------