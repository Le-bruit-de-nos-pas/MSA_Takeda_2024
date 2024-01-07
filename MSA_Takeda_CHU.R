
library(tidyverse)
library(data.table)
library(lubridate)
library(nlme)
library(JM)
library(survival)

options(scipen = 999)

# Theme set -------------------------------------
# rstudioapi::addTheme("https://raw.githubusercontent.com/batpigandme/night-owlish/master/rstheme/night-owlish.rstheme", apply = TRUE)
# 
# install.packages(
#   "rsthemes",
#   repos = c(gadenbuie = 'https://gadenbuie.r-universe.dev', getOption("repos"))
# )
# 
# rsthemes::install_rsthemes()
# 
# rsthemes::list_rsthemes()
# 
# rstudioapi::applyTheme("Elm dark {rsthemes}")

# ------------------------


# Import data ------------------------------------------------

dataCohorteManaged <- readRDS("Source/dataCohorteManaged.rds")

names(dataCohorteManaged)

length(unique(dataCohorteManaged$NUM)) # 732 patients

"#F8B195"   "#F67280"   "#C06C84"   "#6C5B7B"   "#355C7D" 

setDT(dataCohorteManaged)

sum(is.na(dataCohorteManaged) ) / 
  ( dim(dataCohorteManaged)[1] * dim(dataCohorteManaged)[2] ) # 0.3324094 data missing



# -------------------------------------------------------------



# Total number of Visits per patient --------------------------------------------


dataCohorteManaged[, .(n_visits = .N), by = NUM][
  , .(mean = mean(n_visits),
      sd = sd(n_visits),
      median = median(n_visits),
      Q1 = quantile(n_visits, probs = 0.25),
      Q3 = quantile(n_visits, probs = 0.75))
  ]


#        mean       sd median Q1 Q3
# 1: 3.184426 2.349172      3  1  4


graph <- dataCohorteManaged[, .(n = .N), by = NUM]

setnames(graph, "n", "n_visits")

graph_sum <- graph[, .(n = .N), by = n_visits]

graph_sum %>%
  ggplot(aes(n_visits, n)) +
  geom_col(fill="#355C7D", alpha=0.8) + 
  theme_minimal() + 
  geom_text(aes(label = n), vjust = -0.5) +
  xlab("\n Number of distinct visits/evaluations") + 
  ylab("Number of patients \n")



# -----------------------------------------------------------



# Mortality overall  --------------------------------------------------

dim(dataCohorteManaged[DC == 1, .(NUM)][, .(NUM = unique(NUM))])[1] # 459 out of 732

dim(dataCohorteManaged[DC == 1 & CAUSEDC_1 != "", .(NUM, DC, CAUSEDC_1, CAUSEDC_2, CAUSEDC_3)])[1] # 242 with known cause

dataCohorteManaged[DC == 1 & CAUSEDC_1 != "", .(count = .N), by = CAUSEDC_1][order(-count)]

#                                         CAUSEDC_1 count
# 1:                                 1 Pneumopathie   109
# 2:                                       5 Autres    72
# 3:                                  2 Mort subite    24
# 4: 3 Détresse respiratoire aigüe sur fausse route    21
# 5:                       4 Syndrome de glissement    16


# ------------------------------------------------------------



# Visibility  ------------------------------------------------------

temp <- dataCohorteManaged[, .(NUM, ANDEBSYMPT, dateVisite0, annee_vis0, DATECONSULT, delai_consult_vis0, 
                                DELAIDebutSymptomes, DATE0, DELAI_sympt_vis0, ANDIAG, DIAG, DIAGNIV, UMSARS1_TOT, UMSARS1and2_TOT, UMSARS2_TOT)]


temp  %>% group_by(NUM) %>% summarise(First=min(as.Date(DATECONSULT)), 
                                      Last=max(as.Date(DATECONSULT)))  %>%
  mutate(elapsed=as.numeric(Last-First)/30.5) %>% ungroup() %>% 
  summarise(mean = mean(elapsed),
      sd = sd(elapsed),
      median = median(elapsed),
      Q1 = quantile(elapsed, probs = 0.25),
      Q3 = quantile(elapsed, probs = 0.75))

#    mean    sd median    Q1    Q3
# 1  23.3  26.1   15.2     0  36.3


temp  %>% group_by(NUM) %>% summarise(First=min(as.Date(DATECONSULT)), 
                                      Last=max(as.Date(DATECONSULT)))  %>%
  mutate(elapsed=as.numeric(Last-First)) %>% 
  ggplot(aes(round(elapsed/30.5))) +
  geom_histogram(fill="#355C7D", colour="#355C7D", alpha=0.8, bins=99) +
  theme_minimal() + 
  xlab("\n Total visibility \n Number of elapsed months \n (First to last visit/evaluation)") + 
  ylab("Patient density \n (kernel smoothing) \n")

temp %>% select(NUM) %>% distinct() %>% mutate(rownum=row_number()) %>%
  left_join(temp %>% select(NUM, delai_consult_vis0)) %>%
  ggplot(aes(delai_consult_vis0*12, as.factor(rownum))) +
  geom_jitter(size=1, alpha=0.5) +
  theme(axis.text.y = element_text(size = 0.2))  +
  geom_vline(xintercept = c(6,12,18,24),  color = "firebrick", size=1.5, alpha=0.5) +
  xlab("\n Number of elapsed months \n between subsequent evaluations") + ylab("Patient ID \n") 
 


graph %>% inner_join(
  temp  %>% group_by(NUM) %>% summarise(First=min(as.Date(DATECONSULT)), 
                                        Last=max(as.Date(DATECONSULT)))  %>%
    mutate(elapsed=as.numeric(Last-First)/30.5) %>% ungroup() %>%
    select(NUM, elapsed)
) %>%
  ggplot(aes(n_visits, elapsed)) +
  geom_density2d_filled() +
  theme_minimal() 





# ----------------------------------------------------------------
# Populations ---------------------------------

dataCohorteManaged <- readRDS("Source/dataCohorteManaged.rds")

names(dataCohorteManaged)

length(unique(dataCohorteManaged$NUM)) # 732 patients

setDT(dataCohorteManaged)

dataCohorteManaged <- dataCohorteManaged[, .SD[which.min(DATECONSULT)], by = NUM]

# Diagnostic 1=AMS Parkinson 2=AMS Cérébelleux
data.table(dataCohorteManaged)[, .(count = .N), by = .(DIAG)]

# DIAG count
# 1:    1  498 (68%)
# 2:    2   234 (32%)

# Certitude du diagnostic 1=Possible 2=Probable 3=Certaine
data.table(dataCohorteManaged)[, .(count = .N), by = .(DIAGNIV)]

# DIAGNIV count
# 1:       2   560 (77%)
# 2:       1   171 (23%)
# 3:      NA     1

data.table(dataCohorteManaged)[, .(count = .N), by = .(DIAG, DIAGNIV)]

# DIAG DIAGNIV count
# 1:    1       2   380
# 2:    2       2   180
# 3:    1       1   117
# 4:    2       1    54
# 5:    1      NA     1

# 1 patient has no DIAGNIV specified, defined as Possible for now
dataCohorteManaged[, DIAGNIV := ifelse(is.na(DIAGNIV), 1, DIAGNIV)]

data.table(dataCohorteManaged)[, .(count = .N), by = .(DIAG, DIAGNIV)]

# DIAG DIAGNIV count
# 1:    1       2   380 (52%)
# 2:    2       2   180 (25%)
# 3:    1       1   118 (16%)
# 4:    2       1    54 (7%)


names(dataCohorteManaged)
# Possible or probable MSA (per Gilman et al, 2008 criteria).
# Unified Multiple System Atrophy Rating Scale (UMSARS) Part I ≤21 (excluding sexual function item) ,
#   with a score ≤2 on the items #2 (swallowing), #7 (walking), and #8 (falling).
# UMSARS Part IV disability score ≤3.

UMSARS1_1:UMSARS1_TOT
dataCohorteManaged %>% filter(!is.na(UMSARS1_TOT) & !is.na(UMSARS2_TOT) &  !is.na(UMSARS4)  ) %>% select(NUM) %>% distinct()


data.table(dataCohorteManaged)[, .(NUM, UMSARS1_TOT)][!is.na(UMSARS1_TOT)] # 29 without

data.table(dataCohorteManaged)[, .(NUM, UMSARS2_TOT)][!is.na(UMSARS1_TOT)] # 26 without

data.table(dataCohorteManaged)[, .(NUM, UMSARS1_TOT)][!is.na(UMSARS1_TOT), .(NUM)][
  data.table(dataCohorteManaged)[, .(NUM, UMSARS2_TOT)][!is.na(UMSARS2_TOT), .(NUM)],
  on = "NUM", nomatch = 0]


# 40 patients have no data to check whether they meet the criterion (!)
dataCohorteManaged %>% filter(is.na(UMSARS4) | is.na(UMSARS1_TOT) | is.na(UMSARS1_11) | is.na(UMSARS1_2) | is.na(UMSARS1_7) |is.na(UMSARS1_8))


ClinicalTrialPats <- dataCohorteManaged[UMSARS4 <= 3 & (UMSARS1_TOT - UMSARS1_11) <= 21 & 
                                          UMSARS1_2 <= 2 & UMSARS1_7 <= 2 & UMSARS1_8 <= 2,   .(NUM)]


fwrite(ClinicalTrialPats, "Source/ClinicalTrialPats.txt")

# Target population

data.table(ClinicalTrialPats)[dataCohorteManaged, on = "NUM", nomatch = 0][, .(count = .N), by = .(DIAG)]

# DIAG count
# 1:    1   231
# 2:    2   107

data.table(ClinicalTrialPats)[dataCohorteManaged, on = "NUM", nomatch = 0][, .(count = .N), by = .(DIAGNIV)]

# DIAGNIV count
# 1:       2   228
# 2:       1   110

data.table(ClinicalTrialPats)[dataCohorteManaged, on = "NUM", nomatch = 0][, .(count = .N), by = .(DIAG, DIAGNIV)]

# DIAG DIAGNIV count
# 1:    1       2   154
# 2:    2       2    74
# 3:    1       1    77
# 4:    2       1    33

# ---------------------------------------
# Summary evolution UMSARS all datapoints ---------------------------
dataCohorteManaged <- readRDS("Source/dataCohorteManaged.rds")
setDT(dataCohorteManaged)
dataCohorteManaged[, DIAGNIV := ifelse(is.na(DIAGNIV), 1, DIAGNIV)]
names(dataCohorteManaged)

ClinicalTrialPats <- fread("Source/ClinicalTrialPats.txt")
ClinicalTrialPats <- data.table(ClinicalTrialPats)[dataCohorteManaged, on = "NUM", nomatch = 0]

max(dataCohorteManaged$UMSARS1_TOT, na.rm=T) # 46

# GAM with a cubic spline (with “shrinkage”)  and 3 knots
dataCohorteManaged %>% select(NUM, TIME_STUDY, TIME_SYMPT , UMSARS1_TOT, UMSARS2_TOT, UMSARS4) %>%
  filter(TIME_SYMPT<=15) %>%
  ggplot(aes(TIME_SYMPT, UMSARS1_TOT)) +
  geom_jitter(size=0.1, fill=0.53) +
  geom_smooth(method="gam", colour="midnightblue", fill="deepskyblue4", formula = y ~ s(x, bs = "cs", k =6)) +
  theme_minimal() +
  xlab("\n Elapsed number of years since symptom onset \n [All available patient records]") +
  ylab("Total UMSARS 1 Score \n At each evaluation \n")

max(dataCohorteManaged$UMSARS2_TOT, na.rm=T) # 52

# GAM with a cubic spline (with “shrinkage”)  and 3 knots
dataCohorteManaged %>% select(NUM, TIME_STUDY, TIME_SYMPT , UMSARS1_TOT, UMSARS2_TOT, UMSARS4) %>%
  filter(TIME_SYMPT<=15) %>%
  ggplot(aes(TIME_SYMPT, UMSARS2_TOT)) +
  geom_jitter(size=0.1, fill=0.53) +
  geom_smooth(method="gam", colour="midnightblue", fill="deepskyblue4", formula = y ~ s(x, bs = "cs", k =6)) +
  theme_minimal() +
  xlab("\n Elapsed number of years since symptom onset \n [All available patient records]") +
  ylab("Total UMSARS 2 Score \n At each evaluation \n")

# GAM with a cubic spline (with “shrinkage”)  and 6 knots
dataCohorteManaged %>% select(NUM, TIME_STUDY, TIME_SYMPT , UMSARS1_TOT, UMSARS2_TOT, UMSARS4) %>%
  filter(TIME_SYMPT<=15) %>%
  ggplot(aes(TIME_SYMPT, UMSARS4)) +
  geom_jitter(size=0.1, fill=0.53, height=0.1) +
  geom_smooth(method="gam", colour="midnightblue", fill="deepskyblue4", formula = y ~ s(x, bs = "cs", k =6)) +
  theme_minimal() +
  xlab("\n Elapsed number of years since symptom onset \n [All available patient records]") +
  ylab("Total UMSARS 4 Score \n At each evaluation \n")


# GAM with a cubic spline (with “shrinkage”)  and 6 knots
dataCohorteManaged %>% select(NUM, TIME_STUDY, TIME_SYMPT , PAD_COU, PAS_COU, deltaPAD, deltaPAS) %>%
  filter(TIME_SYMPT<=15) %>%
  gather(Type, Pressure, PAD_COU:PAS_COU) %>% 
  mutate(Type=ifelse(Type=="PAD_COU", "Diastolic", "Systolic")) %>%
  ggplot(aes(TIME_SYMPT, Pressure, colour=Type, fill=Type)) +
  geom_jitter(size=0.1, fill=0.53, height=0.1) +
 # geom_smooth(method="gam", formula = y ~ s(x, bs = "cs", k =6)) +
  geom_smooth(method="loess") +
  theme_minimal() +
  xlab("\n Elapsed number of years since symptom onset \n [All available patient records]") +
  ylab("Supine Diastolic / Systolic \n Blood Pressure (mmHg) \n At each evaluation \n") +
  scale_colour_manual(values=c("#254556" , "#c40234" )) +
  scale_fill_manual(values=c("#254556" , "#c40234" )) 



# GAM with a cubic spline (with “shrinkage”)  and 6 knots
dataCohorteManaged %>% select(NUM, TIME_STUDY, TIME_SYMPT , PAD_COU, PAS_COU, deltaPAD, deltaPAS) %>%
  filter(TIME_SYMPT<=15) %>%
  gather(Type, Pressure, deltaPAD:deltaPAS) %>% 
  mutate(Type=ifelse(Type=="deltaPAS", "Systolic Drop", "Diastolic Drop")) %>%
  ggplot(aes(TIME_SYMPT, Pressure, colour=Type, fill=Type)) +
  geom_jitter(size=0.1, fill=0.53, height=0.1) +
  # geom_smooth(method="gam", formula = y ~ s(x, bs = "cs", k =6)) +
  geom_smooth(method="loess") +
  theme_minimal() +
  ylim(-100,25) +
  xlab("\n Elapsed number of years since symptom onset \n [All available patient records]") +
  ylab("MAX supine-to-standing Diastolic / Systolic \n Blood Pressure Drop (mmHg) \n At each evaluation \n") +
  scale_colour_manual(values=c("#254556" , "#c40234" )) +
  scale_fill_manual(values=c("#254556" , "#c40234" )) 





dataCohorteManaged %>% select(NUM, TIME_STUDY, TIME_SYMPT , UMSARS1_TOT, UMSARS2_TOT, UMSARS4) %>%
  mutate(TIME_SYMPT=round(TIME_SYMPT)) %>%
  group_by(TIME_SYMPT, UMSARS4) %>% count() %>%
  drop_na() %>%
  ungroup() %>% group_by(TIME_SYMPT) %>% mutate(TOTAL=sum(n)) %>%
  mutate(n=n/TOTAL) %>% select(-TOTAL) %>%
  filter(TIME_SYMPT<=20) %>%
  ggplot(aes(TIME_SYMPT, 100*n, colour=as.factor(UMSARS4)), fill=as.factor(UMSARS4)) +
  geom_path(linewidth=2) +
  theme_minimal() +
  xlab("\n Elapsed number of years since symptom onset \n [All available patient records]") +
  ylab("% of patients ON each UMSARS 4 Score \n At each evaluation \n") +
  scale_colour_manual(values=c("#e3dac9", "#f8cd48" , "#ff4f00" , "#254556" , "#c40234" ))





# ----------------------------------------------

# Summary descriptive stats ------------------------

dataCohorteManaged <- readRDS("Source/dataCohorteManaged.rds")

setDT(dataCohorteManaged)

dataCohorteManaged <- dataCohorteManaged[, .SD[which.min(DATECONSULT)], by = NUM]

dataCohorteManaged[, DIAGNIV := ifelse(is.na(DIAGNIV), 1, DIAGNIV)]

ClinicalTrialPats <- fread("Source/ClinicalTrialPats.txt")
ClinicalTrialPats <- data.table(ClinicalTrialPats)[dataCohorteManaged, on = "NUM", nomatch = 0]

# -----------------------
# Age ----------------------------

dataCohorteManaged <- dataCohorteManaged %>% mutate(AGE_VISITE0=ifelse(is.na(AGE_VISITE0), 
                                                                       time_length(difftime(DATECONSULT, DATENAIS), "years") , AGE_VISITE0 ))

sum(is.na(dataCohorteManaged$AGE_VISITE0)) == FALSE
                                                    

ClinicalTrialPats <- ClinicalTrialPats %>% mutate(AGE_VISITE0=ifelse(is.na(AGE_VISITE0), 
                                                                       time_length(difftime(DATECONSULT, DATENAIS), "years") , AGE_VISITE0 ))

sum(is.na(ClinicalTrialPats$AGE_VISITE0)) == FALSE


# Overall
mean(dataCohorteManaged$AGE_VISITE0, na.rm=T) ; sd(dataCohorteManaged$AGE_VISITE0, na.rm=T) # 65.04 , 8.14
median(dataCohorteManaged$AGE_VISITE0, na.rm=T) ; quantile(dataCohorteManaged$AGE_VISITE0, na.rm=T) # 63, 59-71
sum(!is.na(dataCohorteManaged$AGE_VISITE0)) ; sum(!is.na(dataCohorteManaged$AGE_VISITE0))/732 # 732, 98
#P
mean(dataCohorteManaged$AGE_VISITE0[dataCohorteManaged$DIAG==1], na.rm=T) ; sd(dataCohorteManaged$AGE_VISITE0[dataCohorteManaged$DIAG==1], na.rm=T) # 65.56 , 8.24
median(dataCohorteManaged$AGE_VISITE0[dataCohorteManaged$DIAG==1], na.rm=T) ; quantile(dataCohorteManaged$AGE_VISITE0[dataCohorteManaged$DIAG==1], na.rm=T) # 66, 60-71
#C
mean(dataCohorteManaged$AGE_VISITE0[dataCohorteManaged$DIAG==2], na.rm=T) ; sd(dataCohorteManaged$AGE_VISITE0[dataCohorteManaged$DIAG==2], na.rm=T) # 63.92 , 7.82
median(dataCohorteManaged$AGE_VISITE0[dataCohorteManaged$DIAG==2], na.rm=T) ; quantile(dataCohorteManaged$AGE_VISITE0[dataCohorteManaged$DIAG==2], na.rm=T) # 63, 58-70

wilcox.test(
  na.omit(dataCohorteManaged$AGE_VISITE0[dataCohorteManaged$DIAG==1]), 
  na.omit(dataCohorteManaged$AGE_VISITE0[dataCohorteManaged$DIAG==2])
  )


# Overall CT
mean(ClinicalTrialPats$AGE_VISITE0, na.rm=T) ; sd(ClinicalTrialPats$AGE_VISITE0, na.rm=T) # 63.90 , 7.77
median(ClinicalTrialPats$AGE_VISITE0, na.rm=T) ; quantile(ClinicalTrialPats$AGE_VISITE0, na.rm=T) # 64, 59-70
sum(!is.na(ClinicalTrialPats$AGE_VISITE0)) ; sum(!is.na(ClinicalTrialPats$AGE_VISITE0))/338 # 100
#P
mean(ClinicalTrialPats$AGE_VISITE0[ClinicalTrialPats$DIAG==1], na.rm=T) ; sd(ClinicalTrialPats$AGE_VISITE0[ClinicalTrialPats$DIAG==1], na.rm=T) # 64.39 , 7.98
median(ClinicalTrialPats$AGE_VISITE0[ClinicalTrialPats$DIAG==1], na.rm=T) ; quantile(ClinicalTrialPats$AGE_VISITE0[ClinicalTrialPats$DIAG==1], na.rm=T) # 65, 59-70
#C
mean(ClinicalTrialPats$AGE_VISITE0[ClinicalTrialPats$DIAG==2], na.rm=T) ; sd(ClinicalTrialPats$AGE_VISITE0[ClinicalTrialPats$DIAG==2], na.rm=T) # 62.86 , 7.24
median(ClinicalTrialPats$AGE_VISITE0[ClinicalTrialPats$DIAG==2], na.rm=T) ; quantile(ClinicalTrialPats$AGE_VISITE0[ClinicalTrialPats$DIAG==2], na.rm=T) # 63, 57-68

wilcox.test(
  na.omit(ClinicalTrialPats$AGE_VISITE0[ClinicalTrialPats$DIAG==1]), 
  na.omit(ClinicalTrialPats$AGE_VISITE0[ClinicalTrialPats$DIAG==2])
)


# -----------------------
# Gender ----------------------------

names(dataCohorteManaged)



# Overall
dataCohorteManaged[, .(n_gender = .N), by = SEXE] # M 350 F 382
#P
dataCohorteManaged[DIAG==1, .(n_gender = .N), by = SEXE] # M 229 F 269
#C
dataCohorteManaged[DIAG==2, .(n_gender = .N), by = SEXE] # M 121 F 113

M <- as.table(rbind(c(229, 269), c(121, 113)))

dimnames(M) <- list(gender = c("MSAP", "MSAC"),
                    party = c("M","F"))


chisq.test(M,  simulate.p.value =T)

sum(is.na(ClinicalTrialPats$SEXE))

# Overall CT
ClinicalTrialPats[, .(n_gender = .N), by = SEXE] # M 186 F 152
#P
ClinicalTrialPats[DIAG==1, .(n_gender = .N), by = SEXE] # M 123 F 108
#C
ClinicalTrialPats[DIAG==2, .(n_gender = .N), by = SEXE] # M 63 F 44

M <- as.table(rbind(c(123, 108), c(63, 44)))

dimnames(M) <- list(gender = c("MSAP", "MSAC"),
                    party = c("M","F"))


chisq.test(M,  simulate.p.value =T)

# -----------------------
# Mortality ----------------------------

names(dataCohorteManaged)

sum(is.na(dataCohorteManaged$DC))


# Overall
dataCohorteManaged[, .(n_dead = .N), by = DC] # Y 369 N 363
#P
dataCohorteManaged[DIAG==1, .(n_dead = .N), by = DC] # Y 272 N 226
#C
dataCohorteManaged[DIAG==2, .(n_dead = .N), by = DC] # Y 97 N 137

M <- as.table(rbind(c(272, 226), c(97, 137)))

dimnames(M) <- list(gender = c("MSAP", "MSAC"),
                    party = c("Y","N"))


chisq.test(M,  simulate.p.value =T)




# Overall CT
ClinicalTrialPats[, .(n_dead = .N), by = DC] # Y 132 N 206
#P
ClinicalTrialPats[DIAG==1, .(n_dead = .N), by = DC] # Y 99 N 132
#C
ClinicalTrialPats[DIAG==2, .(n_dead = .N), by = DC] # Y 33 N 74

M <- as.table(rbind(c(99, 132), c(33, 74)))

dimnames(M) <- list(gender = c("MSAP", "MSAC"),
                    party = c("Y","N"))


chisq.test(M,  simulate.p.value =T)



# -----------------------
# UMSARS 1 ---------------------------------------------------

# Overall
mean(dataCohorteManaged$UMSARS1_TOT, na.rm=T) ; sd(dataCohorteManaged$UMSARS1_TOT, na.rm=T) # 21.65 , 7.71
median(dataCohorteManaged$UMSARS1_TOT, na.rm=T) ; quantile(dataCohorteManaged$UMSARS1_TOT, na.rm=T) # 21, 16-26
sum(!is.na(dataCohorteManaged$UMSARS1_TOT)) ; sum(!is.na(dataCohorteManaged$UMSARS1_TOT))/732 # 96
#P
mean(dataCohorteManaged$UMSARS1_TOT[dataCohorteManaged$DIAG==1], na.rm=T) ; sd(dataCohorteManaged$UMSARS1_TOT[dataCohorteManaged$DIAG==1], na.rm=T) # 22.19 , 7.76
median(dataCohorteManaged$UMSARS1_TOT[dataCohorteManaged$DIAG==1], na.rm=T) ; quantile(dataCohorteManaged$UMSARS1_TOT[dataCohorteManaged$DIAG==1], na.rm=T) # 22, 17-27
#C
mean(dataCohorteManaged$UMSARS1_TOT[dataCohorteManaged$DIAG==2], na.rm=T) ; sd(dataCohorteManaged$UMSARS1_TOT[dataCohorteManaged$DIAG==2], na.rm=T) # 20.48 , 7.49
median(dataCohorteManaged$UMSARS1_TOT[dataCohorteManaged$DIAG==2], na.rm=T) ; quantile(dataCohorteManaged$UMSARS1_TOT[dataCohorteManaged$DIAG==2], na.rm=T) # 19, 16-25

wilcox.test(
  na.omit(dataCohorteManaged$UMSARS1_TOT[dataCohorteManaged$DIAG==1]), 
  na.omit(dataCohorteManaged$UMSARS1_TOT[dataCohorteManaged$DIAG==2])
)


# Overall CT
mean(ClinicalTrialPats$UMSARS1_TOT, na.rm=T) ; sd(ClinicalTrialPats$UMSARS1_TOT, na.rm=T) # 16.42 , 4.63
median(ClinicalTrialPats$UMSARS1_TOT, na.rm=T) ; quantile(ClinicalTrialPats$UMSARS1_TOT, na.rm=T) # 17, 13-20
sum(!is.na(ClinicalTrialPats$UMSARS1_TOT)) ; sum(!is.na(ClinicalTrialPats$UMSARS1_TOT))/338 # 100
#P
mean(ClinicalTrialPats$UMSARS1_TOT[ClinicalTrialPats$DIAG==1], na.rm=T) ; sd(ClinicalTrialPats$UMSARS1_TOT[ClinicalTrialPats$DIAG==1], na.rm=T) # 16.84 , 4.63
median(ClinicalTrialPats$UMSARS1_TOT[ClinicalTrialPats$DIAG==1], na.rm=T) ; quantile(ClinicalTrialPats$UMSARS1_TOT[ClinicalTrialPats$DIAG==1], na.rm=T) # 17, 13-20
#C
mean(ClinicalTrialPats$UMSARS1_TOT[ClinicalTrialPats$DIAG==2], na.rm=T) ; sd(ClinicalTrialPats$UMSARS1_TOT[ClinicalTrialPats$DIAG==2], na.rm=T) # 15.51 , 4.52
median(ClinicalTrialPats$UMSARS1_TOT[ClinicalTrialPats$DIAG==2], na.rm=T) ; quantile(ClinicalTrialPats$UMSARS1_TOT[ClinicalTrialPats$DIAG==2], na.rm=T) # 16, 13-19

wilcox.test(
  na.omit(ClinicalTrialPats$UMSARS1_TOT[ClinicalTrialPats$DIAG==1]), 
  na.omit(ClinicalTrialPats$UMSARS1_TOT[ClinicalTrialPats$DIAG==2])
)


# -----------------------
# UMSARS 2  ---------------------------------------------------

# Overall
mean(dataCohorteManaged$UMSARS2_TOT, na.rm=T) ; sd(dataCohorteManaged$UMSARS2_TOT, na.rm=T) # 23.54 , 8.64
median(dataCohorteManaged$UMSARS2_TOT, na.rm=T) ; quantile(dataCohorteManaged$UMSARS2_TOT, na.rm=T) # 23, 17.25-29.75
sum(!is.na(dataCohorteManaged$UMSARS2_TOT)) ; sum(!is.na(dataCohorteManaged$UMSARS2_TOT))/732 # 706, 96
#P
mean(dataCohorteManaged$UMSARS2_TOT[dataCohorteManaged$DIAG==1], na.rm=T) ; sd(dataCohorteManaged$UMSARS2_TOT[dataCohorteManaged$DIAG==1], na.rm=T) # 24.35 , 8.81
median(dataCohorteManaged$UMSARS2_TOT[dataCohorteManaged$DIAG==1], na.rm=T) ; quantile(dataCohorteManaged$UMSARS2_TOT[dataCohorteManaged$DIAG==1], na.rm=T) # 24, 18-30
#C
mean(dataCohorteManaged$UMSARS2_TOT[dataCohorteManaged$DIAG==2], na.rm=T) ; sd(dataCohorteManaged$UMSARS2_TOT[dataCohorteManaged$DIAG==2], na.rm=T) # 21.81 , 8.01
median(dataCohorteManaged$UMSARS2_TOT[dataCohorteManaged$DIAG==2], na.rm=T) ; quantile(dataCohorteManaged$UMSARS2_TOT[dataCohorteManaged$DIAG==2], na.rm=T) # 21, 17-26

wilcox.test(dataCohorteManaged$UMSARS2_TOT[dataCohorteManaged$DIAG==1], 
            dataCohorteManaged$UMSARS2_TOT[dataCohorteManaged$DIAG==2])

# Overall CT
mean(ClinicalTrialPats$UMSARS2_TOT, na.rm=T) ; sd(ClinicalTrialPats$UMSARS2_TOT, na.rm=T) # 18.05 , 5.89
median(ClinicalTrialPats$UMSARS2_TOT, na.rm=T) ; quantile(ClinicalTrialPats$UMSARS2_TOT, na.rm=T) # 18, 14-22
sum(!is.na(ClinicalTrialPats$UMSARS2_TOT)) ; sum(!is.na(ClinicalTrialPats$UMSARS2_TOT))/338 # 100
#P
mean(ClinicalTrialPats$UMSARS2_TOT[ClinicalTrialPats$DIAG==1], na.rm=T) ; sd(ClinicalTrialPats$UMSARS2_TOT[ClinicalTrialPats$DIAG==1], na.rm=T) # 18.77 , 6.15
median(ClinicalTrialPats$UMSARS2_TOT[ClinicalTrialPats$DIAG==1], na.rm=T) ; quantile(ClinicalTrialPats$UMSARS2_TOT[ClinicalTrialPats$DIAG==1], na.rm=T) # 19, 15-23
#C
mean(ClinicalTrialPats$UMSARS2_TOT[ClinicalTrialPats$DIAG==2], na.rm=T) ; sd(ClinicalTrialPats$UMSARS2_TOT[ClinicalTrialPats$DIAG==2], na.rm=T) # 16.50 , 4.99
median(ClinicalTrialPats$UMSARS2_TOT[ClinicalTrialPats$DIAG==2], na.rm=T) ; quantile(ClinicalTrialPats$UMSARS2_TOT[ClinicalTrialPats$DIAG==2], na.rm=T) # 17, 13-20

wilcox.test(
  na.omit(ClinicalTrialPats$UMSARS2_TOT[ClinicalTrialPats$DIAG==1]), 
  na.omit(ClinicalTrialPats$UMSARS2_TOT[ClinicalTrialPats$DIAG==2])
       )


# -----------------------
# UMSARS 1+2  ---------------------------------------------------

# Overall
mean(dataCohorteManaged$UMSARS1and2_TOT, na.rm=T) ; sd(dataCohorteManaged$UMSARS1and2_TOT, na.rm=T) # 45.17 , 15.36
median(dataCohorteManaged$UMSARS1and2_TOT, na.rm=T) ; quantile(dataCohorteManaged$UMSARS1and2_TOT, na.rm=T) # 43, 35-55
sum(!is.na(dataCohorteManaged$UMSARS1and2_TOT)) ; sum(!is.na(dataCohorteManaged$UMSARS1and2_TOT))/732 # 706, 96

#P
mean(dataCohorteManaged$UMSARS1and2_TOT[dataCohorteManaged$DIAG==1], na.rm=T) ; sd(dataCohorteManaged$UMSARS1and2_TOT[dataCohorteManaged$DIAG==1], na.rm=T) # 46.55 , 15.52
median(dataCohorteManaged$UMSARS1and2_TOT[dataCohorteManaged$DIAG==1], na.rm=T) ; quantile(dataCohorteManaged$UMSARS1and2_TOT[dataCohorteManaged$DIAG==1], na.rm=T) # 45, 36-57
#C
mean(dataCohorteManaged$UMSARS1and2_TOT[dataCohorteManaged$DIAG==2], na.rm=T) ; sd(dataCohorteManaged$UMSARS1and2_TOT[dataCohorteManaged$DIAG==2], na.rm=T) # 42.22 , 14.63
median(dataCohorteManaged$UMSARS1and2_TOT[dataCohorteManaged$DIAG==2], na.rm=T) ; quantile(dataCohorteManaged$UMSARS1and2_TOT[dataCohorteManaged$DIAG==2], na.rm=T) # 40, 33-51

wilcox.test(
  na.omit(dataCohorteManaged$UMSARS1and2_TOT[dataCohorteManaged$DIAG==1]), 
  na.omit(dataCohorteManaged$UMSARS1and2_TOT[dataCohorteManaged$DIAG==2])
)

# Overall CT
mean(ClinicalTrialPats$UMSARS1and2_TOT, na.rm=T) ; sd(ClinicalTrialPats$UMSARS1and2_TOT, na.rm=T) # 34.45 , 9.13
median(ClinicalTrialPats$UMSARS1and2_TOT, na.rm=T) ; quantile(ClinicalTrialPats$UMSARS1and2_TOT, na.rm=T) # 35, 28-41
sum(!is.na(ClinicalTrialPats$UMSARS1and2_TOT)) ; sum(!is.na(ClinicalTrialPats$UMSARS1and2_TOT))/338 # 100
#P
mean(ClinicalTrialPats$UMSARS1and2_TOT[ClinicalTrialPats$DIAG==1], na.rm=T) ; sd(ClinicalTrialPats$UMSARS1and2_TOT[ClinicalTrialPats$DIAG==1], na.rm=T) # 35.58 , 9.40
median(ClinicalTrialPats$UMSARS1and2_TOT[ClinicalTrialPats$DIAG==1], na.rm=T) ; quantile(ClinicalTrialPats$UMSARS1and2_TOT[ClinicalTrialPats$DIAG==1], na.rm=T) # 36, 29-42
#C
mean(ClinicalTrialPats$UMSARS1and2_TOT[ClinicalTrialPats$DIAG==2], na.rm=T) ; sd(ClinicalTrialPats$UMSARS1and2_TOT[ClinicalTrialPats$DIAG==2], na.rm=T) # 32.02 , 8.05
median(ClinicalTrialPats$UMSARS1and2_TOT[ClinicalTrialPats$DIAG==2], na.rm=T) ; quantile(ClinicalTrialPats$UMSARS1and2_TOT[ClinicalTrialPats$DIAG==2], na.rm=T) # 33, 26-38

wilcox.test(
  na.omit(ClinicalTrialPats$UMSARS1and2_TOT[ClinicalTrialPats$DIAG==1]), 
  na.omit(ClinicalTrialPats$UMSARS1and2_TOT[ClinicalTrialPats$DIAG==2])
)


# -----------------------
# UMSARS 1 exc. 11 ------------------

dataCohorteManaged$UMSARS1_exc11 <- dataCohorteManaged$UMSARS1_TOT -  dataCohorteManaged$UMSARS1_11
ClinicalTrialPats$UMSARS1_exc11 <- ClinicalTrialPats$UMSARS1_TOT -  ClinicalTrialPats$UMSARS1_11


# Overall
mean(dataCohorteManaged$UMSARS1_exc11, na.rm=T) ; sd(dataCohorteManaged$UMSARS1_exc11, na.rm=T) # 18.37 , 7.23
median(dataCohorteManaged$UMSARS1_exc11, na.rm=T) ; quantile(dataCohorteManaged$UMSARS1_exc11, na.rm=T) # 17, 13-23
sum(!is.na(dataCohorteManaged$UMSARS1_exc11)) ; sum(!is.na(dataCohorteManaged$UMSARS1_exc11))/732 # 703, 96
#P
mean(dataCohorteManaged$UMSARS1_exc11[dataCohorteManaged$DIAG==1], na.rm=T) ; sd(dataCohorteManaged$UMSARS1_exc11[dataCohorteManaged$DIAG==1], na.rm=T) # 18.89 , 7.29
median(dataCohorteManaged$UMSARS1_exc11[dataCohorteManaged$DIAG==1], na.rm=T) ; quantile(dataCohorteManaged$UMSARS1_exc11[dataCohorteManaged$DIAG==1], na.rm=T) # 18, 14-23
#C
mean(dataCohorteManaged$UMSARS1_exc11[dataCohorteManaged$DIAG==2], na.rm=T) ; sd(dataCohorteManaged$UMSARS1_exc11[dataCohorteManaged$DIAG==2], na.rm=T) # 17.24 , 7.00
median(dataCohorteManaged$UMSARS1_exc11[dataCohorteManaged$DIAG==2], na.rm=T) ; quantile(dataCohorteManaged$UMSARS1_exc11[dataCohorteManaged$DIAG==2], na.rm=T) # 16, 13-21

wilcox.test(dataCohorteManaged$UMSARS1_exc11[dataCohorteManaged$DIAG==1], 
            dataCohorteManaged$UMSARS1_exc11[dataCohorteManaged$DIAG==2])

# Overall CT
mean(ClinicalTrialPats$UMSARS1_exc11, na.rm=T) ; sd(ClinicalTrialPats$UMSARS1_exc11, na.rm=T) # 13.34 , 4.04
median(ClinicalTrialPats$UMSARS1_exc11, na.rm=T) ; quantile(ClinicalTrialPats$UMSARS1_exc11, na.rm=T) # 14, 11-16
sum(!is.na(ClinicalTrialPats$UMSARS1_exc11)) ; sum(!is.na(ClinicalTrialPats$UMSARS1_exc11))/338 # 338 100
#P
mean(ClinicalTrialPats$UMSARS1_exc11[ClinicalTrialPats$DIAG==1], na.rm=T) ; sd(ClinicalTrialPats$UMSARS1_exc11[ClinicalTrialPats$DIAG==1], na.rm=T) # 13.74 , 4.07
median(ClinicalTrialPats$UMSARS1_exc11[ClinicalTrialPats$DIAG==1], na.rm=T) ; quantile(ClinicalTrialPats$UMSARS1_exc11[ClinicalTrialPats$DIAG==1], na.rm=T) # 14, 12-17
#C
mean(ClinicalTrialPats$UMSARS1_exc11[ClinicalTrialPats$DIAG==2], na.rm=T) ; sd(ClinicalTrialPats$UMSARS1_exc11[ClinicalTrialPats$DIAG==2], na.rm=T) # 12.48 , 3.85
median(ClinicalTrialPats$UMSARS1_exc11[ClinicalTrialPats$DIAG==2], na.rm=T) ; quantile(ClinicalTrialPats$UMSARS1_exc11[ClinicalTrialPats$DIAG==2], na.rm=T) # 13, 11-15

wilcox.test(
  na.omit(ClinicalTrialPats$UMSARS1_exc11[ClinicalTrialPats$DIAG==1]), 
  na.omit(ClinicalTrialPats$UMSARS1_exc11[ClinicalTrialPats$DIAG==2])
)

# -----------------------
# UMSARS 1 exc. 11 TAK 341 collapsed  ------------------

dataCohorteManaged <- dataCohorteManaged %>% left_join(
  dataCohorteManaged %>% select(NUM, DIAG, UMSARS1_1:UMSARS1_12, UMSARS1_TOT) %>%
  mutate(UMSARS1_1=UMSARS1_1-1) %>% mutate(UMSARS1_1=ifelse(UMSARS1_1<0,0, UMSARS1_1)) %>%
  mutate(UMSARS1_2=UMSARS1_2-1) %>% mutate(UMSARS1_2=ifelse(UMSARS1_2<0,0, UMSARS1_2)) %>%
  mutate(UMSARS1_3=UMSARS1_3-1) %>% mutate(UMSARS1_3=ifelse(UMSARS1_3<0,0, UMSARS1_3)) %>%
  mutate(UMSARS1_4=UMSARS1_4-1) %>% mutate(UMSARS1_4=ifelse(UMSARS1_4<0,0, UMSARS1_4)) %>%
  mutate(UMSARS1_5=UMSARS1_5-1) %>% mutate(UMSARS1_5=ifelse(UMSARS1_5<0,0, UMSARS1_5)) %>%
  mutate(UMSARS1_6=UMSARS1_6-1) %>% mutate(UMSARS1_6=ifelse(UMSARS1_6<0,0, UMSARS1_6)) %>%
  mutate(UMSARS1_7=UMSARS1_7-1) %>% mutate(UMSARS1_7=ifelse(UMSARS1_7<0,0, UMSARS1_7)) %>%
  mutate(UMSARS1_8=UMSARS1_8-1) %>% mutate(UMSARS1_8=ifelse(UMSARS1_8<0,0, UMSARS1_8)) %>%
  mutate(UMSARS1_9=UMSARS1_9-1) %>% mutate(UMSARS1_9=ifelse(UMSARS1_9<0,0, UMSARS1_9)) %>%
  mutate(UMSARS1_10=UMSARS1_10-1) %>% mutate(UMSARS1_10=ifelse(UMSARS1_10<0,0, UMSARS1_10)) %>%
  mutate(UMSARS1_11=UMSARS1_11-1) %>% mutate(UMSARS1_11=ifelse(UMSARS1_11<0,0, UMSARS1_11)) %>%
  mutate(UMSARS1_12=UMSARS1_12-1) %>% mutate(UMSARS1_12=ifelse(UMSARS1_12<0,0, UMSARS1_12)) %>%
  mutate(UMSARS1_TOT_collapsed=UMSARS1_1+UMSARS1_2+UMSARS1_3+UMSARS1_4+UMSARS1_5+UMSARS1_6+UMSARS1_7+UMSARS1_8+UMSARS1_9+UMSARS1_10+UMSARS1_12) %>%
  select(NUM, UMSARS1_TOT_collapsed)
  )


ClinicalTrialPats <- ClinicalTrialPats %>% left_join(
  ClinicalTrialPats %>% select(NUM, DIAG, UMSARS1_1:UMSARS1_12, UMSARS1_TOT) %>%
    mutate(UMSARS1_1=UMSARS1_1-1) %>% mutate(UMSARS1_1=ifelse(UMSARS1_1<0,0, UMSARS1_1)) %>%
    mutate(UMSARS1_2=UMSARS1_2-1) %>% mutate(UMSARS1_2=ifelse(UMSARS1_2<0,0, UMSARS1_2)) %>%
    mutate(UMSARS1_3=UMSARS1_3-1) %>% mutate(UMSARS1_3=ifelse(UMSARS1_3<0,0, UMSARS1_3)) %>%
    mutate(UMSARS1_4=UMSARS1_4-1) %>% mutate(UMSARS1_4=ifelse(UMSARS1_4<0,0, UMSARS1_4)) %>%
    mutate(UMSARS1_5=UMSARS1_5-1) %>% mutate(UMSARS1_5=ifelse(UMSARS1_5<0,0, UMSARS1_5)) %>%
    mutate(UMSARS1_6=UMSARS1_6-1) %>% mutate(UMSARS1_6=ifelse(UMSARS1_6<0,0, UMSARS1_6)) %>%
    mutate(UMSARS1_7=UMSARS1_7-1) %>% mutate(UMSARS1_7=ifelse(UMSARS1_7<0,0, UMSARS1_7)) %>%
    mutate(UMSARS1_8=UMSARS1_8-1) %>% mutate(UMSARS1_8=ifelse(UMSARS1_8<0,0, UMSARS1_8)) %>%
    mutate(UMSARS1_9=UMSARS1_9-1) %>% mutate(UMSARS1_9=ifelse(UMSARS1_9<0,0, UMSARS1_9)) %>%
    mutate(UMSARS1_10=UMSARS1_10-1) %>% mutate(UMSARS1_10=ifelse(UMSARS1_10<0,0, UMSARS1_10)) %>%
    mutate(UMSARS1_11=UMSARS1_11-1) %>% mutate(UMSARS1_11=ifelse(UMSARS1_11<0,0, UMSARS1_11)) %>%
    mutate(UMSARS1_12=UMSARS1_12-1) %>% mutate(UMSARS1_12=ifelse(UMSARS1_12<0,0, UMSARS1_12)) %>%
    mutate(UMSARS1_TOT_collapsed=UMSARS1_1+UMSARS1_2+UMSARS1_3+UMSARS1_4+UMSARS1_5+UMSARS1_6+UMSARS1_7+UMSARS1_8+UMSARS1_9+UMSARS1_10+UMSARS1_12) %>%
    select(NUM, UMSARS1_TOT_collapsed)
)



# Overall
mean(dataCohorteManaged$UMSARS1_TOT_collapsed, na.rm=T) ; sd(dataCohorteManaged$UMSARS1_TOT_collapsed, na.rm=T) # 8.75 , 6.27
median(dataCohorteManaged$UMSARS1_TOT_collapsed, na.rm=T) ; quantile(dataCohorteManaged$UMSARS1_TOT_collapsed, na.rm=T) # 7, 4-12
sum(!is.na(dataCohorteManaged$UMSARS1_TOT_collapsed)) ; sum(!is.na(dataCohorteManaged$UMSARS1_TOT_collapsed))/732 # 704, 96
#P
mean(dataCohorteManaged$UMSARS1_TOT_collapsed[dataCohorteManaged$DIAG==1], na.rm=T) ; sd(dataCohorteManaged$UMSARS1_TOT_collapsed[dataCohorteManaged$DIAG==1], na.rm=T) # 9.24 , 6.34
median(dataCohorteManaged$UMSARS1_TOT_collapsed[dataCohorteManaged$DIAG==1], na.rm=T) ; quantile(dataCohorteManaged$UMSARS1_TOT_collapsed[dataCohorteManaged$DIAG==1], na.rm=T) # 8, 4-13
#C
mean(dataCohorteManaged$UMSARS1_TOT_collapsed[dataCohorteManaged$DIAG==2], na.rm=T) ; sd(dataCohorteManaged$UMSARS1_TOT_collapsed[dataCohorteManaged$DIAG==2], na.rm=T) # 7.70 , 6.00
median(dataCohorteManaged$UMSARS1_TOT_collapsed[dataCohorteManaged$DIAG==2], na.rm=T) ; quantile(dataCohorteManaged$UMSARS1_TOT_collapsed[dataCohorteManaged$DIAG==2], na.rm=T) # 6, 3-11

wilcox.test(dataCohorteManaged$UMSARS1_TOT_collapsed[dataCohorteManaged$DIAG==1], 
            dataCohorteManaged$UMSARS1_TOT_collapsed[dataCohorteManaged$DIAG==2])

# Overall CT
mean(ClinicalTrialPats$UMSARS1_TOT_collapsed, na.rm=T) ; sd(ClinicalTrialPats$UMSARS1_TOT_collapsed, na.rm=T) # 4.39 , 2.68
median(ClinicalTrialPats$UMSARS1_TOT_collapsed, na.rm=T) ; quantile(ClinicalTrialPats$UMSARS1_TOT_collapsed, na.rm=T) # 4, 2-6
sum(!is.na(ClinicalTrialPats$UMSARS1_TOT_collapsed)) ; sum(!is.na(ClinicalTrialPats$UMSARS1_TOT_collapsed))/338 # 338 100
#P
mean(ClinicalTrialPats$UMSARS1_TOT_collapsed[ClinicalTrialPats$DIAG==1], na.rm=T) ; sd(ClinicalTrialPats$UMSARS1_TOT_collapsed[ClinicalTrialPats$DIAG==1], na.rm=T) # 4.69 , 2.72
median(ClinicalTrialPats$UMSARS1_TOT_collapsed[ClinicalTrialPats$DIAG==1], na.rm=T) ; quantile(ClinicalTrialPats$UMSARS1_TOT_collapsed[ClinicalTrialPats$DIAG==1], na.rm=T) # 5, 3-6
#C
mean(ClinicalTrialPats$UMSARS1_TOT_collapsed[ClinicalTrialPats$DIAG==2], na.rm=T) ; sd(ClinicalTrialPats$UMSARS1_TOT_collapsed[ClinicalTrialPats$DIAG==2], na.rm=T) # 3.72 , 2.47
median(ClinicalTrialPats$UMSARS1_TOT_collapsed[ClinicalTrialPats$DIAG==2], na.rm=T) ; quantile(ClinicalTrialPats$UMSARS1_TOT_collapsed[ClinicalTrialPats$DIAG==2], na.rm=T) # 3, 2-5

wilcox.test(
  na.omit(ClinicalTrialPats$UMSARS1_TOT_collapsed[ClinicalTrialPats$DIAG==1]), 
  na.omit(ClinicalTrialPats$UMSARS1_TOT_collapsed[ClinicalTrialPats$DIAG==2])
)

# -----------------------
# COMPASS ----------------------------

# Overall
mean(dataCohorteManaged$COMPASS_TOT, na.rm=T) ; sd(dataCohorteManaged$COMPASS_TOT, na.rm=T) # 22.22 , 13.40
median(dataCohorteManaged$COMPASS_TOT, na.rm=T) ; quantile(dataCohorteManaged$COMPASS_TOT, na.rm=T) # 22, 14-31
sum(!is.na(dataCohorteManaged$COMPASS_TOT)) ; sum(!is.na(dataCohorteManaged$COMPASS_TOT))/732 # 118, 16%
#P
mean(dataCohorteManaged$COMPASS_TOT[dataCohorteManaged$DIAG==1], na.rm=T) ; sd(dataCohorteManaged$COMPASS_TOT[dataCohorteManaged$DIAG==1], na.rm=T) # 25.04 , 13.24
median(dataCohorteManaged$COMPASS_TOT[dataCohorteManaged$DIAG==1], na.rm=T) ; quantile(dataCohorteManaged$COMPASS_TOT[dataCohorteManaged$DIAG==1], na.rm=T) # 25, 17-34
#C
mean(dataCohorteManaged$COMPASS_TOT[dataCohorteManaged$DIAG==2], na.rm=T) ; sd(dataCohorteManaged$COMPASS_TOT[dataCohorteManaged$DIAG==2], na.rm=T) # 17.96 , 12.60
median(dataCohorteManaged$COMPASS_TOT[dataCohorteManaged$DIAG==2], na.rm=T) ; quantile(dataCohorteManaged$COMPASS_TOT[dataCohorteManaged$DIAG==2], na.rm=T) # 21, 6-27

wilcox.test(
  na.omit(dataCohorteManaged$COMPASS_TOT[dataCohorteManaged$DIAG==1]), 
  na.omit(dataCohorteManaged$COMPASS_TOT[dataCohorteManaged$DIAG==2])
)


# Overall CT
mean(ClinicalTrialPats$COMPASS_TOT, na.rm=T) ; sd(ClinicalTrialPats$COMPASS_TOT, na.rm=T) # 19.36 , 12.31
median(ClinicalTrialPats$COMPASS_TOT, na.rm=T) ; quantile(ClinicalTrialPats$COMPASS_TOT, na.rm=T) # 19, 11-27
sum(!is.na(ClinicalTrialPats$COMPASS_TOT)) ; sum(!is.na(ClinicalTrialPats$COMPASS_TOT))/338 # 64 19%
#P
mean(ClinicalTrialPats$COMPASS_TOT[ClinicalTrialPats$DIAG==1], na.rm=T) ; sd(ClinicalTrialPats$COMPASS_TOT[ClinicalTrialPats$DIAG==1], na.rm=T) # 22.49 , 11.54
median(ClinicalTrialPats$COMPASS_TOT[ClinicalTrialPats$DIAG==1], na.rm=T) ; quantile(ClinicalTrialPats$COMPASS_TOT[ClinicalTrialPats$DIAG==1], na.rm=T) # 22, 16-30
#C
mean(ClinicalTrialPats$COMPASS_TOT[ClinicalTrialPats$DIAG==2], na.rm=T) ; sd(ClinicalTrialPats$COMPASS_TOT[ClinicalTrialPats$DIAG==2], na.rm=T) # 14.48 , 12.09
median(ClinicalTrialPats$COMPASS_TOT[ClinicalTrialPats$DIAG==2], na.rm=T) ; quantile(ClinicalTrialPats$COMPASS_TOT[ClinicalTrialPats$DIAG==2], na.rm=T) # 13, 4-26

wilcox.test(
  na.omit(ClinicalTrialPats$COMPASS_TOT[ClinicalTrialPats$DIAG==1]), 
  na.omit(ClinicalTrialPats$COMPASS_TOT[ClinicalTrialPats$DIAG==2])
)

# -----------------------
# QoL ----------------------------

# Overall
mean(dataCohorteManaged$ECHANQV, na.rm=T) ; sd(dataCohorteManaged$ECHANQV, na.rm=T) # 42.90 , 21.00
median(dataCohorteManaged$ECHANQV, na.rm=T) ; quantile(dataCohorteManaged$ECHANQV, na.rm=T) # 40, 30-55
sum(!is.na(dataCohorteManaged$ECHANQV)) ; sum(!is.na(dataCohorteManaged$ECHANQV))/732 # 406, 5%
#P
mean(dataCohorteManaged$ECHANQV[dataCohorteManaged$DIAG==1], na.rm=T) ; sd(dataCohorteManaged$ECHANQV[dataCohorteManaged$DIAG==1], na.rm=T) # 41.99 , 20.52
median(dataCohorteManaged$ECHANQV[dataCohorteManaged$DIAG==1], na.rm=T) ; quantile(dataCohorteManaged$ECHANQV[dataCohorteManaged$DIAG==1], na.rm=T) # 40, 30-55
#C
mean(dataCohorteManaged$ECHANQV[dataCohorteManaged$DIAG==2], na.rm=T) ; sd(dataCohorteManaged$ECHANQV[dataCohorteManaged$DIAG==2], na.rm=T) # 44.64 , 21.86
median(dataCohorteManaged$ECHANQV[dataCohorteManaged$DIAG==2], na.rm=T) ; quantile(dataCohorteManaged$ECHANQV[dataCohorteManaged$DIAG==2], na.rm=T) # 50, 30-55

wilcox.test(
  na.omit(dataCohorteManaged$ECHANQV[dataCohorteManaged$DIAG==1]), 
  na.omit(dataCohorteManaged$ECHANQV[dataCohorteManaged$DIAG==2])
)


# Overall CT
mean(ClinicalTrialPats$ECHANQV, na.rm=T) ; sd(ClinicalTrialPats$ECHANQV, na.rm=T) # 46.65 , 20.52
median(ClinicalTrialPats$ECHANQV, na.rm=T) ; quantile(ClinicalTrialPats$ECHANQV, na.rm=T) # 50, 30-60
sum(!is.na(ClinicalTrialPats$ECHANQV)) ; sum(!is.na(ClinicalTrialPats$ECHANQV))/338 # 204 60%
#P
mean(ClinicalTrialPats$ECHANQV[ClinicalTrialPats$DIAG==1], na.rm=T) ; sd(ClinicalTrialPats$ECHANQV[ClinicalTrialPats$DIAG==1], na.rm=T) # 45.05 , 19.59
median(ClinicalTrialPats$ECHANQV[ClinicalTrialPats$DIAG==1], na.rm=T) ; quantile(ClinicalTrialPats$ECHANQV[ClinicalTrialPats$DIAG==1], na.rm=T) # 48, 30-60
#C
mean(ClinicalTrialPats$ECHANQV[ClinicalTrialPats$DIAG==2], na.rm=T) ; sd(ClinicalTrialPats$ECHANQV[ClinicalTrialPats$DIAG==2], na.rm=T) # 49.34 , 21.87
median(ClinicalTrialPats$ECHANQV[ClinicalTrialPats$DIAG==2], na.rm=T) ; quantile(ClinicalTrialPats$ECHANQV[ClinicalTrialPats$DIAG==2], na.rm=T) # 50, 40-66

wilcox.test(
  na.omit(ClinicalTrialPats$ECHANQV[ClinicalTrialPats$DIAG==1]), 
  na.omit(ClinicalTrialPats$ECHANQV[ClinicalTrialPats$DIAG==2])
)

# -----------------------
# UPDRS 3 -------------------------------------

dataCohorteManaged <- dataCohorteManaged %>% 
  left_join(
    dataCohorteManaged %>% select(NUM, DIAG, deltaPAD, deltaPAS, PAS_COU, PAD_COU) %>% 
      mutate(SupHyper=ifelse(PAS_COU>140|PAD_COU>90, 1 , 0)) %>%
      mutate(OrthoHypo=ifelse( abs(deltaPAS) < 20 | abs(deltaPAD) < 10, 1 , 0)) %>%
      select(NUM, SupHyper, OrthoHypo)
    )

ClinicalTrialPats <- ClinicalTrialPats %>% 
  left_join(
    ClinicalTrialPats %>% select(NUM, DIAG, deltaPAD, deltaPAS, PAS_COU, PAD_COU) %>% 
      mutate(SupHyper=ifelse(PAS_COU>140|PAD_COU>90, 1 , 0)) %>%
      mutate(OrthoHypo=ifelse( abs(deltaPAS) < 20 | abs(deltaPAD) < 10, 1 , 0)) %>%
      select(NUM, SupHyper, OrthoHypo)
  )

# SupHyper

sum(!is.na(dataCohorteManaged$SupHyper)) # 664 91%


# Overall
dataCohorteManaged[, .(n_dead = .N), by = SupHyper] # Y 308 N 356
#P
dataCohorteManaged[DIAG==1, .(n_dead = .N), by = SupHyper] # Y 195 N 261
#C
dataCohorteManaged[DIAG==2, .(n_dead = .N), by = SupHyper] # Y 113 N 95

M <- as.table(rbind(c(195, 261), c(113, 95)))

dimnames(M) <- list(gender = c("MSAP", "MSAC"),
                    party = c("Y","N"))


chisq.test(M,  simulate.p.value =T)


sum(!is.na(ClinicalTrialPats$SupHyper)) # 321 95%


# Overall CT
ClinicalTrialPats[, .(n_dead = .N), by = SupHyper] # Y 149 N 172
#P
ClinicalTrialPats[DIAG==1, .(n_dead = .N), by = SupHyper] # Y 98 N 121
#C
ClinicalTrialPats[DIAG==2, .(n_dead = .N), by = SupHyper] # Y 51 N 51

M <- as.table(rbind(c(98, 121), c(51, 51)))

dimnames(M) <- list(gender = c("MSAP", "MSAC"),
                    party = c("Y","N"))


chisq.test(M,  simulate.p.value =T)



# OrthoHypo

sum(!is.na(dataCohorteManaged$OrthoHypo)) # 661 90%


# Overall
dataCohorteManaged[, .(n_dead = .N), by = OrthoHypo] # Y 275 N 386
#P
dataCohorteManaged[DIAG==1, .(n_dead = .N), by = OrthoHypo] # Y 188 N 266
#C
dataCohorteManaged[DIAG==2, .(n_dead = .N), by = OrthoHypo] # Y 87 N 120

M <- as.table(rbind(c(188, 266), c(87, 120)))

dimnames(M) <- list(gender = c("MSAP", "MSAC"),
                    party = c("Y","N"))


chisq.test(M,  simulate.p.value =T)


sum(!is.na(ClinicalTrialPats$OrthoHypo)) # 320 95%


# Overall CT
ClinicalTrialPats[, .(n_dead = .N), by = OrthoHypo] # Y 151 N 169
#P
ClinicalTrialPats[DIAG==1, .(n_dead = .N), by = OrthoHypo] # Y 102 N 116
#C
ClinicalTrialPats[DIAG==2, .(n_dead = .N), by = OrthoHypo] # Y 49 N 53

M <- as.table(rbind(c(102, 116), c(49, 53)))

dimnames(M) <- list(gender = c("MSAP", "MSAC"),
                    party = c("Y","N"))


chisq.test(M,  simulate.p.value =T)


# ----------------------------

# 9 item UMSARS ------------------------------------------------

# 9-item I1, I4, I5, I6, I7, I10, II11, II12, II14
# 11-item UMSARS-1 items 2, 3, 6, 7, 11; UMSARS-2 items 1, 2, 9, 11, 12, 14

dataCohorteManaged <- dataCohorteManaged %>% 
  mutate(UMSARS_9item = UMSARS1_1 + UMSARS1_4 + UMSARS1_5 + UMSARS1_6 + UMSARS1_7 + UMSARS1_10 + UMSARS2_11 + UMSARS2_12 + UMSARS2_14) %>%
  mutate(UMSARS_11item = UMSARS1_2 + UMSARS1_3 + UMSARS1_6 + UMSARS1_7 + UMSARS1_11 + UMSARS2_1 + UMSARS2_2 + UMSARS2_9 + UMSARS2_11 + UMSARS2_12 + UMSARS2_14 ) 

ClinicalTrialPats <- ClinicalTrialPats %>% 
  mutate(UMSARS_9item = UMSARS1_1 + UMSARS1_4 + UMSARS1_5 + UMSARS1_6 + UMSARS1_7 + UMSARS1_10 + UMSARS2_11 + UMSARS2_12 + UMSARS2_14) %>%
  mutate(UMSARS_11item = UMSARS1_2 + UMSARS1_3 + UMSARS1_6 + UMSARS1_7 + UMSARS1_11 + UMSARS2_1 + UMSARS2_2 + UMSARS2_9 + UMSARS2_11 + UMSARS2_12 + UMSARS2_14 ) 


# Overall
mean(dataCohorteManaged$UMSARS_9item, na.rm=T) ; sd(dataCohorteManaged$UMSARS_9item, na.rm=T) # 16.28 , 7.09
median(dataCohorteManaged$UMSARS_9item, na.rm=T) ; quantile(dataCohorteManaged$UMSARS_9item, na.rm=T) # 15, 11-21
sum(!is.na(dataCohorteManaged$UMSARS_9item)) ; sum(!is.na(dataCohorteManaged$UMSARS_9item))/732 # 699 , 95
#P
mean(dataCohorteManaged$UMSARS_9item[dataCohorteManaged$DIAG==1], na.rm=T) ; sd(dataCohorteManaged$UMSARS_9item[dataCohorteManaged$DIAG==1], na.rm=T) # 16.85 , 7.26
median(dataCohorteManaged$UMSARS_9item[dataCohorteManaged$DIAG==1], na.rm=T) ; quantile(dataCohorteManaged$UMSARS_9item[dataCohorteManaged$DIAG==1], na.rm=T) # 16, 11-22
#C
mean(dataCohorteManaged$UMSARS_9item[dataCohorteManaged$DIAG==2], na.rm=T) ; sd(dataCohorteManaged$UMSARS_9item[dataCohorteManaged$DIAG==2], na.rm=T) # 15.04 , 6.55
median(dataCohorteManaged$UMSARS_9item[dataCohorteManaged$DIAG==2], na.rm=T) ; quantile(dataCohorteManaged$UMSARS_9item[dataCohorteManaged$DIAG==2], na.rm=T) # 14, 10-19

wilcox.test(
  na.omit(dataCohorteManaged$UMSARS_9item[dataCohorteManaged$DIAG==1]), 
  na.omit(dataCohorteManaged$UMSARS_9item[dataCohorteManaged$DIAG==2])
)


# Overall CT
mean(ClinicalTrialPats$UMSARS_9item, na.rm=T) ; sd(ClinicalTrialPats$UMSARS_9item, na.rm=T) # 11.08 , 3.78
median(ClinicalTrialPats$UMSARS_9item, na.rm=T) ; quantile(ClinicalTrialPats$UMSARS_9item, na.rm=T) # 11, 8-14
sum(!is.na(ClinicalTrialPats$UMSARS_9item)) ; sum(!is.na(ClinicalTrialPats$UMSARS_9item))/338 # 100
#P
mean(ClinicalTrialPats$UMSARS_9item[ClinicalTrialPats$DIAG==1], na.rm=T) ; sd(ClinicalTrialPats$UMSARS_9item[ClinicalTrialPats$DIAG==1], na.rm=T) # 11.47 , 3.96
median(ClinicalTrialPats$UMSARS_9item[ClinicalTrialPats$DIAG==1], na.rm=T) ; quantile(ClinicalTrialPats$UMSARS_9item[ClinicalTrialPats$DIAG==1], na.rm=T) # 11, 9-14
#C
mean(ClinicalTrialPats$UMSARS_9item[ClinicalTrialPats$DIAG==2], na.rm=T) ; sd(ClinicalTrialPats$UMSARS_9item[ClinicalTrialPats$DIAG==2], na.rm=T) # 10.24 , 3.22
median(ClinicalTrialPats$UMSARS_9item[ClinicalTrialPats$DIAG==2], na.rm=T) ; quantile(ClinicalTrialPats$UMSARS_9item[ClinicalTrialPats$DIAG==2], na.rm=T) # 10, 8-12

wilcox.test(
  na.omit(ClinicalTrialPats$UMSARS_9item[ClinicalTrialPats$DIAG==1]), 
  na.omit(ClinicalTrialPats$UMSARS_9item[ClinicalTrialPats$DIAG==2])
)

# ---------------------
# 11 item UMSARS ------------------------------------------------


# 9-item I1, I4, I5, I6, I7, I10, II11, II12, II14
# 11-item UMSARS-1 items 2, 3, 6, 7, 11; UMSARS-2 items 1, 2, 9, 11, 12, 14

dataCohorteManaged <- dataCohorteManaged %>% 
  mutate(UMSARS_9item = UMSARS1_1 + UMSARS1_4 + UMSARS1_5 + UMSARS1_6 + UMSARS1_7 + UMSARS1_10 + UMSARS2_11 + UMSARS2_12 + UMSARS2_14) %>%
  mutate(UMSARS_11item = UMSARS1_2 + UMSARS1_3 + UMSARS1_6 + UMSARS1_7 + UMSARS1_11 + UMSARS2_1 + UMSARS2_2 + UMSARS2_9 + UMSARS2_11 + UMSARS2_12 + UMSARS2_14 ) 

ClinicalTrialPats <- ClinicalTrialPats %>% 
  mutate(UMSARS_9item = UMSARS1_1 + UMSARS1_4 + UMSARS1_5 + UMSARS1_6 + UMSARS1_7 + UMSARS1_10 + UMSARS2_11 + UMSARS2_12 + UMSARS2_14) %>%
  mutate(UMSARS_11item = UMSARS1_2 + UMSARS1_3 + UMSARS1_6 + UMSARS1_7 + UMSARS1_11 + UMSARS2_1 + UMSARS2_2 + UMSARS2_9 + UMSARS2_11 + UMSARS2_12 + UMSARS2_14 ) 



# Overall
mean(dataCohorteManaged$UMSARS_11item, na.rm=T) ; sd(dataCohorteManaged$UMSARS_11item, na.rm=T) # 20.91 , 7.79
median(dataCohorteManaged$UMSARS_11item, na.rm=T) ; quantile(dataCohorteManaged$UMSARS_11item, na.rm=T) # 20, 15-26
sum(!is.na(dataCohorteManaged$UMSARS_11item)) ; sum(!is.na(dataCohorteManaged$UMSARS_11item))/732 # 697 95
#P
mean(dataCohorteManaged$UMSARS_11item[dataCohorteManaged$DIAG==1], na.rm=T) ; sd(dataCohorteManaged$UMSARS_11item[dataCohorteManaged$DIAG==1], na.rm=T) # 21.59 , 8.01
median(dataCohorteManaged$UMSARS_11item[dataCohorteManaged$DIAG==1], na.rm=T) ; quantile(dataCohorteManaged$UMSARS_11item[dataCohorteManaged$DIAG==1], na.rm=T) # 21, 16-27
#C
mean(dataCohorteManaged$UMSARS_11item[dataCohorteManaged$DIAG==2], na.rm=T) ; sd(dataCohorteManaged$UMSARS_11item[dataCohorteManaged$DIAG==2], na.rm=T) # 20.48 , 7.49
median(dataCohorteManaged$UMSARS_11item[dataCohorteManaged$DIAG==2], na.rm=T) ; quantile(dataCohorteManaged$UMSARS_11item[dataCohorteManaged$DIAG==2], na.rm=T) # 19, 16-25

wilcox.test(
  na.omit(dataCohorteManaged$UMSARS_11item[dataCohorteManaged$DIAG==1]), 
  na.omit(dataCohorteManaged$UMSARS_11item[dataCohorteManaged$DIAG==2])
)


# Overall CT
mean(ClinicalTrialPats$UMSARS_11item, na.rm=T) ; sd(ClinicalTrialPats$UMSARS_11item, na.rm=T) # 15.33 , 4.78
median(ClinicalTrialPats$UMSARS_11item, na.rm=T) ; quantile(ClinicalTrialPats$UMSARS_11item, na.rm=T) # 15, 12-18
sum(!is.na(ClinicalTrialPats$UMSARS_11item)) ; sum(!is.na(ClinicalTrialPats$UMSARS_11item))/338 # 100
#P
mean(ClinicalTrialPats$UMSARS_11item[ClinicalTrialPats$DIAG==1], na.rm=T) ; sd(ClinicalTrialPats$UMSARS_11item[ClinicalTrialPats$DIAG==1], na.rm=T) # 15.83 , 5.07
median(ClinicalTrialPats$UMSARS_11item[ClinicalTrialPats$DIAG==1], na.rm=T) ; quantile(ClinicalTrialPats$UMSARS_11item[ClinicalTrialPats$DIAG==1], na.rm=T) # 16, 13-19
#C
mean(ClinicalTrialPats$UMSARS_11item[ClinicalTrialPats$DIAG==2], na.rm=T) ; sd(ClinicalTrialPats$UMSARS_11item[ClinicalTrialPats$DIAG==2], na.rm=T) # 14.26 , 3.88
median(ClinicalTrialPats$UMSARS_11item[ClinicalTrialPats$DIAG==2], na.rm=T) ; quantile(ClinicalTrialPats$UMSARS_11item[ClinicalTrialPats$DIAG==2], na.rm=T) # 15, 12-17

wilcox.test(
  na.omit(ClinicalTrialPats$UMSARS_11item[ClinicalTrialPats$DIAG==1]), 
  na.omit(ClinicalTrialPats$UMSARS_11item[ClinicalTrialPats$DIAG==2])
)


# -----------------------------------------------


# No of visits --------------------------------

dataCohorteManaged_pats <- dataCohorteManaged %>% select(NUM, DIAG)
ClinicalTrialPats_pats <- ClinicalTrialPats %>% select(NUM, DIAG)

dataCohorteManaged_all <- readRDS("Source/dataCohorteManaged.rds")

n_visits <- data.table(dataCohorteManaged_all)[, .(n_visits = .N), by = NUM]

dataCohorteManaged_pats <- dataCohorteManaged_pats %>% left_join(n_visits)
ClinicalTrialPats_pats <- ClinicalTrialPats_pats %>% left_join(n_visits)

mean(dataCohorteManaged_pats$n_visits)
mean(ClinicalTrialPats_pats$n_visits)


# Overall
mean(dataCohorteManaged_pats$n_visits, na.rm=T) ; sd(dataCohorteManaged_pats$n_visits, na.rm=T) # 3.18 , 2.35
median(dataCohorteManaged_pats$n_visits, na.rm=T) ; quantile(dataCohorteManaged_pats$n_visits, na.rm=T) # 3, 1-4
sum(!is.na(dataCohorteManaged_pats$n_visits)) ; sum(!is.na(dataCohorteManaged_pats$n_visits))/732 # 732, 100
#P
mean(dataCohorteManaged_pats$n_visits[dataCohorteManaged_pats$DIAG==1], na.rm=T) ; sd(dataCohorteManaged_pats$n_visits[dataCohorteManaged_pats$DIAG==1], na.rm=T) # 3.09 , 2.32
median(dataCohorteManaged_pats$n_visits[dataCohorteManaged_pats$DIAG==1], na.rm=T) ; quantile(dataCohorteManaged_pats$n_visits[dataCohorteManaged_pats$DIAG==1], na.rm=T) # 3, 1-4
#C
mean(dataCohorteManaged_pats$n_visits[dataCohorteManaged_pats$DIAG==2], na.rm=T) ; sd(dataCohorteManaged_pats$n_visits[dataCohorteManaged_pats$DIAG==2], na.rm=T) # 3.38 , 2.40
median(dataCohorteManaged_pats$n_visits[dataCohorteManaged_pats$DIAG==2], na.rm=T) ; quantile(dataCohorteManaged_pats$n_visits[dataCohorteManaged_pats$DIAG==2], na.rm=T) # 3, 1-5

wilcox.test(
  na.omit(dataCohorteManaged_pats$n_visits[dataCohorteManaged_pats$DIAG==1]), 
  na.omit(dataCohorteManaged_pats$n_visits[dataCohorteManaged_pats$DIAG==2])
)


# Overall CT
mean(ClinicalTrialPats_pats$n_visits, na.rm=T) ; sd(ClinicalTrialPats_pats$n_visits, na.rm=T) # 3.85 , 2.69
median(ClinicalTrialPats_pats$n_visits, na.rm=T) ; quantile(ClinicalTrialPats_pats$n_visits, na.rm=T) # 3, 2-5
sum(!is.na(ClinicalTrialPats_pats$n_visits)) ; sum(!is.na(ClinicalTrialPats_pats$n_visits))/338 # 100
#P
mean(ClinicalTrialPats_pats$n_visits[ClinicalTrialPats_pats$DIAG==1], na.rm=T) ; sd(ClinicalTrialPats_pats$n_visits[ClinicalTrialPats_pats$DIAG==1], na.rm=T) # 3.79 , 2.63
median(ClinicalTrialPats_pats$n_visits[ClinicalTrialPats_pats$DIAG==1], na.rm=T) ; quantile(ClinicalTrialPats_pats$n_visits[ClinicalTrialPats_pats$DIAG==1], na.rm=T) # 3, 2-5
#C
mean(ClinicalTrialPats_pats$n_visits[ClinicalTrialPats_pats$DIAG==2], na.rm=T) ; sd(ClinicalTrialPats_pats$n_visits[ClinicalTrialPats_pats$DIAG==2], na.rm=T) # 3.97 , 2.82
median(ClinicalTrialPats_pats$n_visits[ClinicalTrialPats_pats$DIAG==2], na.rm=T) ; quantile(ClinicalTrialPats_pats$n_visits[ClinicalTrialPats_pats$DIAG==2], na.rm=T) # 3, 2-6

wilcox.test(
  na.omit(ClinicalTrialPats_pats$n_visits[ClinicalTrialPats_pats$DIAG==1]), 
  na.omit(ClinicalTrialPats_pats$n_visits[ClinicalTrialPats_pats$DIAG==2])
)

# ----------------
# Total months visibility ------------------------------

dataCohorteManaged_all <- readRDS("Source/dataCohorteManaged.rds")

visibility <- dataCohorteManaged_all %>% group_by(NUM) %>% summarise(First=min(as.Date(DATECONSULT)), 
                                                                     Last=max(as.Date(DATECONSULT)))  %>%
  mutate(elapsed=as.numeric(Last-First)/30.5) %>% ungroup() %>% select(NUM, elapsed)


dataCohorteManaged_pats <- dataCohorteManaged_pats %>% left_join(visibility)
ClinicalTrialPats_pats <- ClinicalTrialPats_pats %>% left_join(visibility)

mean(dataCohorteManaged_pats$elapsed)
mean(ClinicalTrialPats_pats$elapsed)


# Overall
mean(dataCohorteManaged_pats$elapsed, na.rm=T) ; sd(dataCohorteManaged_pats$elapsed, na.rm=T) # 23.33 , 26.06
median(dataCohorteManaged_pats$elapsed, na.rm=T) ; quantile(dataCohorteManaged_pats$elapsed, na.rm=T) # 15, 0-36
sum(!is.na(dataCohorteManaged_pats$elapsed)) ; sum(!is.na(dataCohorteManaged_pats$elapsed))/732 # 732, 100
#P
mean(dataCohorteManaged_pats$elapsed[dataCohorteManaged_pats$DIAG==1], na.rm=T) ; sd(dataCohorteManaged_pats$elapsed[dataCohorteManaged_pats$DIAG==1], na.rm=T) # 22.30 , 26.00
median(dataCohorteManaged_pats$elapsed[dataCohorteManaged_pats$DIAG==1], na.rm=T) ; quantile(dataCohorteManaged_pats$elapsed[dataCohorteManaged_pats$DIAG==1], na.rm=T) # 14, 0-32
#C
mean(dataCohorteManaged_pats$elapsed[dataCohorteManaged_pats$DIAG==2], na.rm=T) ; sd(dataCohorteManaged_pats$elapsed[dataCohorteManaged_pats$DIAG==2], na.rm=T) # 25.54 , 26.10
median(dataCohorteManaged_pats$elapsed[dataCohorteManaged_pats$DIAG==2], na.rm=T) ; quantile(dataCohorteManaged_pats$elapsed[dataCohorteManaged_pats$DIAG==2], na.rm=T) # 21, 0-40

wilcox.test(
  na.omit(dataCohorteManaged_pats$elapsed[dataCohorteManaged_pats$DIAG==1]), 
  na.omit(dataCohorteManaged_pats$elapsed[dataCohorteManaged_pats$DIAG==2])
)


# Overall CT
mean(ClinicalTrialPats_pats$elapsed, na.rm=T) ; sd(ClinicalTrialPats_pats$elapsed, na.rm=T) # 30.56 , 29.63
median(ClinicalTrialPats_pats$elapsed, na.rm=T) ; quantile(ClinicalTrialPats_pats$elapsed, na.rm=T) # 24, 8-47
sum(!is.na(ClinicalTrialPats_pats$elapsed)) ; sum(!is.na(ClinicalTrialPats_pats$elapsed))/338 # 100
#P
mean(ClinicalTrialPats_pats$elapsed[ClinicalTrialPats_pats$DIAG==1], na.rm=T) ; sd(ClinicalTrialPats_pats$elapsed[ClinicalTrialPats_pats$DIAG==1], na.rm=T) # 30.16 , 29.20
median(ClinicalTrialPats_pats$elapsed[ClinicalTrialPats_pats$DIAG==1], na.rm=T) ; quantile(ClinicalTrialPats_pats$elapsed[ClinicalTrialPats_pats$DIAG==1], na.rm=T) # 24, 9-46
#C
mean(ClinicalTrialPats_pats$elapsed[ClinicalTrialPats_pats$DIAG==2], na.rm=T) ; sd(ClinicalTrialPats_pats$elapsed[ClinicalTrialPats_pats$DIAG==2], na.rm=T) # 31.42 , 30.65
median(ClinicalTrialPats_pats$elapsed[ClinicalTrialPats_pats$DIAG==2], na.rm=T) ; quantile(ClinicalTrialPats_pats$elapsed[ClinicalTrialPats_pats$DIAG==2], na.rm=T) # 25, 7-49

wilcox.test(
  na.omit(ClinicalTrialPats_pats$elapsed[ClinicalTrialPats_pats$DIAG==1]), 
  na.omit(ClinicalTrialPats_pats$elapsed[ClinicalTrialPats_pats$DIAG==2])
)

# --------------------------------------

# Annual changes -------------------------------------------------

dataCohorteManaged <- readRDS("Source/dataCohorteManaged.rds")
setDT(dataCohorteManaged)
dataCohorteManaged[, DIAGNIV := ifelse(is.na(DIAGNIV), 1, DIAGNIV)]
names(dataCohorteManaged)

ClinicalTrialPats <- fread("Source/ClinicalTrialPats.txt")
ClinicalTrialPats <- data.table(ClinicalTrialPats)[dataCohorteManaged, on = "NUM", nomatch = 0]

dataCohorteManaged %>% group_by(NUM) %>% count() %>% filter(n>1) %>%
  select(NUM) %>% distinct() %>% ungroup() %>%
  left_join(
    dataCohorteManaged
  ) %>% 
  select(NUM, DATE_VISITE0, DATECONSULT, TIME_SYMPT, UMSARS1_TOT) %>%
  group_by(NUM) %>% slice(1:2) %>%
  mutate(elapsed =  time_length(difftime(DATECONSULT, lag(DATECONSULT)), "years") ) %>%
  mutate(change =  UMSARS1_TOT - lag(UMSARS1_TOT) ) %>% 
  ungroup() %>%
  drop_na() %>%
  summarise(mean_change=mean(change/elapsed))


# --------------------------------
# Annual changes between 1st and 2nd visit only -------------------------------------------------

dataCohorteManaged <- readRDS("Source/dataCohorteManaged.rds")
setDT(dataCohorteManaged)
dataCohorteManaged[, DIAGNIV := ifelse(is.na(DIAGNIV), 1, DIAGNIV)]

ClinicalTrialPats <- fread("Source/ClinicalTrialPats.txt")
ClinicalTrialPats <- data.table(ClinicalTrialPats)[dataCohorteManaged, on = "NUM", nomatch = 0]



calculate_mean_change <- function(data, variable) {
  
  data <- data.table(dataCohorteManaged)
  
  multiple_entries <- data[, .N, by = .(NUM)][N > 1, .(NUM)]
  
  filtered_data <- data[multiple_entries, on = "NUM", nomatch = 0]
  
  arranged_data <- filtered_data[order(NUM, DATECONSULT), .(NUM, DATE_VISITE0, DATECONSULT, TIME_STUDY, get(variable))]
  
  names(arranged_data)[5] <- eval(variable)
  
  selected_rows <- arranged_data[, .SD[1:2], by = NUM]
  
  selected_rows <- selected_rows[, .(elapsed = as.numeric(difftime(DATECONSULT, 
                                                                   lag(DATECONSULT)), units = "days") / 365.25 ,
                                     change = get(variable) - lag(get(variable))), by = NUM]
  
  selected_rows <- na.omit(selected_rows)
  
  result <- selected_rows[, .(mean_change = mean(change / elapsed, na.rm = TRUE))]
  
  print(length(unique(selected_rows$NUM)))
  
  return(result)
}




calculate_sd_change <- function(data, variable) {
  
  data <- data.table(dataCohorteManaged)
  
  multiple_entries <- data[, .N, by = .(NUM)][N > 1, .(NUM)]
  
  filtered_data <- data[multiple_entries, on = "NUM", nomatch = 0]
  
  arranged_data <- filtered_data[order(NUM, DATECONSULT), .(NUM, DATE_VISITE0, DATECONSULT, TIME_STUDY, get(variable))]
  
  names(arranged_data)[5] <- eval(variable)
  
  selected_rows <- arranged_data[, .SD[1:2], by = NUM]
  
  selected_rows <- selected_rows[, .(elapsed = as.numeric(difftime(DATECONSULT, 
                                                                   lag(DATECONSULT)), units = "days") / 365.25 ,
                                     change = get(variable) - lag(get(variable))), by = NUM]
  
  
  selected_rows <- na.omit(selected_rows)
  
  result <- selected_rows[, .(sd_change = sd(change / elapsed, na.rm = TRUE))]
  
  print(length(unique(selected_rows$NUM)))
  
  return(result)
  
}



# dataCohorteManaged %>% select(contains("UMSARS1")) 1  12
# dataCohorteManaged %>% select(contains("UMSARS2")) 1  14


calculate_mean_change(dataCohorteManaged, "UMSARS1_TOT") # 6.351315 # 469
calculate_sd_change(dataCohorteManaged, "UMSARS1_TOT") # 21.07343 # 469

calculate_mean_change(dataCohorteManaged, "UMSARS1_1") # 0.569671 
calculate_sd_change(dataCohorteManaged, "UMSARS1_1") # 2.56443

calculate_mean_change(dataCohorteManaged, "UMSARS1_2") # 0.555052 
calculate_sd_change(dataCohorteManaged, "UMSARS1_2") # 1.818505 

calculate_mean_change(dataCohorteManaged, "UMSARS1_3") # 0.5985834 
calculate_sd_change(dataCohorteManaged, "UMSARS1_3") # 2.663461 

calculate_mean_change(dataCohorteManaged, "UMSARS1_4") # 0.6548055 
calculate_sd_change(dataCohorteManaged, "UMSARS1_4") # 1.814806 

calculate_mean_change(dataCohorteManaged, "UMSARS1_5") # 0.8134834 
calculate_sd_change(dataCohorteManaged, "UMSARS1_5") # 1.715658 

calculate_mean_change(dataCohorteManaged, "UMSARS1_6") # 0.6688437 
calculate_sd_change(dataCohorteManaged, "UMSARS1_6") # 1.689408 

calculate_mean_change(dataCohorteManaged, "UMSARS1_7") # 0.5854466 
calculate_sd_change(dataCohorteManaged, "UMSARS1_7") # 2.507582 

calculate_mean_change(dataCohorteManaged, "UMSARS1_8") # 0.5751491 
calculate_sd_change(dataCohorteManaged, "UMSARS1_8") # 3.781044 

calculate_mean_change(dataCohorteManaged, "UMSARS1_9") # 0.2475668 
calculate_sd_change(dataCohorteManaged, "UMSARS1_9") # 2.877324 

calculate_mean_change(dataCohorteManaged, "UMSARS1_10") # 0.3895957 
calculate_sd_change(dataCohorteManaged, "UMSARS1_10") # 1.912053 

calculate_mean_change(dataCohorteManaged, "UMSARS1_11") # 0.3427763 
calculate_sd_change(dataCohorteManaged, "UMSARS1_11") # 2.132229 

calculate_mean_change(dataCohorteManaged, "UMSARS1_12") # 0.3179752 
calculate_sd_change(dataCohorteManaged, "UMSARS1_12") # 1.465573 








calculate_mean_change(dataCohorteManaged, "UMSARS2_TOT") # 6.162232 # 477
calculate_sd_change(dataCohorteManaged, "UMSARS2_TOT") # 23.88615 # 477

calculate_mean_change(dataCohorteManaged, "UMSARS2_1") # 0.4355156 
calculate_sd_change(dataCohorteManaged, "UMSARS2_1") # 1.596423 

calculate_mean_change(dataCohorteManaged, "UMSARS2_2") # 0.4899938 
calculate_sd_change(dataCohorteManaged, "UMSARS2_2") # 2.456193 

calculate_mean_change(dataCohorteManaged, "UMSARS2_3") # 0.1481903 
calculate_sd_change(dataCohorteManaged, "UMSARS2_3") # 1.677974 

calculate_mean_change(dataCohorteManaged, "UMSARS2_4") # 0.2302359 
calculate_sd_change(dataCohorteManaged, "UMSARS2_4") # 1.666864 

calculate_mean_change(dataCohorteManaged, "UMSARS2_5") # 0.1602097 
calculate_sd_change(dataCohorteManaged, "UMSARS2_5") # 1.72882 

calculate_mean_change(dataCohorteManaged, "UMSARS2_6") # 0.2496756 
calculate_sd_change(dataCohorteManaged, "UMSARS2_6") # 1.667659 

calculate_mean_change(dataCohorteManaged, "UMSARS2_7") # 0.3600315 
calculate_sd_change(dataCohorteManaged, "UMSARS2_7") # 2.571654 

calculate_mean_change(dataCohorteManaged, "UMSARS2_8") # 0.5219486 
calculate_sd_change(dataCohorteManaged, "UMSARS2_8") # 3.599404 

calculate_mean_change(dataCohorteManaged, "UMSARS2_9") # 0.5173146 
calculate_sd_change(dataCohorteManaged, "UMSARS2_9") # 2.627202 

calculate_mean_change(dataCohorteManaged, "UMSARS2_10") # 0.3977415 
calculate_sd_change(dataCohorteManaged, "UMSARS2_10") # 1.96035 

calculate_mean_change(dataCohorteManaged, "UMSARS2_11") # 0.8908961 
calculate_sd_change(dataCohorteManaged, "UMSARS2_11") # 2.330879 

calculate_mean_change(dataCohorteManaged, "UMSARS2_12") # 0.585813 
calculate_sd_change(dataCohorteManaged, "UMSARS2_12") # 4.662676 

calculate_mean_change(dataCohorteManaged, "UMSARS2_13") # 0.5614601 
calculate_sd_change(dataCohorteManaged, "UMSARS2_13") # 2.170059 

calculate_mean_change(dataCohorteManaged, "UMSARS2_14") # 0.6123598 
calculate_sd_change(dataCohorteManaged, "UMSARS2_14") # 2.41617 





calculate_mean_change(dataCohorteManaged, "UMSARS1and2_TOT") # 12.57423  # 461
calculate_sd_change(dataCohorteManaged, "UMSARS1and2_TOT") # 43.88907 # 461



calculate_mean_change(dataCohorteManaged, "UMSARS4") # 0.6015983  # 470
calculate_sd_change(dataCohorteManaged, "UMSARS4") # 1.032175 # 470

calculate_mean_change(dataCohorteManaged, "PAD_COU") # -1.411116  # 402
calculate_sd_change(dataCohorteManaged, "PAD_COU") # 24.39399 # 402

calculate_mean_change(dataCohorteManaged, "PAS_COU") # -2.156203  # 402
calculate_sd_change(dataCohorteManaged, "PAS_COU") # 38.68916 # 402

calculate_mean_change(dataCohorteManaged, "deltaPAD") # -2.359843  # 399
calculate_sd_change(dataCohorteManaged, "deltaPAD") # 33.40441 # 399

calculate_mean_change(dataCohorteManaged, "deltaPAS") # -0.8558498  # 399
calculate_sd_change(dataCohorteManaged, "deltaPAS") # 42.69922 # 399







# --------------------------------
# Overall mortality ---------------------------



library(tidyverse)
library(data.table)
library(lubridate)

dataCohorteManaged <- readRDS("Source/dataCohorteManaged.rds")
setDT(dataCohorteManaged)
dataCohorteManaged[, DIAGNIV := ifelse(is.na(DIAGNIV), 1, DIAGNIV)]

df_id <- dataCohorteManaged %>% select(NUM, TIME_STUDY, DC, DATECONSULT, DATEDC) %>%
  mutate(DATECONSULT=as.numeric(DATECONSULT)) %>%
  group_by(NUM) %>% mutate(min=min(DATECONSULT)) %>% mutate(DATECONSULT= (DATECONSULT-min)/365.5) %>%
  group_by(NUM) %>% mutate(DATEDC=as.numeric(DATEDC)) %>% mutate(DATEDC= (DATEDC-min)/365.5) %>%
  select(-min)


df_id <- df_id %>% ungroup() %>% select(NUM) %>% distinct() %>%
  left_join(
    df_id %>% ungroup() %>% select(NUM, DATEDC) %>% group_by(NUM) %>% filter(DATEDC==max(DATEDC)) %>% distinct()
  ) %>%
  left_join(
    df_id %>% ungroup() %>% select(NUM, DC) %>% group_by(NUM) %>% filter(DC==max(DC)) %>% distinct()
  ) %>%
  left_join(
    df_id %>% ungroup() %>% select(NUM, TIME_STUDY) %>% group_by(NUM) %>% filter(TIME_STUDY==max(TIME_STUDY, na.rm = T)) %>% distinct()
  ) %>% 
  left_join(
    dataCohorteManaged %>% group_by(NUM) %>% filter(DATECONSULT==min(DATECONSULT)) %>% select(NUM, DIAG, DIAGNIV, AGE_VISITE0, SEXE, UMSARS1and2_TOT, deltaPAD, deltaPAS, PAD_COU, PAS_COU, UMSARS4 )
  ) %>%
  mutate(DATEDC = ifelse(is.na(DATEDC), TIME_STUDY, DATEDC))

library(survival)
library(survminer)

# + deltaPAD + deltaPAS + PAS_COU + PAD_COU + UMSARS4

coxFit <- coxph(Surv(DATEDC, DC) ~ as.factor(DIAG) +  as.factor(DIAGNIV) + 
                  as.factor(SEXE) + AGE_VISITE0 + UMSARS1and2_TOT , 
                data = df_id, x = TRUE, model=TRUE)

summary(coxFit)


sfit1 <- survfit(Surv(DATEDC, DC)~1, data=df_id)
sfit1
summary(sfit1)

ggsurvplot(sfit1)

ggsurvplot(sfit1, conf.int=TRUE,
           fun = "pct",
           risk.table=TRUE,  
           surv.median.line = "hv" ,  
           palette=c("deepskyblue4"), colour="deepskyblue4", 
           title="Kaplan-Meier Curve for Survival \n Entire French MSA Cohort", 
           risk.table.height=0.2)


ggsurvplot(sfit1, conf.int=TRUE,
           fun = "cumhaz",
           risk.table=TRUE, 
           surv.median.line = "hv" ,  
           palette=c("deepskyblue4"), colour="deepskyblue4", 
           title="Cumulative Hazard \n Entire French MSA Cohort", 
           risk.table.height=0.2)


sfit2 <- survfit(Surv(DATEDC, DC)~DIAGNIV, data=df_id)
sfit2
plot(sfit2, fun = "cumhaz") # proportional hazards (PH) assumption is satisfied
summary(sfit2)

survdiff(Surv(DATEDC, DC) ~ DIAGNIV, data = df_id, rho = 1) # 2e-09 

ggsurvplot(sfit2)

ggsurvplot(sfit2, conf.int=TRUE, pval=TRUE, pval.method = TRUE, risk.table=TRUE, 
           surv.median.line = "hv" ,
           legend.labs=c("1", "2"), legend.title="Possible MSA (1) vs Probable MSA (2)",  
           palette=c("midnightblue", "firebrick"), 
           title="Kaplan-Meier Curve for Survival \n by MSA Diagnostic Certitude", 
           risk.table.height=0.2)


ggsurvplot(sfit2, conf.int=TRUE, pval=TRUE, pval.method = TRUE, 
           fun="cumhaz",
           risk.table=TRUE, 
           surv.median.line = "hv" ,
           legend.labs=c("1", "2"), legend.title="Possible MSA (1) vs Probable MSA (2)",  
           palette=c("midnightblue", "firebrick"), 
           title="Cumulative Hazard \n by MSA Diagnostic Certitude", 
           risk.table.height=0.2)


sfit3 <- survfit(Surv(DATEDC, DC)~DIAG, data=df_id)
sfit3
summary(sfit3)
plot(sfit3, fun = "cumhaz") # proportional hazards (PH) assumption NOT satisfied

summary(coxph(Surv(DATEDC, DC) ~ DIAG, data = df_id, x = TRUE)) 

survdiff(Surv(DATEDC, DC) ~ DIAG, data = df_id, rho = 1) #0.01 

ggsurvplot(sfit3)


ggsurvplot(sfit3, conf.int=TRUE, pval=TRUE, pval.method = TRUE,
           risk.table=TRUE, 
           surv.median.line = "hv" ,
           legend.labs=c("1", "2"), legend.title="MSA-P (1) vs MSA-C (2)",  
           palette=c("midnightblue", "firebrick"), 
           title="Kaplan-Meier Curve for Survival \n by MSA Clinical Phenotype ", 
           risk.table.height=0.3)


ggsurvplot(sfit3, conf.int=TRUE, pval=TRUE, pval.method = TRUE,
           risk.table=TRUE,     fun="cumhaz",
           surv.median.line = "hv" ,
           legend.labs=c("1", "2"), legend.title="MSA-P (1) vs MSA-C (2)",  
           palette=c("midnightblue", "firebrick"), 
           title="Cumulative Hazard \n by MSA Clinical Phenotype ", 
           risk.table.height=0.3)




# -----------------------

# Joint modeling -----------------------------

library(tidyverse)
library(data.table)
library(lubridate)

dataCohorteManaged <- readRDS("Source/dataCohorteManaged.rds")
setDT(dataCohorteManaged)
dataCohorteManaged[, DIAGNIV := ifelse(is.na(DIAGNIV), 1, DIAGNIV)]

df <- dataCohorteManaged[ , c("NUM", "TIME_SYMPT", "UMSARS1and2_TOT")]
df <- na.omit(df)
length(unique(df$NUM))

df_id <- dataCohorteManaged %>% select(NUM, TIME_SYMPT, DC, DATECONSULT, DATEDC) %>%
  mutate(DATECONSULT=as.numeric(DATECONSULT)) %>%
  group_by(NUM) %>% mutate(min=min(DATECONSULT)) %>% mutate(DATECONSULT= (DATECONSULT-min)/365.5) %>%
  group_by(NUM) %>% mutate(DATEDC=as.numeric(DATEDC)) %>% mutate(DATEDC= (DATEDC-min)/365.5) %>%
  select(-min)


df_id <- df_id %>% ungroup() %>% select(NUM) %>% distinct() %>%
  left_join(
    df_id %>% ungroup() %>% select(NUM, DATEDC) %>% group_by(NUM) %>% filter(DATEDC==max(DATEDC, na.rm = T)) %>% distinct()
  ) %>%
  left_join(
    df_id %>% ungroup() %>% select(NUM, DC) %>% group_by(NUM) %>% filter(DC==max(DC, na.rm = T)) %>% distinct()
  ) %>%
  left_join(
    df_id %>% ungroup() %>% select(NUM, TIME_SYMPT) %>% group_by(NUM) %>% filter(TIME_SYMPT==max(TIME_SYMPT, na.rm = T)) %>% distinct()
  ) %>% 
  left_join(
    dataCohorteManaged %>% group_by(NUM) %>% filter(DATECONSULT==max(DATECONSULT)) %>% select(NUM, DIAG, DIAGNIV)
  ) %>%
  mutate(DATEDC = ifelse(is.na(DATEDC), TIME_SYMPT, DATEDC))

df_id <- df %>% select(NUM) %>% inner_join(df_id) %>% distinct() %>% select(NUM, DATEDC, DC)

df <- df %>% left_join(df_id %>% select(NUM, DATEDC, DC))

sum(is.na(df))

df <- df %>% mutate(DATEDC=ifelse(DATEDC==TIME_SYMPT, DATEDC+0.01, DATEDC))

df_id <- df %>% group_by(NUM) %>% filter(DATEDC==max(DATEDC)) %>%  select(NUM, DATEDC, DC) %>% distinct() 


ggplot(df, aes(x = TIME_SYMPT, y = UMSARS1and2_TOT)) +
  geom_line(aes(group=NUM), col="deepskyblue4" , alpha=0.3) +
  geom_jitter(size=0.1, colour="deepskyblue4", alpha=0.7) +
  stat_smooth(method="gam", col="firebrick", fill="firebrick", alpha=0.3, lwd=1.5, se=TRUE, formula = y ~ s(x, bs = "cs", k =6))+
  scale_x_continuous(name = "Time from Symptom Onset (Years)") +
  scale_y_continuous(name = "UMSARS I + II",limits=c(0,100)) +
  theme_minimal() 



library(nlme)
library(JM)
library(survival)



lmeFit.p1 <- lme(UMSARS1and2_TOT    ~ 1 , data = df,
                 random = ~ 1 | NUM)  

summary(lmeFit.p1)
# (10.74232 ^2) / (  (10.74232 ^2) + (14.01453^2)) -> 0.3700955
intervals(lmeFit.p1)



lmeFit.p2 <- lme(UMSARS1and2_TOT    ~ TIME_SYMPT , data = df,
                 random = ~ 1 | NUM)  

summary(lmeFit.p2)
# (15.65778  ^2) / (  (15.65778  ^2) + (8.594463^2)) -> 0.7684713
intervals(lmeFit.p2)



lmeFit.p3 <- lme(UMSARS1and2_TOT    ~ TIME_SYMPT  , data = df,
                 random = ~ TIME_SYMPT | NUM)  


summary(lmeFit.p3)

#  (19.044315^2) / (  (19.044315^2) + (5.876712^2)) -> 0.9130567
intervals(lmeFit.p3)

anova(lmeFit.p2, lmeFit.p3)

# Model df      AIC      BIC    logLik   Test  L.Ratio p-value
# lmeFit.p2     1  4 16863.11 16885.78 -8427.555                        
# lmeFit.p3     2  6 16374.86 16408.87 -8181.429 1 vs 2 492.2525  <.0001




survFit.p1 <- coxph(Surv(DATEDC, DC) ~ 1, data = df_id, x = TRUE)  

summary(survFit.p1)


jointFit.p1 <- jointModel(lmeFit.p3, survFit.p1, timeVar = "TIME_SYMPT",
                          method = "piecewise-PH-aGH")

summary(jointFit.p1)








# dataP_1ARCOL0648 <- df[df$NUM == "1BEFRA0550", ]
# 
# len_id <- nrow(dataP_1ARCOL0648)
# 
# sfit3 <- survfitJM(jointFit.p1, newdata = dataP_1ARCOL0648[1:3, ], idVar = "NUM") 
# sfit4 <- survfitJM(jointFit.p1, newdata = dataP_1ARCOL0648[1:6, ], idVar = "NUM") 
# 
# par(mfrow=c(1,2))
# plotfit3 <- plot(sfit3, estimator="mean", include.y = TRUE, conf.int=0.95, fill.area=TRUE, col.area="lightblue", main="Patient 1ARCOL0648")
# plotfit4 <- plot(sfit4, estimator="mean", include.y = TRUE, conf.int=0.95, fill.area=TRUE, col.area="lightblue", main="Patient 1ARCOL0648")
# 
# 
# library(animation)
# 
# saveGIF({
#   for(i in c(1:len_id)){
#     sfit <- survfitJM(jointFit.p1, newdata = dataP_1ARCOL0648[1:i, ], idVar = "NUM") 
#     plot(sfit, estimator="mean", include.y = TRUE, conf.int=0.95, fill.area=TRUE, col.area="lightblue", main="Patient 1ARCOL0648")
#     
#   }
# },ani.width = 400, ani.height=400)



# ---------------------



