
library(tidyverse)
library(data.table)
library(lubridate)
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

graph <- graph[, .(n = .N), by = n_visits]

graph %>%
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
