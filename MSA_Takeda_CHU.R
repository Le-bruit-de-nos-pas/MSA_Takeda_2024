
library(tidyverse)
library(data.table)
options(scipen = 999)


# Import data ------------------------------------------------

dataCohorteManaged <- readRDS("Source/dataCohorteManaged.rds")

names(dataCohorteManaged)

length(unique(dataCohorteManaged$NUM)) # 732 patients

"#F8B195"   "#F67280"   "#C06C84"   "#6C5B7B"   "#355C7D" 

setDT(dataCohorteManaged)

sum(is.na(dataCohorteManaged) ) / 
  ( dim(dataCohorteManaged)[1] * dim(dataCohorteManaged)[2] ) # 0.3324094 data missing

library(naniar)

vis_miss(dataCohorteManaged)

gg_miss_upset(dataCohorteManaged)







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



# Mortality penetrance --------------------------------------------------

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

wilcox.test(dataCohorteManaged$UMSARS1_TOT[dataCohorteManaged$DIAG==1], 
            dataCohorteManaged$UMSARS1_TOT[dataCohorteManaged$DIAG==2])

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


# ---------------------------------------------