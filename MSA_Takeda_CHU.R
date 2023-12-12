
library(tidyverse)
library(data.table)
options(scipen = 999)


# Import data ------------------------------------------------

dataCohorteManaged <- readRDS("Source/dataCohorteManaged.rds")

names(dataCohorteManaged)

length(unique(dataCohorteManaged$NUM)) # 732

"#F8B195"   "#F67280"   "#C06C84"   "#6C5B7B"   "#355C7D" 

setDT(dataCohorteManaged)


gather(dataCohorteManaged, Var, Val, DATENAIS:deltaPAS ) %>%
  filter(Val== ""|is.na(Val))


sum(dataCohorteManaged =="")

sum(is.na(dataCohorteManaged) ) / 
  ( dim(dataCohorteManaged)[1] * dim(dataCohorteManaged)[2] ) # 0.3324094




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
  ggplot(aes(elapsed/30.5)) +
  geom_density(fill="#355C7D", colour="#355C7D", alpha=0.8) +
  theme_minimal() + 
  xlab("\n Number of elapsed months \n between first and last visit/evaluation") + 
  ylab("Patient density \n (kernel smoothing) \n")

temp %>% select(NUM) %>% distinct() %>% mutate(rownum=row_number()) %>%
  left_join(temp %>% select(NUM, delai_consult_vis0)) %>%
  ggplot(aes(delai_consult_vis0*12, as.factor(rownum))) +
  geom_jitter(size=1, alpha=0.5) +
  theme(axis.text.y = element_text(size = 0.2))  +
  xlab("\n Number of elapsed months \n between subsequent evaluations") + ylab("Patient ID \n") 
  
# ----------------------------------------------------------------