library(data.table)
library(tidyverse)
library(readxl)
library(lubridate)
library(PMCMRplus)  


# ETP Cohort summary --------------------------------- 

# Visites *****************

Visites <- read_excel(path = "data/BDD_ETP_AMS_7122023.xlsx", sheet="Visites")

Visites <- Visites %>% select(num_patient, num_visite, date_visite, visite_realisee) %>%
  mutate(date_visite=as.Date(date_visite))

Visites %>% filter(visite_realisee==1) %>%
  select(num_patient, num_visite, visite_realisee) %>% distinct() %>%
  spread(key=num_visite, value=visite_realisee) 

# 15 -> 14 -> 13



# Info_patient ************

Info_patient <- read_excel(path = "data/BDD_ETP_AMS_7122023.xlsx", sheet="Info_patient")

Info_patient <- Info_patient %>% select(num_patient, sexe, annee_naissance, age_inclusion)

Info_patient %>% group_by(sexe) %>% count() #10H 5F

Info_patient %>%  summarise(mean=mean(age_inclusion), se=sd(age_inclusion),
            median=median(age_inclusion),
            q25=quantile(age_inclusion, 0.25),
            q75=quantile(age_inclusion, 0.75))

#    mean    se median   q25   q75
# 1    65  6.08     64    61    69



# Histoire_AMS ************

Histoire_AMS <- read_excel(path = "data/BDD_ETP_AMS_7122023.xlsx", sheet="Histoire_AMS")

Histoire_AMS <- Histoire_AMS %>% select(num_patient, AMS_C, AMS_P, AMS_cliniquement_etablie, AMS_cliniquement_possible, AMS_date_diag_an)

Histoire_AMS %>% group_by(AMS_C) %>% count() #3C
Histoire_AMS %>% group_by(AMS_P) %>% count() #12P

Histoire_AMS %>% group_by(AMS_cliniquement_etablie) %>% count() #10 possible
Histoire_AMS %>% group_by(AMS_cliniquement_possible) %>% count() #5 probable


Histoire_AMS %>% select(num_patient, AMS_date_diag_an) %>%
  inner_join(Visites %>% filter(num_visite=="V0") %>% select(num_patient, date_visite)) %>%
  mutate(date_visite=as.character(date_visite)) %>%
  mutate(date_visite=str_sub(date_visite,1L, 4L)) %>% mutate(diff=12*(as.numeric(date_visite)-AMS_date_diag_an )) %>%
    summarise(mean=mean(diff), se=sd(diff),
            median=median(diff),
            q25=quantile(diff, 0.25),
            q75=quantile(diff, 0.75))

#    mean    se median   q25   q75
# 1   7.2  7.59     12     0    12



# UMSARS1 *****************

UMSARS1 <- read_excel(path = "data/BDD_ETP_AMS_7122023.xlsx", sheet="UMSARS1")
UMSARS1 <- UMSARS1 %>% filter(UMSARS1_realise==1) %>% select(num_patient, num_visite, UMSARS1_total)

UMSARS1 <- UMSARS1 %>% ungroup() %>%  mutate(UMSARS1_total=as.numeric(UMSARS1_total)) %>% drop_na()

UMSARS1 %>% 
  group_by(num_visite) %>%  
  summarise(mean=mean(UMSARS1_total), se=sd(UMSARS1_total),
            median=median(UMSARS1_total),
            q25=quantile(UMSARS1_total, 0.25),
            q75=quantile(UMSARS1_total, 0.75))

#   num_visite  mean    se median   q25   q75
# 1 V0          20.2  6.47     18  16    24  
# 2 V1          22.9  6.94     21  18.2  26.2
# 3 V2          25.5  8.07     25  21    31 



# 500 #500
UMSARS1 %>% 
  ggplot(aes(num_visite , UMSARS1_total, colour=num_visite , fill=num_visite )) +
  geom_boxplot(alpha=0.7, notch = TRUE, width = 0.5, outlier.shape = NA) +
  geom_jitter(size=3,shape=1,stroke=2.5, width=0.3 ) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 10, vjust = -0.5),
        axis.title.y = element_text(size = 10, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_color_manual(values=c("#baebff", "#69adc6", "#283f60")) +
  scale_fill_manual(values=c("#baebff", "#69adc6", "#283f60")) +
  xlab("\n Visit Number") +
  ylab("UMSARS 1 Total \n [Patient] \n") +
  ylim(0, 50)



# Step 1: Filter patients who have all three visits (V0, V1, V2)
UMSARS1 <- UMSARS1 %>%
  group_by(num_patient) %>% count() %>% filter(n==3) %>% select(num_patient) %>% ungroup() %>%
  left_join(UMSARS1)

length(unique(UMSARS1$num_patient))  # 13


UMSARS1 %>% group_by(num_visite) %>%  
  summarise(mean=mean(UMSARS1_total), se=sd(UMSARS1_total),
            median=median(UMSARS1_total),
            q25=quantile(UMSARS1_total, 0.25),
            q75=quantile(UMSARS1_total, 0.75))

#   num_visite  mean    se median   q25   q75
# 1 V0          19.2  6.34     17    16    19
# 2 V1          22.2  6.68     21    18    24
# 3 V2          25.5  8.07     25    21    31


# Step 2: Reshape the data to wide format for Friedman test
UMSARS1 <- UMSARS1 %>% 
  tidyr::pivot_wider(names_from = num_visite, values_from = UMSARS1_total )

# Step 3: Friedman Test
friedman_result <- friedman.test(as.matrix(UMSARS1[, c("V0", "V1", "V2")]))
print(friedman_result)

# Friedman rank sum test
# 
# data:  as.matrix(UMSARS1[, c("V0", "V1", "V2")])
# Friedman chi-squared = 10.042, df = 2, p-value = 0.006599

# Step 4: Post-hoc test (Pairwise comparisons using Nemenyi test)
data_matrix <- as.matrix(UMSARS1[, c("V0", "V1", "V2")])
rownames(data_matrix) <- UMSARS1$num_patient  # Assign patient IDs as row names

posthoc_result <- frdAllPairsNemenyiTest(data_matrix)
print(posthoc_result)

#    V0     V1    
# V1 0.3552 -     
# V2 0.0067 0.2180
# 
# P value adjustment method: single-step







# UMSARS2 *****************

UMSARS2 <- read_excel(path = "data/BDD_ETP_AMS_7122023.xlsx", sheet="UMSARS2")
UMSARS2 <- UMSARS2 %>% filter(UMSARS2_realise ==1) %>% select(num_patient, num_visite, UMSARS2_total)

data.frame(UMSARS2)

UMSARS2 <- UMSARS2 %>% ungroup() %>%  mutate(UMSARS2_total=as.numeric(UMSARS2_total)) %>% drop_na()

UMSARS2 %>% 
  group_by(num_visite) %>%  
  summarise(mean=mean(UMSARS2_total), se=sd(UMSARS2_total),
            median=median(UMSARS2_total),
            q25=quantile(UMSARS2_total, 0.25),
            q75=quantile(UMSARS2_total, 0.75))

#   num_visite  mean    se median   q25   q75
# 1 V0          19.3  9.00     19  12.5  23  
# 2 V1          20.2  9.86     18  13    24  
# 3 V2          26   10.8      22  18.8  34.8



# 500 #500
UMSARS2 %>% 
  ggplot(aes(num_visite , UMSARS2_total, colour=num_visite , fill=num_visite )) +
  geom_boxplot(alpha=0.7, notch = TRUE, width = 0.5, outlier.shape = NA) +
  geom_jitter(size=3,shape=1,stroke=2.5, width=0.3 ) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 10, vjust = -0.5),
        axis.title.y = element_text(size = 10, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_color_manual(values=c("#baebff", "#69adc6", "#283f60")) +
  scale_fill_manual(values=c("#baebff", "#69adc6", "#283f60")) +
  xlab("\n Visit Number") +
  ylab("UMSARS 2 Total \n [Patient] \n") +
  ylim(0, 50)



# Step 1: Filter patients who have all three visits (V0, V1, V2)
UMSARS2 <- UMSARS2 %>%
  group_by(num_patient) %>% count() %>% filter(n==3) %>% select(num_patient) %>% ungroup() %>%
  left_join(UMSARS2)

length(unique(UMSARS2$num_patient))  # 11


UMSARS2 %>% group_by(num_visite) %>%  
  summarise(mean=mean(UMSARS2_total), se=sd(UMSARS2_total),
            median=median(UMSARS2_total),
            q25=quantile(UMSARS2_total, 0.25),
            q75=quantile(UMSARS2_total, 0.75))

#   num_visite  mean    se median   q25   q75
# 1 V0          17.1  7.73     17  12.5  20  
# 2 V1          19.8  9.87     18  13.5  23.5
# 3 V2          25   10.7      21  18.5  31.5


# Step 2: Reshape the data to wide format for Friedman test
UMSARS2 <- UMSARS2 %>% 
  tidyr::pivot_wider(names_from = num_visite, values_from = UMSARS2_total )

# Step 3: Friedman Test
friedman_result <- friedman.test(as.matrix(UMSARS2[, c("V0", "V1", "V2")]))
print(friedman_result)

# 	Friedman rank sum test
# 
# data:  as.matrix(UMSARS2[, c("V0", "V1", "V2")])
# Friedman chi-squared = 8.1395, df = 2, p-value = 0.01708



# Step 4: Post-hoc test (Pairwise comparisons using Nemenyi test)
data_matrix <- as.matrix(UMSARS2[, c("V0", "V1", "V2")])
rownames(data_matrix) <- UMSARS2$num_patient  # Assign patient IDs as row names

posthoc_result <- frdAllPairsNemenyiTest(data_matrix)
print(posthoc_result)

#   V0    V1   
# V1 0.855 -    
# V2 0.021 0.083









# UMSARS 1 + UMSARS2 *****************



UMSARS1 <- read_excel(path = "data/BDD_ETP_AMS_7122023.xlsx", sheet="UMSARS1")
UMSARS1 <- UMSARS1 %>% filter(UMSARS1_realise ==1) %>% select(num_patient, num_visite, UMSARS1_total)

data.frame(UMSARS1)

UMSARS1 <- UMSARS1 %>% ungroup() %>%  mutate(UMSARS1_total=as.numeric(UMSARS1_total)) %>% drop_na()



UMSARS2 <- read_excel(path = "data/BDD_ETP_AMS_7122023.xlsx", sheet="UMSARS2")
UMSARS2 <- UMSARS2 %>% filter(UMSARS2_realise ==1) %>% select(num_patient, num_visite, UMSARS2_total)

data.frame(UMSARS2)

UMSARS2 <- UMSARS2 %>% ungroup() %>%  mutate(UMSARS2_total=as.numeric(UMSARS2_total)) %>% drop_na()

UMSARS_total <- UMSARS1 %>% inner_join(UMSARS2)

UMSARS_total <- UMSARS_total %>% mutate(UMSARS=UMSARS1_total +UMSARS2_total) %>% select(-c(UMSARS1_total , UMSARS2_total))

UMSARS_total %>% 
  group_by(num_visite) %>%  
  summarise(mean=mean(UMSARS), se=sd(UMSARS),
            median=median(UMSARS),
            q25=quantile(UMSARS, 0.25),
            q75=quantile(UMSARS, 0.75))

#  num_visite  mean    se median   q25   q75
# 1 V0          39.5  15.2   36    27.5  47  
# 2 V1          42.4  16.3   40    31    47  
# 3 V2          51.6  18.7   44.5  39.5  66.2


# 500 #500
UMSARS_total %>% 
  ggplot(aes(num_visite , UMSARS, colour=num_visite , fill=num_visite )) +
  geom_boxplot(alpha=0.7, notch = TRUE, width = 0.5, outlier.shape = NA) +
  geom_jitter(size=3,shape=1,stroke=2.5, width=0.3 ) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 10, vjust = -0.5),
        axis.title.y = element_text(size = 10, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_color_manual(values=c("#baebff", "#69adc6", "#283f60")) +
  scale_fill_manual(values=c("#baebff", "#69adc6", "#283f60")) +
  xlab("\n Visit Number") +
  ylab("UMSARS 1+2 Total \n [Patient] \n") +
  ylim(0, 100)



# Step 1: Filter patients who have all three visits (V0, V1, V2)
UMSARS_total <- UMSARS_total %>%
  group_by(num_patient) %>% count() %>% filter(n==3) %>% select(num_patient) %>% ungroup() %>%
  left_join(UMSARS_total)

length(unique(UMSARS_total$num_patient))  # 11


UMSARS_total %>% group_by(num_visite) %>%  
  summarise(mean=mean(UMSARS), se=sd(UMSARS),
            median=median(UMSARS),
            q25=quantile(UMSARS, 0.25),
            q75=quantile(UMSARS, 0.75))

#   num_visite  mean    se median   q25   q75
# 1 V0          35.3  12.6     33  26    38.5
# 2 V1          41.5  16.0     40  32.5  46  
# 3 V2          49.9  18.6     44  39    59.5

# Step 2: Reshape the data to wide format for Friedman test
UMSARS_total <- UMSARS_total %>% 
  tidyr::pivot_wider(names_from = num_visite, values_from = UMSARS )

# Step 3: Friedman Test
friedman_result <- friedman.test(as.matrix(UMSARS_total[, c("V0", "V1", "V2")]))
print(friedman_result)

# 	Friedman rank sum test
# 
# data:  as.matrix(UMSARS_total[, c("V0", "V1", "V2")])
# Friedman chi-squared = 8.9091, df = 2, p-value = 0.01163




# Step 4: Post-hoc test (Pairwise comparisons using Nemenyi test)
data_matrix <- as.matrix(UMSARS_total[, c("V0", "V1", "V2")])
rownames(data_matrix) <- UMSARS_total$num_patient  # Assign patient IDs as row names

posthoc_result <- frdAllPairsNemenyiTest(data_matrix)
print(posthoc_result)

#    V0    V1   
# V1 0.905 -    
# V2 0.015 0.050

# QOL_AMS *****************

QOL_AMS <- read_excel(path = "data/BDD_ETP_AMS_7122023.xlsx", sheet="QOL_AMS")
QOL_AMS <- QOL_AMS %>% filter(QOL_AMS_realise==1) %>% select(num_patient, num_visite, QOL_AMS_total, QOL_AMS_echelle_anal)

data.frame(QOL_AMS)


QOL_AMS %>% ungroup() %>% group_by(num_visite) %>%  
  summarise(mean=mean(QOL_AMS_total), se=sd(QOL_AMS_total),
            median=median(QOL_AMS_total),
            q25=quantile(QOL_AMS_total, 0.25),
            q75=quantile(QOL_AMS_total, 0.75))

#   num_visite  mean    se median   q25   q75
# 1 V0          67.4  23.5   69    46.5  85  
# 2 V1          65.3  16.6   70.5  55.2  74.8
# 3 V2          75.2  27.1   74    64    89  

QOL_AMS %>% ungroup() %>% group_by(num_visite) %>%  
  summarise(mean=mean(QOL_AMS_echelle_anal), se=sd(QOL_AMS_echelle_anal),
            median=median(QOL_AMS_echelle_anal),
            q25=quantile(QOL_AMS_echelle_anal, 0.25),
            q75=quantile(QOL_AMS_echelle_anal, 0.75))

#   num_visite  mean    se median   q25   q75
# 1 V0          50.6  16.4     50  40    55  
# 2 V1          43.4  18.2     40  31.2  49.5
# 3 V2          39.9  15.2     40  29    50  


# 500 #500
QOL_AMS %>%
  ggplot(aes(num_visite , QOL_AMS_total, colour=num_visite , fill=num_visite )) +
  geom_boxplot(alpha=0.7, notch = TRUE, width = 0.5, outlier.shape = NA) +
  geom_jitter(size=3,shape=1,stroke=2.5, width=0.3 ) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 10, vjust = -0.5),
        axis.title.y = element_text(size = 10, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_color_manual(values=c("#baebff", "#69adc6", "#283f60")) +
  scale_fill_manual(values=c("#baebff", "#69adc6", "#283f60")) +
  xlab("\n Visit Number") +
  ylab("MSA QoL Total \n [Patient] \n") +
  ylim(0, 150)




# 500 #500
QOL_AMS %>%
  ggplot(aes(num_visite , QOL_AMS_echelle_anal, colour=num_visite , fill=num_visite )) +
  geom_boxplot(alpha=0.7, notch = TRUE, width = 0.5, outlier.shape = NA) +
  geom_jitter(size=3,shape=1,stroke=2.5, width=0.3 ) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 10, vjust = -0.5),
        axis.title.y = element_text(size = 10, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_color_manual(values=c("#baebff", "#69adc6", "#283f60")) +
  scale_fill_manual(values=c("#baebff", "#69adc6", "#283f60")) +
  xlab("\n Visit Number") +
  ylab("MSA QoL Total Analog \n [Patient] \n") +
  ylim(0, 100)


data.frame(QOL_AMS)

# Step 1: Filter patients who have all three visits (V0, V1, V2)
QOL_AMS <- QOL_AMS %>%
  group_by(num_patient) %>% count() %>% filter(n==3) %>% select(num_patient) %>% ungroup() %>%
  left_join(QOL_AMS)

length(unique(QOL_AMS$num_patient))  # 13


QOL_AMS %>% group_by(num_visite) %>%  
  summarise(mean=mean(QOL_AMS_total), se=sd(QOL_AMS_total),
            median=median(QOL_AMS_total),
            q25=quantile(QOL_AMS_total, 0.25),
            q75=quantile(QOL_AMS_total, 0.75))

#  num_visite  mean    se median   q25   q75
# 1 V0          64.5  24.0     65    46    76
# 2 V1          66.3  16.9     73    56    75
# 3 V2          75.2  27.1     74    64    89


QOL_AMS %>% group_by(num_visite) %>%  
  summarise(mean=mean(QOL_AMS_echelle_anal), se=sd(QOL_AMS_echelle_anal),
            median=median(QOL_AMS_echelle_anal),
            q25=quantile(QOL_AMS_echelle_anal, 0.25),
            q75=quantile(QOL_AMS_echelle_anal, 0.75))

#  num_visite  mean    se median   q25   q75
# 1 V0          48.5  15.6     40    40    50
# 2 V1          39.4  10.9     40    30    48
# 3 V2          39.9  15.2     40    29    50


# Step 2: Reshape the data to wide format for Friedman test
QOL_AMS_tot <- QOL_AMS %>% select(-QOL_AMS_echelle_anal) %>%
  tidyr::pivot_wider(names_from = num_visite, values_from = QOL_AMS_total )

QOL_AMS_analog <- QOL_AMS %>% select(-QOL_AMS_total ) %>%
  tidyr::pivot_wider(names_from = num_visite, values_from = QOL_AMS_echelle_anal )


# Step 3: Friedman Test
friedman_result <- friedman.test(as.matrix(QOL_AMS_tot[, c("V0", "V1", "V2")]))
print(friedman_result)

# 	Friedman rank sum test
# 
# data:  as.matrix(QOL_AMS_tot[, c("V0", "V1", "V2")])
# Friedman chi-squared = 4.3529, df = 2, p-value = 0.1134

friedman_result <- friedman.test(as.matrix(
  
# 	Friedman rank sum test
# 
# data:  as.matrix(QOL_AMS_analog[, c("V0", "V1", "V2")])
# Friedman chi-squared = 3.5111, df = 2, p-value = 0.1728


# Step 4: Post-hoc test (Pairwise comparisons using Nemenyi test)
data_matrix <- as.matrix(QOL_AMS_tot[, c("V0", "V1", "V2")])
rownames(data_matrix) <- QOL_AMS_tot$num_patient  # Assign patient IDs as row names

posthoc_result <- frdAllPairsNemenyiTest(data_matrix)
print(posthoc_result)

#    V0    V1   
# V1 0.651 -    
# V2 0.098 0.467

data_matrix <- as.matrix(QOL_AMS_analog[, c("V0", "V1", "V2")])
rownames(data_matrix) <- QOL_AMS_analog$num_patient  # Assign patient IDs as row names

posthoc_result <- frdAllPairsNemenyiTest(data_matrix)
print(posthoc_result)

#    V0   V1  
# V1 0.41 -   
# V2 0.22 0.92


# PDQ_QOL *****************

PDQ_QOL <- read_excel(path = "data/BDD_ETP_AMS_7122023.xlsx", sheet="PDQ_QOL")
PDQ_QOL <- PDQ_QOL %>% filter(PDQ_QOL_realise==1) %>% select(num_patient, num_visite, PDQ_QOL_item26_total)

data.frame(PDQ_QOL)



PDQ_QOL %>% group_by(num_visite) %>%  
  summarise(mean=mean(PDQ_QOL_item26_total), se=sd(PDQ_QOL_item26_total),
            median=median(PDQ_QOL_item26_total),
            q25=quantile(PDQ_QOL_item26_total, 0.25),
            q75=quantile(PDQ_QOL_item26_total, 0.75))

#   num_visite  mean    se median   q25   q75
# 1 V0          37.3  21.6     34  21.5    49
# 2 V1          41.9  28.6     28  21      62
# 3 V2          47.5  24.1     45  29.5    69


 #500 500
PDQ_QOL %>%
  ggplot(aes(num_visite , PDQ_QOL_item26_total, colour=num_visite , fill=num_visite )) +
  geom_boxplot(alpha=0.7, notch = TRUE, width = 0.5, outlier.shape = NA) +
  geom_jitter(size=3,shape=1,stroke=2.5, width=0.3 ) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 10, vjust = -0.5),
        axis.title.y = element_text(size = 10, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_color_manual(values=c("#baebff", "#69adc6", "#283f60")) +
  scale_fill_manual(values=c("#baebff", "#69adc6", "#283f60")) +
  xlab("\n Visit Number") +
  ylab("PDQ QoL Total \n [Caregiver] \n")



# Step 1: Filter patients who have all three visits (V0, V1, V2)
PDQ_QOL <- PDQ_QOL %>%
  group_by(num_patient) %>% count() %>% filter(n==3) %>% select(num_patient) %>% ungroup() %>%
  left_join(PDQ_QOL)

length(unique(PDQ_QOL$num_patient))  # 9


PDQ_QOL %>% group_by(num_visite) %>%  
  summarise(mean=mean(PDQ_QOL_item26_total), se=sd(PDQ_QOL_item26_total),
            median=median(PDQ_QOL_item26_total),
            q25=quantile(PDQ_QOL_item26_total, 0.25),
            q75=quantile(PDQ_QOL_item26_total, 0.75))

#   num_visite  mean    se median   q25   q75
# 1 V0          36.6  23.2     34    17    48
# 2 V1          41.9  28.6     28    21    62
# 3 V2          42.7  23.8     33    29    64


# Step 2: Reshape the data to wide format for Friedman test
PDQ_QOL <- PDQ_QOL %>%
  tidyr::pivot_wider(names_from = num_visite, values_from = PDQ_QOL_item26_total)

# Step 3: Friedman Test
friedman_result <- friedman.test(as.matrix(PDQ_QOL[, c("V0", "V1", "V2")]))
print(friedman_result)

# 	Friedman rank sum test
# 
# data:  as.matrix(PDQ_QOL[, c("V0", "V1", "V2")])
# Friedman chi-squared = 0.22222, df = 2, p-value = 0.8948

# Step 4: Post-hoc test (Pairwise comparisons using Nemenyi test)
data_matrix <- as.matrix(PDQ_QOL[, c("V0", "V1", "V2")])
rownames(data_matrix) <- PDQ_QOL$num_patient  # Assign patient IDs as row names

posthoc_result <- frdAllPairsNemenyiTest(data_matrix)
print(posthoc_result)

#    V0   V1  
# V1 0.97 -   
# V2 0.88 0.97


# --------------------
# Compare with controls delta ------------------

Visites <- read_excel(path = "data/BDD_ETP_AMS_7122023.xlsx", sheet="Visites")
Visites <- Visites %>% select(num_patient, num_visite, date_visite, visite_realisee) %>%
  mutate(date_visite=as.Date(date_visite))

Info_patient <- read_excel(path = "data/BDD_ETP_AMS_7122023.xlsx", sheet="Info_patient")
Info_patient <- Info_patient %>% select(num_patient, sexe, annee_naissance, age_inclusion)

Histoire_AMS <- read_excel(path = "data/BDD_ETP_AMS_7122023.xlsx", sheet="Histoire_AMS")
Histoire_AMS <- Histoire_AMS %>% select(num_patient, AMS_C, AMS_P, AMS_cliniquement_etablie, AMS_cliniquement_possible, AMS_date_diag_an)

UMSARS1 <- read_excel(path = "data/BDD_ETP_AMS_7122023.xlsx", sheet="UMSARS1")
UMSARS1 <- UMSARS1 %>% filter(UMSARS1_realise==1) %>% select(num_patient, num_visite, UMSARS1_total)
UMSARS1 <- UMSARS1 %>% ungroup() %>%  mutate(UMSARS1_total=as.numeric(UMSARS1_total)) %>% drop_na()

UMSARS2 <- read_excel(path = "data/BDD_ETP_AMS_7122023.xlsx", sheet="UMSARS2")
UMSARS2 <- UMSARS2 %>% filter(UMSARS2_realise ==1) %>% select(num_patient, num_visite, UMSARS2_total)
UMSARS2 <- UMSARS2 %>% ungroup() %>%  mutate(UMSARS2_total=as.numeric(UMSARS2_total)) %>% drop_na()

QOL_AMS <- read_excel(path = "data/BDD_ETP_AMS_7122023.xlsx", sheet="QOL_AMS")
QOL_AMS <- QOL_AMS %>% filter(QOL_AMS_realise==1) %>% select(num_patient, num_visite, QOL_AMS_total, QOL_AMS_echelle_anal)

pats_to_match <- Visites %>% filter(num_visite=="V0") %>% select(num_patient, date_visite ) %>%
  inner_join(Info_patient %>% mutate(sexe=ifelse(sexe=="H",1,0)) ) %>%
  inner_join(Histoire_AMS %>% select(num_patient, AMS_date_diag_an)) %>%
  inner_join(UMSARS1 %>% filter(num_visite=="V0") %>% select(-num_visite) ) %>%
  inner_join(UMSARS2 %>% filter(num_visite=="V0") %>% select(-num_visite) ) %>%
  mutate(UMSARS_tot=UMSARS1_total+UMSARS2_total) %>%
  inner_join(QOL_AMS %>% filter(num_visite=="V0") %>% select(-num_visite)) %>%
   mutate(date_visite=as.character(date_visite)) %>%
  mutate(date_visite=str_sub(date_visite,1L, 4L)) %>% mutate(diff=12*(as.numeric(date_visite)-AMS_date_diag_an ))

pats_to_match

cor(pats_to_match$QOL_AMS_total, pats_to_match$QOL_AMS_echelle_anal)


# Sexe 1/0
# Age_inclusion
# diff
# UMSARS1
# UMSARS2
# QOL

dataCohorteManaged <- readRDS("data/dataCohorteManaged.rds")

dataCohorteManaged <- dataCohorteManaged %>% filter(NUM != "2CAMON0557" & NUM != "2ROSIM1050" & NUM != "2REFLO1261" & NUM != "2BEJEA0261" & NUM != "2BABEA0264" )

NUMS <- dataCohorteManaged %>% select(NUM) %>% distinct()

dataCohorteManaged <- dataCohorteManaged %>% group_by(NUM) %>% mutate(TIME_STUDY = ifelse( is.na(TIME_STUDY), 0, TIME_STUDY)) %>%
  mutate(Year= ifelse(TIME_STUDY==0, 0,
                      ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                             ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                    ifelse(TIME_STUDY>=2.5 ,3, NA))))) 

UMSARS1_all <- dataCohorteManaged %>% 
  select(NUM, DATECONSULT, TIME_STUDY, Year, UMSARS1_1:UMSARS1_TOT)


UMSARS1_all$UMSARS1_TOT_v2 <- rowSums(UMSARS1_all[, 5:16], na.rm = TRUE)

UMSARS1_all$missing_na <- rowSums(is.na(UMSARS1_all[, 5:16]))

range(UMSARS1_all$missing_na)

UMSARS1_all <-  UMSARS1_all %>%  select(NUM, DATECONSULT, TIME_STUDY, Year, UMSARS1_TOT)







dataCohorteManaged <- readRDS("data/dataCohorteManaged.rds")


dataCohorteManaged <- dataCohorteManaged %>% filter(NUM != "2CAMON0557" & NUM != "2ROSIM1050" & NUM != "2REFLO1261" & NUM != "2BEJEA0261" & NUM != "2BABEA0264" )

dataCohorteManaged <- dataCohorteManaged %>% group_by(NUM) %>% mutate(TIME_STUDY = ifelse( is.na(TIME_STUDY), 0, TIME_STUDY)) %>%
  mutate(Year= ifelse(TIME_STUDY==0, 0,
                      ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                             ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                    ifelse(TIME_STUDY>=2.5 ,3, NA))))) 


UMSARS2_all <- dataCohorteManaged %>% 
  select(NUM, DATECONSULT, TIME_STUDY, Year, UMSARS2_1:UMSARS2_TOT)

UMSARS2_all$UMSARS2_TOT_v2 <- rowSums(UMSARS2_all[, 5:19], na.rm = TRUE)

UMSARS2_all$missing_na <- rowSums(is.na(UMSARS2_all[, 5:19]))

UMSARS2_all <-  UMSARS2_all %>%  select(NUM, DATECONSULT, TIME_STUDY, Year, UMSARS2_TOT)

UMSARS_all <- UMSARS1_all %>% inner_join(UMSARS2_all) %>% mutate(UMSARS_tot=UMSARS1_TOT+UMSARS2_TOT)


pats_to_match

dataCohorteManaged <- readRDS("data/dataCohorteManaged.rds")

dataCohorteManaged <- dataCohorteManaged %>% filter(NUM != "2CAMON0557" & NUM != "2ROSIM1050" & NUM != "2REFLO1261" & NUM != "2BEJEA0261" & NUM != "2BABEA0264" )
BaselineDem <- dataCohorteManaged  %>% 
  select(NUM, DATECONSULT, DATENAIS, SEXE, DATE_VISITE0, annee_vis0, ANDIAG, AGE_VISITE0)

BaselineDem <- BaselineDem %>% mutate(SEXE=ifelse(SEXE==2,0,SEXE))

BaselineDem <- BaselineDem %>%
  mutate(DATECONSULT_2 =as.character(DATECONSULT )) %>%
  mutate(DATENAIS_2    =as.character(DATENAIS    )) %>%
  mutate(DATENAIS_2=str_sub(DATENAIS_2,1L, 4L)) %>%
  mutate(DATECONSULT_2 =str_sub(DATECONSULT_2 ,1L, 4L)) %>%
  mutate(Age_visite=(as.numeric(DATECONSULT_2)-as.numeric(DATENAIS_2 )))

BaselineDem <- BaselineDem %>% mutate(diff=12*(as.numeric(DATECONSULT_2)-ANDIAG )) %>%
  select(NUM, DATECONSULT, SEXE, AGE_VISITE0, diff)


BaselineDem <- BaselineDem %>% inner_join(UMSARS_all)







dataCohorteManaged <- readRDS("data/dataCohorteManaged.rds")

dataCohorteManaged <- dataCohorteManaged %>% filter(NUM != "2CAMON0557" & NUM != "2ROSIM1050" & NUM != "2REFLO1261" & NUM != "2BEJEA0261" & NUM != "2BABEA0264" )

dataCohorteManaged <- dataCohorteManaged %>% group_by(NUM) %>% mutate(TIME_STUDY = ifelse( is.na(TIME_STUDY), 0, TIME_STUDY)) %>%
  mutate(Year= ifelse(TIME_STUDY==0, 0,
                      ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                             ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                    ifelse(TIME_STUDY>=2.5 ,3, NA))))) 


SCHRAG <- dataCohorteManaged %>% 
  select(NUM, DATECONSULT, TIME_STUDY, Year, SCHRAG_1:SCHRAG_40, SCHRAG_TOT, SCHRAG_MOTOR, SCHRAG_NONMOTOR, SCHRAG_EMOTIONAL, ECHANQV)

SCHRAG %>% select(SCHRAG_TOT, SCHRAG_NONMOTOR, SCHRAG_MOTOR, SCHRAG_EMOTIONAL, ECHANQV)

names(SCHRAG)

SCHRAG$SCHRAG_TOT_v2 <- rowSums(SCHRAG[, 5:44], na.rm = TRUE)
SCHRAG$SCHRAG_MOTOR_v2 <- rowSums(SCHRAG[, 5:18], na.rm = TRUE)
SCHRAG$SCHRAG_NONMOTOR_v2 <- rowSums(SCHRAG[, 19:30], na.rm = TRUE)
SCHRAG$SCHRAG_EMOTIONAL_v2 <- rowSums(SCHRAG[, 31:44], na.rm = TRUE)

SCHRAG <- SCHRAG %>% select(-c(SCHRAG_TOT, SCHRAG_MOTOR, SCHRAG_NONMOTOR, SCHRAG_EMOTIONAL))

SCHRAG <- SCHRAG %>% filter(SCHRAG_TOT_v2!=0)

SCHRAG$all_na <- rowSums(is.na(SCHRAG[, 5:44])) == (44 - 5 + 1)

SCHRAG$number_na <- rowSums(is.na(SCHRAG[, 5:44])) 

SCHRAG <- SCHRAG %>% filter(!(all_na) )

SCHRAG$motor_na <- rowSums(is.na(SCHRAG[, 5:18])) # 14
SCHRAG$non_motor_na <- rowSums(is.na(SCHRAG[, 19:30]))  # 12
SCHRAG$emotional_na <- rowSums(is.na(SCHRAG[, 31:44]))  # 14

range(SCHRAG$motor_na)
range(SCHRAG$non_motor_na)
range(SCHRAG$emotional_na)


SCHRAG <- SCHRAG %>%  ungroup() %>%
 mutate(SCHRAG_MOTOR_v2= SCHRAG_MOTOR_v2/((14-motor_na)/14) ) %>%
  mutate(SCHRAG_NONMOTOR_v2= SCHRAG_NONMOTOR_v2/((14-non_motor_na)/14) ) %>%
  mutate(SCHRAG_EMOTIONAL_v2= SCHRAG_EMOTIONAL_v2/((14-emotional_na)/14) ) %>%
  mutate(SCHRAG_TOT_v2=SCHRAG_MOTOR_v2+SCHRAG_NONMOTOR_v2+SCHRAG_EMOTIONAL_v2) 

names(SCHRAG)

SCHRAG <- SCHRAG %>% select(NUM, DATECONSULT, TIME_STUDY, Year, SCHRAG_TOT_v2, ECHANQV)

SCHRAG <- SCHRAG %>% drop_na()

cor(SCHRAG$SCHRAG_TOT_v2, SCHRAG$ECHANQV)

mean(SCHRAG$SCHRAG_TOT_v2)
mean(SCHRAG$ECHANQV)

SCHRAG <- SCHRAG %>% select(-ECHANQV)


BaselineDem <- BaselineDem %>% inner_join(SCHRAG) %>% select(-c(UMSARS1_TOT , UMSARS2_TOT))


pats_to_match <- pats_to_match %>% select(-c(date_visite, annee_naissance, AMS_date_diag_an, UMSARS1_total, UMSARS2_total, QOL_AMS_echelle_anal))

# num_patient  sexe age_inclusion UMSARS_tot QOL_AMS_total  diff
BaselineDem <- BaselineDem %>% select(NUM, DATECONSULT, SEXE, AGE_VISITE0, UMSARS_tot, SCHRAG_TOT_v2, diff)

names(BaselineDem) <- c("num_patient", "DATECONSULT",  "sexe", "age_inclusion", "UMSARS_tot" , "QOL_AMS_total", "diff")



# Define Tolerance for Numerical Variables
tolerances <- c(age_inclusion = 5, diff = 12, 
                UMSARS_tot=10, QOL_AMS_total=10)


# Initialize List to Store Matches
matches <- list()

# Matching Loop
for (i in 1:nrow(pats_to_match)) {
  pats <- pats_to_match[i, ]
  
  # Filter for Exact Matches (Row-Specific Matching)
  potential_controls <- BaselineDem %>%
    filter(
      sexe == pats$sexe,      # Exact match on sex
      abs(age_inclusion - pats$age_inclusion) <= tolerances["age_inclusion"],      # age_inclusion ±5
      abs(diff - pats$diff) <= tolerances["diff"],    # diff ±6 months disease duration 
      abs(UMSARS_tot - pats$UMSARS_tot) <= tolerances["UMSARS_tot"], # UMSARS_tot ±10
      abs(QOL_AMS_total - pats$QOL_AMS_total) <= tolerances["QOL_AMS_total"] # QOL_AMS_total  ±10
      
    )
  
  # If there are matches, record them
  if (nrow(potential_controls) > 0) {
    potential_controls$matched_to <- pats$num_patient  # Track matched pump patient
    matches[[as.character(pats$num_patient)]] <- potential_controls
  }
}

# Combine Matches into a Single Data Frame
match_results <- do.call(rbind, matches)

# match_results %>% select(num_patient, matched_to) %>% distinct() %>%
#   group_by(matched_to) %>% count()
# 
# match_results <- match_results %>% select(num_patient, matched_to) %>%
#   distinct() %>% group_by(matched_to) %>% sample_n(3, replace=TRUE)
# 
# length(unique(match_results$matched_to))

controls_2plus_visits <- match_results %>% group_by(matched_to, num_patient) %>%
  summarise(DATECONSULT=min(DATECONSULT)) %>% distinct() %>%
  rename("first"="DATECONSULT") %>%
  left_join(BaselineDem) %>% 
  group_by(num_patient) %>% filter(DATECONSULT>=first) %>%
  group_by(num_patient) %>% count() %>% filter(n>=2)  %>% ungroup() %>% select(-n)
  
  
match_results %>% group_by(matched_to, num_patient) %>%
  summarise(DATECONSULT=min(DATECONSULT)) %>% distinct() %>%
  rename("first"="DATECONSULT") %>%
  left_join(BaselineDem) %>% 
  group_by(num_patient) %>% filter(DATECONSULT>=first) %>%
  inner_join(controls_2plus_visits) 

  

match_results %>% group_by(matched_to, num_patient) %>%
  summarise(DATECONSULT=min(DATECONSULT)) %>% distinct() %>%
  rename("first"="DATECONSULT") %>%
  left_join(BaselineDem) %>% 
  group_by(num_patient) %>% filter(DATECONSULT>=first) %>%
  inner_join(controls_2plus_visits) %>%
  group_by(num_patient) %>% filter(DATECONSULT==first) %>%
  ungroup() %>%
    summarise(mean=mean(age_inclusion    ), se=sd(age_inclusion    ),
            median=median(age_inclusion    ),
            q25=quantile(age_inclusion    , 0.25),
            q75=quantile(age_inclusion    , 0.75))


match_results %>% group_by(matched_to, num_patient) %>%
  summarise(DATECONSULT=min(DATECONSULT)) %>% distinct() %>%
  rename("first"="DATECONSULT") %>%
  left_join(BaselineDem) %>% 
  group_by(num_patient) %>% filter(DATECONSULT>=first) %>%
  inner_join(controls_2plus_visits) %>%
  group_by(num_patient) %>% filter(DATECONSULT==first) %>%
  ungroup() %>%
    summarise(mean=mean(age_inclusion    ), se=sd(age_inclusion    ),
            median=median(age_inclusion    ),
            q25=quantile(age_inclusion    , 0.25),
            q75=quantile(age_inclusion    , 0.75))


match_results %>% group_by(matched_to, num_patient) %>%
  summarise(DATECONSULT=min(DATECONSULT)) %>% distinct() %>%
  rename("first"="DATECONSULT") %>%
  left_join(BaselineDem) %>% 
  group_by(num_patient) %>% filter(DATECONSULT>=first) %>%
  inner_join(controls_2plus_visits) %>%
  group_by(num_patient) %>% filter(DATECONSULT==first) %>%
  arrange(num_patient) %>%
  ungroup() %>%
   group_by(sexe) %>% count()





filtered <- match_results %>% group_by(matched_to, num_patient) %>%
  summarise(DATECONSULT=min(DATECONSULT)) %>% distinct() %>%
  rename("first"="DATECONSULT") %>%
  left_join(BaselineDem) %>% 
  group_by(num_patient) %>% filter(DATECONSULT>=first) %>%
  inner_join(controls_2plus_visits) %>%
  left_join(
    dataCohorteManaged %>% select(NUM, DATECONSULT, TIME_STUDY),
    by=c("num_patient"="NUM", "DATECONSULT"="DATECONSULT")
  ) %>%
  group_by(matched_to, num_patient) %>%
    filter(TIME_STUDY == min(TIME_STUDY) | abs(TIME_STUDY - 1) == min(abs(TIME_STUDY - 1))) %>%
  ungroup()



filtered <- filtered %>% ungroup() %>% group_by(matched_to , num_patient) %>%
  count() %>% filter(n>1) %>%
  select(matched_to , num_patient) %>% distinct() %>% ungroup() %>%
  left_join(filtered) %>%
  group_by(matched_to , num_patient) %>%
  mutate(visite=row_number()) %>%
  filter(visite==1|visite==2)



UMSARS1 <- read_excel(path = "data/BDD_ETP_AMS_7122023.xlsx", sheet="UMSARS1")
UMSARS1 <- UMSARS1 %>% filter(UMSARS1_realise==1) %>% select(num_patient, num_visite, UMSARS1_total)
UMSARS1 <- UMSARS1 %>% ungroup() %>%  mutate(UMSARS1_total=as.numeric(UMSARS1_total)) %>% drop_na()

UMSARS2 <- read_excel(path = "data/BDD_ETP_AMS_7122023.xlsx", sheet="UMSARS2")
UMSARS2 <- UMSARS2 %>% filter(UMSARS2_realise ==1) %>% select(num_patient, num_visite, UMSARS2_total)
UMSARS2 <- UMSARS2 %>% ungroup() %>%  mutate(UMSARS2_total=as.numeric(UMSARS2_total)) %>% drop_na()

UMSARS <- UMSARS1 %>% inner_join(UMSARS2) %>% mutate(UMSARS_tot=UMSARS1_total+UMSARS2_total) %>% select(-c(UMSARS1_total,UMSARS2_total))








UMSARS %>% filter(num_visite=="V0"|num_visite=="V2") %>%
  spread(key=num_visite, value=UMSARS_tot) %>%
  drop_na() %>% mutate(delta=V2-V0) %>% select(-c(V0, V2)) %>% 
  mutate(group="TEP") %>% ungroup() %>% select(-num_patient)  %>%
   summarise(mean=mean(delta), se=sd(delta),
            median=median(delta),
            q25=quantile(delta, 0.25),
            q75=quantile(delta, 0.75))

#   mean    se median   q25   q75
#   <dbl> <dbl>  <dbl> <dbl> <dbl>
# 1  13.3  16.2      7   0.5    20

filtered %>% select(matched_to, num_patient, UMSARS_tot,visite  ) %>%
  spread(key=visite, value=UMSARS_tot) %>% drop_na() %>%
  mutate(delta=`2`-`1`) %>% select(-c(`1`, `2`)) %>% ungroup() %>%
  group_by(matched_to) %>% sample_n(3, replace=T) %>% ungroup() %>%
  mutate(group="Control") %>% select(-c(matched_to, num_patient)) %>%
   summarise(mean=mean(delta), se=sd(delta),
            median=median(delta),
            q25=quantile(delta, 0.25),
            q75=quantile(delta, 0.75))

#   mean    se median   q25   q75
#   <dbl> <dbl>  <dbl> <dbl> <dbl>
# 1  8.79  9.13     11     4    14



tep_table <-  UMSARS %>% filter(num_visite=="V0"|num_visite=="V2") %>%
  spread(key=num_visite, value=UMSARS_tot) %>%
  drop_na() %>% mutate(delta=V2-V0) %>% select(-c(V0, V2)) %>% 
  mutate(group="TEP") %>% ungroup()

control_table <- filtered %>% select(matched_to, num_patient, UMSARS_tot,visite  ) %>%
  spread(key=visite, value=UMSARS_tot) %>% drop_na() %>%
  mutate(delta=`2`-`1`) %>% select(-c(`1`, `2`)) %>% ungroup() %>%
  group_by(matched_to) %>% sample_n(3, replace=T) %>% ungroup() %>%
  mutate(group="Control") 


control_agg <- control_table %>%
  group_by(matched_to) %>%
  summarise(mean_delta_control = mean(delta, na.rm = TRUE))


merged_data <- tep_table %>%
  inner_join(control_agg, by = c("num_patient" = "matched_to"))

wilcox.test(merged_data$delta, merged_data$mean_delta_control, paired = TRUE, 
            alternative = "two.sided")

# 	Wilcoxon signed rank exact test
# 
# data:  merged_data$delta and merged_data$mean_delta_control
# V = 31, p-value = 0.7695
# alternative hypothesis: true location shift is not equal to 0


UMSARS %>% filter(num_visite=="V0"|num_visite=="V2") %>%
  spread(key=num_visite, value=UMSARS_tot) %>%
  drop_na() %>% mutate(delta=V2-V0) %>% select(-c(V0, V2)) %>% 
  mutate(group="TEP") %>% ungroup() %>% select(-num_patient)  %>%
  bind_rows(
    filtered %>% select(matched_to, num_patient, UMSARS_tot,visite  ) %>%
  spread(key=visite, value=UMSARS_tot) %>% drop_na() %>%
  mutate(delta=`2`-`1`) %>% select(-c(`1`, `2`)) %>% ungroup() %>%
  group_by(matched_to) %>% sample_n(3, replace=T) %>% ungroup() %>%
  mutate(group="Control") %>% select(-c(matched_to, num_patient))
  ) %>%
  ggplot(aes(group , delta, colour=group , fill=group )) +
  geom_boxplot(alpha=0.8, notch = TRUE, width = 0.5, outlier.shape = NA) +
  geom_jitter(size=2,shape=1,stroke=2.5, width=0.3 ) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 10, vjust = -0.5),
        axis.title.y = element_text(size = 10, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_color_manual(values=c("#bf4438", "#333652")) +
  scale_fill_manual(values=c("#bf4438", "#333652")) +
  xlab("\n Group Intervention") +
  ylab("Delta UMSRAS Total \n [12 months] \n") +
  ylim(-20, 60)  + geom_hline(yintercept=0)




  












QOL_AMS <- read_excel(path = "data/BDD_ETP_AMS_7122023.xlsx", sheet="QOL_AMS")
QOL_AMS <- QOL_AMS %>% filter(QOL_AMS_realise==1) %>% select(num_patient, num_visite, QOL_AMS_total, QOL_AMS_echelle_anal)





QOL_AMS %>% filter(num_visite=="V0"|num_visite=="V2") %>%select(-QOL_AMS_echelle_anal    ) %>%
  spread(key=num_visite, value=QOL_AMS_total ) %>% 
  drop_na() %>% mutate(delta=V2-V0) %>% select(-c(V0, V2)) %>% 
  mutate(group="TEP") %>% ungroup() %>% select(-num_patient)  %>%
   summarise(mean=mean(delta), se=sd(delta),
            median=median(delta),
            q25=quantile(delta, 0.25),
            q75=quantile(delta, 0.75))

#   mean    se median   q25   q75
#   <dbl> <dbl>  <dbl> <dbl> <dbl>
# 1  10.6  27.0     17   -11    34

filtered %>% select(matched_to, num_patient, QOL_AMS_total  ,visite  ) %>%
  spread(key=visite, value=QOL_AMS_total  ) %>% drop_na() %>%
  mutate(delta=`2`-`1`) %>% select(-c(`1`, `2`)) %>% ungroup() %>%
  group_by(matched_to) %>% sample_n(3, replace=T) %>% ungroup() %>%
  mutate(group="Control") %>% select(-c(matched_to, num_patient)) %>%
   summarise(mean=mean(delta), se=sd(delta),
            median=median(delta),
            q25=quantile(delta, 0.25),
            q75=quantile(delta, 0.75))

#   mean    se median   q25   q75
#   <dbl> <dbl>  <dbl> <dbl> <dbl>
# 1  9.73  20.0   5.91    -1  22.2



tep_table <-  QOL_AMS %>% filter(num_visite=="V0"|num_visite=="V2") %>% select(-QOL_AMS_echelle_anal    ) %>%
  spread(key=num_visite, value=QOL_AMS_total) %>%
  drop_na() %>% mutate(delta=V2-V0) %>% select(-c(V0, V2)) %>% 
  mutate(group="TEP") %>% ungroup()

control_table <- filtered %>% select(matched_to, num_patient, QOL_AMS_total,visite  ) %>%
  spread(key=visite, value=QOL_AMS_total) %>% drop_na() %>%
  mutate(delta=`2`-`1`) %>% select(-c(`1`, `2`)) %>% ungroup() %>%
  group_by(matched_to) %>% sample_n(3, replace=T) %>% ungroup() %>%
  mutate(group="Control") 


control_agg <- control_table %>%
  group_by(matched_to) %>%
  summarise(mean_delta_control = mean(delta, na.rm = TRUE))


merged_data <- tep_table %>%
  inner_join(control_agg, by = c("num_patient" = "matched_to"))

wilcox.test(merged_data$delta, merged_data$mean_delta_control, paired = TRUE, 
            alternative = "two.sided")

# 	Wilcoxon signed rank exact test
# 
# data:  merged_data$delta and merged_data$mean_delta_control
# V = 42, p-value = 0.4648
# alternative hypothesis: true location shift is not equal to 0

QOL_AMS %>% filter(num_visite=="V0"|num_visite=="V2") %>% select(-QOL_AMS_echelle_anal    ) %>%
  spread(key=num_visite, value=QOL_AMS_total) %>%
  drop_na() %>% mutate(delta=V2-V0) %>% select(-c(V0, V2)) %>% 
  mutate(group="TEP") %>% ungroup() %>% select(-num_patient)  %>%
  bind_rows(
    filtered %>% select(matched_to, num_patient, QOL_AMS_total,visite  ) %>%
  spread(key=visite, value=QOL_AMS_total) %>% drop_na() %>%
  mutate(delta=`2`-`1`) %>% select(-c(`1`, `2`)) %>% ungroup() %>%
  group_by(matched_to) %>% sample_n(3, replace=T) %>% ungroup() %>%
  mutate(group="Control") %>% select(-c(matched_to, num_patient))
  ) %>%
  ggplot(aes(group , delta, colour=group , fill=group )) +
  geom_boxplot(alpha=0.8, notch = TRUE, width = 0.5, outlier.shape = NA) +
  geom_jitter(size=2,shape=1,stroke=2.5, width=0.3 ) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 10, vjust = -0.5),
        axis.title.y = element_text(size = 10, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_color_manual(values=c("#bf4438", "#333652")) +
  scale_fill_manual(values=c("#bf4438", "#333652")) +
  xlab("\n Group Intervention") +
  ylab("Delta MSA QoL Total \n [12 months] \n") +
   geom_hline(yintercept=0)


# ------------------