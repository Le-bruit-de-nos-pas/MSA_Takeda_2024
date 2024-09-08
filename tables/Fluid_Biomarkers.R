
library(tidyverse)
library(data.table)
library(moments)
library(lubridate)
options(scipen = 999)

# PLOTS --------------
# ------------
# All data dates groups  --------
dataCohorteManaged <- readRDS("Source/dataCohorteManaged.rds")

Visits_zero <- dataCohorteManaged  %>% select(NUM, dateVisite0) %>% distinct()

AllMSA_Pop_Baseline_671 <- fread("Source/AllMSA_Pop_Baseline_671.txt")
EarlyCT_Pop_Baseline_319 <- fread("Source/EarlyCT_Pop_Baseline_319.txt")

library(UpSetR)
# ----------
# Plasma NFL -----------------------------

biofluidsForPaulo <- readRDS("Source/biofluidsForPaulo.rds")

Plasma_NFL <- biofluidsForPaulo %>% rename("NUM"="idGlobal") %>% 
  select(NUM, datePrelevement, nfl_pgml_plas, nfl_ipp_pgml_plas)

Plasma_NFL <- Visits_zero %>% inner_join(Plasma_NFL) %>%
  mutate(TIME_STUDY=time_length(difftime(datePrelevement, dateVisite0 ), "years")) %>%
  mutate(Year= ifelse(TIME_STUDY<=0.5, 0,
                      ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                             ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                    ifelse(TIME_STUDY>=2.5 ,3, NA))))) 

Plasma_NFL <- Plasma_NFL %>% gather(original, NFL, nfl_pgml_plas:nfl_ipp_pgml_plas) %>%
  drop_na() %>% arrange(NUM, datePrelevement) %>%
  group_by(NUM, datePrelevement) %>% mutate(NFL=mean(NFL)) %>%
  select(-original) %>% distinct()

dataframe <- Plasma_NFL %>% ungroup() %>% select(NUM, Year) %>% distinct() %>%
  mutate(exp=1) %>% spread(key=Year, value=exp) %>% select(-NUM)

dataframe[is.na(dataframe)] <- 0
names(dataframe) <- c("Year_0","Year_1","Year_2","Year_3")

setDT(dataframe)

upset(dataframe, 
      sets=colnames(dataframe),
      nsets = length(colnames(dataframe)), nintersects = NA,
      mainbar.y.label = "Patient Count", point.size = 5,text.scale = 2,
      sets.bar.color = "deepskyblue4", main.bar.color = "deepskyblue4")


# ----------
# CSF NFL -----------------------------

biofluidsForPaulo <- readRDS("Source/biofluidsForPaulo.rds")

CSF_NFL <- biofluidsForPaulo %>% rename("NUM"="idGlobal") %>% 
  select(NUM, datePrelevement, nfl_pgml_csf, nfl_ipp_pgml_csf, nfl_pgml_csf_fusion)

CSF_NFL <- Visits_zero %>% inner_join(CSF_NFL) %>%
  mutate(TIME_STUDY=time_length(difftime(datePrelevement, dateVisite0 ), "years")) %>%
  mutate(Year= ifelse(TIME_STUDY<=0.5, 0,
                      ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                             ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                    ifelse(TIME_STUDY>=2.5 ,3, NA))))) 

CSF_NFL <- CSF_NFL %>% gather(original, NFL, nfl_pgml_csf:nfl_pgml_csf_fusion) %>%
  drop_na() %>% arrange(NUM, datePrelevement) %>%
  group_by(NUM, datePrelevement) %>% mutate(NFL=mean(NFL)) %>%
  select(-original) %>% distinct()


dataframe <- CSF_NFL %>% ungroup() %>% select(NUM, Year) %>% distinct() %>%
  mutate(exp=1) %>% spread(key=Year, value=exp) %>% select(-NUM)

dataframe[is.na(dataframe)] <- 0
names(dataframe) <- c("Year_0","Year_1","Year_2","Year_3")

setDT(dataframe)

upset(dataframe, 
      sets=colnames(dataframe),
      nsets = length(colnames(dataframe)), nintersects = NA,
      mainbar.y.label = "Patient Count", point.size = 5,text.scale = 2,
      sets.bar.color = "deepskyblue4", main.bar.color = "deepskyblue4")

# ----------
# Plasma aSYN -----------------------------

biofluidsForPaulo <- readRDS("Source/biofluidsForPaulo.rds")
names(biofluidsForPaulo)
Plasma_SYN <- biofluidsForPaulo %>% rename("NUM"="idGlobal") %>% 
  select(NUM, datePrelevement, total_alpha_syn_q1_pgml_plas, total_alpha_syn_q2_pgml_plas)

Plasma_SYN <- Visits_zero %>% inner_join(Plasma_SYN) %>%
  mutate(TIME_STUDY=time_length(difftime(datePrelevement, dateVisite0 ), "years")) %>%
  mutate(Year= ifelse(TIME_STUDY<=0.5, 0,
                      ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                             ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                    ifelse(TIME_STUDY>=2.5 ,3, NA))))) 

Plasma_SYN <- Plasma_SYN %>% gather(original, SYN, total_alpha_syn_q1_pgml_plas:total_alpha_syn_q2_pgml_plas) %>%
  drop_na() %>% arrange(NUM, datePrelevement) %>%
  group_by(NUM, datePrelevement) %>% mutate(SYN=mean(SYN)) %>%
  select(-original) %>% distinct()


dataframe <- Plasma_SYN %>% ungroup() %>% select(NUM, Year) %>% distinct() %>%
  mutate(exp=1) %>% spread(key=Year, value=exp) %>% select(-NUM)

dataframe[is.na(dataframe)] <- 0
names(dataframe) <- c("Year_0","Year_1","Year_2","Year_3")

setDT(dataframe)

upset(dataframe, 
      sets=colnames(dataframe),
      nsets = length(colnames(dataframe)), nintersects = NA,
      mainbar.y.label = "Patient Count", point.size = 5,text.scale = 2,
      sets.bar.color = "deepskyblue4", main.bar.color = "deepskyblue4")

# ----------
# CSF aSYN -----------------------------

biofluidsForPaulo <- readRDS("Source/biofluidsForPaulo.rds")
names(biofluidsForPaulo)
CSF_SYN <- biofluidsForPaulo %>% rename("NUM"="idGlobal") %>% 
  select(NUM, datePrelevement, total_alpha_syn_g_pgml_csf, total_alpha_syn_q1_pgml_csf, total_alpha_syn_q2_pgml_csf, 
         total_alpha_syn_ipp_pgml_csf, totalAlphaSyn_Fusion_pgml_csf)

CSF_SYN <- Visits_zero %>% inner_join(CSF_SYN) %>%
  mutate(TIME_STUDY=time_length(difftime(datePrelevement, dateVisite0 ), "years")) %>%
  mutate(Year= ifelse(TIME_STUDY<=0.5, 0,
                      ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                             ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                    ifelse(TIME_STUDY>=2.5 ,3, NA))))) 

CSF_SYN <- CSF_SYN %>% gather(original, SYN, total_alpha_syn_g_pgml_csf:totalAlphaSyn_Fusion_pgml_csf) %>%
  drop_na() %>% arrange(NUM, datePrelevement) %>%
  group_by(NUM, datePrelevement) %>% mutate(SYN=mean(SYN)) %>%
  select(-original) %>% distinct()




dataframe <- CSF_SYN %>% ungroup() %>% select(NUM, Year) %>% distinct() %>%
  mutate(exp=1) %>% spread(key=Year, value=exp) %>% select(-NUM)

dataframe[is.na(dataframe)] <- 0
names(dataframe) <- c("Year_0","Year_1","Year_2","Year_3")

setDT(dataframe)

upset(dataframe, 
      sets=colnames(dataframe),
      nsets = length(colnames(dataframe)), nintersects = NA,
      mainbar.y.label = "Patient Count", point.size = 5,text.scale = 2,
      sets.bar.color = "deepskyblue4", main.bar.color = "deepskyblue4")


# ----------
# END OF PLOTS ---------
# ---------------
# PLOTS LONGITUDINAL --------------
# ------------
# All data dates groups  --------
dataCohorteManaged <- readRDS("Source/dataCohorteManaged.rds")

Visits_zero <- dataCohorteManaged  %>% select(NUM, dateVisite0) %>% distinct()

AllMSA_Pop_Baseline_671 <- fread("Source/AllMSA_Pop_Baseline_671.txt")
EarlyCT_Pop_Baseline_319 <- fread("Source/EarlyCT_Pop_Baseline_319.txt")

# ----------
# Plasma NFL -----------------------------

biofluidsForPaulo <- readRDS("Source/biofluidsForPaulo.rds")

Plasma_NFL <- biofluidsForPaulo %>% rename("NUM"="idGlobal") %>% 
  select(NUM, datePrelevement, nfl_pgml_plas, nfl_ipp_pgml_plas)

Plasma_NFL <- Visits_zero %>% inner_join(Plasma_NFL) %>%
  mutate(TIME_STUDY=time_length(difftime(datePrelevement, dateVisite0 ), "years")) %>%
  mutate(Year= ifelse(TIME_STUDY<=0.5, 0,
                      ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                             ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                    ifelse(TIME_STUDY>=2.5 ,3, NA))))) 

Plasma_NFL <- Plasma_NFL %>% gather(original, NFL, nfl_pgml_plas:nfl_ipp_pgml_plas) %>%
  drop_na() %>% arrange(NUM, datePrelevement) %>%
  group_by(NUM, datePrelevement) %>% mutate(NFL=mean(NFL)) %>%
  select(-original) %>% distinct()


Plasma_NFL %>% 
  inner_join(AllMSA_Pop_Baseline_671) %>%
  ggplot(aes(TIME_STUDY, NFL)) +
  stat_smooth(method="loess", colour="deepskyblue4", fill="deepskyblue4" , alpha=0.25, lwd=1.5, se=TRUE)+
  geom_line(aes(group=NUM), col="black" , linewidth=1, alpha=0.6) +
  geom_jitter(size=2, colour="black", alpha=0.7, shape=1, stroke=2) +
  theme_minimal() +
  xlab("\n Elapsed number of years since study enrollment \n [All available patient records up to year 5]") +
  ylab("Plasma NFL Level \n At each collection \n") +
  scale_color_manual(values=palette) +
  scale_fill_manual(values=palette) +
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
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_x_continuous(breaks = seq(0, 5, by = 0.5)) 

# ----------
# CSF NFL -----------------------------

biofluidsForPaulo <- readRDS("Source/biofluidsForPaulo.rds")

CSF_NFL <- biofluidsForPaulo %>% rename("NUM"="idGlobal") %>% 
  select(NUM, datePrelevement, nfl_pgml_csf, nfl_ipp_pgml_csf, nfl_pgml_csf_fusion)

CSF_NFL <- Visits_zero %>% inner_join(CSF_NFL) %>%
  mutate(TIME_STUDY=time_length(difftime(datePrelevement, dateVisite0 ), "years")) %>%
  mutate(Year= ifelse(TIME_STUDY<=0.5, 0,
                      ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                             ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                    ifelse(TIME_STUDY>=2.5 ,3, NA))))) 

CSF_NFL <- CSF_NFL %>% gather(original, NFL, nfl_pgml_csf:nfl_pgml_csf_fusion) %>%
  drop_na() %>% arrange(NUM, datePrelevement) %>%
  group_by(NUM, datePrelevement) %>% mutate(NFL=mean(NFL)) %>%
  select(-original) %>% distinct()


CSF_NFL %>% 
  inner_join(AllMSA_Pop_Baseline_671) %>%
  ggplot(aes(TIME_STUDY, NFL)) +
  stat_smooth(method="loess", colour="deepskyblue4", fill="deepskyblue4" , alpha=0.25, lwd=1.5, se=TRUE)+
  geom_line(aes(group=NUM), col="black" , linewidth=1, alpha=0.6) +
  geom_jitter(size=2, colour="black", alpha=0.7, shape=1, stroke=2) +
  theme_minimal() +
  xlab("\n Elapsed number of years since study enrollment \n [All available patient records up to year 5]") +
  ylab("CSF NFL Level \n At each collection \n") +
  scale_color_manual(values=palette) +
  scale_fill_manual(values=palette) +
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
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_x_continuous(breaks = seq(0, 5, by = 0.5)) 


# ----------
# Plasma aSYN -----------------------------

biofluidsForPaulo <- readRDS("Source/biofluidsForPaulo.rds")
names(biofluidsForPaulo)
Plasma_SYN <- biofluidsForPaulo %>% rename("NUM"="idGlobal") %>% 
  select(NUM, datePrelevement, total_alpha_syn_q1_pgml_plas, total_alpha_syn_q2_pgml_plas)

Plasma_SYN <- Visits_zero %>% inner_join(Plasma_SYN) %>%
  mutate(TIME_STUDY=time_length(difftime(datePrelevement, dateVisite0 ), "years")) %>%
  mutate(Year= ifelse(TIME_STUDY<=0.5, 0,
                      ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                             ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                    ifelse(TIME_STUDY>=2.5 ,3, NA))))) 

Plasma_SYN <- Plasma_SYN %>% gather(original, SYN, total_alpha_syn_q1_pgml_plas:total_alpha_syn_q2_pgml_plas) %>%
  drop_na() %>% arrange(NUM, datePrelevement) %>%
  group_by(NUM, datePrelevement) %>% mutate(SYN=mean(SYN)) %>%
  select(-original) %>% distinct()



CSF_NFL %>% 
  inner_join(AllMSA_Pop_Baseline_671) %>%
  ggplot(aes(TIME_STUDY, NFL)) +
  stat_smooth(method="loess", colour="deepskyblue4", fill="deepskyblue4" , alpha=0.25, lwd=1.5, se=TRUE)+
  geom_line(aes(group=NUM), col="black" , linewidth=1, alpha=0.6) +
  geom_jitter(size=2, colour="black", alpha=0.7, shape=1, stroke=2) +
  theme_minimal() +
  xlab("\n Elapsed number of years since study enrollment \n [All available patient records up to year 5]") +
  ylab("Plasma Alpha-synucluein Level \n At each collection \n") +
  scale_color_manual(values=palette) +
  scale_fill_manual(values=palette) +
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
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_x_continuous(breaks = seq(0, 5, by = 0.5)) 



# ----------
# CSF aSYN -----------------------------

biofluidsForPaulo <- readRDS("Source/biofluidsForPaulo.rds")
names(biofluidsForPaulo)
CSF_SYN <- biofluidsForPaulo %>% rename("NUM"="idGlobal") %>% 
  select(NUM, datePrelevement, total_alpha_syn_g_pgml_csf, total_alpha_syn_q1_pgml_csf, total_alpha_syn_q2_pgml_csf, 
         total_alpha_syn_ipp_pgml_csf, totalAlphaSyn_Fusion_pgml_csf)

CSF_SYN <- Visits_zero %>% inner_join(CSF_SYN) %>%
  mutate(TIME_STUDY=time_length(difftime(datePrelevement, dateVisite0 ), "years")) %>%
  mutate(Year= ifelse(TIME_STUDY<=0.5, 0,
                      ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                             ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                    ifelse(TIME_STUDY>=2.5 ,3, NA))))) 

CSF_SYN <- CSF_SYN %>% gather(original, SYN, total_alpha_syn_g_pgml_csf:totalAlphaSyn_Fusion_pgml_csf) %>%
  drop_na() %>% arrange(NUM, datePrelevement) %>%
  group_by(NUM, datePrelevement) %>% mutate(SYN=mean(SYN)) %>%
  select(-original) %>% distinct()




CSF_NFL %>% 
  inner_join(AllMSA_Pop_Baseline_671) %>%
  ggplot(aes(TIME_STUDY, NFL)) +
  stat_smooth(method="loess", colour="deepskyblue4", fill="deepskyblue4" , alpha=0.25, lwd=1.5, se=TRUE)+
  geom_line(aes(group=NUM), col="black" , linewidth=1, alpha=0.6) +
  geom_jitter(size=2, colour="black", alpha=0.7, shape=1, stroke=2) +
  theme_minimal() +
  xlab("\n Elapsed number of years since study enrollment \n [All available patient records up to year 5]") +
  ylab("CSF Alpha-synucluein Level \n At each collection \n") +
  scale_color_manual(values=palette) +
  scale_fill_manual(values=palette) +
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
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_x_continuous(breaks = seq(0, 5, by = 0.5)) 





# ----------

# END OF PLOTS LONGITUDINAL ----------
# ----------
# All data dates groups  --------
dataCohorteManaged <- readRDS("Source/dataCohorteManaged.rds")

Visits_zero <- dataCohorteManaged  %>% select(NUM, dateVisite0) %>% distinct()

AllMSA_Pop_Baseline_671 <- fread("Source/AllMSA_Pop_Baseline_671.txt")
EarlyCT_Pop_Baseline_319 <- fread("Source/EarlyCT_Pop_Baseline_319.txt")


# ----------
# Plasma NFL -----------------------------

biofluidsForPaulo <- readRDS("Source/biofluidsForPaulo.rds")

Plasma_NFL <- biofluidsForPaulo %>% rename("NUM"="idGlobal") %>% 
  select(NUM, datePrelevement, nfl_pgml_plas, nfl_ipp_pgml_plas)

Plasma_NFL <- Visits_zero %>% inner_join(Plasma_NFL) %>%
  mutate(TIME_STUDY=time_length(difftime(datePrelevement, dateVisite0 ), "years")) %>%
  mutate(Year= ifelse(TIME_STUDY<=0.5, 0,
                      ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                             ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                    ifelse(TIME_STUDY>=2.5 ,3, NA))))) 

Plasma_NFL <- Plasma_NFL %>% gather(original, NFL, nfl_pgml_plas:nfl_ipp_pgml_plas) %>%
  drop_na() %>% arrange(NUM, datePrelevement) %>%
  group_by(NUM, datePrelevement) %>% mutate(NFL=mean(NFL)) %>%
  select(-original) %>% distinct()

# ----------
# Overall MSA Entire  -----------------------

# ALL Plasma NFL Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(NFL),
    sd=sd(NFL),
    median=median(NFL),
    Q1 = quantile(NFL, 0.25),
    Q3 = quantile(NFL, 0.75),
    min=min(NFL),
    max=max(NFL),
    n=n(),
    skewness = skewness(NFL, na.rm = TRUE),
    kurtosis = kurtosis(NFL, na.rm = TRUE),
    CI_lower = t.test(NFL)$conf.int[1],
    CI_upper = t.test(NFL)$conf.int[2],
    shapiro_w = shapiro.test(NFL)$statistic,
    shapiro_p = shapiro.test(NFL)$p.value
  )


# ALL Plasma NFL Total Year 1

AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_NFL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(NFL),
    sd=sd(NFL),
    median=median(NFL),
    Q1 = quantile(NFL, 0.25),
    Q3 = quantile(NFL, 0.75),
    min=min(NFL),
    max=max(NFL),
    n=n(),
    skewness = skewness(NFL, na.rm = TRUE),
    kurtosis = kurtosis(NFL, na.rm = TRUE),
    CI_lower = t.test(NFL)$conf.int[1],
    CI_upper = t.test(NFL)$conf.int[2],
    shapiro_w = shapiro.test(NFL)$statistic,
    shapiro_p = shapiro.test(NFL)$p.value
  )



# Elapsed Time Year 1


AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_NFL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 1


AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_NFL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL)
  ) %>%
  mutate(Diff=(NFL-Baseline))  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 1

AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_NFL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, TIME_STUDY)
  ) %>%
  mutate(Diff=(NFL-Baseline)/TIME_STUDY)  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )













# ALL Plasma NFL Total Year 2

AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_NFL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(NFL),
    sd=sd(NFL),
    median=median(NFL),
    Q1 = quantile(NFL, 0.25),
    Q3 = quantile(NFL, 0.75),
    min=min(NFL),
    max=max(NFL),
    n=n(),
    skewness = skewness(NFL, na.rm = TRUE),
    kurtosis = kurtosis(NFL, na.rm = TRUE),
    CI_lower = t.test(NFL)$conf.int[1],
    CI_upper = t.test(NFL)$conf.int[2],
    shapiro_w = shapiro.test(NFL)$statistic,
    shapiro_p = shapiro.test(NFL)$p.value
  )



# Elapsed Time Year 2


AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_NFL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 2


AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_NFL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL)
  ) %>%
  mutate(Diff=(NFL-Baseline))  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 2

AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_NFL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, TIME_STUDY)
  ) %>%
  mutate(Diff=(NFL-Baseline)/TIME_STUDY)  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )






# ALL Plasma NFL Total Year 3

AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_NFL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(NFL),
    sd=sd(NFL),
    median=median(NFL),
    Q1 = quantile(NFL, 0.25),
    Q3 = quantile(NFL, 0.75),
    min=min(NFL),
    max=max(NFL),
    n=n(),
    skewness = skewness(NFL, na.rm = TRUE),
    kurtosis = kurtosis(NFL, na.rm = TRUE),
    CI_lower = t.test(NFL)$conf.int[1],
    CI_upper = t.test(NFL)$conf.int[2],
    shapiro_w = shapiro.test(NFL)$statistic,
    shapiro_p = shapiro.test(NFL)$p.value
  )



# Elapsed Time Year 3


AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_NFL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 3


AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_NFL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL)
  ) %>%
  mutate(Diff=(NFL-Baseline))  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 3

AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_NFL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, TIME_STUDY)
  ) %>%
  mutate(Diff=(NFL-Baseline)/TIME_STUDY)  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )



# -----------
# Overall MSA Entire  MSA P vs MSA C  -----------------------

# ALL Plasma NFL Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(NFL),
    sd=sd(NFL),
    median=median(NFL),
    Q1 = quantile(NFL, 0.25),
    Q3 = quantile(NFL, 0.75),
    min=min(NFL),
    max=max(NFL),
    n=n(),
    skewness = skewness(NFL, na.rm = TRUE),
    kurtosis = kurtosis(NFL, na.rm = TRUE),
    CI_lower = t.test(NFL)$conf.int[1],
    CI_upper = t.test(NFL)$conf.int[2],
    shapiro_w = shapiro.test(NFL)$statistic,
    shapiro_p = shapiro.test(NFL)$p.value
  )


# ALL Plasma NFL Total Year 1

AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_NFL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(NFL),
    sd=sd(NFL),
    median=median(NFL),
    Q1 = quantile(NFL, 0.25),
    Q3 = quantile(NFL, 0.75),
    min=min(NFL),
    max=max(NFL),
    n=n(),
    skewness = skewness(NFL, na.rm = TRUE),
    kurtosis = kurtosis(NFL, na.rm = TRUE),
    CI_lower = t.test(NFL)$conf.int[1],
    CI_upper = t.test(NFL)$conf.int[2],
    shapiro_w = shapiro.test(NFL)$statistic,
    shapiro_p = shapiro.test(NFL)$p.value
  )



# Elapsed Time Year 1


AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_NFL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, DIAG, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 1


AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_NFL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, DIAG)
  ) %>%
  mutate(Diff=(NFL-Baseline))  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 1

AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_NFL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, DIAG, TIME_STUDY)
  ) %>%
  mutate(Diff=(NFL-Baseline)/TIME_STUDY)  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )













# ALL Plasma NFL Total Year 2

AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_NFL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(NFL),
    sd=sd(NFL),
    median=median(NFL),
    Q1 = quantile(NFL, 0.25),
    Q3 = quantile(NFL, 0.75),
    min=min(NFL),
    max=max(NFL),
    n=n(),
    skewness = skewness(NFL, na.rm = TRUE),
    kurtosis = kurtosis(NFL, na.rm = TRUE),
    CI_lower = t.test(NFL)$conf.int[1],
    CI_upper = t.test(NFL)$conf.int[2],
    shapiro_w = shapiro.test(NFL)$statistic,
    shapiro_p = shapiro.test(NFL)$p.value
  )



# Elapsed Time Year 2


AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_NFL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, DIAG, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 2


AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_NFL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, DIAG)
  ) %>%
  mutate(Diff=(NFL-Baseline))  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 2

AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_NFL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, DIAG, TIME_STUDY)
  ) %>%
  mutate(Diff=(NFL-Baseline)/TIME_STUDY)  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )






# ALL Plasma NFL Total Year 3

AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_NFL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(NFL),
    sd=sd(NFL),
    median=median(NFL),
    Q1 = quantile(NFL, 0.25),
    Q3 = quantile(NFL, 0.75),
    min=min(NFL),
    max=max(NFL),
    n=n(),
    skewness = skewness(NFL, na.rm = TRUE),
    kurtosis = kurtosis(NFL, na.rm = TRUE),
    CI_lower = t.test(NFL)$conf.int[1],
    CI_upper = t.test(NFL)$conf.int[2],
    shapiro_w = shapiro.test(NFL)$statistic,
    shapiro_p = shapiro.test(NFL)$p.value
  )



# Elapsed Time Year 3


AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_NFL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, DIAG, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 3


AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_NFL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, DIAG)
  ) %>%
  mutate(Diff=(NFL-Baseline))  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 3

AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_NFL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, DIAG, TIME_STUDY)
  ) %>%
  mutate(Diff=(NFL-Baseline)/TIME_STUDY)  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )



# -----------
# Overall MSA Entire  Probable vs Possible  -----------------------

# ALL Plasma NFL Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(NFL),
    sd=sd(NFL),
    median=median(NFL),
    Q1 = quantile(NFL, 0.25),
    Q3 = quantile(NFL, 0.75),
    min=min(NFL),
    max=max(NFL),
    n=n(),
    skewness = skewness(NFL, na.rm = TRUE),
    kurtosis = kurtosis(NFL, na.rm = TRUE),
    CI_lower = t.test(NFL)$conf.int[1],
    CI_upper = t.test(NFL)$conf.int[2],
    shapiro_w = shapiro.test(NFL)$statistic,
    shapiro_p = shapiro.test(NFL)$p.value
  )


# ALL Plasma NFL Total Year 1

AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_NFL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(NFL),
    sd=sd(NFL),
    median=median(NFL),
    Q1 = quantile(NFL, 0.25),
    Q3 = quantile(NFL, 0.75),
    min=min(NFL),
    max=max(NFL),
    n=n(),
    skewness = skewness(NFL, na.rm = TRUE),
    kurtosis = kurtosis(NFL, na.rm = TRUE),
    CI_lower = t.test(NFL)$conf.int[1],
    CI_upper = t.test(NFL)$conf.int[2],
    shapiro_w = shapiro.test(NFL)$statistic,
    shapiro_p = shapiro.test(NFL)$p.value
  )



# Elapsed Time Year 1


AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_NFL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, DIAGNIV, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 1


AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_NFL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, DIAGNIV)
  ) %>%
  mutate(Diff=(NFL-Baseline))  %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 1

AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_NFL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, DIAGNIV, TIME_STUDY)
  ) %>%
  mutate(Diff=(NFL-Baseline)/TIME_STUDY)  %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )













# ALL Plasma NFL Total Year 2

AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_NFL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(NFL),
    sd=sd(NFL),
    median=median(NFL),
    Q1 = quantile(NFL, 0.25),
    Q3 = quantile(NFL, 0.75),
    min=min(NFL),
    max=max(NFL),
    n=n(),
    skewness = skewness(NFL, na.rm = TRUE),
    kurtosis = kurtosis(NFL, na.rm = TRUE),
    CI_lower = t.test(NFL)$conf.int[1],
    CI_upper = t.test(NFL)$conf.int[2],
    shapiro_w = shapiro.test(NFL)$statistic,
    shapiro_p = shapiro.test(NFL)$p.value
  )



# Elapsed Time Year 2


AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_NFL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, DIAGNIV, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 2


AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_NFL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, DIAGNIV)
  ) %>%
  mutate(Diff=(NFL-Baseline))  %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 2

AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_NFL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, DIAGNIV, TIME_STUDY)
  ) %>%
  mutate(Diff=(NFL-Baseline)/TIME_STUDY)  %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )






# ALL Plasma NFL Total Year 3

AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_NFL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(NFL),
    sd=sd(NFL),
    median=median(NFL),
    Q1 = quantile(NFL, 0.25),
    Q3 = quantile(NFL, 0.75),
    min=min(NFL),
    max=max(NFL),
    n=n(),
    skewness = skewness(NFL, na.rm = TRUE),
    kurtosis = kurtosis(NFL, na.rm = TRUE),
    CI_lower = t.test(NFL)$conf.int[1],
    CI_upper = t.test(NFL)$conf.int[2],
    shapiro_w = shapiro.test(NFL)$statistic,
    shapiro_p = shapiro.test(NFL)$p.value
  )



# Elapsed Time Year 3


AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_NFL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, DIAGNIV, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 3


AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_NFL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, DIAGNIV)
  ) %>%
  mutate(Diff=(NFL-Baseline))  %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 3

AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_NFL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, DIAGNIV, TIME_STUDY)
  ) %>%
  mutate(Diff=(NFL-Baseline)/TIME_STUDY)  %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )

# ---------------


# Target MSA Entire  -----------------------

# ALL Plasma NFL Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(NFL),
    sd=sd(NFL),
    median=median(NFL),
    Q1 = quantile(NFL, 0.25),
    Q3 = quantile(NFL, 0.75),
    min=min(NFL),
    max=max(NFL),
    n=n(),
    skewness = skewness(NFL, na.rm = TRUE),
    kurtosis = kurtosis(NFL, na.rm = TRUE),
    CI_lower = t.test(NFL)$conf.int[1],
    CI_upper = t.test(NFL)$conf.int[2],
    shapiro_w = shapiro.test(NFL)$statistic,
    shapiro_p = shapiro.test(NFL)$p.value
  )


# ALL Plasma NFL Total Year 1

EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_NFL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(NFL),
    sd=sd(NFL),
    median=median(NFL),
    Q1 = quantile(NFL, 0.25),
    Q3 = quantile(NFL, 0.75),
    min=min(NFL),
    max=max(NFL),
    n=n(),
    skewness = skewness(NFL, na.rm = TRUE),
    kurtosis = kurtosis(NFL, na.rm = TRUE),
    CI_lower = t.test(NFL)$conf.int[1],
    CI_upper = t.test(NFL)$conf.int[2],
    shapiro_w = shapiro.test(NFL)$statistic,
    shapiro_p = shapiro.test(NFL)$p.value
  )



# Elapsed Time Year 1


EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_NFL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 1


EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_NFL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL)
  ) %>%
  mutate(Diff=(NFL-Baseline))  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 1

EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_NFL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, TIME_STUDY)
  ) %>%
  mutate(Diff=(NFL-Baseline)/TIME_STUDY)  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )













# ALL Plasma NFL Total Year 2

EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_NFL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(NFL),
    sd=sd(NFL),
    median=median(NFL),
    Q1 = quantile(NFL, 0.25),
    Q3 = quantile(NFL, 0.75),
    min=min(NFL),
    max=max(NFL),
    n=n(),
    skewness = skewness(NFL, na.rm = TRUE),
    kurtosis = kurtosis(NFL, na.rm = TRUE),
    CI_lower = t.test(NFL)$conf.int[1],
    CI_upper = t.test(NFL)$conf.int[2],
    shapiro_w = shapiro.test(NFL)$statistic,
    shapiro_p = shapiro.test(NFL)$p.value
  )



# Elapsed Time Year 2


EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_NFL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 2


EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_NFL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL)
  ) %>%
  mutate(Diff=(NFL-Baseline))  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 2

EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_NFL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, TIME_STUDY)
  ) %>%
  mutate(Diff=(NFL-Baseline)/TIME_STUDY)  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )






# ALL Plasma NFL Total Year 3

EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_NFL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(NFL),
    sd=sd(NFL),
    median=median(NFL),
    Q1 = quantile(NFL, 0.25),
    Q3 = quantile(NFL, 0.75),
    min=min(NFL),
    max=max(NFL),
    n=n(),
    skewness = skewness(NFL, na.rm = TRUE),
    kurtosis = kurtosis(NFL, na.rm = TRUE),
    CI_lower = t.test(NFL)$conf.int[1],
    CI_upper = t.test(NFL)$conf.int[2],
    shapiro_w = shapiro.test(NFL)$statistic,
    shapiro_p = shapiro.test(NFL)$p.value
  )



# Elapsed Time Year 3


EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_NFL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 3


EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_NFL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL)
  ) %>%
  mutate(Diff=(NFL-Baseline))  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 3

EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_NFL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, TIME_STUDY)
  ) %>%
  mutate(Diff=(NFL-Baseline)/TIME_STUDY)  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )



# -----------
# Target MSA Entire  MSA P vs MSA C  -----------------------

# ALL Plasma NFL Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(NFL),
    sd=sd(NFL),
    median=median(NFL),
    Q1 = quantile(NFL, 0.25),
    Q3 = quantile(NFL, 0.75),
    min=min(NFL),
    max=max(NFL),
    n=n(),
    skewness = skewness(NFL, na.rm = TRUE),
    kurtosis = kurtosis(NFL, na.rm = TRUE),
    CI_lower = t.test(NFL)$conf.int[1],
    CI_upper = t.test(NFL)$conf.int[2],
    shapiro_w = shapiro.test(NFL)$statistic,
    shapiro_p = shapiro.test(NFL)$p.value
  )


# ALL Plasma NFL Total Year 1

EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_NFL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(NFL),
    sd=sd(NFL),
    median=median(NFL),
    Q1 = quantile(NFL, 0.25),
    Q3 = quantile(NFL, 0.75),
    min=min(NFL),
    max=max(NFL),
    n=n(),
    skewness = skewness(NFL, na.rm = TRUE),
    kurtosis = kurtosis(NFL, na.rm = TRUE),
    CI_lower = t.test(NFL)$conf.int[1],
    CI_upper = t.test(NFL)$conf.int[2],
    shapiro_w = shapiro.test(NFL)$statistic,
    shapiro_p = shapiro.test(NFL)$p.value
  )



# Elapsed Time Year 1


EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_NFL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, DIAG, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 1


EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_NFL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, DIAG)
  ) %>%
  mutate(Diff=(NFL-Baseline))  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 1

EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_NFL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, DIAG, TIME_STUDY)
  ) %>%
  mutate(Diff=(NFL-Baseline)/TIME_STUDY)  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )













# ALL Plasma NFL Total Year 2

EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_NFL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(NFL),
    sd=sd(NFL),
    median=median(NFL),
    Q1 = quantile(NFL, 0.25),
    Q3 = quantile(NFL, 0.75),
    min=min(NFL),
    max=max(NFL),
    n=n(),
    skewness = skewness(NFL, na.rm = TRUE),
    kurtosis = kurtosis(NFL, na.rm = TRUE),
    CI_lower = t.test(NFL)$conf.int[1],
    CI_upper = t.test(NFL)$conf.int[2],
    shapiro_w = shapiro.test(NFL)$statistic,
    shapiro_p = shapiro.test(NFL)$p.value
  )



# Elapsed Time Year 2


EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_NFL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, DIAG, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 2


EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_NFL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, DIAG)
  ) %>%
  mutate(Diff=(NFL-Baseline))  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 2

EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_NFL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, DIAG, TIME_STUDY)
  ) %>%
  mutate(Diff=(NFL-Baseline)/TIME_STUDY)  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )






# ALL Plasma NFL Total Year 3

EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_NFL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(NFL),
    sd=sd(NFL),
    median=median(NFL),
    Q1 = quantile(NFL, 0.25),
    Q3 = quantile(NFL, 0.75),
    min=min(NFL),
    max=max(NFL),
    n=n(),
    skewness = skewness(NFL, na.rm = TRUE),
    kurtosis = kurtosis(NFL, na.rm = TRUE),
    CI_lower = t.test(NFL)$conf.int[1],
    CI_upper = t.test(NFL)$conf.int[2],
    shapiro_w = shapiro.test(NFL)$statistic,
    shapiro_p = shapiro.test(NFL)$p.value
  )



# Elapsed Time Year 3


EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_NFL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, DIAG, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 3


EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_NFL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, DIAG)
  ) %>%
  mutate(Diff=(NFL-Baseline))  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 3

EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_NFL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, DIAG, TIME_STUDY)
  ) %>%
  mutate(Diff=(NFL-Baseline)/TIME_STUDY)  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )



# -----------
# Target MSA Entire  Probable vs Possible  -----------------------

# ALL Plasma NFL Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(NFL),
    sd=sd(NFL),
    median=median(NFL),
    Q1 = quantile(NFL, 0.25),
    Q3 = quantile(NFL, 0.75),
    min=min(NFL),
    max=max(NFL),
    n=n(),
    skewness = skewness(NFL, na.rm = TRUE),
    kurtosis = kurtosis(NFL, na.rm = TRUE),
    CI_lower = t.test(NFL)$conf.int[1],
    CI_upper = t.test(NFL)$conf.int[2],
    shapiro_w = shapiro.test(NFL)$statistic,
    shapiro_p = shapiro.test(NFL)$p.value
  )


# ALL Plasma NFL Total Year 1

EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_NFL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(NFL),
    sd=sd(NFL),
    median=median(NFL),
    Q1 = quantile(NFL, 0.25),
    Q3 = quantile(NFL, 0.75),
    min=min(NFL),
    max=max(NFL),
    n=n(),
    skewness = skewness(NFL, na.rm = TRUE),
    kurtosis = kurtosis(NFL, na.rm = TRUE),
    CI_lower = t.test(NFL)$conf.int[1],
    CI_upper = t.test(NFL)$conf.int[2],
    shapiro_w = shapiro.test(NFL)$statistic,
    shapiro_p = shapiro.test(NFL)$p.value
  )



# Elapsed Time Year 1


EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_NFL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, DIAGNIV, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 1


EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_NFL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, DIAGNIV)
  ) %>%
  mutate(Diff=(NFL-Baseline))  %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 1

EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_NFL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, DIAGNIV, TIME_STUDY)
  ) %>%
  mutate(Diff=(NFL-Baseline)/TIME_STUDY)  %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )













# ALL Plasma NFL Total Year 2

EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_NFL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(NFL),
    sd=sd(NFL),
    median=median(NFL),
    Q1 = quantile(NFL, 0.25),
    Q3 = quantile(NFL, 0.75),
    min=min(NFL),
    max=max(NFL),
    n=n(),
    skewness = skewness(NFL, na.rm = TRUE),
    kurtosis = kurtosis(NFL, na.rm = TRUE),
    CI_lower = t.test(NFL)$conf.int[1],
    CI_upper = t.test(NFL)$conf.int[2],
    shapiro_w = shapiro.test(NFL)$statistic,
    shapiro_p = shapiro.test(NFL)$p.value
  )



# Elapsed Time Year 2


EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_NFL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, DIAGNIV, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 2


EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_NFL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, DIAGNIV)
  ) %>%
  mutate(Diff=(NFL-Baseline))  %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 2

EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_NFL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, DIAGNIV, TIME_STUDY)
  ) %>%
  mutate(Diff=(NFL-Baseline)/TIME_STUDY)  %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )






# ALL Plasma NFL Total Year 3

EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_NFL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(NFL),
    sd=sd(NFL),
    median=median(NFL),
    Q1 = quantile(NFL, 0.25),
    Q3 = quantile(NFL, 0.75),
    min=min(NFL),
    max=max(NFL),
    n=n(),
    skewness = skewness(NFL, na.rm = TRUE),
    kurtosis = kurtosis(NFL, na.rm = TRUE),
    CI_lower = t.test(NFL)$conf.int[1],
    CI_upper = t.test(NFL)$conf.int[2],
    shapiro_w = shapiro.test(NFL)$statistic,
    shapiro_p = shapiro.test(NFL)$p.value
  )



# Elapsed Time Year 3


EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_NFL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, DIAGNIV, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 3


EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_NFL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, DIAGNIV)
  ) %>%
  mutate(Diff=(NFL-Baseline))  %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 3

EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_NFL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, DIAGNIV, TIME_STUDY)
  ) %>%
  mutate(Diff=(NFL-Baseline)/TIME_STUDY)  %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )

# ---------------


# All data dates groups  --------
dataCohorteManaged <- readRDS("Source/dataCohorteManaged.rds")

Visits_zero <- dataCohorteManaged  %>% select(NUM, dateVisite0) %>% distinct()

AllMSA_Pop_Baseline_671 <- fread("Source/AllMSA_Pop_Baseline_671.txt")
EarlyCT_Pop_Baseline_319 <- fread("Source/EarlyCT_Pop_Baseline_319.txt")


# ----------
# CSF NFL -----------------------------

biofluidsForPaulo <- readRDS("Source/biofluidsForPaulo.rds")

CSF_NFL <- biofluidsForPaulo %>% rename("NUM"="idGlobal") %>% 
  select(NUM, datePrelevement, nfl_pgml_csf, nfl_ipp_pgml_csf, nfl_pgml_csf_fusion)

CSF_NFL <- Visits_zero %>% inner_join(CSF_NFL) %>%
  mutate(TIME_STUDY=time_length(difftime(datePrelevement, dateVisite0 ), "years")) %>%
  mutate(Year= ifelse(TIME_STUDY<=0.5, 0,
                      ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                             ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                    ifelse(TIME_STUDY>=2.5 ,3, NA))))) 

CSF_NFL <- CSF_NFL %>% gather(original, NFL, nfl_pgml_csf:nfl_pgml_csf_fusion) %>%
  drop_na() %>% arrange(NUM, datePrelevement) %>%
  group_by(NUM, datePrelevement) %>% mutate(NFL=mean(NFL)) %>%
  select(-original) %>% distinct()

# ----------
# Overall MSA Entire  -----------------------

# ALL CSF NFL Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(CSF_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(NFL),
    sd=sd(NFL),
    median=median(NFL),
    Q1 = quantile(NFL, 0.25),
    Q3 = quantile(NFL, 0.75),
    min=min(NFL),
    max=max(NFL),
    n=n(),
    skewness = skewness(NFL, na.rm = TRUE),
    kurtosis = kurtosis(NFL, na.rm = TRUE),
    CI_lower = t.test(NFL)$conf.int[1],
    CI_upper = t.test(NFL)$conf.int[2],
    shapiro_w = shapiro.test(NFL)$statistic,
    shapiro_p = shapiro.test(NFL)$p.value
  )


# ALL CSF NFL Total Year 1

AllMSA_Pop_Baseline_671 %>% inner_join(CSF_NFL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(NFL),
    sd=sd(NFL),
    median=median(NFL),
    Q1 = quantile(NFL, 0.25),
    Q3 = quantile(NFL, 0.75),
    min=min(NFL),
    max=max(NFL),
    n=n(),
    skewness = skewness(NFL, na.rm = TRUE),
    kurtosis = kurtosis(NFL, na.rm = TRUE),
    CI_lower = t.test(NFL)$conf.int[1],
    CI_upper = t.test(NFL)$conf.int[2],
    shapiro_w = shapiro.test(NFL)$statistic,
    shapiro_p = shapiro.test(NFL)$p.value
  )



# Elapsed Time Year 1


AllMSA_Pop_Baseline_671 %>% inner_join(CSF_NFL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 1


AllMSA_Pop_Baseline_671 %>% inner_join(CSF_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(CSF_NFL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL)
  ) %>%
  mutate(Diff=(NFL-Baseline))  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 1

AllMSA_Pop_Baseline_671 %>% inner_join(CSF_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(CSF_NFL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, TIME_STUDY)
  ) %>%
  mutate(Diff=(NFL-Baseline)/TIME_STUDY)  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )













# ALL CSF NFL Total Year 2

AllMSA_Pop_Baseline_671 %>% inner_join(CSF_NFL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(NFL),
    sd=sd(NFL),
    median=median(NFL),
    Q1 = quantile(NFL, 0.25),
    Q3 = quantile(NFL, 0.75),
    min=min(NFL),
    max=max(NFL),
    n=n(),
    skewness = skewness(NFL, na.rm = TRUE),
    kurtosis = kurtosis(NFL, na.rm = TRUE),
    CI_lower = t.test(NFL)$conf.int[1],
    CI_upper = t.test(NFL)$conf.int[2],
    shapiro_w = shapiro.test(NFL)$statistic,
    shapiro_p = shapiro.test(NFL)$p.value
  )



# Elapsed Time Year 2


AllMSA_Pop_Baseline_671 %>% inner_join(CSF_NFL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 2


AllMSA_Pop_Baseline_671 %>% inner_join(CSF_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(CSF_NFL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL)
  ) %>%
  mutate(Diff=(NFL-Baseline))  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 2

AllMSA_Pop_Baseline_671 %>% inner_join(CSF_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(CSF_NFL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, TIME_STUDY)
  ) %>%
  mutate(Diff=(NFL-Baseline)/TIME_STUDY)  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )






# ALL CSF NFL Total Year 3

AllMSA_Pop_Baseline_671 %>% inner_join(CSF_NFL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(NFL),
    sd=sd(NFL),
    median=median(NFL),
    Q1 = quantile(NFL, 0.25),
    Q3 = quantile(NFL, 0.75),
    min=min(NFL),
    max=max(NFL),
    n=n(),
    skewness = skewness(NFL, na.rm = TRUE),
    kurtosis = kurtosis(NFL, na.rm = TRUE),
    CI_lower = t.test(NFL)$conf.int[1],
    CI_upper = t.test(NFL)$conf.int[2],
    shapiro_w = shapiro.test(NFL)$statistic,
    shapiro_p = shapiro.test(NFL)$p.value
  )



# Elapsed Time Year 3


AllMSA_Pop_Baseline_671 %>% inner_join(CSF_NFL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 3


AllMSA_Pop_Baseline_671 %>% inner_join(CSF_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(CSF_NFL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL)
  ) %>%
  mutate(Diff=(NFL-Baseline))  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 3

AllMSA_Pop_Baseline_671 %>% inner_join(CSF_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(CSF_NFL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, TIME_STUDY)
  ) %>%
  mutate(Diff=(NFL-Baseline)/TIME_STUDY)  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )



# -----------
# Overall MSA Entire  MSA P vs MSA C  -----------------------

# ALL CSF NFL Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(CSF_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(NFL),
    sd=sd(NFL),
    median=median(NFL),
    Q1 = quantile(NFL, 0.25),
    Q3 = quantile(NFL, 0.75),
    min=min(NFL),
    max=max(NFL),
    n=n(),
    skewness = skewness(NFL, na.rm = TRUE),
    kurtosis = kurtosis(NFL, na.rm = TRUE),
    CI_lower = t.test(NFL)$conf.int[1],
    CI_upper = t.test(NFL)$conf.int[2],
    shapiro_w = shapiro.test(NFL)$statistic,
    shapiro_p = shapiro.test(NFL)$p.value
  )


# ALL CSF NFL Total Year 1

AllMSA_Pop_Baseline_671 %>% inner_join(CSF_NFL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(NFL),
    sd=sd(NFL),
    median=median(NFL),
    Q1 = quantile(NFL, 0.25),
    Q3 = quantile(NFL, 0.75),
    min=min(NFL),
    max=max(NFL),
    n=n(),
    skewness = skewness(NFL, na.rm = TRUE),
    kurtosis = kurtosis(NFL, na.rm = TRUE),
    CI_lower = t.test(NFL)$conf.int[1],
    CI_upper = t.test(NFL)$conf.int[2],
    shapiro_w = shapiro.test(NFL)$statistic,
    shapiro_p = shapiro.test(NFL)$p.value
  )



# Elapsed Time Year 1


AllMSA_Pop_Baseline_671 %>% inner_join(CSF_NFL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, DIAG, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 1


AllMSA_Pop_Baseline_671 %>% inner_join(CSF_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(CSF_NFL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, DIAG)
  ) %>%
  mutate(Diff=(NFL-Baseline))  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 1

AllMSA_Pop_Baseline_671 %>% inner_join(CSF_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(CSF_NFL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, DIAG, TIME_STUDY)
  ) %>%
  mutate(Diff=(NFL-Baseline)/TIME_STUDY)  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )













# ALL CSF NFL Total Year 2

AllMSA_Pop_Baseline_671 %>% inner_join(CSF_NFL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(NFL),
    sd=sd(NFL),
    median=median(NFL),
    Q1 = quantile(NFL, 0.25),
    Q3 = quantile(NFL, 0.75),
    min=min(NFL),
    max=max(NFL),
    n=n(),
    skewness = skewness(NFL, na.rm = TRUE),
    kurtosis = kurtosis(NFL, na.rm = TRUE),
    CI_lower = t.test(NFL)$conf.int[1],
    CI_upper = t.test(NFL)$conf.int[2],
    shapiro_w = shapiro.test(NFL)$statistic,
    shapiro_p = shapiro.test(NFL)$p.value
  )



# Elapsed Time Year 2


AllMSA_Pop_Baseline_671 %>% inner_join(CSF_NFL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, DIAG, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 2


AllMSA_Pop_Baseline_671 %>% inner_join(CSF_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(CSF_NFL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, DIAG)
  ) %>%
  mutate(Diff=(NFL-Baseline))  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 2

AllMSA_Pop_Baseline_671 %>% inner_join(CSF_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(CSF_NFL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, DIAG, TIME_STUDY)
  ) %>%
  mutate(Diff=(NFL-Baseline)/TIME_STUDY)  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )






# ALL CSF NFL Total Year 3

AllMSA_Pop_Baseline_671 %>% inner_join(CSF_NFL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(NFL),
    sd=sd(NFL),
    median=median(NFL),
    Q1 = quantile(NFL, 0.25),
    Q3 = quantile(NFL, 0.75),
    min=min(NFL),
    max=max(NFL),
    n=n(),
    skewness = skewness(NFL, na.rm = TRUE),
    kurtosis = kurtosis(NFL, na.rm = TRUE),
    CI_lower = t.test(NFL)$conf.int[1],
    CI_upper = t.test(NFL)$conf.int[2],
    shapiro_w = shapiro.test(NFL)$statistic,
    shapiro_p = shapiro.test(NFL)$p.value
  )



# Elapsed Time Year 3


AllMSA_Pop_Baseline_671 %>% inner_join(CSF_NFL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, DIAG, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 3


AllMSA_Pop_Baseline_671 %>% inner_join(CSF_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(CSF_NFL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, DIAG)
  ) %>%
  mutate(Diff=(NFL-Baseline))  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 3

AllMSA_Pop_Baseline_671 %>% inner_join(CSF_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(CSF_NFL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, DIAG, TIME_STUDY)
  ) %>%
  mutate(Diff=(NFL-Baseline)/TIME_STUDY)  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )



# -----------
# Overall MSA Entire  Probable vs Possible  -----------------------

# ALL CSF NFL Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(CSF_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(NFL),
    sd=sd(NFL),
    median=median(NFL),
    Q1 = quantile(NFL, 0.25),
    Q3 = quantile(NFL, 0.75),
    min=min(NFL),
    max=max(NFL),
    n=n(),
    skewness = skewness(NFL, na.rm = TRUE),
    kurtosis = kurtosis(NFL, na.rm = TRUE),
    CI_lower = t.test(NFL)$conf.int[1],
    CI_upper = t.test(NFL)$conf.int[2],
    shapiro_w = shapiro.test(NFL)$statistic,
    shapiro_p = shapiro.test(NFL)$p.value
  )


# ALL CSF NFL Total Year 1

AllMSA_Pop_Baseline_671 %>% inner_join(CSF_NFL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% filter(DIAGNIV=="POS") %>%
  summarise(
    mean=mean(NFL),
    sd=sd(NFL),
    median=median(NFL),
    Q1 = quantile(NFL, 0.25),
    Q3 = quantile(NFL, 0.75),
    min=min(NFL),
    max=max(NFL),
    n=n(),
    skewness = skewness(NFL, na.rm = TRUE),
    kurtosis = kurtosis(NFL, na.rm = TRUE),
    #CI_lower = t.test(NFL)$conf.int[1],
    #CI_upper = t.test(NFL)$conf.int[2],
    #shapiro_w = shapiro.test(NFL)$statistic,
    #shapiro_p = shapiro.test(NFL)$p.value
  )



# Elapsed Time Year 1


AllMSA_Pop_Baseline_671 %>% inner_join(CSF_NFL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, DIAGNIV, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>% filter(DIAGNIV=="PROB") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    #CI_lower = t.test(Diff)$conf.int[1],
    #CI_upper = t.test(Diff)$conf.int[2],
    #shapiro_w = shapiro.test(Diff)$statistic,
    #shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 1


AllMSA_Pop_Baseline_671 %>% inner_join(CSF_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(CSF_NFL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, DIAGNIV)
  ) %>%
  mutate(Diff=(NFL-Baseline))  %>% filter(DIAGNIV=="PROB") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )



# Standardized Year 1

AllMSA_Pop_Baseline_671 %>% inner_join(CSF_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(CSF_NFL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, DIAGNIV, TIME_STUDY)
  ) %>%
  mutate(Diff=(NFL-Baseline)/TIME_STUDY)  %>% filter(DIAGNIV=="POS") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    #skewness = skewness(Diff, na.rm = TRUE),
    #kurtosis = kurtosis(Diff, na.rm = TRUE),
    #CI_lower = t.test(Diff)$conf.int[1],
    #CI_upper = t.test(Diff)$conf.int[2],
    #shapiro_w = shapiro.test(Diff)$statistic,
    #shapiro_p = shapiro.test(Diff)$p.value
  )













# ALL CSF NFL Total Year 2

AllMSA_Pop_Baseline_671 %>% inner_join(CSF_NFL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% filter(DIAGNIV=="POS") %>%
  summarise(
    mean=mean(NFL),
    sd=sd(NFL),
    median=median(NFL),
    Q1 = quantile(NFL, 0.25),
    Q3 = quantile(NFL, 0.75),
    min=min(NFL),
    max=max(NFL),
    n=n(),
    #skewness = skewness(NFL, na.rm = TRUE),
    #kurtosis = kurtosis(NFL, na.rm = TRUE),
    #CI_lower = t.test(NFL)$conf.int[1],
    #CI_upper = t.test(NFL)$conf.int[2],
    #shapiro_w = shapiro.test(NFL)$statistic,
    #shapiro_p = shapiro.test(NFL)$p.value
  )



# Elapsed Time Year 2


AllMSA_Pop_Baseline_671 %>% inner_join(CSF_NFL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, DIAGNIV, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>% filter(DIAGNIV=="POS") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    #skewness = skewness(Diff, na.rm = TRUE),
    #kurtosis = kurtosis(Diff, na.rm = TRUE),
    #CI_lower = t.test(Diff)$conf.int[1],
    #CI_upper = t.test(Diff)$conf.int[2],
    #shapiro_w = shapiro.test(Diff)$statistic,
    #shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 2


AllMSA_Pop_Baseline_671 %>% inner_join(CSF_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(CSF_NFL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, DIAGNIV)
  ) %>%
  mutate(Diff=(NFL-Baseline))  %>% filter(DIAGNIV=="PROB") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 2

AllMSA_Pop_Baseline_671 %>% inner_join(CSF_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(CSF_NFL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, DIAGNIV, TIME_STUDY)
  ) %>%
  mutate(Diff=(NFL-Baseline)/TIME_STUDY)  %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )






# ALL CSF NFL Total Year 3

AllMSA_Pop_Baseline_671 %>% inner_join(CSF_NFL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(NFL),
    sd=sd(NFL),
    median=median(NFL),
    Q1 = quantile(NFL, 0.25),
    Q3 = quantile(NFL, 0.75),
    min=min(NFL),
    max=max(NFL),
    n=n(),
    skewness = skewness(NFL, na.rm = TRUE),
    kurtosis = kurtosis(NFL, na.rm = TRUE),
    CI_lower = t.test(NFL)$conf.int[1],
    CI_upper = t.test(NFL)$conf.int[2],
    shapiro_w = shapiro.test(NFL)$statistic,
    shapiro_p = shapiro.test(NFL)$p.value
  )



# Elapsed Time Year 3


AllMSA_Pop_Baseline_671 %>% inner_join(CSF_NFL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, DIAGNIV, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 3


AllMSA_Pop_Baseline_671 %>% inner_join(CSF_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(CSF_NFL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, DIAGNIV)
  ) %>%
  mutate(Diff=(NFL-Baseline))  %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 3

AllMSA_Pop_Baseline_671 %>% inner_join(CSF_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(CSF_NFL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, DIAGNIV, TIME_STUDY)
  ) %>%
  mutate(Diff=(NFL-Baseline)/TIME_STUDY)  %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )

# ---------------


# Target MSA Entire  -----------------------

# ALL CSF NFL Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(NFL),
    sd=sd(NFL),
    median=median(NFL),
    Q1 = quantile(NFL, 0.25),
    Q3 = quantile(NFL, 0.75),
    min=min(NFL),
    max=max(NFL),
    n=n(),
    skewness = skewness(NFL, na.rm = TRUE),
    kurtosis = kurtosis(NFL, na.rm = TRUE),
    CI_lower = t.test(NFL)$conf.int[1],
    CI_upper = t.test(NFL)$conf.int[2],
    shapiro_w = shapiro.test(NFL)$statistic,
    shapiro_p = shapiro.test(NFL)$p.value
  )


# ALL CSF NFL Total Year 1

EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_NFL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(NFL),
    sd=sd(NFL),
    median=median(NFL),
    Q1 = quantile(NFL, 0.25),
    Q3 = quantile(NFL, 0.75),
    min=min(NFL),
    max=max(NFL),
    n=n(),
    skewness = skewness(NFL, na.rm = TRUE),
    kurtosis = kurtosis(NFL, na.rm = TRUE),
    CI_lower = t.test(NFL)$conf.int[1],
    CI_upper = t.test(NFL)$conf.int[2],
    shapiro_w = shapiro.test(NFL)$statistic,
    shapiro_p = shapiro.test(NFL)$p.value
  )



# Elapsed Time Year 1


EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_NFL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 1


EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_NFL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL)
  ) %>%
  mutate(Diff=(NFL-Baseline))  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 1

EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_NFL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, TIME_STUDY)
  ) %>%
  mutate(Diff=(NFL-Baseline)/TIME_STUDY)  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )













# ALL CSF NFL Total Year 2

EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_NFL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(NFL),
    sd=sd(NFL),
    median=median(NFL),
    Q1 = quantile(NFL, 0.25),
    Q3 = quantile(NFL, 0.75),
    min=min(NFL),
    max=max(NFL),
    n=n(),
    skewness = skewness(NFL, na.rm = TRUE),
    kurtosis = kurtosis(NFL, na.rm = TRUE),
    CI_lower = t.test(NFL)$conf.int[1],
    CI_upper = t.test(NFL)$conf.int[2],
    shapiro_w = shapiro.test(NFL)$statistic,
    shapiro_p = shapiro.test(NFL)$p.value
  )



# Elapsed Time Year 2


EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_NFL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 2


EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_NFL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL)
  ) %>%
  mutate(Diff=(NFL-Baseline))  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 2

EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_NFL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, TIME_STUDY)
  ) %>%
  mutate(Diff=(NFL-Baseline)/TIME_STUDY)  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )






# ALL CSF NFL Total Year 3

EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_NFL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(NFL),
    sd=sd(NFL),
    median=median(NFL),
    Q1 = quantile(NFL, 0.25),
    Q3 = quantile(NFL, 0.75),
    min=min(NFL),
    max=max(NFL),
    n=n(),
    skewness = skewness(NFL, na.rm = TRUE),
    kurtosis = kurtosis(NFL, na.rm = TRUE),
    CI_lower = t.test(NFL)$conf.int[1],
    CI_upper = t.test(NFL)$conf.int[2],
    shapiro_w = shapiro.test(NFL)$statistic,
    shapiro_p = shapiro.test(NFL)$p.value
  )



# Elapsed Time Year 3


EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_NFL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 3


EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_NFL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL)
  ) %>%
  mutate(Diff=(NFL-Baseline))  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 3

EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_NFL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, TIME_STUDY)
  ) %>%
  mutate(Diff=(NFL-Baseline)/TIME_STUDY)  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )



# -----------
# Target MSA Entire  MSA P vs MSA C  -----------------------

# ALL CSF NFL Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(NFL),
    sd=sd(NFL),
    median=median(NFL),
    Q1 = quantile(NFL, 0.25),
    Q3 = quantile(NFL, 0.75),
    min=min(NFL),
    max=max(NFL),
    n=n(),
    skewness = skewness(NFL, na.rm = TRUE),
    kurtosis = kurtosis(NFL, na.rm = TRUE),
    CI_lower = t.test(NFL)$conf.int[1],
    CI_upper = t.test(NFL)$conf.int[2],
    shapiro_w = shapiro.test(NFL)$statistic,
    shapiro_p = shapiro.test(NFL)$p.value
  )


# ALL CSF NFL Total Year 1

EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_NFL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(NFL),
    sd=sd(NFL),
    median=median(NFL),
    Q1 = quantile(NFL, 0.25),
    Q3 = quantile(NFL, 0.75),
    min=min(NFL),
    max=max(NFL),
    n=n(),
    skewness = skewness(NFL, na.rm = TRUE),
    kurtosis = kurtosis(NFL, na.rm = TRUE),
    CI_lower = t.test(NFL)$conf.int[1],
    CI_upper = t.test(NFL)$conf.int[2],
    shapiro_w = shapiro.test(NFL)$statistic,
    shapiro_p = shapiro.test(NFL)$p.value
  )



# Elapsed Time Year 1


EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_NFL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, DIAG, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 1


EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_NFL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, DIAG)
  ) %>%
  mutate(Diff=(NFL-Baseline))  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 1

EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_NFL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, DIAG, TIME_STUDY)
  ) %>%
  mutate(Diff=(NFL-Baseline)/TIME_STUDY)  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )













# ALL CSF NFL Total Year 2

EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_NFL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% filter(DIAG=="CB") %>%
  summarise(
    mean=mean(NFL),
    sd=sd(NFL),
    median=median(NFL),
    Q1 = quantile(NFL, 0.25),
    Q3 = quantile(NFL, 0.75),
    min=min(NFL),
    max=max(NFL),
    n=n(),
    #skewness = skewness(NFL, na.rm = TRUE),
    #kurtosis = kurtosis(NFL, na.rm = TRUE),
    #CI_lower = t.test(NFL)$conf.int[1],
    #CI_upper = t.test(NFL)$conf.int[2],
    #shapiro_w = shapiro.test(NFL)$statistic,
    #shapiro_p = shapiro.test(NFL)$p.value
  )



# Elapsed Time Year 2


EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_NFL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, DIAG, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>% filter(DIAG=="CB") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    #skewness = skewness(Diff, na.rm = TRUE),
    #kurtosis = kurtosis(Diff, na.rm = TRUE),
    #CI_lower = t.test(Diff)$conf.int[1],
    #CI_upper = t.test(Diff)$conf.int[2],
    #shapiro_w = shapiro.test(Diff)$statistic,
    #shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 2


EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_NFL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, DIAG)
  ) %>%
  mutate(Diff=(NFL-Baseline))  %>% filter(DIAG=="CB") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    #skewness = skewness(Diff, na.rm = TRUE),
    #kurtosis = kurtosis(Diff, na.rm = TRUE),
    #CI_lower = t.test(Diff)$conf.int[1],
    #CI_upper = t.test(Diff)$conf.int[2],
    #shapiro_w = shapiro.test(Diff)$statistic,
    #shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 2

EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_NFL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, DIAG, TIME_STUDY)
  ) %>%
  mutate(Diff=(NFL-Baseline)/TIME_STUDY)  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )






# ALL CSF NFL Total Year 3

EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_NFL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(NFL),
    sd=sd(NFL),
    median=median(NFL),
    Q1 = quantile(NFL, 0.25),
    Q3 = quantile(NFL, 0.75),
    min=min(NFL),
    max=max(NFL),
    n=n(),
    skewness = skewness(NFL, na.rm = TRUE),
    kurtosis = kurtosis(NFL, na.rm = TRUE),
    CI_lower = t.test(NFL)$conf.int[1],
    CI_upper = t.test(NFL)$conf.int[2],
    shapiro_w = shapiro.test(NFL)$statistic,
    shapiro_p = shapiro.test(NFL)$p.value
  )



# Elapsed Time Year 3


EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_NFL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, DIAG, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 3


EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_NFL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, DIAG)
  ) %>%
  mutate(Diff=(NFL-Baseline))  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 3

EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_NFL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, DIAG, TIME_STUDY)
  ) %>%
  mutate(Diff=(NFL-Baseline)/TIME_STUDY)  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )



# -----------
# Target MSA Entire  Probable vs Possible  -----------------------

# ALL CSF NFL Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(NFL),
    sd=sd(NFL),
    median=median(NFL),
    Q1 = quantile(NFL, 0.25),
    Q3 = quantile(NFL, 0.75),
    min=min(NFL),
    max=max(NFL),
    n=n(),
    skewness = skewness(NFL, na.rm = TRUE),
    kurtosis = kurtosis(NFL, na.rm = TRUE),
    CI_lower = t.test(NFL)$conf.int[1],
    CI_upper = t.test(NFL)$conf.int[2],
    shapiro_w = shapiro.test(NFL)$statistic,
    shapiro_p = shapiro.test(NFL)$p.value
  )


# ALL CSF NFL Total Year 1

EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_NFL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% filter(DIAGNIV=="POS") %>%
  summarise(
    mean=mean(NFL),
    sd=sd(NFL),
    median=median(NFL),
    Q1 = quantile(NFL, 0.25),
    Q3 = quantile(NFL, 0.75),
    min=min(NFL),
    max=max(NFL),
    n=n(),
    #skewness = skewness(NFL, na.rm = TRUE),
    #kurtosis = kurtosis(NFL, na.rm = TRUE),
    #CI_lower = t.test(NFL)$conf.int[1],
    #CI_upper = t.test(NFL)$conf.int[2],
    #shapiro_w = shapiro.test(NFL)$statistic,
    #shapiro_p = shapiro.test(NFL)$p.value
  )



# Elapsed Time Year 1


EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_NFL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, DIAGNIV, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>% filter(DIAGNIV=="PROB") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    #skewness = skewness(Diff, na.rm = TRUE),
    #kurtosis = kurtosis(Diff, na.rm = TRUE),
    #CI_lower = t.test(Diff)$conf.int[1],
    #CI_upper = t.test(Diff)$conf.int[2],
    #shapiro_w = shapiro.test(Diff)$statistic,
    #shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 1


EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_NFL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, DIAGNIV)
  ) %>%
  mutate(Diff=(NFL-Baseline))  %>% filter(DIAGNIV=="PROB") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 1

EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_NFL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, DIAGNIV, TIME_STUDY)
  ) %>%
  mutate(Diff=(NFL-Baseline)/TIME_STUDY)  %>% filter(DIAGNIV=="PROB") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )













# ALL CSF NFL Total Year 2

EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_NFL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% filter(DIAGNIV=="POS") %>%
  summarise(
    mean=mean(NFL),
    sd=sd(NFL),
    median=median(NFL),
    Q1 = quantile(NFL, 0.25),
    Q3 = quantile(NFL, 0.75),
    min=min(NFL),
    max=max(NFL),
    n=n(),
    #skewness = skewness(NFL, na.rm = TRUE),
    #kurtosis = kurtosis(NFL, na.rm = TRUE),
    #CI_lower = t.test(NFL)$conf.int[1],
    #CI_upper = t.test(NFL)$conf.int[2],
    #shapiro_w = shapiro.test(NFL)$statistic,
    #shapiro_p = shapiro.test(NFL)$p.value
  )



# Elapsed Time Year 2


EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_NFL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, DIAGNIV, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>% filter(DIAGNIV=="PROB") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    #skewness = skewness(Diff, na.rm = TRUE),
    #kurtosis = kurtosis(Diff, na.rm = TRUE),
    #CI_lower = t.test(Diff)$conf.int[1],
    #CI_upper = t.test(Diff)$conf.int[2],
    #shapiro_w = shapiro.test(Diff)$statistic,
    #shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 2


EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_NFL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, DIAGNIV)
  ) %>%
  mutate(Diff=(NFL-Baseline))  %>% filter(DIAGNIV=="PROB") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    #skewness = skewness(Diff, na.rm = TRUE),
    #kurtosis = kurtosis(Diff, na.rm = TRUE),
    #CI_lower = t.test(Diff)$conf.int[1],
    #CI_upper = t.test(Diff)$conf.int[2],
    #shapiro_w = shapiro.test(Diff)$statistic,
    #shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 2

EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_NFL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, DIAGNIV, TIME_STUDY)
  ) %>%
  mutate(Diff=(NFL-Baseline)/TIME_STUDY)  %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )






# ALL CSF NFL Total Year 3

EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_NFL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(NFL),
    sd=sd(NFL),
    median=median(NFL),
    Q1 = quantile(NFL, 0.25),
    Q3 = quantile(NFL, 0.75),
    min=min(NFL),
    max=max(NFL),
    n=n(),
    skewness = skewness(NFL, na.rm = TRUE),
    kurtosis = kurtosis(NFL, na.rm = TRUE),
    CI_lower = t.test(NFL)$conf.int[1],
    CI_upper = t.test(NFL)$conf.int[2],
    shapiro_w = shapiro.test(NFL)$statistic,
    shapiro_p = shapiro.test(NFL)$p.value
  )



# Elapsed Time Year 3


EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_NFL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, DIAGNIV, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 3


EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_NFL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, DIAGNIV)
  ) %>%
  mutate(Diff=(NFL-Baseline))  %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 3

EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_NFL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL, DIAGNIV, TIME_STUDY)
  ) %>%
  mutate(Diff=(NFL-Baseline)/TIME_STUDY)  %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )

# ---------------


# All data dates groups  --------
dataCohorteManaged <- readRDS("Source/dataCohorteManaged.rds")

Visits_zero <- dataCohorteManaged  %>% select(NUM, dateVisite0) %>% distinct()

AllMSA_Pop_Baseline_671 <- fread("Source/AllMSA_Pop_Baseline_671.txt")
EarlyCT_Pop_Baseline_319 <- fread("Source/EarlyCT_Pop_Baseline_319.txt")


# ----------
# TOTAL Plasma aSYN -----------------------------

biofluidsForPaulo <- readRDS("Source/biofluidsForPaulo.rds")
names(biofluidsForPaulo)
Plasma_SYN <- biofluidsForPaulo %>% rename("NUM"="idGlobal") %>% 
  select(NUM, datePrelevement, total_alpha_syn_q1_pgml_plas, total_alpha_syn_q2_pgml_plas)

Plasma_SYN <- Visits_zero %>% inner_join(Plasma_SYN) %>%
  mutate(TIME_STUDY=time_length(difftime(datePrelevement, dateVisite0 ), "years")) %>%
  mutate(Year= ifelse(TIME_STUDY<=0.5, 0,
                      ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                             ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                    ifelse(TIME_STUDY>=2.5 ,3, NA))))) 

Plasma_SYN <- Plasma_SYN %>% gather(original, SYN, total_alpha_syn_q1_pgml_plas:total_alpha_syn_q2_pgml_plas) %>%
  drop_na() %>% arrange(NUM, datePrelevement) %>%
  group_by(NUM, datePrelevement) %>% mutate(SYN=mean(SYN)) %>%
  select(-original) %>% distinct()

# ----------
# Overall MSA Entire  -----------------------

# ALL Plasma SYN Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    skewness = skewness(SYN, na.rm = TRUE),
    kurtosis = kurtosis(SYN, na.rm = TRUE),
    CI_lower = t.test(SYN)$conf.int[1],
    CI_upper = t.test(SYN)$conf.int[2],
    shapiro_w = shapiro.test(SYN)$statistic,
    shapiro_p = shapiro.test(SYN)$p.value
  )


# ALL Plasma SYN Total Year 1

AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    skewness = skewness(SYN, na.rm = TRUE),
    kurtosis = kurtosis(SYN, na.rm = TRUE),
    CI_lower = t.test(SYN)$conf.int[1],
    CI_upper = t.test(SYN)$conf.int[2],
    #shapiro_w = shapiro.test(SYN)$statistic,
    #shapiro_p = shapiro.test(SYN)$p.value
  )



# Elapsed Time Year 1


AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    #shapiro_w = shapiro.test(Diff)$statistic,
    #shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 1


AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN)
  ) %>%
  mutate(Diff=(SYN-Baseline))  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    #skewness = skewness(Diff, na.rm = TRUE),
    #kurtosis = kurtosis(Diff, na.rm = TRUE),
    #CI_lower = t.test(Diff)$conf.int[1],
    #CI_upper = t.test(Diff)$conf.int[2],
    #shapiro_w = shapiro.test(Diff)$statistic,
    #shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 1

AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, TIME_STUDY)
  ) %>%
  mutate(Diff=(SYN-Baseline)/TIME_STUDY)  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )













# ALL Plasma SYN Total Year 2

AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    skewness = skewness(SYN, na.rm = TRUE),
    kurtosis = kurtosis(SYN, na.rm = TRUE),
    CI_lower = t.test(SYN)$conf.int[1],
    CI_upper = t.test(SYN)$conf.int[2],
    shapiro_w = shapiro.test(SYN)$statistic,
    shapiro_p = shapiro.test(SYN)$p.value
  )



# Elapsed Time Year 2


AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 2


AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN)
  ) %>%
  mutate(Diff=(SYN-Baseline))  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    #CI_lower = t.test(Diff)$conf.int[1],
    #CI_upper = t.test(Diff)$conf.int[2],
    #shapiro_w = shapiro.test(Diff)$statistic,
    #shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 2

AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, TIME_STUDY)
  ) %>%
  mutate(Diff=(SYN-Baseline)/TIME_STUDY)  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )






# ALL Plasma SYN Total Year 3

AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    skewness = skewness(SYN, na.rm = TRUE),
    kurtosis = kurtosis(SYN, na.rm = TRUE),
    CI_lower = t.test(SYN)$conf.int[1],
    CI_upper = t.test(SYN)$conf.int[2],
    shapiro_w = shapiro.test(SYN)$statistic,
    shapiro_p = shapiro.test(SYN)$p.value
  )



# Elapsed Time Year 3


AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 3


AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN)
  ) %>%
  mutate(Diff=(SYN-Baseline))  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 3

AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, TIME_STUDY)
  ) %>%
  mutate(Diff=(SYN-Baseline)/TIME_STUDY)  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )



# -----------
# Overall MSA Entire  MSA P vs MSA C  -----------------------

# ALL Plasma SYN Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    skewness = skewness(SYN, na.rm = TRUE),
    kurtosis = kurtosis(SYN, na.rm = TRUE),
    CI_lower = t.test(SYN)$conf.int[1],
    CI_upper = t.test(SYN)$conf.int[2],
    shapiro_w = shapiro.test(SYN)$statistic,
    shapiro_p = shapiro.test(SYN)$p.value
  )


# ALL Plasma SYN Total Year 1

AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% filter(DIAG=="CB") %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    #skewness = skewness(SYN, na.rm = TRUE),
    #kurtosis = kurtosis(SYN, na.rm = TRUE),
    #CI_lower = t.test(SYN)$conf.int[1],
    #CI_upper = t.test(SYN)$conf.int[2],
    #shapiro_w = shapiro.test(SYN)$statistic,
    #shapiro_p = shapiro.test(SYN)$p.value
  )



# Elapsed Time Year 1


AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAG, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>% filter(DIAG=="CB") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    #skewness = skewness(Diff, na.rm = TRUE),
    #kurtosis = kurtosis(Diff, na.rm = TRUE),
    #CI_lower = t.test(Diff)$conf.int[1],
    #CI_upper = t.test(Diff)$conf.int[2],
    #shapiro_w = shapiro.test(Diff)$statistic,
    #shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 1


AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAG)
  ) %>%
  mutate(Diff=(SYN-Baseline))  %>% filter(DIAG=="PD") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    #skewness = skewness(Diff, na.rm = TRUE),
    #kurtosis = kurtosis(Diff, na.rm = TRUE),
    #CI_lower = t.test(Diff)$conf.int[1],
    #CI_upper = t.test(Diff)$conf.int[2],
    #shapiro_w = shapiro.test(Diff)$statistic,
    #shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 1

AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAG, TIME_STUDY)
  ) %>%
  mutate(Diff=(SYN-Baseline)/TIME_STUDY)  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )













# ALL Plasma SYN Total Year 2

AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% filter(DIAG=="CB") %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    #skewness = skewness(SYN, na.rm = TRUE),
    #kurtosis = kurtosis(SYN, na.rm = TRUE),
    #CI_lower = t.test(SYN)$conf.int[1],
    #CI_upper = t.test(SYN)$conf.int[2],
    #shapiro_w = shapiro.test(SYN)$statistic,
    #shapiro_p = shapiro.test(SYN)$p.value
  )



# Elapsed Time Year 2


AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAG, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>% filter(DIAG=="CB") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    #skewness = skewness(Diff, na.rm = TRUE),
    #kurtosis = kurtosis(Diff, na.rm = TRUE),
    #CI_lower = t.test(Diff)$conf.int[1],
    #CI_upper = t.test(Diff)$conf.int[2],
    #shapiro_w = shapiro.test(Diff)$statistic,
    #shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 2


AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAG)
  ) %>%
  mutate(Diff=(SYN-Baseline))  %>% filter(DIAG=="PD") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    #skewness = skewness(Diff, na.rm = TRUE),
    #kurtosis = kurtosis(Diff, na.rm = TRUE),
    #CI_lower = t.test(Diff)$conf.int[1],
    #CI_upper = t.test(Diff)$conf.int[2],
    #shapiro_w = shapiro.test(Diff)$statistic,
    #shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 2

AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAG, TIME_STUDY)
  ) %>%
  mutate(Diff=(SYN-Baseline)/TIME_STUDY)  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )






# ALL Plasma SYN Total Year 3

AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    skewness = skewness(SYN, na.rm = TRUE),
    kurtosis = kurtosis(SYN, na.rm = TRUE),
    CI_lower = t.test(SYN)$conf.int[1],
    CI_upper = t.test(SYN)$conf.int[2],
    shapiro_w = shapiro.test(SYN)$statistic,
    shapiro_p = shapiro.test(SYN)$p.value
  )



# Elapsed Time Year 3


AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAG, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>% filter(DIAG=="CB") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 3


AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAG)
  ) %>%
  mutate(Diff=(SYN-Baseline))  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 3

AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAG, TIME_STUDY)
  ) %>%
  mutate(Diff=(SYN-Baseline)/TIME_STUDY)  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )



# -----------
# Overall MSA Entire  Probable vs Possible  -----------------------

# ALL Plasma SYN Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% filter(DIAGNIV=="POS") %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    #skewness = skewness(SYN, na.rm = TRUE),
    #kurtosis = kurtosis(SYN, na.rm = TRUE),
    #CI_lower = t.test(SYN)$conf.int[1],
    #CI_upper = t.test(SYN)$conf.int[2],
    #shapiro_w = shapiro.test(SYN)$statistic,
    #shapiro_p = shapiro.test(SYN)$p.value
  )


# ALL Plasma SYN Total Year 1

AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% filter(DIAGNIV=="PROB") %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    skewness = skewness(SYN, na.rm = TRUE),
    kurtosis = kurtosis(SYN, na.rm = TRUE),
    #CI_lower = t.test(SYN)$conf.int[1],
    #CI_upper = t.test(SYN)$conf.int[2],
    #shapiro_w = shapiro.test(SYN)$statistic,
    #shapiro_p = shapiro.test(SYN)$p.value
  )



# Elapsed Time Year 1


AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAGNIV, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>% filter(DIAGNIV=="PROB") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    #CI_lower = t.test(Diff)$conf.int[1],
    #CI_upper = t.test(Diff)$conf.int[2],
    #shapiro_w = shapiro.test(Diff)$statistic,
    #shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 1


AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAGNIV)
  ) %>%
  mutate(Diff=(SYN-Baseline))  %>% filter(DIAGNIV=="PROB") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )



# Standardized Year 1

AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAGNIV, TIME_STUDY)
  ) %>%
  mutate(Diff=(SYN-Baseline)/TIME_STUDY)  %>% filter(DIAGNIV=="POS") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    #skewness = skewness(Diff, na.rm = TRUE),
    #kurtosis = kurtosis(Diff, na.rm = TRUE),
    #CI_lower = t.test(Diff)$conf.int[1],
    #CI_upper = t.test(Diff)$conf.int[2],
    #shapiro_w = shapiro.test(Diff)$statistic,
    #shapiro_p = shapiro.test(Diff)$p.value
  )













# ALL Plasma SYN Total Year 2

AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% filter(DIAGNIV=="PROB") %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    skewness = skewness(SYN, na.rm = TRUE),
    kurtosis = kurtosis(SYN, na.rm = TRUE),
    CI_lower = t.test(SYN)$conf.int[1],
    CI_upper = t.test(SYN)$conf.int[2],
    shapiro_w = shapiro.test(SYN)$statistic,
    shapiro_p = shapiro.test(SYN)$p.value
  )



# Elapsed Time Year 2


AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAGNIV, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>% filter(DIAGNIV=="PROB") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 2


AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAGNIV)
  ) %>%
  mutate(Diff=(SYN-Baseline))  %>% filter(DIAGNIV=="PROB") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    #skewness = skewness(Diff, na.rm = TRUE),
    #kurtosis = kurtosis(Diff, na.rm = TRUE),
    #CI_lower = t.test(Diff)$conf.int[1],
    #CI_upper = t.test(Diff)$conf.int[2],
    #shapiro_w = shapiro.test(Diff)$statistic,
    #shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 2

AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAGNIV, TIME_STUDY)
  ) %>%
  mutate(Diff=(SYN-Baseline)/TIME_STUDY)  %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )






# ALL Plasma SYN Total Year 3

AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    skewness = skewness(SYN, na.rm = TRUE),
    kurtosis = kurtosis(SYN, na.rm = TRUE),
    CI_lower = t.test(SYN)$conf.int[1],
    CI_upper = t.test(SYN)$conf.int[2],
    shapiro_w = shapiro.test(SYN)$statistic,
    shapiro_p = shapiro.test(SYN)$p.value
  )



# Elapsed Time Year 3


AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAGNIV, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 3


AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAGNIV)
  ) %>%
  mutate(Diff=(SYN-Baseline))  %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 3

AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAGNIV, TIME_STUDY)
  ) %>%
  mutate(Diff=(SYN-Baseline)/TIME_STUDY)  %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )

# ---------------


# Target MSA Entire  -----------------------

# ALL Plasma SYN Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    skewness = skewness(SYN, na.rm = TRUE),
    kurtosis = kurtosis(SYN, na.rm = TRUE),
    CI_lower = t.test(SYN)$conf.int[1],
    CI_upper = t.test(SYN)$conf.int[2],
    shapiro_w = shapiro.test(SYN)$statistic,
    shapiro_p = shapiro.test(SYN)$p.value
  )


# ALL Plasma SYN Total Year 1

EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    #skewness = skewness(SYN, na.rm = TRUE),
    #kurtosis = kurtosis(SYN, na.rm = TRUE),
    #CI_lower = t.test(SYN)$conf.int[1],
    #CI_upper = t.test(SYN)$conf.int[2],
    #shapiro_w = shapiro.test(SYN)$statistic,
    #shapiro_p = shapiro.test(SYN)$p.value
  )



# Elapsed Time Year 1


EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 1


EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN)
  ) %>%
  mutate(Diff=(SYN-Baseline))  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 1

EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, TIME_STUDY)
  ) %>%
  mutate(Diff=(SYN-Baseline)/TIME_STUDY)  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )













# ALL Plasma SYN Total Year 2

EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    skewness = skewness(SYN, na.rm = TRUE),
    kurtosis = kurtosis(SYN, na.rm = TRUE),
    CI_lower = t.test(SYN)$conf.int[1],
    CI_upper = t.test(SYN)$conf.int[2],
    shapiro_w = shapiro.test(SYN)$statistic,
    shapiro_p = shapiro.test(SYN)$p.value
  )



# Elapsed Time Year 2


EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 2


EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN)
  ) %>%
  mutate(Diff=(SYN-Baseline))  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    #skewness = skewness(Diff, na.rm = TRUE),
    #kurtosis = kurtosis(Diff, na.rm = TRUE),
    #CI_lower = t.test(Diff)$conf.int[1],
    #CI_upper = t.test(Diff)$conf.int[2],
    #shapiro_w = shapiro.test(Diff)$statistic,
    #shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 2

EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, TIME_STUDY)
  ) %>%
  mutate(Diff=(SYN-Baseline)/TIME_STUDY)  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )






# ALL Plasma SYN Total Year 3

EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    skewness = skewness(SYN, na.rm = TRUE),
    kurtosis = kurtosis(SYN, na.rm = TRUE),
    CI_lower = t.test(SYN)$conf.int[1],
    CI_upper = t.test(SYN)$conf.int[2],
    shapiro_w = shapiro.test(SYN)$statistic,
    shapiro_p = shapiro.test(SYN)$p.value
  )



# Elapsed Time Year 3


EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 3


EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN)
  ) %>%
  mutate(Diff=(SYN-Baseline))  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    #skewness = skewness(Diff, na.rm = TRUE),
    #kurtosis = kurtosis(Diff, na.rm = TRUE),
    #CI_lower = t.test(Diff)$conf.int[1],
    #CI_upper = t.test(Diff)$conf.int[2],
    #shapiro_w = shapiro.test(Diff)$statistic,
    #shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 3

EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, TIME_STUDY)
  ) %>%
  mutate(Diff=(SYN-Baseline)/TIME_STUDY)  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )



# -----------
# Target MSA Entire  MSA P vs MSA C  -----------------------

# ALL Plasma SYN Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% filter(DIAG=="CB") %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    #skewness = skewness(SYN, na.rm = TRUE),
    #kurtosis = kurtosis(SYN, na.rm = TRUE),
    #CI_lower = t.test(SYN)$conf.int[1],
    #CI_upper = t.test(SYN)$conf.int[2],
    #shapiro_w = shapiro.test(SYN)$statistic,
    #shapiro_p = shapiro.test(SYN)$p.value
  )


# ALL Plasma SYN Total Year 1

EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% filter(DIAG=="CB") %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    #skewness = skewness(SYN, na.rm = TRUE),
    #kurtosis = kurtosis(SYN, na.rm = TRUE),
    #CI_lower = t.test(SYN)$conf.int[1],
    #CI_upper = t.test(SYN)$conf.int[2],
    #shapiro_w = shapiro.test(SYN)$statistic,
    #shapiro_p = shapiro.test(SYN)$p.value
  )



# Elapsed Time Year 1


EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAG, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>% filter(DIAG=="PD") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    #skewness = skewness(Diff, na.rm = TRUE),
    #kurtosis = kurtosis(Diff, na.rm = TRUE),
    #CI_lower = t.test(Diff)$conf.int[1],
    #CI_upper = t.test(Diff)$conf.int[2],
    #shapiro_w = shapiro.test(Diff)$statistic,
    #shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 1


EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAG)
  ) %>%
  mutate(Diff=(SYN-Baseline))  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 1

EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAG, TIME_STUDY)
  ) %>%
  mutate(Diff=(SYN-Baseline)/TIME_STUDY)  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )













# ALL Plasma SYN Total Year 2

EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% filter(DIAG=="PD") %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    #skewness = skewness(SYN, na.rm = TRUE),
    #kurtosis = kurtosis(SYN, na.rm = TRUE),
    #CI_lower = t.test(SYN)$conf.int[1],
    #CI_upper = t.test(SYN)$conf.int[2],
    #shapiro_w = shapiro.test(SYN)$statistic,
    #shapiro_p = shapiro.test(SYN)$p.value
  )



# Elapsed Time Year 2


EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAG, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>% filter(DIAG=="PD") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    #skewness = skewness(Diff, na.rm = TRUE),
    #kurtosis = kurtosis(Diff, na.rm = TRUE),
    #CI_lower = t.test(Diff)$conf.int[1],
    #CI_upper = t.test(Diff)$conf.int[2],
    #shapiro_w = shapiro.test(Diff)$statistic,
    #shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 2


EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAG)
  ) %>%
  mutate(Diff=(SYN-Baseline))  %>% filter(DIAG=="CB") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    #skewness = skewness(Diff, na.rm = TRUE),
    #kurtosis = kurtosis(Diff, na.rm = TRUE),
    #CI_lower = t.test(Diff)$conf.int[1],
    #CI_upper = t.test(Diff)$conf.int[2],
    #shapiro_w = shapiro.test(Diff)$statistic,
    #shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 2

EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAG, TIME_STUDY)
  ) %>%
  mutate(Diff=(SYN-Baseline)/TIME_STUDY)  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )






# ALL Plasma SYN Total Year 3

EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    skewness = skewness(SYN, na.rm = TRUE),
    kurtosis = kurtosis(SYN, na.rm = TRUE),
    CI_lower = t.test(SYN)$conf.int[1],
    CI_upper = t.test(SYN)$conf.int[2],
    shapiro_w = shapiro.test(SYN)$statistic,
    shapiro_p = shapiro.test(SYN)$p.value
  )



# Elapsed Time Year 3


EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAG, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 3


EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAG)
  ) %>%
  mutate(Diff=(SYN-Baseline))  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 3

EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAG, TIME_STUDY)
  ) %>%
  mutate(Diff=(SYN-Baseline)/TIME_STUDY)  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )



# -----------
# Target MSA Entire  Probable vs Possible  -----------------------

# ALL Plasma SYN Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    skewness = skewness(SYN, na.rm = TRUE),
    kurtosis = kurtosis(SYN, na.rm = TRUE),
    CI_lower = t.test(SYN)$conf.int[1],
    CI_upper = t.test(SYN)$conf.int[2],
    shapiro_w = shapiro.test(SYN)$statistic,
    shapiro_p = shapiro.test(SYN)$p.value
  )


# ALL Plasma SYN Total Year 1

EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% filter(DIAGNIV=="PROB") %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    #skewness = skewness(SYN, na.rm = TRUE),
    #kurtosis = kurtosis(SYN, na.rm = TRUE),
    #CI_lower = t.test(SYN)$conf.int[1],
    #CI_upper = t.test(SYN)$conf.int[2],
    #shapiro_w = shapiro.test(SYN)$statistic,
    #shapiro_p = shapiro.test(SYN)$p.value
  )



# Elapsed Time Year 1


EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAGNIV, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>% filter(DIAGNIV=="PROB") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    #skewness = skewness(Diff, na.rm = TRUE),
    #kurtosis = kurtosis(Diff, na.rm = TRUE),
    #CI_lower = t.test(Diff)$conf.int[1],
    #CI_upper = t.test(Diff)$conf.int[2],
    #shapiro_w = shapiro.test(Diff)$statistic,
    #shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 1


EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAGNIV)
  ) %>%
  mutate(Diff=(SYN-Baseline))  %>% filter(DIAGNIV=="PROB") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 1

EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAGNIV, TIME_STUDY)
  ) %>%
  mutate(Diff=(SYN-Baseline)/TIME_STUDY)  %>% filter(DIAGNIV=="PROB") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )













# ALL Plasma SYN Total Year 2

EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% filter(DIAGNIV=="PROB") %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    #skewness = skewness(SYN, na.rm = TRUE),
    #kurtosis = kurtosis(SYN, na.rm = TRUE),
    #CI_lower = t.test(SYN)$conf.int[1],
    #CI_upper = t.test(SYN)$conf.int[2],
    #shapiro_w = shapiro.test(SYN)$statistic,
    #shapiro_p = shapiro.test(SYN)$p.value
  )



# Elapsed Time Year 2


EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAGNIV, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>% filter(DIAGNIV=="PROB") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    #skewness = skewness(Diff, na.rm = TRUE),
    #kurtosis = kurtosis(Diff, na.rm = TRUE),
    #CI_lower = t.test(Diff)$conf.int[1],
    #CI_upper = t.test(Diff)$conf.int[2],
    #shapiro_w = shapiro.test(Diff)$statistic,
    #shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 2


EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAGNIV)
  ) %>%
  mutate(Diff=(SYN-Baseline))  %>% filter(DIAGNIV=="PROB") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    #skewness = skewness(Diff, na.rm = TRUE),
    #kurtosis = kurtosis(Diff, na.rm = TRUE),
    #CI_lower = t.test(Diff)$conf.int[1],
    #CI_upper = t.test(Diff)$conf.int[2],
    #shapiro_w = shapiro.test(Diff)$statistic,
    #shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 2

EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAGNIV, TIME_STUDY)
  ) %>%
  mutate(Diff=(SYN-Baseline)/TIME_STUDY)  %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )






# ALL Plasma SYN Total Year 3

EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    skewness = skewness(SYN, na.rm = TRUE),
    kurtosis = kurtosis(SYN, na.rm = TRUE),
    CI_lower = t.test(SYN)$conf.int[1],
    CI_upper = t.test(SYN)$conf.int[2],
    shapiro_w = shapiro.test(SYN)$statistic,
    shapiro_p = shapiro.test(SYN)$p.value
  )



# Elapsed Time Year 3


EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAGNIV, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 3


EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAGNIV)
  ) %>%
  mutate(Diff=(SYN-Baseline))  %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 3

EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAGNIV, TIME_STUDY)
  ) %>%
  mutate(Diff=(SYN-Baseline)/TIME_STUDY)  %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )

# ---------------


# All data dates groups  --------
dataCohorteManaged <- readRDS("Source/dataCohorteManaged.rds")

Visits_zero <- dataCohorteManaged  %>% select(NUM, dateVisite0) %>% distinct()

AllMSA_Pop_Baseline_671 <- fread("Source/AllMSA_Pop_Baseline_671.txt")
EarlyCT_Pop_Baseline_319 <- fread("Source/EarlyCT_Pop_Baseline_319.txt")


# ----------
# TOTAL CSF aSYN -----------------------------

biofluidsForPaulo <- readRDS("Source/biofluidsForPaulo.rds")
names(biofluidsForPaulo)
CSF_SYN <- biofluidsForPaulo %>% rename("NUM"="idGlobal") %>% 
  select(NUM, datePrelevement, total_alpha_syn_g_pgml_csf, total_alpha_syn_q1_pgml_csf, total_alpha_syn_q2_pgml_csf, 
         total_alpha_syn_ipp_pgml_csf, totalAlphaSyn_Fusion_pgml_csf)

CSF_SYN <- Visits_zero %>% inner_join(CSF_SYN) %>%
  mutate(TIME_STUDY=time_length(difftime(datePrelevement, dateVisite0 ), "years")) %>%
  mutate(Year= ifelse(TIME_STUDY<=0.5, 0,
                      ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                             ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                    ifelse(TIME_STUDY>=2.5 ,3, NA))))) 

CSF_SYN <- CSF_SYN %>% gather(original, SYN, total_alpha_syn_g_pgml_csf:totalAlphaSyn_Fusion_pgml_csf) %>%
  drop_na() %>% arrange(NUM, datePrelevement) %>%
  group_by(NUM, datePrelevement) %>% mutate(SYN=mean(SYN)) %>%
  select(-original) %>% distinct()

# ----------
# Overall MSA Entire  -----------------------

# ALL CSF SYN Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    skewness = skewness(SYN, na.rm = TRUE),
    kurtosis = kurtosis(SYN, na.rm = TRUE),
    CI_lower = t.test(SYN)$conf.int[1],
    CI_upper = t.test(SYN)$conf.int[2],
    shapiro_w = shapiro.test(SYN)$statistic,
    shapiro_p = shapiro.test(SYN)$p.value
  )


# ALL CSF SYN Total Year 1

AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    skewness = skewness(SYN, na.rm = TRUE),
    kurtosis = kurtosis(SYN, na.rm = TRUE),
    CI_lower = t.test(SYN)$conf.int[1],
    CI_upper = t.test(SYN)$conf.int[2],
    shapiro_w = shapiro.test(SYN)$statistic,
    shapiro_p = shapiro.test(SYN)$p.value
  )



# Elapsed Time Year 1


AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 1


AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN)
  ) %>%
  mutate(Diff=(SYN-Baseline))  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 1

AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, TIME_STUDY)
  ) %>%
  mutate(Diff=(SYN-Baseline)/TIME_STUDY)  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )













# ALL CSF SYN Total Year 2

AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    skewness = skewness(SYN, na.rm = TRUE),
    kurtosis = kurtosis(SYN, na.rm = TRUE),
    CI_lower = t.test(SYN)$conf.int[1],
    CI_upper = t.test(SYN)$conf.int[2],
    shapiro_w = shapiro.test(SYN)$statistic,
    shapiro_p = shapiro.test(SYN)$p.value
  )



# Elapsed Time Year 2


AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 2


AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN)
  ) %>%
  mutate(Diff=(SYN-Baseline))  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    #CI_lower = t.test(Diff)$conf.int[1],
    #CI_upper = t.test(Diff)$conf.int[2],
    #shapiro_w = shapiro.test(Diff)$statistic,
    #shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 2

AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, TIME_STUDY)
  ) %>%
  mutate(Diff=(SYN-Baseline)/TIME_STUDY)  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )






# ALL CSF SYN Total Year 3

AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    skewness = skewness(SYN, na.rm = TRUE),
    kurtosis = kurtosis(SYN, na.rm = TRUE),
    CI_lower = t.test(SYN)$conf.int[1],
    CI_upper = t.test(SYN)$conf.int[2],
    shapiro_w = shapiro.test(SYN)$statistic,
    shapiro_p = shapiro.test(SYN)$p.value
  )



# Elapsed Time Year 3


AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 3


AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN)
  ) %>%
  mutate(Diff=(SYN-Baseline))  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 3

AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, TIME_STUDY)
  ) %>%
  mutate(Diff=(SYN-Baseline)/TIME_STUDY)  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )



# -----------
# Overall MSA Entire  MSA P vs MSA C  -----------------------

# ALL CSF SYN Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    skewness = skewness(SYN, na.rm = TRUE),
    kurtosis = kurtosis(SYN, na.rm = TRUE),
    CI_lower = t.test(SYN)$conf.int[1],
    CI_upper = t.test(SYN)$conf.int[2],
    shapiro_w = shapiro.test(SYN)$statistic,
    shapiro_p = shapiro.test(SYN)$p.value
  )


# ALL CSF SYN Total Year 1

AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    skewness = skewness(SYN, na.rm = TRUE),
    kurtosis = kurtosis(SYN, na.rm = TRUE),
    CI_lower = t.test(SYN)$conf.int[1],
    CI_upper = t.test(SYN)$conf.int[2],
    shapiro_w = shapiro.test(SYN)$statistic,
    shapiro_p = shapiro.test(SYN)$p.value
  )



# Elapsed Time Year 1


AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAG, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 1


AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAG)
  ) %>%
  mutate(Diff=(SYN-Baseline))  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 1

AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAG, TIME_STUDY)
  ) %>%
  mutate(Diff=(SYN-Baseline)/TIME_STUDY)  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )













# ALL CSF SYN Total Year 2

AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    skewness = skewness(SYN, na.rm = TRUE),
    kurtosis = kurtosis(SYN, na.rm = TRUE),
    CI_lower = t.test(SYN)$conf.int[1],
    CI_upper = t.test(SYN)$conf.int[2],
    shapiro_w = shapiro.test(SYN)$statistic,
    shapiro_p = shapiro.test(SYN)$p.value
  )



# Elapsed Time Year 2


AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAG, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 2


AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAG)
  ) %>%
  mutate(Diff=(SYN-Baseline))  %>% filter(DIAG=="PD") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    #skewness = skewness(Diff, na.rm = TRUE),
    #kurtosis = kurtosis(Diff, na.rm = TRUE),
    #CI_lower = t.test(Diff)$conf.int[1],
    #CI_upper = t.test(Diff)$conf.int[2],
    #shapiro_w = shapiro.test(Diff)$statistic,
    #shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 2

AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAG, TIME_STUDY)
  ) %>%
  mutate(Diff=(SYN-Baseline)/TIME_STUDY)  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )






# ALL CSF SYN Total Year 3

AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    skewness = skewness(SYN, na.rm = TRUE),
    kurtosis = kurtosis(SYN, na.rm = TRUE),
    CI_lower = t.test(SYN)$conf.int[1],
    CI_upper = t.test(SYN)$conf.int[2],
    shapiro_w = shapiro.test(SYN)$statistic,
    shapiro_p = shapiro.test(SYN)$p.value
  )



# Elapsed Time Year 3


AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAG, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 3


AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAG)
  ) %>%
  mutate(Diff=(SYN-Baseline))  %>% filter(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 3

AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAG, TIME_STUDY)
  ) %>%
  mutate(Diff=(SYN-Baseline)/TIME_STUDY)  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )



# -----------
# Overall MSA Entire  Probable vs Possible  -----------------------

# ALL CSF SYN Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    skewness = skewness(SYN, na.rm = TRUE),
    kurtosis = kurtosis(SYN, na.rm = TRUE),
    CI_lower = t.test(SYN)$conf.int[1],
    CI_upper = t.test(SYN)$conf.int[2],
    shapiro_w = shapiro.test(SYN)$statistic,
    shapiro_p = shapiro.test(SYN)$p.value
  )


# ALL CSF SYN Total Year 1

AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% filter(DIAGNIV=="PROB") %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    skewness = skewness(SYN, na.rm = TRUE),
    kurtosis = kurtosis(SYN, na.rm = TRUE),
    CI_lower = t.test(SYN)$conf.int[1],
    CI_upper = t.test(SYN)$conf.int[2],
    shapiro_w = shapiro.test(SYN)$statistic,
    shapiro_p = shapiro.test(SYN)$p.value
  )



# Elapsed Time Year 1


AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAGNIV, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>% filter(DIAGNIV=="POS") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    #skewness = skewness(Diff, na.rm = TRUE),
    #kurtosis = kurtosis(Diff, na.rm = TRUE),
    #CI_lower = t.test(Diff)$conf.int[1],
    #CI_upper = t.test(Diff)$conf.int[2],
    #shapiro_w = shapiro.test(Diff)$statistic,
    #shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 1


AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAGNIV)
  ) %>%
  mutate(Diff=(SYN-Baseline))  %>% filter(DIAGNIV=="POS") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    #skewness = skewness(Diff, na.rm = TRUE),
    #kurtosis = kurtosis(Diff, na.rm = TRUE),
    #CI_lower = t.test(Diff)$conf.int[1],
    #CI_upper = t.test(Diff)$conf.int[2],
    #shapiro_w = shapiro.test(Diff)$statistic,
    #shapiro_p = shapiro.test(Diff)$p.value
  )



# Standardized Year 1

AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAGNIV, TIME_STUDY)
  ) %>%
  mutate(Diff=(SYN-Baseline)/TIME_STUDY)  %>% filter(DIAGNIV=="PROB") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )













# ALL CSF SYN Total Year 2

AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% filter(DIAGNIV=="POS") %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    #skewness = skewness(SYN, na.rm = TRUE),
    #kurtosis = kurtosis(SYN, na.rm = TRUE),
    #CI_lower = t.test(SYN)$conf.int[1],
    #CI_upper = t.test(SYN)$conf.int[2],
    #shapiro_w = shapiro.test(SYN)$statistic,
    #shapiro_p = shapiro.test(SYN)$p.value
  )



# Elapsed Time Year 2


AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAGNIV, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>% filter(DIAGNIV=="POS") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    #skewness = skewness(Diff, na.rm = TRUE),
    #kurtosis = kurtosis(Diff, na.rm = TRUE),
    #CI_lower = t.test(Diff)$conf.int[1],
    #CI_upper = t.test(Diff)$conf.int[2],
    #shapiro_w = shapiro.test(Diff)$statistic,
    #shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 2


AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAGNIV)
  ) %>%
  mutate(Diff=(SYN-Baseline))  %>% filter(DIAGNIV=="PROB") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    #skewness = skewness(Diff, na.rm = TRUE),
    #kurtosis = kurtosis(Diff, na.rm = TRUE),
    #CI_lower = t.test(Diff)$conf.int[1],
    #CI_upper = t.test(Diff)$conf.int[2],
    #shapiro_w = shapiro.test(Diff)$statistic,
    #shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 2

AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAGNIV, TIME_STUDY)
  ) %>%
  mutate(Diff=(SYN-Baseline)/TIME_STUDY)  %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )






# ALL CSF SYN Total Year 3

AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    skewness = skewness(SYN, na.rm = TRUE),
    kurtosis = kurtosis(SYN, na.rm = TRUE),
    CI_lower = t.test(SYN)$conf.int[1],
    CI_upper = t.test(SYN)$conf.int[2],
    shapiro_w = shapiro.test(SYN)$statistic,
    shapiro_p = shapiro.test(SYN)$p.value
  )



# Elapsed Time Year 3


AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAGNIV, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 3


AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAGNIV)
  ) %>%
  mutate(Diff=(SYN-Baseline))  %>% filter(DIAGNIV=="PROB") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    #skewness = skewness(Diff, na.rm = TRUE),
    #kurtosis = kurtosis(Diff, na.rm = TRUE),
    #CI_lower = t.test(Diff)$conf.int[1],
    #CI_upper = t.test(Diff)$conf.int[2],
    #shapiro_w = shapiro.test(Diff)$statistic,
    #shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 3

AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAGNIV, TIME_STUDY)
  ) %>%
  mutate(Diff=(SYN-Baseline)/TIME_STUDY)  %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )

# ---------------


# Target MSA Entire  -----------------------

# ALL CSF SYN Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    skewness = skewness(SYN, na.rm = TRUE),
    kurtosis = kurtosis(SYN, na.rm = TRUE),
    CI_lower = t.test(SYN)$conf.int[1],
    CI_upper = t.test(SYN)$conf.int[2],
    shapiro_w = shapiro.test(SYN)$statistic,
    shapiro_p = shapiro.test(SYN)$p.value
  )


# ALL CSF SYN Total Year 1

EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    skewness = skewness(SYN, na.rm = TRUE),
    kurtosis = kurtosis(SYN, na.rm = TRUE),
    CI_lower = t.test(SYN)$conf.int[1],
    CI_upper = t.test(SYN)$conf.int[2],
    shapiro_w = shapiro.test(SYN)$statistic,
    shapiro_p = shapiro.test(SYN)$p.value
  )



# Elapsed Time Year 1


EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 1


EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN)
  ) %>%
  mutate(Diff=(SYN-Baseline))  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 1

EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, TIME_STUDY)
  ) %>%
  mutate(Diff=(SYN-Baseline)/TIME_STUDY)  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )













# ALL CSF SYN Total Year 2

EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    skewness = skewness(SYN, na.rm = TRUE),
    kurtosis = kurtosis(SYN, na.rm = TRUE),
    CI_lower = t.test(SYN)$conf.int[1],
    CI_upper = t.test(SYN)$conf.int[2],
    shapiro_w = shapiro.test(SYN)$statistic,
    shapiro_p = shapiro.test(SYN)$p.value
  )



# Elapsed Time Year 2


EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 2


EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN)
  ) %>%
  mutate(Diff=(SYN-Baseline))  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    #skewness = skewness(Diff, na.rm = TRUE),
    #kurtosis = kurtosis(Diff, na.rm = TRUE),
    #CI_lower = t.test(Diff)$conf.int[1],
    #CI_upper = t.test(Diff)$conf.int[2],
    #shapiro_w = shapiro.test(Diff)$statistic,
    #shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 2

EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, TIME_STUDY)
  ) %>%
  mutate(Diff=(SYN-Baseline)/TIME_STUDY)  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )






# ALL CSF SYN Total Year 3

EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    skewness = skewness(SYN, na.rm = TRUE),
    kurtosis = kurtosis(SYN, na.rm = TRUE),
    CI_lower = t.test(SYN)$conf.int[1],
    CI_upper = t.test(SYN)$conf.int[2],
    shapiro_w = shapiro.test(SYN)$statistic,
    shapiro_p = shapiro.test(SYN)$p.value
  )



# Elapsed Time Year 3


EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 3


EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN)
  ) %>%
  mutate(Diff=(SYN-Baseline))  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    #skewness = skewness(Diff, na.rm = TRUE),
    #kurtosis = kurtosis(Diff, na.rm = TRUE),
    #CI_lower = t.test(Diff)$conf.int[1],
    #CI_upper = t.test(Diff)$conf.int[2],
    #shapiro_w = shapiro.test(Diff)$statistic,
    #shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 3

EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, TIME_STUDY)
  ) %>%
  mutate(Diff=(SYN-Baseline)/TIME_STUDY)  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )



# -----------
# Target MSA Entire  MSA P vs MSA C  -----------------------

# ALL CSF SYN Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    skewness = skewness(SYN, na.rm = TRUE),
    kurtosis = kurtosis(SYN, na.rm = TRUE),
    CI_lower = t.test(SYN)$conf.int[1],
    CI_upper = t.test(SYN)$conf.int[2],
    shapiro_w = shapiro.test(SYN)$statistic,
    shapiro_p = shapiro.test(SYN)$p.value
  )


# ALL CSF SYN Total Year 1

EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    skewness = skewness(SYN, na.rm = TRUE),
    kurtosis = kurtosis(SYN, na.rm = TRUE),
    CI_lower = t.test(SYN)$conf.int[1],
    CI_upper = t.test(SYN)$conf.int[2],
    shapiro_w = shapiro.test(SYN)$statistic,
    shapiro_p = shapiro.test(SYN)$p.value
  )



# Elapsed Time Year 1


EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAG, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    #skewness = skewness(Diff, na.rm = TRUE),
    #kurtosis = kurtosis(Diff, na.rm = TRUE),
    #CI_lower = t.test(Diff)$conf.int[1],
    #CI_upper = t.test(Diff)$conf.int[2],
    #shapiro_w = shapiro.test(Diff)$statistic,
    #shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 1


EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAG)
  ) %>%
  mutate(Diff=(SYN-Baseline))  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 1

EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAG, TIME_STUDY)
  ) %>%
  mutate(Diff=(SYN-Baseline)/TIME_STUDY)  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )













# ALL CSF SYN Total Year 2

EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% filter(DIAG=="PD") %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    skewness = skewness(SYN, na.rm = TRUE),
    kurtosis = kurtosis(SYN, na.rm = TRUE),
    CI_lower = t.test(SYN)$conf.int[1],
    CI_upper = t.test(SYN)$conf.int[2],
    shapiro_w = shapiro.test(SYN)$statistic,
    shapiro_p = shapiro.test(SYN)$p.value
  )



# Elapsed Time Year 2


EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAG, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>% filter(DIAG=="CB") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    #skewness = skewness(Diff, na.rm = TRUE),
    #kurtosis = kurtosis(Diff, na.rm = TRUE),
    #CI_lower = t.test(Diff)$conf.int[1],
    #CI_upper = t.test(Diff)$conf.int[2],
    #shapiro_w = shapiro.test(Diff)$statistic,
    #shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 2


EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAG)
  ) %>%
  mutate(Diff=(SYN-Baseline))  %>% filter(DIAG=="CB") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    #skewness = skewness(Diff, na.rm = TRUE),
    #kurtosis = kurtosis(Diff, na.rm = TRUE),
    #CI_lower = t.test(Diff)$conf.int[1],
    #CI_upper = t.test(Diff)$conf.int[2],
    #shapiro_w = shapiro.test(Diff)$statistic,
    #shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 2

EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAG, TIME_STUDY)
  ) %>%
  mutate(Diff=(SYN-Baseline)/TIME_STUDY)  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )






# ALL CSF SYN Total Year 3

EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    skewness = skewness(SYN, na.rm = TRUE),
    kurtosis = kurtosis(SYN, na.rm = TRUE),
    CI_lower = t.test(SYN)$conf.int[1],
    CI_upper = t.test(SYN)$conf.int[2],
    shapiro_w = shapiro.test(SYN)$statistic,
    shapiro_p = shapiro.test(SYN)$p.value
  )



# Elapsed Time Year 3


EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAG, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 3


EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAG)
  ) %>%
  mutate(Diff=(SYN-Baseline))  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 3

EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAG, TIME_STUDY)
  ) %>%
  mutate(Diff=(SYN-Baseline)/TIME_STUDY)  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )



# -----------
# Target MSA Entire  Probable vs Possible  -----------------------

# ALL CSF SYN Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    skewness = skewness(SYN, na.rm = TRUE),
    kurtosis = kurtosis(SYN, na.rm = TRUE),
    CI_lower = t.test(SYN)$conf.int[1],
    CI_upper = t.test(SYN)$conf.int[2],
    shapiro_w = shapiro.test(SYN)$statistic,
    shapiro_p = shapiro.test(SYN)$p.value
  )


# ALL CSF SYN Total Year 1

EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% filter(DIAGNIV=="PROB") %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    skewness = skewness(SYN, na.rm = TRUE),
    kurtosis = kurtosis(SYN, na.rm = TRUE),
    CI_lower = t.test(SYN)$conf.int[1],
    CI_upper = t.test(SYN)$conf.int[2],
    shapiro_w = shapiro.test(SYN)$statistic,
    shapiro_p = shapiro.test(SYN)$p.value
  )



# Elapsed Time Year 1


EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAGNIV, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>% filter(DIAGNIV=="PROB") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 1


EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAGNIV)
  ) %>%
  mutate(Diff=(SYN-Baseline))  %>% filter(DIAGNIV=="POS") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    #skewness = skewness(Diff, na.rm = TRUE),
    #kurtosis = kurtosis(Diff, na.rm = TRUE),
    #CI_lower = t.test(Diff)$conf.int[1],
    #CI_upper = t.test(Diff)$conf.int[2],
    #shapiro_w = shapiro.test(Diff)$statistic,
    #shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 1

EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAGNIV, TIME_STUDY)
  ) %>%
  mutate(Diff=(SYN-Baseline)/TIME_STUDY)  %>% filter(DIAGNIV=="POS") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    # skewness = skewness(Diff, na.rm = TRUE),
    # kurtosis = kurtosis(Diff, na.rm = TRUE),
    #CI_lower = t.test(Diff)$conf.int[1],
    #CI_upper = t.test(Diff)$conf.int[2],
    #shapiro_w = shapiro.test(Diff)$statistic,
    #shapiro_p = shapiro.test(Diff)$p.value
  )













# ALL CSF SYN Total Year 2

EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% filter(DIAGNIV=="POS") %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    #skewness = skewness(SYN, na.rm = TRUE),
    #kurtosis = kurtosis(SYN, na.rm = TRUE),
    #CI_lower = t.test(SYN)$conf.int[1],
    #CI_upper = t.test(SYN)$conf.int[2],
    #shapiro_w = shapiro.test(SYN)$statistic,
    #shapiro_p = shapiro.test(SYN)$p.value
  )



# Elapsed Time Year 2


EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAGNIV, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>% filter(DIAGNIV=="POS") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    #skewness = skewness(Diff, na.rm = TRUE),
    #kurtosis = kurtosis(Diff, na.rm = TRUE),
    #CI_lower = t.test(Diff)$conf.int[1],
    #CI_upper = t.test(Diff)$conf.int[2],
    #shapiro_w = shapiro.test(Diff)$statistic,
    #shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 2


EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAGNIV)
  ) %>%
  mutate(Diff=(SYN-Baseline))  %>% filter(DIAGNIV=="PROB") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    #skewness = skewness(Diff, na.rm = TRUE),
    #kurtosis = kurtosis(Diff, na.rm = TRUE),
    #CI_lower = t.test(Diff)$conf.int[1],
    #CI_upper = t.test(Diff)$conf.int[2],
    #shapiro_w = shapiro.test(Diff)$statistic,
    #shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 2

EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAGNIV, TIME_STUDY)
  ) %>%
  mutate(Diff=(SYN-Baseline)/TIME_STUDY)  %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )






# ALL CSF SYN Total Year 3

EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    skewness = skewness(SYN, na.rm = TRUE),
    kurtosis = kurtosis(SYN, na.rm = TRUE),
    CI_lower = t.test(SYN)$conf.int[1],
    CI_upper = t.test(SYN)$conf.int[2],
    shapiro_w = shapiro.test(SYN)$statistic,
    shapiro_p = shapiro.test(SYN)$p.value
  )



# Elapsed Time Year 3


EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAGNIV, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 3


EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAGNIV)
  ) %>%
  mutate(Diff=(SYN-Baseline))  %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 3

EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAGNIV, TIME_STUDY)
  ) %>%
  mutate(Diff=(SYN-Baseline)/TIME_STUDY)  %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )

# ---------------


# OLIGO Plasma aSYN -----------------------------

biofluidsForPaulo <- readRDS("Source/biofluidsForPaulo.rds")
names(biofluidsForPaulo)
Plasma_SYN <- biofluidsForPaulo %>% rename("NUM"="idGlobal") %>% 
  select(NUM, datePrelevement, oligo_alpha_syn_q1_pgml_plas, oligo_alpha_syn_q2_pgml_plas)

Plasma_SYN <- Visits_zero %>% inner_join(Plasma_SYN) %>%
  mutate(TIME_STUDY=time_length(difftime(datePrelevement, dateVisite0 ), "years")) %>%
  mutate(Year= ifelse(TIME_STUDY<=0.5, 0,
                      ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                             ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                    ifelse(TIME_STUDY>=2.5 ,3, NA))))) 

Plasma_SYN <- Plasma_SYN %>% gather(original, SYN, oligo_alpha_syn_q1_pgml_plas:oligo_alpha_syn_q2_pgml_plas) %>%
  drop_na() %>% arrange(NUM, datePrelevement) %>%
  group_by(NUM, datePrelevement) %>% mutate(SYN=mean(SYN)) %>%
  select(-original) %>% distinct()

# ----------
# Overall MSA Entire  -----------------------

# ALL Plasma SYN Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    skewness = skewness(SYN, na.rm = TRUE),
    kurtosis = kurtosis(SYN, na.rm = TRUE),
    CI_lower = t.test(SYN)$conf.int[1],
    CI_upper = t.test(SYN)$conf.int[2],
    shapiro_w = shapiro.test(SYN)$statistic,
    shapiro_p = shapiro.test(SYN)$p.value
  )


# ALL Plasma SYN Total Year 1

AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    skewness = skewness(SYN, na.rm = TRUE),
    kurtosis = kurtosis(SYN, na.rm = TRUE),
    CI_lower = t.test(SYN)$conf.int[1],
    CI_upper = t.test(SYN)$conf.int[2],
    shapiro_w = shapiro.test(SYN)$statistic,
    shapiro_p = shapiro.test(SYN)$p.value
  )



# Elapsed Time Year 1


AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    #shapiro_w = shapiro.test(Diff)$statistic,
    #shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 1


AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN)
  ) %>%
  mutate(Diff=(SYN-Baseline))  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    #skewness = skewness(Diff, na.rm = TRUE),
    #kurtosis = kurtosis(Diff, na.rm = TRUE),
    #CI_lower = t.test(Diff)$conf.int[1],
    #CI_upper = t.test(Diff)$conf.int[2],
    #shapiro_w = shapiro.test(Diff)$statistic,
    #shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 1

AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, TIME_STUDY)
  ) %>%
  mutate(Diff=(SYN-Baseline)/TIME_STUDY)  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )













# ALL Plasma SYN Total Year 2

AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    skewness = skewness(SYN, na.rm = TRUE),
    kurtosis = kurtosis(SYN, na.rm = TRUE),
    CI_lower = t.test(SYN)$conf.int[1],
    CI_upper = t.test(SYN)$conf.int[2],
    shapiro_w = shapiro.test(SYN)$statistic,
    shapiro_p = shapiro.test(SYN)$p.value
  )



# Elapsed Time Year 2


AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 2


AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN)
  ) %>%
  mutate(Diff=(SYN-Baseline))  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    #CI_lower = t.test(Diff)$conf.int[1],
    #CI_upper = t.test(Diff)$conf.int[2],
    #shapiro_w = shapiro.test(Diff)$statistic,
    #shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 2

AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, TIME_STUDY)
  ) %>%
  mutate(Diff=(SYN-Baseline)/TIME_STUDY)  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )






# ALL Plasma SYN Total Year 3

AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    skewness = skewness(SYN, na.rm = TRUE),
    kurtosis = kurtosis(SYN, na.rm = TRUE),
    CI_lower = t.test(SYN)$conf.int[1],
    CI_upper = t.test(SYN)$conf.int[2],
    shapiro_w = shapiro.test(SYN)$statistic,
    shapiro_p = shapiro.test(SYN)$p.value
  )



# Elapsed Time Year 3


AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 3


AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN)
  ) %>%
  mutate(Diff=(SYN-Baseline))  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 3

AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, TIME_STUDY)
  ) %>%
  mutate(Diff=(SYN-Baseline)/TIME_STUDY)  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )



# -----------
# Overall MSA Entire  MSA P vs MSA C  -----------------------

# ALL Plasma SYN Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    skewness = skewness(SYN, na.rm = TRUE),
    kurtosis = kurtosis(SYN, na.rm = TRUE),
    CI_lower = t.test(SYN)$conf.int[1],
    CI_upper = t.test(SYN)$conf.int[2],
    shapiro_w = shapiro.test(SYN)$statistic,
    shapiro_p = shapiro.test(SYN)$p.value
  )


# ALL Plasma SYN Total Year 1

AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% filter(DIAG=="PD") %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    #skewness = skewness(SYN, na.rm = TRUE),
    #kurtosis = kurtosis(SYN, na.rm = TRUE),
    #CI_lower = t.test(SYN)$conf.int[1],
    #CI_upper = t.test(SYN)$conf.int[2],
    #shapiro_w = shapiro.test(SYN)$statistic,
    #shapiro_p = shapiro.test(SYN)$p.value
  )



# Elapsed Time Year 1


AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAG, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>% filter(DIAG=="CB") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    #skewness = skewness(Diff, na.rm = TRUE),
    #kurtosis = kurtosis(Diff, na.rm = TRUE),
    #CI_lower = t.test(Diff)$conf.int[1],
    #CI_upper = t.test(Diff)$conf.int[2],
    #shapiro_w = shapiro.test(Diff)$statistic,
    #shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 1


AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAG)
  ) %>%
  mutate(Diff=(SYN-Baseline))  %>% filter(DIAG=="PD") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    #skewness = skewness(Diff, na.rm = TRUE),
    #kurtosis = kurtosis(Diff, na.rm = TRUE),
    #CI_lower = t.test(Diff)$conf.int[1],
    #CI_upper = t.test(Diff)$conf.int[2],
    #shapiro_w = shapiro.test(Diff)$statistic,
    #shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 1

AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAG, TIME_STUDY)
  ) %>%
  mutate(Diff=(SYN-Baseline)/TIME_STUDY)  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )













# ALL Plasma SYN Total Year 2

AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% filter(DIAG=="PD") %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    skewness = skewness(SYN, na.rm = TRUE),
    kurtosis = kurtosis(SYN, na.rm = TRUE),
    CI_lower = t.test(SYN)$conf.int[1],
    CI_upper = t.test(SYN)$conf.int[2],
    shapiro_w = shapiro.test(SYN)$statistic,
    shapiro_p = shapiro.test(SYN)$p.value
  )



# Elapsed Time Year 2


AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAG, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>% filter(DIAG=="CB") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    #skewness = skewness(Diff, na.rm = TRUE),
    #kurtosis = kurtosis(Diff, na.rm = TRUE),
    #CI_lower = t.test(Diff)$conf.int[1],
    #CI_upper = t.test(Diff)$conf.int[2],
    #shapiro_w = shapiro.test(Diff)$statistic,
    #shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 2


AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAG)
  ) %>%
  mutate(Diff=(SYN-Baseline))  %>% filter(DIAG=="PD") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    #skewness = skewness(Diff, na.rm = TRUE),
    #kurtosis = kurtosis(Diff, na.rm = TRUE),
    #CI_lower = t.test(Diff)$conf.int[1],
    #CI_upper = t.test(Diff)$conf.int[2],
    #shapiro_w = shapiro.test(Diff)$statistic,
    #shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 2

AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAG, TIME_STUDY)
  ) %>%
  mutate(Diff=(SYN-Baseline)/TIME_STUDY)  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )






# ALL Plasma SYN Total Year 3

AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    skewness = skewness(SYN, na.rm = TRUE),
    kurtosis = kurtosis(SYN, na.rm = TRUE),
    CI_lower = t.test(SYN)$conf.int[1],
    CI_upper = t.test(SYN)$conf.int[2],
    shapiro_w = shapiro.test(SYN)$statistic,
    shapiro_p = shapiro.test(SYN)$p.value
  )



# Elapsed Time Year 3


AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAG, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>% filter(DIAG=="CB") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 3


AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAG)
  ) %>%
  mutate(Diff=(SYN-Baseline))  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 3

AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAG, TIME_STUDY)
  ) %>%
  mutate(Diff=(SYN-Baseline)/TIME_STUDY)  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )



# -----------
# Overall MSA Entire  Probable vs Possible  -----------------------

# ALL Plasma SYN Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% filter(DIAGNIV=="PROB") %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    skewness = skewness(SYN, na.rm = TRUE),
    kurtosis = kurtosis(SYN, na.rm = TRUE),
    CI_lower = t.test(SYN)$conf.int[1],
    CI_upper = t.test(SYN)$conf.int[2],
    shapiro_w = shapiro.test(SYN)$statistic,
    shapiro_p = shapiro.test(SYN)$p.value
  )


# ALL Plasma SYN Total Year 1

AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% filter(DIAGNIV=="PROB") %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    skewness = skewness(SYN, na.rm = TRUE),
    kurtosis = kurtosis(SYN, na.rm = TRUE),
    #CI_lower = t.test(SYN)$conf.int[1],
    #CI_upper = t.test(SYN)$conf.int[2],
    #shapiro_w = shapiro.test(SYN)$statistic,
    #shapiro_p = shapiro.test(SYN)$p.value
  )



# Elapsed Time Year 1


AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAGNIV, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>% filter(DIAGNIV=="PROB") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    #CI_lower = t.test(Diff)$conf.int[1],
    #CI_upper = t.test(Diff)$conf.int[2],
    #shapiro_w = shapiro.test(Diff)$statistic,
    #shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 1


AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAGNIV)
  ) %>%
  mutate(Diff=(SYN-Baseline))  %>% filter(DIAGNIV=="PROB") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )



# Standardized Year 1

AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAGNIV, TIME_STUDY)
  ) %>%
  mutate(Diff=(SYN-Baseline)/TIME_STUDY)  %>% filter(DIAGNIV=="POS") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    #skewness = skewness(Diff, na.rm = TRUE),
    #kurtosis = kurtosis(Diff, na.rm = TRUE),
    #CI_lower = t.test(Diff)$conf.int[1],
    #CI_upper = t.test(Diff)$conf.int[2],
    #shapiro_w = shapiro.test(Diff)$statistic,
    #shapiro_p = shapiro.test(Diff)$p.value
  )













# ALL Plasma SYN Total Year 2

AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% filter(DIAGNIV=="POS") %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    #skewness = skewness(SYN, na.rm = TRUE),
    #kurtosis = kurtosis(SYN, na.rm = TRUE),
    #CI_lower = t.test(SYN)$conf.int[1],
    #CI_upper = t.test(SYN)$conf.int[2],
    #shapiro_w = shapiro.test(SYN)$statistic,
    #shapiro_p = shapiro.test(SYN)$p.value
  )



# Elapsed Time Year 2


AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAGNIV, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>% filter(DIAGNIV=="PROB") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 2


AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAGNIV)
  ) %>%
  mutate(Diff=(SYN-Baseline))  %>% filter(DIAGNIV=="PROB") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    #skewness = skewness(Diff, na.rm = TRUE),
    #kurtosis = kurtosis(Diff, na.rm = TRUE),
    #CI_lower = t.test(Diff)$conf.int[1],
    #CI_upper = t.test(Diff)$conf.int[2],
    #shapiro_w = shapiro.test(Diff)$statistic,
    #shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 2

AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAGNIV, TIME_STUDY)
  ) %>%
  mutate(Diff=(SYN-Baseline)/TIME_STUDY)  %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )






# ALL Plasma SYN Total Year 3

AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    skewness = skewness(SYN, na.rm = TRUE),
    kurtosis = kurtosis(SYN, na.rm = TRUE),
    CI_lower = t.test(SYN)$conf.int[1],
    CI_upper = t.test(SYN)$conf.int[2],
    shapiro_w = shapiro.test(SYN)$statistic,
    shapiro_p = shapiro.test(SYN)$p.value
  )



# Elapsed Time Year 3


AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAGNIV, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 3


AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAGNIV)
  ) %>%
  mutate(Diff=(SYN-Baseline))  %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 3

AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAGNIV, TIME_STUDY)
  ) %>%
  mutate(Diff=(SYN-Baseline)/TIME_STUDY)  %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )

# ---------------


# Target MSA Entire  -----------------------

# ALL Plasma SYN Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    skewness = skewness(SYN, na.rm = TRUE),
    kurtosis = kurtosis(SYN, na.rm = TRUE),
    CI_lower = t.test(SYN)$conf.int[1],
    CI_upper = t.test(SYN)$conf.int[2],
    shapiro_w = shapiro.test(SYN)$statistic,
    shapiro_p = shapiro.test(SYN)$p.value
  )


# ALL Plasma SYN Total Year 1

EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    #skewness = skewness(SYN, na.rm = TRUE),
    #kurtosis = kurtosis(SYN, na.rm = TRUE),
    #CI_lower = t.test(SYN)$conf.int[1],
    #CI_upper = t.test(SYN)$conf.int[2],
    #shapiro_w = shapiro.test(SYN)$statistic,
    #shapiro_p = shapiro.test(SYN)$p.value
  )



# Elapsed Time Year 1


EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 1


EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN)
  ) %>%
  mutate(Diff=(SYN-Baseline))  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 1

EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, TIME_STUDY)
  ) %>%
  mutate(Diff=(SYN-Baseline)/TIME_STUDY)  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )













# ALL Plasma SYN Total Year 2

EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    skewness = skewness(SYN, na.rm = TRUE),
    kurtosis = kurtosis(SYN, na.rm = TRUE),
    CI_lower = t.test(SYN)$conf.int[1],
    CI_upper = t.test(SYN)$conf.int[2],
    shapiro_w = shapiro.test(SYN)$statistic,
    shapiro_p = shapiro.test(SYN)$p.value
  )



# Elapsed Time Year 2


EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 2


EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN)
  ) %>%
  mutate(Diff=(SYN-Baseline))  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    #skewness = skewness(Diff, na.rm = TRUE),
    #kurtosis = kurtosis(Diff, na.rm = TRUE),
    #CI_lower = t.test(Diff)$conf.int[1],
    #CI_upper = t.test(Diff)$conf.int[2],
    #shapiro_w = shapiro.test(Diff)$statistic,
    #shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 2

EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, TIME_STUDY)
  ) %>%
  mutate(Diff=(SYN-Baseline)/TIME_STUDY)  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )






# ALL Plasma SYN Total Year 3

EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    skewness = skewness(SYN, na.rm = TRUE),
    kurtosis = kurtosis(SYN, na.rm = TRUE),
    CI_lower = t.test(SYN)$conf.int[1],
    CI_upper = t.test(SYN)$conf.int[2],
    shapiro_w = shapiro.test(SYN)$statistic,
    shapiro_p = shapiro.test(SYN)$p.value
  )



# Elapsed Time Year 3


EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 3


EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN)
  ) %>%
  mutate(Diff=(SYN-Baseline))  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    #skewness = skewness(Diff, na.rm = TRUE),
    #kurtosis = kurtosis(Diff, na.rm = TRUE),
    #CI_lower = t.test(Diff)$conf.int[1],
    #CI_upper = t.test(Diff)$conf.int[2],
    #shapiro_w = shapiro.test(Diff)$statistic,
    #shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 3

EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, TIME_STUDY)
  ) %>%
  mutate(Diff=(SYN-Baseline)/TIME_STUDY)  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )



# -----------
# Target MSA Entire  MSA P vs MSA C  -----------------------

# ALL Plasma SYN Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% filter(DIAG=="PD") %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    skewness = skewness(SYN, na.rm = TRUE),
    kurtosis = kurtosis(SYN, na.rm = TRUE),
    CI_lower = t.test(SYN)$conf.int[1],
    CI_upper = t.test(SYN)$conf.int[2],
    shapiro_w = shapiro.test(SYN)$statistic,
    shapiro_p = shapiro.test(SYN)$p.value
  )


# ALL Plasma SYN Total Year 1

EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% filter(DIAG=="CB") %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    #skewness = skewness(SYN, na.rm = TRUE),
    #kurtosis = kurtosis(SYN, na.rm = TRUE),
    #CI_lower = t.test(SYN)$conf.int[1],
    #CI_upper = t.test(SYN)$conf.int[2],
    #shapiro_w = shapiro.test(SYN)$statistic,
    #shapiro_p = shapiro.test(SYN)$p.value
  )



# Elapsed Time Year 1


EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAG, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>% filter(DIAG=="PD") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    #skewness = skewness(Diff, na.rm = TRUE),
    #kurtosis = kurtosis(Diff, na.rm = TRUE),
    #CI_lower = t.test(Diff)$conf.int[1],
    #CI_upper = t.test(Diff)$conf.int[2],
    #shapiro_w = shapiro.test(Diff)$statistic,
    #shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 1


EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAG)
  ) %>%
  mutate(Diff=(SYN-Baseline))  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 1

EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAG, TIME_STUDY)
  ) %>%
  mutate(Diff=(SYN-Baseline)/TIME_STUDY)  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )













# ALL Plasma SYN Total Year 2

EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% filter(DIAG=="PD") %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    #skewness = skewness(SYN, na.rm = TRUE),
    #kurtosis = kurtosis(SYN, na.rm = TRUE),
    #CI_lower = t.test(SYN)$conf.int[1],
    #CI_upper = t.test(SYN)$conf.int[2],
    #shapiro_w = shapiro.test(SYN)$statistic,
    #shapiro_p = shapiro.test(SYN)$p.value
  )



# Elapsed Time Year 2


EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAG, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>% filter(DIAG=="PD") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    #skewness = skewness(Diff, na.rm = TRUE),
    #kurtosis = kurtosis(Diff, na.rm = TRUE),
    #CI_lower = t.test(Diff)$conf.int[1],
    #CI_upper = t.test(Diff)$conf.int[2],
    #shapiro_w = shapiro.test(Diff)$statistic,
    #shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 2


EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAG)
  ) %>%
  mutate(Diff=(SYN-Baseline))  %>% filter(DIAG=="CB") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    #skewness = skewness(Diff, na.rm = TRUE),
    #kurtosis = kurtosis(Diff, na.rm = TRUE),
    #CI_lower = t.test(Diff)$conf.int[1],
    #CI_upper = t.test(Diff)$conf.int[2],
    #shapiro_w = shapiro.test(Diff)$statistic,
    #shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 2

EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAG, TIME_STUDY)
  ) %>%
  mutate(Diff=(SYN-Baseline)/TIME_STUDY)  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )






# ALL Plasma SYN Total Year 3

EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    skewness = skewness(SYN, na.rm = TRUE),
    kurtosis = kurtosis(SYN, na.rm = TRUE),
    CI_lower = t.test(SYN)$conf.int[1],
    CI_upper = t.test(SYN)$conf.int[2],
    shapiro_w = shapiro.test(SYN)$statistic,
    shapiro_p = shapiro.test(SYN)$p.value
  )



# Elapsed Time Year 3


EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAG, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 3


EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAG)
  ) %>%
  mutate(Diff=(SYN-Baseline))  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 3

EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAG, TIME_STUDY)
  ) %>%
  mutate(Diff=(SYN-Baseline)/TIME_STUDY)  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )



# -----------
# Target MSA Entire  Probable vs Possible  -----------------------

# ALL Plasma SYN Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    skewness = skewness(SYN, na.rm = TRUE),
    kurtosis = kurtosis(SYN, na.rm = TRUE),
    CI_lower = t.test(SYN)$conf.int[1],
    CI_upper = t.test(SYN)$conf.int[2],
    shapiro_w = shapiro.test(SYN)$statistic,
    shapiro_p = shapiro.test(SYN)$p.value
  )


# ALL Plasma SYN Total Year 1

EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% filter(DIAGNIV=="PROB") %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    #skewness = skewness(SYN, na.rm = TRUE),
    #kurtosis = kurtosis(SYN, na.rm = TRUE),
    #CI_lower = t.test(SYN)$conf.int[1],
    #CI_upper = t.test(SYN)$conf.int[2],
    #shapiro_w = shapiro.test(SYN)$statistic,
    #shapiro_p = shapiro.test(SYN)$p.value
  )



# Elapsed Time Year 1


EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAGNIV, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>% filter(DIAGNIV=="PROB") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    #skewness = skewness(Diff, na.rm = TRUE),
    #kurtosis = kurtosis(Diff, na.rm = TRUE),
    #CI_lower = t.test(Diff)$conf.int[1],
    #CI_upper = t.test(Diff)$conf.int[2],
    #shapiro_w = shapiro.test(Diff)$statistic,
    #shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 1


EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAGNIV)
  ) %>%
  mutate(Diff=(SYN-Baseline))  %>% filter(DIAGNIV=="PROB") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 1

EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAGNIV, TIME_STUDY)
  ) %>%
  mutate(Diff=(SYN-Baseline)/TIME_STUDY)  %>% filter(DIAGNIV=="PROB") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )













# ALL Plasma SYN Total Year 2

EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% filter(DIAGNIV=="PROB") %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    #skewness = skewness(SYN, na.rm = TRUE),
    #kurtosis = kurtosis(SYN, na.rm = TRUE),
    #CI_lower = t.test(SYN)$conf.int[1],
    #CI_upper = t.test(SYN)$conf.int[2],
    #shapiro_w = shapiro.test(SYN)$statistic,
    #shapiro_p = shapiro.test(SYN)$p.value
  )



# Elapsed Time Year 2


EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAGNIV, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>% filter(DIAGNIV=="PROB") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    #skewness = skewness(Diff, na.rm = TRUE),
    #kurtosis = kurtosis(Diff, na.rm = TRUE),
    #CI_lower = t.test(Diff)$conf.int[1],
    #CI_upper = t.test(Diff)$conf.int[2],
    #shapiro_w = shapiro.test(Diff)$statistic,
    #shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 2


EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAGNIV)
  ) %>%
  mutate(Diff=(SYN-Baseline))  %>% filter(DIAGNIV=="PROB") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    #skewness = skewness(Diff, na.rm = TRUE),
    #kurtosis = kurtosis(Diff, na.rm = TRUE),
    #CI_lower = t.test(Diff)$conf.int[1],
    #CI_upper = t.test(Diff)$conf.int[2],
    #shapiro_w = shapiro.test(Diff)$statistic,
    #shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 2

EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAGNIV, TIME_STUDY)
  ) %>%
  mutate(Diff=(SYN-Baseline)/TIME_STUDY)  %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )






# ALL Plasma SYN Total Year 3

EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    skewness = skewness(SYN, na.rm = TRUE),
    kurtosis = kurtosis(SYN, na.rm = TRUE),
    CI_lower = t.test(SYN)$conf.int[1],
    CI_upper = t.test(SYN)$conf.int[2],
    shapiro_w = shapiro.test(SYN)$statistic,
    shapiro_p = shapiro.test(SYN)$p.value
  )



# Elapsed Time Year 3


EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAGNIV, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 3


EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAGNIV)
  ) %>%
  mutate(Diff=(SYN-Baseline))  %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 3

EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAGNIV, TIME_STUDY)
  ) %>%
  mutate(Diff=(SYN-Baseline)/TIME_STUDY)  %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )

# ---------------


# All data dates groups  --------
dataCohorteManaged <- readRDS("Source/dataCohorteManaged.rds")

Visits_zero <- dataCohorteManaged  %>% select(NUM, dateVisite0) %>% distinct()

AllMSA_Pop_Baseline_671 <- fread("Source/AllMSA_Pop_Baseline_671.txt")
EarlyCT_Pop_Baseline_319 <- fread("Source/EarlyCT_Pop_Baseline_319.txt")


# ----------
# OLIGO CSF aSYN -----------------------------

biofluidsForPaulo <- readRDS("Source/biofluidsForPaulo.rds")
names(biofluidsForPaulo)
CSF_SYN <- biofluidsForPaulo %>% rename("NUM"="idGlobal") %>% 
  select(NUM, datePrelevement, oligo_alpha_syn_q1_pgml_csf, oligo_alpha_syn_q2_pgml_csf, oligo_alpha_syn_q3_ngml_csf)

CSF_SYN <- Visits_zero %>% inner_join(CSF_SYN) %>%
  mutate(TIME_STUDY=time_length(difftime(datePrelevement, dateVisite0 ), "years")) %>%
  mutate(Year= ifelse(TIME_STUDY<=0.5, 0,
                      ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                             ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                    ifelse(TIME_STUDY>=2.5 ,3, NA))))) 

CSF_SYN <- CSF_SYN %>% gather(original, SYN, oligo_alpha_syn_q1_pgml_csf:oligo_alpha_syn_q3_ngml_csf) %>%
  drop_na() %>% arrange(NUM, datePrelevement) %>%
  group_by(NUM, datePrelevement) %>% mutate(SYN=mean(SYN)) %>%
  select(-original) %>% distinct()

# ----------
# Overall MSA Entire  -----------------------

# ALL CSF SYN Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    skewness = skewness(SYN, na.rm = TRUE),
    kurtosis = kurtosis(SYN, na.rm = TRUE),
    CI_lower = t.test(SYN)$conf.int[1],
    CI_upper = t.test(SYN)$conf.int[2],
    shapiro_w = shapiro.test(SYN)$statistic,
    shapiro_p = shapiro.test(SYN)$p.value
  )


# ALL CSF SYN Total Year 1

AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    skewness = skewness(SYN, na.rm = TRUE),
    kurtosis = kurtosis(SYN, na.rm = TRUE),
    CI_lower = t.test(SYN)$conf.int[1],
    CI_upper = t.test(SYN)$conf.int[2],
    shapiro_w = shapiro.test(SYN)$statistic,
    shapiro_p = shapiro.test(SYN)$p.value
  )



# Elapsed Time Year 1


AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 1


AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN)
  ) %>%
  mutate(Diff=(SYN-Baseline))  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 1

AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, TIME_STUDY)
  ) %>%
  mutate(Diff=(SYN-Baseline)/TIME_STUDY)  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )













# ALL CSF SYN Total Year 2

AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    skewness = skewness(SYN, na.rm = TRUE),
    kurtosis = kurtosis(SYN, na.rm = TRUE),
    CI_lower = t.test(SYN)$conf.int[1],
    CI_upper = t.test(SYN)$conf.int[2],
    shapiro_w = shapiro.test(SYN)$statistic,
    shapiro_p = shapiro.test(SYN)$p.value
  )



# Elapsed Time Year 2


AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 2


AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN)
  ) %>%
  mutate(Diff=(SYN-Baseline))  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    #CI_lower = t.test(Diff)$conf.int[1],
    #CI_upper = t.test(Diff)$conf.int[2],
    #shapiro_w = shapiro.test(Diff)$statistic,
    #shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 2

AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, TIME_STUDY)
  ) %>%
  mutate(Diff=(SYN-Baseline)/TIME_STUDY)  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )






# ALL CSF SYN Total Year 3

AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    skewness = skewness(SYN, na.rm = TRUE),
    kurtosis = kurtosis(SYN, na.rm = TRUE),
    CI_lower = t.test(SYN)$conf.int[1],
    CI_upper = t.test(SYN)$conf.int[2],
    shapiro_w = shapiro.test(SYN)$statistic,
    shapiro_p = shapiro.test(SYN)$p.value
  )



# Elapsed Time Year 3


AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 3


AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN)
  ) %>%
  mutate(Diff=(SYN-Baseline))  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 3

AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, TIME_STUDY)
  ) %>%
  mutate(Diff=(SYN-Baseline)/TIME_STUDY)  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )



# -----------
# Overall MSA Entire  MSA P vs MSA C  -----------------------

# ALL CSF SYN Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    skewness = skewness(SYN, na.rm = TRUE),
    kurtosis = kurtosis(SYN, na.rm = TRUE),
    CI_lower = t.test(SYN)$conf.int[1],
    CI_upper = t.test(SYN)$conf.int[2],
    shapiro_w = shapiro.test(SYN)$statistic,
    shapiro_p = shapiro.test(SYN)$p.value
  )


# ALL CSF SYN Total Year 1

AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% filter(DIAG=="CB") %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    skewness = skewness(SYN, na.rm = TRUE),
    kurtosis = kurtosis(SYN, na.rm = TRUE),
    CI_lower = t.test(SYN)$conf.int[1],
    CI_upper = t.test(SYN)$conf.int[2],
    shapiro_w = shapiro.test(SYN)$statistic,
    shapiro_p = shapiro.test(SYN)$p.value
  )



# Elapsed Time Year 1


AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAG, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    #skewness = skewness(Diff, na.rm = TRUE),
    #kurtosis = kurtosis(Diff, na.rm = TRUE),
    #CI_lower = t.test(Diff)$conf.int[1],
    #CI_upper = t.test(Diff)$conf.int[2],
    #shapiro_w = shapiro.test(Diff)$statistic,
    #shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 1


AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAG)
  ) %>%
  mutate(Diff=(SYN-Baseline))  %>% filter(DIAG=="CB") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 1

AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAG, TIME_STUDY)
  ) %>%
  mutate(Diff=(SYN-Baseline)/TIME_STUDY)  %>% filter(DIAG=="PD") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    #skewness = skewness(Diff, na.rm = TRUE),
    #kurtosis = kurtosis(Diff, na.rm = TRUE),
    #CI_lower = t.test(Diff)$conf.int[1],
    #CI_upper = t.test(Diff)$conf.int[2],
    #shapiro_w = shapiro.test(Diff)$statistic,
    #shapiro_p = shapiro.test(Diff)$p.value
  )













# ALL CSF SYN Total Year 2

AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% filter(DIAG=="PD") %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    skewness = skewness(SYN, na.rm = TRUE),
    kurtosis = kurtosis(SYN, na.rm = TRUE),
    CI_lower = t.test(SYN)$conf.int[1],
    CI_upper = t.test(SYN)$conf.int[2],
    shapiro_w = shapiro.test(SYN)$statistic,
    shapiro_p = shapiro.test(SYN)$p.value
  )



# Elapsed Time Year 2


AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAG, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 2


AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAG)
  ) %>%
  mutate(Diff=(SYN-Baseline))  %>% filter(DIAG=="PD") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    #skewness = skewness(Diff, na.rm = TRUE),
    #kurtosis = kurtosis(Diff, na.rm = TRUE),
    #CI_lower = t.test(Diff)$conf.int[1],
    #CI_upper = t.test(Diff)$conf.int[2],
    #shapiro_w = shapiro.test(Diff)$statistic,
    #shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 2

AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAG, TIME_STUDY)
  ) %>%
  mutate(Diff=(SYN-Baseline)/TIME_STUDY)  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )






# ALL CSF SYN Total Year 3

AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    skewness = skewness(SYN, na.rm = TRUE),
    kurtosis = kurtosis(SYN, na.rm = TRUE),
    CI_lower = t.test(SYN)$conf.int[1],
    CI_upper = t.test(SYN)$conf.int[2],
    shapiro_w = shapiro.test(SYN)$statistic,
    shapiro_p = shapiro.test(SYN)$p.value
  )



# Elapsed Time Year 3


AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAG, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 3


AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAG)
  ) %>%
  mutate(Diff=(SYN-Baseline))  %>% filter(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 3

AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAG, TIME_STUDY)
  ) %>%
  mutate(Diff=(SYN-Baseline)/TIME_STUDY)  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )



# -----------
# Overall MSA Entire  Probable vs Possible  -----------------------

# ALL CSF SYN Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    skewness = skewness(SYN, na.rm = TRUE),
    kurtosis = kurtosis(SYN, na.rm = TRUE),
    CI_lower = t.test(SYN)$conf.int[1],
    CI_upper = t.test(SYN)$conf.int[2],
    shapiro_w = shapiro.test(SYN)$statistic,
    shapiro_p = shapiro.test(SYN)$p.value
  )


# ALL CSF SYN Total Year 1

AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% filter(DIAGNIV=="PROB") %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    skewness = skewness(SYN, na.rm = TRUE),
    kurtosis = kurtosis(SYN, na.rm = TRUE),
    CI_lower = t.test(SYN)$conf.int[1],
    CI_upper = t.test(SYN)$conf.int[2],
    shapiro_w = shapiro.test(SYN)$statistic,
    shapiro_p = shapiro.test(SYN)$p.value
  )



# Elapsed Time Year 1


AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAGNIV, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>% filter(DIAGNIV=="POS") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    #skewness = skewness(Diff, na.rm = TRUE),
    #kurtosis = kurtosis(Diff, na.rm = TRUE),
    #CI_lower = t.test(Diff)$conf.int[1],
    #CI_upper = t.test(Diff)$conf.int[2],
    #shapiro_w = shapiro.test(Diff)$statistic,
    #shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 1


AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAGNIV)
  ) %>%
  mutate(Diff=(SYN-Baseline))  %>% filter(DIAGNIV=="POS") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    #skewness = skewness(Diff, na.rm = TRUE),
    #kurtosis = kurtosis(Diff, na.rm = TRUE),
    #CI_lower = t.test(Diff)$conf.int[1],
    #CI_upper = t.test(Diff)$conf.int[2],
    #shapiro_w = shapiro.test(Diff)$statistic,
    #shapiro_p = shapiro.test(Diff)$p.value
  )



# Standardized Year 1

AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAGNIV, TIME_STUDY)
  ) %>%
  mutate(Diff=(SYN-Baseline)/TIME_STUDY)  %>% filter(DIAGNIV=="PROB") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )













# ALL CSF SYN Total Year 2

AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% filter(DIAGNIV=="POS") %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    #skewness = skewness(SYN, na.rm = TRUE),
    #kurtosis = kurtosis(SYN, na.rm = TRUE),
    #CI_lower = t.test(SYN)$conf.int[1],
    #CI_upper = t.test(SYN)$conf.int[2],
    #shapiro_w = shapiro.test(SYN)$statistic,
    #shapiro_p = shapiro.test(SYN)$p.value
  )



# Elapsed Time Year 2


AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAGNIV, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>% filter(DIAGNIV=="POS") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    #skewness = skewness(Diff, na.rm = TRUE),
    #kurtosis = kurtosis(Diff, na.rm = TRUE),
    #CI_lower = t.test(Diff)$conf.int[1],
    #CI_upper = t.test(Diff)$conf.int[2],
    #shapiro_w = shapiro.test(Diff)$statistic,
    #shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 2


AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAGNIV)
  ) %>%
  mutate(Diff=(SYN-Baseline))  %>% filter(DIAGNIV=="PROB") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    #skewness = skewness(Diff, na.rm = TRUE),
    #kurtosis = kurtosis(Diff, na.rm = TRUE),
    #CI_lower = t.test(Diff)$conf.int[1],
    #CI_upper = t.test(Diff)$conf.int[2],
    #shapiro_w = shapiro.test(Diff)$statistic,
    #shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 2

AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAGNIV, TIME_STUDY)
  ) %>%
  mutate(Diff=(SYN-Baseline)/TIME_STUDY)  %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )






# ALL CSF SYN Total Year 3

AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    skewness = skewness(SYN, na.rm = TRUE),
    kurtosis = kurtosis(SYN, na.rm = TRUE),
    CI_lower = t.test(SYN)$conf.int[1],
    CI_upper = t.test(SYN)$conf.int[2],
    shapiro_w = shapiro.test(SYN)$statistic,
    shapiro_p = shapiro.test(SYN)$p.value
  )



# Elapsed Time Year 3


AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAGNIV, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 3


AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAGNIV)
  ) %>%
  mutate(Diff=(SYN-Baseline))  %>% filter(DIAGNIV=="PROB") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    #skewness = skewness(Diff, na.rm = TRUE),
    #kurtosis = kurtosis(Diff, na.rm = TRUE),
    #CI_lower = t.test(Diff)$conf.int[1],
    #CI_upper = t.test(Diff)$conf.int[2],
    #shapiro_w = shapiro.test(Diff)$statistic,
    #shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 3

AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAGNIV, TIME_STUDY)
  ) %>%
  mutate(Diff=(SYN-Baseline)/TIME_STUDY)  %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )

# ---------------


# Target MSA Entire  -----------------------

# ALL CSF SYN Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    skewness = skewness(SYN, na.rm = TRUE),
    kurtosis = kurtosis(SYN, na.rm = TRUE),
    CI_lower = t.test(SYN)$conf.int[1],
    CI_upper = t.test(SYN)$conf.int[2],
    shapiro_w = shapiro.test(SYN)$statistic,
    shapiro_p = shapiro.test(SYN)$p.value
  )


# ALL CSF SYN Total Year 1

EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    skewness = skewness(SYN, na.rm = TRUE),
    kurtosis = kurtosis(SYN, na.rm = TRUE),
    CI_lower = t.test(SYN)$conf.int[1],
    CI_upper = t.test(SYN)$conf.int[2],
    shapiro_w = shapiro.test(SYN)$statistic,
    shapiro_p = shapiro.test(SYN)$p.value
  )



# Elapsed Time Year 1


EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 1


EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN)
  ) %>%
  mutate(Diff=(SYN-Baseline))  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 1

EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, TIME_STUDY)
  ) %>%
  mutate(Diff=(SYN-Baseline)/TIME_STUDY)  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )













# ALL CSF SYN Total Year 2

EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    skewness = skewness(SYN, na.rm = TRUE),
    kurtosis = kurtosis(SYN, na.rm = TRUE),
    CI_lower = t.test(SYN)$conf.int[1],
    CI_upper = t.test(SYN)$conf.int[2],
    shapiro_w = shapiro.test(SYN)$statistic,
    shapiro_p = shapiro.test(SYN)$p.value
  )



# Elapsed Time Year 2


EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 2


EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN)
  ) %>%
  mutate(Diff=(SYN-Baseline))  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    #skewness = skewness(Diff, na.rm = TRUE),
    #kurtosis = kurtosis(Diff, na.rm = TRUE),
    #CI_lower = t.test(Diff)$conf.int[1],
    #CI_upper = t.test(Diff)$conf.int[2],
    #shapiro_w = shapiro.test(Diff)$statistic,
    #shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 2

EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, TIME_STUDY)
  ) %>%
  mutate(Diff=(SYN-Baseline)/TIME_STUDY)  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )






# ALL CSF SYN Total Year 3

EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    skewness = skewness(SYN, na.rm = TRUE),
    kurtosis = kurtosis(SYN, na.rm = TRUE),
    CI_lower = t.test(SYN)$conf.int[1],
    CI_upper = t.test(SYN)$conf.int[2],
    shapiro_w = shapiro.test(SYN)$statistic,
    shapiro_p = shapiro.test(SYN)$p.value
  )



# Elapsed Time Year 3


EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 3


EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN)
  ) %>%
  mutate(Diff=(SYN-Baseline))  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    #skewness = skewness(Diff, na.rm = TRUE),
    #kurtosis = kurtosis(Diff, na.rm = TRUE),
    #CI_lower = t.test(Diff)$conf.int[1],
    #CI_upper = t.test(Diff)$conf.int[2],
    #shapiro_w = shapiro.test(Diff)$statistic,
    #shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 3

EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, TIME_STUDY)
  ) %>%
  mutate(Diff=(SYN-Baseline)/TIME_STUDY)  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )



# -----------
# Target MSA Entire  MSA P vs MSA C  -----------------------

# ALL CSF SYN Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    skewness = skewness(SYN, na.rm = TRUE),
    kurtosis = kurtosis(SYN, na.rm = TRUE),
    CI_lower = t.test(SYN)$conf.int[1],
    CI_upper = t.test(SYN)$conf.int[2],
    shapiro_w = shapiro.test(SYN)$statistic,
    shapiro_p = shapiro.test(SYN)$p.value
  )


# ALL CSF SYN Total Year 1

EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    skewness = skewness(SYN, na.rm = TRUE),
    kurtosis = kurtosis(SYN, na.rm = TRUE),
    CI_lower = t.test(SYN)$conf.int[1],
    CI_upper = t.test(SYN)$conf.int[2],
    shapiro_w = shapiro.test(SYN)$statistic,
    shapiro_p = shapiro.test(SYN)$p.value
  )



# Elapsed Time Year 1


EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAG, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    #skewness = skewness(Diff, na.rm = TRUE),
    #kurtosis = kurtosis(Diff, na.rm = TRUE),
    #CI_lower = t.test(Diff)$conf.int[1],
    #CI_upper = t.test(Diff)$conf.int[2],
    #shapiro_w = shapiro.test(Diff)$statistic,
    #shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 1


EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAG)
  ) %>%
  mutate(Diff=(SYN-Baseline))  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 1

EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAG, TIME_STUDY)
  ) %>%
  mutate(Diff=(SYN-Baseline)/TIME_STUDY)  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )













# ALL CSF SYN Total Year 2

EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% filter(DIAG=="PD") %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    skewness = skewness(SYN, na.rm = TRUE),
    kurtosis = kurtosis(SYN, na.rm = TRUE),
    CI_lower = t.test(SYN)$conf.int[1],
    CI_upper = t.test(SYN)$conf.int[2],
    shapiro_w = shapiro.test(SYN)$statistic,
    shapiro_p = shapiro.test(SYN)$p.value
  )



# Elapsed Time Year 2


EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAG, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>% filter(DIAG=="CB") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    #skewness = skewness(Diff, na.rm = TRUE),
    #kurtosis = kurtosis(Diff, na.rm = TRUE),
    #CI_lower = t.test(Diff)$conf.int[1],
    #CI_upper = t.test(Diff)$conf.int[2],
    #shapiro_w = shapiro.test(Diff)$statistic,
    #shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 2


EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAG)
  ) %>%
  mutate(Diff=(SYN-Baseline))  %>% filter(DIAG=="CB") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    #skewness = skewness(Diff, na.rm = TRUE),
    #kurtosis = kurtosis(Diff, na.rm = TRUE),
    #CI_lower = t.test(Diff)$conf.int[1],
    #CI_upper = t.test(Diff)$conf.int[2],
    #shapiro_w = shapiro.test(Diff)$statistic,
    #shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 2

EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAG, TIME_STUDY)
  ) %>%
  mutate(Diff=(SYN-Baseline)/TIME_STUDY)  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )






# ALL CSF SYN Total Year 3

EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    skewness = skewness(SYN, na.rm = TRUE),
    kurtosis = kurtosis(SYN, na.rm = TRUE),
    CI_lower = t.test(SYN)$conf.int[1],
    CI_upper = t.test(SYN)$conf.int[2],
    shapiro_w = shapiro.test(SYN)$statistic,
    shapiro_p = shapiro.test(SYN)$p.value
  )



# Elapsed Time Year 3


EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAG, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 3


EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAG)
  ) %>%
  mutate(Diff=(SYN-Baseline))  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 3

EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAG, TIME_STUDY)
  ) %>%
  mutate(Diff=(SYN-Baseline)/TIME_STUDY)  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )



# -----------
# Target MSA Entire  Probable vs Possible  -----------------------

# ALL CSF SYN Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    skewness = skewness(SYN, na.rm = TRUE),
    kurtosis = kurtosis(SYN, na.rm = TRUE),
    CI_lower = t.test(SYN)$conf.int[1],
    CI_upper = t.test(SYN)$conf.int[2],
    shapiro_w = shapiro.test(SYN)$statistic,
    shapiro_p = shapiro.test(SYN)$p.value
  )


# ALL CSF SYN Total Year 1

EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% filter(DIAGNIV=="PROB") %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    skewness = skewness(SYN, na.rm = TRUE),
    kurtosis = kurtosis(SYN, na.rm = TRUE),
    CI_lower = t.test(SYN)$conf.int[1],
    CI_upper = t.test(SYN)$conf.int[2],
    shapiro_w = shapiro.test(SYN)$statistic,
    shapiro_p = shapiro.test(SYN)$p.value
  )



# Elapsed Time Year 1


EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAGNIV, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>% filter(DIAGNIV=="PROB") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 1


EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAGNIV)
  ) %>%
  mutate(Diff=(SYN-Baseline))  %>% filter(DIAGNIV=="POS") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    #skewness = skewness(Diff, na.rm = TRUE),
    #kurtosis = kurtosis(Diff, na.rm = TRUE),
    #CI_lower = t.test(Diff)$conf.int[1],
    #CI_upper = t.test(Diff)$conf.int[2],
    #shapiro_w = shapiro.test(Diff)$statistic,
    #shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 1

EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAGNIV, TIME_STUDY)
  ) %>%
  mutate(Diff=(SYN-Baseline)/TIME_STUDY)  %>% filter(DIAGNIV=="POS") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    # skewness = skewness(Diff, na.rm = TRUE),
    # kurtosis = kurtosis(Diff, na.rm = TRUE),
    #CI_lower = t.test(Diff)$conf.int[1],
    #CI_upper = t.test(Diff)$conf.int[2],
    #shapiro_w = shapiro.test(Diff)$statistic,
    #shapiro_p = shapiro.test(Diff)$p.value
  )













# ALL CSF SYN Total Year 2

EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% filter(DIAGNIV=="POS") %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    #skewness = skewness(SYN, na.rm = TRUE),
    #kurtosis = kurtosis(SYN, na.rm = TRUE),
    #CI_lower = t.test(SYN)$conf.int[1],
    #CI_upper = t.test(SYN)$conf.int[2],
    #shapiro_w = shapiro.test(SYN)$statistic,
    #shapiro_p = shapiro.test(SYN)$p.value
  )



# Elapsed Time Year 2


EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAGNIV, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>% filter(DIAGNIV=="POS") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    #skewness = skewness(Diff, na.rm = TRUE),
    #kurtosis = kurtosis(Diff, na.rm = TRUE),
    #CI_lower = t.test(Diff)$conf.int[1],
    #CI_upper = t.test(Diff)$conf.int[2],
    #shapiro_w = shapiro.test(Diff)$statistic,
    #shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 2


EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAGNIV)
  ) %>%
  mutate(Diff=(SYN-Baseline))  %>% filter(DIAGNIV=="PROB") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    #skewness = skewness(Diff, na.rm = TRUE),
    #kurtosis = kurtosis(Diff, na.rm = TRUE),
    #CI_lower = t.test(Diff)$conf.int[1],
    #CI_upper = t.test(Diff)$conf.int[2],
    #shapiro_w = shapiro.test(Diff)$statistic,
    #shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 2

EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAGNIV, TIME_STUDY)
  ) %>%
  mutate(Diff=(SYN-Baseline)/TIME_STUDY)  %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )






# ALL CSF SYN Total Year 3

EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(SYN),
    sd=sd(SYN),
    median=median(SYN),
    Q1 = quantile(SYN, 0.25),
    Q3 = quantile(SYN, 0.75),
    min=min(SYN),
    max=max(SYN),
    n=n(),
    skewness = skewness(SYN, na.rm = TRUE),
    kurtosis = kurtosis(SYN, na.rm = TRUE),
    CI_lower = t.test(SYN)$conf.int[1],
    CI_upper = t.test(SYN)$conf.int[2],
    shapiro_w = shapiro.test(SYN)$statistic,
    shapiro_p = shapiro.test(SYN)$p.value
  )



# Elapsed Time Year 3


EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAGNIV, TIME_STUDY) %>%
  mutate(Diff=TIME_STUDY*12)  %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Change from baseline Year 3


EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAGNIV)
  ) %>%
  mutate(Diff=(SYN-Baseline))  %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )


# Standardized Year 3

EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN, DIAGNIV, TIME_STUDY)
  ) %>%
  mutate(Diff=(SYN-Baseline)/TIME_STUDY)  %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )

# ---------------


# Student T-tests for deltas in Fluid Biormarkers Prob vs Poss, MSA-C vs MSA-P ---------------------------------------
library(rstatix)

# -----------
# All data dates groups  --------
dataCohorteManaged <- readRDS("Source/dataCohorteManaged.rds")

Visits_zero <- dataCohorteManaged  %>% select(NUM, dateVisite0) %>% distinct()


AllMSA_Pop_Baseline_671 <- fread("Source/AllMSA_Pop_Baseline_671.txt")
AllMSA_Pop_BaselineYear1_410 <- fread("Source/AllMSA_Pop_BaselineYear1_410.txt")
AllMSA_Pop_BaselineYear1Year2_245 <- fread("Source/AllMSA_Pop_BaselineYear1Year2_245.txt")
AllMSA_Pop_BaselineYear1Year2Year3Plus_158 <- fread("Source/AllMSA_Pop_BaselineYear1Year2Year3Plus_158.txt")

EarlyCT_Pop_Baseline_319 <- fread("Source/EarlyCT_Pop_Baseline_319.txt")
EarlyCT_Pop_BaselineYear1_208 <- fread("Source/EarlyCT_Pop_BaselineYear1_208.txt")
EarlyCT_Pop_BaselineYear1Year2_134 <- fread("Source/EarlyCT_Pop_BaselineYear1Year2_134.txt")
EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 <- fread( "Source/EarlyCT_Pop_BaselineYear1Year2Year3Plus_99.txt")

# ----------
# CSF NFL  -----------------------------

biofluidsForPaulo <- readRDS("Source/biofluidsForPaulo.rds")

CSF_NFL <- biofluidsForPaulo %>% rename("NUM"="idGlobal") %>% 
  select(NUM, datePrelevement, nfl_pgml_csf, nfl_ipp_pgml_csf, nfl_pgml_csf_fusion)

CSF_NFL <- Visits_zero %>% inner_join(CSF_NFL) %>%
  mutate(TIME_STUDY=time_length(difftime(datePrelevement, dateVisite0 ), "years")) %>%
  mutate(Year= ifelse(TIME_STUDY<=0.5, 0,
                      ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                             ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                    ifelse(TIME_STUDY>=2.5 ,3, NA))))) 

CSF_NFL <- CSF_NFL %>% gather(original, NFL, nfl_pgml_csf:nfl_pgml_csf_fusion) %>%
  drop_na() %>% arrange(NUM, datePrelevement) %>%
  group_by(NUM, datePrelevement) %>% mutate(NFL=mean(NFL)) %>%
  select(-original) %>% distinct()

# -------
# Overall MSA -------------------------------

# Year 1
AllMSA_Pop_Baseline_671 %>% inner_join(CSF_NFL) %>% filter(Year==0) %>% 
  filter(!is.na(NFL)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  NFL) %>%
  rename("Baseline"="NFL") %>%
  left_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(CSF_NFL) %>% filter(Year==1) %>%
      filter(!is.na(NFL)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, NFL)
  ) %>% 
  mutate(Diff= (NFL-Baseline)/TIME_STUDY)  %>%
  t_test(Diff~DIAG, detailed = T,var.equal = TRUE) 
 #t_test(Diff~DIAGNIV, detailed = T, var.equal = TRUE) 


# Year 2
AllMSA_Pop_Baseline_671 %>% inner_join(CSF_NFL) %>% filter(Year==0) %>% 
  filter(!is.na(NFL)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  NFL) %>%
  rename("Baseline"="NFL") %>%
  left_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(CSF_NFL) %>% filter(Year==2) %>%
      filter(!is.na(NFL)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, NFL)
  ) %>% 
  mutate(Diff= (NFL-Baseline)/TIME_STUDY)  %>%
  #t_test(Diff~DIAG, detailed = T,var.equal = TRUE) 
  t_test(Diff~DIAGNIV,  detailed = T,var.equal = TRUE) 


# Year 3
AllMSA_Pop_Baseline_671 %>% inner_join(CSF_NFL) %>% filter(Year==0) %>% 
  filter(!is.na(NFL)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  NFL) %>%
  rename("Baseline"="NFL") %>%
  left_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(CSF_NFL) %>% filter(Year==3) %>%
      filter(!is.na(NFL)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, NFL)
  ) %>% 
  mutate(Diff= (NFL-Baseline)/TIME_STUDY)  %>%
  t_test(Diff~DIAG, detailed = T,var.equal = TRUE) 
  #t_test(Diff~DIAGNIV, detailed = T, var.equal = TRUE) 

# --------------


# Early CT Takeda  -------------------------------

# Year 1
EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_NFL) %>% filter(Year==0) %>% 
  filter(!is.na(NFL)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  NFL) %>%
  rename("Baseline"="NFL") %>%
  left_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_NFL) %>% filter(Year==1) %>%
      filter(!is.na(NFL)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, NFL)
  ) %>% 
  mutate(Diff= (NFL-Baseline)/TIME_STUDY)  %>%
  #t_test(Diff~DIAG, detailed = T,var.equal = TRUE) 
 t_test(Diff~DIAGNIV, detailed = T, var.equal = TRUE) 


# Year 2
EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_NFL) %>% filter(Year==0) %>% 
  filter(!is.na(NFL)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  NFL) %>%
  rename("Baseline"="NFL") %>%
  left_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_NFL) %>% filter(Year==2) %>%
      filter(!is.na(NFL)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, NFL)
  ) %>% 
  mutate(Diff= (NFL-Baseline)/TIME_STUDY)  %>%
  t_test(Diff~DIAG, detailed = T,var.equal = TRUE) 
  #t_test(Diff~DIAGNIV,  detailed = T,var.equal = TRUE) 


# Year 3
EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_NFL) %>% filter(Year==0) %>% 
  filter(!is.na(NFL)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  NFL) %>%
  rename("Baseline"="NFL") %>%
  left_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_NFL) %>% filter(Year==3) %>%
      filter(!is.na(NFL)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, NFL)
  ) %>% 
  mutate(Diff= (NFL-Baseline)/TIME_STUDY)  %>%
  t_test(Diff~DIAG, detailed = T,var.equal = TRUE) 
  #t_test(Diff~DIAGNIV, detailed = T, var.equal = TRUE) 

# --------------



# Plasma NFL  -----------------------------

biofluidsForPaulo <- readRDS("Source/biofluidsForPaulo.rds")

Plasma_NFL <- biofluidsForPaulo %>% rename("NUM"="idGlobal") %>% 
  select(NUM, datePrelevement, nfl_pgml_plas, nfl_ipp_pgml_plas)

Plasma_NFL <- Visits_zero %>% inner_join(Plasma_NFL) %>%
  mutate(TIME_STUDY=time_length(difftime(datePrelevement, dateVisite0 ), "years")) %>%
  mutate(Year= ifelse(TIME_STUDY<=0.5, 0,
                      ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                             ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                    ifelse(TIME_STUDY>=2.5 ,3, NA))))) 

Plasma_NFL <- Plasma_NFL %>% gather(original, NFL, nfl_pgml_plas:nfl_ipp_pgml_plas) %>%
  drop_na() %>% arrange(NUM, datePrelevement) %>%
  group_by(NUM, datePrelevement) %>% mutate(NFL=mean(NFL)) %>%
  select(-original) %>% distinct()

# -------
# Overall MSA -------------------------------

# Year 1
AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_NFL) %>% filter(Year==0) %>% 
  filter(!is.na(NFL)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  NFL) %>%
  rename("Baseline"="NFL") %>%
  left_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_NFL) %>% filter(Year==1) %>%
      filter(!is.na(NFL)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, NFL)
  ) %>% 
  mutate(Diff= (NFL-Baseline)/TIME_STUDY)  %>%
  #t_test(Diff~DIAG, detailed = T,var.equal = TRUE) 
 t_test(Diff~DIAGNIV, detailed = T, var.equal = TRUE) 


# Year 2
AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_NFL) %>% filter(Year==0) %>% 
  filter(!is.na(NFL)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  NFL) %>%
  rename("Baseline"="NFL") %>%
  left_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_NFL) %>% filter(Year==2) %>%
      filter(!is.na(NFL)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, NFL)
  ) %>% 
  mutate(Diff= (NFL-Baseline)/TIME_STUDY)  %>%
  #t_test(Diff~DIAG, detailed = T,var.equal = TRUE) 
  t_test(Diff~DIAGNIV,  detailed = T,var.equal = TRUE) 


# Year 3
AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_NFL) %>% filter(Year==0) %>% 
  filter(!is.na(NFL)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  NFL) %>%
  rename("Baseline"="NFL") %>%
  left_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_NFL) %>% filter(Year==3) %>%
      filter(!is.na(NFL)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, NFL)
  ) %>% 
  mutate(Diff= (NFL-Baseline)/TIME_STUDY)  %>%
  t_test(Diff~DIAG, detailed = T,var.equal = TRUE) 
  #t_test(Diff~DIAGNIV, detailed = T, var.equal = TRUE) 

# --------------


# Early CT Takeda  -------------------------------

# Year 1
EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_NFL) %>% filter(Year==0) %>% 
  filter(!is.na(NFL)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  NFL) %>%
  rename("Baseline"="NFL") %>%
  left_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_NFL) %>% filter(Year==1) %>%
      filter(!is.na(NFL)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, NFL)
  ) %>% 
  mutate(Diff= (NFL-Baseline)/TIME_STUDY)  %>%
  t_test(Diff~DIAG, detailed = T,var.equal = TRUE) 
 #t_test(Diff~DIAGNIV, detailed = T, var.equal = TRUE) 


# Year 2
EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_NFL) %>% filter(Year==0) %>% 
  filter(!is.na(NFL)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  NFL) %>%
  rename("Baseline"="NFL") %>%
  left_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_NFL) %>% filter(Year==2) %>%
      filter(!is.na(NFL)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, NFL)
  ) %>% 
  mutate(Diff= (NFL-Baseline)/TIME_STUDY)  %>%
  t_test(Diff~DIAG, detailed = T,var.equal = TRUE) 
  #t_test(Diff~DIAGNIV,  detailed = T,var.equal = TRUE) 


# Year 3
EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_NFL) %>% filter(Year==0) %>% 
  filter(!is.na(NFL)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  NFL) %>%
  rename("Baseline"="NFL") %>%
  left_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_NFL) %>% filter(Year==3) %>%
      filter(!is.na(NFL)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, NFL)
  ) %>% 
  mutate(Diff= (NFL-Baseline)/TIME_STUDY)  %>%
  t_test(Diff~DIAG, detailed = T,var.equal = TRUE) 
  #t_test(Diff~DIAGNIV, detailed = T, var.equal = TRUE) 

# --------------



# Plasma aSYN  -----------------------------

biofluidsForPaulo <- readRDS("Source/biofluidsForPaulo.rds")
names(biofluidsForPaulo)
Plasma_SYN <- biofluidsForPaulo %>% rename("NUM"="idGlobal") %>% 
  select(NUM, datePrelevement, total_alpha_syn_q1_pgml_plas, total_alpha_syn_q2_pgml_plas)

Plasma_SYN <- Visits_zero %>% inner_join(Plasma_SYN) %>%
  mutate(TIME_STUDY=time_length(difftime(datePrelevement, dateVisite0 ), "years")) %>%
  mutate(Year= ifelse(TIME_STUDY<=0.5, 0,
                      ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                             ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                    ifelse(TIME_STUDY>=2.5 ,3, NA))))) 

Plasma_SYN <- Plasma_SYN %>% gather(original, SYN, total_alpha_syn_q1_pgml_plas:total_alpha_syn_q2_pgml_plas) %>%
  drop_na() %>% arrange(NUM, datePrelevement) %>%
  group_by(NUM, datePrelevement) %>% mutate(SYN=mean(SYN)) %>%
  select(-original) %>% distinct()

# -------
# Overall MSA -------------------------------

# Year 1
AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>% 
  filter(!is.na(SYN)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SYN) %>%
  rename("Baseline"="SYN") %>%
  left_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==1) %>%
      filter(!is.na(SYN)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SYN)
  ) %>% 
  mutate(Diff= (SYN-Baseline)/TIME_STUDY)  %>%
  #t_test(Diff~DIAG, detailed = T,var.equal = TRUE) 
 t_test(Diff~DIAGNIV, detailed = T, var.equal = TRUE) 


# Year 2
AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>% 
  filter(!is.na(SYN)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SYN) %>%
  rename("Baseline"="SYN") %>%
  left_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==2) %>%
      filter(!is.na(SYN)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SYN)
  ) %>% 
  mutate(Diff= (SYN-Baseline)/TIME_STUDY)  %>%
  #t_test(Diff~DIAG, detailed = T,var.equal = TRUE) 
  t_test(Diff~DIAGNIV,  detailed = T,var.equal = TRUE) 


# Year 3
AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>% 
  filter(!is.na(SYN)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SYN) %>%
  rename("Baseline"="SYN") %>%
  left_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==3) %>%
      filter(!is.na(SYN)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SYN)
  ) %>% 
  mutate(Diff= (SYN-Baseline)/TIME_STUDY)  %>%
  t_test(Diff~DIAG, detailed = T,var.equal = TRUE) 
  #t_test(Diff~DIAGNIV, detailed = T, var.equal = TRUE) 

# --------------


# Early CT Takeda  -------------------------------

# Year 1
EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>% 
  filter(!is.na(SYN)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SYN) %>%
  rename("Baseline"="SYN") %>%
  left_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==1) %>%
      filter(!is.na(SYN)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SYN)
  ) %>% 
  mutate(Diff= (SYN-Baseline)/TIME_STUDY)  %>%
  t_test(Diff~DIAG, detailed = T,var.equal = TRUE) 
 #t_test(Diff~DIAGNIV, detailed = T, var.equal = TRUE) 


# Year 2
EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>% 
  filter(!is.na(SYN)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SYN) %>%
  rename("Baseline"="SYN") %>%
  left_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==2) %>%
      filter(!is.na(SYN)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SYN)
  ) %>% 
  mutate(Diff= (SYN-Baseline)/TIME_STUDY)  %>%
  t_test(Diff~DIAG, detailed = T,var.equal = TRUE) 
  #t_test(Diff~DIAGNIV,  detailed = T,var.equal = TRUE) 


# Year 3
EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>% 
  filter(!is.na(SYN)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SYN) %>%
  rename("Baseline"="SYN") %>%
  left_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==3) %>%
      filter(!is.na(SYN)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SYN)
  ) %>% 
  mutate(Diff= (SYN-Baseline)/TIME_STUDY)  %>%
  t_test(Diff~DIAG, detailed = T,var.equal = TRUE) 
  #t_test(Diff~DIAGNIV, detailed = T, var.equal = TRUE) 

# --------------



# CSF aSYN  -----------------------------


biofluidsForPaulo <- readRDS("Source/biofluidsForPaulo.rds")
names(biofluidsForPaulo)
CSF_SYN <- biofluidsForPaulo %>% rename("NUM"="idGlobal") %>% 
  select(NUM, datePrelevement, total_alpha_syn_g_pgml_csf, total_alpha_syn_q1_pgml_csf, total_alpha_syn_q2_pgml_csf, 
         total_alpha_syn_ipp_pgml_csf, totalAlphaSyn_Fusion_pgml_csf)

CSF_SYN <- Visits_zero %>% inner_join(CSF_SYN) %>%
  mutate(TIME_STUDY=time_length(difftime(datePrelevement, dateVisite0 ), "years")) %>%
  mutate(Year= ifelse(TIME_STUDY<=0.5, 0,
                      ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                             ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                    ifelse(TIME_STUDY>=2.5 ,3, NA))))) 

CSF_SYN <- CSF_SYN %>% gather(original, SYN, total_alpha_syn_g_pgml_csf:totalAlphaSyn_Fusion_pgml_csf) %>%
  drop_na() %>% arrange(NUM, datePrelevement) %>%
  group_by(NUM, datePrelevement) %>% mutate(SYN=mean(SYN)) %>%
  select(-original) %>% distinct()

# -------
# Overall MSA -------------------------------

# Year 1
AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>% 
  filter(!is.na(SYN)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SYN) %>%
  rename("Baseline"="SYN") %>%
  left_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==1) %>%
      filter(!is.na(SYN)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SYN)
  ) %>% 
  mutate(Diff= (SYN-Baseline)/TIME_STUDY)  %>%
  t_test(Diff~DIAG, detailed = T,var.equal = TRUE) 
 #t_test(Diff~DIAGNIV, detailed = T, var.equal = TRUE) 


# Year 2
AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>% 
  filter(!is.na(SYN)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SYN) %>%
  rename("Baseline"="SYN") %>%
  left_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==2) %>%
      filter(!is.na(SYN)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SYN)
  ) %>% 
  mutate(Diff= (SYN-Baseline)/TIME_STUDY)  %>%
  #t_test(Diff~DIAG, detailed = T,var.equal = TRUE) 
  t_test(Diff~DIAGNIV,  detailed = T,var.equal = TRUE) 


# Year 3
AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>% 
  filter(!is.na(SYN)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SYN) %>%
  rename("Baseline"="SYN") %>%
  left_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==3) %>%
      filter(!is.na(SYN)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SYN)
  ) %>% 
  mutate(Diff= (SYN-Baseline)/TIME_STUDY)  %>%
  t_test(Diff~DIAG, detailed = T,var.equal = TRUE) 
  #t_test(Diff~DIAGNIV, detailed = T, var.equal = TRUE) 

# --------------


# Early CT Takeda  -------------------------------

# Year 1
EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>% 
  filter(!is.na(SYN)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SYN) %>%
  rename("Baseline"="SYN") %>%
  left_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==1) %>%
      filter(!is.na(SYN)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SYN)
  ) %>% 
  mutate(Diff= (SYN-Baseline)/TIME_STUDY)  %>%
  #t_test(Diff~DIAG, detailed = T,var.equal = TRUE) 
 t_test(Diff~DIAGNIV, detailed = T, var.equal = TRUE) 


# Year 2
EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>% 
  filter(!is.na(SYN)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SYN) %>%
  rename("Baseline"="SYN") %>%
  left_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==2) %>%
      filter(!is.na(SYN)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SYN)
  ) %>% 
  mutate(Diff= (SYN-Baseline)/TIME_STUDY)  %>%
  t_test(Diff~DIAG, detailed = T,var.equal = TRUE) 
  #t_test(Diff~DIAGNIV,  detailed = T,var.equal = TRUE) 


# Year 3
EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>% 
  filter(!is.na(SYN)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SYN) %>%
  rename("Baseline"="SYN") %>%
  left_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==3) %>%
      filter(!is.na(SYN)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SYN)
  ) %>% 
  mutate(Diff= (SYN-Baseline)/TIME_STUDY)  %>%
  t_test(Diff~DIAG, detailed = T,var.equal = TRUE) 
  #t_test(Diff~DIAGNIV, detailed = T, var.equal = TRUE) 

# --------------



# Correlations Internal Between Fluid Biomarkers -----------
# ---------
# All data dates groups  --------
dataCohorteManaged <- readRDS("Source/dataCohorteManaged.rds")

Visits_zero <- dataCohorteManaged  %>% select(NUM, dateVisite0) %>% distinct()

AllMSA_Pop_Baseline_671 <- fread("Source/AllMSA_Pop_Baseline_671.txt")
EarlyCT_Pop_Baseline_319 <- fread("Source/EarlyCT_Pop_Baseline_319.txt")

# --------
# Plasma NFL vs CSF NFL Exact Change --------


# Plasma NFL 

biofluidsForPaulo <- readRDS("Source/biofluidsForPaulo.rds")

Plasma_NFL <- biofluidsForPaulo %>% rename("NUM"="idGlobal") %>% 
  select(NUM, datePrelevement, nfl_pgml_plas, nfl_ipp_pgml_plas)

Plasma_NFL <- Visits_zero %>% inner_join(Plasma_NFL) %>%
  mutate(TIME_STUDY=time_length(difftime(datePrelevement, dateVisite0 ), "years")) %>%
  mutate(Year= ifelse(TIME_STUDY<=0.5, 0,
                      ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                             ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                    ifelse(TIME_STUDY>=2.5 ,3, NA))))) 

Plasma_NFL <- Plasma_NFL %>% gather(original, NFL, nfl_pgml_plas:nfl_ipp_pgml_plas) %>%
  drop_na() %>% arrange(NUM, datePrelevement) %>%
  group_by(NUM, datePrelevement) %>% mutate(NFL=mean(NFL)) %>%
  select(-original) %>% distinct()


# CSF NFL

biofluidsForPaulo <- readRDS("Source/biofluidsForPaulo.rds")

CSF_NFL <- biofluidsForPaulo %>% rename("NUM"="idGlobal") %>% 
  select(NUM, datePrelevement, nfl_pgml_csf, nfl_ipp_pgml_csf, nfl_pgml_csf_fusion)

CSF_NFL <- Visits_zero %>% inner_join(CSF_NFL) %>%
  mutate(TIME_STUDY=time_length(difftime(datePrelevement, dateVisite0 ), "years")) %>%
  mutate(Year= ifelse(TIME_STUDY<=0.5, 0,
                      ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                             ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                    ifelse(TIME_STUDY>=2.5 ,3, NA))))) 

CSF_NFL <- CSF_NFL %>% gather(original, NFL, nfl_pgml_csf:nfl_pgml_csf_fusion) %>%
  drop_na() %>% arrange(NUM, datePrelevement) %>%
  group_by(NUM, datePrelevement) %>% mutate(NFL=mean(NFL)) %>%
  select(-original) %>% distinct()

# ----------


# Overall MSA ---------

# Year 1

AllMSA_Pop_Baseline_671 %>% inner_join(CSF_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(CSF_NFL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL)
  ) %>%
  mutate(Diff=(NFL-Baseline)) %>% select(NUM, Diff) %>% rename("CSF_NFL"="Diff") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_NFL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL)
  ) %>%
  mutate(Diff=(NFL-Baseline)) %>% select(NUM, Diff) %>% rename("Plasma_NFL"="Diff")
  ) %>% 
  inner_join(AllMSA_Pop_Baseline_671) %>%
  # summarise(cor=cor(CSF_NFL, Plasma_NFL, method="spearman"))
  # summarise(cor=broom::tidy(cor.test(CSF_NFL, Plasma_NFL, method="spearman")))
  # group_by(DIAG) %>% summarise(cor=cor(CSF_NFL, Plasma_NFL, method="spearman"))
  # group_by(DIAG) %>% summarise(cor=broom::tidy(cor.test(CSF_NFL, Plasma_NFL, method="spearman")))
  # group_by(DIAGNIV) %>% summarise(cor=broom::tidy(cor.test(CSF_NFL, Plasma_NFL, method="spearman")))
  group_by(DIAGNIV) %>% summarise(cor= broom::tidy(cor.test(CSF_NFL, Plasma_NFL, method="spearman")))



# ----------
# Early CT Takeda  ---------

# Year 1

EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_NFL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL)
  ) %>%
  mutate(Diff=(NFL-Baseline)) %>% select(NUM, Diff) %>% rename("CSF_NFL"="Diff") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_NFL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL)
  ) %>%
  mutate(Diff=(NFL-Baseline)) %>% select(NUM, Diff) %>% rename("Plasma_NFL"="Diff")
  ) %>% 
  inner_join(EarlyCT_Pop_Baseline_319) %>%
  # summarise(cor=cor(CSF_NFL, Plasma_NFL, method="spearman"))
  # summarise(cor=broom::tidy(cor.test(CSF_NFL, Plasma_NFL, method="spearman")))
  # group_by(DIAG) %>% summarise(cor=cor(CSF_NFL, Plasma_NFL, method="spearman"))
  # group_by(DIAG) %>% summarise(cor=broom::tidy(cor.test(CSF_NFL, Plasma_NFL, method="spearman")))
  # group_by(DIAGNIV) %>% summarise(cor=cor(CSF_NFL, Plasma_NFL, method="spearman"))
   group_by(DIAGNIV) %>% summarise(cor=broom::tidy(cor.test(CSF_NFL, Plasma_NFL, method="spearman")))
   

# ---------

# CSF aSYN vs CSF NFL Exact Change --------


# CSF aSYN  

biofluidsForPaulo <- readRDS("Source/biofluidsForPaulo.rds")
names(biofluidsForPaulo)
CSF_SYN <- biofluidsForPaulo %>% rename("NUM"="idGlobal") %>% 
  select(NUM, datePrelevement, total_alpha_syn_g_pgml_csf, total_alpha_syn_q1_pgml_csf, total_alpha_syn_q2_pgml_csf, 
         total_alpha_syn_ipp_pgml_csf, totalAlphaSyn_Fusion_pgml_csf)

CSF_SYN <- Visits_zero %>% inner_join(CSF_SYN) %>%
  mutate(TIME_STUDY=time_length(difftime(datePrelevement, dateVisite0 ), "years")) %>%
  mutate(Year= ifelse(TIME_STUDY<=0.5, 0,
                      ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                             ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                    ifelse(TIME_STUDY>=2.5 ,3, NA))))) 

CSF_SYN <- CSF_SYN %>% gather(original, SYN, total_alpha_syn_g_pgml_csf:totalAlphaSyn_Fusion_pgml_csf) %>%
  drop_na() %>% arrange(NUM, datePrelevement) %>%
  group_by(NUM, datePrelevement) %>% mutate(SYN=mean(SYN)) %>%
  select(-original) %>% distinct()


# CSF NFL

biofluidsForPaulo <- readRDS("Source/biofluidsForPaulo.rds")

CSF_NFL <- biofluidsForPaulo %>% rename("NUM"="idGlobal") %>% 
  select(NUM, datePrelevement, nfl_pgml_csf, nfl_ipp_pgml_csf, nfl_pgml_csf_fusion)

CSF_NFL <- Visits_zero %>% inner_join(CSF_NFL) %>%
  mutate(TIME_STUDY=time_length(difftime(datePrelevement, dateVisite0 ), "years")) %>%
  mutate(Year= ifelse(TIME_STUDY<=0.5, 0,
                      ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                             ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                    ifelse(TIME_STUDY>=2.5 ,3, NA))))) 

CSF_NFL <- CSF_NFL %>% gather(original, NFL, nfl_pgml_csf:nfl_pgml_csf_fusion) %>%
  drop_na() %>% arrange(NUM, datePrelevement) %>%
  group_by(NUM, datePrelevement) %>% mutate(NFL=mean(NFL)) %>%
  select(-original) %>% distinct()

# ----------


# Overall MSA ---------

# Year 1

AllMSA_Pop_Baseline_671 %>% inner_join(CSF_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(CSF_NFL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL)
  ) %>%
  mutate(Diff=(NFL-Baseline)) %>% select(NUM, Diff) %>% rename("CSF_NFL"="Diff") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN)
  ) %>%
  mutate(Diff=(SYN-Baseline)) %>% select(NUM, Diff) %>% rename("CSF_SYN"="Diff")
  ) %>% 
  inner_join(AllMSA_Pop_Baseline_671) %>%
   #summarise(cor=cor(CSF_NFL, CSF_SYN, method="spearman"))
   #summarise(cor=broom::tidy(cor.test(CSF_NFL, CSF_SYN, method="spearman")))
  # group_by(DIAG) %>% summarise(cor=cor(CSF_NFL, CSF_SYN, method="spearman"))
  # group_by(DIAG) %>% summarise(cor=broom::tidy(cor.test(CSF_NFL, CSF_SYN, method="spearman")))
  # group_by(DIAGNIV) %>% summarise(cor=cor(CSF_NFL, CSF_SYN, method="spearman"))
  group_by(DIAGNIV) %>% summarise(cor=broom::tidy(cor.test(CSF_NFL, CSF_SYN, method="spearman")))


# ----------
# Early CT Takeda  ---------

# Year 1

EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_NFL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL)
  ) %>%
  mutate(Diff=(NFL-Baseline)) %>% select(NUM, Diff) %>% rename("CSF_NFL"="Diff") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN)
  ) %>%
  mutate(Diff=(SYN-Baseline)) %>% select(NUM, Diff) %>% rename("CSF_SYN"="Diff")
  ) %>% 
  inner_join(EarlyCT_Pop_Baseline_319) %>%
  # summarise(cor=cor(CSF_NFL, CSF_SYN, method="spearman"))
  # summarise(cor=broom::tidy(cor.test(CSF_NFL, CSF_SYN, method="spearman")))
  # group_by(DIAG) %>% summarise(cor=cor(CSF_NFL, CSF_SYN, method="spearman"))
  group_by(DIAG) %>% summarise(cor=broom::tidy(cor.test(CSF_NFL, CSF_SYN, method="spearman")))
  # group_by(DIAGNIV) %>% summarise(cor=cor(CSF_NFL, CSF_SYN, method="spearman"))


# ---------

# CSF aSYN vs Plasma aSYN  Exact Change --------


# CSF aSYN  

biofluidsForPaulo <- readRDS("Source/biofluidsForPaulo.rds")
names(biofluidsForPaulo)
CSF_SYN <- biofluidsForPaulo %>% rename("NUM"="idGlobal") %>% 
  select(NUM, datePrelevement, total_alpha_syn_g_pgml_csf, total_alpha_syn_q1_pgml_csf, total_alpha_syn_q2_pgml_csf, 
         total_alpha_syn_ipp_pgml_csf, totalAlphaSyn_Fusion_pgml_csf)

CSF_SYN <- Visits_zero %>% inner_join(CSF_SYN) %>%
  mutate(TIME_STUDY=time_length(difftime(datePrelevement, dateVisite0 ), "years")) %>%
  mutate(Year= ifelse(TIME_STUDY<=0.5, 0,
                      ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                             ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                    ifelse(TIME_STUDY>=2.5 ,3, NA))))) 

CSF_SYN <- CSF_SYN %>% gather(original, SYN, total_alpha_syn_g_pgml_csf:totalAlphaSyn_Fusion_pgml_csf) %>%
  drop_na() %>% arrange(NUM, datePrelevement) %>%
  group_by(NUM, datePrelevement) %>% mutate(SYN=mean(SYN)) %>%
  select(-original) %>% distinct()


# Plasma aSYN 


biofluidsForPaulo <- readRDS("Source/biofluidsForPaulo.rds")
names(biofluidsForPaulo)
Plasma_SYN <- biofluidsForPaulo %>% rename("NUM"="idGlobal") %>% 
  select(NUM, datePrelevement, total_alpha_syn_q1_pgml_plas, total_alpha_syn_q2_pgml_plas)

Plasma_SYN <- Visits_zero %>% inner_join(Plasma_SYN) %>%
  mutate(TIME_STUDY=time_length(difftime(datePrelevement, dateVisite0 ), "years")) %>%
  mutate(Year= ifelse(TIME_STUDY<=0.5, 0,
                      ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                             ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                    ifelse(TIME_STUDY>=2.5 ,3, NA))))) 

Plasma_SYN <- Plasma_SYN %>% gather(original, SYN, total_alpha_syn_q1_pgml_plas:total_alpha_syn_q2_pgml_plas) %>%
  drop_na() %>% arrange(NUM, datePrelevement) %>%
  group_by(NUM, datePrelevement) %>% mutate(SYN=mean(SYN)) %>%
  select(-original) %>% distinct()


# ----------


# Overall MSA ---------

# Year 1

AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_SYN) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN)
  ) %>%
  mutate(Diff=(SYN-Baseline)) %>% select(NUM, Diff) %>% rename("Plasma_SYN"="Diff") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN)
  ) %>%
  mutate(Diff=(SYN-Baseline)) %>% select(NUM, Diff) %>% rename("CSF_SYN"="Diff")
  ) %>% 
  inner_join(AllMSA_Pop_Baseline_671) %>%
   summarise(cor=cor(Plasma_SYN, CSF_SYN, method="spearman"))
  # group_by(DIAG) %>% summarise(cor=cor(Plasma_SYN, CSF_SYN, method="spearman"))
  # group_by(DIAGNIV) %>% summarise(cor=cor(Plasma_SYN, CSF_SYN, method="spearman"))


# ----------
# Early CT Takeda  ---------

# Year 1

EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_SYN) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN)
  ) %>%
  mutate(Diff=(SYN-Baseline)) %>% select(NUM, Diff) %>% rename("Plasma_SYN"="Diff") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN)
  ) %>%
  mutate(Diff=(SYN-Baseline)) %>% select(NUM, Diff) %>% rename("CSF_SYN"="Diff")
  ) %>% 
  inner_join(EarlyCT_Pop_Baseline_319) %>%
  # summarise(cor=cor(Plasma_SYN, CSF_SYN, method="spearman"))
  # group_by(DIAG) %>% summarise(cor=cor(Plasma_SYN, CSF_SYN, method="spearman"))
   group_by(DIAGNIV) %>% summarise(cor=cor(Plasma_SYN, CSF_SYN, method="spearman"))


# ---------

# Correlations Between Clinical and Fluid Biomarkers -----------
# ---------
# All data dates groups  --------
dataCohorteManaged <- readRDS("Source/dataCohorteManaged.rds")


dataCohorteManaged <- dataCohorteManaged %>% mutate(Year= ifelse(TIME_STUDY<=0.5, 0,
                                                                 ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                                                                        ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                                                               ifelse(TIME_STUDY>=2.5 ,3, NA))))) 


Visits_zero <- dataCohorteManaged  %>% select(NUM, dateVisite0) %>% distinct()

AllMSA_Pop_Baseline_671 <- fread("Source/AllMSA_Pop_Baseline_671.txt")
EarlyCT_Pop_Baseline_319 <- fread("Source/EarlyCT_Pop_Baseline_319.txt")

# --------
# CSF NFL vs Modified UMSARS 1 Exact Change --------


# UMSARS 1 

UMSARS1 <- dataCohorteManaged %>% 
  select(NUM, DATECONSULT, TIME_STUDY, Year, UMSARS1_1:UMSARS1_TOT)
UMSARS1$UMSARS1_TOT_v2 <- rowSums(UMSARS1[, 5:16], na.rm = TRUE)
UMSARS1$missing_na <- rowSums(is.na(UMSARS1[, 5:16]))
UMSARS1 <-  UMSARS1 %>%  select(NUM, TIME_STUDY, Year, UMSARS1_TOT, UMSARS1_11)
UMSARS1 <- UMSARS1 %>% mutate(UMSARS1_TOT_FDA=UMSARS1_TOT-UMSARS1_11)




# CSF NFL

biofluidsForPaulo <- readRDS("Source/biofluidsForPaulo.rds")

CSF_NFL <- biofluidsForPaulo %>% rename("NUM"="idGlobal") %>% 
  select(NUM, datePrelevement, nfl_pgml_csf, nfl_ipp_pgml_csf, nfl_pgml_csf_fusion)

CSF_NFL <- Visits_zero %>% inner_join(CSF_NFL) %>%
  mutate(TIME_STUDY=time_length(difftime(datePrelevement, dateVisite0 ), "years")) %>%
  mutate(Year= ifelse(TIME_STUDY<=0.5, 0,
                      ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                             ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                    ifelse(TIME_STUDY>=2.5 ,3, NA))))) 

CSF_NFL <- CSF_NFL %>% gather(original, NFL, nfl_pgml_csf:nfl_pgml_csf_fusion) %>%
  drop_na() %>% arrange(NUM, datePrelevement) %>%
  group_by(NUM, datePrelevement) %>% mutate(NFL=mean(NFL)) %>%
  select(-original) %>% distinct()

# ----------


# Overall MSA ---------

# Year 1

AllMSA_Pop_Baseline_671 %>% inner_join(CSF_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(CSF_NFL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL)
  ) %>%
  mutate(Diff=(NFL-Baseline)) %>% select(NUM, Diff) %>% rename("CSF_NFL"="Diff") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, UMSARS1_TOT_FDA) %>%
  rename("Baseline"="UMSARS1_TOT_FDA") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(UMSARS1) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, UMSARS1_TOT_FDA)
  ) %>%
  mutate(Diff=(UMSARS1_TOT_FDA-Baseline)) %>% select(NUM, Diff) %>% rename("UMSARS1_TOT_FDA"="Diff")
  ) %>% 
  inner_join(AllMSA_Pop_Baseline_671) %>%
  # summarise(cor=cor(CSF_NFL, UMSARS1_TOT_FDA, method="spearman"))
   summarise(cor=broom::tidy(cor.test(CSF_NFL, UMSARS1_TOT_FDA, method="spearman")))
  # group_by(DIAG) %>% summarise(cor=cor(CSF_NFL, UMSARS1_TOT_FDA, method="spearman"))
  # group_by(DIAG) %>% summarise(cor=broom::tidy(cor.test(CSF_NFL, UMSARS1_TOT_FDA, method="spearman")))
  # group_by(DIAGNIV) %>% summarise(cor=cor(CSF_NFL, UMSARS1_TOT_FDA, method="spearman"))
  # group_by(DIAGNIV) %>% summarise(cor=broom::tidy(cor.test(CSF_NFL, UMSARS1_TOT_FDA, method="spearman")))



# ----------
# Early CT Takeda  ---------

# Year 1

EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_NFL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL)
  ) %>%
  mutate(Diff=(NFL-Baseline)) %>% select(NUM, Diff) %>% rename("CSF_NFL"="Diff") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, UMSARS1_TOT_FDA) %>%
  rename("Baseline"="UMSARS1_TOT_FDA") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(UMSARS1) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, UMSARS1_TOT_FDA)
  ) %>%
  mutate(Diff=(UMSARS1_TOT_FDA-Baseline)) %>% select(NUM, Diff) %>% rename("UMSARS1_TOT_FDA"="Diff")
  ) %>% 
  inner_join(EarlyCT_Pop_Baseline_319) %>%
  # summarise(cor=cor(CSF_NFL, UMSARS1_TOT_FDA, method="spearman"))
  summarise(cor=broom::tidy(cor.test(CSF_NFL, UMSARS1_TOT_FDA, method="spearman")))
# group_by(DIAG) %>% summarise(cor=cor(CSF_NFL, UMSARS1_TOT_FDA, method="spearman"))
# group_by(DIAG) %>% summarise(cor=broom::tidy(cor.test(CSF_NFL, UMSARS1_TOT_FDA, method="spearman")))
# group_by(DIAGNIV) %>% summarise(cor=cor(CSF_NFL, UMSARS1_TOT_FDA, method="spearman"))
# group_by(DIAGNIV) %>% summarise(cor=broom::tidy(cor.test(CSF_NFL, UMSARS1_TOT_FDA, method="spearman")))




# ---------

# CSF NFL vs SCOPA AUT Exact Change --------



# SCOPA   
dataCohorteManaged <- readRDS("Source/dataCohorteManaged.rds")

dataCohorteManaged <- dataCohorteManaged %>% group_by(NUM) %>% mutate(TIME_STUDY = ifelse( is.na(TIME_STUDY), 0, TIME_STUDY)) %>%
  mutate(Year= ifelse(TIME_STUDY==0, 0,
                      ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                             ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                    ifelse(TIME_STUDY>=2.5 ,3, NA))))) 

SCOPA <- dataCohorteManaged %>% 
  select(NUM, DATECONSULT, TIME_STUDY, Year) %>% rename("DATE"="DATECONSULT")

ams_scopa_bx_2015_16 <- haven::read_sas("Source/ams_scopa_bx_2015_16.sas7bdat")
names(ams_scopa_bx_2015_16)
ams_scopa_bx_2015_16 <- ams_scopa_bx_2015_16 %>% filter(!is.na(DATE))
ams_scopa_bx_2015_16[is.na(ams_scopa_bx_2015_16)] <- 0

ams_scopa_bx_2015_16 <- ams_scopa_bx_2015_16 %>%
  mutate(SCOPA_DIG=SCOPA1+SCOPA2+SCOPA3+SCOPA4+SCOPA5+SCOPA6+SCOPA7) %>%
  mutate(SCOPA_URI=SCOPA8+SCOPA9+SCOPA10+SCOPA11+SCOPA12+SCOPA13) %>%
  mutate(SCOPA_CDV=SCOPA14+SCOPA15+SCOPA16) %>%
  mutate(SCOPA_THE=SCOPA17+SCOPA18+SCOPA19+SCOPA20) %>%
  mutate(SCOPA_PUP=SCOPA21) %>%
  mutate(SCOPA_SEX=SCOPA22H+SCOPA22F+SCOPA23F+SCOPA23H) %>%
  mutate(SCOPA_TOT=SCOPA_SEX+SCOPA_PUP+SCOPA_THE+SCOPA_CDV+SCOPA_URI+SCOPA_DIG)

ams_scopa_bx_2015_16 <- ams_scopa_bx_2015_16 %>% 
  select(NUM, DATE, SCOPA_TOT, SCOPA_SEX, SCOPA_PUP, SCOPA_THE, SCOPA_CDV, SCOPA_URI, SCOPA_DIG)
SCOPA <- ams_scopa_bx_2015_16 %>% inner_join(SCOPA) 
SCOPA <- SCOPA %>% filter(SCOPA_TOT!=0) 
SCOPA_Baseline_Pats <- SCOPA %>% filter(Year==0) %>% filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM) %>% distinct() %>%
  inner_join(AllMSA_Pop_Baseline_671)





# CSF NFL

biofluidsForPaulo <- readRDS("Source/biofluidsForPaulo.rds")

CSF_NFL <- biofluidsForPaulo %>% rename("NUM"="idGlobal") %>% 
  select(NUM, datePrelevement, nfl_pgml_csf, nfl_ipp_pgml_csf, nfl_pgml_csf_fusion)

CSF_NFL <- Visits_zero %>% inner_join(CSF_NFL) %>%
  mutate(TIME_STUDY=time_length(difftime(datePrelevement, dateVisite0 ), "years")) %>%
  mutate(Year= ifelse(TIME_STUDY<=0.5, 0,
                      ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                             ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                    ifelse(TIME_STUDY>=2.5 ,3, NA))))) 

CSF_NFL <- CSF_NFL %>% gather(original, NFL, nfl_pgml_csf:nfl_pgml_csf_fusion) %>%
  drop_na() %>% arrange(NUM, datePrelevement) %>%
  group_by(NUM, datePrelevement) %>% mutate(NFL=mean(NFL)) %>%
  select(-original) %>% distinct()

# ----------


# Overall MSA ---------

# Year 1

AllMSA_Pop_Baseline_671 %>% inner_join(CSF_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(CSF_NFL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL)
  ) %>%
  mutate(Diff=(NFL-Baseline)) %>% select(NUM, Diff) %>% rename("CSF_NFL"="Diff") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SCOPA_TOT)
  ) %>%
  mutate(Diff=(SCOPA_TOT-Baseline)) %>% select(NUM, Diff) %>% rename("SCOPA_TOT"="Diff")
  ) %>% 
  inner_join(AllMSA_Pop_Baseline_671) %>%
  # summarise(cor=cor(CSF_NFL, SCOPA_TOT, method="spearman"))
  summarise(cor=broom::tidy(cor.test(CSF_NFL, SCOPA_TOT, method="spearman")))
# group_by(DIAG) %>% summarise(cor=cor(CSF_NFL, SCOPA_TOT, method="spearman"))
# group_by(DIAG) %>% summarise(cor=broom::tidy(cor.test(CSF_NFL, SCOPA_TOT, method="spearman")))
# group_by(DIAGNIV) %>% summarise(cor=cor(CSF_NFL, SCOPA_TOT, method="spearman"))
# group_by(DIAGNIV) %>% summarise(cor=broom::tidy(cor.test(CSF_NFL, SCOPA_TOT, method="spearman")))




# ----------
# Early CT Takeda  ---------

# Year 1

EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_NFL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL)
  ) %>%
  mutate(Diff=(NFL-Baseline)) %>% select(NUM, Diff) %>% rename("CSF_NFL"="Diff") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SCOPA_TOT)
  ) %>%
  mutate(Diff=(SCOPA_TOT-Baseline)) %>% select(NUM, Diff) %>% rename("SCOPA_TOT"="Diff")
  ) %>% 
  inner_join(EarlyCT_Pop_Baseline_319) %>%
  # summarise(cor=cor(CSF_NFL, SCOPA_TOT, method="spearman"))
  summarise(cor=broom::tidy(cor.test(CSF_NFL, SCOPA_TOT, method="spearman")))
# group_by(DIAG) %>% summarise(cor=cor(CSF_NFL, SCOPA_TOT, method="spearman"))
# group_by(DIAG) %>% summarise(cor=broom::tidy(cor.test(CSF_NFL, SCOPA_TOT, method="spearman")))
# group_by(DIAGNIV) %>% summarise(cor=cor(CSF_NFL, SCOPA_TOT, method="spearman"))
# group_by(DIAGNIV) %>% summarise(cor=broom::tidy(cor.test(CSF_NFL, SCOPA_TOT, method="spearman")))


# ---------

# CSF NFL vs COMPASS31 Exact Change --------


# COMPASS
dataCohorteManaged <- readRDS("Source/dataCohorteManaged.rds")

dataCohorteManaged <- dataCohorteManaged %>% group_by(NUM) %>% mutate(TIME_STUDY = ifelse( is.na(TIME_STUDY), 0, TIME_STUDY)) %>%
  mutate(Year= ifelse(TIME_STUDY==0, 0,
                      ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                             ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                    ifelse(TIME_STUDY>=2.5 ,3, NA))))) 

compass31_20240412 <- haven::read_sas("Source/compass31_20240412.sas7bdat")

COMPASS <- dataCohorteManaged %>% 
  select(NUM, DATECONSULT, TIME_STUDY, Year) %>%
  inner_join(compass31_20240412, by=c("NUM"="NUM", "DATECONSULT"="DATECONSULT"))

COMPASS <- COMPASS %>% filter(!is.na(COMPASS_TOT) )
COMPASS <- COMPASS[COMPASS$COMPASS_TOT>0,]

COMPASS_Baseline_Pats <- COMPASS %>% filter(Year==0) %>% filter(!is.na(COMPASS_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM) %>% distinct() %>%
  inner_join(AllMSA_Pop_Baseline_671)





# CSF NFL

biofluidsForPaulo <- readRDS("Source/biofluidsForPaulo.rds")

CSF_NFL <- biofluidsForPaulo %>% rename("NUM"="idGlobal") %>% 
  select(NUM, datePrelevement, nfl_pgml_csf, nfl_ipp_pgml_csf, nfl_pgml_csf_fusion)

CSF_NFL <- Visits_zero %>% inner_join(CSF_NFL) %>%
  mutate(TIME_STUDY=time_length(difftime(datePrelevement, dateVisite0 ), "years")) %>%
  mutate(Year= ifelse(TIME_STUDY<=0.5, 0,
                      ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                             ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                    ifelse(TIME_STUDY>=2.5 ,3, NA))))) 

CSF_NFL <- CSF_NFL %>% gather(original, NFL, nfl_pgml_csf:nfl_pgml_csf_fusion) %>%
  drop_na() %>% arrange(NUM, datePrelevement) %>%
  group_by(NUM, datePrelevement) %>% mutate(NFL=mean(NFL)) %>%
  select(-original) %>% distinct()

# ----------


# Overall MSA ---------

# Year 1

AllMSA_Pop_Baseline_671 %>% inner_join(CSF_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(CSF_NFL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL)
  ) %>%
  mutate(Diff=(NFL-Baseline)) %>% select(NUM, Diff) %>% rename("CSF_NFL"="Diff") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, COMPASS_TOT) %>%
  rename("Baseline"="COMPASS_TOT") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, COMPASS_TOT)
  ) %>%
  mutate(Diff=(COMPASS_TOT-Baseline)) %>% select(NUM, Diff) %>% rename("COMPASS_TOT"="Diff")
  ) %>% 
  inner_join(AllMSA_Pop_Baseline_671) %>%
  filter(DIAGNIV=="PROB") %>%
  # summarise(cor=cor(CSF_NFL, COMPASS_TOT, method="spearman"))
   summarise(cor=broom::tidy(cor.test(CSF_NFL, COMPASS_TOT, method="spearman")))
  # group_by(DIAG) %>% summarise(cor=cor(CSF_NFL, COMPASS_TOT, method="spearman"))
  # group_by(DIAG) %>% summarise(cor=broom::tidy(cor.test(CSF_NFL, COMPASS_TOT, method="spearman")))
  # group_by(DIAGNIV) %>% summarise(cor=cor(CSF_NFL, COMPASS_TOT, method="spearman"))
  # group_by(DIAGNIV) %>% summarise(cor=broom::tidy(cor.test(CSF_NFL, COMPASS_TOT, method="spearman")))



# ----------
# Early CT Takeda  ---------

# Year 1

EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_NFL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL)
  ) %>%
  mutate(Diff=(NFL-Baseline)) %>% select(NUM, Diff) %>% rename("CSF_NFL"="Diff") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, COMPASS_TOT) %>%
  rename("Baseline"="COMPASS_TOT") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, COMPASS_TOT)
  ) %>%
  mutate(Diff=(COMPASS_TOT-Baseline)) %>% select(NUM, Diff) %>% rename("COMPASS_TOT"="Diff")
  ) %>% 
  inner_join(EarlyCT_Pop_Baseline_319) %>%
  # summarise(cor=cor(CSF_NFL, COMPASS_TOT, method="spearman"))
  summarise(cor=broom::tidy(cor.test(CSF_NFL, COMPASS_TOT, method="spearman")))
# group_by(DIAG) %>% summarise(cor=cor(CSF_NFL, COMPASS_TOT, method="spearman"))
# group_by(DIAG) %>% summarise(cor=broom::tidy(cor.test(CSF_NFL, COMPASS_TOT, method="spearman")))
# group_by(DIAGNIV) %>% summarise(cor=cor(CSF_NFL, COMPASS_TOT, method="spearman"))
# group_by(DIAGNIV) %>% summarise(cor=broom::tidy(cor.test(CSF_NFL, COMPASS_TOT, method="spearman")))


# ---------

# CSF NFL vs MSA QoL Exact Change --------


# SCHRAG
dataCohorteManaged <- readRDS("Source/dataCohorteManaged.rds")

dataCohorteManaged <- dataCohorteManaged %>% group_by(NUM) %>% mutate(TIME_STUDY = ifelse( is.na(TIME_STUDY), 0, TIME_STUDY)) %>%
  mutate(Year= ifelse(TIME_STUDY==0, 0,
                      ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                             ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                    ifelse(TIME_STUDY>=2.5 ,3, NA))))) 

SCHRAG <- dataCohorteManaged %>% 
  select(NUM, DATECONSULT, TIME_STUDY, Year, SCHRAG_1:SCHRAG_40, SCHRAG_TOT, SCHRAG_MOTOR, SCHRAG_NONMOTOR, SCHRAG_EMOTIONAL, ECHANQV)

SCHRAG$SCHRAG_TOT_v2 <- rowSums(SCHRAG[, 5:44], na.rm = TRUE)

SCHRAG$SCHRAG_TOT_v2 <- rowSums(SCHRAG[, 5:44], na.rm = TRUE)
SCHRAG$SCHRAG_MOTOR_v2 <- rowSums(SCHRAG[, 5:18], na.rm = TRUE)
SCHRAG$SCHRAG_NONMOTOR_v2 <- rowSums(SCHRAG[, 19:30], na.rm = TRUE)
SCHRAG$SCHRAG_EMOTIONAL_v2 <- rowSums(SCHRAG[, 31:44], na.rm = TRUE)

SCHRAG <- SCHRAG %>% select(-c(SCHRAG_TOT, SCHRAG_MOTOR, SCHRAG_NONMOTOR, SCHRAG_EMOTIONAL))
SCHRAG <- SCHRAG %>% filter(SCHRAG_TOT_v2!=0)
SCHRAG$all_na <- rowSums(is.na(SCHRAG[, 5:44])) == (44 - 5 + 1)
SCHRAG$number_na <- rowSums(is.na(SCHRAG[, 5:44])) 
SCHRAG <- SCHRAG %>% filter(!(all_na) )

SCHRAG_Baseline_Pats <- SCHRAG %>% filter(Year==0) %>% filter(!is.na(SCHRAG_TOT_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM) %>% distinct() %>%
  inner_join(AllMSA_Pop_Baseline_671)





# CSF NFL

biofluidsForPaulo <- readRDS("Source/biofluidsForPaulo.rds")

CSF_NFL <- biofluidsForPaulo %>% rename("NUM"="idGlobal") %>% 
  select(NUM, datePrelevement, nfl_pgml_csf, nfl_ipp_pgml_csf, nfl_pgml_csf_fusion)

CSF_NFL <- Visits_zero %>% inner_join(CSF_NFL) %>%
  mutate(TIME_STUDY=time_length(difftime(datePrelevement, dateVisite0 ), "years")) %>%
  mutate(Year= ifelse(TIME_STUDY<=0.5, 0,
                      ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                             ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                    ifelse(TIME_STUDY>=2.5 ,3, NA))))) 

CSF_NFL <- CSF_NFL %>% gather(original, NFL, nfl_pgml_csf:nfl_pgml_csf_fusion) %>%
  drop_na() %>% arrange(NUM, datePrelevement) %>%
  group_by(NUM, datePrelevement) %>% mutate(NFL=mean(NFL)) %>%
  select(-original) %>% distinct()

# ----------


# Overall MSA ---------

# Year 1

AllMSA_Pop_Baseline_671 %>% inner_join(CSF_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(CSF_NFL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL)
  ) %>%
  mutate(Diff=(NFL-Baseline)) %>% select(NUM, Diff) %>% rename("CSF_NFL"="Diff") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SCHRAG_TOT_v2) %>%
  rename("Baseline"="SCHRAG_TOT_v2") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SCHRAG_TOT_v2)
  ) %>%
  mutate(Diff=(SCHRAG_TOT_v2-Baseline)) %>% select(NUM, Diff) %>% rename("SCHRAG_TOT_v2"="Diff")
  ) %>% 
  inner_join(AllMSA_Pop_Baseline_671) %>%
  filter(DIAGNIV=="PROB") %>%
   # summarise(cor=cor(CSF_NFL, SCHRAG_TOT_v2, method="spearman"))
   summarise(cor=broom::tidy(cor.test(CSF_NFL, SCHRAG_TOT_v2, method="spearman")))
  # group_by(DIAG) %>% summarise(cor=cor(CSF_NFL, SCHRAG_TOT_v2, method="spearman"))
   #group_by(DIAG) %>% summarise(cor=broom::tidy(cor.test(CSF_NFL, SCHRAG_TOT_v2, method="spearman")))
  # group_by(DIAGNIV) %>% summarise(cor=cor(CSF_NFL, SCHRAG_TOT_v2, method="spearman"))
   #group_by(DIAGNIV) %>% summarise(cor=broom::tidy(cor.test(CSF_NFL, SCHRAG_TOT_v2, method="spearman")))


# ----------
# Early CT Takeda  ---------

# Year 1

EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_NFL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL)
  ) %>%
  mutate(Diff=(NFL-Baseline)) %>% select(NUM, Diff) %>% rename("CSF_NFL"="Diff") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SCHRAG_TOT_v2) %>%
  rename("Baseline"="SCHRAG_TOT_v2") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SCHRAG_TOT_v2)
  ) %>%
  mutate(Diff=(SCHRAG_TOT_v2-Baseline)) %>% select(NUM, Diff) %>% rename("SCHRAG_TOT_v2"="Diff")
  ) %>% 
  inner_join(EarlyCT_Pop_Baseline_319) %>%
  # summarise(cor=cor(CSF_NFL, SCHRAG_TOT_v2, method="spearman"))
  summarise(cor=broom::tidy(cor.test(CSF_NFL, SCHRAG_TOT_v2, method="spearman")))
# group_by(DIAG) %>% summarise(cor=cor(CSF_NFL, SCHRAG_TOT_v2, method="spearman"))
#group_by(DIAG) %>% summarise(cor=broom::tidy(cor.test(CSF_NFL, SCHRAG_TOT_v2, method="spearman")))
# group_by(DIAGNIV) %>% summarise(cor=cor(CSF_NFL, SCHRAG_TOT_v2, method="spearman"))
#group_by(DIAGNIV) %>% summarise(cor=broom::tidy(cor.test(CSF_NFL, SCHRAG_TOT_v2, method="spearman")))



# ---------

# Correlations Between Clinical and Fluid Biomarkers -----------
# ---------
# All data dates groups  --------
dataCohorteManaged <- readRDS("Source/dataCohorteManaged.rds")

dataCohorteManaged <- dataCohorteManaged %>% mutate(Year= ifelse(TIME_STUDY<=0.5, 0,
                    ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                           ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                  ifelse(TIME_STUDY>=2.5 ,3, NA))))) 

Visits_zero <- dataCohorteManaged  %>% select(NUM, dateVisite0) %>% distinct()

AllMSA_Pop_Baseline_671 <- fread("Source/AllMSA_Pop_Baseline_671.txt")
EarlyCT_Pop_Baseline_319 <- fread("Source/EarlyCT_Pop_Baseline_319.txt")

# --------
# Plasma NFL vs Modified UMSARS 1 Exact Change --------


# UMSARS 1 

UMSARS1 <- dataCohorteManaged %>% 
  select(NUM, DATECONSULT, TIME_STUDY, Year, UMSARS1_1:UMSARS1_TOT)
UMSARS1$UMSARS1_TOT_v2 <- rowSums(UMSARS1[, 5:16], na.rm = TRUE)
UMSARS1$missing_na <- rowSums(is.na(UMSARS1[, 5:16]))
UMSARS1 <-  UMSARS1 %>%  select(NUM, TIME_STUDY, Year, UMSARS1_TOT, UMSARS1_11)
UMSARS1 <- UMSARS1 %>% mutate(UMSARS1_TOT_FDA=UMSARS1_TOT-UMSARS1_11)



# Plasma NFL 

biofluidsForPaulo <- readRDS("Source/biofluidsForPaulo.rds")

Plasma_NFL <- biofluidsForPaulo %>% rename("NUM"="idGlobal") %>% 
  select(NUM, datePrelevement, nfl_pgml_plas, nfl_ipp_pgml_plas)

Plasma_NFL <- Visits_zero %>% inner_join(Plasma_NFL) %>%
  mutate(TIME_STUDY=time_length(difftime(datePrelevement, dateVisite0 ), "years")) %>%
  mutate(Year= ifelse(TIME_STUDY<=0.5, 0,
                      ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                             ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                    ifelse(TIME_STUDY>=2.5 ,3, NA))))) 

Plasma_NFL <- Plasma_NFL %>% gather(original, NFL, nfl_pgml_plas:nfl_ipp_pgml_plas) %>%
  drop_na() %>% arrange(NUM, datePrelevement) %>%
  group_by(NUM, datePrelevement) %>% mutate(NFL=mean(NFL)) %>%
  select(-original) %>% distinct()


# ----------


# Overall MSA ---------

# Year 1

AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_NFL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL)
  ) %>%
  mutate(Diff=(NFL-Baseline)) %>% select(NUM, Diff) %>% rename("Plasma_NFL"="Diff") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, UMSARS1_TOT_FDA) %>%
  rename("Baseline"="UMSARS1_TOT_FDA") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(UMSARS1) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, UMSARS1_TOT_FDA)
  ) %>%
  mutate(Diff=(UMSARS1_TOT_FDA-Baseline)) %>% select(NUM, Diff) %>% rename("UMSARS1_TOT_FDA"="Diff")
  ) %>% 
  inner_join(AllMSA_Pop_Baseline_671) %>%
  # summarise(cor=cor(Plasma_NFL, UMSARS1_TOT_FDA, method="spearman"))
  # summarise(cor=broom::tidy(cor.test(Plasma_NFL, UMSARS1_TOT_FDA, method="spearman")))
  # group_by(DIAG) %>% summarise(cor=cor(Plasma_NFL, UMSARS1_TOT_FDA, method="spearman"))
   #group_by(DIAG) %>% summarise(cor=broom::tidy(cor.test(Plasma_NFL, UMSARS1_TOT_FDA, method="spearman")))
   #group_by(DIAGNIV) %>% summarise(cor=cor(Plasma_NFL, UMSARS1_TOT_FDA, method="spearman"))
  group_by(DIAGNIV) %>% summarise(cor=broom::tidy(cor.test(Plasma_NFL, UMSARS1_TOT_FDA, method="spearman")))



# ----------
# Early CT Takeda  ---------

# Year 1

EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_NFL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL)
  ) %>%
  mutate(Diff=(NFL-Baseline)) %>% select(NUM, Diff) %>% rename("Plasma_NFL"="Diff") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, UMSARS1_TOT_FDA) %>%
  rename("Baseline"="UMSARS1_TOT_FDA") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(UMSARS1) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, UMSARS1_TOT_FDA)
  ) %>%
  mutate(Diff=(UMSARS1_TOT_FDA-Baseline)) %>% select(NUM, Diff) %>% rename("UMSARS1_TOT_FDA"="Diff")
  ) %>% 
  inner_join(EarlyCT_Pop_Baseline_319) %>%
  # summarise(cor=cor(Plasma_NFL, UMSARS1_TOT_FDA, method="spearman"))
  # summarise(cor=broom::tidy(cor.test(Plasma_NFL, UMSARS1_TOT_FDA, method="spearman")))
  # group_by(DIAG) %>% summarise(cor=cor(Plasma_NFL, UMSARS1_TOT_FDA, method="spearman"))
  #group_by(DIAG) %>% summarise(cor=broom::tidy(cor.test(Plasma_NFL, UMSARS1_TOT_FDA, method="spearman")))
  #group_by(DIAGNIV) %>% summarise(cor=cor(Plasma_NFL, UMSARS1_TOT_FDA, method="spearman"))
  group_by(DIAGNIV) %>% summarise(cor=broom::tidy(cor.test(Plasma_NFL, UMSARS1_TOT_FDA, method="spearman")))


# ---------

# Plasma NFL vs SCOPA AUT Exact Change --------



# SCOPA   
dataCohorteManaged <- readRDS("Source/dataCohorteManaged.rds")

dataCohorteManaged <- dataCohorteManaged %>% group_by(NUM) %>% mutate(TIME_STUDY = ifelse( is.na(TIME_STUDY), 0, TIME_STUDY)) %>%
  mutate(Year= ifelse(TIME_STUDY==0, 0,
                      ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                             ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                    ifelse(TIME_STUDY>=2.5 ,3, NA))))) 

SCOPA <- dataCohorteManaged %>% 
  select(NUM, DATECONSULT, TIME_STUDY, Year) %>% rename("DATE"="DATECONSULT")

ams_scopa_bx_2015_16 <- haven::read_sas("Source/ams_scopa_bx_2015_16.sas7bdat")
names(ams_scopa_bx_2015_16)
ams_scopa_bx_2015_16 <- ams_scopa_bx_2015_16 %>% filter(!is.na(DATE))
ams_scopa_bx_2015_16[is.na(ams_scopa_bx_2015_16)] <- 0

ams_scopa_bx_2015_16 <- ams_scopa_bx_2015_16 %>%
  mutate(SCOPA_DIG=SCOPA1+SCOPA2+SCOPA3+SCOPA4+SCOPA5+SCOPA6+SCOPA7) %>%
  mutate(SCOPA_URI=SCOPA8+SCOPA9+SCOPA10+SCOPA11+SCOPA12+SCOPA13) %>%
  mutate(SCOPA_CDV=SCOPA14+SCOPA15+SCOPA16) %>%
  mutate(SCOPA_THE=SCOPA17+SCOPA18+SCOPA19+SCOPA20) %>%
  mutate(SCOPA_PUP=SCOPA21) %>%
  mutate(SCOPA_SEX=SCOPA22H+SCOPA22F+SCOPA23F+SCOPA23H) %>%
  mutate(SCOPA_TOT=SCOPA_SEX+SCOPA_PUP+SCOPA_THE+SCOPA_CDV+SCOPA_URI+SCOPA_DIG)

ams_scopa_bx_2015_16 <- ams_scopa_bx_2015_16 %>% 
  select(NUM, DATE, SCOPA_TOT, SCOPA_SEX, SCOPA_PUP, SCOPA_THE, SCOPA_CDV, SCOPA_URI, SCOPA_DIG)
SCOPA <- ams_scopa_bx_2015_16 %>% inner_join(SCOPA) 
SCOPA <- SCOPA %>% filter(SCOPA_TOT!=0) 
SCOPA_Baseline_Pats <- SCOPA %>% filter(Year==0) %>% filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM) %>% distinct() %>%
  inner_join(AllMSA_Pop_Baseline_671)





# Plasma NFL

biofluidsForPaulo <- readRDS("Source/biofluidsForPaulo.rds")

Plasma_NFL <- biofluidsForPaulo %>% rename("NUM"="idGlobal") %>% 
  select(NUM, datePrelevement, nfl_pgml_Plasma, nfl_ipp_pgml_Plasma, nfl_pgml_Plasma_fusion)

Plasma_NFL <- Visits_zero %>% inner_join(Plasma_NFL) %>%
  mutate(TIME_STUDY=time_length(difftime(datePrelevement, dateVisite0 ), "years")) %>%
  mutate(Year= ifelse(TIME_STUDY<=0.5, 0,
                      ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                             ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                    ifelse(TIME_STUDY>=2.5 ,3, NA))))) 

Plasma_NFL <- Plasma_NFL %>% gather(original, NFL, nfl_pgml_Plasma:nfl_pgml_Plasma_fusion) %>%
  drop_na() %>% arrange(NUM, datePrelevement) %>%
  group_by(NUM, datePrelevement) %>% mutate(NFL=mean(NFL)) %>%
  select(-original) %>% distinct()

# ----------


# Overall MSA ---------

# Year 1

AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_NFL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL)
  ) %>%
  mutate(Diff=(NFL-Baseline)) %>% select(NUM, Diff) %>% rename("Plasma_NFL"="Diff") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SCOPA_TOT)
  ) %>%
  mutate(Diff=(SCOPA_TOT-Baseline)) %>% select(NUM, Diff) %>% rename("SCOPA_TOT"="Diff")
  ) %>% 
  inner_join(AllMSA_Pop_Baseline_671) %>%
   # summarise(cor=cor(Plasma_NFL, SCOPA_TOT, method="spearman"))
  #summarise(cor=broom::tidy(cor.test(Plasma_NFL, SCOPA_TOT, method="spearman")))
  # group_by(DIAG) %>% summarise(cor=cor(Plasma_NFL, SCOPA_TOT, method="spearman"))
  # group_by(DIAG) %>% summarise(cor=broom::tidy(cor.test(Plasma_NFL, SCOPA_TOT, method="spearman")))
  # group_by(DIAGNIV) %>% summarise(cor=cor(Plasma_NFL, SCOPA_TOT, method="spearman"))
  # group_by(DIAGNIV) %>% summarise(cor=broom::tidy(cor.test(Plasma_NFL, SCOPA_TOT, method="spearman")))


# ----------
# Early CT Takeda  ---------

# Year 1

EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_NFL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL)
  ) %>%
  mutate(Diff=(NFL-Baseline)) %>% select(NUM, Diff) %>% rename("Plasma_NFL"="Diff") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SCOPA_TOT)
  ) %>%
  mutate(Diff=(SCOPA_TOT-Baseline)) %>% select(NUM, Diff) %>% rename("SCOPA_TOT"="Diff")
  ) %>% 
  inner_join(EarlyCT_Pop_Baseline_319) %>%
  # summarise(cor=cor(Plasma_NFL, SCOPA_TOT, method="spearman"))
  #summarise(cor=broom::tidy(cor.test(Plasma_NFL, SCOPA_TOT, method="spearman")))
  # group_by(DIAG) %>% summarise(cor=cor(Plasma_NFL, SCOPA_TOT, method="spearman"))
  # group_by(DIAG) %>% summarise(cor=broom::tidy(cor.test(Plasma_NFL, SCOPA_TOT, method="spearman")))
  # group_by(DIAGNIV) %>% summarise(cor=cor(Plasma_NFL, SCOPA_TOT, method="spearman"))
  # group_by(DIAGNIV) %>% summarise(cor=broom::tidy(cor.test(Plasma_NFL, SCOPA_TOT, method="spearman")))

# ---------

# Plasma NFL vs COMPASS31 Exact Change --------


# COMPASS
dataCohorteManaged <- readRDS("Source/dataCohorteManaged.rds")

dataCohorteManaged <- dataCohorteManaged %>% group_by(NUM) %>% mutate(TIME_STUDY = ifelse( is.na(TIME_STUDY), 0, TIME_STUDY)) %>%
  mutate(Year= ifelse(TIME_STUDY==0, 0,
                      ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                             ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                    ifelse(TIME_STUDY>=2.5 ,3, NA))))) 

compass31_20240412 <- haven::read_sas("Source/compass31_20240412.sas7bdat")

COMPASS <- dataCohorteManaged %>% 
  select(NUM, DATECONSULT, TIME_STUDY, Year) %>%
  inner_join(compass31_20240412, by=c("NUM"="NUM", "DATECONSULT"="DATECONSULT"))

COMPASS <- COMPASS %>% filter(!is.na(COMPASS_TOT) )
COMPASS <- COMPASS[COMPASS$COMPASS_TOT>0,]

COMPASS_Baseline_Pats <- COMPASS %>% filter(Year==0) %>% filter(!is.na(COMPASS_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM) %>% distinct() %>%
  inner_join(AllMSA_Pop_Baseline_671)





# Plasma NFL

biofluidsForPaulo <- readRDS("Source/biofluidsForPaulo.rds")

Plasma_NFL <- biofluidsForPaulo %>% rename("NUM"="idGlobal") %>% 
  select(NUM, datePrelevement, nfl_pgml_Plasma, nfl_ipp_pgml_Plasma, nfl_pgml_Plasma_fusion)

Plasma_NFL <- Visits_zero %>% inner_join(Plasma_NFL) %>%
  mutate(TIME_STUDY=time_length(difftime(datePrelevement, dateVisite0 ), "years")) %>%
  mutate(Year= ifelse(TIME_STUDY<=0.5, 0,
                      ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                             ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                    ifelse(TIME_STUDY>=2.5 ,3, NA))))) 

Plasma_NFL <- Plasma_NFL %>% gather(original, NFL, nfl_pgml_Plasma:nfl_pgml_Plasma_fusion) %>%
  drop_na() %>% arrange(NUM, datePrelevement) %>%
  group_by(NUM, datePrelevement) %>% mutate(NFL=mean(NFL)) %>%
  select(-original) %>% distinct()

# ----------


# Overall MSA ---------

# Year 1

AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_NFL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL)
  ) %>%
  mutate(Diff=(NFL-Baseline)) %>% select(NUM, Diff) %>% rename("Plasma_NFL"="Diff") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, COMPASS_TOT) %>%
  rename("Baseline"="COMPASS_TOT") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, COMPASS_TOT)
  ) %>%
  mutate(Diff=(COMPASS_TOT-Baseline)) %>% select(NUM, Diff) %>% rename("COMPASS_TOT"="Diff")
  ) %>% 
  inner_join(AllMSA_Pop_Baseline_671) %>%
  # summarise(cor=cor(Plasma_NFL, COMPASS_TOT, method="spearman"))
  summarise(cor=broom::tidy(cor.test(Plasma_NFL, COMPASS_TOT, method="spearman")))
  # group_by(DIAG) %>% summarise(cor=cor(Plasma_NFL, COMPASS_TOT, method="spearman"))
  # group_by(DIAG) %>% summarise(cor=broom::tidy(cor.test(Plasma_NFL, COMPASS_TOT, method="spearman")))
  # group_by(DIAGNIV) %>% summarise(cor=cor(Plasma_NFL, COMPASS_TOT, method="spearman"))
  # group_by(DIAGNIV) %>% summarise(cor=broom::tidy(cor.test(Plasma_NFL, COMPASS_TOT, method="spearman")))


# ----------
# Early CT Takeda  ---------

# Year 1

EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_NFL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL)
  ) %>%
  mutate(Diff=(NFL-Baseline)) %>% select(NUM, Diff) %>% rename("Plasma_NFL"="Diff") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, COMPASS_TOT) %>%
  rename("Baseline"="COMPASS_TOT") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, COMPASS_TOT)
  ) %>%
  mutate(Diff=(COMPASS_TOT-Baseline)) %>% select(NUM, Diff) %>% rename("COMPASS_TOT"="Diff")
  ) %>% 
  inner_join(EarlyCT_Pop_Baseline_319) %>%
  # summarise(cor=cor(Plasma_NFL, COMPASS_TOT, method="spearman"))
  summarise(cor=broom::tidy(cor.test(Plasma_NFL, COMPASS_TOT, method="spearman")))
# group_by(DIAG) %>% summarise(cor=cor(Plasma_NFL, COMPASS_TOT, method="spearman"))
# group_by(DIAG) %>% summarise(cor=broom::tidy(cor.test(Plasma_NFL, COMPASS_TOT, method="spearman")))
# group_by(DIAGNIV) %>% summarise(cor=cor(Plasma_NFL, COMPASS_TOT, method="spearman"))
# group_by(DIAGNIV) %>% summarise(cor=broom::tidy(cor.test(Plasma_NFL, COMPASS_TOT, method="spearman")))

# ---------

# Plasma NFL vs MSA QoL Exact Change --------


# SCHRAG
dataCohorteManaged <- readRDS("Source/dataCohorteManaged.rds")

dataCohorteManaged <- dataCohorteManaged %>% group_by(NUM) %>% mutate(TIME_STUDY = ifelse( is.na(TIME_STUDY), 0, TIME_STUDY)) %>%
  mutate(Year= ifelse(TIME_STUDY==0, 0,
                      ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                             ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                    ifelse(TIME_STUDY>=2.5 ,3, NA))))) 

SCHRAG <- dataCohorteManaged %>% 
  select(NUM, DATECONSULT, TIME_STUDY, Year, SCHRAG_1:SCHRAG_40, SCHRAG_TOT, SCHRAG_MOTOR, SCHRAG_NONMOTOR, SCHRAG_EMOTIONAL, ECHANQV)

SCHRAG$SCHRAG_TOT_v2 <- rowSums(SCHRAG[, 5:44], na.rm = TRUE)

SCHRAG$SCHRAG_TOT_v2 <- rowSums(SCHRAG[, 5:44], na.rm = TRUE)
SCHRAG$SCHRAG_MOTOR_v2 <- rowSums(SCHRAG[, 5:18], na.rm = TRUE)
SCHRAG$SCHRAG_NONMOTOR_v2 <- rowSums(SCHRAG[, 19:30], na.rm = TRUE)
SCHRAG$SCHRAG_EMOTIONAL_v2 <- rowSums(SCHRAG[, 31:44], na.rm = TRUE)

SCHRAG <- SCHRAG %>% select(-c(SCHRAG_TOT, SCHRAG_MOTOR, SCHRAG_NONMOTOR, SCHRAG_EMOTIONAL))
SCHRAG <- SCHRAG %>% filter(SCHRAG_TOT_v2!=0)
SCHRAG$all_na <- rowSums(is.na(SCHRAG[, 5:44])) == (44 - 5 + 1)
SCHRAG$number_na <- rowSums(is.na(SCHRAG[, 5:44])) 
SCHRAG <- SCHRAG %>% filter(!(all_na) )

SCHRAG_Baseline_Pats <- SCHRAG %>% filter(Year==0) %>% filter(!is.na(SCHRAG_TOT_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM) %>% distinct() %>%
  inner_join(AllMSA_Pop_Baseline_671)





# Plasma NFL

biofluidsForPaulo <- readRDS("Source/biofluidsForPaulo.rds")

Plasma_NFL <- biofluidsForPaulo %>% rename("NUM"="idGlobal") %>% 
  select(NUM, datePrelevement, nfl_pgml_Plasma, nfl_ipp_pgml_Plasma, nfl_pgml_Plasma_fusion)

Plasma_NFL <- Visits_zero %>% inner_join(Plasma_NFL) %>%
  mutate(TIME_STUDY=time_length(difftime(datePrelevement, dateVisite0 ), "years")) %>%
  mutate(Year= ifelse(TIME_STUDY<=0.5, 0,
                      ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                             ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                    ifelse(TIME_STUDY>=2.5 ,3, NA))))) 

Plasma_NFL <- Plasma_NFL %>% gather(original, NFL, nfl_pgml_Plasma:nfl_pgml_Plasma_fusion) %>%
  drop_na() %>% arrange(NUM, datePrelevement) %>%
  group_by(NUM, datePrelevement) %>% mutate(NFL=mean(NFL)) %>%
  select(-original) %>% distinct()

# ----------


# Overall MSA ---------

# Year 1

AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Plasma_NFL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL)
  ) %>%
  mutate(Diff=(NFL-Baseline)) %>% select(NUM, Diff) %>% rename("Plasma_NFL"="Diff") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SCHRAG_TOT_v2) %>%
  rename("Baseline"="SCHRAG_TOT_v2") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SCHRAG_TOT_v2)
  ) %>%
  mutate(Diff=(SCHRAG_TOT_v2-Baseline)) %>% select(NUM, Diff) %>% rename("SCHRAG_TOT_v2"="Diff")
  ) %>% 
  inner_join(AllMSA_Pop_Baseline_671) %>%
  # summarise(cor=cor(Plasma_NFL, SCHRAG_TOT_v2, method="spearman"))
  summarise(cor=broom::tidy(cor.test(Plasma_NFL, SCHRAG_TOT_v2, method="spearman")))
  # group_by(DIAG) %>% summarise(cor=cor(Plasma_NFL, SCHRAG_TOT_v2, method="spearman"))
  # group_by(DIAG) %>% summarise(cor=broom::tidy(cor.test(Plasma_NFL, SCHRAG_TOT_v2, method="spearman")))
  # group_by(DIAGNIV) %>% summarise(cor=cor(Plasma_NFL, SCHRAG_TOT_v2, method="spearman"))
  # group_by(DIAGNIV) %>% summarise(cor=broom::tidy(cor.test(Plasma_NFL, SCHRAG_TOT_v2, method="spearman")))



  
# ----------
# Early CT Takeda  ---------

# Year 1

EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_NFL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL) %>%
  rename("Baseline"="NFL") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Plasma_NFL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, NFL)
  ) %>%
  mutate(Diff=(NFL-Baseline)) %>% select(NUM, Diff) %>% rename("Plasma_NFL"="Diff") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SCHRAG_TOT_v2) %>%
  rename("Baseline"="SCHRAG_TOT_v2") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SCHRAG_TOT_v2)
  ) %>%
  mutate(Diff=(SCHRAG_TOT_v2-Baseline)) %>% select(NUM, Diff) %>% rename("SCHRAG_TOT_v2"="Diff")
  ) %>% 
  inner_join(EarlyCT_Pop_Baseline_319) %>%
  # summarise(cor=cor(Plasma_NFL, SCHRAG_TOT_v2, method="spearman"))
  summarise(cor=broom::tidy(cor.test(Plasma_NFL, SCHRAG_TOT_v2, method="spearman")))
# group_by(DIAG) %>% summarise(cor=cor(Plasma_NFL, SCHRAG_TOT_v2, method="spearman"))
# group_by(DIAG) %>% summarise(cor=broom::tidy(cor.test(Plasma_NFL, SCHRAG_TOT_v2, method="spearman")))
# group_by(DIAGNIV) %>% summarise(cor=cor(Plasma_NFL, SCHRAG_TOT_v2, method="spearman"))
# group_by(DIAGNIV) %>% summarise(cor=broom::tidy(cor.test(Plasma_NFL, SCHRAG_TOT_v2, method="spearman")))


# ---------

# Correlations Between Clinical and Fluid Biomarkers -----------
# ---------
# All data dates groups  --------
dataCohorteManaged <- readRDS("Source/dataCohorteManaged.rds")

Visits_zero <- dataCohorteManaged  %>% select(NUM, dateVisite0) %>% distinct()

AllMSA_Pop_Baseline_671 <- fread("Source/AllMSA_Pop_Baseline_671.txt")
EarlyCT_Pop_Baseline_319 <- fread("Source/EarlyCT_Pop_Baseline_319.txt")

# --------
# CSF SYN vs Modified UMSARS 1 Exact Change --------


# UMSARS 1 
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
UMSARS1 <-  UMSARS1 %>%  select(NUM, TIME_STUDY, Year, UMSARS1_TOT, UMSARS1_11)
UMSARS1 <- UMSARS1 %>% mutate(UMSARS1_TOT_FDA=UMSARS1_TOT-UMSARS1_11)



# CSF SYN 

biofluidsForPaulo <- readRDS("Source/biofluidsForPaulo.rds")
names(biofluidsForPaulo)
CSF_SYN <- biofluidsForPaulo %>% rename("NUM"="idGlobal") %>% 
  select(NUM, datePrelevement, total_alpha_syn_g_pgml_csf, total_alpha_syn_q1_pgml_csf, total_alpha_syn_q2_pgml_csf, 
         total_alpha_syn_ipp_pgml_csf, totalAlphaSyn_Fusion_pgml_csf)

CSF_SYN <- Visits_zero %>% inner_join(CSF_SYN) %>%
  mutate(TIME_STUDY=time_length(difftime(datePrelevement, dateVisite0 ), "years")) %>%
  mutate(Year= ifelse(TIME_STUDY<=0.5, 0,
                      ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                             ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                    ifelse(TIME_STUDY>=2.5 ,3, NA))))) 

CSF_SYN <- CSF_SYN %>% gather(original, SYN, total_alpha_syn_g_pgml_csf:totalAlphaSyn_Fusion_pgml_csf) %>%
  drop_na() %>% arrange(NUM, datePrelevement) %>%
  group_by(NUM, datePrelevement) %>% mutate(SYN=mean(SYN)) %>%
  select(-original) %>% distinct()


# ----------


# Overall MSA ---------

# Year 1

AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN)
  ) %>%
  mutate(Diff=(SYN-Baseline)) %>% select(NUM, Diff) %>% rename("CSF_SYN"="Diff") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, UMSARS1_TOT_FDA) %>%
  rename("Baseline"="UMSARS1_TOT_FDA") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(UMSARS1) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, UMSARS1_TOT_FDA)
  ) %>%
  mutate(Diff=(UMSARS1_TOT_FDA-Baseline)) %>% select(NUM, Diff) %>% rename("UMSARS1_TOT_FDA"="Diff")
  ) %>% 
  inner_join(AllMSA_Pop_Baseline_671) %>%
  filter(DIAGNIV=="PROB") %>%
  # summarise(cor=cor(CSF_SYN, UMSARS1_TOT_FDA, method="spearman"))
   summarise(cor=broom::tidy(cor.test(CSF_SYN, UMSARS1_TOT_FDA, method="spearman")))
  # group_by(DIAG) %>% summarise(cor=cor(CSF_SYN, UMSARS1_TOT_FDA, method="spearman"))
   #group_by(DIAG) %>% summarise(cor=broom::tidy(cor.test(CSF_SYN, UMSARS1_TOT_FDA, method="spearman")))
  # group_by(DIAGNIV) %>% summarise(cor=cor(CSF_SYN, UMSARS1_TOT_FDA, method="spearman"))
  # group_by(DIAGNIV) %>% summarise(cor=broom::tidy(cor.test(CSF_SYN, UMSARS1_TOT_FDA, method="spearman")))


# ----------
# Early CT Takeda  ---------

# Year 1

EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN)
  ) %>%
  mutate(Diff=(SYN-Baseline)) %>% select(NUM, Diff) %>% rename("CSF_SYN"="Diff") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, UMSARS1_TOT_FDA) %>%
  rename("Baseline"="UMSARS1_TOT_FDA") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(UMSARS1) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, UMSARS1_TOT_FDA)
  ) %>%
  mutate(Diff=(UMSARS1_TOT_FDA-Baseline)) %>% select(NUM, Diff) %>% rename("UMSARS1_TOT_FDA"="Diff")
  ) %>% 
  inner_join(EarlyCT_Pop_Baseline_319) %>%
  # summarise(cor=cor(CSF_SYN, UMSARS1_TOT_FDA, method="spearman"))
  summarise(cor=broom::tidy(cor.test(CSF_SYN, UMSARS1_TOT_FDA, method="spearman")))
# group_by(DIAG) %>% summarise(cor=cor(CSF_SYN, UMSARS1_TOT_FDA, method="spearman"))
#group_by(DIAG) %>% summarise(cor=broom::tidy(cor.test(CSF_SYN, UMSARS1_TOT_FDA, method="spearman")))
# group_by(DIAGNIV) %>% summarise(cor=cor(CSF_SYN, UMSARS1_TOT_FDA, method="spearman"))
# group_by(DIAGNIV) %>% summarise(cor=broom::tidy(cor.test(CSF_SYN, UMSARS1_TOT_FDA, method="spearman")))


# ---------

# CSF SYN vs SCOPA AUT Exact Change --------



# SCOPA   
dataCohorteManaged <- readRDS("Source/dataCohorteManaged.rds")

dataCohorteManaged <- dataCohorteManaged %>% group_by(NUM) %>% mutate(TIME_STUDY = ifelse( is.na(TIME_STUDY), 0, TIME_STUDY)) %>%
  mutate(Year= ifelse(TIME_STUDY==0, 0,
                      ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                             ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                    ifelse(TIME_STUDY>=2.5 ,3, NA))))) 

SCOPA <- dataCohorteManaged %>% 
  select(NUM, DATECONSULT, TIME_STUDY, Year) %>% rename("DATE"="DATECONSULT")

ams_scopa_bx_2015_16 <- haven::read_sas("Source/ams_scopa_bx_2015_16.sas7bdat")
names(ams_scopa_bx_2015_16)
ams_scopa_bx_2015_16 <- ams_scopa_bx_2015_16 %>% filter(!is.na(DATE))
ams_scopa_bx_2015_16[is.na(ams_scopa_bx_2015_16)] <- 0

ams_scopa_bx_2015_16 <- ams_scopa_bx_2015_16 %>%
  mutate(SCOPA_DIG=SCOPA1+SCOPA2+SCOPA3+SCOPA4+SCOPA5+SCOPA6+SCOPA7) %>%
  mutate(SCOPA_URI=SCOPA8+SCOPA9+SCOPA10+SCOPA11+SCOPA12+SCOPA13) %>%
  mutate(SCOPA_CDV=SCOPA14+SCOPA15+SCOPA16) %>%
  mutate(SCOPA_THE=SCOPA17+SCOPA18+SCOPA19+SCOPA20) %>%
  mutate(SCOPA_PUP=SCOPA21) %>%
  mutate(SCOPA_SEX=SCOPA22H+SCOPA22F+SCOPA23F+SCOPA23H) %>%
  mutate(SCOPA_TOT=SCOPA_SEX+SCOPA_PUP+SCOPA_THE+SCOPA_CDV+SCOPA_URI+SCOPA_DIG)

ams_scopa_bx_2015_16 <- ams_scopa_bx_2015_16 %>% 
  select(NUM, DATE, SCOPA_TOT, SCOPA_SEX, SCOPA_PUP, SCOPA_THE, SCOPA_CDV, SCOPA_URI, SCOPA_DIG)
SCOPA <- ams_scopa_bx_2015_16 %>% inner_join(SCOPA) 
SCOPA <- SCOPA %>% filter(SCOPA_TOT!=0) 
SCOPA_Baseline_Pats <- SCOPA %>% filter(Year==0) %>% filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM) %>% distinct() %>%
  inner_join(AllMSA_Pop_Baseline_671)





# CSF SYN

biofluidsForPaulo <- readRDS("Source/biofluidsForPaulo.rds")

CSF_SYN <- biofluidsForPaulo %>% rename("NUM"="idGlobal") %>% 
  select(NUM, datePrelevement, SYN_pgml_CSF, SYN_ipp_pgml_CSF, SYN_pgml_CSF_fusion)

CSF_SYN <- Visits_zero %>% inner_join(CSF_SYN) %>%
  mutate(TIME_STUDY=time_length(difftime(datePrelevement, dateVisite0 ), "years")) %>%
  mutate(Year= ifelse(TIME_STUDY<=0.5, 0,
                      ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                             ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                    ifelse(TIME_STUDY>=2.5 ,3, NA))))) 

CSF_SYN <- CSF_SYN %>% gather(original, SYN, SYN_pgml_CSF:SYN_pgml_CSF_fusion) %>%
  drop_na() %>% arrange(NUM, datePrelevement) %>%
  group_by(NUM, datePrelevement) %>% mutate(SYN=mean(SYN)) %>%
  select(-original) %>% distinct()

# ----------


# Overall MSA ---------

# Year 1

AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN)
  ) %>%
  mutate(Diff=(SYN-Baseline)) %>% select(NUM, Diff) %>% rename("CSF_SYN"="Diff") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SCOPA_TOT)
  ) %>%
  mutate(Diff=(SCOPA_TOT-Baseline)) %>% select(NUM, Diff) %>% rename("SCOPA_TOT"="Diff")
  ) %>% 
  inner_join(AllMSA_Pop_Baseline_671) %>%
  # summarise(cor=cor(CSF_SYN, SCOPA_TOT, method="spearman"))
   group_by(DIAG) %>% summarise(cor=cor(CSF_SYN, SCOPA_TOT, method="spearman"))
  # group_by(DIAGNIV) %>% summarise(cor=cor(CSF_SYN, SCOPA_TOT, method="spearman"))


# ----------
# Early CT Takeda  ---------

# Year 1

EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN)
  ) %>%
  mutate(Diff=(SYN-Baseline)) %>% select(NUM, Diff) %>% rename("CSF_SYN"="Diff") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SCOPA_TOT)
  ) %>%
  mutate(Diff=(SCOPA_TOT-Baseline)) %>% select(NUM, Diff) %>% rename("SCOPA_TOT"="Diff")
  ) %>% 
  inner_join(EarlyCT_Pop_Baseline_319) %>%
  # summarise(cor=cor(CSF_SYN, SCOPA_TOT, method="spearman"))
   group_by(DIAG) %>% summarise(cor=cor(CSF_SYN, SCOPA_TOT, method="spearman"))
  # group_by(DIAGNIV) %>% summarise(cor=cor(CSF_SYN, SCOPA_TOT, method="spearman"))


# ---------

# CSF SYN vs COMPASS31 Exact Change --------


# COMPASS
dataCohorteManaged <- readRDS("Source/dataCohorteManaged.rds")

dataCohorteManaged <- dataCohorteManaged %>% group_by(NUM) %>% mutate(TIME_STUDY = ifelse( is.na(TIME_STUDY), 0, TIME_STUDY)) %>%
  mutate(Year= ifelse(TIME_STUDY==0, 0,
                      ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                             ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                    ifelse(TIME_STUDY>=2.5 ,3, NA))))) 

compass31_20240412 <- haven::read_sas("Source/compass31_20240412.sas7bdat")

COMPASS <- dataCohorteManaged %>% 
  select(NUM, DATECONSULT, TIME_STUDY, Year) %>%
  inner_join(compass31_20240412, by=c("NUM"="NUM", "DATECONSULT"="DATECONSULT"))

COMPASS <- COMPASS %>% filter(!is.na(COMPASS_TOT) )
COMPASS <- COMPASS[COMPASS$COMPASS_TOT>0,]

COMPASS_Baseline_Pats <- COMPASS %>% filter(Year==0) %>% filter(!is.na(COMPASS_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM) %>% distinct() %>%
  inner_join(AllMSA_Pop_Baseline_671)





# CSF SYN

biofluidsForPaulo <- readRDS("Source/biofluidsForPaulo.rds")
names(biofluidsForPaulo)
CSF_SYN <- biofluidsForPaulo %>% rename("NUM"="idGlobal") %>% 
  select(NUM, datePrelevement, total_alpha_syn_g_pgml_csf, total_alpha_syn_q1_pgml_csf, total_alpha_syn_q2_pgml_csf, 
         total_alpha_syn_ipp_pgml_csf, totalAlphaSyn_Fusion_pgml_csf)

CSF_SYN <- Visits_zero %>% inner_join(CSF_SYN) %>%
  mutate(TIME_STUDY=time_length(difftime(datePrelevement, dateVisite0 ), "years")) %>%
  mutate(Year= ifelse(TIME_STUDY<=0.5, 0,
                      ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                             ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                    ifelse(TIME_STUDY>=2.5 ,3, NA))))) 

CSF_SYN <- CSF_SYN %>% gather(original, SYN, total_alpha_syn_g_pgml_csf:totalAlphaSyn_Fusion_pgml_csf) %>%
  drop_na() %>% arrange(NUM, datePrelevement) %>%
  group_by(NUM, datePrelevement) %>% mutate(SYN=mean(SYN)) %>%
  select(-original) %>% distinct()

# ----------


# Overall MSA ---------

# Year 1

AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN)
  ) %>%
  mutate(Diff=(SYN-Baseline)) %>% select(NUM, Diff) %>% rename("CSF_SYN"="Diff") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, COMPASS_TOT) %>%
  rename("Baseline"="COMPASS_TOT") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, COMPASS_TOT)
  ) %>%
  mutate(Diff=(COMPASS_TOT-Baseline)) %>% select(NUM, Diff) %>% rename("COMPASS_TOT"="Diff")
  ) %>% 
  inner_join(AllMSA_Pop_Baseline_671) %>%
  filter(DIAGNIV=="PROB") %>%
   #summarise(cor=cor(CSF_SYN, COMPASS_TOT, method="spearman"))
   summarise(cor=broom::tidy(cor.test(CSF_SYN, COMPASS_TOT, method="spearman")))
  # group_by(DIAG) %>% summarise(cor=cor(CSF_SYN, COMPASS_TOT, method="spearman"))
  #  group_by(DIAG) %>% summarise(cor=broom::tidy(cor.test(CSF_SYN, COMPASS_TOT, method="spearman")))
  # group_by(DIAGNIV) %>% summarise(cor=cor(CSF_SYN, COMPASS_TOT, method="spearman"))
  # group_by(DIAGNIV) %>% summarise(cor=broom::tidy(cor.test(CSF_SYN, COMPASS_TOT, method="spearman")))


# ----------
# Early CT Takeda  ---------

# Year 1

EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN)
  ) %>%
  mutate(Diff=(SYN-Baseline)) %>% select(NUM, Diff) %>% rename("CSF_SYN"="Diff") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, COMPASS_TOT) %>%
  rename("Baseline"="COMPASS_TOT") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, COMPASS_TOT)
  ) %>%
  mutate(Diff=(COMPASS_TOT-Baseline)) %>% select(NUM, Diff) %>% rename("COMPASS_TOT"="Diff")
  ) %>% 
  inner_join(EarlyCT_Pop_Baseline_319) %>%
  #summarise(cor=cor(CSF_SYN, COMPASS_TOT, method="spearman"))
  summarise(cor=broom::tidy(cor.test(CSF_SYN, COMPASS_TOT, method="spearman")))
# group_by(DIAG) %>% summarise(cor=cor(CSF_SYN, COMPASS_TOT, method="spearman"))
#  group_by(DIAG) %>% summarise(cor=broom::tidy(cor.test(CSF_SYN, COMPASS_TOT, method="spearman")))
# group_by(DIAGNIV) %>% summarise(cor=cor(CSF_SYN, COMPASS_TOT, method="spearman"))
# group_by(DIAGNIV) %>% summarise(cor=broom::tidy(cor.test(CSF_SYN, COMPASS_TOT, method="spearman")))


# ---------

# CSF SYN vs MSA QoL Exact Change --------


# SCHRAG
dataCohorteManaged <- readRDS("Source/dataCohorteManaged.rds")

dataCohorteManaged <- dataCohorteManaged %>% group_by(NUM) %>% mutate(TIME_STUDY = ifelse( is.na(TIME_STUDY), 0, TIME_STUDY)) %>%
  mutate(Year= ifelse(TIME_STUDY==0, 0,
                      ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                             ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                    ifelse(TIME_STUDY>=2.5 ,3, NA))))) 

SCHRAG <- dataCohorteManaged %>% 
  select(NUM, DATECONSULT, TIME_STUDY, Year, SCHRAG_1:SCHRAG_40, SCHRAG_TOT, SCHRAG_MOTOR, SCHRAG_NONMOTOR, SCHRAG_EMOTIONAL, ECHANQV)

SCHRAG$SCHRAG_TOT_v2 <- rowSums(SCHRAG[, 5:44], na.rm = TRUE)

SCHRAG$SCHRAG_TOT_v2 <- rowSums(SCHRAG[, 5:44], na.rm = TRUE)
SCHRAG$SCHRAG_MOTOR_v2 <- rowSums(SCHRAG[, 5:18], na.rm = TRUE)
SCHRAG$SCHRAG_NONMOTOR_v2 <- rowSums(SCHRAG[, 19:30], na.rm = TRUE)
SCHRAG$SCHRAG_EMOTIONAL_v2 <- rowSums(SCHRAG[, 31:44], na.rm = TRUE)

SCHRAG <- SCHRAG %>% select(-c(SCHRAG_TOT, SCHRAG_MOTOR, SCHRAG_NONMOTOR, SCHRAG_EMOTIONAL))
SCHRAG <- SCHRAG %>% filter(SCHRAG_TOT_v2!=0)
SCHRAG$all_na <- rowSums(is.na(SCHRAG[, 5:44])) == (44 - 5 + 1)
SCHRAG$number_na <- rowSums(is.na(SCHRAG[, 5:44])) 
SCHRAG <- SCHRAG %>% filter(!(all_na) )

SCHRAG_Baseline_Pats <- SCHRAG %>% filter(Year==0) %>% filter(!is.na(SCHRAG_TOT_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM) %>% distinct() %>%
  inner_join(AllMSA_Pop_Baseline_671)





# CSF SYN

biofluidsForPaulo <- readRDS("Source/biofluidsForPaulo.rds")
names(biofluidsForPaulo)
CSF_SYN <- biofluidsForPaulo %>% rename("NUM"="idGlobal") %>% 
  select(NUM, datePrelevement, total_alpha_syn_g_pgml_csf, total_alpha_syn_q1_pgml_csf, total_alpha_syn_q2_pgml_csf, 
         total_alpha_syn_ipp_pgml_csf, totalAlphaSyn_Fusion_pgml_csf)

CSF_SYN <- Visits_zero %>% inner_join(CSF_SYN) %>%
  mutate(TIME_STUDY=time_length(difftime(datePrelevement, dateVisite0 ), "years")) %>%
  mutate(Year= ifelse(TIME_STUDY<=0.5, 0,
                      ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                             ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                    ifelse(TIME_STUDY>=2.5 ,3, NA))))) 

CSF_SYN <- CSF_SYN %>% gather(original, SYN, total_alpha_syn_g_pgml_csf:totalAlphaSyn_Fusion_pgml_csf) %>%
  drop_na() %>% arrange(NUM, datePrelevement) %>%
  group_by(NUM, datePrelevement) %>% mutate(SYN=mean(SYN)) %>%
  select(-original) %>% distinct()
# ----------


# Overall MSA ---------

# Year 1

AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(CSF_SYN) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN)
  ) %>%
  mutate(Diff=(SYN-Baseline)) %>% select(NUM, Diff) %>% rename("CSF_SYN"="Diff") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SCHRAG_TOT_v2) %>%
  rename("Baseline"="SCHRAG_TOT_v2") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SCHRAG_TOT_v2)
  ) %>%
  mutate(Diff=(SCHRAG_TOT_v2-Baseline)) %>% select(NUM, Diff) %>% rename("SCHRAG_TOT_v2"="Diff")
  ) %>% 
  inner_join(AllMSA_Pop_Baseline_671) %>%
  #filter(DIAGNIV=="PROB") %>%
  #summarise(cor=cor(CSF_SYN, SCHRAG_TOT_v2, method="spearman"))
  # summarise(cor=broom::tidy(cor.test(CSF_SYN, SCHRAG_TOT_v2, method="spearman")))
 # group_by(DIAG) %>% summarise(cor=cor(CSF_SYN, SCHRAG_TOT_v2, method="spearman"))
   group_by(DIAG) %>% summarise(cor=broom::tidy(cor.test(CSF_SYN, SCHRAG_TOT_v2, method="spearman")))
 # group_by(DIAGNIV) %>% summarise(cor=cor(CSF_SYN, SCHRAG_TOT_v2, method="spearman"))
 # group_by(DIAGNIV) %>% summarise(cor=broom::tidy(cor.test(CSF_SYN, SCHRAG_TOT_v2, method="spearman")))


# ----------
# Early CT Takeda  ---------

# Year 1

EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN) %>%
  rename("Baseline"="SYN") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(CSF_SYN) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SYN)
  ) %>%
  mutate(Diff=(SYN-Baseline)) %>% select(NUM, Diff) %>% rename("CSF_SYN"="Diff") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SCHRAG_TOT_v2) %>%
  rename("Baseline"="SCHRAG_TOT_v2") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>% group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, SCHRAG_TOT_v2)
  ) %>%
  mutate(Diff=(SCHRAG_TOT_v2-Baseline)) %>% select(NUM, Diff) %>% rename("SCHRAG_TOT_v2"="Diff")
  ) %>% 
  inner_join(EarlyCT_Pop_Baseline_319) %>%
   summarise(cor=cor(CSF_SYN, SCHRAG_TOT_v2, method="spearman"))
  # group_by(DIAG) %>% summarise(cor=cor(CSF_SYN, SCHRAG_TOT_v2, method="spearman"))
  # group_by(DIAGNIV) %>% summarise(cor=cor(CSF_SYN, SCHRAG_TOT_v2, method="spearman"))


# ---------
