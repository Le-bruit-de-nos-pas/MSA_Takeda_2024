
library(tidyverse)
library(data.table)
library(moments)
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

