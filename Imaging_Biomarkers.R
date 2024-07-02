
library(tidyverse)
library(data.table)
library(moments)
library(lubridate)
options(scipen = 999)


# IMAGING BIOMARKERS -------------------------------
# ------------
# PLOTS --------------
# ------------
# All data dates groups  --------
dataCohorteManaged <- readRDS("Source/dataCohorteManaged.rds")

Visits_zero <- dataCohorteManaged  %>% select(NUM, dateVisite0) %>% distinct()

AllMSA_Pop_Baseline_671 <- fread("Source/AllMSA_Pop_Baseline_671.txt")
EarlyCT_Pop_Baseline_319 <- fread("Source/EarlyCT_Pop_Baseline_319.txt")

library(UpSetR)
# ----------
# Left Putamen Volume -----------------------------

irmForPaulo <- readRDS("Source/irmForPaulo.rds")
names(irmForPaulo)
Left_Putamen_VOL <- irmForPaulo %>% select(NUM, DateIRM, Left_Putamen)

Left_Putamen_VOL <- Visits_zero %>% inner_join(Left_Putamen_VOL) %>%
  mutate(TIME_STUDY=time_length(difftime(DateIRM, dateVisite0 ), "years")) %>%
  mutate(Year= ifelse(TIME_STUDY<=0.5, 0,
                      ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                             ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                    ifelse(TIME_STUDY>=2.5 ,3, NA))))) 

Left_Putamen_VOL <- Left_Putamen_VOL %>% 
  drop_na() %>% arrange(NUM, DateIRM) %>%
  group_by(NUM, DateIRM) %>% mutate(Left_Putamen=mean(Left_Putamen)) %>%
  distinct()

dataframe <- Left_Putamen_VOL %>% ungroup() %>% select(NUM, Year) %>% distinct() %>%
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
# Right Putamen Volume -----------------------------

irmForPaulo <- readRDS("Source/irmForPaulo.rds")
names(irmForPaulo)
Right_Putamen_VOL <- irmForPaulo %>% select(NUM, DateIRM, Right_Putamen)

Right_Putamen_VOL <- Visits_zero %>% inner_join(Right_Putamen_VOL) %>%
  mutate(TIME_STUDY=time_length(difftime(DateIRM, dateVisite0 ), "years")) %>%
  mutate(Year= ifelse(TIME_STUDY<=0.5, 0,
                      ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                             ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                    ifelse(TIME_STUDY>=2.5 ,3, NA))))) 

Right_Putamen_VOL <- Right_Putamen_VOL %>% 
  drop_na() %>% arrange(NUM, DateIRM) %>%
  group_by(NUM, DateIRM) %>% mutate(Right_Putamen=mean(Right_Putamen)) %>%
  distinct()

dataframe <- Right_Putamen_VOL %>% ungroup() %>% select(NUM, Year) %>% distinct() %>%
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
# Cerebellum Volume -----------------------------
irmForPaulo <- readRDS("Source/irmForPaulo.rds")
names(irmForPaulo)
Cerebellum_VOL <- irmForPaulo %>% select(NUM, DateIRM, Left_Cerebellum_White_Matter,Right_Cerebellum_White_Matter,
                                            Left_Cerebellum_Cortex, Right_Cerebellum_Cortex)

Cerebellum_VOL <- Cerebellum_VOL %>% mutate(Cerebellum=Left_Cerebellum_White_Matter+Right_Cerebellum_White_Matter++Left_Cerebellum_Cortex+Right_Cerebellum_Cortex)


Cerebellum_VOL <- Visits_zero %>% inner_join(Cerebellum_VOL) %>%
  mutate(TIME_STUDY=time_length(difftime(DateIRM, dateVisite0 ), "years")) %>%
  mutate(Year= ifelse(TIME_STUDY<=0.5, 0,
                      ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                             ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                    ifelse(TIME_STUDY>=2.5 ,3, NA))))) 

Cerebellum_VOL <- Cerebellum_VOL %>% 
  drop_na() %>% arrange(NUM, DateIRM) %>%
  group_by(NUM, DateIRM) %>% mutate(Cerebellum=mean(as.numeric(Cerebellum))) %>%
  distinct()

dataframe <- Cerebellum_VOL %>% ungroup() %>% select(NUM, Year) %>% distinct() %>%
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
# Midbrain Volume -----------------------------
irmForPaulo <- readRDS("Source/irmForPaulo.rds")
names(irmForPaulo)
Midbrain_VOL <- irmForPaulo %>% select(NUM, DateIRM, Midbrain)

Midbrain_VOL <- Visits_zero %>% inner_join(Midbrain_VOL) %>%
  mutate(TIME_STUDY=time_length(difftime(DateIRM, dateVisite0 ), "years")) %>%
  mutate(Year= ifelse(TIME_STUDY<=0.5, 0,
                      ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                             ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                    ifelse(TIME_STUDY>=2.5 ,3, NA))))) 

Midbrain_VOL <- Midbrain_VOL %>% 
  drop_na() %>% arrange(NUM, DateIRM) %>%
  group_by(NUM, DateIRM) %>% mutate(Midbrain=mean(as.numeric(Midbrain), na.rm=T)) %>%
  distinct()

dataframe <- Cerebellum_VOL %>% ungroup() %>% select(NUM, Year) %>% distinct() %>%
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
# PONS Volume -----------------------------
irmForPaulo <- readRDS("Source/irmForPaulo.rds")
names(irmForPaulo)
Pons_VOL <- irmForPaulo %>% select(NUM, DateIRM, Pons)

Pons_VOL <- Visits_zero %>% inner_join(Pons_VOL) %>%
  mutate(TIME_STUDY=time_length(difftime(DateIRM, dateVisite0 ), "years")) %>%
  mutate(Year= ifelse(TIME_STUDY<=0.5, 0,
                      ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                             ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                    ifelse(TIME_STUDY>=2.5 ,3, NA))))) 

Pons_VOL <- Pons_VOL %>% 
  drop_na() %>% arrange(NUM, DateIRM) %>%
  group_by(NUM, DateIRM) %>% mutate(Pons=mean(as.numeric(Pons), na.rm=T)) %>%
  distinct()

dataframe <- Pons_VOL %>% ungroup() %>% select(NUM, Year) %>% distinct() %>%
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
# Medulla Volume -----------------------------
irmForPaulo <- readRDS("Source/irmForPaulo.rds")
names(irmForPaulo)
Medulla_VOL <- irmForPaulo %>% select(NUM, DateIRM, Medulla)

Medulla_VOL <- Visits_zero %>% inner_join(Medulla_VOL) %>%
  mutate(TIME_STUDY=time_length(difftime(DateIRM, dateVisite0 ), "years")) %>%
  mutate(Year= ifelse(TIME_STUDY<=0.5, 0,
                      ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                             ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                    ifelse(TIME_STUDY>=2.5 ,3, NA))))) 

Medulla_VOL <- Medulla_VOL %>% 
  drop_na() %>% arrange(NUM, DateIRM) %>%
  group_by(NUM, DateIRM) %>% mutate(Medulla=mean(as.numeric(Medulla), na.rm=T)) %>%
  distinct()

dataframe <- Medulla_VOL %>% ungroup() %>% select(NUM, Year) %>% distinct() %>%
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
# Whole Brain Volume  -----------------------------
irmForPaulo <- readRDS("Source/irmForPaulo.rds")
names(irmForPaulo)
Total_VOL <- irmForPaulo %>% select(NUM, DateIRM, CerebralWhiteMatterVol, TotalGrayVol)

Total_VOL <- Total_VOL %>% mutate(Total=as.numeric(CerebralWhiteMatterVol)+as.numeric(TotalGrayVol) )

Total_VOL <- Visits_zero %>% inner_join(Total_VOL) %>%
  mutate(TIME_STUDY=time_length(difftime(DateIRM, dateVisite0 ), "years")) %>%
  mutate(Year= ifelse(TIME_STUDY<=0.5, 0,
                      ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                             ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                    ifelse(TIME_STUDY>=2.5 ,3, NA))))) 

Total_VOL <- Total_VOL %>% 
  drop_na() %>% arrange(NUM, DateIRM) %>%
  group_by(NUM, DateIRM) %>% mutate(Total=mean(as.numeric(Total), na.rm=T)) %>%
  distinct()

dataframe <- Total_VOL %>% ungroup() %>% select(NUM, Year) %>% distinct() %>%
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
# Left Putamen Volume -----------------------------

irmForPaulo <- readRDS("Source/irmForPaulo.rds")

Left_Putamen_VOL <- irmForPaulo %>% select(NUM, DateIRM, Left_Putamen)

Left_Putamen_VOL <- Visits_zero %>% inner_join(Left_Putamen_VOL) %>%
  mutate(TIME_STUDY=time_length(difftime(DateIRM, dateVisite0 ), "years")) %>%
  mutate(Year= ifelse(TIME_STUDY<=0.5, 0,
                      ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                             ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                    ifelse(TIME_STUDY>=2.5 ,3, NA))))) 


Left_Putamen_VOL <- Left_Putamen_VOL %>% 
  drop_na() %>% arrange(NUM, DateIRM) %>%
  group_by(NUM, DateIRM) %>% mutate(Left_Putamen=mean(Left_Putamen)) %>%
  distinct()


Left_Putamen_VOL %>% 
  inner_join(AllMSA_Pop_Baseline_671) %>%
  ggplot(aes(TIME_STUDY, Left_Putamen)) +
  stat_smooth(method="loess", colour="deepskyblue4", fill="deepskyblue4" , alpha=0.25, lwd=1.5, se=TRUE)+
  geom_line(aes(group=NUM), col="black" , linewidth=1, alpha=0.6) +
  geom_jitter(size=2, colour="black", alpha=0.7, shape=1, stroke=2) +
  theme_minimal() +
  xlab("\n Elapsed number of years since study enrollment \n [All available patient records up to year 5]") +
  ylab("Left Putamen Volume \n At each MRI \n") +
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
# Right Putamen Volume -----------------------------

irmForPaulo <- readRDS("Source/irmForPaulo.rds")
names(irmForPaulo)
Right_Putamen_VOL <- irmForPaulo %>% select(NUM, DateIRM, Right_Putamen)

Right_Putamen_VOL <- Visits_zero %>% inner_join(Right_Putamen_VOL) %>%
  mutate(TIME_STUDY=time_length(difftime(DateIRM, dateVisite0 ), "years")) %>%
  mutate(Year= ifelse(TIME_STUDY<=0.5, 0,
                      ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                             ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                    ifelse(TIME_STUDY>=2.5 ,3, NA))))) 

Right_Putamen_VOL <- Right_Putamen_VOL %>% 
  drop_na() %>% arrange(NUM, DateIRM) %>%
  group_by(NUM, DateIRM) %>% mutate(Right_Putamen=mean(Right_Putamen)) %>%
  distinct()

Right_Putamen_VOL %>% 
  inner_join(AllMSA_Pop_Baseline_671) %>%
  ggplot(aes(TIME_STUDY, Right_Putamen)) +
  stat_smooth(method="loess", colour="deepskyblue4", fill="deepskyblue4" , alpha=0.25, lwd=1.5, se=TRUE)+
  geom_line(aes(group=NUM), col="black" , linewidth=1, alpha=0.6) +
  geom_jitter(size=2, colour="black", alpha=0.7, shape=1, stroke=2) +
  theme_minimal() +
  xlab("\n Elapsed number of years since study enrollment \n [All available patient records up to year 5]") +
  ylab("Right Putamen Volume \n At each MRI \n") +
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

# ---------------------
# Cerebellum Volume -----------------------------
irmForPaulo <- readRDS("Source/irmForPaulo.rds")
names(irmForPaulo)
Cerebellum_VOL <- irmForPaulo %>% select(NUM, DateIRM, Left_Cerebellum_White_Matter,Right_Cerebellum_White_Matter,
                                         Left_Cerebellum_Cortex, Right_Cerebellum_Cortex)

Cerebellum_VOL <- Cerebellum_VOL %>% mutate(Cerebellum=Left_Cerebellum_White_Matter+Right_Cerebellum_White_Matter++Left_Cerebellum_Cortex+Right_Cerebellum_Cortex)


Cerebellum_VOL <- Visits_zero %>% inner_join(Cerebellum_VOL) %>%
  mutate(TIME_STUDY=time_length(difftime(DateIRM, dateVisite0 ), "years")) %>%
  mutate(Year= ifelse(TIME_STUDY<=0.5, 0,
                      ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                             ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                    ifelse(TIME_STUDY>=2.5 ,3, NA))))) 

Cerebellum_VOL <- Cerebellum_VOL %>% 
  drop_na() %>% arrange(NUM, DateIRM) %>%
  group_by(NUM, DateIRM) %>% mutate(Cerebellum=mean(as.numeric(Cerebellum))) %>%
  distinct()

Cerebellum_VOL %>% 
  inner_join(AllMSA_Pop_Baseline_671) %>%
  ggplot(aes(TIME_STUDY, Cerebellum)) +
  stat_smooth(method="loess", colour="deepskyblue4", fill="deepskyblue4" , alpha=0.25, lwd=1.5, se=TRUE)+
  geom_line(aes(group=NUM), col="black" , linewidth=1, alpha=0.6) +
  geom_jitter(size=2, colour="black", alpha=0.7, shape=1, stroke=2) +
  theme_minimal() +
  xlab("\n Elapsed number of years since study enrollment \n [All available patient records up to year 5]") +
  ylab("Cerebellum Volume \n At each MRI \n") +
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

# ---------------------
# Midbrain Volume -----------------------------
irmForPaulo <- readRDS("Source/irmForPaulo.rds")
names(irmForPaulo)
Midbrain_VOL <- irmForPaulo %>% select(NUM, DateIRM, Midbrain)

Midbrain_VOL <- Visits_zero %>% inner_join(Midbrain_VOL) %>%
  mutate(TIME_STUDY=time_length(difftime(DateIRM, dateVisite0 ), "years")) %>%
  mutate(Year= ifelse(TIME_STUDY<=0.5, 0,
                      ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                             ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                    ifelse(TIME_STUDY>=2.5 ,3, NA))))) 

Midbrain_VOL <- Midbrain_VOL %>% 
  drop_na() %>% arrange(NUM, DateIRM) %>%
  group_by(NUM, DateIRM) %>% mutate(Midbrain=mean(as.numeric(Midbrain), na.rm=T)) %>%
  distinct()

Midbrain_VOL %>% 
  inner_join(AllMSA_Pop_Baseline_671) %>%
  ggplot(aes(TIME_STUDY, Midbrain)) +
  stat_smooth(method="loess", colour="deepskyblue4", fill="deepskyblue4" , alpha=0.25, lwd=1.5, se=TRUE)+
  geom_line(aes(group=NUM), col="black" , linewidth=1, alpha=0.6) +
  geom_jitter(size=2, colour="black", alpha=0.7, shape=1, stroke=2) +
  theme_minimal() +
  xlab("\n Elapsed number of years since study enrollment \n [All available patient records up to year 5]") +
  ylab("Midbrain Volume \n At each MRI \n") +
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

# ---------------------
# Pons Volume -----------------------------

irmForPaulo <- readRDS("Source/irmForPaulo.rds")
names(irmForPaulo)
Pons_VOL <- irmForPaulo %>% select(NUM, DateIRM, Pons)

Pons_VOL <- Visits_zero %>% inner_join(Pons_VOL) %>%
  mutate(TIME_STUDY=time_length(difftime(DateIRM, dateVisite0 ), "years")) %>%
  mutate(Year= ifelse(TIME_STUDY<=0.5, 0,
                      ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                             ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                    ifelse(TIME_STUDY>=2.5 ,3, NA))))) 

Pons_VOL <- Pons_VOL %>% 
  drop_na() %>% arrange(NUM, DateIRM) %>%
  group_by(NUM, DateIRM) %>% mutate(Pons=mean(as.numeric(Pons), na.rm=T)) %>%
  distinct()

Pons_VOL %>% 
  inner_join(AllMSA_Pop_Baseline_671) %>%
  ggplot(aes(TIME_STUDY, Pons)) +
  stat_smooth(method="loess", colour="deepskyblue4", fill="deepskyblue4" , alpha=0.25, lwd=1.5, se=TRUE)+
  geom_line(aes(group=NUM), col="black" , linewidth=1, alpha=0.6) +
  geom_jitter(size=2, colour="black", alpha=0.7, shape=1, stroke=2) +
  theme_minimal() +
  xlab("\n Elapsed number of years since study enrollment \n [All available patient records up to year 5]") +
  ylab("Pons Volume \n At each MRI \n") +
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

# ---------------------
# Medulla Volume -----------------------------

irmForPaulo <- readRDS("Source/irmForPaulo.rds")
names(irmForPaulo)
Medulla_VOL <- irmForPaulo %>% select(NUM, DateIRM, Medulla)

Medulla_VOL <- Visits_zero %>% inner_join(Medulla_VOL) %>%
  mutate(TIME_STUDY=time_length(difftime(DateIRM, dateVisite0 ), "years")) %>%
  mutate(Year= ifelse(TIME_STUDY<=0.5, 0,
                      ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                             ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                    ifelse(TIME_STUDY>=2.5 ,3, NA))))) 

Medulla_VOL <- Medulla_VOL %>% 
  drop_na() %>% arrange(NUM, DateIRM) %>%
  group_by(NUM, DateIRM) %>% mutate(Medulla=mean(as.numeric(Medulla), na.rm=T)) %>%
  distinct()

Medulla_VOL %>% 
  inner_join(AllMSA_Pop_Baseline_671) %>%
  ggplot(aes(TIME_STUDY, Medulla)) +
  stat_smooth(method="loess", colour="deepskyblue4", fill="deepskyblue4" , alpha=0.25, lwd=1.5, se=TRUE)+
  geom_line(aes(group=NUM), col="black" , linewidth=1, alpha=0.6) +
  geom_jitter(size=2, colour="black", alpha=0.7, shape=1, stroke=2) +
  theme_minimal() +
  xlab("\n Elapsed number of years since study enrollment \n [All available patient records up to year 5]") +
  ylab("Medulla Volume \n At each MRI \n") +
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

# ---------------------
# Whole Brain Volume -----------------------------

irmForPaulo <- readRDS("Source/irmForPaulo.rds")
names(irmForPaulo)
Total_VOL <- irmForPaulo %>% select(NUM, DateIRM, CerebralWhiteMatterVol, TotalGrayVol)

Total_VOL <- Total_VOL %>% mutate(Total=as.numeric(CerebralWhiteMatterVol)+as.numeric(TotalGrayVol) )

Total_VOL <- Visits_zero %>% inner_join(Total_VOL) %>%
  mutate(TIME_STUDY=time_length(difftime(DateIRM, dateVisite0 ), "years")) %>%
  mutate(Year= ifelse(TIME_STUDY<=0.5, 0,
                      ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                             ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                    ifelse(TIME_STUDY>=2.5 ,3, NA))))) 

Total_VOL <- Total_VOL %>% 
  drop_na() %>% arrange(NUM, DateIRM) %>%
  group_by(NUM, DateIRM) %>% mutate(Total=mean(as.numeric(Total), na.rm=T)) %>%
  distinct()

Total_VOL %>% 
  inner_join(AllMSA_Pop_Baseline_671) %>%
  ggplot(aes(TIME_STUDY, Total)) +
  stat_smooth(method="loess", colour="deepskyblue4", fill="deepskyblue4" , alpha=0.25, lwd=1.5, se=TRUE)+
  geom_line(aes(group=NUM), col="black" , linewidth=1, alpha=0.6) +
  geom_jitter(size=2, colour="black", alpha=0.7, shape=1, stroke=2) +
  theme_minimal() +
  xlab("\n Elapsed number of years since study enrollment \n [All available patient records up to year 5]") +
  ylab("Whole Brain Volume \n At each MRI \n") +
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

# ---------------------
# END OF PLOTS LONGITUDINAL ----------
# ----------
# All data dates groups  --------
dataCohorteManaged <- readRDS("Source/dataCohorteManaged.rds")

Visits_zero <- dataCohorteManaged  %>% select(NUM, dateVisite0) %>% distinct()

AllMSA_Pop_Baseline_671 <- fread("Source/AllMSA_Pop_Baseline_671.txt")
EarlyCT_Pop_Baseline_319 <- fread("Source/EarlyCT_Pop_Baseline_319.txt")


# ----------
# Left Putamen Volume -----------------------------

irmForPaulo <- readRDS("Source/irmForPaulo.rds")

Left_Putamen_VOL <- irmForPaulo %>% select(NUM, DateIRM, Left_Putamen)

Left_Putamen_VOL <- Visits_zero %>% inner_join(Left_Putamen_VOL) %>%
  mutate(TIME_STUDY=time_length(difftime(DateIRM, dateVisite0 ), "years")) %>%
  mutate(Year= ifelse(TIME_STUDY<=0.5, 0,
                      ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                             ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                    ifelse(TIME_STUDY>=2.5 ,3, NA))))) 

Left_Putamen_VOL <- Left_Putamen_VOL %>% 
  drop_na() %>% arrange(NUM, DateIRM) %>%
  group_by(NUM, DateIRM) %>% mutate(Left_Putamen=mean(Left_Putamen)) %>%
  distinct()

# ----------
# Overall MSA Entire  -----------------------

# ALL  Left_Putamen Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(Left_Putamen),
    sd=sd(Left_Putamen),
    median=median(Left_Putamen),
    Q1 = quantile(Left_Putamen, 0.25),
    Q3 = quantile(Left_Putamen, 0.75),
    min=min(Left_Putamen),
    max=max(Left_Putamen),
    n=n(),
    skewness = skewness(Left_Putamen, na.rm = TRUE),
    kurtosis = kurtosis(Left_Putamen, na.rm = TRUE),
    CI_lower = t.test(Left_Putamen)$conf.int[1],
    CI_upper = t.test(Left_Putamen)$conf.int[2],
    shapiro_w = shapiro.test(Left_Putamen)$statistic,
    shapiro_p = shapiro.test(Left_Putamen)$p.value
  )


# ALL  Left_Putamen Total Year 1

AllMSA_Pop_Baseline_671 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(Left_Putamen),
    sd=sd(Left_Putamen),
    median=median(Left_Putamen),
    Q1 = quantile(Left_Putamen, 0.25),
    Q3 = quantile(Left_Putamen, 0.75),
    min=min(Left_Putamen),
    max=max(Left_Putamen),
    n=n(),
    skewness = skewness(Left_Putamen, na.rm = TRUE),
    kurtosis = kurtosis(Left_Putamen, na.rm = TRUE),
    CI_lower = t.test(Left_Putamen)$conf.int[1],
    CI_upper = t.test(Left_Putamen)$conf.int[2],
    shapiro_w = shapiro.test(Left_Putamen)$statistic,
    shapiro_p = shapiro.test(Left_Putamen)$p.value
  )



# Elapsed Time Year 1


AllMSA_Pop_Baseline_671 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen, TIME_STUDY) %>%
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


AllMSA_Pop_Baseline_671 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen) %>%
  rename("Baseline"="Left_Putamen") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen)
  ) %>%
  mutate(Diff=(Left_Putamen-Baseline))  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
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

AllMSA_Pop_Baseline_671 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen) %>%
  rename("Baseline"="Left_Putamen") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen, TIME_STUDY)
  ) %>%
  mutate(Diff=(Left_Putamen-Baseline)/TIME_STUDY)  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )













# ALL  Left_Putamen Total Year 2

AllMSA_Pop_Baseline_671 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(Left_Putamen),
    sd=sd(Left_Putamen),
    median=median(Left_Putamen),
    Q1 = quantile(Left_Putamen, 0.25),
    Q3 = quantile(Left_Putamen, 0.75),
    min=min(Left_Putamen),
    max=max(Left_Putamen),
    n=n(),
    skewness = skewness(Left_Putamen, na.rm = TRUE),
    kurtosis = kurtosis(Left_Putamen, na.rm = TRUE),
    CI_lower = t.test(Left_Putamen)$conf.int[1],
    CI_upper = t.test(Left_Putamen)$conf.int[2],
    shapiro_w = shapiro.test(Left_Putamen)$statistic,
    shapiro_p = shapiro.test(Left_Putamen)$p.value
  )



# Elapsed Time Year 2


AllMSA_Pop_Baseline_671 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen, TIME_STUDY) %>%
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


AllMSA_Pop_Baseline_671 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen) %>%
  rename("Baseline"="Left_Putamen") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen)
  ) %>%
  mutate(Diff=(Left_Putamen-Baseline))  %>%
  summarise(
    mean=mean(Diff),
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

AllMSA_Pop_Baseline_671 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen) %>%
  rename("Baseline"="Left_Putamen") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen, TIME_STUDY)
  ) %>%
  mutate(Diff=(Left_Putamen-Baseline)/TIME_STUDY)  %>%
  summarise(
    mean=mean(Diff),
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






# ALL  Left_Putamen Total Year 3

AllMSA_Pop_Baseline_671 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(Left_Putamen),
    sd=sd(Left_Putamen),
    median=median(Left_Putamen),
    Q1 = quantile(Left_Putamen, 0.25),
    Q3 = quantile(Left_Putamen, 0.75),
    min=min(Left_Putamen),
    max=max(Left_Putamen),
    n=n(),
    skewness = skewness(Left_Putamen, na.rm = TRUE),
    kurtosis = kurtosis(Left_Putamen, na.rm = TRUE),
    CI_lower = t.test(Left_Putamen)$conf.int[1],
    CI_upper = t.test(Left_Putamen)$conf.int[2],
    shapiro_w = shapiro.test(Left_Putamen)$statistic,
    shapiro_p = shapiro.test(Left_Putamen)$p.value
  )



# Elapsed Time Year 3


AllMSA_Pop_Baseline_671 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen, TIME_STUDY) %>%
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


AllMSA_Pop_Baseline_671 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen) %>%
  rename("Baseline"="Left_Putamen") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen)
  ) %>%
  mutate(Diff=(Left_Putamen-Baseline))  %>%
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

AllMSA_Pop_Baseline_671 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen) %>%
  rename("Baseline"="Left_Putamen") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen, TIME_STUDY)
  ) %>%
  mutate(Diff=(Left_Putamen-Baseline)/TIME_STUDY)  %>%
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



# -----------
# Overall MSA Entire  MSA P vs MSA C  -----------------------

# ALL  Left_Putamen Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Left_Putamen),
    sd=sd(Left_Putamen),
    median=median(Left_Putamen),
    Q1 = quantile(Left_Putamen, 0.25),
    Q3 = quantile(Left_Putamen, 0.75),
    min=min(Left_Putamen),
    max=max(Left_Putamen),
    n=n(),
    skewness = skewness(Left_Putamen, na.rm = TRUE),
    kurtosis = kurtosis(Left_Putamen, na.rm = TRUE),
    CI_lower = t.test(Left_Putamen)$conf.int[1],
    CI_upper = t.test(Left_Putamen)$conf.int[2],
    shapiro_w = shapiro.test(Left_Putamen)$statistic,
    shapiro_p = shapiro.test(Left_Putamen)$p.value
  )


# ALL  Left_Putamen Total Year 1

AllMSA_Pop_Baseline_671 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Left_Putamen),
    sd=sd(Left_Putamen),
    median=median(Left_Putamen),
    Q1 = quantile(Left_Putamen, 0.25),
    Q3 = quantile(Left_Putamen, 0.75),
    min=min(Left_Putamen),
    max=max(Left_Putamen),
    n=n(),
    skewness = skewness(Left_Putamen, na.rm = TRUE),
    kurtosis = kurtosis(Left_Putamen, na.rm = TRUE),
    CI_lower = t.test(Left_Putamen)$conf.int[1],
    CI_upper = t.test(Left_Putamen)$conf.int[2],
    shapiro_w = shapiro.test(Left_Putamen)$statistic,
    shapiro_p = shapiro.test(Left_Putamen)$p.value
  )



# Elapsed Time Year 1


AllMSA_Pop_Baseline_671 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen, DIAG, TIME_STUDY) %>%
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


AllMSA_Pop_Baseline_671 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen) %>%
  rename("Baseline"="Left_Putamen") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen, DIAG)
  ) %>%
  mutate(Diff=(Left_Putamen-Baseline))  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
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

AllMSA_Pop_Baseline_671 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen) %>%
  rename("Baseline"="Left_Putamen") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen, DIAG, TIME_STUDY)
  ) %>%
  mutate(Diff=(Left_Putamen-Baseline)/TIME_STUDY)  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )













# ALL  Left_Putamen Total Year 2

AllMSA_Pop_Baseline_671 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Left_Putamen),
    sd=sd(Left_Putamen),
    median=median(Left_Putamen),
    Q1 = quantile(Left_Putamen, 0.25),
    Q3 = quantile(Left_Putamen, 0.75),
    min=min(Left_Putamen),
    max=max(Left_Putamen),
    n=n(),
    skewness = skewness(Left_Putamen, na.rm = TRUE),
    kurtosis = kurtosis(Left_Putamen, na.rm = TRUE),
    CI_lower = t.test(Left_Putamen)$conf.int[1],
    CI_upper = t.test(Left_Putamen)$conf.int[2],
    shapiro_w = shapiro.test(Left_Putamen)$statistic,
    shapiro_p = shapiro.test(Left_Putamen)$p.value
  )



# Elapsed Time Year 2


AllMSA_Pop_Baseline_671 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen, DIAG, TIME_STUDY) %>%
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


AllMSA_Pop_Baseline_671 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen) %>%
  rename("Baseline"="Left_Putamen") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen, DIAG)
  ) %>%
  mutate(Diff=(Left_Putamen-Baseline))  %>% group_by(DIAG) %>%
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

AllMSA_Pop_Baseline_671 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen) %>%
  rename("Baseline"="Left_Putamen") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen, DIAG, TIME_STUDY)
  ) %>%
  mutate(Diff=(Left_Putamen-Baseline)/TIME_STUDY)  %>% filter(DIAG=="PD") %>%
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






# ALL  Left_Putamen Total Year 3

AllMSA_Pop_Baseline_671 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Left_Putamen),
    sd=sd(Left_Putamen),
    median=median(Left_Putamen),
    Q1 = quantile(Left_Putamen, 0.25),
    Q3 = quantile(Left_Putamen, 0.75),
    min=min(Left_Putamen),
    max=max(Left_Putamen),
    n=n(),
    skewness = skewness(Left_Putamen, na.rm = TRUE),
    kurtosis = kurtosis(Left_Putamen, na.rm = TRUE),
    CI_lower = t.test(Left_Putamen)$conf.int[1],
    CI_upper = t.test(Left_Putamen)$conf.int[2],
    shapiro_w = shapiro.test(Left_Putamen)$statistic,
    shapiro_p = shapiro.test(Left_Putamen)$p.value
  )



# Elapsed Time Year 3


AllMSA_Pop_Baseline_671 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen, DIAG, TIME_STUDY) %>%
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


AllMSA_Pop_Baseline_671 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen) %>%
  rename("Baseline"="Left_Putamen") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen, DIAG)
  ) %>%
  mutate(Diff=(Left_Putamen-Baseline))  %>% filter(DIAG=="PD") %>%
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

AllMSA_Pop_Baseline_671 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen) %>%
  rename("Baseline"="Left_Putamen") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen, DIAG, TIME_STUDY)
  ) %>%
  mutate(Diff=(Left_Putamen-Baseline)/TIME_STUDY)  %>% filter(DIAG=="PD") %>%
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



# -----------
# Overall MSA Entire  Probable vs Possible  -----------------------

# ALL  Left_Putamen Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Left_Putamen),
    sd=sd(Left_Putamen),
    median=median(Left_Putamen),
    Q1 = quantile(Left_Putamen, 0.25),
    Q3 = quantile(Left_Putamen, 0.75),
    min=min(Left_Putamen),
    max=max(Left_Putamen),
    n=n(),
    skewness = skewness(Left_Putamen, na.rm = TRUE),
    kurtosis = kurtosis(Left_Putamen, na.rm = TRUE),
    CI_lower = t.test(Left_Putamen)$conf.int[1],
    CI_upper = t.test(Left_Putamen)$conf.int[2],
    shapiro_w = shapiro.test(Left_Putamen)$statistic,
    shapiro_p = shapiro.test(Left_Putamen)$p.value
  )


# ALL  Left_Putamen Total Year 1

AllMSA_Pop_Baseline_671 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Left_Putamen),
    sd=sd(Left_Putamen),
    median=median(Left_Putamen),
    Q1 = quantile(Left_Putamen, 0.25),
    Q3 = quantile(Left_Putamen, 0.75),
    min=min(Left_Putamen),
    max=max(Left_Putamen),
    n=n(),
    skewness = skewness(Left_Putamen, na.rm = TRUE),
    kurtosis = kurtosis(Left_Putamen, na.rm = TRUE),
    CI_lower = t.test(Left_Putamen)$conf.int[1],
    CI_upper = t.test(Left_Putamen)$conf.int[2],
    shapiro_w = shapiro.test(Left_Putamen)$statistic,
    shapiro_p = shapiro.test(Left_Putamen)$p.value
  )



# Elapsed Time Year 1


AllMSA_Pop_Baseline_671 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen, DIAGNIV, TIME_STUDY) %>%
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


AllMSA_Pop_Baseline_671 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen) %>%
  rename("Baseline"="Left_Putamen") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen, DIAGNIV)
  ) %>%
  mutate(Diff=(Left_Putamen-Baseline))  %>% filter(DIAGNIV=="POS") %>%
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

AllMSA_Pop_Baseline_671 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen) %>%
  rename("Baseline"="Left_Putamen") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen, DIAGNIV, TIME_STUDY)
  ) %>%
  mutate(Diff=(Left_Putamen-Baseline)/TIME_STUDY)  %>% filter(DIAGNIV=="POS") %>%
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













# ALL  Left_Putamen Total Year 2

AllMSA_Pop_Baseline_671 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Left_Putamen),
    sd=sd(Left_Putamen),
    median=median(Left_Putamen),
    Q1 = quantile(Left_Putamen, 0.25),
    Q3 = quantile(Left_Putamen, 0.75),
    min=min(Left_Putamen),
    max=max(Left_Putamen),
    n=n(),
    skewness = skewness(Left_Putamen, na.rm = TRUE),
    kurtosis = kurtosis(Left_Putamen, na.rm = TRUE),
    CI_lower = t.test(Left_Putamen)$conf.int[1],
    CI_upper = t.test(Left_Putamen)$conf.int[2],
    shapiro_w = shapiro.test(Left_Putamen)$statistic,
    shapiro_p = shapiro.test(Left_Putamen)$p.value
  )



# Elapsed Time Year 2


AllMSA_Pop_Baseline_671 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen, DIAGNIV, TIME_STUDY) %>%
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


AllMSA_Pop_Baseline_671 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen) %>%
  rename("Baseline"="Left_Putamen") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen, DIAGNIV)
  ) %>%
  mutate(Diff=(Left_Putamen-Baseline))  %>% filter(DIAGNIV=="PROB") %>%
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

AllMSA_Pop_Baseline_671 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen) %>%
  rename("Baseline"="Left_Putamen") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen, DIAGNIV, TIME_STUDY)
  ) %>%
  mutate(Diff=(Left_Putamen-Baseline)/TIME_STUDY)  %>% filter(DIAGNIV=="PROB") %>%
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






# ALL  Left_Putamen Total Year 3

AllMSA_Pop_Baseline_671 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Left_Putamen),
    sd=sd(Left_Putamen),
    median=median(Left_Putamen),
    Q1 = quantile(Left_Putamen, 0.25),
    Q3 = quantile(Left_Putamen, 0.75),
    min=min(Left_Putamen),
    max=max(Left_Putamen),
    n=n(),
    skewness = skewness(Left_Putamen, na.rm = TRUE),
    kurtosis = kurtosis(Left_Putamen, na.rm = TRUE),
    CI_lower = t.test(Left_Putamen)$conf.int[1],
    CI_upper = t.test(Left_Putamen)$conf.int[2],
    shapiro_w = shapiro.test(Left_Putamen)$statistic,
    shapiro_p = shapiro.test(Left_Putamen)$p.value
  )



# Elapsed Time Year 3


AllMSA_Pop_Baseline_671 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen, DIAGNIV, TIME_STUDY) %>%
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


AllMSA_Pop_Baseline_671 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen) %>%
  rename("Baseline"="Left_Putamen") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen, DIAGNIV)
  ) %>%
  mutate(Diff=(Left_Putamen-Baseline))  %>% filter(DIAGNIV=="PROB") %>%
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

AllMSA_Pop_Baseline_671 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen) %>%
  rename("Baseline"="Left_Putamen") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen, DIAGNIV, TIME_STUDY)
  ) %>%
  mutate(Diff=(Left_Putamen-Baseline)/TIME_STUDY)  %>% filter(DIAGNIV=="PROB") %>%
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

# ---------------


# Target MSA Entire  -----------------------

# ALL  Left_Putamen Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(Left_Putamen),
    sd=sd(Left_Putamen),
    median=median(Left_Putamen),
    Q1 = quantile(Left_Putamen, 0.25),
    Q3 = quantile(Left_Putamen, 0.75),
    min=min(Left_Putamen),
    max=max(Left_Putamen),
    n=n(),
    skewness = skewness(Left_Putamen, na.rm = TRUE),
    kurtosis = kurtosis(Left_Putamen, na.rm = TRUE),
    CI_lower = t.test(Left_Putamen)$conf.int[1],
    CI_upper = t.test(Left_Putamen)$conf.int[2],
    shapiro_w = shapiro.test(Left_Putamen)$statistic,
    shapiro_p = shapiro.test(Left_Putamen)$p.value
  )


# ALL  Left_Putamen Total Year 1

EarlyCT_Pop_Baseline_319 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(Left_Putamen),
    sd=sd(Left_Putamen),
    median=median(Left_Putamen),
    Q1 = quantile(Left_Putamen, 0.25),
    Q3 = quantile(Left_Putamen, 0.75),
    min=min(Left_Putamen),
    max=max(Left_Putamen),
    n=n(),
    skewness = skewness(Left_Putamen, na.rm = TRUE),
    kurtosis = kurtosis(Left_Putamen, na.rm = TRUE),
    CI_lower = t.test(Left_Putamen)$conf.int[1],
    CI_upper = t.test(Left_Putamen)$conf.int[2],
    shapiro_w = shapiro.test(Left_Putamen)$statistic,
    shapiro_p = shapiro.test(Left_Putamen)$p.value
  )



# Elapsed Time Year 1


EarlyCT_Pop_Baseline_319 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen, TIME_STUDY) %>%
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


EarlyCT_Pop_Baseline_319 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen) %>%
  rename("Baseline"="Left_Putamen") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen)
  ) %>%
  mutate(Diff=(Left_Putamen-Baseline))  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
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

EarlyCT_Pop_Baseline_319 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen) %>%
  rename("Baseline"="Left_Putamen") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen, TIME_STUDY)
  ) %>%
  mutate(Diff=(Left_Putamen-Baseline)/TIME_STUDY)  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )













# ALL  Left_Putamen Total Year 2

EarlyCT_Pop_Baseline_319 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(Left_Putamen),
    sd=sd(Left_Putamen),
    median=median(Left_Putamen),
    Q1 = quantile(Left_Putamen, 0.25),
    Q3 = quantile(Left_Putamen, 0.75),
    min=min(Left_Putamen),
    max=max(Left_Putamen),
    n=n(),
    skewness = skewness(Left_Putamen, na.rm = TRUE),
    kurtosis = kurtosis(Left_Putamen, na.rm = TRUE),
    CI_lower = t.test(Left_Putamen)$conf.int[1],
    CI_upper = t.test(Left_Putamen)$conf.int[2],
    shapiro_w = shapiro.test(Left_Putamen)$statistic,
    shapiro_p = shapiro.test(Left_Putamen)$p.value
  )



# Elapsed Time Year 2


EarlyCT_Pop_Baseline_319 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen, TIME_STUDY) %>%
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


EarlyCT_Pop_Baseline_319 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen) %>%
  rename("Baseline"="Left_Putamen") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen)
  ) %>%
  mutate(Diff=(Left_Putamen-Baseline))  %>%
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

EarlyCT_Pop_Baseline_319 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen) %>%
  rename("Baseline"="Left_Putamen") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen, TIME_STUDY)
  ) %>%
  mutate(Diff=(Left_Putamen-Baseline)/TIME_STUDY)  %>%
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






# ALL  Left_Putamen Total Year 3

EarlyCT_Pop_Baseline_319 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(Left_Putamen),
    sd=sd(Left_Putamen),
    median=median(Left_Putamen),
    Q1 = quantile(Left_Putamen, 0.25),
    Q3 = quantile(Left_Putamen, 0.75),
    min=min(Left_Putamen),
    max=max(Left_Putamen),
    n=n(),
    skewness = skewness(Left_Putamen, na.rm = TRUE),
    kurtosis = kurtosis(Left_Putamen, na.rm = TRUE),
    CI_lower = t.test(Left_Putamen)$conf.int[1],
    CI_upper = t.test(Left_Putamen)$conf.int[2],
    shapiro_w = shapiro.test(Left_Putamen)$statistic,
    shapiro_p = shapiro.test(Left_Putamen)$p.value
  )



# Elapsed Time Year 3


EarlyCT_Pop_Baseline_319 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen, TIME_STUDY) %>%
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


EarlyCT_Pop_Baseline_319 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen) %>%
  rename("Baseline"="Left_Putamen") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen)
  ) %>%
  mutate(Diff=(Left_Putamen-Baseline))  %>%
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

EarlyCT_Pop_Baseline_319 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen) %>%
  rename("Baseline"="Left_Putamen") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen, TIME_STUDY)
  ) %>%
  mutate(Diff=(Left_Putamen-Baseline)/TIME_STUDY)  %>%
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



# -----------
# Target MSA Entire  MSA P vs MSA C  -----------------------

# ALL  Left_Putamen Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Left_Putamen),
    sd=sd(Left_Putamen),
    median=median(Left_Putamen),
    Q1 = quantile(Left_Putamen, 0.25),
    Q3 = quantile(Left_Putamen, 0.75),
    min=min(Left_Putamen),
    max=max(Left_Putamen),
    n=n(),
    skewness = skewness(Left_Putamen, na.rm = TRUE),
    kurtosis = kurtosis(Left_Putamen, na.rm = TRUE),
    CI_lower = t.test(Left_Putamen)$conf.int[1],
    CI_upper = t.test(Left_Putamen)$conf.int[2],
    shapiro_w = shapiro.test(Left_Putamen)$statistic,
    shapiro_p = shapiro.test(Left_Putamen)$p.value
  )


# ALL  Left_Putamen Total Year 1

EarlyCT_Pop_Baseline_319 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Left_Putamen),
    sd=sd(Left_Putamen),
    median=median(Left_Putamen),
    Q1 = quantile(Left_Putamen, 0.25),
    Q3 = quantile(Left_Putamen, 0.75),
    min=min(Left_Putamen),
    max=max(Left_Putamen),
    n=n(),
    skewness = skewness(Left_Putamen, na.rm = TRUE),
    kurtosis = kurtosis(Left_Putamen, na.rm = TRUE),
    CI_lower = t.test(Left_Putamen)$conf.int[1],
    CI_upper = t.test(Left_Putamen)$conf.int[2],
    shapiro_w = shapiro.test(Left_Putamen)$statistic,
    shapiro_p = shapiro.test(Left_Putamen)$p.value
  )



# Elapsed Time Year 1


EarlyCT_Pop_Baseline_319 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen, DIAG, TIME_STUDY) %>%
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


EarlyCT_Pop_Baseline_319 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen) %>%
  rename("Baseline"="Left_Putamen") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen, DIAG)
  ) %>%
  mutate(Diff=(Left_Putamen-Baseline))  %>% filter(DIAG=="CB") %>%
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

EarlyCT_Pop_Baseline_319 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen) %>%
  rename("Baseline"="Left_Putamen") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen, DIAG, TIME_STUDY)
  ) %>%
  mutate(Diff=(Left_Putamen-Baseline)/TIME_STUDY)  %>% filter(DIAG=="CB") %>%
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













# ALL  Left_Putamen Total Year 2

EarlyCT_Pop_Baseline_319 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Left_Putamen),
    sd=sd(Left_Putamen),
    median=median(Left_Putamen),
    Q1 = quantile(Left_Putamen, 0.25),
    Q3 = quantile(Left_Putamen, 0.75),
    min=min(Left_Putamen),
    max=max(Left_Putamen),
    n=n(),
    skewness = skewness(Left_Putamen, na.rm = TRUE),
    kurtosis = kurtosis(Left_Putamen, na.rm = TRUE),
    CI_lower = t.test(Left_Putamen)$conf.int[1],
    CI_upper = t.test(Left_Putamen)$conf.int[2],
    shapiro_w = shapiro.test(Left_Putamen)$statistic,
    shapiro_p = shapiro.test(Left_Putamen)$p.value
  )



# Elapsed Time Year 2


EarlyCT_Pop_Baseline_319 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen, DIAG, TIME_STUDY) %>%
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


EarlyCT_Pop_Baseline_319 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen) %>%
  rename("Baseline"="Left_Putamen") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen, DIAG)
  ) %>%
  mutate(Diff=(Left_Putamen-Baseline))  %>% filter(DIAG=="PD") %>%
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

EarlyCT_Pop_Baseline_319 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen) %>%
  rename("Baseline"="Left_Putamen") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen, DIAG, TIME_STUDY)
  ) %>%
  mutate(Diff=(Left_Putamen-Baseline)/TIME_STUDY)  %>% filter(DIAG=="PD") %>%
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






# ALL  Left_Putamen Total Year 3

EarlyCT_Pop_Baseline_319 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Left_Putamen),
    sd=sd(Left_Putamen),
    median=median(Left_Putamen),
    Q1 = quantile(Left_Putamen, 0.25),
    Q3 = quantile(Left_Putamen, 0.75),
    min=min(Left_Putamen),
    max=max(Left_Putamen),
    n=n(),
    skewness = skewness(Left_Putamen, na.rm = TRUE),
    kurtosis = kurtosis(Left_Putamen, na.rm = TRUE),
    CI_lower = t.test(Left_Putamen)$conf.int[1],
    CI_upper = t.test(Left_Putamen)$conf.int[2],
    shapiro_w = shapiro.test(Left_Putamen)$statistic,
    shapiro_p = shapiro.test(Left_Putamen)$p.value
  )



# Elapsed Time Year 3


EarlyCT_Pop_Baseline_319 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen, DIAG, TIME_STUDY) %>%
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


EarlyCT_Pop_Baseline_319 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen) %>%
  rename("Baseline"="Left_Putamen") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen, DIAG)
  ) %>%
  mutate(Diff=(Left_Putamen-Baseline))  %>% filter(DIAG=="PD") %>%
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

EarlyCT_Pop_Baseline_319 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen) %>%
  rename("Baseline"="Left_Putamen") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen, DIAG, TIME_STUDY)
  ) %>%
  mutate(Diff=(Left_Putamen-Baseline)/TIME_STUDY)  %>% filter(DIAG=="PD") %>%
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



# -----------
# Target MSA Entire  Probable vs Possible  -----------------------

# ALL  Left_Putamen Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Left_Putamen),
    sd=sd(Left_Putamen),
    median=median(Left_Putamen),
    Q1 = quantile(Left_Putamen, 0.25),
    Q3 = quantile(Left_Putamen, 0.75),
    min=min(Left_Putamen),
    max=max(Left_Putamen),
    n=n(),
    skewness = skewness(Left_Putamen, na.rm = TRUE),
    kurtosis = kurtosis(Left_Putamen, na.rm = TRUE),
    CI_lower = t.test(Left_Putamen)$conf.int[1],
    CI_upper = t.test(Left_Putamen)$conf.int[2],
    shapiro_w = shapiro.test(Left_Putamen)$statistic,
    shapiro_p = shapiro.test(Left_Putamen)$p.value
  )


# ALL  Left_Putamen Total Year 1

EarlyCT_Pop_Baseline_319 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Left_Putamen),
    sd=sd(Left_Putamen),
    median=median(Left_Putamen),
    Q1 = quantile(Left_Putamen, 0.25),
    Q3 = quantile(Left_Putamen, 0.75),
    min=min(Left_Putamen),
    max=max(Left_Putamen),
    n=n(),
    skewness = skewness(Left_Putamen, na.rm = TRUE),
    kurtosis = kurtosis(Left_Putamen, na.rm = TRUE),
    CI_lower = t.test(Left_Putamen)$conf.int[1],
    CI_upper = t.test(Left_Putamen)$conf.int[2],
    shapiro_w = shapiro.test(Left_Putamen)$statistic,
    shapiro_p = shapiro.test(Left_Putamen)$p.value
  )



# Elapsed Time Year 1


EarlyCT_Pop_Baseline_319 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen, DIAGNIV, TIME_STUDY) %>%
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


EarlyCT_Pop_Baseline_319 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen) %>%
  rename("Baseline"="Left_Putamen") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen, DIAGNIV)
  ) %>%
  mutate(Diff=(Left_Putamen-Baseline))  %>% filter(DIAGNIV=="PROB") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
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

EarlyCT_Pop_Baseline_319 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen) %>%
  rename("Baseline"="Left_Putamen") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen, DIAGNIV, TIME_STUDY)
  ) %>%
  mutate(Diff=(Left_Putamen-Baseline)/TIME_STUDY)  %>% filter(DIAGNIV=="PROB") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )













# ALL  Left_Putamen Total Year 2

EarlyCT_Pop_Baseline_319 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% filter(DIAGNIV=="PROB") %>%
  summarise(
    mean=mean(Left_Putamen),
    sd=sd(Left_Putamen),
    median=median(Left_Putamen),
    Q1 = quantile(Left_Putamen, 0.25),
    Q3 = quantile(Left_Putamen, 0.75),
    min=min(Left_Putamen),
    max=max(Left_Putamen),
    n=n(),
    skewness = skewness(Left_Putamen, na.rm = TRUE),
    kurtosis = kurtosis(Left_Putamen, na.rm = TRUE),
    CI_lower = t.test(Left_Putamen)$conf.int[1],
    CI_upper = t.test(Left_Putamen)$conf.int[2],
    shapiro_w = shapiro.test(Left_Putamen)$statistic,
    shapiro_p = shapiro.test(Left_Putamen)$p.value
  )



# Elapsed Time Year 2


EarlyCT_Pop_Baseline_319 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen, DIAGNIV, TIME_STUDY) %>%
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


EarlyCT_Pop_Baseline_319 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen) %>%
  rename("Baseline"="Left_Putamen") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen, DIAGNIV)
  ) %>%
  mutate(Diff=(Left_Putamen-Baseline))  %>% filter(DIAGNIV=="PROB") %>%
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

EarlyCT_Pop_Baseline_319 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen) %>%
  rename("Baseline"="Left_Putamen") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen, DIAGNIV, TIME_STUDY)
  ) %>%
  mutate(Diff=(Left_Putamen-Baseline)/TIME_STUDY)  %>% filter(DIAGNIV=="PROB") %>%
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






# ALL  Left_Putamen Total Year 3

EarlyCT_Pop_Baseline_319 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% filter(DIAGNIV=="POS") %>%
  summarise(
    mean=mean(Left_Putamen),
    sd=sd(Left_Putamen),
    median=median(Left_Putamen),
    Q1 = quantile(Left_Putamen, 0.25),
    Q3 = quantile(Left_Putamen, 0.75),
    min=min(Left_Putamen),
    max=max(Left_Putamen),
    n=n(),
    #skewness = skewness(Left_Putamen, na.rm = TRUE),
    #kurtosis = kurtosis(Left_Putamen, na.rm = TRUE),
    #CI_lower = t.test(Left_Putamen)$conf.int[1],
    #CI_upper = t.test(Left_Putamen)$conf.int[2],
    #shapiro_w = shapiro.test(Left_Putamen)$statistic,
    #shapiro_p = shapiro.test(Left_Putamen)$p.value
  )



# Elapsed Time Year 3


EarlyCT_Pop_Baseline_319 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen, DIAGNIV, TIME_STUDY) %>%
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


# Change from baseline Year 3


EarlyCT_Pop_Baseline_319 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen) %>%
  rename("Baseline"="Left_Putamen") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen, DIAGNIV)
  ) %>%
  mutate(Diff=(Left_Putamen-Baseline))  %>% filter(DIAGNIV=="PROB") %>%
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

EarlyCT_Pop_Baseline_319 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen) %>%
  rename("Baseline"="Left_Putamen") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen, DIAGNIV, TIME_STUDY)
  ) %>%
  mutate(Diff=(Left_Putamen-Baseline)/TIME_STUDY)  %>% filter(DIAGNIV=="PROB") %>%
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

# ---------------


# All data dates groups  --------
dataCohorteManaged <- readRDS("Source/dataCohorteManaged.rds")

Visits_zero <- dataCohorteManaged  %>% select(NUM, dateVisite0) %>% distinct()

AllMSA_Pop_Baseline_671 <- fread("Source/AllMSA_Pop_Baseline_671.txt")
EarlyCT_Pop_Baseline_319 <- fread("Source/EarlyCT_Pop_Baseline_319.txt")


# ----------
# Right Putamen Volume -----------------------------

irmForPaulo <- readRDS("Source/irmForPaulo.rds")

Right_Putamen_VOL <- irmForPaulo %>% select(NUM, DateIRM, Right_Putamen)

Right_Putamen_VOL <- Visits_zero %>% inner_join(Right_Putamen_VOL) %>%
  mutate(TIME_STUDY=time_length(difftime(DateIRM, dateVisite0 ), "years")) %>%
  mutate(Year= ifelse(TIME_STUDY<=0.5, 0,
                      ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                             ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                    ifelse(TIME_STUDY>=2.5 ,3, NA))))) 

Right_Putamen_VOL <- Right_Putamen_VOL %>% 
  drop_na() %>% arrange(NUM, DateIRM) %>%
  group_by(NUM, DateIRM) %>% mutate(Right_Putamen=mean(Right_Putamen)) %>%
  distinct()

# ----------
# Overall MSA Entire  -----------------------

# ALL  Right_Putamen Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(Right_Putamen),
    sd=sd(Right_Putamen),
    median=median(Right_Putamen),
    Q1 = quantile(Right_Putamen, 0.25),
    Q3 = quantile(Right_Putamen, 0.75),
    min=min(Right_Putamen),
    max=max(Right_Putamen),
    n=n(),
    skewness = skewness(Right_Putamen, na.rm = TRUE),
    kurtosis = kurtosis(Right_Putamen, na.rm = TRUE),
    CI_lower = t.test(Right_Putamen)$conf.int[1],
    CI_upper = t.test(Right_Putamen)$conf.int[2],
    shapiro_w = shapiro.test(Right_Putamen)$statistic,
    shapiro_p = shapiro.test(Right_Putamen)$p.value
  )


# ALL  Right_Putamen Total Year 1

AllMSA_Pop_Baseline_671 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(Right_Putamen),
    sd=sd(Right_Putamen),
    median=median(Right_Putamen),
    Q1 = quantile(Right_Putamen, 0.25),
    Q3 = quantile(Right_Putamen, 0.75),
    min=min(Right_Putamen),
    max=max(Right_Putamen),
    n=n(),
    skewness = skewness(Right_Putamen, na.rm = TRUE),
    kurtosis = kurtosis(Right_Putamen, na.rm = TRUE),
    CI_lower = t.test(Right_Putamen)$conf.int[1],
    CI_upper = t.test(Right_Putamen)$conf.int[2],
    shapiro_w = shapiro.test(Right_Putamen)$statistic,
    shapiro_p = shapiro.test(Right_Putamen)$p.value
  )



# Elapsed Time Year 1


AllMSA_Pop_Baseline_671 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen, TIME_STUDY) %>%
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


AllMSA_Pop_Baseline_671 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen) %>%
  rename("Baseline"="Right_Putamen") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen)
  ) %>%
  mutate(Diff=(Right_Putamen-Baseline))  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
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

AllMSA_Pop_Baseline_671 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen) %>%
  rename("Baseline"="Right_Putamen") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen, TIME_STUDY)
  ) %>%
  mutate(Diff=(Right_Putamen-Baseline)/TIME_STUDY)  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )













# ALL  Right_Putamen Total Year 2

AllMSA_Pop_Baseline_671 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(Right_Putamen),
    sd=sd(Right_Putamen),
    median=median(Right_Putamen),
    Q1 = quantile(Right_Putamen, 0.25),
    Q3 = quantile(Right_Putamen, 0.75),
    min=min(Right_Putamen),
    max=max(Right_Putamen),
    n=n(),
    skewness = skewness(Right_Putamen, na.rm = TRUE),
    kurtosis = kurtosis(Right_Putamen, na.rm = TRUE),
    CI_lower = t.test(Right_Putamen)$conf.int[1],
    CI_upper = t.test(Right_Putamen)$conf.int[2],
    shapiro_w = shapiro.test(Right_Putamen)$statistic,
    shapiro_p = shapiro.test(Right_Putamen)$p.value
  )



# Elapsed Time Year 2


AllMSA_Pop_Baseline_671 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen, TIME_STUDY) %>%
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


AllMSA_Pop_Baseline_671 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen) %>%
  rename("Baseline"="Right_Putamen") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen)
  ) %>%
  mutate(Diff=(Right_Putamen-Baseline))  %>%
  summarise(
    mean=mean(Diff),
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

AllMSA_Pop_Baseline_671 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen) %>%
  rename("Baseline"="Right_Putamen") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen, TIME_STUDY)
  ) %>%
  mutate(Diff=(Right_Putamen-Baseline)/TIME_STUDY)  %>%
  summarise(
    mean=mean(Diff),
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






# ALL  Right_Putamen Total Year 3

AllMSA_Pop_Baseline_671 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(Right_Putamen),
    sd=sd(Right_Putamen),
    median=median(Right_Putamen),
    Q1 = quantile(Right_Putamen, 0.25),
    Q3 = quantile(Right_Putamen, 0.75),
    min=min(Right_Putamen),
    max=max(Right_Putamen),
    n=n(),
    skewness = skewness(Right_Putamen, na.rm = TRUE),
    kurtosis = kurtosis(Right_Putamen, na.rm = TRUE),
    CI_lower = t.test(Right_Putamen)$conf.int[1],
    CI_upper = t.test(Right_Putamen)$conf.int[2],
    shapiro_w = shapiro.test(Right_Putamen)$statistic,
    shapiro_p = shapiro.test(Right_Putamen)$p.value
  )



# Elapsed Time Year 3


AllMSA_Pop_Baseline_671 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen, TIME_STUDY) %>%
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


AllMSA_Pop_Baseline_671 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen) %>%
  rename("Baseline"="Right_Putamen") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen)
  ) %>%
  mutate(Diff=(Right_Putamen-Baseline))  %>%
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

AllMSA_Pop_Baseline_671 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen) %>%
  rename("Baseline"="Right_Putamen") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen, TIME_STUDY)
  ) %>%
  mutate(Diff=(Right_Putamen-Baseline)/TIME_STUDY)  %>%
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



# -----------
# Overall MSA Entire  MSA P vs MSA C  -----------------------

# ALL  Right_Putamen Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Right_Putamen),
    sd=sd(Right_Putamen),
    median=median(Right_Putamen),
    Q1 = quantile(Right_Putamen, 0.25),
    Q3 = quantile(Right_Putamen, 0.75),
    min=min(Right_Putamen),
    max=max(Right_Putamen),
    n=n(),
    skewness = skewness(Right_Putamen, na.rm = TRUE),
    kurtosis = kurtosis(Right_Putamen, na.rm = TRUE),
    CI_lower = t.test(Right_Putamen)$conf.int[1],
    CI_upper = t.test(Right_Putamen)$conf.int[2],
    shapiro_w = shapiro.test(Right_Putamen)$statistic,
    shapiro_p = shapiro.test(Right_Putamen)$p.value
  )


# ALL  Right_Putamen Total Year 1

AllMSA_Pop_Baseline_671 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Right_Putamen),
    sd=sd(Right_Putamen),
    median=median(Right_Putamen),
    Q1 = quantile(Right_Putamen, 0.25),
    Q3 = quantile(Right_Putamen, 0.75),
    min=min(Right_Putamen),
    max=max(Right_Putamen),
    n=n(),
    skewness = skewness(Right_Putamen, na.rm = TRUE),
    kurtosis = kurtosis(Right_Putamen, na.rm = TRUE),
    CI_lower = t.test(Right_Putamen)$conf.int[1],
    CI_upper = t.test(Right_Putamen)$conf.int[2],
    shapiro_w = shapiro.test(Right_Putamen)$statistic,
    shapiro_p = shapiro.test(Right_Putamen)$p.value
  )



# Elapsed Time Year 1


AllMSA_Pop_Baseline_671 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen, DIAG, TIME_STUDY) %>%
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


AllMSA_Pop_Baseline_671 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen) %>%
  rename("Baseline"="Right_Putamen") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen, DIAG)
  ) %>%
  mutate(Diff=(Right_Putamen-Baseline))  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
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

AllMSA_Pop_Baseline_671 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen) %>%
  rename("Baseline"="Right_Putamen") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen, DIAG, TIME_STUDY)
  ) %>%
  mutate(Diff=(Right_Putamen-Baseline)/TIME_STUDY)  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )













# ALL  Right_Putamen Total Year 2

AllMSA_Pop_Baseline_671 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Right_Putamen),
    sd=sd(Right_Putamen),
    median=median(Right_Putamen),
    Q1 = quantile(Right_Putamen, 0.25),
    Q3 = quantile(Right_Putamen, 0.75),
    min=min(Right_Putamen),
    max=max(Right_Putamen),
    n=n(),
    skewness = skewness(Right_Putamen, na.rm = TRUE),
    kurtosis = kurtosis(Right_Putamen, na.rm = TRUE),
    CI_lower = t.test(Right_Putamen)$conf.int[1],
    CI_upper = t.test(Right_Putamen)$conf.int[2],
    shapiro_w = shapiro.test(Right_Putamen)$statistic,
    shapiro_p = shapiro.test(Right_Putamen)$p.value
  )



# Elapsed Time Year 2


AllMSA_Pop_Baseline_671 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen, DIAG, TIME_STUDY) %>%
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


AllMSA_Pop_Baseline_671 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen) %>%
  rename("Baseline"="Right_Putamen") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen, DIAG)
  ) %>%
  mutate(Diff=(Right_Putamen-Baseline))  %>% group_by(DIAG) %>%
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

AllMSA_Pop_Baseline_671 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen) %>%
  rename("Baseline"="Right_Putamen") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen, DIAG, TIME_STUDY)
  ) %>%
  mutate(Diff=(Right_Putamen-Baseline)/TIME_STUDY)  %>% filter(DIAG=="PD") %>%
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






# ALL  Right_Putamen Total Year 3

AllMSA_Pop_Baseline_671 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Right_Putamen),
    sd=sd(Right_Putamen),
    median=median(Right_Putamen),
    Q1 = quantile(Right_Putamen, 0.25),
    Q3 = quantile(Right_Putamen, 0.75),
    min=min(Right_Putamen),
    max=max(Right_Putamen),
    n=n(),
    skewness = skewness(Right_Putamen, na.rm = TRUE),
    kurtosis = kurtosis(Right_Putamen, na.rm = TRUE),
    CI_lower = t.test(Right_Putamen)$conf.int[1],
    CI_upper = t.test(Right_Putamen)$conf.int[2],
    shapiro_w = shapiro.test(Right_Putamen)$statistic,
    shapiro_p = shapiro.test(Right_Putamen)$p.value
  )



# Elapsed Time Year 3


AllMSA_Pop_Baseline_671 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen, DIAG, TIME_STUDY) %>%
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


AllMSA_Pop_Baseline_671 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen) %>%
  rename("Baseline"="Right_Putamen") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen, DIAG)
  ) %>%
  mutate(Diff=(Right_Putamen-Baseline))  %>% filter(DIAG=="PD") %>%
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

AllMSA_Pop_Baseline_671 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen) %>%
  rename("Baseline"="Right_Putamen") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen, DIAG, TIME_STUDY)
  ) %>%
  mutate(Diff=(Right_Putamen-Baseline)/TIME_STUDY)  %>% filter(DIAG=="PD") %>%
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



# -----------
# Overall MSA Entire  Probable vs Possible  -----------------------

# ALL  Right_Putamen Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Right_Putamen),
    sd=sd(Right_Putamen),
    median=median(Right_Putamen),
    Q1 = quantile(Right_Putamen, 0.25),
    Q3 = quantile(Right_Putamen, 0.75),
    min=min(Right_Putamen),
    max=max(Right_Putamen),
    n=n(),
    skewness = skewness(Right_Putamen, na.rm = TRUE),
    kurtosis = kurtosis(Right_Putamen, na.rm = TRUE),
    CI_lower = t.test(Right_Putamen)$conf.int[1],
    CI_upper = t.test(Right_Putamen)$conf.int[2],
    shapiro_w = shapiro.test(Right_Putamen)$statistic,
    shapiro_p = shapiro.test(Right_Putamen)$p.value
  )


# ALL  Right_Putamen Total Year 1

AllMSA_Pop_Baseline_671 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Right_Putamen),
    sd=sd(Right_Putamen),
    median=median(Right_Putamen),
    Q1 = quantile(Right_Putamen, 0.25),
    Q3 = quantile(Right_Putamen, 0.75),
    min=min(Right_Putamen),
    max=max(Right_Putamen),
    n=n(),
    skewness = skewness(Right_Putamen, na.rm = TRUE),
    kurtosis = kurtosis(Right_Putamen, na.rm = TRUE),
    CI_lower = t.test(Right_Putamen)$conf.int[1],
    CI_upper = t.test(Right_Putamen)$conf.int[2],
    shapiro_w = shapiro.test(Right_Putamen)$statistic,
    shapiro_p = shapiro.test(Right_Putamen)$p.value
  )



# Elapsed Time Year 1


AllMSA_Pop_Baseline_671 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen, DIAGNIV, TIME_STUDY) %>%
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


AllMSA_Pop_Baseline_671 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen) %>%
  rename("Baseline"="Right_Putamen") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen, DIAGNIV)
  ) %>%
  mutate(Diff=(Right_Putamen-Baseline))  %>% filter(DIAGNIV=="PROB") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
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

AllMSA_Pop_Baseline_671 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen) %>%
  rename("Baseline"="Right_Putamen") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen, DIAGNIV, TIME_STUDY)
  ) %>%
  mutate(Diff=(Right_Putamen-Baseline)/TIME_STUDY)  %>% filter(DIAGNIV=="PROB") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )













# ALL  Right_Putamen Total Year 2

AllMSA_Pop_Baseline_671 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Right_Putamen),
    sd=sd(Right_Putamen),
    median=median(Right_Putamen),
    Q1 = quantile(Right_Putamen, 0.25),
    Q3 = quantile(Right_Putamen, 0.75),
    min=min(Right_Putamen),
    max=max(Right_Putamen),
    n=n(),
    skewness = skewness(Right_Putamen, na.rm = TRUE),
    kurtosis = kurtosis(Right_Putamen, na.rm = TRUE),
    CI_lower = t.test(Right_Putamen)$conf.int[1],
    CI_upper = t.test(Right_Putamen)$conf.int[2],
    shapiro_w = shapiro.test(Right_Putamen)$statistic,
    shapiro_p = shapiro.test(Right_Putamen)$p.value
  )



# Elapsed Time Year 2


AllMSA_Pop_Baseline_671 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen, DIAGNIV, TIME_STUDY) %>%
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


AllMSA_Pop_Baseline_671 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen) %>%
  rename("Baseline"="Right_Putamen") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen, DIAGNIV)
  ) %>%
  mutate(Diff=(Right_Putamen-Baseline))  %>% filter(DIAGNIV=="PROB") %>%
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

AllMSA_Pop_Baseline_671 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen) %>%
  rename("Baseline"="Right_Putamen") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen, DIAGNIV, TIME_STUDY)
  ) %>%
  mutate(Diff=(Right_Putamen-Baseline)/TIME_STUDY)  %>% filter(DIAGNIV=="PROB") %>%
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






# ALL  Right_Putamen Total Year 3

AllMSA_Pop_Baseline_671 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Right_Putamen),
    sd=sd(Right_Putamen),
    median=median(Right_Putamen),
    Q1 = quantile(Right_Putamen, 0.25),
    Q3 = quantile(Right_Putamen, 0.75),
    min=min(Right_Putamen),
    max=max(Right_Putamen),
    n=n(),
    skewness = skewness(Right_Putamen, na.rm = TRUE),
    kurtosis = kurtosis(Right_Putamen, na.rm = TRUE),
    CI_lower = t.test(Right_Putamen)$conf.int[1],
    CI_upper = t.test(Right_Putamen)$conf.int[2],
    shapiro_w = shapiro.test(Right_Putamen)$statistic,
    shapiro_p = shapiro.test(Right_Putamen)$p.value
  )



# Elapsed Time Year 3


AllMSA_Pop_Baseline_671 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen, DIAGNIV, TIME_STUDY) %>%
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


AllMSA_Pop_Baseline_671 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen) %>%
  rename("Baseline"="Right_Putamen") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen, DIAGNIV)
  ) %>%
  mutate(Diff=(Right_Putamen-Baseline))  %>% filter(DIAGNIV=="PROB") %>%
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

AllMSA_Pop_Baseline_671 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen) %>%
  rename("Baseline"="Right_Putamen") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen, DIAGNIV, TIME_STUDY)
  ) %>%
  mutate(Diff=(Right_Putamen-Baseline)/TIME_STUDY)  %>% filter(DIAGNIV=="PROB") %>%
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

# ---------------


# Target MSA Entire  -----------------------

# ALL  Right_Putamen Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(Right_Putamen),
    sd=sd(Right_Putamen),
    median=median(Right_Putamen),
    Q1 = quantile(Right_Putamen, 0.25),
    Q3 = quantile(Right_Putamen, 0.75),
    min=min(Right_Putamen),
    max=max(Right_Putamen),
    n=n(),
    skewness = skewness(Right_Putamen, na.rm = TRUE),
    kurtosis = kurtosis(Right_Putamen, na.rm = TRUE),
    CI_lower = t.test(Right_Putamen)$conf.int[1],
    CI_upper = t.test(Right_Putamen)$conf.int[2],
    shapiro_w = shapiro.test(Right_Putamen)$statistic,
    shapiro_p = shapiro.test(Right_Putamen)$p.value
  )


# ALL  Right_Putamen Total Year 1

EarlyCT_Pop_Baseline_319 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(Right_Putamen),
    sd=sd(Right_Putamen),
    median=median(Right_Putamen),
    Q1 = quantile(Right_Putamen, 0.25),
    Q3 = quantile(Right_Putamen, 0.75),
    min=min(Right_Putamen),
    max=max(Right_Putamen),
    n=n(),
    skewness = skewness(Right_Putamen, na.rm = TRUE),
    kurtosis = kurtosis(Right_Putamen, na.rm = TRUE),
    CI_lower = t.test(Right_Putamen)$conf.int[1],
    CI_upper = t.test(Right_Putamen)$conf.int[2],
    shapiro_w = shapiro.test(Right_Putamen)$statistic,
    shapiro_p = shapiro.test(Right_Putamen)$p.value
  )



# Elapsed Time Year 1


EarlyCT_Pop_Baseline_319 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen, TIME_STUDY) %>%
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


EarlyCT_Pop_Baseline_319 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen) %>%
  rename("Baseline"="Right_Putamen") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen)
  ) %>%
  mutate(Diff=(Right_Putamen-Baseline))  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
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

EarlyCT_Pop_Baseline_319 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen) %>%
  rename("Baseline"="Right_Putamen") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen, TIME_STUDY)
  ) %>%
  mutate(Diff=(Right_Putamen-Baseline)/TIME_STUDY)  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )













# ALL  Right_Putamen Total Year 2

EarlyCT_Pop_Baseline_319 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(Right_Putamen),
    sd=sd(Right_Putamen),
    median=median(Right_Putamen),
    Q1 = quantile(Right_Putamen, 0.25),
    Q3 = quantile(Right_Putamen, 0.75),
    min=min(Right_Putamen),
    max=max(Right_Putamen),
    n=n(),
    skewness = skewness(Right_Putamen, na.rm = TRUE),
    kurtosis = kurtosis(Right_Putamen, na.rm = TRUE),
    CI_lower = t.test(Right_Putamen)$conf.int[1],
    CI_upper = t.test(Right_Putamen)$conf.int[2],
    shapiro_w = shapiro.test(Right_Putamen)$statistic,
    shapiro_p = shapiro.test(Right_Putamen)$p.value
  )



# Elapsed Time Year 2


EarlyCT_Pop_Baseline_319 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen, TIME_STUDY) %>%
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


EarlyCT_Pop_Baseline_319 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen) %>%
  rename("Baseline"="Right_Putamen") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen)
  ) %>%
  mutate(Diff=(Right_Putamen-Baseline))  %>%
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

EarlyCT_Pop_Baseline_319 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen) %>%
  rename("Baseline"="Right_Putamen") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen, TIME_STUDY)
  ) %>%
  mutate(Diff=(Right_Putamen-Baseline)/TIME_STUDY)  %>%
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






# ALL  Right_Putamen Total Year 3

EarlyCT_Pop_Baseline_319 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(Right_Putamen),
    sd=sd(Right_Putamen),
    median=median(Right_Putamen),
    Q1 = quantile(Right_Putamen, 0.25),
    Q3 = quantile(Right_Putamen, 0.75),
    min=min(Right_Putamen),
    max=max(Right_Putamen),
    n=n(),
    skewness = skewness(Right_Putamen, na.rm = TRUE),
    kurtosis = kurtosis(Right_Putamen, na.rm = TRUE),
    CI_lower = t.test(Right_Putamen)$conf.int[1],
    CI_upper = t.test(Right_Putamen)$conf.int[2],
    shapiro_w = shapiro.test(Right_Putamen)$statistic,
    shapiro_p = shapiro.test(Right_Putamen)$p.value
  )



# Elapsed Time Year 3


EarlyCT_Pop_Baseline_319 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen, TIME_STUDY) %>%
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


EarlyCT_Pop_Baseline_319 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen) %>%
  rename("Baseline"="Right_Putamen") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen)
  ) %>%
  mutate(Diff=(Right_Putamen-Baseline))  %>%
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

EarlyCT_Pop_Baseline_319 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen) %>%
  rename("Baseline"="Right_Putamen") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen, TIME_STUDY)
  ) %>%
  mutate(Diff=(Right_Putamen-Baseline)/TIME_STUDY)  %>%
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



# -----------
# Target MSA Entire  MSA P vs MSA C  -----------------------

# ALL  Right_Putamen Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Right_Putamen),
    sd=sd(Right_Putamen),
    median=median(Right_Putamen),
    Q1 = quantile(Right_Putamen, 0.25),
    Q3 = quantile(Right_Putamen, 0.75),
    min=min(Right_Putamen),
    max=max(Right_Putamen),
    n=n(),
    skewness = skewness(Right_Putamen, na.rm = TRUE),
    kurtosis = kurtosis(Right_Putamen, na.rm = TRUE),
    CI_lower = t.test(Right_Putamen)$conf.int[1],
    CI_upper = t.test(Right_Putamen)$conf.int[2],
    shapiro_w = shapiro.test(Right_Putamen)$statistic,
    shapiro_p = shapiro.test(Right_Putamen)$p.value
  )


# ALL  Right_Putamen Total Year 1

EarlyCT_Pop_Baseline_319 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Right_Putamen),
    sd=sd(Right_Putamen),
    median=median(Right_Putamen),
    Q1 = quantile(Right_Putamen, 0.25),
    Q3 = quantile(Right_Putamen, 0.75),
    min=min(Right_Putamen),
    max=max(Right_Putamen),
    n=n(),
    skewness = skewness(Right_Putamen, na.rm = TRUE),
    kurtosis = kurtosis(Right_Putamen, na.rm = TRUE),
    CI_lower = t.test(Right_Putamen)$conf.int[1],
    CI_upper = t.test(Right_Putamen)$conf.int[2],
    shapiro_w = shapiro.test(Right_Putamen)$statistic,
    shapiro_p = shapiro.test(Right_Putamen)$p.value
  )



# Elapsed Time Year 1


EarlyCT_Pop_Baseline_319 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen, DIAG, TIME_STUDY) %>%
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


EarlyCT_Pop_Baseline_319 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen) %>%
  rename("Baseline"="Right_Putamen") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen, DIAG)
  ) %>%
  mutate(Diff=(Right_Putamen-Baseline))  %>% filter(DIAG=="PD") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
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

EarlyCT_Pop_Baseline_319 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen) %>%
  rename("Baseline"="Right_Putamen") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen, DIAG, TIME_STUDY)
  ) %>%
  mutate(Diff=(Right_Putamen-Baseline)/TIME_STUDY)  %>% filter(DIAG=="PD") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )













# ALL  Right_Putamen Total Year 2

EarlyCT_Pop_Baseline_319 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Right_Putamen),
    sd=sd(Right_Putamen),
    median=median(Right_Putamen),
    Q1 = quantile(Right_Putamen, 0.25),
    Q3 = quantile(Right_Putamen, 0.75),
    min=min(Right_Putamen),
    max=max(Right_Putamen),
    n=n(),
    skewness = skewness(Right_Putamen, na.rm = TRUE),
    kurtosis = kurtosis(Right_Putamen, na.rm = TRUE),
    CI_lower = t.test(Right_Putamen)$conf.int[1],
    CI_upper = t.test(Right_Putamen)$conf.int[2],
    shapiro_w = shapiro.test(Right_Putamen)$statistic,
    shapiro_p = shapiro.test(Right_Putamen)$p.value
  )



# Elapsed Time Year 2


EarlyCT_Pop_Baseline_319 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen, DIAG, TIME_STUDY) %>%
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


EarlyCT_Pop_Baseline_319 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen) %>%
  rename("Baseline"="Right_Putamen") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen, DIAG)
  ) %>%
  mutate(Diff=(Right_Putamen-Baseline))  %>% filter(DIAG=="PD") %>%
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

EarlyCT_Pop_Baseline_319 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen) %>%
  rename("Baseline"="Right_Putamen") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen, DIAG, TIME_STUDY)
  ) %>%
  mutate(Diff=(Right_Putamen-Baseline)/TIME_STUDY)  %>% filter(DIAG=="PD") %>%
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






# ALL  Right_Putamen Total Year 3

EarlyCT_Pop_Baseline_319 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Right_Putamen),
    sd=sd(Right_Putamen),
    median=median(Right_Putamen),
    Q1 = quantile(Right_Putamen, 0.25),
    Q3 = quantile(Right_Putamen, 0.75),
    min=min(Right_Putamen),
    max=max(Right_Putamen),
    n=n(),
    skewness = skewness(Right_Putamen, na.rm = TRUE),
    kurtosis = kurtosis(Right_Putamen, na.rm = TRUE),
    CI_lower = t.test(Right_Putamen)$conf.int[1],
    CI_upper = t.test(Right_Putamen)$conf.int[2],
    shapiro_w = shapiro.test(Right_Putamen)$statistic,
    shapiro_p = shapiro.test(Right_Putamen)$p.value
  )



# Elapsed Time Year 3


EarlyCT_Pop_Baseline_319 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen, DIAG, TIME_STUDY) %>%
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


EarlyCT_Pop_Baseline_319 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen) %>%
  rename("Baseline"="Right_Putamen") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen, DIAG)
  ) %>%
  mutate(Diff=(Right_Putamen-Baseline))  %>% filter(DIAG=="PD") %>%
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

EarlyCT_Pop_Baseline_319 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen) %>%
  rename("Baseline"="Right_Putamen") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen, DIAG, TIME_STUDY)
  ) %>%
  mutate(Diff=(Right_Putamen-Baseline)/TIME_STUDY)  %>% filter(DIAG=="PD") %>%
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



# -----------
# Target MSA Entire  Probable vs Possible  -----------------------

# ALL  Right_Putamen Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Right_Putamen),
    sd=sd(Right_Putamen),
    median=median(Right_Putamen),
    Q1 = quantile(Right_Putamen, 0.25),
    Q3 = quantile(Right_Putamen, 0.75),
    min=min(Right_Putamen),
    max=max(Right_Putamen),
    n=n(),
    skewness = skewness(Right_Putamen, na.rm = TRUE),
    kurtosis = kurtosis(Right_Putamen, na.rm = TRUE),
    CI_lower = t.test(Right_Putamen)$conf.int[1],
    CI_upper = t.test(Right_Putamen)$conf.int[2],
    shapiro_w = shapiro.test(Right_Putamen)$statistic,
    shapiro_p = shapiro.test(Right_Putamen)$p.value
  )


# ALL  Right_Putamen Total Year 1

EarlyCT_Pop_Baseline_319 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Right_Putamen),
    sd=sd(Right_Putamen),
    median=median(Right_Putamen),
    Q1 = quantile(Right_Putamen, 0.25),
    Q3 = quantile(Right_Putamen, 0.75),
    min=min(Right_Putamen),
    max=max(Right_Putamen),
    n=n(),
    skewness = skewness(Right_Putamen, na.rm = TRUE),
    kurtosis = kurtosis(Right_Putamen, na.rm = TRUE),
    CI_lower = t.test(Right_Putamen)$conf.int[1],
    CI_upper = t.test(Right_Putamen)$conf.int[2],
    shapiro_w = shapiro.test(Right_Putamen)$statistic,
    shapiro_p = shapiro.test(Right_Putamen)$p.value
  )



# Elapsed Time Year 1


EarlyCT_Pop_Baseline_319 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen, DIAGNIV, TIME_STUDY) %>%
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


EarlyCT_Pop_Baseline_319 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen) %>%
  rename("Baseline"="Right_Putamen") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen, DIAGNIV)
  ) %>%
  mutate(Diff=(Right_Putamen-Baseline))  %>% filter(DIAGNIV=="POS") %>%
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

EarlyCT_Pop_Baseline_319 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen) %>%
  rename("Baseline"="Right_Putamen") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen, DIAGNIV, TIME_STUDY)
  ) %>%
  mutate(Diff=(Right_Putamen-Baseline)/TIME_STUDY)  %>% filter(DIAGNIV=="POS") %>%
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













# ALL  Right_Putamen Total Year 2

EarlyCT_Pop_Baseline_319 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% filter(DIAGNIV=="POS") %>%
  summarise(
    mean=mean(Right_Putamen),
    sd=sd(Right_Putamen),
    median=median(Right_Putamen),
    Q1 = quantile(Right_Putamen, 0.25),
    Q3 = quantile(Right_Putamen, 0.75),
    min=min(Right_Putamen),
    max=max(Right_Putamen),
    n=n(),
    #skewness = skewness(Right_Putamen, na.rm = TRUE),
    #kurtosis = kurtosis(Right_Putamen, na.rm = TRUE),
    #CI_lower = t.test(Right_Putamen)$conf.int[1],
    #CI_upper = t.test(Right_Putamen)$conf.int[2],
    #shapiro_w = shapiro.test(Right_Putamen)$statistic,
    #shapiro_p = shapiro.test(Right_Putamen)$p.value
  )



# Elapsed Time Year 2


EarlyCT_Pop_Baseline_319 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen, DIAGNIV, TIME_STUDY) %>%
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


EarlyCT_Pop_Baseline_319 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen) %>%
  rename("Baseline"="Right_Putamen") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen, DIAGNIV)
  ) %>%
  mutate(Diff=(Right_Putamen-Baseline))  %>% filter(DIAGNIV=="PROB") %>%
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

EarlyCT_Pop_Baseline_319 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen) %>%
  rename("Baseline"="Right_Putamen") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen, DIAGNIV, TIME_STUDY)
  ) %>%
  mutate(Diff=(Right_Putamen-Baseline)/TIME_STUDY)  %>% filter(DIAGNIV=="PROB") %>%
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






# ALL  Right_Putamen Total Year 3

EarlyCT_Pop_Baseline_319 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% filter(DIAGNIV=="PROB") %>%
  summarise(
    mean=mean(Right_Putamen),
    sd=sd(Right_Putamen),
    median=median(Right_Putamen),
    Q1 = quantile(Right_Putamen, 0.25),
    Q3 = quantile(Right_Putamen, 0.75),
    min=min(Right_Putamen),
    max=max(Right_Putamen),
    n=n(),
    skewness = skewness(Right_Putamen, na.rm = TRUE),
    kurtosis = kurtosis(Right_Putamen, na.rm = TRUE),
    CI_lower = t.test(Right_Putamen)$conf.int[1],
    CI_upper = t.test(Right_Putamen)$conf.int[2],
    shapiro_w = shapiro.test(Right_Putamen)$statistic,
    shapiro_p = shapiro.test(Right_Putamen)$p.value
  )



# Elapsed Time Year 3


EarlyCT_Pop_Baseline_319 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen, DIAGNIV, TIME_STUDY) %>%
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


# Change from baseline Year 3


EarlyCT_Pop_Baseline_319 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen) %>%
  rename("Baseline"="Right_Putamen") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen, DIAGNIV)
  ) %>%
  mutate(Diff=(Right_Putamen-Baseline))  %>% filter(DIAGNIV=="PROB") %>%
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

EarlyCT_Pop_Baseline_319 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen) %>%
  rename("Baseline"="Right_Putamen") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen, DIAGNIV, TIME_STUDY)
  ) %>%
  mutate(Diff=(Right_Putamen-Baseline)/TIME_STUDY)  %>% filter(DIAGNIV=="PROB") %>%
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

# ---------------


# All data dates groups  --------
dataCohorteManaged <- readRDS("Source/dataCohorteManaged.rds")

Visits_zero <- dataCohorteManaged  %>% select(NUM, dateVisite0) %>% distinct()

AllMSA_Pop_Baseline_671 <- fread("Source/AllMSA_Pop_Baseline_671.txt")
EarlyCT_Pop_Baseline_319 <- fread("Source/EarlyCT_Pop_Baseline_319.txt")


# ----------
# Cerebellum Volume -----------------------------

irmForPaulo <- readRDS("Source/irmForPaulo.rds")

Cerebellum_VOL <- irmForPaulo %>% select(NUM, DateIRM, Left_Cerebellum_White_Matter,Right_Cerebellum_White_Matter,
                                         Left_Cerebellum_Cortex, Right_Cerebellum_Cortex)

Cerebellum_VOL <- Cerebellum_VOL %>% mutate(Cerebellum=Left_Cerebellum_White_Matter+Right_Cerebellum_White_Matter++Left_Cerebellum_Cortex+Right_Cerebellum_Cortex)

Cerebellum_VOL <- Visits_zero %>% inner_join(Cerebellum_VOL) %>%
  mutate(TIME_STUDY=time_length(difftime(DateIRM, dateVisite0 ), "years")) %>%
  mutate(Year= ifelse(TIME_STUDY<=0.5, 0,
                      ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                             ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                    ifelse(TIME_STUDY>=2.5 ,3, NA))))) 

Cerebellum_VOL <- Cerebellum_VOL %>% 
  drop_na() %>% arrange(NUM, DateIRM) %>%
  group_by(NUM, DateIRM) %>% mutate(Cerebellum=mean(Cerebellum)) %>%
  distinct()

# ----------
# Overall MSA Entire  -----------------------

# ALL  Cerebellum Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(Cerebellum_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(Cerebellum),
    sd=sd(Cerebellum),
    median=median(Cerebellum),
    Q1 = quantile(Cerebellum, 0.25),
    Q3 = quantile(Cerebellum, 0.75),
    min=min(Cerebellum),
    max=max(Cerebellum),
    n=n(),
    skewness = skewness(Cerebellum, na.rm = TRUE),
    kurtosis = kurtosis(Cerebellum, na.rm = TRUE),
    CI_lower = t.test(Cerebellum)$conf.int[1],
    CI_upper = t.test(Cerebellum)$conf.int[2],
    shapiro_w = shapiro.test(Cerebellum)$statistic,
    shapiro_p = shapiro.test(Cerebellum)$p.value
  )


# ALL  Cerebellum Total Year 1

AllMSA_Pop_Baseline_671 %>% inner_join(Cerebellum_VOL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(Cerebellum),
    sd=sd(Cerebellum),
    median=median(Cerebellum),
    Q1 = quantile(Cerebellum, 0.25),
    Q3 = quantile(Cerebellum, 0.75),
    min=min(Cerebellum),
    max=max(Cerebellum),
    n=n(),
    skewness = skewness(Cerebellum, na.rm = TRUE),
    kurtosis = kurtosis(Cerebellum, na.rm = TRUE),
    CI_lower = t.test(Cerebellum)$conf.int[1],
    CI_upper = t.test(Cerebellum)$conf.int[2],
    shapiro_w = shapiro.test(Cerebellum)$statistic,
    shapiro_p = shapiro.test(Cerebellum)$p.value
  )



# Elapsed Time Year 1


AllMSA_Pop_Baseline_671 %>% inner_join(Cerebellum_VOL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum, TIME_STUDY) %>%
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


AllMSA_Pop_Baseline_671 %>% inner_join(Cerebellum_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum) %>%
  rename("Baseline"="Cerebellum") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Cerebellum_VOL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum)
  ) %>%
  mutate(Diff=(Cerebellum-Baseline))  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
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

AllMSA_Pop_Baseline_671 %>% inner_join(Cerebellum_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum) %>%
  rename("Baseline"="Cerebellum") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Cerebellum_VOL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum, TIME_STUDY)
  ) %>%
  mutate(Diff=(Cerebellum-Baseline)/TIME_STUDY)  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )













# ALL  Cerebellum Total Year 2

AllMSA_Pop_Baseline_671 %>% inner_join(Cerebellum_VOL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(Cerebellum),
    sd=sd(Cerebellum),
    median=median(Cerebellum),
    Q1 = quantile(Cerebellum, 0.25),
    Q3 = quantile(Cerebellum, 0.75),
    min=min(Cerebellum),
    max=max(Cerebellum),
    n=n(),
    skewness = skewness(Cerebellum, na.rm = TRUE),
    kurtosis = kurtosis(Cerebellum, na.rm = TRUE),
    CI_lower = t.test(Cerebellum)$conf.int[1],
    CI_upper = t.test(Cerebellum)$conf.int[2],
    shapiro_w = shapiro.test(Cerebellum)$statistic,
    shapiro_p = shapiro.test(Cerebellum)$p.value
  )



# Elapsed Time Year 2


AllMSA_Pop_Baseline_671 %>% inner_join(Cerebellum_VOL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum, TIME_STUDY) %>%
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


AllMSA_Pop_Baseline_671 %>% inner_join(Cerebellum_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum) %>%
  rename("Baseline"="Cerebellum") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Cerebellum_VOL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum)
  ) %>%
  mutate(Diff=(Cerebellum-Baseline))  %>%
  summarise(
    mean=mean(Diff),
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

AllMSA_Pop_Baseline_671 %>% inner_join(Cerebellum_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum) %>%
  rename("Baseline"="Cerebellum") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Cerebellum_VOL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum, TIME_STUDY)
  ) %>%
  mutate(Diff=(Cerebellum-Baseline)/TIME_STUDY)  %>%
  summarise(
    mean=mean(Diff),
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






# ALL  Cerebellum Total Year 3

AllMSA_Pop_Baseline_671 %>% inner_join(Cerebellum_VOL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(Cerebellum),
    sd=sd(Cerebellum),
    median=median(Cerebellum),
    Q1 = quantile(Cerebellum, 0.25),
    Q3 = quantile(Cerebellum, 0.75),
    min=min(Cerebellum),
    max=max(Cerebellum),
    n=n(),
    skewness = skewness(Cerebellum, na.rm = TRUE),
    kurtosis = kurtosis(Cerebellum, na.rm = TRUE),
    CI_lower = t.test(Cerebellum)$conf.int[1],
    CI_upper = t.test(Cerebellum)$conf.int[2],
    shapiro_w = shapiro.test(Cerebellum)$statistic,
    shapiro_p = shapiro.test(Cerebellum)$p.value
  )



# Elapsed Time Year 3


AllMSA_Pop_Baseline_671 %>% inner_join(Cerebellum_VOL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum, TIME_STUDY) %>%
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


AllMSA_Pop_Baseline_671 %>% inner_join(Cerebellum_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum) %>%
  rename("Baseline"="Cerebellum") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Cerebellum_VOL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum)
  ) %>%
  mutate(Diff=(Cerebellum-Baseline))  %>%
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

AllMSA_Pop_Baseline_671 %>% inner_join(Cerebellum_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum) %>%
  rename("Baseline"="Cerebellum") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Cerebellum_VOL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum, TIME_STUDY)
  ) %>%
  mutate(Diff=(Cerebellum-Baseline)/TIME_STUDY)  %>%
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



# -----------
# Overall MSA Entire  MSA P vs MSA C  -----------------------

# ALL  Cerebellum Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(Cerebellum_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Cerebellum),
    sd=sd(Cerebellum),
    median=median(Cerebellum),
    Q1 = quantile(Cerebellum, 0.25),
    Q3 = quantile(Cerebellum, 0.75),
    min=min(Cerebellum),
    max=max(Cerebellum),
    n=n(),
    skewness = skewness(Cerebellum, na.rm = TRUE),
    kurtosis = kurtosis(Cerebellum, na.rm = TRUE),
    CI_lower = t.test(Cerebellum)$conf.int[1],
    CI_upper = t.test(Cerebellum)$conf.int[2],
    shapiro_w = shapiro.test(Cerebellum)$statistic,
    shapiro_p = shapiro.test(Cerebellum)$p.value
  )


# ALL  Cerebellum Total Year 1

AllMSA_Pop_Baseline_671 %>% inner_join(Cerebellum_VOL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Cerebellum),
    sd=sd(Cerebellum),
    median=median(Cerebellum),
    Q1 = quantile(Cerebellum, 0.25),
    Q3 = quantile(Cerebellum, 0.75),
    min=min(Cerebellum),
    max=max(Cerebellum),
    n=n(),
    skewness = skewness(Cerebellum, na.rm = TRUE),
    kurtosis = kurtosis(Cerebellum, na.rm = TRUE),
    CI_lower = t.test(Cerebellum)$conf.int[1],
    CI_upper = t.test(Cerebellum)$conf.int[2],
    shapiro_w = shapiro.test(Cerebellum)$statistic,
    shapiro_p = shapiro.test(Cerebellum)$p.value
  )



# Elapsed Time Year 1


AllMSA_Pop_Baseline_671 %>% inner_join(Cerebellum_VOL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum, DIAG, TIME_STUDY) %>%
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


AllMSA_Pop_Baseline_671 %>% inner_join(Cerebellum_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum) %>%
  rename("Baseline"="Cerebellum") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Cerebellum_VOL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum, DIAG)
  ) %>%
  mutate(Diff=(Cerebellum-Baseline))  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
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

AllMSA_Pop_Baseline_671 %>% inner_join(Cerebellum_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum) %>%
  rename("Baseline"="Cerebellum") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Cerebellum_VOL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum, DIAG, TIME_STUDY)
  ) %>%
  mutate(Diff=(Cerebellum-Baseline)/TIME_STUDY)  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )













# ALL  Cerebellum Total Year 2

AllMSA_Pop_Baseline_671 %>% inner_join(Cerebellum_VOL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Cerebellum),
    sd=sd(Cerebellum),
    median=median(Cerebellum),
    Q1 = quantile(Cerebellum, 0.25),
    Q3 = quantile(Cerebellum, 0.75),
    min=min(Cerebellum),
    max=max(Cerebellum),
    n=n(),
    skewness = skewness(Cerebellum, na.rm = TRUE),
    kurtosis = kurtosis(Cerebellum, na.rm = TRUE),
    CI_lower = t.test(Cerebellum)$conf.int[1],
    CI_upper = t.test(Cerebellum)$conf.int[2],
    shapiro_w = shapiro.test(Cerebellum)$statistic,
    shapiro_p = shapiro.test(Cerebellum)$p.value
  )



# Elapsed Time Year 2


AllMSA_Pop_Baseline_671 %>% inner_join(Cerebellum_VOL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum, DIAG, TIME_STUDY) %>%
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


AllMSA_Pop_Baseline_671 %>% inner_join(Cerebellum_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum) %>%
  rename("Baseline"="Cerebellum") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Cerebellum_VOL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum, DIAG)
  ) %>%
  mutate(Diff=(Cerebellum-Baseline))  %>% group_by(DIAG) %>%
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

AllMSA_Pop_Baseline_671 %>% inner_join(Cerebellum_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum) %>%
  rename("Baseline"="Cerebellum") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Cerebellum_VOL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum, DIAG, TIME_STUDY)
  ) %>%
  mutate(Diff=(Cerebellum-Baseline)/TIME_STUDY)  %>% filter(DIAG=="PD") %>%
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






# ALL  Cerebellum Total Year 3

AllMSA_Pop_Baseline_671 %>% inner_join(Cerebellum_VOL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Cerebellum),
    sd=sd(Cerebellum),
    median=median(Cerebellum),
    Q1 = quantile(Cerebellum, 0.25),
    Q3 = quantile(Cerebellum, 0.75),
    min=min(Cerebellum),
    max=max(Cerebellum),
    n=n(),
    skewness = skewness(Cerebellum, na.rm = TRUE),
    kurtosis = kurtosis(Cerebellum, na.rm = TRUE),
    CI_lower = t.test(Cerebellum)$conf.int[1],
    CI_upper = t.test(Cerebellum)$conf.int[2],
    shapiro_w = shapiro.test(Cerebellum)$statistic,
    shapiro_p = shapiro.test(Cerebellum)$p.value
  )



# Elapsed Time Year 3


AllMSA_Pop_Baseline_671 %>% inner_join(Cerebellum_VOL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum, DIAG, TIME_STUDY) %>%
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


AllMSA_Pop_Baseline_671 %>% inner_join(Cerebellum_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum) %>%
  rename("Baseline"="Cerebellum") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Cerebellum_VOL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum, DIAG)
  ) %>%
  mutate(Diff=(Cerebellum-Baseline))  %>% filter(DIAG=="PD") %>%
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

AllMSA_Pop_Baseline_671 %>% inner_join(Cerebellum_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum) %>%
  rename("Baseline"="Cerebellum") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Cerebellum_VOL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum, DIAG, TIME_STUDY)
  ) %>%
  mutate(Diff=(Cerebellum-Baseline)/TIME_STUDY)  %>% filter(DIAG=="PD") %>%
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



# -----------
# Overall MSA Entire  Probable vs Possible  -----------------------

# ALL  Cerebellum Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(Cerebellum_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Cerebellum),
    sd=sd(Cerebellum),
    median=median(Cerebellum),
    Q1 = quantile(Cerebellum, 0.25),
    Q3 = quantile(Cerebellum, 0.75),
    min=min(Cerebellum),
    max=max(Cerebellum),
    n=n(),
    skewness = skewness(Cerebellum, na.rm = TRUE),
    kurtosis = kurtosis(Cerebellum, na.rm = TRUE),
    CI_lower = t.test(Cerebellum)$conf.int[1],
    CI_upper = t.test(Cerebellum)$conf.int[2],
    shapiro_w = shapiro.test(Cerebellum)$statistic,
    shapiro_p = shapiro.test(Cerebellum)$p.value
  )


# ALL  Cerebellum Total Year 1

AllMSA_Pop_Baseline_671 %>% inner_join(Cerebellum_VOL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Cerebellum),
    sd=sd(Cerebellum),
    median=median(Cerebellum),
    Q1 = quantile(Cerebellum, 0.25),
    Q3 = quantile(Cerebellum, 0.75),
    min=min(Cerebellum),
    max=max(Cerebellum),
    n=n(),
    skewness = skewness(Cerebellum, na.rm = TRUE),
    kurtosis = kurtosis(Cerebellum, na.rm = TRUE),
    CI_lower = t.test(Cerebellum)$conf.int[1],
    CI_upper = t.test(Cerebellum)$conf.int[2],
    shapiro_w = shapiro.test(Cerebellum)$statistic,
    shapiro_p = shapiro.test(Cerebellum)$p.value
  )



# Elapsed Time Year 1


AllMSA_Pop_Baseline_671 %>% inner_join(Cerebellum_VOL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum, DIAGNIV, TIME_STUDY) %>%
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


AllMSA_Pop_Baseline_671 %>% inner_join(Cerebellum_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum) %>%
  rename("Baseline"="Cerebellum") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Cerebellum_VOL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum, DIAGNIV)
  ) %>%
  mutate(Diff=(Cerebellum-Baseline))  %>% filter(DIAGNIV=="POS") %>%
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

AllMSA_Pop_Baseline_671 %>% inner_join(Cerebellum_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum) %>%
  rename("Baseline"="Cerebellum") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Cerebellum_VOL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum, DIAGNIV, TIME_STUDY)
  ) %>%
  mutate(Diff=(Cerebellum-Baseline)/TIME_STUDY)  %>% filter(DIAGNIV=="POS") %>%
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













# ALL  Cerebellum Total Year 2

AllMSA_Pop_Baseline_671 %>% inner_join(Cerebellum_VOL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Cerebellum),
    sd=sd(Cerebellum),
    median=median(Cerebellum),
    Q1 = quantile(Cerebellum, 0.25),
    Q3 = quantile(Cerebellum, 0.75),
    min=min(Cerebellum),
    max=max(Cerebellum),
    n=n(),
    skewness = skewness(Cerebellum, na.rm = TRUE),
    kurtosis = kurtosis(Cerebellum, na.rm = TRUE),
    CI_lower = t.test(Cerebellum)$conf.int[1],
    CI_upper = t.test(Cerebellum)$conf.int[2],
    shapiro_w = shapiro.test(Cerebellum)$statistic,
    shapiro_p = shapiro.test(Cerebellum)$p.value
  )



# Elapsed Time Year 2


AllMSA_Pop_Baseline_671 %>% inner_join(Cerebellum_VOL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum, DIAGNIV, TIME_STUDY) %>%
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


AllMSA_Pop_Baseline_671 %>% inner_join(Cerebellum_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum) %>%
  rename("Baseline"="Cerebellum") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Cerebellum_VOL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum, DIAGNIV)
  ) %>%
  mutate(Diff=(Cerebellum-Baseline))  %>% filter(DIAGNIV=="PROB") %>%
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

AllMSA_Pop_Baseline_671 %>% inner_join(Cerebellum_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum) %>%
  rename("Baseline"="Cerebellum") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Cerebellum_VOL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum, DIAGNIV, TIME_STUDY)
  ) %>%
  mutate(Diff=(Cerebellum-Baseline)/TIME_STUDY)  %>% filter(DIAGNIV=="PROB") %>%
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






# ALL  Cerebellum Total Year 3

AllMSA_Pop_Baseline_671 %>% inner_join(Cerebellum_VOL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Cerebellum),
    sd=sd(Cerebellum),
    median=median(Cerebellum),
    Q1 = quantile(Cerebellum, 0.25),
    Q3 = quantile(Cerebellum, 0.75),
    min=min(Cerebellum),
    max=max(Cerebellum),
    n=n(),
    skewness = skewness(Cerebellum, na.rm = TRUE),
    kurtosis = kurtosis(Cerebellum, na.rm = TRUE),
    CI_lower = t.test(Cerebellum)$conf.int[1],
    CI_upper = t.test(Cerebellum)$conf.int[2],
    shapiro_w = shapiro.test(Cerebellum)$statistic,
    shapiro_p = shapiro.test(Cerebellum)$p.value
  )



# Elapsed Time Year 3


AllMSA_Pop_Baseline_671 %>% inner_join(Cerebellum_VOL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum, DIAGNIV, TIME_STUDY) %>%
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


AllMSA_Pop_Baseline_671 %>% inner_join(Cerebellum_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum) %>%
  rename("Baseline"="Cerebellum") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Cerebellum_VOL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum, DIAGNIV)
  ) %>%
  mutate(Diff=(Cerebellum-Baseline))  %>% filter(DIAGNIV=="PROB") %>%
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

AllMSA_Pop_Baseline_671 %>% inner_join(Cerebellum_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum) %>%
  rename("Baseline"="Cerebellum") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Cerebellum_VOL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum, DIAGNIV, TIME_STUDY)
  ) %>%
  mutate(Diff=(Cerebellum-Baseline)/TIME_STUDY)  %>% filter(DIAGNIV=="PROB") %>%
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

# ---------------


# Target MSA Entire  -----------------------

# ALL  Cerebellum Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(Cerebellum_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(Cerebellum),
    sd=sd(Cerebellum),
    median=median(Cerebellum),
    Q1 = quantile(Cerebellum, 0.25),
    Q3 = quantile(Cerebellum, 0.75),
    min=min(Cerebellum),
    max=max(Cerebellum),
    n=n(),
    skewness = skewness(Cerebellum, na.rm = TRUE),
    kurtosis = kurtosis(Cerebellum, na.rm = TRUE),
    CI_lower = t.test(Cerebellum)$conf.int[1],
    CI_upper = t.test(Cerebellum)$conf.int[2],
    shapiro_w = shapiro.test(Cerebellum)$statistic,
    shapiro_p = shapiro.test(Cerebellum)$p.value
  )


# ALL  Cerebellum Total Year 1

EarlyCT_Pop_Baseline_319 %>% inner_join(Cerebellum_VOL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(Cerebellum),
    sd=sd(Cerebellum),
    median=median(Cerebellum),
    Q1 = quantile(Cerebellum, 0.25),
    Q3 = quantile(Cerebellum, 0.75),
    min=min(Cerebellum),
    max=max(Cerebellum),
    n=n(),
    skewness = skewness(Cerebellum, na.rm = TRUE),
    kurtosis = kurtosis(Cerebellum, na.rm = TRUE),
    CI_lower = t.test(Cerebellum)$conf.int[1],
    CI_upper = t.test(Cerebellum)$conf.int[2],
    shapiro_w = shapiro.test(Cerebellum)$statistic,
    shapiro_p = shapiro.test(Cerebellum)$p.value
  )



# Elapsed Time Year 1


EarlyCT_Pop_Baseline_319 %>% inner_join(Cerebellum_VOL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum, TIME_STUDY) %>%
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


EarlyCT_Pop_Baseline_319 %>% inner_join(Cerebellum_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum) %>%
  rename("Baseline"="Cerebellum") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Cerebellum_VOL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum)
  ) %>%
  mutate(Diff=(Cerebellum-Baseline))  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
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

EarlyCT_Pop_Baseline_319 %>% inner_join(Cerebellum_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum) %>%
  rename("Baseline"="Cerebellum") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Cerebellum_VOL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum, TIME_STUDY)
  ) %>%
  mutate(Diff=(Cerebellum-Baseline)/TIME_STUDY)  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )













# ALL  Cerebellum Total Year 2

EarlyCT_Pop_Baseline_319 %>% inner_join(Cerebellum_VOL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(Cerebellum),
    sd=sd(Cerebellum),
    median=median(Cerebellum),
    Q1 = quantile(Cerebellum, 0.25),
    Q3 = quantile(Cerebellum, 0.75),
    min=min(Cerebellum),
    max=max(Cerebellum),
    n=n(),
    skewness = skewness(Cerebellum, na.rm = TRUE),
    kurtosis = kurtosis(Cerebellum, na.rm = TRUE),
    CI_lower = t.test(Cerebellum)$conf.int[1],
    CI_upper = t.test(Cerebellum)$conf.int[2],
    shapiro_w = shapiro.test(Cerebellum)$statistic,
    shapiro_p = shapiro.test(Cerebellum)$p.value
  )



# Elapsed Time Year 2


EarlyCT_Pop_Baseline_319 %>% inner_join(Cerebellum_VOL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum, TIME_STUDY) %>%
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


EarlyCT_Pop_Baseline_319 %>% inner_join(Cerebellum_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum) %>%
  rename("Baseline"="Cerebellum") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Cerebellum_VOL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum)
  ) %>%
  mutate(Diff=(Cerebellum-Baseline))  %>%
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

EarlyCT_Pop_Baseline_319 %>% inner_join(Cerebellum_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum) %>%
  rename("Baseline"="Cerebellum") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Cerebellum_VOL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum, TIME_STUDY)
  ) %>%
  mutate(Diff=(Cerebellum-Baseline)/TIME_STUDY)  %>%
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






# ALL  Cerebellum Total Year 3

EarlyCT_Pop_Baseline_319 %>% inner_join(Cerebellum_VOL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(Cerebellum),
    sd=sd(Cerebellum),
    median=median(Cerebellum),
    Q1 = quantile(Cerebellum, 0.25),
    Q3 = quantile(Cerebellum, 0.75),
    min=min(Cerebellum),
    max=max(Cerebellum),
    n=n(),
    skewness = skewness(Cerebellum, na.rm = TRUE),
    kurtosis = kurtosis(Cerebellum, na.rm = TRUE),
    CI_lower = t.test(Cerebellum)$conf.int[1],
    CI_upper = t.test(Cerebellum)$conf.int[2],
    shapiro_w = shapiro.test(Cerebellum)$statistic,
    shapiro_p = shapiro.test(Cerebellum)$p.value
  )



# Elapsed Time Year 3


EarlyCT_Pop_Baseline_319 %>% inner_join(Cerebellum_VOL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum, TIME_STUDY) %>%
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


EarlyCT_Pop_Baseline_319 %>% inner_join(Cerebellum_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum) %>%
  rename("Baseline"="Cerebellum") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Cerebellum_VOL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum)
  ) %>%
  mutate(Diff=(Cerebellum-Baseline))  %>%
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

EarlyCT_Pop_Baseline_319 %>% inner_join(Cerebellum_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum) %>%
  rename("Baseline"="Cerebellum") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Cerebellum_VOL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum, TIME_STUDY)
  ) %>%
  mutate(Diff=(Cerebellum-Baseline)/TIME_STUDY)  %>%
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



# -----------
# Target MSA Entire  MSA P vs MSA C  -----------------------

# ALL  Cerebellum Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(Cerebellum_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Cerebellum),
    sd=sd(Cerebellum),
    median=median(Cerebellum),
    Q1 = quantile(Cerebellum, 0.25),
    Q3 = quantile(Cerebellum, 0.75),
    min=min(Cerebellum),
    max=max(Cerebellum),
    n=n(),
    skewness = skewness(Cerebellum, na.rm = TRUE),
    kurtosis = kurtosis(Cerebellum, na.rm = TRUE),
    CI_lower = t.test(Cerebellum)$conf.int[1],
    CI_upper = t.test(Cerebellum)$conf.int[2],
    shapiro_w = shapiro.test(Cerebellum)$statistic,
    shapiro_p = shapiro.test(Cerebellum)$p.value
  )


# ALL  Cerebellum Total Year 1

EarlyCT_Pop_Baseline_319 %>% inner_join(Cerebellum_VOL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Cerebellum),
    sd=sd(Cerebellum),
    median=median(Cerebellum),
    Q1 = quantile(Cerebellum, 0.25),
    Q3 = quantile(Cerebellum, 0.75),
    min=min(Cerebellum),
    max=max(Cerebellum),
    n=n(),
    skewness = skewness(Cerebellum, na.rm = TRUE),
    kurtosis = kurtosis(Cerebellum, na.rm = TRUE),
    CI_lower = t.test(Cerebellum)$conf.int[1],
    CI_upper = t.test(Cerebellum)$conf.int[2],
    shapiro_w = shapiro.test(Cerebellum)$statistic,
    shapiro_p = shapiro.test(Cerebellum)$p.value
  )



# Elapsed Time Year 1


EarlyCT_Pop_Baseline_319 %>% inner_join(Cerebellum_VOL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum, DIAG, TIME_STUDY) %>%
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


EarlyCT_Pop_Baseline_319 %>% inner_join(Cerebellum_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum) %>%
  rename("Baseline"="Cerebellum") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Cerebellum_VOL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum, DIAG)
  ) %>%
  mutate(Diff=(Cerebellum-Baseline))  %>% filter(DIAG=="CB") %>%
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

EarlyCT_Pop_Baseline_319 %>% inner_join(Cerebellum_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum) %>%
  rename("Baseline"="Cerebellum") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Cerebellum_VOL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum, DIAG, TIME_STUDY)
  ) %>%
  mutate(Diff=(Cerebellum-Baseline)/TIME_STUDY)  %>% filter(DIAG=="CB") %>%
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













# ALL  Cerebellum Total Year 2

EarlyCT_Pop_Baseline_319 %>% inner_join(Cerebellum_VOL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Cerebellum),
    sd=sd(Cerebellum),
    median=median(Cerebellum),
    Q1 = quantile(Cerebellum, 0.25),
    Q3 = quantile(Cerebellum, 0.75),
    min=min(Cerebellum),
    max=max(Cerebellum),
    n=n(),
    skewness = skewness(Cerebellum, na.rm = TRUE),
    kurtosis = kurtosis(Cerebellum, na.rm = TRUE),
    CI_lower = t.test(Cerebellum)$conf.int[1],
    CI_upper = t.test(Cerebellum)$conf.int[2],
    shapiro_w = shapiro.test(Cerebellum)$statistic,
    shapiro_p = shapiro.test(Cerebellum)$p.value
  )



# Elapsed Time Year 2


EarlyCT_Pop_Baseline_319 %>% inner_join(Cerebellum_VOL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum, DIAG, TIME_STUDY) %>%
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


EarlyCT_Pop_Baseline_319 %>% inner_join(Cerebellum_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum) %>%
  rename("Baseline"="Cerebellum") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Cerebellum_VOL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum, DIAG)
  ) %>%
  mutate(Diff=(Cerebellum-Baseline))  %>% filter(DIAG=="PD") %>%
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

EarlyCT_Pop_Baseline_319 %>% inner_join(Cerebellum_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum) %>%
  rename("Baseline"="Cerebellum") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Cerebellum_VOL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum, DIAG, TIME_STUDY)
  ) %>%
  mutate(Diff=(Cerebellum-Baseline)/TIME_STUDY)  %>% filter(DIAG=="PD") %>%
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






# ALL  Cerebellum Total Year 3

EarlyCT_Pop_Baseline_319 %>% inner_join(Cerebellum_VOL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Cerebellum),
    sd=sd(Cerebellum),
    median=median(Cerebellum),
    Q1 = quantile(Cerebellum, 0.25),
    Q3 = quantile(Cerebellum, 0.75),
    min=min(Cerebellum),
    max=max(Cerebellum),
    n=n(),
    skewness = skewness(Cerebellum, na.rm = TRUE),
    kurtosis = kurtosis(Cerebellum, na.rm = TRUE),
    CI_lower = t.test(Cerebellum)$conf.int[1],
    CI_upper = t.test(Cerebellum)$conf.int[2],
    shapiro_w = shapiro.test(Cerebellum)$statistic,
    shapiro_p = shapiro.test(Cerebellum)$p.value
  )



# Elapsed Time Year 3


EarlyCT_Pop_Baseline_319 %>% inner_join(Cerebellum_VOL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum, DIAG, TIME_STUDY) %>%
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


EarlyCT_Pop_Baseline_319 %>% inner_join(Cerebellum_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum) %>%
  rename("Baseline"="Cerebellum") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Cerebellum_VOL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum, DIAG)
  ) %>%
  mutate(Diff=(Cerebellum-Baseline))  %>% filter(DIAG=="PD") %>%
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

EarlyCT_Pop_Baseline_319 %>% inner_join(Cerebellum_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum) %>%
  rename("Baseline"="Cerebellum") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Cerebellum_VOL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum, DIAG, TIME_STUDY)
  ) %>%
  mutate(Diff=(Cerebellum-Baseline)/TIME_STUDY)  %>% filter(DIAG=="PD") %>%
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



# -----------
# Target MSA Entire  Probable vs Possible  -----------------------

# ALL  Cerebellum Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(Cerebellum_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Cerebellum),
    sd=sd(Cerebellum),
    median=median(Cerebellum),
    Q1 = quantile(Cerebellum, 0.25),
    Q3 = quantile(Cerebellum, 0.75),
    min=min(Cerebellum),
    max=max(Cerebellum),
    n=n(),
    skewness = skewness(Cerebellum, na.rm = TRUE),
    kurtosis = kurtosis(Cerebellum, na.rm = TRUE),
    CI_lower = t.test(Cerebellum)$conf.int[1],
    CI_upper = t.test(Cerebellum)$conf.int[2],
    shapiro_w = shapiro.test(Cerebellum)$statistic,
    shapiro_p = shapiro.test(Cerebellum)$p.value
  )


# ALL  Cerebellum Total Year 1

EarlyCT_Pop_Baseline_319 %>% inner_join(Cerebellum_VOL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Cerebellum),
    sd=sd(Cerebellum),
    median=median(Cerebellum),
    Q1 = quantile(Cerebellum, 0.25),
    Q3 = quantile(Cerebellum, 0.75),
    min=min(Cerebellum),
    max=max(Cerebellum),
    n=n(),
    skewness = skewness(Cerebellum, na.rm = TRUE),
    kurtosis = kurtosis(Cerebellum, na.rm = TRUE),
    CI_lower = t.test(Cerebellum)$conf.int[1],
    CI_upper = t.test(Cerebellum)$conf.int[2],
    shapiro_w = shapiro.test(Cerebellum)$statistic,
    shapiro_p = shapiro.test(Cerebellum)$p.value
  )



# Elapsed Time Year 1


EarlyCT_Pop_Baseline_319 %>% inner_join(Cerebellum_VOL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum, DIAGNIV, TIME_STUDY) %>%
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


EarlyCT_Pop_Baseline_319 %>% inner_join(Cerebellum_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum) %>%
  rename("Baseline"="Cerebellum") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Cerebellum_VOL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum, DIAGNIV)
  ) %>%
  mutate(Diff=(Cerebellum-Baseline))  %>% filter(DIAGNIV=="PROB") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
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

EarlyCT_Pop_Baseline_319 %>% inner_join(Cerebellum_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum) %>%
  rename("Baseline"="Cerebellum") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Cerebellum_VOL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum, DIAGNIV, TIME_STUDY)
  ) %>%
  mutate(Diff=(Cerebellum-Baseline)/TIME_STUDY)  %>% filter(DIAGNIV=="PROB") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )













# ALL  Cerebellum Total Year 2

EarlyCT_Pop_Baseline_319 %>% inner_join(Cerebellum_VOL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% filter(DIAGNIV=="PROB") %>%
  summarise(
    mean=mean(Cerebellum),
    sd=sd(Cerebellum),
    median=median(Cerebellum),
    Q1 = quantile(Cerebellum, 0.25),
    Q3 = quantile(Cerebellum, 0.75),
    min=min(Cerebellum),
    max=max(Cerebellum),
    n=n(),
    skewness = skewness(Cerebellum, na.rm = TRUE),
    kurtosis = kurtosis(Cerebellum, na.rm = TRUE),
    CI_lower = t.test(Cerebellum)$conf.int[1],
    CI_upper = t.test(Cerebellum)$conf.int[2],
    shapiro_w = shapiro.test(Cerebellum)$statistic,
    shapiro_p = shapiro.test(Cerebellum)$p.value
  )



# Elapsed Time Year 2


EarlyCT_Pop_Baseline_319 %>% inner_join(Cerebellum_VOL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum, DIAGNIV, TIME_STUDY) %>%
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


EarlyCT_Pop_Baseline_319 %>% inner_join(Cerebellum_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum) %>%
  rename("Baseline"="Cerebellum") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Cerebellum_VOL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum, DIAGNIV)
  ) %>%
  mutate(Diff=(Cerebellum-Baseline))  %>% filter(DIAGNIV=="PROB") %>%
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

EarlyCT_Pop_Baseline_319 %>% inner_join(Cerebellum_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum) %>%
  rename("Baseline"="Cerebellum") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Cerebellum_VOL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum, DIAGNIV, TIME_STUDY)
  ) %>%
  mutate(Diff=(Cerebellum-Baseline)/TIME_STUDY)  %>% filter(DIAGNIV=="PROB") %>%
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






# ALL  Cerebellum Total Year 3

EarlyCT_Pop_Baseline_319 %>% inner_join(Cerebellum_VOL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% filter(DIAGNIV=="POS") %>%
  summarise(
    mean=mean(Cerebellum),
    sd=sd(Cerebellum),
    median=median(Cerebellum),
    Q1 = quantile(Cerebellum, 0.25),
    Q3 = quantile(Cerebellum, 0.75),
    min=min(Cerebellum),
    max=max(Cerebellum),
    n=n(),
    #skewness = skewness(Cerebellum, na.rm = TRUE),
    #kurtosis = kurtosis(Cerebellum, na.rm = TRUE),
    #CI_lower = t.test(Cerebellum)$conf.int[1],
    #CI_upper = t.test(Cerebellum)$conf.int[2],
    #shapiro_w = shapiro.test(Cerebellum)$statistic,
    #shapiro_p = shapiro.test(Cerebellum)$p.value
  )



# Elapsed Time Year 3


EarlyCT_Pop_Baseline_319 %>% inner_join(Cerebellum_VOL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum, DIAGNIV, TIME_STUDY) %>%
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


# Change from baseline Year 3


EarlyCT_Pop_Baseline_319 %>% inner_join(Cerebellum_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum) %>%
  rename("Baseline"="Cerebellum") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Cerebellum_VOL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum, DIAGNIV)
  ) %>%
  mutate(Diff=(Cerebellum-Baseline))  %>% filter(DIAGNIV=="PROB") %>%
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

EarlyCT_Pop_Baseline_319 %>% inner_join(Cerebellum_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum) %>%
  rename("Baseline"="Cerebellum") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Cerebellum_VOL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum, DIAGNIV, TIME_STUDY)
  ) %>%
  mutate(Diff=(Cerebellum-Baseline)/TIME_STUDY)  %>% filter(DIAGNIV=="PROB") %>%
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

# ---------------


# All data dates groups  --------
dataCohorteManaged <- readRDS("Source/dataCohorteManaged.rds")

Visits_zero <- dataCohorteManaged  %>% select(NUM, dateVisite0) %>% distinct()

AllMSA_Pop_Baseline_671 <- fread("Source/AllMSA_Pop_Baseline_671.txt")
EarlyCT_Pop_Baseline_319 <- fread("Source/EarlyCT_Pop_Baseline_319.txt")


# ----------
# Midbrain Volume -----------------------------

irmForPaulo <- readRDS("Source/irmForPaulo.rds")

Midbrain_VOL <- irmForPaulo %>% select(NUM, DateIRM, Midbrain)

Midbrain_VOL$Midbrain <- as.numeric(Midbrain_VOL$Midbrain)

Midbrain_VOL <- Visits_zero %>% inner_join(Midbrain_VOL) %>%
  mutate(TIME_STUDY=time_length(difftime(DateIRM, dateVisite0 ), "years")) %>%
  mutate(Year= ifelse(TIME_STUDY<=0.5, 0,
                      ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                             ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                    ifelse(TIME_STUDY>=2.5 ,3, NA))))) 

Midbrain_VOL <- Midbrain_VOL %>% 
  drop_na() %>% arrange(NUM, DateIRM) %>%
  group_by(NUM, DateIRM) %>% mutate(Midbrain=mean(Midbrain)) %>%
  distinct()

# ----------
# Overall MSA Entire  -----------------------

# ALL  Midbrain Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(Midbrain_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(Midbrain),
    sd=sd(Midbrain),
    median=median(Midbrain),
    Q1 = quantile(Midbrain, 0.25),
    Q3 = quantile(Midbrain, 0.75),
    min=min(Midbrain),
    max=max(Midbrain),
    n=n(),
    skewness = skewness(Midbrain, na.rm = TRUE),
    kurtosis = kurtosis(Midbrain, na.rm = TRUE),
    CI_lower = t.test(Midbrain)$conf.int[1],
    CI_upper = t.test(Midbrain)$conf.int[2],
    shapiro_w = shapiro.test(Midbrain)$statistic,
    shapiro_p = shapiro.test(Midbrain)$p.value
  )


# ALL  Midbrain Total Year 1

AllMSA_Pop_Baseline_671 %>% inner_join(Midbrain_VOL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(Midbrain),
    sd=sd(Midbrain),
    median=median(Midbrain),
    Q1 = quantile(Midbrain, 0.25),
    Q3 = quantile(Midbrain, 0.75),
    min=min(Midbrain),
    max=max(Midbrain),
    n=n(),
    skewness = skewness(Midbrain, na.rm = TRUE),
    kurtosis = kurtosis(Midbrain, na.rm = TRUE),
    CI_lower = t.test(Midbrain)$conf.int[1],
    CI_upper = t.test(Midbrain)$conf.int[2],
    shapiro_w = shapiro.test(Midbrain)$statistic,
    shapiro_p = shapiro.test(Midbrain)$p.value
  )



# Elapsed Time Year 1


AllMSA_Pop_Baseline_671 %>% inner_join(Midbrain_VOL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain, TIME_STUDY) %>%
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


AllMSA_Pop_Baseline_671 %>% inner_join(Midbrain_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain) %>%
  rename("Baseline"="Midbrain") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Midbrain_VOL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain)
  ) %>%
  mutate(Diff=(Midbrain-Baseline))  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
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

AllMSA_Pop_Baseline_671 %>% inner_join(Midbrain_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain) %>%
  rename("Baseline"="Midbrain") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Midbrain_VOL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain, TIME_STUDY)
  ) %>%
  mutate(Diff=(Midbrain-Baseline)/TIME_STUDY)  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )













# ALL  Midbrain Total Year 2

AllMSA_Pop_Baseline_671 %>% inner_join(Midbrain_VOL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(Midbrain),
    sd=sd(Midbrain),
    median=median(Midbrain),
    Q1 = quantile(Midbrain, 0.25),
    Q3 = quantile(Midbrain, 0.75),
    min=min(Midbrain),
    max=max(Midbrain),
    n=n(),
    skewness = skewness(Midbrain, na.rm = TRUE),
    kurtosis = kurtosis(Midbrain, na.rm = TRUE),
    CI_lower = t.test(Midbrain)$conf.int[1],
    CI_upper = t.test(Midbrain)$conf.int[2],
    shapiro_w = shapiro.test(Midbrain)$statistic,
    shapiro_p = shapiro.test(Midbrain)$p.value
  )



# Elapsed Time Year 2


AllMSA_Pop_Baseline_671 %>% inner_join(Midbrain_VOL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain, TIME_STUDY) %>%
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


AllMSA_Pop_Baseline_671 %>% inner_join(Midbrain_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain) %>%
  rename("Baseline"="Midbrain") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Midbrain_VOL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain)
  ) %>%
  mutate(Diff=(Midbrain-Baseline))  %>%
  summarise(
    mean=mean(Diff),
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

AllMSA_Pop_Baseline_671 %>% inner_join(Midbrain_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain) %>%
  rename("Baseline"="Midbrain") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Midbrain_VOL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain, TIME_STUDY)
  ) %>%
  mutate(Diff=(Midbrain-Baseline)/TIME_STUDY)  %>%
  summarise(
    mean=mean(Diff),
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






# ALL  Midbrain Total Year 3

AllMSA_Pop_Baseline_671 %>% inner_join(Midbrain_VOL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(Midbrain),
    sd=sd(Midbrain),
    median=median(Midbrain),
    Q1 = quantile(Midbrain, 0.25),
    Q3 = quantile(Midbrain, 0.75),
    min=min(Midbrain),
    max=max(Midbrain),
    n=n(),
    skewness = skewness(Midbrain, na.rm = TRUE),
    kurtosis = kurtosis(Midbrain, na.rm = TRUE),
    CI_lower = t.test(Midbrain)$conf.int[1],
    CI_upper = t.test(Midbrain)$conf.int[2],
    shapiro_w = shapiro.test(Midbrain)$statistic,
    shapiro_p = shapiro.test(Midbrain)$p.value
  )



# Elapsed Time Year 3


AllMSA_Pop_Baseline_671 %>% inner_join(Midbrain_VOL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain, TIME_STUDY) %>%
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


AllMSA_Pop_Baseline_671 %>% inner_join(Midbrain_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain) %>%
  rename("Baseline"="Midbrain") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Midbrain_VOL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain)
  ) %>%
  mutate(Diff=(Midbrain-Baseline))  %>%
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

AllMSA_Pop_Baseline_671 %>% inner_join(Midbrain_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain) %>%
  rename("Baseline"="Midbrain") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Midbrain_VOL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain, TIME_STUDY)
  ) %>%
  mutate(Diff=(Midbrain-Baseline)/TIME_STUDY)  %>%
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



# -----------
# Overall MSA Entire  MSA P vs MSA C  -----------------------

# ALL  Midbrain Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(Midbrain_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Midbrain),
    sd=sd(Midbrain),
    median=median(Midbrain),
    Q1 = quantile(Midbrain, 0.25),
    Q3 = quantile(Midbrain, 0.75),
    min=min(Midbrain),
    max=max(Midbrain),
    n=n(),
    skewness = skewness(Midbrain, na.rm = TRUE),
    kurtosis = kurtosis(Midbrain, na.rm = TRUE),
    CI_lower = t.test(Midbrain)$conf.int[1],
    CI_upper = t.test(Midbrain)$conf.int[2],
    shapiro_w = shapiro.test(Midbrain)$statistic,
    shapiro_p = shapiro.test(Midbrain)$p.value
  )


# ALL  Midbrain Total Year 1

AllMSA_Pop_Baseline_671 %>% inner_join(Midbrain_VOL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Midbrain),
    sd=sd(Midbrain),
    median=median(Midbrain),
    Q1 = quantile(Midbrain, 0.25),
    Q3 = quantile(Midbrain, 0.75),
    min=min(Midbrain),
    max=max(Midbrain),
    n=n(),
    skewness = skewness(Midbrain, na.rm = TRUE),
    kurtosis = kurtosis(Midbrain, na.rm = TRUE),
    CI_lower = t.test(Midbrain)$conf.int[1],
    CI_upper = t.test(Midbrain)$conf.int[2],
    shapiro_w = shapiro.test(Midbrain)$statistic,
    shapiro_p = shapiro.test(Midbrain)$p.value
  )



# Elapsed Time Year 1


AllMSA_Pop_Baseline_671 %>% inner_join(Midbrain_VOL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain, DIAG, TIME_STUDY) %>%
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


AllMSA_Pop_Baseline_671 %>% inner_join(Midbrain_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain) %>%
  rename("Baseline"="Midbrain") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Midbrain_VOL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain, DIAG)
  ) %>%
  mutate(Diff=(Midbrain-Baseline))  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
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

AllMSA_Pop_Baseline_671 %>% inner_join(Midbrain_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain) %>%
  rename("Baseline"="Midbrain") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Midbrain_VOL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain, DIAG, TIME_STUDY)
  ) %>%
  mutate(Diff=(Midbrain-Baseline)/TIME_STUDY)  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )













# ALL  Midbrain Total Year 2

AllMSA_Pop_Baseline_671 %>% inner_join(Midbrain_VOL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Midbrain),
    sd=sd(Midbrain),
    median=median(Midbrain),
    Q1 = quantile(Midbrain, 0.25),
    Q3 = quantile(Midbrain, 0.75),
    min=min(Midbrain),
    max=max(Midbrain),
    n=n(),
    skewness = skewness(Midbrain, na.rm = TRUE),
    kurtosis = kurtosis(Midbrain, na.rm = TRUE),
    CI_lower = t.test(Midbrain)$conf.int[1],
    CI_upper = t.test(Midbrain)$conf.int[2],
    shapiro_w = shapiro.test(Midbrain)$statistic,
    shapiro_p = shapiro.test(Midbrain)$p.value
  )



# Elapsed Time Year 2


AllMSA_Pop_Baseline_671 %>% inner_join(Midbrain_VOL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain, DIAG, TIME_STUDY) %>%
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


AllMSA_Pop_Baseline_671 %>% inner_join(Midbrain_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain) %>%
  rename("Baseline"="Midbrain") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Midbrain_VOL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain, DIAG)
  ) %>%
  mutate(Diff=(Midbrain-Baseline))  %>% group_by(DIAG) %>%
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

AllMSA_Pop_Baseline_671 %>% inner_join(Midbrain_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain) %>%
  rename("Baseline"="Midbrain") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Midbrain_VOL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain, DIAG, TIME_STUDY)
  ) %>%
  mutate(Diff=(Midbrain-Baseline)/TIME_STUDY)  %>% filter(DIAG=="PD") %>%
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






# ALL  Midbrain Total Year 3

AllMSA_Pop_Baseline_671 %>% inner_join(Midbrain_VOL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Midbrain),
    sd=sd(Midbrain),
    median=median(Midbrain),
    Q1 = quantile(Midbrain, 0.25),
    Q3 = quantile(Midbrain, 0.75),
    min=min(Midbrain),
    max=max(Midbrain),
    n=n(),
    skewness = skewness(Midbrain, na.rm = TRUE),
    kurtosis = kurtosis(Midbrain, na.rm = TRUE),
    CI_lower = t.test(Midbrain)$conf.int[1],
    CI_upper = t.test(Midbrain)$conf.int[2],
    shapiro_w = shapiro.test(Midbrain)$statistic,
    shapiro_p = shapiro.test(Midbrain)$p.value
  )



# Elapsed Time Year 3


AllMSA_Pop_Baseline_671 %>% inner_join(Midbrain_VOL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain, DIAG, TIME_STUDY) %>%
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


AllMSA_Pop_Baseline_671 %>% inner_join(Midbrain_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain) %>%
  rename("Baseline"="Midbrain") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Midbrain_VOL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain, DIAG)
  ) %>%
  mutate(Diff=(Midbrain-Baseline))  %>% filter(DIAG=="PD") %>%
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

AllMSA_Pop_Baseline_671 %>% inner_join(Midbrain_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain) %>%
  rename("Baseline"="Midbrain") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Midbrain_VOL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain, DIAG, TIME_STUDY)
  ) %>%
  mutate(Diff=(Midbrain-Baseline)/TIME_STUDY)  %>% filter(DIAG=="PD") %>%
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



# -----------
# Overall MSA Entire  Probable vs Possible  -----------------------

# ALL  Midbrain Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(Midbrain_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Midbrain),
    sd=sd(Midbrain),
    median=median(Midbrain),
    Q1 = quantile(Midbrain, 0.25),
    Q3 = quantile(Midbrain, 0.75),
    min=min(Midbrain),
    max=max(Midbrain),
    n=n(),
    skewness = skewness(Midbrain, na.rm = TRUE),
    kurtosis = kurtosis(Midbrain, na.rm = TRUE),
    CI_lower = t.test(Midbrain)$conf.int[1],
    CI_upper = t.test(Midbrain)$conf.int[2],
    shapiro_w = shapiro.test(Midbrain)$statistic,
    shapiro_p = shapiro.test(Midbrain)$p.value
  )


# ALL  Midbrain Total Year 1

AllMSA_Pop_Baseline_671 %>% inner_join(Midbrain_VOL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Midbrain),
    sd=sd(Midbrain),
    median=median(Midbrain),
    Q1 = quantile(Midbrain, 0.25),
    Q3 = quantile(Midbrain, 0.75),
    min=min(Midbrain),
    max=max(Midbrain),
    n=n(),
    skewness = skewness(Midbrain, na.rm = TRUE),
    kurtosis = kurtosis(Midbrain, na.rm = TRUE),
    CI_lower = t.test(Midbrain)$conf.int[1],
    CI_upper = t.test(Midbrain)$conf.int[2],
    shapiro_w = shapiro.test(Midbrain)$statistic,
    shapiro_p = shapiro.test(Midbrain)$p.value
  )



# Elapsed Time Year 1


AllMSA_Pop_Baseline_671 %>% inner_join(Midbrain_VOL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain, DIAGNIV, TIME_STUDY) %>%
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


AllMSA_Pop_Baseline_671 %>% inner_join(Midbrain_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain) %>%
  rename("Baseline"="Midbrain") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Midbrain_VOL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain, DIAGNIV)
  ) %>%
  mutate(Diff=(Midbrain-Baseline))  %>% filter(DIAGNIV=="PROB") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
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

AllMSA_Pop_Baseline_671 %>% inner_join(Midbrain_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain) %>%
  rename("Baseline"="Midbrain") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Midbrain_VOL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain, DIAGNIV, TIME_STUDY)
  ) %>%
  mutate(Diff=(Midbrain-Baseline)/TIME_STUDY)  %>% filter(DIAGNIV=="PROB") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )













# ALL  Midbrain Total Year 2

AllMSA_Pop_Baseline_671 %>% inner_join(Midbrain_VOL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Midbrain),
    sd=sd(Midbrain),
    median=median(Midbrain),
    Q1 = quantile(Midbrain, 0.25),
    Q3 = quantile(Midbrain, 0.75),
    min=min(Midbrain),
    max=max(Midbrain),
    n=n(),
    skewness = skewness(Midbrain, na.rm = TRUE),
    kurtosis = kurtosis(Midbrain, na.rm = TRUE),
    CI_lower = t.test(Midbrain)$conf.int[1],
    CI_upper = t.test(Midbrain)$conf.int[2],
    shapiro_w = shapiro.test(Midbrain)$statistic,
    shapiro_p = shapiro.test(Midbrain)$p.value
  )



# Elapsed Time Year 2


AllMSA_Pop_Baseline_671 %>% inner_join(Midbrain_VOL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain, DIAGNIV, TIME_STUDY) %>%
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


AllMSA_Pop_Baseline_671 %>% inner_join(Midbrain_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain) %>%
  rename("Baseline"="Midbrain") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Midbrain_VOL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain, DIAGNIV)
  ) %>%
  mutate(Diff=(Midbrain-Baseline))  %>% filter(DIAGNIV=="PROB") %>%
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

AllMSA_Pop_Baseline_671 %>% inner_join(Midbrain_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain) %>%
  rename("Baseline"="Midbrain") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Midbrain_VOL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain, DIAGNIV, TIME_STUDY)
  ) %>%
  mutate(Diff=(Midbrain-Baseline)/TIME_STUDY)  %>% filter(DIAGNIV=="PROB") %>%
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






# ALL  Midbrain Total Year 3

AllMSA_Pop_Baseline_671 %>% inner_join(Midbrain_VOL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Midbrain),
    sd=sd(Midbrain),
    median=median(Midbrain),
    Q1 = quantile(Midbrain, 0.25),
    Q3 = quantile(Midbrain, 0.75),
    min=min(Midbrain),
    max=max(Midbrain),
    n=n(),
    skewness = skewness(Midbrain, na.rm = TRUE),
    kurtosis = kurtosis(Midbrain, na.rm = TRUE),
    CI_lower = t.test(Midbrain)$conf.int[1],
    CI_upper = t.test(Midbrain)$conf.int[2],
    shapiro_w = shapiro.test(Midbrain)$statistic,
    shapiro_p = shapiro.test(Midbrain)$p.value
  )



# Elapsed Time Year 3


AllMSA_Pop_Baseline_671 %>% inner_join(Midbrain_VOL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain, DIAGNIV, TIME_STUDY) %>%
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


AllMSA_Pop_Baseline_671 %>% inner_join(Midbrain_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain) %>%
  rename("Baseline"="Midbrain") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Midbrain_VOL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain, DIAGNIV)
  ) %>%
  mutate(Diff=(Midbrain-Baseline))  %>% filter(DIAGNIV=="PROB") %>%
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

AllMSA_Pop_Baseline_671 %>% inner_join(Midbrain_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain) %>%
  rename("Baseline"="Midbrain") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Midbrain_VOL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain, DIAGNIV, TIME_STUDY)
  ) %>%
  mutate(Diff=(Midbrain-Baseline)/TIME_STUDY)  %>% filter(DIAGNIV=="PROB") %>%
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

# ---------------


# Target MSA Entire  -----------------------

# ALL  Midbrain Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(Midbrain_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(Midbrain),
    sd=sd(Midbrain),
    median=median(Midbrain),
    Q1 = quantile(Midbrain, 0.25),
    Q3 = quantile(Midbrain, 0.75),
    min=min(Midbrain),
    max=max(Midbrain),
    n=n(),
    skewness = skewness(Midbrain, na.rm = TRUE),
    kurtosis = kurtosis(Midbrain, na.rm = TRUE),
    CI_lower = t.test(Midbrain)$conf.int[1],
    CI_upper = t.test(Midbrain)$conf.int[2],
    shapiro_w = shapiro.test(Midbrain)$statistic,
    shapiro_p = shapiro.test(Midbrain)$p.value
  )


# ALL  Midbrain Total Year 1

EarlyCT_Pop_Baseline_319 %>% inner_join(Midbrain_VOL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(Midbrain),
    sd=sd(Midbrain),
    median=median(Midbrain),
    Q1 = quantile(Midbrain, 0.25),
    Q3 = quantile(Midbrain, 0.75),
    min=min(Midbrain),
    max=max(Midbrain),
    n=n(),
    skewness = skewness(Midbrain, na.rm = TRUE),
    kurtosis = kurtosis(Midbrain, na.rm = TRUE),
    CI_lower = t.test(Midbrain)$conf.int[1],
    CI_upper = t.test(Midbrain)$conf.int[2],
    shapiro_w = shapiro.test(Midbrain)$statistic,
    shapiro_p = shapiro.test(Midbrain)$p.value
  )



# Elapsed Time Year 1


EarlyCT_Pop_Baseline_319 %>% inner_join(Midbrain_VOL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain, TIME_STUDY) %>%
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


EarlyCT_Pop_Baseline_319 %>% inner_join(Midbrain_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain) %>%
  rename("Baseline"="Midbrain") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Midbrain_VOL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain)
  ) %>%
  mutate(Diff=(Midbrain-Baseline))  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
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

EarlyCT_Pop_Baseline_319 %>% inner_join(Midbrain_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain) %>%
  rename("Baseline"="Midbrain") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Midbrain_VOL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain, TIME_STUDY)
  ) %>%
  mutate(Diff=(Midbrain-Baseline)/TIME_STUDY)  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )













# ALL  Midbrain Total Year 2

EarlyCT_Pop_Baseline_319 %>% inner_join(Midbrain_VOL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(Midbrain),
    sd=sd(Midbrain),
    median=median(Midbrain),
    Q1 = quantile(Midbrain, 0.25),
    Q3 = quantile(Midbrain, 0.75),
    min=min(Midbrain),
    max=max(Midbrain),
    n=n(),
    skewness = skewness(Midbrain, na.rm = TRUE),
    kurtosis = kurtosis(Midbrain, na.rm = TRUE),
    CI_lower = t.test(Midbrain)$conf.int[1],
    CI_upper = t.test(Midbrain)$conf.int[2],
    shapiro_w = shapiro.test(Midbrain)$statistic,
    shapiro_p = shapiro.test(Midbrain)$p.value
  )



# Elapsed Time Year 2


EarlyCT_Pop_Baseline_319 %>% inner_join(Midbrain_VOL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain, TIME_STUDY) %>%
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


EarlyCT_Pop_Baseline_319 %>% inner_join(Midbrain_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain) %>%
  rename("Baseline"="Midbrain") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Midbrain_VOL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain)
  ) %>%
  mutate(Diff=(Midbrain-Baseline))  %>%
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

EarlyCT_Pop_Baseline_319 %>% inner_join(Midbrain_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain) %>%
  rename("Baseline"="Midbrain") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Midbrain_VOL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain, TIME_STUDY)
  ) %>%
  mutate(Diff=(Midbrain-Baseline)/TIME_STUDY)  %>%
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






# ALL  Midbrain Total Year 3

EarlyCT_Pop_Baseline_319 %>% inner_join(Midbrain_VOL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(Midbrain),
    sd=sd(Midbrain),
    median=median(Midbrain),
    Q1 = quantile(Midbrain, 0.25),
    Q3 = quantile(Midbrain, 0.75),
    min=min(Midbrain),
    max=max(Midbrain),
    n=n(),
    skewness = skewness(Midbrain, na.rm = TRUE),
    kurtosis = kurtosis(Midbrain, na.rm = TRUE),
    CI_lower = t.test(Midbrain)$conf.int[1],
    CI_upper = t.test(Midbrain)$conf.int[2],
    shapiro_w = shapiro.test(Midbrain)$statistic,
    shapiro_p = shapiro.test(Midbrain)$p.value
  )



# Elapsed Time Year 3


EarlyCT_Pop_Baseline_319 %>% inner_join(Midbrain_VOL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain, TIME_STUDY) %>%
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


EarlyCT_Pop_Baseline_319 %>% inner_join(Midbrain_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain) %>%
  rename("Baseline"="Midbrain") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Midbrain_VOL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain)
  ) %>%
  mutate(Diff=(Midbrain-Baseline))  %>%
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

EarlyCT_Pop_Baseline_319 %>% inner_join(Midbrain_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain) %>%
  rename("Baseline"="Midbrain") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Midbrain_VOL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain, TIME_STUDY)
  ) %>%
  mutate(Diff=(Midbrain-Baseline)/TIME_STUDY)  %>%
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



# -----------
# Target MSA Entire  MSA P vs MSA C  -----------------------

# ALL  Midbrain Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(Midbrain_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Midbrain),
    sd=sd(Midbrain),
    median=median(Midbrain),
    Q1 = quantile(Midbrain, 0.25),
    Q3 = quantile(Midbrain, 0.75),
    min=min(Midbrain),
    max=max(Midbrain),
    n=n(),
    skewness = skewness(Midbrain, na.rm = TRUE),
    kurtosis = kurtosis(Midbrain, na.rm = TRUE),
    CI_lower = t.test(Midbrain)$conf.int[1],
    CI_upper = t.test(Midbrain)$conf.int[2],
    shapiro_w = shapiro.test(Midbrain)$statistic,
    shapiro_p = shapiro.test(Midbrain)$p.value
  )


# ALL  Midbrain Total Year 1

EarlyCT_Pop_Baseline_319 %>% inner_join(Midbrain_VOL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Midbrain),
    sd=sd(Midbrain),
    median=median(Midbrain),
    Q1 = quantile(Midbrain, 0.25),
    Q3 = quantile(Midbrain, 0.75),
    min=min(Midbrain),
    max=max(Midbrain),
    n=n(),
    skewness = skewness(Midbrain, na.rm = TRUE),
    kurtosis = kurtosis(Midbrain, na.rm = TRUE),
    CI_lower = t.test(Midbrain)$conf.int[1],
    CI_upper = t.test(Midbrain)$conf.int[2],
    shapiro_w = shapiro.test(Midbrain)$statistic,
    shapiro_p = shapiro.test(Midbrain)$p.value
  )



# Elapsed Time Year 1


EarlyCT_Pop_Baseline_319 %>% inner_join(Midbrain_VOL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain, DIAG, TIME_STUDY) %>%
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


EarlyCT_Pop_Baseline_319 %>% inner_join(Midbrain_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain) %>%
  rename("Baseline"="Midbrain") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Midbrain_VOL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain, DIAG)
  ) %>%
  mutate(Diff=(Midbrain-Baseline))  %>% filter(DIAG=="PD") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
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

EarlyCT_Pop_Baseline_319 %>% inner_join(Midbrain_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain) %>%
  rename("Baseline"="Midbrain") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Midbrain_VOL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain, DIAG, TIME_STUDY)
  ) %>%
  mutate(Diff=(Midbrain-Baseline)/TIME_STUDY)  %>% filter(DIAG=="PD") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )













# ALL  Midbrain Total Year 2

EarlyCT_Pop_Baseline_319 %>% inner_join(Midbrain_VOL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Midbrain),
    sd=sd(Midbrain),
    median=median(Midbrain),
    Q1 = quantile(Midbrain, 0.25),
    Q3 = quantile(Midbrain, 0.75),
    min=min(Midbrain),
    max=max(Midbrain),
    n=n(),
    skewness = skewness(Midbrain, na.rm = TRUE),
    kurtosis = kurtosis(Midbrain, na.rm = TRUE),
    CI_lower = t.test(Midbrain)$conf.int[1],
    CI_upper = t.test(Midbrain)$conf.int[2],
    shapiro_w = shapiro.test(Midbrain)$statistic,
    shapiro_p = shapiro.test(Midbrain)$p.value
  )



# Elapsed Time Year 2


EarlyCT_Pop_Baseline_319 %>% inner_join(Midbrain_VOL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain, DIAG, TIME_STUDY) %>%
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


EarlyCT_Pop_Baseline_319 %>% inner_join(Midbrain_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain) %>%
  rename("Baseline"="Midbrain") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Midbrain_VOL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain, DIAG)
  ) %>%
  mutate(Diff=(Midbrain-Baseline))  %>% filter(DIAG=="PD") %>%
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

EarlyCT_Pop_Baseline_319 %>% inner_join(Midbrain_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain) %>%
  rename("Baseline"="Midbrain") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Midbrain_VOL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain, DIAG, TIME_STUDY)
  ) %>%
  mutate(Diff=(Midbrain-Baseline)/TIME_STUDY)  %>% filter(DIAG=="PD") %>%
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






# ALL  Midbrain Total Year 3

EarlyCT_Pop_Baseline_319 %>% inner_join(Midbrain_VOL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Midbrain),
    sd=sd(Midbrain),
    median=median(Midbrain),
    Q1 = quantile(Midbrain, 0.25),
    Q3 = quantile(Midbrain, 0.75),
    min=min(Midbrain),
    max=max(Midbrain),
    n=n(),
    skewness = skewness(Midbrain, na.rm = TRUE),
    kurtosis = kurtosis(Midbrain, na.rm = TRUE),
    CI_lower = t.test(Midbrain)$conf.int[1],
    CI_upper = t.test(Midbrain)$conf.int[2],
    shapiro_w = shapiro.test(Midbrain)$statistic,
    shapiro_p = shapiro.test(Midbrain)$p.value
  )



# Elapsed Time Year 3


EarlyCT_Pop_Baseline_319 %>% inner_join(Midbrain_VOL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain, DIAG, TIME_STUDY) %>%
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


EarlyCT_Pop_Baseline_319 %>% inner_join(Midbrain_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain) %>%
  rename("Baseline"="Midbrain") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Midbrain_VOL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain, DIAG)
  ) %>%
  mutate(Diff=(Midbrain-Baseline))  %>% filter(DIAG=="PD") %>%
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

EarlyCT_Pop_Baseline_319 %>% inner_join(Midbrain_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain) %>%
  rename("Baseline"="Midbrain") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Midbrain_VOL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain, DIAG, TIME_STUDY)
  ) %>%
  mutate(Diff=(Midbrain-Baseline)/TIME_STUDY)  %>% filter(DIAG=="PD") %>%
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



# -----------
# Target MSA Entire  Probable vs Possible  -----------------------

# ALL  Midbrain Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(Midbrain_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Midbrain),
    sd=sd(Midbrain),
    median=median(Midbrain),
    Q1 = quantile(Midbrain, 0.25),
    Q3 = quantile(Midbrain, 0.75),
    min=min(Midbrain),
    max=max(Midbrain),
    n=n(),
    skewness = skewness(Midbrain, na.rm = TRUE),
    kurtosis = kurtosis(Midbrain, na.rm = TRUE),
    CI_lower = t.test(Midbrain)$conf.int[1],
    CI_upper = t.test(Midbrain)$conf.int[2],
    shapiro_w = shapiro.test(Midbrain)$statistic,
    shapiro_p = shapiro.test(Midbrain)$p.value
  )


# ALL  Midbrain Total Year 1

EarlyCT_Pop_Baseline_319 %>% inner_join(Midbrain_VOL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Midbrain),
    sd=sd(Midbrain),
    median=median(Midbrain),
    Q1 = quantile(Midbrain, 0.25),
    Q3 = quantile(Midbrain, 0.75),
    min=min(Midbrain),
    max=max(Midbrain),
    n=n(),
    skewness = skewness(Midbrain, na.rm = TRUE),
    kurtosis = kurtosis(Midbrain, na.rm = TRUE),
    CI_lower = t.test(Midbrain)$conf.int[1],
    CI_upper = t.test(Midbrain)$conf.int[2],
    shapiro_w = shapiro.test(Midbrain)$statistic,
    shapiro_p = shapiro.test(Midbrain)$p.value
  )



# Elapsed Time Year 1


EarlyCT_Pop_Baseline_319 %>% inner_join(Midbrain_VOL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain, DIAGNIV, TIME_STUDY) %>%
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


EarlyCT_Pop_Baseline_319 %>% inner_join(Midbrain_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain) %>%
  rename("Baseline"="Midbrain") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Midbrain_VOL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain, DIAGNIV)
  ) %>%
  mutate(Diff=(Midbrain-Baseline))  %>% filter(DIAGNIV=="POS") %>%
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

EarlyCT_Pop_Baseline_319 %>% inner_join(Midbrain_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain) %>%
  rename("Baseline"="Midbrain") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Midbrain_VOL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain, DIAGNIV, TIME_STUDY)
  ) %>%
  mutate(Diff=(Midbrain-Baseline)/TIME_STUDY)  %>% filter(DIAGNIV=="POS") %>%
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













# ALL  Midbrain Total Year 2

EarlyCT_Pop_Baseline_319 %>% inner_join(Midbrain_VOL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% filter(DIAGNIV=="POS") %>%
  summarise(
    mean=mean(Midbrain),
    sd=sd(Midbrain),
    median=median(Midbrain),
    Q1 = quantile(Midbrain, 0.25),
    Q3 = quantile(Midbrain, 0.75),
    min=min(Midbrain),
    max=max(Midbrain),
    n=n(),
    #skewness = skewness(Midbrain, na.rm = TRUE),
    #kurtosis = kurtosis(Midbrain, na.rm = TRUE),
    #CI_lower = t.test(Midbrain)$conf.int[1],
    #CI_upper = t.test(Midbrain)$conf.int[2],
    #shapiro_w = shapiro.test(Midbrain)$statistic,
    #shapiro_p = shapiro.test(Midbrain)$p.value
  )



# Elapsed Time Year 2


EarlyCT_Pop_Baseline_319 %>% inner_join(Midbrain_VOL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain, DIAGNIV, TIME_STUDY) %>%
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


EarlyCT_Pop_Baseline_319 %>% inner_join(Midbrain_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain) %>%
  rename("Baseline"="Midbrain") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Midbrain_VOL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain, DIAGNIV)
  ) %>%
  mutate(Diff=(Midbrain-Baseline))  %>% filter(DIAGNIV=="PROB") %>%
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

EarlyCT_Pop_Baseline_319 %>% inner_join(Midbrain_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain) %>%
  rename("Baseline"="Midbrain") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Midbrain_VOL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain, DIAGNIV, TIME_STUDY)
  ) %>%
  mutate(Diff=(Midbrain-Baseline)/TIME_STUDY)  %>% filter(DIAGNIV=="PROB") %>%
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






# ALL  Midbrain Total Year 3

EarlyCT_Pop_Baseline_319 %>% inner_join(Midbrain_VOL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% filter(DIAGNIV=="PROB") %>%
  summarise(
    mean=mean(Midbrain),
    sd=sd(Midbrain),
    median=median(Midbrain),
    Q1 = quantile(Midbrain, 0.25),
    Q3 = quantile(Midbrain, 0.75),
    min=min(Midbrain),
    max=max(Midbrain),
    n=n(),
    skewness = skewness(Midbrain, na.rm = TRUE),
    kurtosis = kurtosis(Midbrain, na.rm = TRUE),
    CI_lower = t.test(Midbrain)$conf.int[1],
    CI_upper = t.test(Midbrain)$conf.int[2],
    shapiro_w = shapiro.test(Midbrain)$statistic,
    shapiro_p = shapiro.test(Midbrain)$p.value
  )



# Elapsed Time Year 3


EarlyCT_Pop_Baseline_319 %>% inner_join(Midbrain_VOL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain, DIAGNIV, TIME_STUDY) %>%
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


# Change from baseline Year 3


EarlyCT_Pop_Baseline_319 %>% inner_join(Midbrain_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain) %>%
  rename("Baseline"="Midbrain") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Midbrain_VOL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain, DIAGNIV)
  ) %>%
  mutate(Diff=(Midbrain-Baseline))  %>% filter(DIAGNIV=="PROB") %>%
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

EarlyCT_Pop_Baseline_319 %>% inner_join(Midbrain_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain) %>%
  rename("Baseline"="Midbrain") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Midbrain_VOL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain, DIAGNIV, TIME_STUDY)
  ) %>%
  mutate(Diff=(Midbrain-Baseline)/TIME_STUDY)  %>% filter(DIAGNIV=="PROB") %>%
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

# ---------------


# All data dates groups  --------
dataCohorteManaged <- readRDS("Source/dataCohorteManaged.rds")

Visits_zero <- dataCohorteManaged  %>% select(NUM, dateVisite0) %>% distinct()

AllMSA_Pop_Baseline_671 <- fread("Source/AllMSA_Pop_Baseline_671.txt")
EarlyCT_Pop_Baseline_319 <- fread("Source/EarlyCT_Pop_Baseline_319.txt")


# ----------
# PONS Volume -----------------------------

irmForPaulo <- readRDS("Source/irmForPaulo.rds")

Pons_VOL <- irmForPaulo %>% select(NUM, DateIRM, Pons)

Pons_VOL$Pons <- as.numeric(Pons_VOL$Pons)

Pons_VOL <- Visits_zero %>% inner_join(Pons_VOL) %>%
  mutate(TIME_STUDY=time_length(difftime(DateIRM, dateVisite0 ), "years")) %>%
  mutate(Year= ifelse(TIME_STUDY<=0.5, 0,
                      ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                             ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                    ifelse(TIME_STUDY>=2.5 ,3, NA))))) 

Pons_VOL <- Pons_VOL %>% 
  drop_na() %>% arrange(NUM, DateIRM) %>%
  group_by(NUM, DateIRM) %>% mutate(Pons=mean(Pons)) %>%
  distinct()

# ----------
# Overall MSA Entire  -----------------------

# ALL  Pons Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(Pons_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(Pons),
    sd=sd(Pons),
    median=median(Pons),
    Q1 = quantile(Pons, 0.25),
    Q3 = quantile(Pons, 0.75),
    min=min(Pons),
    max=max(Pons),
    n=n(),
    skewness = skewness(Pons, na.rm = TRUE),
    kurtosis = kurtosis(Pons, na.rm = TRUE),
    CI_lower = t.test(Pons)$conf.int[1],
    CI_upper = t.test(Pons)$conf.int[2],
    shapiro_w = shapiro.test(Pons)$statistic,
    shapiro_p = shapiro.test(Pons)$p.value
  )


# ALL  Pons Total Year 1

AllMSA_Pop_Baseline_671 %>% inner_join(Pons_VOL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(Pons),
    sd=sd(Pons),
    median=median(Pons),
    Q1 = quantile(Pons, 0.25),
    Q3 = quantile(Pons, 0.75),
    min=min(Pons),
    max=max(Pons),
    n=n(),
    skewness = skewness(Pons, na.rm = TRUE),
    kurtosis = kurtosis(Pons, na.rm = TRUE),
    CI_lower = t.test(Pons)$conf.int[1],
    CI_upper = t.test(Pons)$conf.int[2],
    shapiro_w = shapiro.test(Pons)$statistic,
    shapiro_p = shapiro.test(Pons)$p.value
  )



# Elapsed Time Year 1


AllMSA_Pop_Baseline_671 %>% inner_join(Pons_VOL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons, TIME_STUDY) %>%
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


AllMSA_Pop_Baseline_671 %>% inner_join(Pons_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons) %>%
  rename("Baseline"="Pons") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Pons_VOL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons)
  ) %>%
  mutate(Diff=(Pons-Baseline))  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
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

AllMSA_Pop_Baseline_671 %>% inner_join(Pons_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons) %>%
  rename("Baseline"="Pons") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Pons_VOL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons, TIME_STUDY)
  ) %>%
  mutate(Diff=(Pons-Baseline)/TIME_STUDY)  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )













# ALL  Pons Total Year 2

AllMSA_Pop_Baseline_671 %>% inner_join(Pons_VOL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(Pons),
    sd=sd(Pons),
    median=median(Pons),
    Q1 = quantile(Pons, 0.25),
    Q3 = quantile(Pons, 0.75),
    min=min(Pons),
    max=max(Pons),
    n=n(),
    skewness = skewness(Pons, na.rm = TRUE),
    kurtosis = kurtosis(Pons, na.rm = TRUE),
    CI_lower = t.test(Pons)$conf.int[1],
    CI_upper = t.test(Pons)$conf.int[2],
    shapiro_w = shapiro.test(Pons)$statistic,
    shapiro_p = shapiro.test(Pons)$p.value
  )



# Elapsed Time Year 2


AllMSA_Pop_Baseline_671 %>% inner_join(Pons_VOL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons, TIME_STUDY) %>%
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


AllMSA_Pop_Baseline_671 %>% inner_join(Pons_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons) %>%
  rename("Baseline"="Pons") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Pons_VOL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons)
  ) %>%
  mutate(Diff=(Pons-Baseline))  %>%
  summarise(
    mean=mean(Diff),
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

AllMSA_Pop_Baseline_671 %>% inner_join(Pons_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons) %>%
  rename("Baseline"="Pons") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Pons_VOL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons, TIME_STUDY)
  ) %>%
  mutate(Diff=(Pons-Baseline)/TIME_STUDY)  %>%
  summarise(
    mean=mean(Diff),
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






# ALL  Pons Total Year 3

AllMSA_Pop_Baseline_671 %>% inner_join(Pons_VOL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(Pons),
    sd=sd(Pons),
    median=median(Pons),
    Q1 = quantile(Pons, 0.25),
    Q3 = quantile(Pons, 0.75),
    min=min(Pons),
    max=max(Pons),
    n=n(),
    skewness = skewness(Pons, na.rm = TRUE),
    kurtosis = kurtosis(Pons, na.rm = TRUE),
    CI_lower = t.test(Pons)$conf.int[1],
    CI_upper = t.test(Pons)$conf.int[2],
    shapiro_w = shapiro.test(Pons)$statistic,
    shapiro_p = shapiro.test(Pons)$p.value
  )



# Elapsed Time Year 3


AllMSA_Pop_Baseline_671 %>% inner_join(Pons_VOL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons, TIME_STUDY) %>%
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


AllMSA_Pop_Baseline_671 %>% inner_join(Pons_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons) %>%
  rename("Baseline"="Pons") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Pons_VOL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons)
  ) %>%
  mutate(Diff=(Pons-Baseline))  %>%
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

AllMSA_Pop_Baseline_671 %>% inner_join(Pons_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons) %>%
  rename("Baseline"="Pons") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Pons_VOL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons, TIME_STUDY)
  ) %>%
  mutate(Diff=(Pons-Baseline)/TIME_STUDY)  %>%
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



# -----------
# Overall MSA Entire  MSA P vs MSA C  -----------------------

# ALL  Pons Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(Pons_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Pons),
    sd=sd(Pons),
    median=median(Pons),
    Q1 = quantile(Pons, 0.25),
    Q3 = quantile(Pons, 0.75),
    min=min(Pons),
    max=max(Pons),
    n=n(),
    skewness = skewness(Pons, na.rm = TRUE),
    kurtosis = kurtosis(Pons, na.rm = TRUE),
    CI_lower = t.test(Pons)$conf.int[1],
    CI_upper = t.test(Pons)$conf.int[2],
    shapiro_w = shapiro.test(Pons)$statistic,
    shapiro_p = shapiro.test(Pons)$p.value
  )


# ALL  Pons Total Year 1

AllMSA_Pop_Baseline_671 %>% inner_join(Pons_VOL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Pons),
    sd=sd(Pons),
    median=median(Pons),
    Q1 = quantile(Pons, 0.25),
    Q3 = quantile(Pons, 0.75),
    min=min(Pons),
    max=max(Pons),
    n=n(),
    skewness = skewness(Pons, na.rm = TRUE),
    kurtosis = kurtosis(Pons, na.rm = TRUE),
    CI_lower = t.test(Pons)$conf.int[1],
    CI_upper = t.test(Pons)$conf.int[2],
    shapiro_w = shapiro.test(Pons)$statistic,
    shapiro_p = shapiro.test(Pons)$p.value
  )



# Elapsed Time Year 1


AllMSA_Pop_Baseline_671 %>% inner_join(Pons_VOL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons, DIAG, TIME_STUDY) %>%
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


AllMSA_Pop_Baseline_671 %>% inner_join(Pons_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons) %>%
  rename("Baseline"="Pons") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Pons_VOL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons, DIAG)
  ) %>%
  mutate(Diff=(Pons-Baseline))  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
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

AllMSA_Pop_Baseline_671 %>% inner_join(Pons_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons) %>%
  rename("Baseline"="Pons") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Pons_VOL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons, DIAG, TIME_STUDY)
  ) %>%
  mutate(Diff=(Pons-Baseline)/TIME_STUDY)  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )













# ALL  Pons Total Year 2

AllMSA_Pop_Baseline_671 %>% inner_join(Pons_VOL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Pons),
    sd=sd(Pons),
    median=median(Pons),
    Q1 = quantile(Pons, 0.25),
    Q3 = quantile(Pons, 0.75),
    min=min(Pons),
    max=max(Pons),
    n=n(),
    skewness = skewness(Pons, na.rm = TRUE),
    kurtosis = kurtosis(Pons, na.rm = TRUE),
    CI_lower = t.test(Pons)$conf.int[1],
    CI_upper = t.test(Pons)$conf.int[2],
    shapiro_w = shapiro.test(Pons)$statistic,
    shapiro_p = shapiro.test(Pons)$p.value
  )



# Elapsed Time Year 2


AllMSA_Pop_Baseline_671 %>% inner_join(Pons_VOL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons, DIAG, TIME_STUDY) %>%
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


AllMSA_Pop_Baseline_671 %>% inner_join(Pons_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons) %>%
  rename("Baseline"="Pons") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Pons_VOL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons, DIAG)
  ) %>%
  mutate(Diff=(Pons-Baseline))  %>% group_by(DIAG) %>%
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

AllMSA_Pop_Baseline_671 %>% inner_join(Pons_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons) %>%
  rename("Baseline"="Pons") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Pons_VOL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons, DIAG, TIME_STUDY)
  ) %>%
  mutate(Diff=(Pons-Baseline)/TIME_STUDY)  %>% filter(DIAG=="PD") %>%
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






# ALL  Pons Total Year 3

AllMSA_Pop_Baseline_671 %>% inner_join(Pons_VOL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Pons),
    sd=sd(Pons),
    median=median(Pons),
    Q1 = quantile(Pons, 0.25),
    Q3 = quantile(Pons, 0.75),
    min=min(Pons),
    max=max(Pons),
    n=n(),
    skewness = skewness(Pons, na.rm = TRUE),
    kurtosis = kurtosis(Pons, na.rm = TRUE),
    CI_lower = t.test(Pons)$conf.int[1],
    CI_upper = t.test(Pons)$conf.int[2],
    shapiro_w = shapiro.test(Pons)$statistic,
    shapiro_p = shapiro.test(Pons)$p.value
  )



# Elapsed Time Year 3


AllMSA_Pop_Baseline_671 %>% inner_join(Pons_VOL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons, DIAG, TIME_STUDY) %>%
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


AllMSA_Pop_Baseline_671 %>% inner_join(Pons_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons) %>%
  rename("Baseline"="Pons") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Pons_VOL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons, DIAG)
  ) %>%
  mutate(Diff=(Pons-Baseline))  %>% filter(DIAG=="PD") %>%
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

AllMSA_Pop_Baseline_671 %>% inner_join(Pons_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons) %>%
  rename("Baseline"="Pons") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Pons_VOL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons, DIAG, TIME_STUDY)
  ) %>%
  mutate(Diff=(Pons-Baseline)/TIME_STUDY)  %>% filter(DIAG=="PD") %>%
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



# -----------
# Overall MSA Entire  Probable vs Possible  -----------------------

# ALL  Pons Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(Pons_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Pons),
    sd=sd(Pons),
    median=median(Pons),
    Q1 = quantile(Pons, 0.25),
    Q3 = quantile(Pons, 0.75),
    min=min(Pons),
    max=max(Pons),
    n=n(),
    skewness = skewness(Pons, na.rm = TRUE),
    kurtosis = kurtosis(Pons, na.rm = TRUE),
    CI_lower = t.test(Pons)$conf.int[1],
    CI_upper = t.test(Pons)$conf.int[2],
    shapiro_w = shapiro.test(Pons)$statistic,
    shapiro_p = shapiro.test(Pons)$p.value
  )


# ALL  Pons Total Year 1

AllMSA_Pop_Baseline_671 %>% inner_join(Pons_VOL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Pons),
    sd=sd(Pons),
    median=median(Pons),
    Q1 = quantile(Pons, 0.25),
    Q3 = quantile(Pons, 0.75),
    min=min(Pons),
    max=max(Pons),
    n=n(),
    skewness = skewness(Pons, na.rm = TRUE),
    kurtosis = kurtosis(Pons, na.rm = TRUE),
    CI_lower = t.test(Pons)$conf.int[1],
    CI_upper = t.test(Pons)$conf.int[2],
    shapiro_w = shapiro.test(Pons)$statistic,
    shapiro_p = shapiro.test(Pons)$p.value
  )



# Elapsed Time Year 1


AllMSA_Pop_Baseline_671 %>% inner_join(Pons_VOL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons, DIAGNIV, TIME_STUDY) %>%
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


AllMSA_Pop_Baseline_671 %>% inner_join(Pons_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons) %>%
  rename("Baseline"="Pons") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Pons_VOL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons, DIAGNIV)
  ) %>%
  mutate(Diff=(Pons-Baseline))  %>% filter(DIAGNIV=="POS") %>%
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

AllMSA_Pop_Baseline_671 %>% inner_join(Pons_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons) %>%
  rename("Baseline"="Pons") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Pons_VOL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons, DIAGNIV, TIME_STUDY)
  ) %>%
  mutate(Diff=(Pons-Baseline)/TIME_STUDY)  %>% filter(DIAGNIV=="POS") %>%
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













# ALL  Pons Total Year 2

AllMSA_Pop_Baseline_671 %>% inner_join(Pons_VOL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Pons),
    sd=sd(Pons),
    median=median(Pons),
    Q1 = quantile(Pons, 0.25),
    Q3 = quantile(Pons, 0.75),
    min=min(Pons),
    max=max(Pons),
    n=n(),
    skewness = skewness(Pons, na.rm = TRUE),
    kurtosis = kurtosis(Pons, na.rm = TRUE),
    CI_lower = t.test(Pons)$conf.int[1],
    CI_upper = t.test(Pons)$conf.int[2],
    shapiro_w = shapiro.test(Pons)$statistic,
    shapiro_p = shapiro.test(Pons)$p.value
  )



# Elapsed Time Year 2


AllMSA_Pop_Baseline_671 %>% inner_join(Pons_VOL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons, DIAGNIV, TIME_STUDY) %>%
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


AllMSA_Pop_Baseline_671 %>% inner_join(Pons_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons) %>%
  rename("Baseline"="Pons") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Pons_VOL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons, DIAGNIV)
  ) %>%
  mutate(Diff=(Pons-Baseline))  %>% filter(DIAGNIV=="PROB") %>%
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

AllMSA_Pop_Baseline_671 %>% inner_join(Pons_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons) %>%
  rename("Baseline"="Pons") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Pons_VOL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons, DIAGNIV, TIME_STUDY)
  ) %>%
  mutate(Diff=(Pons-Baseline)/TIME_STUDY)  %>% filter(DIAGNIV=="PROB") %>%
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






# ALL  Pons Total Year 3

AllMSA_Pop_Baseline_671 %>% inner_join(Pons_VOL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Pons),
    sd=sd(Pons),
    median=median(Pons),
    Q1 = quantile(Pons, 0.25),
    Q3 = quantile(Pons, 0.75),
    min=min(Pons),
    max=max(Pons),
    n=n(),
    skewness = skewness(Pons, na.rm = TRUE),
    kurtosis = kurtosis(Pons, na.rm = TRUE),
    CI_lower = t.test(Pons)$conf.int[1],
    CI_upper = t.test(Pons)$conf.int[2],
    shapiro_w = shapiro.test(Pons)$statistic,
    shapiro_p = shapiro.test(Pons)$p.value
  )



# Elapsed Time Year 3


AllMSA_Pop_Baseline_671 %>% inner_join(Pons_VOL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons, DIAGNIV, TIME_STUDY) %>%
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


AllMSA_Pop_Baseline_671 %>% inner_join(Pons_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons) %>%
  rename("Baseline"="Pons") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Pons_VOL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons, DIAGNIV)
  ) %>%
  mutate(Diff=(Pons-Baseline))  %>% filter(DIAGNIV=="PROB") %>%
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

AllMSA_Pop_Baseline_671 %>% inner_join(Pons_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons) %>%
  rename("Baseline"="Pons") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Pons_VOL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons, DIAGNIV, TIME_STUDY)
  ) %>%
  mutate(Diff=(Pons-Baseline)/TIME_STUDY)  %>% filter(DIAGNIV=="PROB") %>%
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

# ---------------


# Target MSA Entire  -----------------------

# ALL  Pons Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(Pons_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(Pons),
    sd=sd(Pons),
    median=median(Pons),
    Q1 = quantile(Pons, 0.25),
    Q3 = quantile(Pons, 0.75),
    min=min(Pons),
    max=max(Pons),
    n=n(),
    skewness = skewness(Pons, na.rm = TRUE),
    kurtosis = kurtosis(Pons, na.rm = TRUE),
    CI_lower = t.test(Pons)$conf.int[1],
    CI_upper = t.test(Pons)$conf.int[2],
    shapiro_w = shapiro.test(Pons)$statistic,
    shapiro_p = shapiro.test(Pons)$p.value
  )


# ALL  Pons Total Year 1

EarlyCT_Pop_Baseline_319 %>% inner_join(Pons_VOL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(Pons),
    sd=sd(Pons),
    median=median(Pons),
    Q1 = quantile(Pons, 0.25),
    Q3 = quantile(Pons, 0.75),
    min=min(Pons),
    max=max(Pons),
    n=n(),
    skewness = skewness(Pons, na.rm = TRUE),
    kurtosis = kurtosis(Pons, na.rm = TRUE),
    CI_lower = t.test(Pons)$conf.int[1],
    CI_upper = t.test(Pons)$conf.int[2],
    shapiro_w = shapiro.test(Pons)$statistic,
    shapiro_p = shapiro.test(Pons)$p.value
  )



# Elapsed Time Year 1


EarlyCT_Pop_Baseline_319 %>% inner_join(Pons_VOL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons, TIME_STUDY) %>%
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


EarlyCT_Pop_Baseline_319 %>% inner_join(Pons_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons) %>%
  rename("Baseline"="Pons") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Pons_VOL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons)
  ) %>%
  mutate(Diff=(Pons-Baseline))  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
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

EarlyCT_Pop_Baseline_319 %>% inner_join(Pons_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons) %>%
  rename("Baseline"="Pons") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Pons_VOL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons, TIME_STUDY)
  ) %>%
  mutate(Diff=(Pons-Baseline)/TIME_STUDY)  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )













# ALL  Pons Total Year 2

EarlyCT_Pop_Baseline_319 %>% inner_join(Pons_VOL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(Pons),
    sd=sd(Pons),
    median=median(Pons),
    Q1 = quantile(Pons, 0.25),
    Q3 = quantile(Pons, 0.75),
    min=min(Pons),
    max=max(Pons),
    n=n(),
    skewness = skewness(Pons, na.rm = TRUE),
    kurtosis = kurtosis(Pons, na.rm = TRUE),
    CI_lower = t.test(Pons)$conf.int[1],
    CI_upper = t.test(Pons)$conf.int[2],
    shapiro_w = shapiro.test(Pons)$statistic,
    shapiro_p = shapiro.test(Pons)$p.value
  )



# Elapsed Time Year 2


EarlyCT_Pop_Baseline_319 %>% inner_join(Pons_VOL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons, TIME_STUDY) %>%
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


EarlyCT_Pop_Baseline_319 %>% inner_join(Pons_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons) %>%
  rename("Baseline"="Pons") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Pons_VOL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons)
  ) %>%
  mutate(Diff=(Pons-Baseline))  %>%
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

EarlyCT_Pop_Baseline_319 %>% inner_join(Pons_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons) %>%
  rename("Baseline"="Pons") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Pons_VOL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons, TIME_STUDY)
  ) %>%
  mutate(Diff=(Pons-Baseline)/TIME_STUDY)  %>%
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






# ALL  Pons Total Year 3

EarlyCT_Pop_Baseline_319 %>% inner_join(Pons_VOL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(Pons),
    sd=sd(Pons),
    median=median(Pons),
    Q1 = quantile(Pons, 0.25),
    Q3 = quantile(Pons, 0.75),
    min=min(Pons),
    max=max(Pons),
    n=n(),
    skewness = skewness(Pons, na.rm = TRUE),
    kurtosis = kurtosis(Pons, na.rm = TRUE),
    CI_lower = t.test(Pons)$conf.int[1],
    CI_upper = t.test(Pons)$conf.int[2],
    shapiro_w = shapiro.test(Pons)$statistic,
    shapiro_p = shapiro.test(Pons)$p.value
  )



# Elapsed Time Year 3


EarlyCT_Pop_Baseline_319 %>% inner_join(Pons_VOL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons, TIME_STUDY) %>%
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


EarlyCT_Pop_Baseline_319 %>% inner_join(Pons_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons) %>%
  rename("Baseline"="Pons") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Pons_VOL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons)
  ) %>%
  mutate(Diff=(Pons-Baseline))  %>%
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

EarlyCT_Pop_Baseline_319 %>% inner_join(Pons_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons) %>%
  rename("Baseline"="Pons") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Pons_VOL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons, TIME_STUDY)
  ) %>%
  mutate(Diff=(Pons-Baseline)/TIME_STUDY)  %>%
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



# -----------
# Target MSA Entire  MSA P vs MSA C  -----------------------

# ALL  Pons Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(Pons_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Pons),
    sd=sd(Pons),
    median=median(Pons),
    Q1 = quantile(Pons, 0.25),
    Q3 = quantile(Pons, 0.75),
    min=min(Pons),
    max=max(Pons),
    n=n(),
    skewness = skewness(Pons, na.rm = TRUE),
    kurtosis = kurtosis(Pons, na.rm = TRUE),
    CI_lower = t.test(Pons)$conf.int[1],
    CI_upper = t.test(Pons)$conf.int[2],
    shapiro_w = shapiro.test(Pons)$statistic,
    shapiro_p = shapiro.test(Pons)$p.value
  )


# ALL  Pons Total Year 1

EarlyCT_Pop_Baseline_319 %>% inner_join(Pons_VOL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Pons),
    sd=sd(Pons),
    median=median(Pons),
    Q1 = quantile(Pons, 0.25),
    Q3 = quantile(Pons, 0.75),
    min=min(Pons),
    max=max(Pons),
    n=n(),
    skewness = skewness(Pons, na.rm = TRUE),
    kurtosis = kurtosis(Pons, na.rm = TRUE),
    CI_lower = t.test(Pons)$conf.int[1],
    CI_upper = t.test(Pons)$conf.int[2],
    shapiro_w = shapiro.test(Pons)$statistic,
    shapiro_p = shapiro.test(Pons)$p.value
  )



# Elapsed Time Year 1


EarlyCT_Pop_Baseline_319 %>% inner_join(Pons_VOL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons, DIAG, TIME_STUDY) %>%
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


EarlyCT_Pop_Baseline_319 %>% inner_join(Pons_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons) %>%
  rename("Baseline"="Pons") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Pons_VOL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons, DIAG)
  ) %>%
  mutate(Diff=(Pons-Baseline))  %>% filter(DIAG=="CB") %>%
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

EarlyCT_Pop_Baseline_319 %>% inner_join(Pons_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons) %>%
  rename("Baseline"="Pons") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Pons_VOL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons, DIAG, TIME_STUDY)
  ) %>%
  mutate(Diff=(Pons-Baseline)/TIME_STUDY)  %>% filter(DIAG=="CB") %>%
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













# ALL  Pons Total Year 2

EarlyCT_Pop_Baseline_319 %>% inner_join(Pons_VOL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Pons),
    sd=sd(Pons),
    median=median(Pons),
    Q1 = quantile(Pons, 0.25),
    Q3 = quantile(Pons, 0.75),
    min=min(Pons),
    max=max(Pons),
    n=n(),
    skewness = skewness(Pons, na.rm = TRUE),
    kurtosis = kurtosis(Pons, na.rm = TRUE),
    CI_lower = t.test(Pons)$conf.int[1],
    CI_upper = t.test(Pons)$conf.int[2],
    shapiro_w = shapiro.test(Pons)$statistic,
    shapiro_p = shapiro.test(Pons)$p.value
  )



# Elapsed Time Year 2


EarlyCT_Pop_Baseline_319 %>% inner_join(Pons_VOL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons, DIAG, TIME_STUDY) %>%
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


EarlyCT_Pop_Baseline_319 %>% inner_join(Pons_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons) %>%
  rename("Baseline"="Pons") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Pons_VOL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons, DIAG)
  ) %>%
  mutate(Diff=(Pons-Baseline))  %>% filter(DIAG=="PD") %>%
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

EarlyCT_Pop_Baseline_319 %>% inner_join(Pons_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons) %>%
  rename("Baseline"="Pons") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Pons_VOL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons, DIAG, TIME_STUDY)
  ) %>%
  mutate(Diff=(Pons-Baseline)/TIME_STUDY)  %>% filter(DIAG=="PD") %>%
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






# ALL  Pons Total Year 3

EarlyCT_Pop_Baseline_319 %>% inner_join(Pons_VOL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Pons),
    sd=sd(Pons),
    median=median(Pons),
    Q1 = quantile(Pons, 0.25),
    Q3 = quantile(Pons, 0.75),
    min=min(Pons),
    max=max(Pons),
    n=n(),
    skewness = skewness(Pons, na.rm = TRUE),
    kurtosis = kurtosis(Pons, na.rm = TRUE),
    CI_lower = t.test(Pons)$conf.int[1],
    CI_upper = t.test(Pons)$conf.int[2],
    shapiro_w = shapiro.test(Pons)$statistic,
    shapiro_p = shapiro.test(Pons)$p.value
  )



# Elapsed Time Year 3


EarlyCT_Pop_Baseline_319 %>% inner_join(Pons_VOL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons, DIAG, TIME_STUDY) %>%
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


EarlyCT_Pop_Baseline_319 %>% inner_join(Pons_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons) %>%
  rename("Baseline"="Pons") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Pons_VOL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons, DIAG)
  ) %>%
  mutate(Diff=(Pons-Baseline))  %>% filter(DIAG=="PD") %>%
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

EarlyCT_Pop_Baseline_319 %>% inner_join(Pons_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons) %>%
  rename("Baseline"="Pons") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Pons_VOL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons, DIAG, TIME_STUDY)
  ) %>%
  mutate(Diff=(Pons-Baseline)/TIME_STUDY)  %>% filter(DIAG=="PD") %>%
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



# -----------
# Target MSA Entire  Probable vs Possible  -----------------------

# ALL  Pons Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(Pons_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Pons),
    sd=sd(Pons),
    median=median(Pons),
    Q1 = quantile(Pons, 0.25),
    Q3 = quantile(Pons, 0.75),
    min=min(Pons),
    max=max(Pons),
    n=n(),
    skewness = skewness(Pons, na.rm = TRUE),
    kurtosis = kurtosis(Pons, na.rm = TRUE),
    CI_lower = t.test(Pons)$conf.int[1],
    CI_upper = t.test(Pons)$conf.int[2],
    shapiro_w = shapiro.test(Pons)$statistic,
    shapiro_p = shapiro.test(Pons)$p.value
  )


# ALL  Pons Total Year 1

EarlyCT_Pop_Baseline_319 %>% inner_join(Pons_VOL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Pons),
    sd=sd(Pons),
    median=median(Pons),
    Q1 = quantile(Pons, 0.25),
    Q3 = quantile(Pons, 0.75),
    min=min(Pons),
    max=max(Pons),
    n=n(),
    skewness = skewness(Pons, na.rm = TRUE),
    kurtosis = kurtosis(Pons, na.rm = TRUE),
    CI_lower = t.test(Pons)$conf.int[1],
    CI_upper = t.test(Pons)$conf.int[2],
    shapiro_w = shapiro.test(Pons)$statistic,
    shapiro_p = shapiro.test(Pons)$p.value
  )



# Elapsed Time Year 1


EarlyCT_Pop_Baseline_319 %>% inner_join(Pons_VOL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons, DIAGNIV, TIME_STUDY) %>%
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


EarlyCT_Pop_Baseline_319 %>% inner_join(Pons_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons) %>%
  rename("Baseline"="Pons") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Pons_VOL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons, DIAGNIV)
  ) %>%
  mutate(Diff=(Pons-Baseline))  %>% filter(DIAGNIV=="PROB") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
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

EarlyCT_Pop_Baseline_319 %>% inner_join(Pons_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons) %>%
  rename("Baseline"="Pons") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Pons_VOL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons, DIAGNIV, TIME_STUDY)
  ) %>%
  mutate(Diff=(Pons-Baseline)/TIME_STUDY)  %>% filter(DIAGNIV=="PROB") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )













# ALL  Pons Total Year 2

EarlyCT_Pop_Baseline_319 %>% inner_join(Pons_VOL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% filter(DIAGNIV=="PROB") %>%
  summarise(
    mean=mean(Pons),
    sd=sd(Pons),
    median=median(Pons),
    Q1 = quantile(Pons, 0.25),
    Q3 = quantile(Pons, 0.75),
    min=min(Pons),
    max=max(Pons),
    n=n(),
    skewness = skewness(Pons, na.rm = TRUE),
    kurtosis = kurtosis(Pons, na.rm = TRUE),
    CI_lower = t.test(Pons)$conf.int[1],
    CI_upper = t.test(Pons)$conf.int[2],
    shapiro_w = shapiro.test(Pons)$statistic,
    shapiro_p = shapiro.test(Pons)$p.value
  )



# Elapsed Time Year 2


EarlyCT_Pop_Baseline_319 %>% inner_join(Pons_VOL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons, DIAGNIV, TIME_STUDY) %>%
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


EarlyCT_Pop_Baseline_319 %>% inner_join(Pons_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons) %>%
  rename("Baseline"="Pons") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Pons_VOL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons, DIAGNIV)
  ) %>%
  mutate(Diff=(Pons-Baseline))  %>% filter(DIAGNIV=="PROB") %>%
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

EarlyCT_Pop_Baseline_319 %>% inner_join(Pons_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons) %>%
  rename("Baseline"="Pons") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Pons_VOL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons, DIAGNIV, TIME_STUDY)
  ) %>%
  mutate(Diff=(Pons-Baseline)/TIME_STUDY)  %>% filter(DIAGNIV=="PROB") %>%
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






# ALL  Pons Total Year 3

EarlyCT_Pop_Baseline_319 %>% inner_join(Pons_VOL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% filter(DIAGNIV=="POS") %>%
  summarise(
    mean=mean(Pons),
    sd=sd(Pons),
    median=median(Pons),
    Q1 = quantile(Pons, 0.25),
    Q3 = quantile(Pons, 0.75),
    min=min(Pons),
    max=max(Pons),
    n=n(),
    #skewness = skewness(Pons, na.rm = TRUE),
    #kurtosis = kurtosis(Pons, na.rm = TRUE),
    #CI_lower = t.test(Pons)$conf.int[1],
    #CI_upper = t.test(Pons)$conf.int[2],
    #shapiro_w = shapiro.test(Pons)$statistic,
    #shapiro_p = shapiro.test(Pons)$p.value
  )



# Elapsed Time Year 3


EarlyCT_Pop_Baseline_319 %>% inner_join(Pons_VOL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons, DIAGNIV, TIME_STUDY) %>%
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


# Change from baseline Year 3


EarlyCT_Pop_Baseline_319 %>% inner_join(Pons_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons) %>%
  rename("Baseline"="Pons") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Pons_VOL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons, DIAGNIV)
  ) %>%
  mutate(Diff=(Pons-Baseline))  %>% filter(DIAGNIV=="PROB") %>%
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

EarlyCT_Pop_Baseline_319 %>% inner_join(Pons_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons) %>%
  rename("Baseline"="Pons") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Pons_VOL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons, DIAGNIV, TIME_STUDY)
  ) %>%
  mutate(Diff=(Pons-Baseline)/TIME_STUDY)  %>% filter(DIAGNIV=="PROB") %>%
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

# ---------------


# All data dates groups  --------
dataCohorteManaged <- readRDS("Source/dataCohorteManaged.rds")

Visits_zero <- dataCohorteManaged  %>% select(NUM, dateVisite0) %>% distinct()

AllMSA_Pop_Baseline_671 <- fread("Source/AllMSA_Pop_Baseline_671.txt")
EarlyCT_Pop_Baseline_319 <- fread("Source/EarlyCT_Pop_Baseline_319.txt")


# ----------
# Medulla Volume -----------------------------

irmForPaulo <- readRDS("Source/irmForPaulo.rds")

Medulla_VOL <- irmForPaulo %>% select(NUM, DateIRM, Medulla)

Medulla_VOL$Medulla <- as.numeric(Medulla_VOL$Medulla)

Medulla_VOL <- Visits_zero %>% inner_join(Medulla_VOL) %>%
  mutate(TIME_STUDY=time_length(difftime(DateIRM, dateVisite0 ), "years")) %>%
  mutate(Year= ifelse(TIME_STUDY<=0.5, 0,
                      ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                             ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                    ifelse(TIME_STUDY>=2.5 ,3, NA))))) 

Medulla_VOL <- Medulla_VOL %>% 
  drop_na() %>% arrange(NUM, DateIRM) %>%
  group_by(NUM, DateIRM) %>% mutate(Medulla=mean(Medulla)) %>%
  distinct()

# ----------
# Overall MSA Entire  -----------------------

# ALL  Medulla Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(Medulla_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(Medulla),
    sd=sd(Medulla),
    median=median(Medulla),
    Q1 = quantile(Medulla, 0.25),
    Q3 = quantile(Medulla, 0.75),
    min=min(Medulla),
    max=max(Medulla),
    n=n(),
    skewness = skewness(Medulla, na.rm = TRUE),
    kurtosis = kurtosis(Medulla, na.rm = TRUE),
    CI_lower = t.test(Medulla)$conf.int[1],
    CI_upper = t.test(Medulla)$conf.int[2],
    shapiro_w = shapiro.test(Medulla)$statistic,
    shapiro_p = shapiro.test(Medulla)$p.value
  )


# ALL  Medulla Total Year 1

AllMSA_Pop_Baseline_671 %>% inner_join(Medulla_VOL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(Medulla),
    sd=sd(Medulla),
    median=median(Medulla),
    Q1 = quantile(Medulla, 0.25),
    Q3 = quantile(Medulla, 0.75),
    min=min(Medulla),
    max=max(Medulla),
    n=n(),
    skewness = skewness(Medulla, na.rm = TRUE),
    kurtosis = kurtosis(Medulla, na.rm = TRUE),
    CI_lower = t.test(Medulla)$conf.int[1],
    CI_upper = t.test(Medulla)$conf.int[2],
    shapiro_w = shapiro.test(Medulla)$statistic,
    shapiro_p = shapiro.test(Medulla)$p.value
  )



# Elapsed Time Year 1


AllMSA_Pop_Baseline_671 %>% inner_join(Medulla_VOL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla, TIME_STUDY) %>%
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


AllMSA_Pop_Baseline_671 %>% inner_join(Medulla_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla) %>%
  rename("Baseline"="Medulla") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Medulla_VOL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla)
  ) %>%
  mutate(Diff=(Medulla-Baseline))  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
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

AllMSA_Pop_Baseline_671 %>% inner_join(Medulla_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla) %>%
  rename("Baseline"="Medulla") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Medulla_VOL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla, TIME_STUDY)
  ) %>%
  mutate(Diff=(Medulla-Baseline)/TIME_STUDY)  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )













# ALL  Medulla Total Year 2

AllMSA_Pop_Baseline_671 %>% inner_join(Medulla_VOL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(Medulla),
    sd=sd(Medulla),
    median=median(Medulla),
    Q1 = quantile(Medulla, 0.25),
    Q3 = quantile(Medulla, 0.75),
    min=min(Medulla),
    max=max(Medulla),
    n=n(),
    skewness = skewness(Medulla, na.rm = TRUE),
    kurtosis = kurtosis(Medulla, na.rm = TRUE),
    CI_lower = t.test(Medulla)$conf.int[1],
    CI_upper = t.test(Medulla)$conf.int[2],
    shapiro_w = shapiro.test(Medulla)$statistic,
    shapiro_p = shapiro.test(Medulla)$p.value
  )



# Elapsed Time Year 2


AllMSA_Pop_Baseline_671 %>% inner_join(Medulla_VOL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla, TIME_STUDY) %>%
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


AllMSA_Pop_Baseline_671 %>% inner_join(Medulla_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla) %>%
  rename("Baseline"="Medulla") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Medulla_VOL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla)
  ) %>%
  mutate(Diff=(Medulla-Baseline))  %>%
  summarise(
    mean=mean(Diff),
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

AllMSA_Pop_Baseline_671 %>% inner_join(Medulla_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla) %>%
  rename("Baseline"="Medulla") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Medulla_VOL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla, TIME_STUDY)
  ) %>%
  mutate(Diff=(Medulla-Baseline)/TIME_STUDY)  %>%
  summarise(
    mean=mean(Diff),
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






# ALL  Medulla Total Year 3

AllMSA_Pop_Baseline_671 %>% inner_join(Medulla_VOL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(Medulla),
    sd=sd(Medulla),
    median=median(Medulla),
    Q1 = quantile(Medulla, 0.25),
    Q3 = quantile(Medulla, 0.75),
    min=min(Medulla),
    max=max(Medulla),
    n=n(),
    skewness = skewness(Medulla, na.rm = TRUE),
    kurtosis = kurtosis(Medulla, na.rm = TRUE),
    CI_lower = t.test(Medulla)$conf.int[1],
    CI_upper = t.test(Medulla)$conf.int[2],
    shapiro_w = shapiro.test(Medulla)$statistic,
    shapiro_p = shapiro.test(Medulla)$p.value
  )



# Elapsed Time Year 3


AllMSA_Pop_Baseline_671 %>% inner_join(Medulla_VOL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla, TIME_STUDY) %>%
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


AllMSA_Pop_Baseline_671 %>% inner_join(Medulla_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla) %>%
  rename("Baseline"="Medulla") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Medulla_VOL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla)
  ) %>%
  mutate(Diff=(Medulla-Baseline))  %>%
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

AllMSA_Pop_Baseline_671 %>% inner_join(Medulla_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla) %>%
  rename("Baseline"="Medulla") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Medulla_VOL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla, TIME_STUDY)
  ) %>%
  mutate(Diff=(Medulla-Baseline)/TIME_STUDY)  %>%
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



# -----------
# Overall MSA Entire  MSA P vs MSA C  -----------------------

# ALL  Medulla Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(Medulla_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Medulla),
    sd=sd(Medulla),
    median=median(Medulla),
    Q1 = quantile(Medulla, 0.25),
    Q3 = quantile(Medulla, 0.75),
    min=min(Medulla),
    max=max(Medulla),
    n=n(),
    skewness = skewness(Medulla, na.rm = TRUE),
    kurtosis = kurtosis(Medulla, na.rm = TRUE),
    CI_lower = t.test(Medulla)$conf.int[1],
    CI_upper = t.test(Medulla)$conf.int[2],
    shapiro_w = shapiro.test(Medulla)$statistic,
    shapiro_p = shapiro.test(Medulla)$p.value
  )


# ALL  Medulla Total Year 1

AllMSA_Pop_Baseline_671 %>% inner_join(Medulla_VOL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Medulla),
    sd=sd(Medulla),
    median=median(Medulla),
    Q1 = quantile(Medulla, 0.25),
    Q3 = quantile(Medulla, 0.75),
    min=min(Medulla),
    max=max(Medulla),
    n=n(),
    skewness = skewness(Medulla, na.rm = TRUE),
    kurtosis = kurtosis(Medulla, na.rm = TRUE),
    CI_lower = t.test(Medulla)$conf.int[1],
    CI_upper = t.test(Medulla)$conf.int[2],
    shapiro_w = shapiro.test(Medulla)$statistic,
    shapiro_p = shapiro.test(Medulla)$p.value
  )



# Elapsed Time Year 1


AllMSA_Pop_Baseline_671 %>% inner_join(Medulla_VOL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla, DIAG, TIME_STUDY) %>%
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


AllMSA_Pop_Baseline_671 %>% inner_join(Medulla_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla) %>%
  rename("Baseline"="Medulla") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Medulla_VOL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla, DIAG)
  ) %>%
  mutate(Diff=(Medulla-Baseline))  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
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

AllMSA_Pop_Baseline_671 %>% inner_join(Medulla_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla) %>%
  rename("Baseline"="Medulla") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Medulla_VOL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla, DIAG, TIME_STUDY)
  ) %>%
  mutate(Diff=(Medulla-Baseline)/TIME_STUDY)  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )













# ALL  Medulla Total Year 2

AllMSA_Pop_Baseline_671 %>% inner_join(Medulla_VOL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Medulla),
    sd=sd(Medulla),
    median=median(Medulla),
    Q1 = quantile(Medulla, 0.25),
    Q3 = quantile(Medulla, 0.75),
    min=min(Medulla),
    max=max(Medulla),
    n=n(),
    skewness = skewness(Medulla, na.rm = TRUE),
    kurtosis = kurtosis(Medulla, na.rm = TRUE),
    CI_lower = t.test(Medulla)$conf.int[1],
    CI_upper = t.test(Medulla)$conf.int[2],
    shapiro_w = shapiro.test(Medulla)$statistic,
    shapiro_p = shapiro.test(Medulla)$p.value
  )



# Elapsed Time Year 2


AllMSA_Pop_Baseline_671 %>% inner_join(Medulla_VOL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla, DIAG, TIME_STUDY) %>%
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


AllMSA_Pop_Baseline_671 %>% inner_join(Medulla_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla) %>%
  rename("Baseline"="Medulla") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Medulla_VOL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla, DIAG)
  ) %>%
  mutate(Diff=(Medulla-Baseline))  %>% group_by(DIAG) %>%
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

AllMSA_Pop_Baseline_671 %>% inner_join(Medulla_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla) %>%
  rename("Baseline"="Medulla") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Medulla_VOL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla, DIAG, TIME_STUDY)
  ) %>%
  mutate(Diff=(Medulla-Baseline)/TIME_STUDY)  %>% filter(DIAG=="PD") %>%
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






# ALL  Medulla Total Year 3

AllMSA_Pop_Baseline_671 %>% inner_join(Medulla_VOL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Medulla),
    sd=sd(Medulla),
    median=median(Medulla),
    Q1 = quantile(Medulla, 0.25),
    Q3 = quantile(Medulla, 0.75),
    min=min(Medulla),
    max=max(Medulla),
    n=n(),
    skewness = skewness(Medulla, na.rm = TRUE),
    kurtosis = kurtosis(Medulla, na.rm = TRUE),
    CI_lower = t.test(Medulla)$conf.int[1],
    CI_upper = t.test(Medulla)$conf.int[2],
    shapiro_w = shapiro.test(Medulla)$statistic,
    shapiro_p = shapiro.test(Medulla)$p.value
  )



# Elapsed Time Year 3


AllMSA_Pop_Baseline_671 %>% inner_join(Medulla_VOL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla, DIAG, TIME_STUDY) %>%
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


AllMSA_Pop_Baseline_671 %>% inner_join(Medulla_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla) %>%
  rename("Baseline"="Medulla") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Medulla_VOL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla, DIAG)
  ) %>%
  mutate(Diff=(Medulla-Baseline))  %>% filter(DIAG=="PD") %>%
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

AllMSA_Pop_Baseline_671 %>% inner_join(Medulla_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla) %>%
  rename("Baseline"="Medulla") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Medulla_VOL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla, DIAG, TIME_STUDY)
  ) %>%
  mutate(Diff=(Medulla-Baseline)/TIME_STUDY)  %>% filter(DIAG=="PD") %>%
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



# -----------
# Overall MSA Entire  Probable vs Possible  -----------------------

# ALL  Medulla Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(Medulla_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Medulla),
    sd=sd(Medulla),
    median=median(Medulla),
    Q1 = quantile(Medulla, 0.25),
    Q3 = quantile(Medulla, 0.75),
    min=min(Medulla),
    max=max(Medulla),
    n=n(),
    skewness = skewness(Medulla, na.rm = TRUE),
    kurtosis = kurtosis(Medulla, na.rm = TRUE),
    CI_lower = t.test(Medulla)$conf.int[1],
    CI_upper = t.test(Medulla)$conf.int[2],
    shapiro_w = shapiro.test(Medulla)$statistic,
    shapiro_p = shapiro.test(Medulla)$p.value
  )


# ALL  Medulla Total Year 1

AllMSA_Pop_Baseline_671 %>% inner_join(Medulla_VOL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Medulla),
    sd=sd(Medulla),
    median=median(Medulla),
    Q1 = quantile(Medulla, 0.25),
    Q3 = quantile(Medulla, 0.75),
    min=min(Medulla),
    max=max(Medulla),
    n=n(),
    skewness = skewness(Medulla, na.rm = TRUE),
    kurtosis = kurtosis(Medulla, na.rm = TRUE),
    CI_lower = t.test(Medulla)$conf.int[1],
    CI_upper = t.test(Medulla)$conf.int[2],
    shapiro_w = shapiro.test(Medulla)$statistic,
    shapiro_p = shapiro.test(Medulla)$p.value
  )



# Elapsed Time Year 1


AllMSA_Pop_Baseline_671 %>% inner_join(Medulla_VOL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla, DIAGNIV, TIME_STUDY) %>%
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


AllMSA_Pop_Baseline_671 %>% inner_join(Medulla_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla) %>%
  rename("Baseline"="Medulla") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Medulla_VOL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla, DIAGNIV)
  ) %>%
  mutate(Diff=(Medulla-Baseline))  %>% filter(DIAGNIV=="PROB") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
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

AllMSA_Pop_Baseline_671 %>% inner_join(Medulla_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla) %>%
  rename("Baseline"="Medulla") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Medulla_VOL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla, DIAGNIV, TIME_STUDY)
  ) %>%
  mutate(Diff=(Medulla-Baseline)/TIME_STUDY)  %>% filter(DIAGNIV=="PROB") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )













# ALL  Medulla Total Year 2

AllMSA_Pop_Baseline_671 %>% inner_join(Medulla_VOL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Medulla),
    sd=sd(Medulla),
    median=median(Medulla),
    Q1 = quantile(Medulla, 0.25),
    Q3 = quantile(Medulla, 0.75),
    min=min(Medulla),
    max=max(Medulla),
    n=n(),
    skewness = skewness(Medulla, na.rm = TRUE),
    kurtosis = kurtosis(Medulla, na.rm = TRUE),
    CI_lower = t.test(Medulla)$conf.int[1],
    CI_upper = t.test(Medulla)$conf.int[2],
    shapiro_w = shapiro.test(Medulla)$statistic,
    shapiro_p = shapiro.test(Medulla)$p.value
  )



# Elapsed Time Year 2


AllMSA_Pop_Baseline_671 %>% inner_join(Medulla_VOL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla, DIAGNIV, TIME_STUDY) %>%
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


AllMSA_Pop_Baseline_671 %>% inner_join(Medulla_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla) %>%
  rename("Baseline"="Medulla") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Medulla_VOL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla, DIAGNIV)
  ) %>%
  mutate(Diff=(Medulla-Baseline))  %>% filter(DIAGNIV=="PROB") %>%
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

AllMSA_Pop_Baseline_671 %>% inner_join(Medulla_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla) %>%
  rename("Baseline"="Medulla") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Medulla_VOL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla, DIAGNIV, TIME_STUDY)
  ) %>%
  mutate(Diff=(Medulla-Baseline)/TIME_STUDY)  %>% filter(DIAGNIV=="PROB") %>%
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






# ALL  Medulla Total Year 3

AllMSA_Pop_Baseline_671 %>% inner_join(Medulla_VOL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Medulla),
    sd=sd(Medulla),
    median=median(Medulla),
    Q1 = quantile(Medulla, 0.25),
    Q3 = quantile(Medulla, 0.75),
    min=min(Medulla),
    max=max(Medulla),
    n=n(),
    skewness = skewness(Medulla, na.rm = TRUE),
    kurtosis = kurtosis(Medulla, na.rm = TRUE),
    CI_lower = t.test(Medulla)$conf.int[1],
    CI_upper = t.test(Medulla)$conf.int[2],
    shapiro_w = shapiro.test(Medulla)$statistic,
    shapiro_p = shapiro.test(Medulla)$p.value
  )



# Elapsed Time Year 3


AllMSA_Pop_Baseline_671 %>% inner_join(Medulla_VOL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla, DIAGNIV, TIME_STUDY) %>%
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


AllMSA_Pop_Baseline_671 %>% inner_join(Medulla_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla) %>%
  rename("Baseline"="Medulla") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Medulla_VOL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla, DIAGNIV)
  ) %>%
  mutate(Diff=(Medulla-Baseline))  %>% filter(DIAGNIV=="PROB") %>%
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

AllMSA_Pop_Baseline_671 %>% inner_join(Medulla_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla) %>%
  rename("Baseline"="Medulla") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Medulla_VOL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla, DIAGNIV, TIME_STUDY)
  ) %>%
  mutate(Diff=(Medulla-Baseline)/TIME_STUDY)  %>% filter(DIAGNIV=="PROB") %>%
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

# ---------------


# Target MSA Entire  -----------------------

# ALL  Medulla Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(Medulla_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(Medulla),
    sd=sd(Medulla),
    median=median(Medulla),
    Q1 = quantile(Medulla, 0.25),
    Q3 = quantile(Medulla, 0.75),
    min=min(Medulla),
    max=max(Medulla),
    n=n(),
    skewness = skewness(Medulla, na.rm = TRUE),
    kurtosis = kurtosis(Medulla, na.rm = TRUE),
    CI_lower = t.test(Medulla)$conf.int[1],
    CI_upper = t.test(Medulla)$conf.int[2],
    shapiro_w = shapiro.test(Medulla)$statistic,
    shapiro_p = shapiro.test(Medulla)$p.value
  )


# ALL  Medulla Total Year 1

EarlyCT_Pop_Baseline_319 %>% inner_join(Medulla_VOL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(Medulla),
    sd=sd(Medulla),
    median=median(Medulla),
    Q1 = quantile(Medulla, 0.25),
    Q3 = quantile(Medulla, 0.75),
    min=min(Medulla),
    max=max(Medulla),
    n=n(),
    skewness = skewness(Medulla, na.rm = TRUE),
    kurtosis = kurtosis(Medulla, na.rm = TRUE),
    CI_lower = t.test(Medulla)$conf.int[1],
    CI_upper = t.test(Medulla)$conf.int[2],
    shapiro_w = shapiro.test(Medulla)$statistic,
    shapiro_p = shapiro.test(Medulla)$p.value
  )



# Elapsed Time Year 1


EarlyCT_Pop_Baseline_319 %>% inner_join(Medulla_VOL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla, TIME_STUDY) %>%
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


EarlyCT_Pop_Baseline_319 %>% inner_join(Medulla_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla) %>%
  rename("Baseline"="Medulla") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Medulla_VOL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla)
  ) %>%
  mutate(Diff=(Medulla-Baseline))  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
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

EarlyCT_Pop_Baseline_319 %>% inner_join(Medulla_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla) %>%
  rename("Baseline"="Medulla") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Medulla_VOL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla, TIME_STUDY)
  ) %>%
  mutate(Diff=(Medulla-Baseline)/TIME_STUDY)  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )













# ALL  Medulla Total Year 2

EarlyCT_Pop_Baseline_319 %>% inner_join(Medulla_VOL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(Medulla),
    sd=sd(Medulla),
    median=median(Medulla),
    Q1 = quantile(Medulla, 0.25),
    Q3 = quantile(Medulla, 0.75),
    min=min(Medulla),
    max=max(Medulla),
    n=n(),
    skewness = skewness(Medulla, na.rm = TRUE),
    kurtosis = kurtosis(Medulla, na.rm = TRUE),
    CI_lower = t.test(Medulla)$conf.int[1],
    CI_upper = t.test(Medulla)$conf.int[2],
    shapiro_w = shapiro.test(Medulla)$statistic,
    shapiro_p = shapiro.test(Medulla)$p.value
  )



# Elapsed Time Year 2


EarlyCT_Pop_Baseline_319 %>% inner_join(Medulla_VOL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla, TIME_STUDY) %>%
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


EarlyCT_Pop_Baseline_319 %>% inner_join(Medulla_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla) %>%
  rename("Baseline"="Medulla") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Medulla_VOL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla)
  ) %>%
  mutate(Diff=(Medulla-Baseline))  %>%
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

EarlyCT_Pop_Baseline_319 %>% inner_join(Medulla_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla) %>%
  rename("Baseline"="Medulla") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Medulla_VOL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla, TIME_STUDY)
  ) %>%
  mutate(Diff=(Medulla-Baseline)/TIME_STUDY)  %>%
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






# ALL  Medulla Total Year 3

EarlyCT_Pop_Baseline_319 %>% inner_join(Medulla_VOL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(Medulla),
    sd=sd(Medulla),
    median=median(Medulla),
    Q1 = quantile(Medulla, 0.25),
    Q3 = quantile(Medulla, 0.75),
    min=min(Medulla),
    max=max(Medulla),
    n=n(),
    skewness = skewness(Medulla, na.rm = TRUE),
    kurtosis = kurtosis(Medulla, na.rm = TRUE),
    CI_lower = t.test(Medulla)$conf.int[1],
    CI_upper = t.test(Medulla)$conf.int[2],
    shapiro_w = shapiro.test(Medulla)$statistic,
    shapiro_p = shapiro.test(Medulla)$p.value
  )



# Elapsed Time Year 3


EarlyCT_Pop_Baseline_319 %>% inner_join(Medulla_VOL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla, TIME_STUDY) %>%
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


EarlyCT_Pop_Baseline_319 %>% inner_join(Medulla_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla) %>%
  rename("Baseline"="Medulla") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Medulla_VOL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla)
  ) %>%
  mutate(Diff=(Medulla-Baseline))  %>%
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

EarlyCT_Pop_Baseline_319 %>% inner_join(Medulla_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla) %>%
  rename("Baseline"="Medulla") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Medulla_VOL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla, TIME_STUDY)
  ) %>%
  mutate(Diff=(Medulla-Baseline)/TIME_STUDY)  %>%
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



# -----------
# Target MSA Entire  MSA P vs MSA C  -----------------------

# ALL  Medulla Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(Medulla_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Medulla),
    sd=sd(Medulla),
    median=median(Medulla),
    Q1 = quantile(Medulla, 0.25),
    Q3 = quantile(Medulla, 0.75),
    min=min(Medulla),
    max=max(Medulla),
    n=n(),
    skewness = skewness(Medulla, na.rm = TRUE),
    kurtosis = kurtosis(Medulla, na.rm = TRUE),
    CI_lower = t.test(Medulla)$conf.int[1],
    CI_upper = t.test(Medulla)$conf.int[2],
    shapiro_w = shapiro.test(Medulla)$statistic,
    shapiro_p = shapiro.test(Medulla)$p.value
  )


# ALL  Medulla Total Year 1

EarlyCT_Pop_Baseline_319 %>% inner_join(Medulla_VOL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Medulla),
    sd=sd(Medulla),
    median=median(Medulla),
    Q1 = quantile(Medulla, 0.25),
    Q3 = quantile(Medulla, 0.75),
    min=min(Medulla),
    max=max(Medulla),
    n=n(),
    skewness = skewness(Medulla, na.rm = TRUE),
    kurtosis = kurtosis(Medulla, na.rm = TRUE),
    CI_lower = t.test(Medulla)$conf.int[1],
    CI_upper = t.test(Medulla)$conf.int[2],
    shapiro_w = shapiro.test(Medulla)$statistic,
    shapiro_p = shapiro.test(Medulla)$p.value
  )



# Elapsed Time Year 1


EarlyCT_Pop_Baseline_319 %>% inner_join(Medulla_VOL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla, DIAG, TIME_STUDY) %>%
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


EarlyCT_Pop_Baseline_319 %>% inner_join(Medulla_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla) %>%
  rename("Baseline"="Medulla") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Medulla_VOL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla, DIAG)
  ) %>%
  mutate(Diff=(Medulla-Baseline))  %>% filter(DIAG=="PD") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
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

EarlyCT_Pop_Baseline_319 %>% inner_join(Medulla_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla) %>%
  rename("Baseline"="Medulla") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Medulla_VOL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla, DIAG, TIME_STUDY)
  ) %>%
  mutate(Diff=(Medulla-Baseline)/TIME_STUDY)  %>% filter(DIAG=="PD") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )













# ALL  Medulla Total Year 2

EarlyCT_Pop_Baseline_319 %>% inner_join(Medulla_VOL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Medulla),
    sd=sd(Medulla),
    median=median(Medulla),
    Q1 = quantile(Medulla, 0.25),
    Q3 = quantile(Medulla, 0.75),
    min=min(Medulla),
    max=max(Medulla),
    n=n(),
    skewness = skewness(Medulla, na.rm = TRUE),
    kurtosis = kurtosis(Medulla, na.rm = TRUE),
    CI_lower = t.test(Medulla)$conf.int[1],
    CI_upper = t.test(Medulla)$conf.int[2],
    shapiro_w = shapiro.test(Medulla)$statistic,
    shapiro_p = shapiro.test(Medulla)$p.value
  )



# Elapsed Time Year 2


EarlyCT_Pop_Baseline_319 %>% inner_join(Medulla_VOL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla, DIAG, TIME_STUDY) %>%
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


EarlyCT_Pop_Baseline_319 %>% inner_join(Medulla_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla) %>%
  rename("Baseline"="Medulla") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Medulla_VOL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla, DIAG)
  ) %>%
  mutate(Diff=(Medulla-Baseline))  %>% filter(DIAG=="PD") %>%
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

EarlyCT_Pop_Baseline_319 %>% inner_join(Medulla_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla) %>%
  rename("Baseline"="Medulla") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Medulla_VOL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla, DIAG, TIME_STUDY)
  ) %>%
  mutate(Diff=(Medulla-Baseline)/TIME_STUDY)  %>% filter(DIAG=="PD") %>%
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






# ALL  Medulla Total Year 3

EarlyCT_Pop_Baseline_319 %>% inner_join(Medulla_VOL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Medulla),
    sd=sd(Medulla),
    median=median(Medulla),
    Q1 = quantile(Medulla, 0.25),
    Q3 = quantile(Medulla, 0.75),
    min=min(Medulla),
    max=max(Medulla),
    n=n(),
    skewness = skewness(Medulla, na.rm = TRUE),
    kurtosis = kurtosis(Medulla, na.rm = TRUE),
    CI_lower = t.test(Medulla)$conf.int[1],
    CI_upper = t.test(Medulla)$conf.int[2],
    shapiro_w = shapiro.test(Medulla)$statistic,
    shapiro_p = shapiro.test(Medulla)$p.value
  )



# Elapsed Time Year 3


EarlyCT_Pop_Baseline_319 %>% inner_join(Medulla_VOL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla, DIAG, TIME_STUDY) %>%
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


EarlyCT_Pop_Baseline_319 %>% inner_join(Medulla_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla) %>%
  rename("Baseline"="Medulla") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Medulla_VOL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla, DIAG)
  ) %>%
  mutate(Diff=(Medulla-Baseline))  %>% filter(DIAG=="PD") %>%
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

EarlyCT_Pop_Baseline_319 %>% inner_join(Medulla_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla) %>%
  rename("Baseline"="Medulla") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Medulla_VOL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla, DIAG, TIME_STUDY)
  ) %>%
  mutate(Diff=(Medulla-Baseline)/TIME_STUDY)  %>% filter(DIAG=="PD") %>%
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



# -----------
# Target MSA Entire  Probable vs Possible  -----------------------

# ALL  Medulla Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(Medulla_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Medulla),
    sd=sd(Medulla),
    median=median(Medulla),
    Q1 = quantile(Medulla, 0.25),
    Q3 = quantile(Medulla, 0.75),
    min=min(Medulla),
    max=max(Medulla),
    n=n(),
    skewness = skewness(Medulla, na.rm = TRUE),
    kurtosis = kurtosis(Medulla, na.rm = TRUE),
    CI_lower = t.test(Medulla)$conf.int[1],
    CI_upper = t.test(Medulla)$conf.int[2],
    shapiro_w = shapiro.test(Medulla)$statistic,
    shapiro_p = shapiro.test(Medulla)$p.value
  )


# ALL  Medulla Total Year 1

EarlyCT_Pop_Baseline_319 %>% inner_join(Medulla_VOL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Medulla),
    sd=sd(Medulla),
    median=median(Medulla),
    Q1 = quantile(Medulla, 0.25),
    Q3 = quantile(Medulla, 0.75),
    min=min(Medulla),
    max=max(Medulla),
    n=n(),
    skewness = skewness(Medulla, na.rm = TRUE),
    kurtosis = kurtosis(Medulla, na.rm = TRUE),
    CI_lower = t.test(Medulla)$conf.int[1],
    CI_upper = t.test(Medulla)$conf.int[2],
    shapiro_w = shapiro.test(Medulla)$statistic,
    shapiro_p = shapiro.test(Medulla)$p.value
  )



# Elapsed Time Year 1


EarlyCT_Pop_Baseline_319 %>% inner_join(Medulla_VOL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla, DIAGNIV, TIME_STUDY) %>%
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


EarlyCT_Pop_Baseline_319 %>% inner_join(Medulla_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla) %>%
  rename("Baseline"="Medulla") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Medulla_VOL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla, DIAGNIV)
  ) %>%
  mutate(Diff=(Medulla-Baseline))  %>% filter(DIAGNIV=="POS") %>%
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

EarlyCT_Pop_Baseline_319 %>% inner_join(Medulla_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla) %>%
  rename("Baseline"="Medulla") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Medulla_VOL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla, DIAGNIV, TIME_STUDY)
  ) %>%
  mutate(Diff=(Medulla-Baseline)/TIME_STUDY)  %>% filter(DIAGNIV=="POS") %>%
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













# ALL  Medulla Total Year 2

EarlyCT_Pop_Baseline_319 %>% inner_join(Medulla_VOL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% filter(DIAGNIV=="POS") %>%
  summarise(
    mean=mean(Medulla),
    sd=sd(Medulla),
    median=median(Medulla),
    Q1 = quantile(Medulla, 0.25),
    Q3 = quantile(Medulla, 0.75),
    min=min(Medulla),
    max=max(Medulla),
    n=n(),
    #skewness = skewness(Medulla, na.rm = TRUE),
    #kurtosis = kurtosis(Medulla, na.rm = TRUE),
    #CI_lower = t.test(Medulla)$conf.int[1],
    #CI_upper = t.test(Medulla)$conf.int[2],
    #shapiro_w = shapiro.test(Medulla)$statistic,
    #shapiro_p = shapiro.test(Medulla)$p.value
  )



# Elapsed Time Year 2


EarlyCT_Pop_Baseline_319 %>% inner_join(Medulla_VOL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla, DIAGNIV, TIME_STUDY) %>%
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


EarlyCT_Pop_Baseline_319 %>% inner_join(Medulla_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla) %>%
  rename("Baseline"="Medulla") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Medulla_VOL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla, DIAGNIV)
  ) %>%
  mutate(Diff=(Medulla-Baseline))  %>% filter(DIAGNIV=="PROB") %>%
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

EarlyCT_Pop_Baseline_319 %>% inner_join(Medulla_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla) %>%
  rename("Baseline"="Medulla") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Medulla_VOL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla, DIAGNIV, TIME_STUDY)
  ) %>%
  mutate(Diff=(Medulla-Baseline)/TIME_STUDY)  %>% filter(DIAGNIV=="PROB") %>%
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






# ALL  Medulla Total Year 3

EarlyCT_Pop_Baseline_319 %>% inner_join(Medulla_VOL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% filter(DIAGNIV=="PROB") %>%
  summarise(
    mean=mean(Medulla),
    sd=sd(Medulla),
    median=median(Medulla),
    Q1 = quantile(Medulla, 0.25),
    Q3 = quantile(Medulla, 0.75),
    min=min(Medulla),
    max=max(Medulla),
    n=n(),
    skewness = skewness(Medulla, na.rm = TRUE),
    kurtosis = kurtosis(Medulla, na.rm = TRUE),
    CI_lower = t.test(Medulla)$conf.int[1],
    CI_upper = t.test(Medulla)$conf.int[2],
    shapiro_w = shapiro.test(Medulla)$statistic,
    shapiro_p = shapiro.test(Medulla)$p.value
  )



# Elapsed Time Year 3


EarlyCT_Pop_Baseline_319 %>% inner_join(Medulla_VOL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla, DIAGNIV, TIME_STUDY) %>%
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


# Change from baseline Year 3


EarlyCT_Pop_Baseline_319 %>% inner_join(Medulla_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla) %>%
  rename("Baseline"="Medulla") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Medulla_VOL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla, DIAGNIV)
  ) %>%
  mutate(Diff=(Medulla-Baseline))  %>% filter(DIAGNIV=="PROB") %>%
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

EarlyCT_Pop_Baseline_319 %>% inner_join(Medulla_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla) %>%
  rename("Baseline"="Medulla") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Medulla_VOL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla, DIAGNIV, TIME_STUDY)
  ) %>%
  mutate(Diff=(Medulla-Baseline)/TIME_STUDY)  %>% filter(DIAGNIV=="PROB") %>%
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

# ---------------


# All data dates groups  --------
dataCohorteManaged <- readRDS("Source/dataCohorteManaged.rds")

Visits_zero <- dataCohorteManaged  %>% select(NUM, dateVisite0) %>% distinct()

AllMSA_Pop_Baseline_671 <- fread("Source/AllMSA_Pop_Baseline_671.txt")
EarlyCT_Pop_Baseline_319 <- fread("Source/EarlyCT_Pop_Baseline_319.txt")


# ----------
# Total Volume -----------------------------

irmForPaulo <- readRDS("Source/irmForPaulo.rds")
Total_VOL <- irmForPaulo %>% select(NUM, DateIRM, CerebralWhiteMatterVol, TotalGrayVol)

Total_VOL <- Total_VOL %>% mutate(Total=as.numeric(CerebralWhiteMatterVol)+as.numeric(TotalGrayVol) )

Total_VOL <- Visits_zero %>% inner_join(Total_VOL) %>%
  mutate(TIME_STUDY=time_length(difftime(DateIRM, dateVisite0 ), "years")) %>%
  mutate(Year= ifelse(TIME_STUDY<=0.5, 0,
                      ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                             ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                    ifelse(TIME_STUDY>=2.5 ,3, NA))))) 

Total_VOL <- Total_VOL %>% 
  drop_na() %>% arrange(NUM, DateIRM) %>%
  group_by(NUM, DateIRM) %>% mutate(Total=mean(Total)) %>%
  distinct()

# ----------
# Overall MSA Entire  -----------------------

# ALL  Total Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(Total_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(Total),
    sd=sd(Total),
    median=median(Total),
    Q1 = quantile(Total, 0.25),
    Q3 = quantile(Total, 0.75),
    min=min(Total),
    max=max(Total),
    n=n(),
    skewness = skewness(Total, na.rm = TRUE),
    kurtosis = kurtosis(Total, na.rm = TRUE),
    CI_lower = t.test(Total)$conf.int[1],
    CI_upper = t.test(Total)$conf.int[2],
    shapiro_w = shapiro.test(Total)$statistic,
    shapiro_p = shapiro.test(Total)$p.value
  )


# ALL  Total Total Year 1

AllMSA_Pop_Baseline_671 %>% inner_join(Total_VOL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(Total),
    sd=sd(Total),
    median=median(Total),
    Q1 = quantile(Total, 0.25),
    Q3 = quantile(Total, 0.75),
    min=min(Total),
    max=max(Total),
    n=n(),
    skewness = skewness(Total, na.rm = TRUE),
    kurtosis = kurtosis(Total, na.rm = TRUE),
    CI_lower = t.test(Total)$conf.int[1],
    CI_upper = t.test(Total)$conf.int[2],
    shapiro_w = shapiro.test(Total)$statistic,
    shapiro_p = shapiro.test(Total)$p.value
  )



# Elapsed Time Year 1


AllMSA_Pop_Baseline_671 %>% inner_join(Total_VOL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total, TIME_STUDY) %>%
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


AllMSA_Pop_Baseline_671 %>% inner_join(Total_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total) %>%
  rename("Baseline"="Total") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Total_VOL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total)
  ) %>%
  mutate(Diff=(Total-Baseline))  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
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

AllMSA_Pop_Baseline_671 %>% inner_join(Total_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total) %>%
  rename("Baseline"="Total") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Total_VOL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total, TIME_STUDY)
  ) %>%
  mutate(Diff=(Total-Baseline)/TIME_STUDY)  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )













# ALL  Total Total Year 2

AllMSA_Pop_Baseline_671 %>% inner_join(Total_VOL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(Total),
    sd=sd(Total),
    median=median(Total),
    Q1 = quantile(Total, 0.25),
    Q3 = quantile(Total, 0.75),
    min=min(Total),
    max=max(Total),
    n=n(),
    skewness = skewness(Total, na.rm = TRUE),
    kurtosis = kurtosis(Total, na.rm = TRUE),
    CI_lower = t.test(Total)$conf.int[1],
    CI_upper = t.test(Total)$conf.int[2],
    shapiro_w = shapiro.test(Total)$statistic,
    shapiro_p = shapiro.test(Total)$p.value
  )



# Elapsed Time Year 2


AllMSA_Pop_Baseline_671 %>% inner_join(Total_VOL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total, TIME_STUDY) %>%
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


AllMSA_Pop_Baseline_671 %>% inner_join(Total_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total) %>%
  rename("Baseline"="Total") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Total_VOL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total)
  ) %>%
  mutate(Diff=(Total-Baseline))  %>%
  summarise(
    mean=mean(Diff),
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

AllMSA_Pop_Baseline_671 %>% inner_join(Total_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total) %>%
  rename("Baseline"="Total") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Total_VOL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total, TIME_STUDY)
  ) %>%
  mutate(Diff=(Total-Baseline)/TIME_STUDY)  %>%
  summarise(
    mean=mean(Diff),
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






# ALL  Total Total Year 3

AllMSA_Pop_Baseline_671 %>% inner_join(Total_VOL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(Total),
    sd=sd(Total),
    median=median(Total),
    Q1 = quantile(Total, 0.25),
    Q3 = quantile(Total, 0.75),
    min=min(Total),
    max=max(Total),
    n=n(),
    skewness = skewness(Total, na.rm = TRUE),
    kurtosis = kurtosis(Total, na.rm = TRUE),
    CI_lower = t.test(Total)$conf.int[1],
    CI_upper = t.test(Total)$conf.int[2],
    shapiro_w = shapiro.test(Total)$statistic,
    shapiro_p = shapiro.test(Total)$p.value
  )



# Elapsed Time Year 3


AllMSA_Pop_Baseline_671 %>% inner_join(Total_VOL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total, TIME_STUDY) %>%
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


AllMSA_Pop_Baseline_671 %>% inner_join(Total_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total) %>%
  rename("Baseline"="Total") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Total_VOL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total)
  ) %>%
  mutate(Diff=(Total-Baseline))  %>%
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

AllMSA_Pop_Baseline_671 %>% inner_join(Total_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total) %>%
  rename("Baseline"="Total") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Total_VOL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total, TIME_STUDY)
  ) %>%
  mutate(Diff=(Total-Baseline)/TIME_STUDY)  %>%
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



# -----------
# Overall MSA Entire  MSA P vs MSA C  -----------------------

# ALL  Total Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(Total_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Total),
    sd=sd(Total),
    median=median(Total),
    Q1 = quantile(Total, 0.25),
    Q3 = quantile(Total, 0.75),
    min=min(Total),
    max=max(Total),
    n=n(),
    skewness = skewness(Total, na.rm = TRUE),
    kurtosis = kurtosis(Total, na.rm = TRUE),
    CI_lower = t.test(Total)$conf.int[1],
    CI_upper = t.test(Total)$conf.int[2],
    shapiro_w = shapiro.test(Total)$statistic,
    shapiro_p = shapiro.test(Total)$p.value
  )


# ALL  Total Total Year 1

AllMSA_Pop_Baseline_671 %>% inner_join(Total_VOL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Total),
    sd=sd(Total),
    median=median(Total),
    Q1 = quantile(Total, 0.25),
    Q3 = quantile(Total, 0.75),
    min=min(Total),
    max=max(Total),
    n=n(),
    skewness = skewness(Total, na.rm = TRUE),
    kurtosis = kurtosis(Total, na.rm = TRUE),
    CI_lower = t.test(Total)$conf.int[1],
    CI_upper = t.test(Total)$conf.int[2],
    shapiro_w = shapiro.test(Total)$statistic,
    shapiro_p = shapiro.test(Total)$p.value
  )



# Elapsed Time Year 1


AllMSA_Pop_Baseline_671 %>% inner_join(Total_VOL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total, DIAG, TIME_STUDY) %>%
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


AllMSA_Pop_Baseline_671 %>% inner_join(Total_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total) %>%
  rename("Baseline"="Total") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Total_VOL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total, DIAG)
  ) %>%
  mutate(Diff=(Total-Baseline))  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
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

AllMSA_Pop_Baseline_671 %>% inner_join(Total_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total) %>%
  rename("Baseline"="Total") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Total_VOL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total, DIAG, TIME_STUDY)
  ) %>%
  mutate(Diff=(Total-Baseline)/TIME_STUDY)  %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )













# ALL  Total Total Year 2

AllMSA_Pop_Baseline_671 %>% inner_join(Total_VOL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Total),
    sd=sd(Total),
    median=median(Total),
    Q1 = quantile(Total, 0.25),
    Q3 = quantile(Total, 0.75),
    min=min(Total),
    max=max(Total),
    n=n(),
    skewness = skewness(Total, na.rm = TRUE),
    kurtosis = kurtosis(Total, na.rm = TRUE),
    CI_lower = t.test(Total)$conf.int[1],
    CI_upper = t.test(Total)$conf.int[2],
    shapiro_w = shapiro.test(Total)$statistic,
    shapiro_p = shapiro.test(Total)$p.value
  )



# Elapsed Time Year 2


AllMSA_Pop_Baseline_671 %>% inner_join(Total_VOL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total, DIAG, TIME_STUDY) %>%
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


AllMSA_Pop_Baseline_671 %>% inner_join(Total_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total) %>%
  rename("Baseline"="Total") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Total_VOL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total, DIAG)
  ) %>%
  mutate(Diff=(Total-Baseline))  %>% group_by(DIAG) %>%
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

AllMSA_Pop_Baseline_671 %>% inner_join(Total_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total) %>%
  rename("Baseline"="Total") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Total_VOL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total, DIAG, TIME_STUDY)
  ) %>%
  mutate(Diff=(Total-Baseline)/TIME_STUDY)  %>% filter(DIAG=="PD") %>%
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






# ALL  Total Total Year 3

AllMSA_Pop_Baseline_671 %>% inner_join(Total_VOL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Total),
    sd=sd(Total),
    median=median(Total),
    Q1 = quantile(Total, 0.25),
    Q3 = quantile(Total, 0.75),
    min=min(Total),
    max=max(Total),
    n=n(),
    skewness = skewness(Total, na.rm = TRUE),
    kurtosis = kurtosis(Total, na.rm = TRUE),
    CI_lower = t.test(Total)$conf.int[1],
    CI_upper = t.test(Total)$conf.int[2],
    shapiro_w = shapiro.test(Total)$statistic,
    shapiro_p = shapiro.test(Total)$p.value
  )



# Elapsed Time Year 3


AllMSA_Pop_Baseline_671 %>% inner_join(Total_VOL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total, DIAG, TIME_STUDY) %>%
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


AllMSA_Pop_Baseline_671 %>% inner_join(Total_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total) %>%
  rename("Baseline"="Total") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Total_VOL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total, DIAG)
  ) %>%
  mutate(Diff=(Total-Baseline))  %>% filter(DIAG=="PD") %>%
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

AllMSA_Pop_Baseline_671 %>% inner_join(Total_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total) %>%
  rename("Baseline"="Total") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Total_VOL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total, DIAG, TIME_STUDY)
  ) %>%
  mutate(Diff=(Total-Baseline)/TIME_STUDY)  %>% filter(DIAG=="PD") %>%
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



# -----------
# Overall MSA Entire  Probable vs Possible  -----------------------

# ALL  Total Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(Total_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Total),
    sd=sd(Total),
    median=median(Total),
    Q1 = quantile(Total, 0.25),
    Q3 = quantile(Total, 0.75),
    min=min(Total),
    max=max(Total),
    n=n(),
    skewness = skewness(Total, na.rm = TRUE),
    kurtosis = kurtosis(Total, na.rm = TRUE),
    CI_lower = t.test(Total)$conf.int[1],
    CI_upper = t.test(Total)$conf.int[2],
    shapiro_w = shapiro.test(Total)$statistic,
    shapiro_p = shapiro.test(Total)$p.value
  )


# ALL  Total Total Year 1

AllMSA_Pop_Baseline_671 %>% inner_join(Total_VOL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Total),
    sd=sd(Total),
    median=median(Total),
    Q1 = quantile(Total, 0.25),
    Q3 = quantile(Total, 0.75),
    min=min(Total),
    max=max(Total),
    n=n(),
    skewness = skewness(Total, na.rm = TRUE),
    kurtosis = kurtosis(Total, na.rm = TRUE),
    CI_lower = t.test(Total)$conf.int[1],
    CI_upper = t.test(Total)$conf.int[2],
    shapiro_w = shapiro.test(Total)$statistic,
    shapiro_p = shapiro.test(Total)$p.value
  )



# Elapsed Time Year 1


AllMSA_Pop_Baseline_671 %>% inner_join(Total_VOL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total, DIAGNIV, TIME_STUDY) %>%
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


AllMSA_Pop_Baseline_671 %>% inner_join(Total_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total) %>%
  rename("Baseline"="Total") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Total_VOL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total, DIAGNIV)
  ) %>%
  mutate(Diff=(Total-Baseline))  %>% filter(DIAGNIV=="POS") %>%
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

AllMSA_Pop_Baseline_671 %>% inner_join(Total_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total) %>%
  rename("Baseline"="Total") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Total_VOL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total, DIAGNIV, TIME_STUDY)
  ) %>%
  mutate(Diff=(Total-Baseline)/TIME_STUDY)  %>% filter(DIAGNIV=="POS") %>%
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













# ALL  Total Total Year 2

AllMSA_Pop_Baseline_671 %>% inner_join(Total_VOL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Total),
    sd=sd(Total),
    median=median(Total),
    Q1 = quantile(Total, 0.25),
    Q3 = quantile(Total, 0.75),
    min=min(Total),
    max=max(Total),
    n=n(),
    skewness = skewness(Total, na.rm = TRUE),
    kurtosis = kurtosis(Total, na.rm = TRUE),
    CI_lower = t.test(Total)$conf.int[1],
    CI_upper = t.test(Total)$conf.int[2],
    shapiro_w = shapiro.test(Total)$statistic,
    shapiro_p = shapiro.test(Total)$p.value
  )



# Elapsed Time Year 2


AllMSA_Pop_Baseline_671 %>% inner_join(Total_VOL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total, DIAGNIV, TIME_STUDY) %>%
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


AllMSA_Pop_Baseline_671 %>% inner_join(Total_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total) %>%
  rename("Baseline"="Total") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Total_VOL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total, DIAGNIV)
  ) %>%
  mutate(Diff=(Total-Baseline))  %>% filter(DIAGNIV=="PROB") %>%
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

AllMSA_Pop_Baseline_671 %>% inner_join(Total_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total) %>%
  rename("Baseline"="Total") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Total_VOL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total, DIAGNIV, TIME_STUDY)
  ) %>%
  mutate(Diff=(Total-Baseline)/TIME_STUDY)  %>% filter(DIAGNIV=="PROB") %>%
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






# ALL  Total Total Year 3

AllMSA_Pop_Baseline_671 %>% inner_join(Total_VOL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Total),
    sd=sd(Total),
    median=median(Total),
    Q1 = quantile(Total, 0.25),
    Q3 = quantile(Total, 0.75),
    min=min(Total),
    max=max(Total),
    n=n(),
    skewness = skewness(Total, na.rm = TRUE),
    kurtosis = kurtosis(Total, na.rm = TRUE),
    CI_lower = t.test(Total)$conf.int[1],
    CI_upper = t.test(Total)$conf.int[2],
    shapiro_w = shapiro.test(Total)$statistic,
    shapiro_p = shapiro.test(Total)$p.value
  )



# Elapsed Time Year 3


AllMSA_Pop_Baseline_671 %>% inner_join(Total_VOL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total, DIAGNIV, TIME_STUDY) %>%
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


AllMSA_Pop_Baseline_671 %>% inner_join(Total_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total) %>%
  rename("Baseline"="Total") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Total_VOL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total, DIAGNIV)
  ) %>%
  mutate(Diff=(Total-Baseline))  %>% filter(DIAGNIV=="PROB") %>%
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

AllMSA_Pop_Baseline_671 %>% inner_join(Total_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total) %>%
  rename("Baseline"="Total") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Total_VOL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total, DIAGNIV, TIME_STUDY)
  ) %>%
  mutate(Diff=(Total-Baseline)/TIME_STUDY)  %>% filter(DIAGNIV=="PROB") %>%
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

# ---------------


# Target MSA Entire  -----------------------

# ALL  Total Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(Total_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(Total),
    sd=sd(Total),
    median=median(Total),
    Q1 = quantile(Total, 0.25),
    Q3 = quantile(Total, 0.75),
    min=min(Total),
    max=max(Total),
    n=n(),
    skewness = skewness(Total, na.rm = TRUE),
    kurtosis = kurtosis(Total, na.rm = TRUE),
    CI_lower = t.test(Total)$conf.int[1],
    CI_upper = t.test(Total)$conf.int[2],
    shapiro_w = shapiro.test(Total)$statistic,
    shapiro_p = shapiro.test(Total)$p.value
  )


# ALL  Total Total Year 1

EarlyCT_Pop_Baseline_319 %>% inner_join(Total_VOL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(Total),
    sd=sd(Total),
    median=median(Total),
    Q1 = quantile(Total, 0.25),
    Q3 = quantile(Total, 0.75),
    min=min(Total),
    max=max(Total),
    n=n(),
    skewness = skewness(Total, na.rm = TRUE),
    kurtosis = kurtosis(Total, na.rm = TRUE),
    CI_lower = t.test(Total)$conf.int[1],
    CI_upper = t.test(Total)$conf.int[2],
    shapiro_w = shapiro.test(Total)$statistic,
    shapiro_p = shapiro.test(Total)$p.value
  )



# Elapsed Time Year 1


EarlyCT_Pop_Baseline_319 %>% inner_join(Total_VOL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total, TIME_STUDY) %>%
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


EarlyCT_Pop_Baseline_319 %>% inner_join(Total_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total) %>%
  rename("Baseline"="Total") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Total_VOL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total)
  ) %>%
  mutate(Diff=(Total-Baseline))  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
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

EarlyCT_Pop_Baseline_319 %>% inner_join(Total_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total) %>%
  rename("Baseline"="Total") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Total_VOL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total, TIME_STUDY)
  ) %>%
  mutate(Diff=(Total-Baseline)/TIME_STUDY)  %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )













# ALL  Total Total Year 2

EarlyCT_Pop_Baseline_319 %>% inner_join(Total_VOL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(Total),
    sd=sd(Total),
    median=median(Total),
    Q1 = quantile(Total, 0.25),
    Q3 = quantile(Total, 0.75),
    min=min(Total),
    max=max(Total),
    n=n(),
    skewness = skewness(Total, na.rm = TRUE),
    kurtosis = kurtosis(Total, na.rm = TRUE),
    CI_lower = t.test(Total)$conf.int[1],
    CI_upper = t.test(Total)$conf.int[2],
    shapiro_w = shapiro.test(Total)$statistic,
    shapiro_p = shapiro.test(Total)$p.value
  )



# Elapsed Time Year 2


EarlyCT_Pop_Baseline_319 %>% inner_join(Total_VOL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total, TIME_STUDY) %>%
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


EarlyCT_Pop_Baseline_319 %>% inner_join(Total_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total) %>%
  rename("Baseline"="Total") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Total_VOL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total)
  ) %>%
  mutate(Diff=(Total-Baseline))  %>%
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

EarlyCT_Pop_Baseline_319 %>% inner_join(Total_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total) %>%
  rename("Baseline"="Total") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Total_VOL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total, TIME_STUDY)
  ) %>%
  mutate(Diff=(Total-Baseline)/TIME_STUDY)  %>%
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






# ALL  Total Total Year 3

EarlyCT_Pop_Baseline_319 %>% inner_join(Total_VOL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(Total),
    sd=sd(Total),
    median=median(Total),
    Q1 = quantile(Total, 0.25),
    Q3 = quantile(Total, 0.75),
    min=min(Total),
    max=max(Total),
    n=n(),
    skewness = skewness(Total, na.rm = TRUE),
    kurtosis = kurtosis(Total, na.rm = TRUE),
    CI_lower = t.test(Total)$conf.int[1],
    CI_upper = t.test(Total)$conf.int[2],
    shapiro_w = shapiro.test(Total)$statistic,
    shapiro_p = shapiro.test(Total)$p.value
  )



# Elapsed Time Year 3


EarlyCT_Pop_Baseline_319 %>% inner_join(Total_VOL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total, TIME_STUDY) %>%
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


EarlyCT_Pop_Baseline_319 %>% inner_join(Total_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total) %>%
  rename("Baseline"="Total") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Total_VOL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total)
  ) %>%
  mutate(Diff=(Total-Baseline))  %>%
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

EarlyCT_Pop_Baseline_319 %>% inner_join(Total_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total) %>%
  rename("Baseline"="Total") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Total_VOL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total, TIME_STUDY)
  ) %>%
  mutate(Diff=(Total-Baseline)/TIME_STUDY)  %>%
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



# -----------
# Target MSA Entire  MSA P vs MSA C  -----------------------

# ALL  Total Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(Total_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Total),
    sd=sd(Total),
    median=median(Total),
    Q1 = quantile(Total, 0.25),
    Q3 = quantile(Total, 0.75),
    min=min(Total),
    max=max(Total),
    n=n(),
    skewness = skewness(Total, na.rm = TRUE),
    kurtosis = kurtosis(Total, na.rm = TRUE),
    CI_lower = t.test(Total)$conf.int[1],
    CI_upper = t.test(Total)$conf.int[2],
    shapiro_w = shapiro.test(Total)$statistic,
    shapiro_p = shapiro.test(Total)$p.value
  )


# ALL  Total Total Year 1

EarlyCT_Pop_Baseline_319 %>% inner_join(Total_VOL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Total),
    sd=sd(Total),
    median=median(Total),
    Q1 = quantile(Total, 0.25),
    Q3 = quantile(Total, 0.75),
    min=min(Total),
    max=max(Total),
    n=n(),
    skewness = skewness(Total, na.rm = TRUE),
    kurtosis = kurtosis(Total, na.rm = TRUE),
    CI_lower = t.test(Total)$conf.int[1],
    CI_upper = t.test(Total)$conf.int[2],
    shapiro_w = shapiro.test(Total)$statistic,
    shapiro_p = shapiro.test(Total)$p.value
  )



# Elapsed Time Year 1


EarlyCT_Pop_Baseline_319 %>% inner_join(Total_VOL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total, DIAG, TIME_STUDY) %>%
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


EarlyCT_Pop_Baseline_319 %>% inner_join(Total_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total) %>%
  rename("Baseline"="Total") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Total_VOL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total, DIAG)
  ) %>%
  mutate(Diff=(Total-Baseline))  %>% filter(DIAG=="CB") %>%
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

EarlyCT_Pop_Baseline_319 %>% inner_join(Total_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total) %>%
  rename("Baseline"="Total") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Total_VOL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total, DIAG, TIME_STUDY)
  ) %>%
  mutate(Diff=(Total-Baseline)/TIME_STUDY)  %>% filter(DIAG=="CB") %>%
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













# ALL  Total Total Year 2

EarlyCT_Pop_Baseline_319 %>% inner_join(Total_VOL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Total),
    sd=sd(Total),
    median=median(Total),
    Q1 = quantile(Total, 0.25),
    Q3 = quantile(Total, 0.75),
    min=min(Total),
    max=max(Total),
    n=n(),
    skewness = skewness(Total, na.rm = TRUE),
    kurtosis = kurtosis(Total, na.rm = TRUE),
    CI_lower = t.test(Total)$conf.int[1],
    CI_upper = t.test(Total)$conf.int[2],
    shapiro_w = shapiro.test(Total)$statistic,
    shapiro_p = shapiro.test(Total)$p.value
  )



# Elapsed Time Year 2


EarlyCT_Pop_Baseline_319 %>% inner_join(Total_VOL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total, DIAG, TIME_STUDY) %>%
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


EarlyCT_Pop_Baseline_319 %>% inner_join(Total_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total) %>%
  rename("Baseline"="Total") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Total_VOL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total, DIAG)
  ) %>%
  mutate(Diff=(Total-Baseline))  %>% filter(DIAG=="PD") %>%
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

EarlyCT_Pop_Baseline_319 %>% inner_join(Total_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total) %>%
  rename("Baseline"="Total") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Total_VOL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total, DIAG, TIME_STUDY)
  ) %>%
  mutate(Diff=(Total-Baseline)/TIME_STUDY)  %>% filter(DIAG=="PD") %>%
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






# ALL  Total Total Year 3

EarlyCT_Pop_Baseline_319 %>% inner_join(Total_VOL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(Total),
    sd=sd(Total),
    median=median(Total),
    Q1 = quantile(Total, 0.25),
    Q3 = quantile(Total, 0.75),
    min=min(Total),
    max=max(Total),
    n=n(),
    skewness = skewness(Total, na.rm = TRUE),
    kurtosis = kurtosis(Total, na.rm = TRUE),
    CI_lower = t.test(Total)$conf.int[1],
    CI_upper = t.test(Total)$conf.int[2],
    shapiro_w = shapiro.test(Total)$statistic,
    shapiro_p = shapiro.test(Total)$p.value
  )



# Elapsed Time Year 3


EarlyCT_Pop_Baseline_319 %>% inner_join(Total_VOL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total, DIAG, TIME_STUDY) %>%
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


EarlyCT_Pop_Baseline_319 %>% inner_join(Total_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total) %>%
  rename("Baseline"="Total") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Total_VOL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total, DIAG)
  ) %>%
  mutate(Diff=(Total-Baseline))  %>% filter(DIAG=="PD") %>%
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

EarlyCT_Pop_Baseline_319 %>% inner_join(Total_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total) %>%
  rename("Baseline"="Total") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Total_VOL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total, DIAG, TIME_STUDY)
  ) %>%
  mutate(Diff=(Total-Baseline)/TIME_STUDY)  %>% filter(DIAG=="PD") %>%
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



# -----------
# Target MSA Entire  Probable vs Possible  -----------------------

# ALL  Total Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(Total_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Total),
    sd=sd(Total),
    median=median(Total),
    Q1 = quantile(Total, 0.25),
    Q3 = quantile(Total, 0.75),
    min=min(Total),
    max=max(Total),
    n=n(),
    skewness = skewness(Total, na.rm = TRUE),
    kurtosis = kurtosis(Total, na.rm = TRUE),
    CI_lower = t.test(Total)$conf.int[1],
    CI_upper = t.test(Total)$conf.int[2],
    shapiro_w = shapiro.test(Total)$statistic,
    shapiro_p = shapiro.test(Total)$p.value
  )


# ALL  Total Total Year 1

EarlyCT_Pop_Baseline_319 %>% inner_join(Total_VOL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Total),
    sd=sd(Total),
    median=median(Total),
    Q1 = quantile(Total, 0.25),
    Q3 = quantile(Total, 0.75),
    min=min(Total),
    max=max(Total),
    n=n(),
    skewness = skewness(Total, na.rm = TRUE),
    kurtosis = kurtosis(Total, na.rm = TRUE),
    CI_lower = t.test(Total)$conf.int[1],
    CI_upper = t.test(Total)$conf.int[2],
    shapiro_w = shapiro.test(Total)$statistic,
    shapiro_p = shapiro.test(Total)$p.value
  )



# Elapsed Time Year 1


EarlyCT_Pop_Baseline_319 %>% inner_join(Total_VOL) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total, DIAGNIV, TIME_STUDY) %>%
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


EarlyCT_Pop_Baseline_319 %>% inner_join(Total_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total) %>%
  rename("Baseline"="Total") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Total_VOL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total, DIAGNIV)
  ) %>%
  mutate(Diff=(Total-Baseline))  %>% filter(DIAGNIV=="PROB") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
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

EarlyCT_Pop_Baseline_319 %>% inner_join(Total_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total) %>%
  rename("Baseline"="Total") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Total_VOL) %>% filter(Year==1) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total, DIAGNIV, TIME_STUDY)
  ) %>%
  mutate(Diff=(Total-Baseline)/TIME_STUDY)  %>% filter(DIAGNIV=="PROB") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n(),
    skewness = skewness(Diff, na.rm = TRUE),
    kurtosis = kurtosis(Diff, na.rm = TRUE),
    CI_lower = t.test(Diff)$conf.int[1],
    CI_upper = t.test(Diff)$conf.int[2],
    shapiro_w = shapiro.test(Diff)$statistic,
    shapiro_p = shapiro.test(Diff)$p.value
  )













# ALL  Total Total Year 2

EarlyCT_Pop_Baseline_319 %>% inner_join(Total_VOL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% filter(DIAGNIV=="PROB") %>%
  summarise(
    mean=mean(Total),
    sd=sd(Total),
    median=median(Total),
    Q1 = quantile(Total, 0.25),
    Q3 = quantile(Total, 0.75),
    min=min(Total),
    max=max(Total),
    n=n(),
    skewness = skewness(Total, na.rm = TRUE),
    kurtosis = kurtosis(Total, na.rm = TRUE),
    CI_lower = t.test(Total)$conf.int[1],
    CI_upper = t.test(Total)$conf.int[2],
    shapiro_w = shapiro.test(Total)$statistic,
    shapiro_p = shapiro.test(Total)$p.value
  )



# Elapsed Time Year 2


EarlyCT_Pop_Baseline_319 %>% inner_join(Total_VOL) %>% filter(Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total, DIAGNIV, TIME_STUDY) %>%
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


EarlyCT_Pop_Baseline_319 %>% inner_join(Total_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total) %>%
  rename("Baseline"="Total") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Total_VOL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total, DIAGNIV)
  ) %>%
  mutate(Diff=(Total-Baseline))  %>% filter(DIAGNIV=="PROB") %>%
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

EarlyCT_Pop_Baseline_319 %>% inner_join(Total_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total) %>%
  rename("Baseline"="Total") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Total_VOL) %>% filter(Year==2) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total, DIAGNIV, TIME_STUDY)
  ) %>%
  mutate(Diff=(Total-Baseline)/TIME_STUDY)  %>% filter(DIAGNIV=="PROB") %>%
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






# ALL  Total Total Year 3

EarlyCT_Pop_Baseline_319 %>% inner_join(Total_VOL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% filter(DIAGNIV=="POS") %>%
  summarise(
    mean=mean(Total),
    sd=sd(Total),
    median=median(Total),
    Q1 = quantile(Total, 0.25),
    Q3 = quantile(Total, 0.75),
    min=min(Total),
    max=max(Total),
    n=n(),
    #skewness = skewness(Total, na.rm = TRUE),
    #kurtosis = kurtosis(Total, na.rm = TRUE),
    #CI_lower = t.test(Total)$conf.int[1],
    #CI_upper = t.test(Total)$conf.int[2],
    #shapiro_w = shapiro.test(Total)$statistic,
    #shapiro_p = shapiro.test(Total)$p.value
  )



# Elapsed Time Year 3


EarlyCT_Pop_Baseline_319 %>% inner_join(Total_VOL) %>% filter(Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total, DIAGNIV, TIME_STUDY) %>%
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


# Change from baseline Year 3


EarlyCT_Pop_Baseline_319 %>% inner_join(Total_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total) %>%
  rename("Baseline"="Total") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Total_VOL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total, DIAGNIV)
  ) %>%
  mutate(Diff=(Total-Baseline))  %>% filter(DIAGNIV=="PROB") %>%
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

EarlyCT_Pop_Baseline_319 %>% inner_join(Total_VOL) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% 
  filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total) %>%
  rename("Baseline"="Total") %>%
  inner_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Total_VOL) %>% filter(Year==3) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% 
      filter(Elapsed==min(Elapsed)) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total, DIAGNIV, TIME_STUDY)
  ) %>%
  mutate(Diff=(Total-Baseline)/TIME_STUDY)  %>% filter(DIAGNIV=="PROB") %>%
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

# ---------------

