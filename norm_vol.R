
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
Left_Putamen_VOL <- irmForPaulo %>% select(NUM, DateIRM, Left_Putamen, EstimatedTotalIntraCranialVol) %>%
  mutate_(Left_Putamen=Left_Putamen/EstimatedTotalIntraCranialVol) %>% select(-EstimatedTotalIntraCranialVol)

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

Right_Putamen_VOL <- irmForPaulo %>% select(NUM, DateIRM, Right_Putamen, EstimatedTotalIntraCranialVol) %>%
  mutate(Right_Putamen=Right_Putamen/EstimatedTotalIntraCranialVol) %>% select(-EstimatedTotalIntraCranialVol)

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

Cerebellum_VOL <- irmForPaulo %>% select(NUM, DateIRM, Left_Cerebellum_White_Matter,Right_Cerebellum_White_Matter, EstimatedTotalIntraCranialVol) %>%
   mutate(Cerebellum=Left_Cerebellum_White_Matter+Right_Cerebellum_White_Matter) %>%
  mutate(Cerebellum=Cerebellum/EstimatedTotalIntraCranialVol) %>% select(-EstimatedTotalIntraCranialVol)


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

Midbrain_VOL <- irmForPaulo %>% select(NUM, DateIRM, Midbrain, EstimatedTotalIntraCranialVol) %>%
  mutate(Midbrain=Midbrain/EstimatedTotalIntraCranialVol) %>% select(-EstimatedTotalIntraCranialVol)


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


Pons_VOL <- irmForPaulo %>% select(NUM, DateIRM, Pons, EstimatedTotalIntraCranialVol) %>%
  mutate(Pons=Pons/EstimatedTotalIntraCranialVol) %>% select(-EstimatedTotalIntraCranialVol)


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


Medulla_VOL <- irmForPaulo %>% select(NUM, DateIRM, Medulla, EstimatedTotalIntraCranialVol) %>%
  mutate(Medulla=Medulla/EstimatedTotalIntraCranialVol) %>% select(-EstimatedTotalIntraCranialVol)


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
Total_VOL <- irmForPaulo %>% select(NUM, DateIRM, CerebralWhiteMatterVol, TotalGrayVol, EstimatedTotalIntraCranialVol)

Total_VOL <- Total_VOL %>% mutate(Total=as.numeric(CerebralWhiteMatterVol)+as.numeric(TotalGrayVol) )

Total_VOL <- Total_VOL %>% mutate(Total=Total/EstimatedTotalIntraCranialVol ) %>% select(-EstimatedTotalIntraCranialVol)

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

Left_Putamen_VOL <- irmForPaulo %>% select(NUM, DateIRM, Left_Putamen, EstimatedTotalIntraCranialVol)

Left_Putamen_VOL <- Left_Putamen_VOL %>% mutate(Left_Putamen=Left_Putamen/EstimatedTotalIntraCranialVol ) %>% select(-EstimatedTotalIntraCranialVol)


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
  ylab("Normalized Left Putamen Volume \n At each MRI \n") +
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
Right_Putamen_VOL <- irmForPaulo %>% select(NUM, DateIRM, Right_Putamen, EstimatedTotalIntraCranialVol)


Right_Putamen_VOL <- Right_Putamen_VOL %>% mutate(Right_Putamen=Right_Putamen/EstimatedTotalIntraCranialVol ) %>% select(-EstimatedTotalIntraCranialVol)

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
  ylab("Normalized Right Putamen Volume \n At each MRI \n") +
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
Cerebellum_VOL <- irmForPaulo %>% select(NUM, DateIRM, Left_Cerebellum_White_Matter,Right_Cerebellum_White_Matter, EstimatedTotalIntraCranialVol)

Cerebellum_VOL <- Cerebellum_VOL %>% mutate(Cerebellum=Left_Cerebellum_White_Matter+Right_Cerebellum_White_Matter)

Cerebellum_VOL <- Cerebellum_VOL %>% mutate(Cerebellum=Cerebellum/EstimatedTotalIntraCranialVol)

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
  ylab("Normalized Cerebellum White Volume \n At each MRI \n") +
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
Midbrain_VOL <- irmForPaulo %>% select(NUM, DateIRM, Midbrain, EstimatedTotalIntraCranialVol) %>%
  mutate(Midbrain=as.numeric(Midbrain)/EstimatedTotalIntraCranialVol)
 
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
  ylab("Normalized Midbrain Volume \n At each MRI \n") +
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
Pons_VOL <- irmForPaulo %>% select(NUM, DateIRM, Pons, EstimatedTotalIntraCranialVol) %>%
  mutate(Pons=Pons/EstimatedTotalIntraCranialVol)

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
  ylab("Normalized Pons Volume \n At each MRI \n") +
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
Medulla_VOL <- irmForPaulo %>% select(NUM, DateIRM, Medulla, EstimatedTotalIntraCranialVol) %>%
  mutate(Medulla=as.numeric(Medulla)/EstimatedTotalIntraCranialVol)
 
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
  ylab("Normalized Medulla Volume \n At each MRI \n") +
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
Total_VOL <- irmForPaulo %>% select(NUM, DateIRM, CerebralWhiteMatterVol, TotalGrayVol, EstimatedTotalIntraCranialVol)

Total_VOL <- Total_VOL %>% mutate(Total=as.numeric(CerebralWhiteMatterVol)+as.numeric(TotalGrayVol) )

Total_VOL <- Total_VOL %>% mutate(Total=Total/EstimatedTotalIntraCranialVol)

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
  ylab("Normalized Whole Brain Volume \n At each MRI \n") +
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

Left_Putamen_VOL <- irmForPaulo %>% select(NUM, DateIRM, Left_Putamen, EstimatedTotalIntraCranialVol) %>%
  mutate(Left_Putamen=Left_Putamen/EstimatedTotalIntraCranialVol) %>% select(-EstimatedTotalIntraCranialVol)

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
    # shapiro_w = shapiro.test(Diff)$statistic,
   # shapiro_p = shapiro.test(Diff)$p.value
  )













# ALL  Left_Putamen Total Year 2

EarlyCT_Pop_Baseline_319 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==2) %>%
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

Right_Putamen_VOL <- irmForPaulo %>% select(NUM, DateIRM, Right_Putamen, EstimatedTotalIntraCranialVol) %>%
  mutate(Right_Putamen=Right_Putamen/EstimatedTotalIntraCranialVol)
 
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
  mutate(Diff=(Right_Putamen-Baseline))  %>% filter(DIAG=="CB") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
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
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen, DIAG, TIME_STUDY)
  ) %>%
  mutate(Diff=(Right_Putamen-Baseline)/TIME_STUDY)  %>% filter(DIAG=="CB") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
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

EarlyCT_Pop_Baseline_319 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==2) %>%
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

Cerebellum_VOL <- irmForPaulo %>% select(NUM, DateIRM, Left_Cerebellum_White_Matter,Right_Cerebellum_White_Matter,EstimatedTotalIntraCranialVol )

Cerebellum_VOL <- Cerebellum_VOL %>% mutate(Cerebellum=Left_Cerebellum_White_Matter+Right_Cerebellum_White_Matter)
Cerebellum_VOL <- Cerebellum_VOL %>% mutate(Cerebellum=Cerebellum/EstimatedTotalIntraCranialVol)

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

EarlyCT_Pop_Baseline_319 %>% inner_join(Cerebellum_VOL) %>% filter(Year==2) %>%
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

Midbrain_VOL <- irmForPaulo %>% select(NUM, DateIRM, Midbrain, EstimatedTotalIntraCranialVol)

Midbrain_VOL$Midbrain <- as.numeric(Midbrain_VOL$Midbrain)

Midbrain_VOL <- Midbrain_VOL %>% mutate(Midbrain=Midbrain/EstimatedTotalIntraCranialVol)

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
  mutate(Diff=(Midbrain-Baseline))  %>% filter(DIAG=="CB") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
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
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain, DIAG, TIME_STUDY)
  ) %>%
  mutate(Diff=(Midbrain-Baseline)/TIME_STUDY)  %>% filter(DIAG=="CB") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
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

EarlyCT_Pop_Baseline_319 %>% inner_join(Midbrain_VOL) %>% filter(Year==2) %>%
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

Pons_VOL <- irmForPaulo %>% select(NUM, DateIRM, Pons, EstimatedTotalIntraCranialVol)

Pons_VOL$Pons <- as.numeric(Pons_VOL$Pons)

Pons_VOL <- Pons_VOL %>% mutate(Pons=Pons/EstimatedTotalIntraCranialVol)

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
  mutate(Elapsed=abs
         (TIME_STUDY-Year)) %>% 
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

EarlyCT_Pop_Baseline_319 %>% inner_join(Pons_VOL) %>% filter(Year==2) %>%
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

Medulla_VOL <- irmForPaulo %>% select(NUM, DateIRM, Medulla, EstimatedTotalIntraCranialVol)

Medulla_VOL$Medulla <- as.numeric(Medulla_VOL$Medulla)

Medulla_VOL <- Medulla_VOL %>% mutate(Medulla=Medulla/EstimatedTotalIntraCranialVol)

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
  mutate(Diff=(Medulla-Baseline))  %>% filter(DIAG=="CB") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
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
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla, DIAG, TIME_STUDY)
  ) %>%
  mutate(Diff=(Medulla-Baseline)/TIME_STUDY)  %>% filter(DIAG=="CB") %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
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
Total_VOL <- irmForPaulo %>% select(NUM, DateIRM, CerebralWhiteMatterVol, TotalGrayVol, EstimatedTotalIntraCranialVol)

Total_VOL <- Total_VOL %>% mutate(Total=as.numeric(CerebralWhiteMatterVol)+as.numeric(TotalGrayVol) )
Total_VOL <- Total_VOL %>% mutate(Total=Total/EstimatedTotalIntraCranialVol )

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

EarlyCT_Pop_Baseline_319 %>% inner_join(Total_VOL) %>% filter(Year==2) %>%
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


# Student T-tests for deltas in Normalized Volumetric MRI Prob vs Poss, MSA-C vs MSA-P ---------------------------------------
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
# Left Putamen Volume -----------------------------

irmForPaulo <- readRDS("Source/irmForPaulo.rds")

Left_Putamen_VOL <- irmForPaulo %>% select(NUM, DateIRM, Left_Putamen, EstimatedTotalIntraCranialVol) %>% 
  mutate(Left_Putamen=Left_Putamen/EstimatedTotalIntraCranialVol) %>% select(-EstimatedTotalIntraCranialVol)

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


# -------
# Overall MSA -------------------------------

# Year 1
AllMSA_Pop_Baseline_671 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==0) %>% 
  filter(!is.na(Left_Putamen)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Left_Putamen) %>%
  rename("Baseline"="Left_Putamen") %>%
  left_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==1) %>%
      filter(!is.na(Left_Putamen)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Left_Putamen)
  ) %>% 
  mutate(Diff= (Left_Putamen-Baseline)/TIME_STUDY)  %>%
  t_test(Diff~DIAG, detailed = T,var.equal = TRUE) 
  #t_test(Diff~DIAGNIV, detailed = T, var.equal = TRUE) 


# Year 2
AllMSA_Pop_Baseline_671 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==0) %>% 
  filter(!is.na(Left_Putamen)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Left_Putamen) %>%
  rename("Baseline"="Left_Putamen") %>%
  left_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==2) %>%
      filter(!is.na(Left_Putamen)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Left_Putamen)
  ) %>% 
  mutate(Diff= (Left_Putamen-Baseline)/TIME_STUDY)  %>%
  #t_test(Diff~DIAG, detailed = T,var.equal = TRUE) 
  t_test(Diff~DIAGNIV,  detailed = T,var.equal = TRUE) 


# Year 3
AllMSA_Pop_Baseline_671 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==0) %>% 
  filter(!is.na(Left_Putamen)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Left_Putamen) %>%
  rename("Baseline"="Left_Putamen") %>%
  left_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==3) %>%
      filter(!is.na(Left_Putamen)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Left_Putamen)
  ) %>% 
  mutate(Diff= (Left_Putamen-Baseline)/TIME_STUDY)  %>%
  t_test(Diff~DIAG, detailed = T,var.equal = TRUE) 
#t_test(Diff~DIAGNIV, detailed = T, var.equal = TRUE) 

# --------------


# Early CT Takeda  -------------------------------

# Year 1
EarlyCT_Pop_Baseline_319 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==0) %>% 
  filter(!is.na(Left_Putamen)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Left_Putamen) %>%
  rename("Baseline"="Left_Putamen") %>%
  left_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==1) %>%
      filter(!is.na(Left_Putamen)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Left_Putamen)
  ) %>% 
  mutate(Diff= (Left_Putamen-Baseline)/TIME_STUDY)  %>%
  #t_test(Diff~DIAG, detailed = T,var.equal = TRUE) 
 t_test(Diff~DIAGNIV, detailed = T, var.equal = TRUE) 


# Year 2
EarlyCT_Pop_Baseline_319 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==0) %>% 
  filter(!is.na(Left_Putamen)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Left_Putamen) %>%
  rename("Baseline"="Left_Putamen") %>%
  left_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==2) %>%
      filter(!is.na(Left_Putamen)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Left_Putamen)
  ) %>% 
  mutate(Diff= (Left_Putamen-Baseline)/TIME_STUDY)  %>%
  t_test(Diff~DIAG, detailed = T,var.equal = TRUE) 
#t_test(Diff~DIAGNIV,  detailed = T,var.equal = TRUE) 


# Year 3
EarlyCT_Pop_Baseline_319 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==0) %>% 
  filter(!is.na(Left_Putamen)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Left_Putamen) %>%
  rename("Baseline"="Left_Putamen") %>%
  left_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==3) %>%
      filter(!is.na(Left_Putamen)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Left_Putamen)
  ) %>% 
  mutate(Diff= (Left_Putamen-Baseline)/TIME_STUDY)  %>%
  t_test(Diff~DIAG, detailed = T,var.equal = TRUE) 
#t_test(Diff~DIAGNIV, detailed = T, var.equal = TRUE) 

# --------------


# Right Putamen Volume -----------------------------

irmForPaulo <- readRDS("Source/irmForPaulo.rds")

Right_Putamen_VOL <- irmForPaulo %>% select(NUM, DateIRM, Right_Putamen, EstimatedTotalIntraCranialVol) %>%
  mutate(Right_Putamen=Right_Putamen/EstimatedTotalIntraCranialVol) %>% select(-EstimatedTotalIntraCranialVol)

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


# -------
# Overall MSA -------------------------------

# Year 1
AllMSA_Pop_Baseline_671 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==0) %>% 
  filter(!is.na(Right_Putamen)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Right_Putamen) %>%
  rename("Baseline"="Right_Putamen") %>%
  left_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==1) %>%
      filter(!is.na(Right_Putamen)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Right_Putamen)
  ) %>% 
  mutate(Diff= (Right_Putamen-Baseline)/TIME_STUDY)  %>%
  #t_test(Diff~DIAG, detailed = T,var.equal = TRUE) 
  t_test(Diff~DIAGNIV, detailed = T, var.equal = TRUE) 


# Year 2
AllMSA_Pop_Baseline_671 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==0) %>% 
  filter(!is.na(Right_Putamen)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Right_Putamen) %>%
  rename("Baseline"="Right_Putamen") %>%
  left_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==2) %>%
      filter(!is.na(Right_Putamen)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Right_Putamen)
  ) %>% 
  mutate(Diff= (Right_Putamen-Baseline)/TIME_STUDY)  %>%
  #t_test(Diff~DIAG, detailed = T,var.equal = TRUE) 
  t_test(Diff~DIAGNIV,  detailed = T,var.equal = TRUE) 


# Year 3
AllMSA_Pop_Baseline_671 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==0) %>% 
  filter(!is.na(Right_Putamen)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Right_Putamen) %>%
  rename("Baseline"="Right_Putamen") %>%
  left_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==3) %>%
      filter(!is.na(Right_Putamen)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Right_Putamen)
  ) %>% 
  mutate(Diff= (Right_Putamen-Baseline)/TIME_STUDY)  %>%
  t_test(Diff~DIAG, detailed = T,var.equal = TRUE) 
#t_test(Diff~DIAGNIV, detailed = T, var.equal = TRUE) 

# --------------


# Early CT Takeda  -------------------------------

# Year 1
EarlyCT_Pop_Baseline_319 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==0) %>% 
  filter(!is.na(Right_Putamen)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Right_Putamen) %>%
  rename("Baseline"="Right_Putamen") %>%
  left_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==1) %>%
      filter(!is.na(Right_Putamen)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Right_Putamen)
  ) %>% 
  mutate(Diff= (Right_Putamen-Baseline)/TIME_STUDY)  %>%
  t_test(Diff~DIAG, detailed = T,var.equal = TRUE) 
  #t_test(Diff~DIAGNIV, detailed = T, var.equal = TRUE) 


# Year 2
EarlyCT_Pop_Baseline_319 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==0) %>% 
  filter(!is.na(Right_Putamen)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Right_Putamen) %>%
  rename("Baseline"="Right_Putamen") %>%
  left_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==2) %>%
      filter(!is.na(Right_Putamen)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Right_Putamen)
  ) %>% 
  mutate(Diff= (Right_Putamen-Baseline)/TIME_STUDY)  %>%
  t_test(Diff~DIAG, detailed = T,var.equal = TRUE) 
#t_test(Diff~DIAGNIV,  detailed = T,var.equal = TRUE) 


# Year 3
EarlyCT_Pop_Baseline_319 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==0) %>% 
  filter(!is.na(Right_Putamen)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Right_Putamen) %>%
  rename("Baseline"="Right_Putamen") %>%
  left_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==3) %>%
      filter(!is.na(Right_Putamen)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Right_Putamen)
  ) %>% 
  mutate(Diff= (Right_Putamen-Baseline)/TIME_STUDY)  %>%
  t_test(Diff~DIAG, detailed = T,var.equal = TRUE) 
#t_test(Diff~DIAGNIV, detailed = T, var.equal = TRUE) 

# --------------


# Cerebellum Volume -----------------------------

irmForPaulo <- readRDS("Source/irmForPaulo.rds")

Cerebellum_VOL <- irmForPaulo %>% select(NUM, DateIRM, Left_Cerebellum_White_Matter,Right_Cerebellum_White_Matter, EstimatedTotalIntraCranialVol)
Cerebellum_VOL <- Cerebellum_VOL %>% mutate(Cerebellum=Left_Cerebellum_White_Matter+Right_Cerebellum_White_Matter)

Cerebellum_VOL <- Cerebellum_VOL %>% mutate(Cerebellum=Cerebellum/EstimatedTotalIntraCranialVol) %>% select(-EstimatedTotalIntraCranialVol)

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


# -------
# Overall MSA -------------------------------

# Year 1
AllMSA_Pop_Baseline_671 %>% inner_join(Cerebellum_VOL) %>% filter(Year==0) %>% 
  filter(!is.na(Cerebellum)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Cerebellum) %>%
  rename("Baseline"="Cerebellum") %>%
  left_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Cerebellum_VOL) %>% filter(Year==1) %>%
      filter(!is.na(Cerebellum)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Cerebellum)
  ) %>% 
  mutate(Diff= (Cerebellum-Baseline)/TIME_STUDY)  %>%
  #t_test(Diff~DIAG, detailed = T,var.equal = TRUE) 
  t_test(Diff~DIAGNIV, detailed = T, var.equal = TRUE) 


# Year 2
AllMSA_Pop_Baseline_671 %>% inner_join(Cerebellum_VOL) %>% filter(Year==0) %>% 
  filter(!is.na(Cerebellum)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Cerebellum) %>%
  rename("Baseline"="Cerebellum") %>%
  left_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Cerebellum_VOL) %>% filter(Year==2) %>%
      filter(!is.na(Cerebellum)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Cerebellum)
  ) %>% 
  mutate(Diff= (Cerebellum-Baseline)/TIME_STUDY)  %>%
  #t_test(Diff~DIAG, detailed = T,var.equal = TRUE) 
  t_test(Diff~DIAGNIV,  detailed = T,var.equal = TRUE) 


# Year 3
AllMSA_Pop_Baseline_671 %>% inner_join(Cerebellum_VOL) %>% filter(Year==0) %>% 
  filter(!is.na(Cerebellum)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Cerebellum) %>%
  rename("Baseline"="Cerebellum") %>%
  left_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Cerebellum_VOL) %>% filter(Year==3) %>%
      filter(!is.na(Cerebellum)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Cerebellum)
  ) %>% 
  mutate(Diff= (Cerebellum-Baseline)/TIME_STUDY)  %>%
  t_test(Diff~DIAG, detailed = T,var.equal = TRUE) 
#t_test(Diff~DIAGNIV, detailed = T, var.equal = TRUE) 

# --------------


# Early CT Takeda  -------------------------------

# Year 1
EarlyCT_Pop_Baseline_319 %>% inner_join(Cerebellum_VOL) %>% filter(Year==0) %>% 
  filter(!is.na(Cerebellum)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Cerebellum) %>%
  rename("Baseline"="Cerebellum") %>%
  left_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Cerebellum_VOL) %>% filter(Year==1) %>%
      filter(!is.na(Cerebellum)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Cerebellum)
  ) %>% 
  mutate(Diff= (Cerebellum-Baseline)/TIME_STUDY)  %>%
  #t_test(Diff~DIAG, detailed = T,var.equal = TRUE) 
 t_test(Diff~DIAGNIV, detailed = T, var.equal = TRUE) 


# Year 2
EarlyCT_Pop_Baseline_319 %>% inner_join(Cerebellum_VOL) %>% filter(Year==0) %>% 
  filter(!is.na(Cerebellum)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Cerebellum) %>%
  rename("Baseline"="Cerebellum") %>%
  left_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Cerebellum_VOL) %>% filter(Year==2) %>%
      filter(!is.na(Cerebellum)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Cerebellum)
  ) %>% 
  mutate(Diff= (Cerebellum-Baseline)/TIME_STUDY)  %>%
  t_test(Diff~DIAG, detailed = T,var.equal = TRUE) 
#t_test(Diff~DIAGNIV,  detailed = T,var.equal = TRUE) 


# Year 3
EarlyCT_Pop_Baseline_319 %>% inner_join(Cerebellum_VOL) %>% filter(Year==0) %>% 
  filter(!is.na(Cerebellum)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Cerebellum) %>%
  rename("Baseline"="Cerebellum") %>%
  left_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Cerebellum_VOL) %>% filter(Year==3) %>%
      filter(!is.na(Cerebellum)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Cerebellum)
  ) %>% 
  mutate(Diff= (Cerebellum-Baseline)/TIME_STUDY)  %>%
  t_test(Diff~DIAG, detailed = T,var.equal = TRUE) 
#t_test(Diff~DIAGNIV, detailed = T, var.equal = TRUE) 

# --------------


# Midbrain Volume -----------------------------


irmForPaulo <- readRDS("Source/irmForPaulo.rds")

Midbrain_VOL <- irmForPaulo %>% select(NUM, DateIRM, Midbrain, EstimatedTotalIntraCranialVol) 

Midbrain_VOL$Midbrain <- as.numeric(Midbrain_VOL$Midbrain)

Midbrain_VOL <- Midbrain_VOL %>% mutate(Midbrain=Midbrain/EstimatedTotalIntraCranialVol) %>%
  select(-EstimatedTotalIntraCranialVol)


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



# -------
# Overall MSA -------------------------------

# Year 1
AllMSA_Pop_Baseline_671 %>% inner_join(Midbrain_VOL) %>% filter(Year==0) %>% 
  filter(!is.na(Midbrain)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Midbrain) %>%
  rename("Baseline"="Midbrain") %>%
  left_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Midbrain_VOL) %>% filter(Year==1) %>%
      filter(!is.na(Midbrain)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Midbrain)
  ) %>% 
  mutate(Diff= (Midbrain-Baseline)/TIME_STUDY)  %>%
  t_test(Diff~DIAG, detailed = T,var.equal = TRUE) 
  #t_test(Diff~DIAGNIV, detailed = T, var.equal = TRUE) 


# Year 2
AllMSA_Pop_Baseline_671 %>% inner_join(Midbrain_VOL) %>% filter(Year==0) %>% 
  filter(!is.na(Midbrain)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Midbrain) %>%
  rename("Baseline"="Midbrain") %>%
  left_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Midbrain_VOL) %>% filter(Year==2) %>%
      filter(!is.na(Midbrain)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Midbrain)
  ) %>% 
  mutate(Diff= (Midbrain-Baseline)/TIME_STUDY)  %>%
  #t_test(Diff~DIAG, detailed = T,var.equal = TRUE) 
  t_test(Diff~DIAGNIV,  detailed = T,var.equal = TRUE) 


# Year 3
AllMSA_Pop_Baseline_671 %>% inner_join(Midbrain_VOL) %>% filter(Year==0) %>% 
  filter(!is.na(Midbrain)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Midbrain) %>%
  rename("Baseline"="Midbrain") %>%
  left_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Midbrain_VOL) %>% filter(Year==3) %>%
      filter(!is.na(Midbrain)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Midbrain)
  ) %>% 
  mutate(Diff= (Midbrain-Baseline)/TIME_STUDY)  %>%
  t_test(Diff~DIAG, detailed = T,var.equal = TRUE) 
#t_test(Diff~DIAGNIV, detailed = T, var.equal = TRUE) 

# --------------


# Early CT Takeda  -------------------------------

# Year 1
EarlyCT_Pop_Baseline_319 %>% inner_join(Midbrain_VOL) %>% filter(Year==0) %>% 
  filter(!is.na(Midbrain)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Midbrain) %>%
  rename("Baseline"="Midbrain") %>%
  left_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Midbrain_VOL) %>% filter(Year==1) %>%
      filter(!is.na(Midbrain)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Midbrain)
  ) %>% 
  mutate(Diff= (Midbrain-Baseline)/TIME_STUDY)  %>%
  t_test(Diff~DIAG, detailed = T,var.equal = TRUE) 
  #t_test(Diff~DIAGNIV, detailed = T, var.equal = TRUE) 


# Year 2
EarlyCT_Pop_Baseline_319 %>% inner_join(Midbrain_VOL) %>% filter(Year==0) %>% 
  filter(!is.na(Midbrain)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Midbrain) %>%
  rename("Baseline"="Midbrain") %>%
  left_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Midbrain_VOL) %>% filter(Year==2) %>%
      filter(!is.na(Midbrain)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Midbrain)
  ) %>% 
  mutate(Diff= (Midbrain-Baseline)/TIME_STUDY)  %>%
  t_test(Diff~DIAG, detailed = T,var.equal = TRUE) 
#t_test(Diff~DIAGNIV,  detailed = T,var.equal = TRUE) 


# Year 3
EarlyCT_Pop_Baseline_319 %>% inner_join(Midbrain_VOL) %>% filter(Year==0) %>% 
  filter(!is.na(Midbrain)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Midbrain) %>%
  rename("Baseline"="Midbrain") %>%
  left_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Midbrain_VOL) %>% filter(Year==3) %>%
      filter(!is.na(Midbrain)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Midbrain)
  ) %>% 
  mutate(Diff= (Midbrain-Baseline)/TIME_STUDY)  %>%
  t_test(Diff~DIAG, detailed = T,var.equal = TRUE) 
#t_test(Diff~DIAGNIV, detailed = T, var.equal = TRUE) 

# --------------


# PONS Volume -----------------------------

irmForPaulo <- readRDS("Source/irmForPaulo.rds")

Pons_VOL <- irmForPaulo %>% select(NUM, DateIRM, Pons, EstimatedTotalIntraCranialVol)

Pons_VOL$Pons <- as.numeric(Pons_VOL$Pons)

Pons_VOL <- Pons_VOL %>% mutate(Pons=Pons/EstimatedTotalIntraCranialVol) %>%
  select(-EstimatedTotalIntraCranialVol)


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




# -------
# Overall MSA -------------------------------

# Year 1
AllMSA_Pop_Baseline_671 %>% inner_join(Pons_VOL) %>% filter(Year==0) %>% 
  filter(!is.na(Pons)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Pons) %>%
  rename("Baseline"="Pons") %>%
  left_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Pons_VOL) %>% filter(Year==1) %>%
      filter(!is.na(Pons)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Pons)
  ) %>% 
  mutate(Diff= (Pons-Baseline)/TIME_STUDY)  %>%
  #t_test(Diff~DIAG, detailed = T,var.equal = TRUE) 
 t_test(Diff~DIAGNIV, detailed = T, var.equal = TRUE) 


# Year 2
AllMSA_Pop_Baseline_671 %>% inner_join(Pons_VOL) %>% filter(Year==0) %>% 
  filter(!is.na(Pons)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Pons) %>%
  rename("Baseline"="Pons") %>%
  left_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Pons_VOL) %>% filter(Year==2) %>%
      filter(!is.na(Pons)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Pons)
  ) %>% 
  mutate(Diff= (Pons-Baseline)/TIME_STUDY)  %>%
  #t_test(Diff~DIAG, detailed = T,var.equal = TRUE) 
  t_test(Diff~DIAGNIV,  detailed = T,var.equal = TRUE) 


# Year 3
AllMSA_Pop_Baseline_671 %>% inner_join(Pons_VOL) %>% filter(Year==0) %>% 
  filter(!is.na(Pons)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Pons) %>%
  rename("Baseline"="Pons") %>%
  left_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Pons_VOL) %>% filter(Year==3) %>%
      filter(!is.na(Pons)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Pons)
  ) %>% 
  mutate(Diff= (Pons-Baseline)/TIME_STUDY)  %>%
  t_test(Diff~DIAG, detailed = T,var.equal = TRUE) 
#t_test(Diff~DIAGNIV, detailed = T, var.equal = TRUE) 

# --------------


# Early CT Takeda  -------------------------------

# Year 1
EarlyCT_Pop_Baseline_319 %>% inner_join(Pons_VOL) %>% filter(Year==0) %>% 
  filter(!is.na(Pons)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Pons) %>%
  rename("Baseline"="Pons") %>%
  left_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Pons_VOL) %>% filter(Year==1) %>%
      filter(!is.na(Pons)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Pons)
  ) %>% 
  mutate(Diff= (Pons-Baseline)/TIME_STUDY)  %>%
  #t_test(Diff~DIAG, detailed = T,var.equal = TRUE) 
  t_test(Diff~DIAGNIV, detailed = T, var.equal = TRUE) 


# Year 2
EarlyCT_Pop_Baseline_319 %>% inner_join(Pons_VOL) %>% filter(Year==0) %>% 
  filter(!is.na(Pons)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Pons) %>%
  rename("Baseline"="Pons") %>%
  left_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Pons_VOL) %>% filter(Year==2) %>%
      filter(!is.na(Pons)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Pons)
  ) %>% 
  mutate(Diff= (Pons-Baseline)/TIME_STUDY)  %>%
  t_test(Diff~DIAG, detailed = T,var.equal = TRUE) 
#t_test(Diff~DIAGNIV,  detailed = T,var.equal = TRUE) 


# Year 3
EarlyCT_Pop_Baseline_319 %>% inner_join(Pons_VOL) %>% filter(Year==0) %>% 
  filter(!is.na(Pons)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Pons) %>%
  rename("Baseline"="Pons") %>%
  left_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Pons_VOL) %>% filter(Year==3) %>%
      filter(!is.na(Pons)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Pons)
  ) %>% 
  mutate(Diff= (Pons-Baseline)/TIME_STUDY)  %>%
  t_test(Diff~DIAG, detailed = T,var.equal = TRUE) 
#t_test(Diff~DIAGNIV, detailed = T, var.equal = TRUE) 

# --------------


# Medulla Volume -----------------------------


irmForPaulo <- readRDS("Source/irmForPaulo.rds")

Medulla_VOL <- irmForPaulo %>% select(NUM, DateIRM, Medulla, EstimatedTotalIntraCranialVol)

Medulla_VOL$Medulla <- as.numeric(Medulla_VOL$Medulla)

Medulla_VOL <- Medulla_VOL %>% mutate(Medulla=Medulla/EstimatedTotalIntraCranialVol) %>%
  select(-EstimatedTotalIntraCranialVol)


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



# -------
# Overall MSA -------------------------------

# Year 1
AllMSA_Pop_Baseline_671 %>% inner_join(Medulla_VOL) %>% filter(Year==0) %>% 
  filter(!is.na(Medulla)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Medulla) %>%
  rename("Baseline"="Medulla") %>%
  left_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Medulla_VOL) %>% filter(Year==1) %>%
      filter(!is.na(Medulla)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Medulla)
  ) %>% 
  mutate(Diff= (Medulla-Baseline)/TIME_STUDY)  %>%
  #t_test(Diff~DIAG, detailed = T,var.equal = TRUE) 
 t_test(Diff~DIAGNIV, detailed = T, var.equal = TRUE) 


# Year 2
AllMSA_Pop_Baseline_671 %>% inner_join(Medulla_VOL) %>% filter(Year==0) %>% 
  filter(!is.na(Medulla)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Medulla) %>%
  rename("Baseline"="Medulla") %>%
  left_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Medulla_VOL) %>% filter(Year==2) %>%
      filter(!is.na(Medulla)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Medulla)
  ) %>% 
  mutate(Diff= (Medulla-Baseline)/TIME_STUDY)  %>%
  #t_test(Diff~DIAG, detailed = T,var.equal = TRUE) 
  t_test(Diff~DIAGNIV,  detailed = T,var.equal = TRUE) 


# Year 3
AllMSA_Pop_Baseline_671 %>% inner_join(Medulla_VOL) %>% filter(Year==0) %>% 
  filter(!is.na(Medulla)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Medulla) %>%
  rename("Baseline"="Medulla") %>%
  left_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Medulla_VOL) %>% filter(Year==3) %>%
      filter(!is.na(Medulla)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Medulla)
  ) %>% 
  mutate(Diff= (Medulla-Baseline)/TIME_STUDY)  %>%
  t_test(Diff~DIAG, detailed = T,var.equal = TRUE) 
#t_test(Diff~DIAGNIV, detailed = T, var.equal = TRUE) 

# --------------


# Early CT Takeda  -------------------------------

# Year 1
EarlyCT_Pop_Baseline_319 %>% inner_join(Medulla_VOL) %>% filter(Year==0) %>% 
  filter(!is.na(Medulla)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Medulla) %>%
  rename("Baseline"="Medulla") %>%
  left_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Medulla_VOL) %>% filter(Year==1) %>%
      filter(!is.na(Medulla)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Medulla)
  ) %>% 
  mutate(Diff= (Medulla-Baseline)/TIME_STUDY)  %>%
  t_test(Diff~DIAG, detailed = T,var.equal = TRUE) 
  #t_test(Diff~DIAGNIV, detailed = T, var.equal = TRUE) 


# Year 2
EarlyCT_Pop_Baseline_319 %>% inner_join(Medulla_VOL) %>% filter(Year==0) %>% 
  filter(!is.na(Medulla)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Medulla) %>%
  rename("Baseline"="Medulla") %>%
  left_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Medulla_VOL) %>% filter(Year==2) %>%
      filter(!is.na(Medulla)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Medulla)
  ) %>% 
  mutate(Diff= (Medulla-Baseline)/TIME_STUDY)  %>%
  t_test(Diff~DIAG, detailed = T,var.equal = TRUE) 
#t_test(Diff~DIAGNIV,  detailed = T,var.equal = TRUE) 


# Year 3
EarlyCT_Pop_Baseline_319 %>% inner_join(Medulla_VOL) %>% filter(Year==0) %>% 
  filter(!is.na(Medulla)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Medulla) %>%
  rename("Baseline"="Medulla") %>%
  left_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Medulla_VOL) %>% filter(Year==3) %>%
      filter(!is.na(Medulla)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Medulla)
  ) %>% 
  mutate(Diff= (Medulla-Baseline)/TIME_STUDY)  %>%
  t_test(Diff~DIAG, detailed = T,var.equal = TRUE) 
#t_test(Diff~DIAGNIV, detailed = T, var.equal = TRUE) 

# --------------


# Total Volume -----------------------------

irmForPaulo <- readRDS("Source/irmForPaulo.rds")
Total_VOL <- irmForPaulo %>% select(NUM, DateIRM, CerebralWhiteMatterVol, TotalGrayVol, EstimatedTotalIntraCranialVol)

Total_VOL <- Total_VOL %>% mutate(Total=as.numeric(CerebralWhiteMatterVol)+as.numeric(TotalGrayVol) )

Total_VOL <- Total_VOL %>% mutate(Total=Total/EstimatedTotalIntraCranialVol) %>%
  select(-EstimatedTotalIntraCranialVol)
 
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




# -------
# Overall MSA -------------------------------

# Year 1
AllMSA_Pop_Baseline_671 %>% inner_join(Total_VOL) %>% filter(Year==0) %>% 
  filter(!is.na(Total)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Total) %>%
  rename("Baseline"="Total") %>%
  left_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Total_VOL) %>% filter(Year==1) %>%
      filter(!is.na(Total)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Total)
  ) %>% 
  mutate(Diff= (Total-Baseline)/TIME_STUDY)  %>%
  t_test(Diff~DIAG, detailed = T,var.equal = TRUE) 
  #t_test(Diff~DIAGNIV, detailed = T, var.equal = TRUE) 


# Year 2
AllMSA_Pop_Baseline_671 %>% inner_join(Total_VOL) %>% filter(Year==0) %>% 
  filter(!is.na(Total)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Total) %>%
  rename("Baseline"="Total") %>%
  left_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Total_VOL) %>% filter(Year==2) %>%
      filter(!is.na(Total)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Total)
  ) %>% 
  mutate(Diff= (Total-Baseline)/TIME_STUDY)  %>%
  #t_test(Diff~DIAG, detailed = T,var.equal = TRUE) 
  t_test(Diff~DIAGNIV,  detailed = T,var.equal = TRUE) 


# Year 3
AllMSA_Pop_Baseline_671 %>% inner_join(Total_VOL) %>% filter(Year==0) %>% 
  filter(!is.na(Total)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Total) %>%
  rename("Baseline"="Total") %>%
  left_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(Total_VOL) %>% filter(Year==3) %>%
      filter(!is.na(Total)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Total)
  ) %>% 
  mutate(Diff= (Total-Baseline)/TIME_STUDY)  %>%
  t_test(Diff~DIAG, detailed = T,var.equal = TRUE) 
#t_test(Diff~DIAGNIV, detailed = T, var.equal = TRUE) 

# --------------


# Early CT Takeda  -------------------------------

# Year 1
EarlyCT_Pop_Baseline_319 %>% inner_join(Total_VOL) %>% filter(Year==0) %>% 
  filter(!is.na(Total)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Total) %>%
  rename("Baseline"="Total") %>%
  left_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Total_VOL) %>% filter(Year==1) %>%
      filter(!is.na(Total)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Total)
  ) %>% 
  mutate(Diff= (Total-Baseline)/TIME_STUDY)  %>%
  #t_test(Diff~DIAG, detailed = T,var.equal = TRUE) 
t_test(Diff~DIAGNIV, detailed = T, var.equal = TRUE) 


# Year 2
EarlyCT_Pop_Baseline_319 %>% inner_join(Total_VOL) %>% filter(Year==0) %>% 
  filter(!is.na(Total)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Total) %>%
  rename("Baseline"="Total") %>%
  left_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Total_VOL) %>% filter(Year==2) %>%
      filter(!is.na(Total)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Total)
  ) %>% 
  mutate(Diff= (Total-Baseline)/TIME_STUDY)  %>%
  t_test(Diff~DIAG, detailed = T,var.equal = TRUE) 
#t_test(Diff~DIAGNIV,  detailed = T,var.equal = TRUE) 


# Year 3
EarlyCT_Pop_Baseline_319 %>% inner_join(Total_VOL) %>% filter(Year==0) %>% 
  filter(!is.na(Total)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Total) %>%
  rename("Baseline"="Total") %>%
  left_join(
    EarlyCT_Pop_Baseline_319 %>% inner_join(Total_VOL) %>% filter(Year==3) %>%
      filter(!is.na(Total)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Total)
  ) %>% 
  mutate(Diff= (Total-Baseline)/TIME_STUDY)  %>%
  t_test(Diff~DIAG, detailed = T,var.equal = TRUE) 
#t_test(Diff~DIAGNIV, detailed = T, var.equal = TRUE) 

# --------------


# Correlations between baseline NORMALIZED VOLUMETRIC --------
# --------
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

# ---------------


# Left Putamen Volume -----------------------------

irmForPaulo <- readRDS("Source/irmForPaulo.rds")

Left_Putamen_VOL <- irmForPaulo %>% select(NUM, DateIRM, Left_Putamen, EstimatedTotalIntraCranialVol) %>%
  mutate(Left_Putamen=Left_Putamen/EstimatedTotalIntraCranialVol) %>% select(-EstimatedTotalIntraCranialVol)

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


Left_Putamen_VOL <- AllMSA_Pop_Baseline_671 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==0) %>% 
  filter(!is.na(Left_Putamen)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Left_Putamen)



# ---------------
# Right Putamen Volume -----------------------------

irmForPaulo <- readRDS("Source/irmForPaulo.rds")

Right_Putamen_VOL <- irmForPaulo %>% select(NUM, DateIRM, Right_Putamen, EstimatedTotalIntraCranialVol) %>%
  mutate(Right_Putamen=Right_Putamen/EstimatedTotalIntraCranialVol) %>% select(-EstimatedTotalIntraCranialVol)
                                                                             
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

Right_Putamen_VOL <- AllMSA_Pop_Baseline_671 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==0) %>% 
  filter(!is.na(Right_Putamen)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Right_Putamen)

# -------
# Cerebellum Volume -----------------------------

irmForPaulo <- readRDS("Source/irmForPaulo.rds")

Cerebellum_VOL <- irmForPaulo %>% select(NUM, DateIRM, Left_Cerebellum_White_Matter,Right_Cerebellum_White_Matter,EstimatedTotalIntraCranialVol)

Cerebellum_VOL <- Cerebellum_VOL %>% mutate(Cerebellum=Left_Cerebellum_White_Matter+Right_Cerebellum_White_Matter)
Cerebellum_VOL <- Cerebellum_VOL %>% mutate(Cerebellum=Cerebellum/EstimatedTotalIntraCranialVol) %>% select(-EstimatedTotalIntraCranialVol)



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

Cerebellum_VOL <- AllMSA_Pop_Baseline_671 %>% inner_join(Cerebellum_VOL) %>% filter(Year==0) %>% 
  filter(!is.na(Cerebellum)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Cerebellum)

# -------
# Midbrain Volume -----------------------------


irmForPaulo <- readRDS("Source/irmForPaulo.rds")

Midbrain_VOL <- irmForPaulo %>% select(NUM, DateIRM, Midbrain, EstimatedTotalIntraCranialVol)

Midbrain_VOL$Midbrain <- as.numeric(Midbrain_VOL$Midbrain)

Midbrain_VOL <- Midbrain_VOL %>% mutate(Midbrain=Midbrain/EstimatedTotalIntraCranialVol) %>% select(-EstimatedTotalIntraCranialVol)

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


Midbrain_VOL <- AllMSA_Pop_Baseline_671 %>% inner_join(Midbrain_VOL) %>% filter(Year==0) %>% 
  filter(!is.na(Midbrain)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Midbrain)

# -------
# PONS Volume -----------------------------

irmForPaulo <- readRDS("Source/irmForPaulo.rds")

Pons_VOL <- irmForPaulo %>% select(NUM, DateIRM, Pons, EstimatedTotalIntraCranialVol)

Pons_VOL$Pons <- as.numeric(Pons_VOL$Pons)

Pons_VOL <- Pons_VOL %>% mutate(Pons=Pons/EstimatedTotalIntraCranialVol) %>% select(-EstimatedTotalIntraCranialVol)

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


Pons_VOL <- AllMSA_Pop_Baseline_671 %>% inner_join(Pons_VOL) %>% filter(Year==0) %>% 
  filter(!is.na(Pons)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Pons)



# -------
# Medulla Volume -----------------------------


irmForPaulo <- readRDS("Source/irmForPaulo.rds")

Medulla_VOL <- irmForPaulo %>% select(NUM, DateIRM, Medulla, EstimatedTotalIntraCranialVol)

Medulla_VOL$Medulla <- as.numeric(Medulla_VOL$Medulla)

Medulla_VOL <- Medulla_VOL %>% mutate(Medulla=Medulla/EstimatedTotalIntraCranialVol) %>% select(-EstimatedTotalIntraCranialVol)

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

Medulla_VOL <- AllMSA_Pop_Baseline_671 %>% inner_join(Medulla_VOL) %>% filter(Year==0) %>% 
  filter(!is.na(Medulla)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Medulla)



# -------
# Total Volume -----------------------------

irmForPaulo <- readRDS("Source/irmForPaulo.rds")
Total_VOL <- irmForPaulo %>% select(NUM, DateIRM, CerebralWhiteMatterVol, TotalGrayVol, EstimatedTotalIntraCranialVol)

Total_VOL <- Total_VOL %>% mutate(Total=as.numeric(CerebralWhiteMatterVol)+as.numeric(TotalGrayVol) )

Total_VOL <- Total_VOL %>% mutate(Total=Total/EstimatedTotalIntraCranialVol ) %>% select(-EstimatedTotalIntraCranialVol)


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


Total_VOL <- AllMSA_Pop_Baseline_671 %>% inner_join(Total_VOL) %>% filter(Year==0) %>% 
  filter(!is.na(Total)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, Total)




# -------
# Correlations baseline Overall and Early CT -----------

# Overall

Baselines <- Left_Putamen_VOL %>% inner_join(Right_Putamen_VOL) %>%
  inner_join(Cerebellum_VOL) %>% inner_join(Midbrain_VOL) %>%
  inner_join(Pons_VOL) %>% inner_join(Medulla_VOL) %>%
  inner_join(Total_VOL) %>% select(-NUM) 

Correlations <- Baselines %>% cor(method = "spearman")

round(Correlations, 3)

get_p_value <- function(x, y) {
  cor_test <- cor.test(x, y, method = "spearman")
  broom::tidy(cor_test)$p.value
}

p_value_matrix <- matrix(NA, ncol = 7, nrow = 7, 
                         dimnames = list(names(Baselines), names(Baselines)))


for (i in 1:ncol(Baselines)) {
  for (j in 1:ncol(Baselines)) {
    p_value_matrix[i, j] <- get_p_value(Baselines[[i]], Baselines[[j]])
  }
}

Correlations <- round(Correlations, 3)
p_value_matrix <- round(p_value_matrix, 3)


combined_matrix <- matrix(NA, ncol = 7, nrow = 7, 
                          dimnames = list(names(Baselines), names(Baselines)))


for (i in 1:ncol(Baselines)) {
  for (j in 1:ncol(Baselines)) {
    combined_matrix[i, j] <- paste0(Correlations[i, j], ", ", p_value_matrix[i, j])
  }
}


# Target

Baselines <- Left_Putamen_VOL %>% inner_join(Right_Putamen_VOL) %>%
  inner_join(Cerebellum_VOL) %>% inner_join(Midbrain_VOL) %>%
  inner_join(Pons_VOL) %>% inner_join(Medulla_VOL) %>%
  inner_join(Total_VOL) %>% inner_join(EarlyCT_Pop_Baseline_319 %>% select(NUM)) %>% select(-NUM) 

Correlations <- Baselines %>% cor(method = "spearman")

round(Correlations, 3)

get_p_value <- function(x, y) {
  cor_test <- cor.test(x, y, method = "spearman")
  broom::tidy(cor_test)$p.value
}

p_value_matrix <- matrix(NA, ncol = 7, nrow = 7, 
                         dimnames = list(names(Baselines), names(Baselines)))


for (i in 1:ncol(Baselines)) {
  for (j in 1:ncol(Baselines)) {
    p_value_matrix[i, j] <- get_p_value(Baselines[[i]], Baselines[[j]])
  }
}

Correlations <- round(Correlations, 3)
p_value_matrix <- round(p_value_matrix, 3)


combined_matrix <- matrix(NA, ncol = 7, nrow = 7, 
                          dimnames = list(names(Baselines), names(Baselines)))


for (i in 1:ncol(Baselines)) {
  for (j in 1:ncol(Baselines)) {
    combined_matrix[i, j] <- paste0(Correlations[i, j], ", ", p_value_matrix[i, j])
  }
}


# -----
# Correlations between Deltas Year 1 NORMALIZED VOLUMETRIC --------
# --------
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

# ---------------


# Left Putamen Volume -----------------------------

irmForPaulo <- readRDS("Source/irmForPaulo.rds")

Left_Putamen_VOL <- irmForPaulo %>% select(NUM, DateIRM, Left_Putamen, EstimatedTotalIntraCranialVol) %>%
  mutate(Left_Putamen=Left_Putamen/EstimatedTotalIntraCranialVol) %>% select(-EstimatedTotalIntraCranialVol)

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


Left_Putamen_VOL <- AllMSA_Pop_Baseline_671 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==0) %>%
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
  mutate(Diff=(Left_Putamen-Baseline)) %>% select(NUM, Diff) %>% rename("Left_Putamen"="Diff")



# ---------------
# Right Putamen Volume -----------------------------

irmForPaulo <- readRDS("Source/irmForPaulo.rds")

Right_Putamen_VOL <- irmForPaulo %>% select(NUM, DateIRM, Right_Putamen, EstimatedTotalIntraCranialVol) %>%
  mutate(Right_Putamen=Right_Putamen/EstimatedTotalIntraCranialVol) %>% select(-EstimatedTotalIntraCranialVol)

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

Right_Putamen_VOL <- AllMSA_Pop_Baseline_671 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==0) %>%
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
  mutate(Diff=(Right_Putamen-Baseline)) %>% select(NUM, Diff) %>% rename("Right_Putamen"="Diff")

# -------
# Cerebellum Volume -----------------------------

irmForPaulo <- readRDS("Source/irmForPaulo.rds")

Cerebellum_VOL <- irmForPaulo %>% select(NUM, DateIRM, Left_Cerebellum_White_Matter,Right_Cerebellum_White_Matter,EstimatedTotalIntraCranialVol)

Cerebellum_VOL <- Cerebellum_VOL %>% mutate(Cerebellum=Left_Cerebellum_White_Matter+Right_Cerebellum_White_Matter) %>%
  mutate(Cerebellum=Cerebellum/EstimatedTotalIntraCranialVol) %>% select(-EstimatedTotalIntraCranialVol)

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

Cerebellum_VOL <- AllMSA_Pop_Baseline_671 %>% inner_join(Cerebellum_VOL) %>% filter(Year==0) %>%
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
  mutate(Diff=(Cerebellum-Baseline)) %>% select(NUM, Diff) %>% rename("Cerebellum"="Diff")


# -------
# Midbrain Volume -----------------------------


irmForPaulo <- readRDS("Source/irmForPaulo.rds")

Midbrain_VOL <- irmForPaulo %>% select(NUM, DateIRM, Midbrain, EstimatedTotalIntraCranialVol)

Midbrain_VOL$Midbrain <- as.numeric(Midbrain_VOL$Midbrain)

Midbrain_VOL <- Midbrain_VOL %>% mutate(Midbrain=Midbrain/EstimatedTotalIntraCranialVol) %>% select(-EstimatedTotalIntraCranialVol)


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


Midbrain_VOL <- AllMSA_Pop_Baseline_671 %>% inner_join(Midbrain_VOL) %>% filter(Year==0) %>%
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
  mutate(Diff=(Midbrain-Baseline)) %>% select(NUM, Diff) %>% rename("Midbrain"="Diff")

# -------
# PONS Volume -----------------------------

irmForPaulo <- readRDS("Source/irmForPaulo.rds")

Pons_VOL <- irmForPaulo %>% select(NUM, DateIRM, Pons, EstimatedTotalIntraCranialVol)

Pons_VOL$Pons <- as.numeric(Pons_VOL$Pons)

Pons_VOL <- Pons_VOL %>% mutate(Pons=Pons/EstimatedTotalIntraCranialVol) %>% select(-EstimatedTotalIntraCranialVol)

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


Pons_VOL <- AllMSA_Pop_Baseline_671 %>% inner_join(Pons_VOL) %>% filter(Year==0) %>%
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
  mutate(Diff=(Pons-Baseline)) %>% select(NUM, Diff) %>% rename("Pons"="Diff")



# -------
# Medulla Volume -----------------------------


irmForPaulo <- readRDS("Source/irmForPaulo.rds")

Medulla_VOL <- irmForPaulo %>% select(NUM, DateIRM, Medulla, EstimatedTotalIntraCranialVol)

Medulla_VOL$Medulla <- as.numeric(Medulla_VOL$Medulla)

Medulla_VOL <- Medulla_VOL %>% mutate(Medulla=Medulla/EstimatedTotalIntraCranialVol) %>% select(-EstimatedTotalIntraCranialVol)

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

Medulla_VOL <- AllMSA_Pop_Baseline_671 %>% inner_join(Medulla_VOL) %>% filter(Year==0) %>%
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
  mutate(Diff=(Medulla-Baseline)) %>% select(NUM, Diff) %>% rename("Medulla"="Diff")



# -------
# Total Volume -----------------------------

irmForPaulo <- readRDS("Source/irmForPaulo.rds")
Total_VOL <- irmForPaulo %>% select(NUM, DateIRM, CerebralWhiteMatterVol, TotalGrayVol, EstimatedTotalIntraCranialVol)

Total_VOL <- Total_VOL %>% mutate(Total=as.numeric(CerebralWhiteMatterVol)+as.numeric(TotalGrayVol) )

Total_VOL <- Total_VOL %>% mutate(Total=Total/EstimatedTotalIntraCranialVol) %>% select(-EstimatedTotalIntraCranialVol)

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


Total_VOL <- AllMSA_Pop_Baseline_671 %>% inner_join(Total_VOL) %>% filter(Year==0) %>%
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
  mutate(Diff=(Total-Baseline)) %>% select(NUM, Diff) %>% rename("Total"="Diff")


# -------
# Correlations Deltas Year 1 Overall and Early CT -----------

# Overall

Baselines <- Left_Putamen_VOL %>% inner_join(Right_Putamen_VOL) %>%
  inner_join(Cerebellum_VOL) %>% inner_join(Midbrain_VOL) %>%
  inner_join(Pons_VOL) %>% inner_join(Medulla_VOL) %>%
  inner_join(Total_VOL) %>% select(-NUM) 

Correlations <- Baselines %>% cor(method = "spearman")

round(Correlations, 3)

get_p_value <- function(x, y) {
  cor_test <- cor.test(x, y, method = "spearman")
  broom::tidy(cor_test)$p.value
}

p_value_matrix <- matrix(NA, ncol = 7, nrow = 7, 
                         dimnames = list(names(Baselines), names(Baselines)))


for (i in 1:ncol(Baselines)) {
  for (j in 1:ncol(Baselines)) {
    p_value_matrix[i, j] <- get_p_value(Baselines[[i]], Baselines[[j]])
  }
}

Correlations <- round(Correlations, 3)
p_value_matrix <- round(p_value_matrix, 3)


combined_matrix <- matrix(NA, ncol = 7, nrow = 7, 
                          dimnames = list(names(Baselines), names(Baselines)))


for (i in 1:ncol(Baselines)) {
  for (j in 1:ncol(Baselines)) {
    combined_matrix[i, j] <- paste0(Correlations[i, j], ", ", p_value_matrix[i, j])
  }
}


# Target

Baselines <- Left_Putamen_VOL %>% inner_join(Right_Putamen_VOL) %>%
  inner_join(Cerebellum_VOL) %>% inner_join(Midbrain_VOL) %>%
  inner_join(Pons_VOL) %>% inner_join(Medulla_VOL) %>%
  inner_join(Total_VOL) %>% inner_join(EarlyCT_Pop_Baseline_319 %>% select(NUM)) %>% select(-NUM) 

Correlations <- Baselines %>% cor(method = "spearman")

round(Correlations, 3)

get_p_value <- function(x, y) {
  cor_test <- cor.test(x, y, method = "spearman")
  broom::tidy(cor_test)$p.value
}

p_value_matrix <- matrix(NA, ncol = 7, nrow = 7, 
                         dimnames = list(names(Baselines), names(Baselines)))


for (i in 1:ncol(Baselines)) {
  for (j in 1:ncol(Baselines)) {
    p_value_matrix[i, j] <- get_p_value(Baselines[[i]], Baselines[[j]])
  }
}

Correlations <- round(Correlations, 3)
p_value_matrix <- round(p_value_matrix, 3)


combined_matrix <- matrix(NA, ncol = 7, nrow = 7, 
                          dimnames = list(names(Baselines), names(Baselines)))


for (i in 1:ncol(Baselines)) {
  for (j in 1:ncol(Baselines)) {
    combined_matrix[i, j] <- paste0(Correlations[i, j], ", ", p_value_matrix[i, j])
  }
}


# -----

# Correlations between Standardized Deltas Year 1 NORMALIZED VOLUMETRIC --------
# --------
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

# ---------------


# Left Putamen Volume -----------------------------

irmForPaulo <- readRDS("Source/irmForPaulo.rds")

Left_Putamen_VOL <- irmForPaulo %>% select(NUM, DateIRM, Left_Putamen, EstimatedTotalIntraCranialVol) %>%
  mutate(Left_Putamen=Left_Putamen/EstimatedTotalIntraCranialVol) %>% select(-EstimatedTotalIntraCranialVol)

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


Left_Putamen_VOL <- AllMSA_Pop_Baseline_671 %>% inner_join(Left_Putamen_VOL) %>% filter(Year==0) %>%
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
  mutate(Diff=(Left_Putamen-Baseline)/TIME_STUDY) %>% select(NUM, Diff) %>% rename("Left_Putamen"="Diff")



# ---------------
# Right Putamen Volume -----------------------------

irmForPaulo <- readRDS("Source/irmForPaulo.rds")

Right_Putamen_VOL <- irmForPaulo %>% select(NUM, DateIRM, Right_Putamen, EstimatedTotalIntraCranialVol)

Right_Putamen_VOL <- Right_Putamen_VOL %>% mutate(Right_Putamen=Right_Putamen/EstimatedTotalIntraCranialVol) %>% select(-EstimatedTotalIntraCranialVol)

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

Right_Putamen_VOL <- AllMSA_Pop_Baseline_671 %>% inner_join(Right_Putamen_VOL) %>% filter(Year==0) %>%
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
  mutate(Diff=(Right_Putamen-Baseline)/TIME_STUDY) %>% select(NUM, Diff) %>% rename("Right_Putamen"="Diff")

# -------
# Cerebellum Volume -----------------------------

irmForPaulo <- readRDS("Source/irmForPaulo.rds")

Cerebellum_VOL <- irmForPaulo %>% select(NUM, DateIRM, Left_Cerebellum_White_Matter,Right_Cerebellum_White_Matter,EstimatedTotalIntraCranialVol)

Cerebellum_VOL <- Cerebellum_VOL %>% mutate(Cerebellum=Left_Cerebellum_White_Matter+Right_Cerebellum_White_Matter) %>%
  mutate(Cerebellum=Cerebellum/EstimatedTotalIntraCranialVol) %>% select(-EstimatedTotalIntraCranialVol)

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

Cerebellum_VOL <- AllMSA_Pop_Baseline_671 %>% inner_join(Cerebellum_VOL) %>% filter(Year==0) %>%
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
  mutate(Diff=(Cerebellum-Baseline)/TIME_STUDY) %>% select(NUM, Diff) %>% rename("Cerebellum"="Diff")


# -------
# Midbrain Volume -----------------------------


irmForPaulo <- readRDS("Source/irmForPaulo.rds")

Midbrain_VOL <- irmForPaulo %>% select(NUM, DateIRM, Midbrain, EstimatedTotalIntraCranialVol) 

Midbrain_VOL$Midbrain <- as.numeric(Midbrain_VOL$Midbrain)

Midbrain_VOL <- Midbrain_VOL %>% mutate(Midbrain=Midbrain/EstimatedTotalIntraCranialVol)

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


Midbrain_VOL <- AllMSA_Pop_Baseline_671 %>% inner_join(Midbrain_VOL) %>% filter(Year==0) %>%
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
  mutate(Diff=(Midbrain-Baseline)/TIME_STUDY) %>% select(NUM, Diff) %>% rename("Midbrain"="Diff")

# -------
# PONS Volume -----------------------------

irmForPaulo <- readRDS("Source/irmForPaulo.rds")

Pons_VOL <- irmForPaulo %>% select(NUM, DateIRM, Pons, EstimatedTotalIntraCranialVol)

Pons_VOL$Pons <- as.numeric(Pons_VOL$Pons)

Pons_VOL <- Pons_VOL %>% mutate(Pons=Pons/EstimatedTotalIntraCranialVol) %>% select(-EstimatedTotalIntraCranialVol)

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


Pons_VOL <- AllMSA_Pop_Baseline_671 %>% inner_join(Pons_VOL) %>% filter(Year==0) %>%
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
  mutate(Diff=(Pons-Baseline)/TIME_STUDY) %>% select(NUM, Diff) %>% rename("Pons"="Diff")



# -------
# Medulla Volume -----------------------------


irmForPaulo <- readRDS("Source/irmForPaulo.rds")

Medulla_VOL <- irmForPaulo %>% select(NUM, DateIRM, Medulla, EstimatedTotalIntraCranialVol)

Medulla_VOL$Medulla <- as.numeric(Medulla_VOL$Medulla)

Medulla_VOL <- Medulla_VOL %>% mutate(Medulla=Medulla/EstimatedTotalIntraCranialVol) %>%
  select(-EstimatedTotalIntraCranialVol)

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

Medulla_VOL <- AllMSA_Pop_Baseline_671 %>% inner_join(Medulla_VOL) %>% filter(Year==0) %>%
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
  mutate(Diff=(Medulla-Baseline)/TIME_STUDY) %>% select(NUM, Diff) %>% rename("Medulla"="Diff")



# -------
# Total Volume -----------------------------

irmForPaulo <- readRDS("Source/irmForPaulo.rds")
Total_VOL <- irmForPaulo %>% select(NUM, DateIRM, CerebralWhiteMatterVol, TotalGrayVol, EstimatedTotalIntraCranialVol)

Total_VOL <- Total_VOL %>% mutate(Total=as.numeric(CerebralWhiteMatterVol)+as.numeric(TotalGrayVol) )

Total_VOL <- Total_VOL %>% mutate(Total=Total/EstimatedTotalIntraCranialVol) %>% select(-EstimatedTotalIntraCranialVol)

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


Total_VOL <- AllMSA_Pop_Baseline_671 %>% inner_join(Total_VOL) %>% filter(Year==0) %>%
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
  mutate(Diff=(Total-Baseline)/TIME_STUDY) %>% select(NUM, Diff) %>% rename("Total"="Diff")

# -------------
# UMSARS 1 DA-modified exc. #11 collapsed ---------
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
UMSARS1 <-  UMSARS1 %>%  select(NUM, TIME_STUDY, Year, UMSARS1_TOT_v2, UMSARS1_11)
UMSARS1 <- UMSARS1 %>% mutate(UMSARS1_TOT_FDA=UMSARS1_TOT_v2-UMSARS1_11)

UMSARS1 <- AllMSA_Pop_Baseline_671 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
  filter(!is.na(UMSARS1_TOT_FDA)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  UMSARS1_TOT_FDA) %>%
  rename("Baseline"="UMSARS1_TOT_FDA") %>%
  left_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(UMSARS1) %>% filter(Year==1) %>%
      filter(!is.na(UMSARS1_TOT_FDA)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, UMSARS1_TOT_FDA)
  ) %>% 
  mutate(Diff= (UMSARS1_TOT_FDA-Baseline)/TIME_STUDY) %>% select(NUM, Diff) %>% rename("UMSARS1_TOT_FDA"="Diff")

# -------


# SCHRAG MSA QoL ----------


dataCohorteManaged <- readRDS("Source/dataCohorteManaged.rds")

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

range(SCHRAG$SCHRAG_TOT_v2)

names(SCHRAG)

SCHRAG$all_na <- rowSums(is.na(SCHRAG[, 5:44])) == (44 - 5 + 1)

SCHRAG$number_na <- rowSums(is.na(SCHRAG[, 5:44])) 

SCHRAG <- SCHRAG %>% filter(!(all_na) )

names(SCHRAG)

range(SCHRAG$number_na)

SCHRAG %>% ggplot(aes(number_na)) + geom_histogram()

mean(SCHRAG$number_na)

SCHRAG_Baseline_Pats <- SCHRAG %>% filter(Year==0) %>% filter(!is.na(SCHRAG_TOT_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM) %>% distinct() %>%
  inner_join(AllMSA_Pop_Baseline_671)

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

SCHRAG <- AllMSA_Pop_Baseline_671 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_TOT_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_TOT_v2) %>%
  rename("Baseline"="SCHRAG_TOT_v2") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
      filter(!is.na(SCHRAG_TOT_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_TOT_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_TOT_v2-Baseline)/TIME_STUDY) %>% inner_join(SCHRAG_Baseline_Pats) %>% 
  select(NUM, Diff) %>% rename("SCHRAG"="Diff")

# -----------

# SCOPA ------------




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




mean(ams_scopa_bx_2015_16$SCOPA_TOT, na.rm=T)

ams_scopa_bx_2015_16 <- ams_scopa_bx_2015_16 %>%
  mutate(SCOPA_DIG = rowSums(select(., SCOPA1,SCOPA2,SCOPA3,SCOPA4,SCOPA5,SCOPA6,SCOPA7), na.rm = TRUE)) %>%
  mutate(SCOPA_URI = rowSums(select(., SCOPA8,SCOPA9,SCOPA10,SCOPA11,SCOPA12,SCOPA13), na.rm = TRUE)) %>%
  mutate(SCOPA_CDV = rowSums(select(., SCOPA14,SCOPA15,SCOPA16), na.rm = TRUE)) %>%
  mutate(SCOPA_THE = rowSums(select(., SCOPA17,SCOPA18,SCOPA19,SCOPA20), na.rm = TRUE)) %>%
  mutate(SCOPA_PUP = rowSums(select(., SCOPA21), na.rm = TRUE)) %>%
  mutate(SCOPA_SEX = rowSums(select(., SCOPA22H,SCOPA22F,SCOPA23F,SCOPA23H), na.rm = TRUE)) %>%
  mutate(SCOPA_TOT = rowSums(select(., SCOPA_SEX,SCOPA_PUP,SCOPA_THE,SCOPA_CDV,SCOPA_URI,SCOPA_DIG), na.rm = TRUE))


columns_to_check <- c("SCOPA1", "SCOPA2", "SCOPA3",
                      "SCOPA4", "SCOPA5", "SCOPA6",
                      "SCOPA7", "SCOPA8", "SCOPA9",
                      "SCOPA10", "SCOPA11", "SCOPA12",
                      "SCOPA13", "SCOPA14", "SCOPA15",
                      "SCOPA16", "SCOPA17", "SCOPA18",
                      "SCOPA19", "SCOPA20", "SCOPA21",
                      "SCOPA22H", "SCOPA22F", "SCOPA23F", "SCOPA23H"
)

ams_scopa_bx_2015_16 <- ams_scopa_bx_2015_16 %>%
  mutate(
    number_na = rowSums(is.na(select(., all_of(columns_to_check))))
  )

ams_scopa_bx_2015_16 <- ams_scopa_bx_2015_16 %>%
  mutate(number_na=number_na-2) %>%
  mutate(number_na=ifelse(number_na<0,0,number_na))


names(ams_scopa_bx_2015_16)

ams_scopa_bx_2015_16$dig_na <- rowSums(is.na(ams_scopa_bx_2015_16[, 3:9])) # 7
ams_scopa_bx_2015_16$uri_na <- rowSums(is.na(ams_scopa_bx_2015_16[, 11:16])) # 6
ams_scopa_bx_2015_16$cdv_na <- rowSums(is.na(ams_scopa_bx_2015_16[, 18:20])) # 3
ams_scopa_bx_2015_16$the_na <- rowSums(is.na(ams_scopa_bx_2015_16[, 22:25])) # 4
ams_scopa_bx_2015_16$pup_na <- rowSums(is.na(ams_scopa_bx_2015_16[, 27:27])) # 1
ams_scopa_bx_2015_16$sex_na <- rowSums(is.na(ams_scopa_bx_2015_16[, 29:32])) # 2

ams_scopa_bx_2015_16 <- ams_scopa_bx_2015_16 %>%
  mutate(sex_na=sex_na-2) %>%
  mutate(sex_na=ifelse(sex_na<0,0,sex_na))



range(ams_scopa_bx_2015_16$dig_na)
range(ams_scopa_bx_2015_16$uri_na)
range(ams_scopa_bx_2015_16$cdv_na)
range(ams_scopa_bx_2015_16$the_na)
range(ams_scopa_bx_2015_16$pup_na)
range(ams_scopa_bx_2015_16$sex_na)

ams_scopa_bx_2015_16 <- ams_scopa_bx_2015_16 %>%
  select(NUM, DATE, SCOPA_TOT, SCOPA_SEX, SCOPA_PUP, SCOPA_THE, SCOPA_CDV, SCOPA_URI, SCOPA_DIG,
         dig_na, uri_na, cdv_na, the_na, pup_na, sex_na)




ams_scopa_bx_2015_16 <- ams_scopa_bx_2015_16 %>%  ungroup() %>%
  mutate(SCOPA_DIG = SCOPA_DIG / ((7-dig_na)/7) ) %>%
  mutate(SCOPA_URI = SCOPA_URI / ((6-uri_na)/6) ) %>%
  mutate(SCOPA_CDV = SCOPA_CDV / ((3-cdv_na)/3) ) %>%
  mutate(SCOPA_THE = SCOPA_THE / ((4-the_na)/4) ) %>%
  mutate(SCOPA_PUP = SCOPA_PUP/ ((1-pup_na)/1) ) %>%
  mutate(SCOPA_SEX = SCOPA_SEX/ ((2-sex_na)/2) )

mean(ams_scopa_bx_2015_16$SCOPA_TOT)


ams_scopa_bx_2015_16 <- ams_scopa_bx_2015_16 %>%
  mutate(SCOPA_TOT = rowSums(select(., SCOPA_SEX,SCOPA_PUP,SCOPA_THE,SCOPA_CDV,SCOPA_URI,SCOPA_DIG), na.rm = TRUE))

ams_scopa_bx_2015_16 <- ams_scopa_bx_2015_16 %>%
  select(NUM, DATE, SCOPA_TOT, SCOPA_SEX, SCOPA_PUP, SCOPA_THE, SCOPA_CDV, SCOPA_URI, SCOPA_DIG)

length(unique(ams_scopa_bx_2015_16$NUM))
length(unique(SCOPA$NUM))


SCOPA <- ams_scopa_bx_2015_16 %>% inner_join(SCOPA)

SCOPA <- SCOPA %>% filter(SCOPA_TOT!=0)

length(unique(SCOPA$NUM))


SCOPA_Baseline_Pats <- SCOPA %>% filter(Year==0) %>% filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM) %>% distinct() %>%
  inner_join(AllMSA_Pop_Baseline_671)


SCOPA <- AllMSA_Pop_Baseline_671 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
  ) %>%
  mutate(Diff= (SCOPA_TOT-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats)  %>% 
  select(NUM, Diff) %>% rename("SCOPA"="Diff")


# ---------

# COMPASS -----------------


dataCohorteManaged <- readRDS("Source/dataCohorteManaged.rds")

dataCohorteManaged <- dataCohorteManaged %>% group_by(NUM) %>% mutate(TIME_STUDY = ifelse( is.na(TIME_STUDY), 0, TIME_STUDY)) %>%
  mutate(Year= ifelse(TIME_STUDY==0, 0,
                      ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                             ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                    ifelse(TIME_STUDY>=2.5 ,3, NA))))) 


COMPASS <- dataCohorteManaged %>% 
  select(NUM, DATECONSULT, TIME_STUDY, Year, COMPASS1:COMPASS_TOT)

COMPASS$COMPASS_TOT_v2 <- rowSums(COMPASS[, 5:42], na.rm = TRUE)

COMPASS$all_na <- rowSums(is.na(COMPASS[, 5:42])) == (42 - 5 + 1)

COMPASS$number_na <- rowSums(is.na(COMPASS[, 5:42])) 

COMPASS <- COMPASS %>% filter(!(all_na) )

replace_9_with_0 <- function(x) {
  gsub("9", "0", x)
}

names(COMPASS)

COMPASS <- mutate_at(COMPASS, vars(COMPASS1:COMPASS31), ~replace_9_with_0(.))
COMPASS <- mutate_at(COMPASS, vars(COMPASS1:COMPASS31), ~coalesce(., "0"))
COMPASS <- mutate_at(COMPASS, vars(COMPASS1:COMPASS31), as.numeric)

COMPASS$COMPASS_TOT_v2_exc9 <- rowSums(COMPASS[, 5:42], na.rm = TRUE)

COMPASS %>% group_by(Year) %>% summarise(mean=mean(COMPASS_TOT_v2_exc9, na.rm=T),sd=sd(COMPASS_TOT_v2_exc9, na.rm=T), median=median(COMPASS_TOT_v2_exc9, na.rm=T))

names(COMPASS)

COMPASS$Orthostatic <- COMPASS$COMPASS1 + COMPASS$COMPASS2 +
  COMPASS$COMPASS3 + COMPASS$COMPASS4 

COMPASS$Vasomotor <- COMPASS$COMPASS5 + COMPASS$COMPASS6_1 +
  COMPASS$COMPASS6_2 + COMPASS$COMPASS7 

COMPASS$Secretomotor <- COMPASS$COMPASS8 + COMPASS$COMPASS9 +
  COMPASS$COMPASS10 + COMPASS$COMPASS11 

COMPASS$Gastrointestinal <- COMPASS$COMPASS12 + COMPASS$COMPASS13 +
  COMPASS$COMPASS14 + COMPASS$COMPASS15  + COMPASS$COMPASS16 +
  COMPASS$COMPASS17_1  + COMPASS$COMPASS17_2  + COMPASS$COMPASS18 +
  COMPASS$COMPASS19 + COMPASS$COMPASS20 + COMPASS$COMPASS21_1 +
  COMPASS$COMPASS21_2 + COMPASS$COMPASS22 + COMPASS$COMPASS23


COMPASS$Bladder <- COMPASS$COMPASS24_1 + COMPASS$COMPASS24_2 +
  COMPASS$COMPASS25_1 + COMPASS$COMPASS25_2  + COMPASS$COMPASS26_1 +
  COMPASS$COMPASS26_2  

COMPASS$Pupillomotor <-  COMPASS$COMPASS27_1  + COMPASS$COMPASS27_2 +
  COMPASS$COMPASS28  + COMPASS$COMPASS29 + COMPASS$COMPASS30 + COMPASS$COMPASS31


COMPASS$DomainTotals <-  COMPASS$Orthostatic + COMPASS$Vasomotor +  COMPASS$Secretomotor + COMPASS$Gastrointestinal +
  COMPASS$Bladder + COMPASS$Pupillomotor


COMPASS <- COMPASS %>% select(NUM, DATECONSULT, TIME_STUDY, Year, Orthostatic, Vasomotor, Secretomotor, Gastrointestinal, Bladder, Pupillomotor, DomainTotals)

range(COMPASS$DomainTotals)

COMPASS <- COMPASS[COMPASS$DomainTotals>0,]

COMPASS_Baseline_Pats <- COMPASS %>% filter(Year==0) %>% filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM) %>% distinct() %>%
  inner_join(AllMSA_Pop_Baseline_671)



COMPASS <- AllMSA_Pop_Baseline_671 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  DomainTotals) %>%
  rename("Baseline"="DomainTotals") %>%
  inner_join(
    AllMSA_Pop_Baseline_671 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(DomainTotals)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, DomainTotals)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  mutate(Diff= (DomainTotals-Baseline)/TIME_STUDY) %>% 
  select(NUM, Diff) %>% rename("COMPASS"="Diff")

# ------------

# Correlations SCHRAG Standardized Deltas Year 1 Overall and Early CT -----------

# Overall

Baselines <- Left_Putamen_VOL %>% inner_join(Right_Putamen_VOL) %>%
  inner_join(Cerebellum_VOL) %>% inner_join(Midbrain_VOL) %>%
  inner_join(Pons_VOL) %>% inner_join(Medulla_VOL) %>%
  inner_join(Total_VOL) %>% 
  inner_join(SCHRAG) %>%  
  select(-NUM) 

Correlations <- Baselines %>% cor(method = "spearman")

round(Correlations, 3)

get_p_value <- function(x, y) {
  cor_test <- cor.test(x, y, method = "spearman")
  broom::tidy(cor_test)$p.value
}

p_value_matrix <- matrix(NA, ncol = 8, nrow = 8, 
                         dimnames = list(names(Baselines), names(Baselines)))


for (i in 1:ncol(Baselines)) {
  for (j in 1:ncol(Baselines)) {
    p_value_matrix[i, j] <- get_p_value(Baselines[[i]], Baselines[[j]])
  }
}

Correlations <- round(Correlations, 3)
p_value_matrix <- round(p_value_matrix, 3)


combined_matrix <- matrix(NA, ncol = 8, nrow = 8, 
                          dimnames = list(names(Baselines), names(Baselines)))


for (i in 1:ncol(Baselines)) {
  for (j in 1:ncol(Baselines)) {
    combined_matrix[i, j] <- paste0(Correlations[i, j], ", ", p_value_matrix[i, j])
  }
}


# Target

Baselines <- Left_Putamen_VOL %>% inner_join(Right_Putamen_VOL) %>%
  inner_join(Cerebellum_VOL) %>% inner_join(Midbrain_VOL) %>%
  inner_join(Pons_VOL) %>% inner_join(Medulla_VOL) %>%
  inner_join(Total_VOL) %>% inner_join(EarlyCT_Pop_Baseline_319 %>% select(NUM)) %>%
  inner_join(SCHRAG) %>% select(-NUM) 

Correlations <- Baselines %>% cor(method = "spearman")

round(Correlations, 3)

get_p_value <- function(x, y) {
  cor_test <- cor.test(x, y, method = "spearman")
  broom::tidy(cor_test)$p.value
}

p_value_matrix <- matrix(NA, ncol = 8, nrow = 8, 
                         dimnames = list(names(Baselines), names(Baselines)))


for (i in 1:ncol(Baselines)) {
  for (j in 1:ncol(Baselines)) {
    p_value_matrix[i, j] <- get_p_value(Baselines[[i]], Baselines[[j]])
  }
}

Correlations <- round(Correlations, 3)
p_value_matrix <- round(p_value_matrix, 3)


combined_matrix <- matrix(NA, ncol = 8, nrow = 8, 
                          dimnames = list(names(Baselines), names(Baselines)))


for (i in 1:ncol(Baselines)) {
  for (j in 1:ncol(Baselines)) {
    combined_matrix[i, j] <- paste0(Correlations[i, j], ", ", p_value_matrix[i, j])
  }
}


# -----
# Correlations SCOPA Standardized Deltas Year 1 Overall and Early CT -----------

# Overall

Baselines <- Left_Putamen_VOL %>% inner_join(Right_Putamen_VOL) %>%
  inner_join(Cerebellum_VOL) %>% inner_join(Midbrain_VOL) %>%
  inner_join(Pons_VOL) %>% inner_join(Medulla_VOL) %>%
  inner_join(Total_VOL) %>% 
  inner_join(SCOPA) %>%  
  select(-NUM) 

Correlations <- Baselines %>% cor(method = "spearman")

round(Correlations, 3)

get_p_value <- function(x, y) {
  cor_test <- cor.test(x, y, method = "spearman")
  broom::tidy(cor_test)$p.value
}

p_value_matrix <- matrix(NA, ncol = 8, nrow = 8, 
                         dimnames = list(names(Baselines), names(Baselines)))


for (i in 1:ncol(Baselines)) {
  for (j in 1:ncol(Baselines)) {
    p_value_matrix[i, j] <- get_p_value(Baselines[[i]], Baselines[[j]])
  }
}

Correlations <- round(Correlations, 3)
p_value_matrix <- round(p_value_matrix, 3)


combined_matrix <- matrix(NA, ncol = 8, nrow = 8, 
                          dimnames = list(names(Baselines), names(Baselines)))


for (i in 1:ncol(Baselines)) {
  for (j in 1:ncol(Baselines)) {
    combined_matrix[i, j] <- paste0(Correlations[i, j], ", ", p_value_matrix[i, j])
  }
}


# Target

Baselines <- Left_Putamen_VOL %>% inner_join(Right_Putamen_VOL) %>%
  inner_join(Cerebellum_VOL) %>% inner_join(Midbrain_VOL) %>%
  inner_join(Pons_VOL) %>% inner_join(Medulla_VOL) %>%
  inner_join(Total_VOL) %>% inner_join(EarlyCT_Pop_Baseline_319 %>% select(NUM)) %>%
  inner_join(SCOPA) %>% select(-NUM) 

Correlations <- Baselines %>% cor(method = "spearman")

round(Correlations, 3)

get_p_value <- function(x, y) {
  cor_test <- cor.test(x, y, method = "spearman")
  broom::tidy(cor_test)$p.value
}

p_value_matrix <- matrix(NA, ncol = 8, nrow = 8, 
                         dimnames = list(names(Baselines), names(Baselines)))


for (i in 1:ncol(Baselines)) {
  for (j in 1:ncol(Baselines)) {
    p_value_matrix[i, j] <- get_p_value(Baselines[[i]], Baselines[[j]])
  }
}

Correlations <- round(Correlations, 3)
p_value_matrix <- round(p_value_matrix, 3)


combined_matrix <- matrix(NA, ncol = 8, nrow = 8, 
                          dimnames = list(names(Baselines), names(Baselines)))


for (i in 1:ncol(Baselines)) {
  for (j in 1:ncol(Baselines)) {
    combined_matrix[i, j] <- paste0(Correlations[i, j], ", ", p_value_matrix[i, j])
  }
}


# -----
# Correlations COMPASS Standardized Deltas Year 1 Overall and Early CT -----------

# Overall

Baselines <- Left_Putamen_VOL %>% inner_join(Right_Putamen_VOL) %>%
  inner_join(Cerebellum_VOL) %>% inner_join(Midbrain_VOL) %>%
  inner_join(Pons_VOL) %>% inner_join(Medulla_VOL) %>%
  inner_join(Total_VOL) %>% 
  inner_join(COMPASS) %>%  
  select(-NUM) 

Correlations <- Baselines %>% cor(method = "spearman")

round(Correlations, 3)

get_p_value <- function(x, y) {
  cor_test <- cor.test(x, y, method = "spearman")
  broom::tidy(cor_test)$p.value
}

p_value_matrix <- matrix(NA, ncol = 8, nrow = 8, 
                         dimnames = list(names(Baselines), names(Baselines)))


for (i in 1:ncol(Baselines)) {
  for (j in 1:ncol(Baselines)) {
    p_value_matrix[i, j] <- get_p_value(Baselines[[i]], Baselines[[j]])
  }
}

Correlations <- round(Correlations, 3)
p_value_matrix <- round(p_value_matrix, 3)


combined_matrix <- matrix(NA, ncol = 8, nrow = 8, 
                          dimnames = list(names(Baselines), names(Baselines)))


for (i in 1:ncol(Baselines)) {
  for (j in 1:ncol(Baselines)) {
    combined_matrix[i, j] <- paste0(Correlations[i, j], ", ", p_value_matrix[i, j])
  }
}


# Target

Baselines <- Left_Putamen_VOL %>% inner_join(Right_Putamen_VOL) %>%
  inner_join(Cerebellum_VOL) %>% inner_join(Midbrain_VOL) %>%
  inner_join(Pons_VOL) %>% inner_join(Medulla_VOL) %>%
  inner_join(Total_VOL) %>% inner_join(EarlyCT_Pop_Baseline_319 %>% select(NUM)) %>%
  inner_join(COMPASS) %>% select(-NUM) 

Correlations <- Baselines %>% cor(method = "spearman")

round(Correlations, 3)

get_p_value <- function(x, y) {
  cor_test <- cor.test(x, y, method = "spearman")
  broom::tidy(cor_test)$p.value
}

p_value_matrix <- matrix(NA, ncol = 8, nrow = 8, 
                         dimnames = list(names(Baselines), names(Baselines)))


for (i in 1:ncol(Baselines)) {
  for (j in 1:ncol(Baselines)) {
    p_value_matrix[i, j] <- get_p_value(Baselines[[i]], Baselines[[j]])
  }
}

Correlations <- round(Correlations, 3)
p_value_matrix <- round(p_value_matrix, 3)


combined_matrix <- matrix(NA, ncol = 8, nrow = 8, 
                          dimnames = list(names(Baselines), names(Baselines)))


for (i in 1:ncol(Baselines)) {
  for (j in 1:ncol(Baselines)) {
    combined_matrix[i, j] <- paste0(Correlations[i, j], ", ", p_value_matrix[i, j])
  }
}


# -----
# Correlations UMSARS 1 DA-modified exc. #11 collapsed  Standardized Deltas Year 1 Overall and Early CT -----------

# Overall

Baselines <- Left_Putamen_VOL %>% inner_join(Right_Putamen_VOL) %>%
  inner_join(Cerebellum_VOL) %>% inner_join(Midbrain_VOL) %>%
  inner_join(Pons_VOL) %>% inner_join(Medulla_VOL) %>%
  inner_join(Total_VOL) %>% 
  inner_join(UMSARS1) %>%  
  select(-NUM) 

Correlations <- Baselines %>% cor(method = "spearman")

round(Correlations, 3)

get_p_value <- function(x, y) {
  cor_test <- cor.test(x, y, method = "spearman")
  broom::tidy(cor_test)$p.value
}

p_value_matrix <- matrix(NA, ncol = 8, nrow = 8, 
                         dimnames = list(names(Baselines), names(Baselines)))


for (i in 1:ncol(Baselines)) {
  for (j in 1:ncol(Baselines)) {
    p_value_matrix[i, j] <- get_p_value(Baselines[[i]], Baselines[[j]])
  }
}

Correlations <- round(Correlations, 3)
p_value_matrix <- round(p_value_matrix, 3)


combined_matrix <- matrix(NA, ncol = 8, nrow = 8, 
                          dimnames = list(names(Baselines), names(Baselines)))


for (i in 1:ncol(Baselines)) {
  for (j in 1:ncol(Baselines)) {
    combined_matrix[i, j] <- paste0(Correlations[i, j], ", ", p_value_matrix[i, j])
  }
}


# Target

Baselines <- Left_Putamen_VOL %>% inner_join(Right_Putamen_VOL) %>%
  inner_join(Cerebellum_VOL) %>% inner_join(Midbrain_VOL) %>%
  inner_join(Pons_VOL) %>% inner_join(Medulla_VOL) %>%
  inner_join(Total_VOL) %>% inner_join(EarlyCT_Pop_Baseline_319 %>% select(NUM)) %>%
  inner_join(UMSARS1) %>% select(-NUM) 

Correlations <- Baselines %>% cor(method = "spearman")

round(Correlations, 3)

get_p_value <- function(x, y) {
  cor_test <- cor.test(x, y, method = "spearman")
  broom::tidy(cor_test)$p.value
}

p_value_matrix <- matrix(NA, ncol = 8, nrow = 8, 
                         dimnames = list(names(Baselines), names(Baselines)))


for (i in 1:ncol(Baselines)) {
  for (j in 1:ncol(Baselines)) {
    p_value_matrix[i, j] <- get_p_value(Baselines[[i]], Baselines[[j]])
  }
}

Correlations <- round(Correlations, 3)
p_value_matrix <- round(p_value_matrix, 3)


combined_matrix <- matrix(NA, ncol = 8, nrow = 8, 
                          dimnames = list(names(Baselines), names(Baselines)))


for (i in 1:ncol(Baselines)) {
  for (j in 1:ncol(Baselines)) {
    combined_matrix[i, j] <- paste0(Correlations[i, j], ", ", p_value_matrix[i, j])
  }
}


# -----





