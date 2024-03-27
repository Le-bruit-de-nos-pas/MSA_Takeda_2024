
library(tidyverse)
library(data.table)
options(scipen = 999)


# Inputs UMSARS 3 SBP Supine ----------------------------------

AllMSA_Pop_Baseline_671 <- fread("Source/AllMSA_Pop_Baseline_671.txt")
AllMSA_Pop_BaselineYear1_410 <- fread("Source/AllMSA_Pop_BaselineYear1_410.txt")
AllMSA_Pop_BaselineYear1Year2_245 <- fread("Source/AllMSA_Pop_BaselineYear1Year2_245.txt")
AllMSA_Pop_BaselineYear1Year2Year3Plus_158 <- fread("Source/AllMSA_Pop_BaselineYear1Year2Year3Plus_158.txt")

EarlyCT_Pop_Baseline_319 <- fread("Source/EarlyCT_Pop_Baseline_319.txt")
EarlyCT_Pop_BaselineYear1_208 <- fread("Source/EarlyCT_Pop_BaselineYear1_208.txt")
EarlyCT_Pop_BaselineYear1Year2_134 <- fread("Source/EarlyCT_Pop_BaselineYear1Year2_134.txt")
EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 <- fread( "Source/EarlyCT_Pop_BaselineYear1Year2Year3Plus_99.txt")


dataCohorteManaged <- readRDS("Source/dataCohorteManaged.rds")

dataCohorteManaged <- dataCohorteManaged %>% group_by(NUM) %>% mutate(TIME_STUDY = ifelse( is.na(TIME_STUDY), 0, TIME_STUDY)) %>%
  mutate(Year= ifelse(TIME_STUDY==0, 0,
                      ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                             ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                    ifelse(TIME_STUDY>=2.5 ,3, NA))))) 

# SBP <- dataCohorteManaged %>% 
#   select(NUM, DATECONSULT, TIME_STUDY, Year, UMSARS3_ORTHO, PAS_COU, PAS_1MN:PAS_10MN, FC_COU:FC_10MN, deltaPAS)

SBP <- dataCohorteManaged %>%
  select(NUM, DATECONSULT, TIME_STUDY, Year, UMSARS3_ORTHO, PAS_COU, PAS_1MN:PAS_10MN, deltaPAS)

SBP <- SBP %>% filter(!is.na(deltaPAS))
SBP <-  gather(SBP, measure_PAS, PAS_DEB, PAS_1MN:PAS_10MN)
SBP <-  SBP %>% filter(deltaPAS == PAS_DEB-PAS_COU)

# SBP <- gather(SBP, measure_FC, FC_DEB, FC_1MN:FC_10MN)

SBP$measure_PAS <- substr(SBP$measure_PAS, start = 5, stop=nchar(SBP$measure_PAS))
SBP$measure_PAS <- substr(SBP$measure_PAS, start = 1, stop=nchar(SBP$measure_PAS)-2)
unique(SBP$measure_PAS)

# SBP$measure_FC <- substr(SBP$measure_FC, start = 4, stop=nchar(SBP$measure_FC))
# SBP$measure_FC <- substr(SBP$measure_FC, start = 1, stop=nchar(SBP$measure_FC)-2)
# unique(SBP$measure_FC)

# SBP <- SBP %>% filter(measure_PAS==measure_FC)

# SBP <- SBP %>% select(-c(measure_PAS, measure_FC)) %>% distinct()
SBP <- SBP %>% select(-c(measure_PAS)) %>% distinct()
SBP <-  SBP %>% ungroup()
mean(SBP$PAS_COU); mean(SBP$PAS_DEB); mean(SBP$deltaPAS); 
#mean(SBP$FC_COU); mean(SBP$FC_DEB); 
#sum(is.na(SBP$FC_COU))
#sum(is.na(SBP$FC_DEB))

# All we need for 
#orthos 
#PAS COU 
#PAS DEB 
#delta PAS
#FC DEB 
#FC COU

old <- options(pillar.sigfig=2)
options(digits = 8)
PAS_COU_Baseline_Pats <- SBP %>% filter(Year==0) %>% filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM) %>% distinct()

# ----------------
# Overall MSA Entire -------------------------------

# ALL UMSARS Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(SBP) %>% filter(Year==0) %>% 
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(PAS_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(PAS_COU),
    sd=sd(PAS_COU),
    median=median(PAS_COU),
    Q1 = quantile(PAS_COU, 0.25),
    Q3 = quantile(PAS_COU, 0.75),
    min=min(PAS_COU),
    max=max(PAS_COU),
    n=n()
  )



# ALL UMSARS Total Year 1

AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==1) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(PAS_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(PAS_COU),
    sd=sd(PAS_COU),
    median=median(PAS_COU),
    Q1 = quantile(PAS_COU, 0.25),
    Q3 = quantile(PAS_COU, 0.75),
    min=min(PAS_COU),
    max=max(PAS_COU),
    n=n()
  )

# Change from baseline

AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_COU) %>%
  rename("Baseline"="PAS_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(PAS_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_COU)
  ) %>%  inner_join(PAS_COU_Baseline_Pats) %>%
  summarise(mean= mean((PAS_COU-Baseline)/TIME_STUDY)) 



AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_COU) %>%
  rename("Baseline"="PAS_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(PAS_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_COU)
  ) %>%  inner_join(PAS_COU_Baseline_Pats) %>%
  mutate(Diff= (PAS_COU-Baseline)) %>% 
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )




AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_COU) %>%
  rename("Baseline"="PAS_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(PAS_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_COU)
  ) %>%  inner_join(PAS_COU_Baseline_Pats) %>%
  mutate(Diff= TIME_STUDY*12) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_COU) %>%
  rename("Baseline"="PAS_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(PAS_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_COU)
  ) %>%  inner_join(PAS_COU_Baseline_Pats) %>%
  mutate(Diff= (PAS_COU-Baseline)/TIME_STUDY) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



# ALL UMSARS Total Year 2

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==2) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(PAS_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(PAS_COU),
    sd=sd(PAS_COU),
    median=median(PAS_COU),
    Q1 = quantile(PAS_COU, 0.25),
    Q3 = quantile(PAS_COU, 0.75),
    min=min(PAS_COU),
    max=max(PAS_COU),
    n=n()
  )


# Change from baseline

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_COU) %>%
  rename("Baseline"="PAS_COU") %>% 
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(PAS_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_COU)
  ) %>%  inner_join(PAS_COU_Baseline_Pats) %>%
  summarise(mean= mean((PAS_COU-Baseline)/TIME_STUDY)) 



AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_COU) %>%
  rename("Baseline"="PAS_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(PAS_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_COU)
  ) %>%  inner_join(PAS_COU_Baseline_Pats) %>%
  mutate(Diff= (PAS_COU-Baseline)) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_COU) %>%
  rename("Baseline"="PAS_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(PAS_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_COU)
  ) %>%  inner_join(PAS_COU_Baseline_Pats) %>%
  mutate(Diff= TIME_STUDY*12) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_COU) %>%
  rename("Baseline"="PAS_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(PAS_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_COU)
  ) %>% 
  mutate(Diff= (PAS_COU-Baseline)/TIME_STUDY) %>% inner_join(PAS_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


# ALL UMSARS Total Year 3

AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==3) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(PAS_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(PAS_COU),
    sd=sd(PAS_COU),
    median=median(PAS_COU),
    Q1 = quantile(PAS_COU, 0.25),
    Q3 = quantile(PAS_COU, 0.75),
    min=min(PAS_COU),
    max=max(PAS_COU),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_COU) %>%
  rename("Baseline"="PAS_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(PAS_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_COU)
  ) %>%  inner_join(PAS_COU_Baseline_Pats) %>%
  summarise(mean= mean((PAS_COU-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_COU) %>%
  rename("Baseline"="PAS_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(PAS_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_COU)
  ) %>%  
  mutate(Diff= (PAS_COU-Baseline)) %>% inner_join(PAS_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_COU) %>%
  rename("Baseline"="PAS_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(PAS_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_COU)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(PAS_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_COU) %>%
  rename("Baseline"="PAS_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(PAS_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_COU)
  ) %>% 
  mutate(Diff= (PAS_COU-Baseline)/TIME_STUDY) %>% inner_join(PAS_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )

# --------------


# By MSA P vs MSA C -----------------
# ALL UMSARS Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(PAS_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(PAS_COU),
    sd=sd(PAS_COU),
    median=median(PAS_COU),
    Q1 = quantile(PAS_COU, 0.25),
    Q3 = quantile(PAS_COU, 0.75),
    min=min(PAS_COU),
    max=max(PAS_COU),
    n=n()
  )



# ALL UMSARS Total Year 1

AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==1) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(PAS_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(PAS_COU),
    sd=sd(PAS_COU),
    median=median(PAS_COU),
    Q1 = quantile(PAS_COU, 0.25),
    Q3 = quantile(PAS_COU, 0.75),
    min=min(PAS_COU),
    max=max(PAS_COU),
    n=n()
  )

# Change from baseline

AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_COU) %>%
  rename("Baseline"="PAS_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(PAS_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_COU)
  ) %>%  inner_join(PAS_COU_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((PAS_COU-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_COU) %>%
  rename("Baseline"="PAS_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(PAS_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_COU)
  ) %>% 
  mutate(Diff= (PAS_COU-Baseline)) %>% inner_join(PAS_COU_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_COU) %>%
  rename("Baseline"="PAS_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(PAS_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_COU)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(PAS_COU_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_COU) %>%
  rename("Baseline"="PAS_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(PAS_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_COU)
  ) %>% 
  mutate(Diff= (PAS_COU-Baseline)/TIME_STUDY) %>% inner_join(PAS_COU_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



# ALL UMSARS Total Year 2

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==2) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(PAS_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(PAS_COU),
    sd=sd(PAS_COU),
    median=median(PAS_COU),
    Q1 = quantile(PAS_COU, 0.25),
    Q3 = quantile(PAS_COU, 0.75),
    min=min(PAS_COU),
    max=max(PAS_COU),
    n=n()
  )


# Change from baseline

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_COU) %>%
  rename("Baseline"="PAS_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(PAS_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_COU)
  ) %>%  inner_join(PAS_COU_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((PAS_COU-Baseline)/TIME_STUDY)) 



AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_COU) %>%
  rename("Baseline"="PAS_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(PAS_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_COU)
  ) %>% 
  mutate(Diff= (PAS_COU-Baseline)) %>% inner_join(PAS_COU_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_COU) %>%
  rename("Baseline"="PAS_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(PAS_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_COU)
  ) %>%  inner_join(PAS_COU_Baseline_Pats) %>%
  mutate(Diff= TIME_STUDY*12) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_COU) %>%
  rename("Baseline"="PAS_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(PAS_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_COU)
  ) %>% 
  mutate(Diff= (PAS_COU-Baseline)/TIME_STUDY) %>% inner_join(PAS_COU_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


# ALL UMSARS Total Year 3

AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==3) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(PAS_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(PAS_COU),
    sd=sd(PAS_COU),
    median=median(PAS_COU),
    Q1 = quantile(PAS_COU, 0.25),
    Q3 = quantile(PAS_COU, 0.75),
    min=min(PAS_COU),
    max=max(PAS_COU),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_COU) %>%
  rename("Baseline"="PAS_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(PAS_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_COU)
  ) %>% group_by(DIAG) %>% inner_join(PAS_COU_Baseline_Pats) %>%
  summarise(mean= mean((PAS_COU-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_COU) %>%
  rename("Baseline"="PAS_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(PAS_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_COU)
  ) %>%  inner_join(PAS_COU_Baseline_Pats) %>%
  mutate(Diff= (PAS_COU-Baseline)) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_COU) %>%
  rename("Baseline"="PAS_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(PAS_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_COU)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(PAS_COU_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_COU) %>%
  rename("Baseline"="PAS_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(PAS_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_COU)
  ) %>% 
  mutate(Diff= (PAS_COU-Baseline)/TIME_STUDY) %>% inner_join(PAS_COU_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



# ------------
# By Possible vs probable --------------------

# ALL UMSARS Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(PAS_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(PAS_COU),
    sd=sd(PAS_COU),
    median=median(PAS_COU),
    Q1 = quantile(PAS_COU, 0.25),
    Q3 = quantile(PAS_COU, 0.75),
    min=min(PAS_COU),
    max=max(PAS_COU),
    n=n()
  )



# ALL UMSARS Total Year 1

AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==1) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(PAS_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(PAS_COU),
    sd=sd(PAS_COU),
    median=median(PAS_COU),
    Q1 = quantile(PAS_COU, 0.25),
    Q3 = quantile(PAS_COU, 0.75),
    min=min(PAS_COU),
    max=max(PAS_COU),
    n=n()
  )

# Change from baseline


AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_COU) %>%
  rename("Baseline"="PAS_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(PAS_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_COU)
  ) %>%  inner_join(PAS_COU_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((PAS_COU-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_COU) %>%
  rename("Baseline"="PAS_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(PAS_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_COU)
  ) %>% 
  mutate(Diff= (PAS_COU-Baseline)) %>% inner_join(PAS_COU_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_COU) %>%
  rename("Baseline"="PAS_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(PAS_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_COU)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(PAS_COU_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_COU) %>%
  rename("Baseline"="PAS_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(PAS_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_COU)
  ) %>% 
  mutate(Diff= (PAS_COU-Baseline)/TIME_STUDY) %>% inner_join(PAS_COU_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



# ALL UMSARS Total Year 2

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==2) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(PAS_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(PAS_COU),
    sd=sd(PAS_COU),
    median=median(PAS_COU),
    Q1 = quantile(PAS_COU, 0.25),
    Q3 = quantile(PAS_COU, 0.75),
    min=min(PAS_COU),
    max=max(PAS_COU),
    n=n()
  )


# Change from baseline

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_COU) %>%
  rename("Baseline"="PAS_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(PAS_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_COU)
  ) %>%  inner_join(PAS_COU_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((PAS_COU-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_COU) %>%
  rename("Baseline"="PAS_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(PAS_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_COU)
  ) %>% 
  mutate(Diff= (PAS_COU-Baseline)) %>% inner_join(PAS_COU_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_COU) %>%
  rename("Baseline"="PAS_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(PAS_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_COU)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(PAS_COU_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_COU) %>%
  rename("Baseline"="PAS_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(PAS_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_COU)
  ) %>% 
  mutate(Diff= (PAS_COU-Baseline)/TIME_STUDY) %>% inner_join(PAS_COU_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


# ALL UMSARS Total Year 3

AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==3) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(PAS_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(PAS_COU),
    sd=sd(PAS_COU),
    median=median(PAS_COU),
    Q1 = quantile(PAS_COU, 0.25),
    Q3 = quantile(PAS_COU, 0.75),
    min=min(PAS_COU),
    max=max(PAS_COU),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_COU) %>%
  rename("Baseline"="PAS_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(PAS_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_COU)
  ) %>% group_by(DIAGNIV) %>% inner_join(PAS_COU_Baseline_Pats) %>%
  summarise(mean= mean((PAS_COU-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_COU) %>%
  rename("Baseline"="PAS_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(PAS_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_COU)
  ) %>% 
  mutate(Diff= (PAS_COU-Baseline)) %>% inner_join(PAS_COU_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_COU) %>%
  rename("Baseline"="PAS_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(PAS_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_COU)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(PAS_COU_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_COU) %>%
  rename("Baseline"="PAS_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(PAS_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_COU)
  ) %>% 
  mutate(Diff= (PAS_COU-Baseline)/TIME_STUDY) %>% inner_join(PAS_COU_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )

# -----------------------------
# Overall Early CT  -------------------------------

# ALL UMSARS Total Year 0 


EarlyCT_Pop_Baseline_319 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(PAS_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(PAS_COU),
    sd=sd(PAS_COU),
    median=median(PAS_COU),
    Q1 = quantile(PAS_COU, 0.25),
    Q3 = quantile(PAS_COU, 0.75),
    min=min(PAS_COU),
    max=max(PAS_COU),
    n=n()
  )



# ALL UMSARS Total Year 1

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==1) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(PAS_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(PAS_COU),
    sd=sd(PAS_COU),
    median=median(PAS_COU),
    Q1 = quantile(PAS_COU, 0.25),
    Q3 = quantile(PAS_COU, 0.75),
    min=min(PAS_COU),
    max=max(PAS_COU),
    n=n()
  )

# Change from baseline

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_COU) %>%
  rename("Baseline"="PAS_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(PAS_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_COU)
  ) %>%  inner_join(PAS_COU_Baseline_Pats) %>%
  summarise(mean= mean((PAS_COU-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_COU) %>%
  rename("Baseline"="PAS_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(PAS_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_COU)
  ) %>% 
  mutate(Diff= (PAS_COU-Baseline)) %>% inner_join(PAS_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_COU) %>%
  rename("Baseline"="PAS_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(PAS_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_COU)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(PAS_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_COU) %>%
  rename("Baseline"="PAS_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(PAS_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_COU)
  ) %>% 
  mutate(Diff= (PAS_COU-Baseline)/TIME_STUDY) %>% inner_join(PAS_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



# ALL UMSARS Total Year 2

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==2) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(PAS_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(PAS_COU),
    sd=sd(PAS_COU),
    median=median(PAS_COU),
    Q1 = quantile(PAS_COU, 0.25),
    Q3 = quantile(PAS_COU, 0.75),
    min=min(PAS_COU),
    max=max(PAS_COU),
    n=n()
  )


# Change from baseline

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_COU) %>%
  rename("Baseline"="PAS_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(PAS_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_COU)
  ) %>%  inner_join(PAS_COU_Baseline_Pats) %>%
  summarise(mean= mean((PAS_COU-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_COU) %>%
  rename("Baseline"="PAS_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(PAS_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_COU)
  ) %>% 
  mutate(Diff= (PAS_COU-Baseline)) %>% inner_join(PAS_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_COU) %>%
  rename("Baseline"="PAS_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(PAS_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_COU)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(PAS_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_COU) %>%
  rename("Baseline"="PAS_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(PAS_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_COU)
  ) %>% 
  mutate(Diff= (PAS_COU-Baseline)/TIME_STUDY) %>% inner_join(PAS_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


# ALL UMSARS Total Year 3

EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==3) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(PAS_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(PAS_COU),
    sd=sd(PAS_COU),
    median=median(PAS_COU),
    Q1 = quantile(PAS_COU, 0.25),
    Q3 = quantile(PAS_COU, 0.75),
    min=min(PAS_COU),
    max=max(PAS_COU),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_COU) %>%
  rename("Baseline"="PAS_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(PAS_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_COU)
  ) %>%  inner_join(PAS_COU_Baseline_Pats) %>%
  summarise(mean= mean((PAS_COU-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_COU) %>%
  rename("Baseline"="PAS_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(PAS_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_COU)
  ) %>% 
  mutate(Diff= (PAS_COU-Baseline)) %>% inner_join(PAS_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_COU) %>%
  rename("Baseline"="PAS_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(PAS_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_COU)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(PAS_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_COU) %>%
  rename("Baseline"="PAS_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(PAS_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_COU)
  ) %>% 
  mutate(Diff= (PAS_COU-Baseline)/TIME_STUDY) %>% inner_join(PAS_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )

# --------------


# By MSA P vs MSA C -----------------
# ALL UMSARS Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(PAS_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(PAS_COU),
    sd=sd(PAS_COU),
    median=median(PAS_COU),
    Q1 = quantile(PAS_COU, 0.25),
    Q3 = quantile(PAS_COU, 0.75),
    min=min(PAS_COU),
    max=max(PAS_COU),
    n=n()
  )



# ALL UMSARS Total Year 1

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==1) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(PAS_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(PAS_COU),
    sd=sd(PAS_COU),
    median=median(PAS_COU),
    Q1 = quantile(PAS_COU, 0.25),
    Q3 = quantile(PAS_COU, 0.75),
    min=min(PAS_COU),
    max=max(PAS_COU),
    n=n()
  )

# Change from baseline

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_COU) %>%
  rename("Baseline"="PAS_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(PAS_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_COU)
  ) %>%  inner_join(PAS_COU_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((PAS_COU-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_COU) %>%
  rename("Baseline"="PAS_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(PAS_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_COU)
  ) %>% 
  mutate(Diff= (PAS_COU-Baseline)) %>% inner_join(PAS_COU_Baseline_Pats) %>%
  group_by(DIAG) %>% 
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_COU) %>%
  rename("Baseline"="PAS_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(PAS_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_COU)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(PAS_COU_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_COU) %>%
  rename("Baseline"="PAS_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(PAS_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_COU)
  ) %>% 
  mutate(Diff= (PAS_COU-Baseline)/TIME_STUDY) %>% inner_join(PAS_COU_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



# ALL UMSARS Total Year 2

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==2) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(PAS_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(PAS_COU),
    sd=sd(PAS_COU),
    median=median(PAS_COU),
    Q1 = quantile(PAS_COU, 0.25),
    Q3 = quantile(PAS_COU, 0.75),
    min=min(PAS_COU),
    max=max(PAS_COU),
    n=n()
  )


# Change from baseline

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_COU) %>%
  rename("Baseline"="PAS_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(PAS_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_COU)
  ) %>% inner_join(PAS_COU_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((PAS_COU-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_COU) %>%
  rename("Baseline"="PAS_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(PAS_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_COU)
  ) %>% 
  mutate(Diff= (PAS_COU-Baseline)) %>% inner_join(PAS_COU_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_COU) %>%
  rename("Baseline"="PAS_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(PAS_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_COU)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(PAS_COU_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_COU) %>%
  rename("Baseline"="PAS_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(PAS_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_COU)
  ) %>% 
  mutate(Diff= (PAS_COU-Baseline)/TIME_STUDY) %>% inner_join(PAS_COU_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


# ALL UMSARS Total Year 3

EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==3) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(PAS_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(PAS_COU),
    sd=sd(PAS_COU),
    median=median(PAS_COU),
    Q1 = quantile(PAS_COU, 0.25),
    Q3 = quantile(PAS_COU, 0.75),
    min=min(PAS_COU),
    max=max(PAS_COU),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_COU) %>%
  rename("Baseline"="PAS_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(PAS_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_COU)
  ) %>% group_by(DIAG) %>% inner_join(PAS_COU_Baseline_Pats) %>%
  summarise(mean= mean((PAS_COU-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_COU) %>%
  rename("Baseline"="PAS_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(PAS_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_COU)
  ) %>% 
  mutate(Diff= (PAS_COU-Baseline)) %>% inner_join(PAS_COU_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_COU) %>%
  rename("Baseline"="PAS_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(PAS_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_COU)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(PAS_COU_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_COU) %>%
  rename("Baseline"="PAS_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(PAS_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_COU)
  ) %>% 
  mutate(Diff= (PAS_COU-Baseline)/TIME_STUDY) %>% inner_join(PAS_COU_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



# ------------
# By Possible vs probable --------------------

# ALL UMSARS Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(PAS_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(PAS_COU),
    sd=sd(PAS_COU),
    median=median(PAS_COU),
    Q1 = quantile(PAS_COU, 0.25),
    Q3 = quantile(PAS_COU, 0.75),
    min=min(PAS_COU),
    max=max(PAS_COU),
    n=n()
  )



# ALL UMSARS Total Year 1

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==1) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(PAS_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(PAS_COU),
    sd=sd(PAS_COU),
    median=median(PAS_COU),
    Q1 = quantile(PAS_COU, 0.25),
    Q3 = quantile(PAS_COU, 0.75),
    min=min(PAS_COU),
    max=max(PAS_COU),
    n=n()
  )

# Change from baseline


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_COU) %>%
  rename("Baseline"="PAS_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(PAS_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_COU)
  ) %>%  inner_join(PAS_COU_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((PAS_COU-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_COU) %>%
  rename("Baseline"="PAS_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(PAS_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_COU)
  ) %>% 
  mutate(Diff= (PAS_COU-Baseline)) %>% inner_join(PAS_COU_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )




EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_COU) %>%
  rename("Baseline"="PAS_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(PAS_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_COU)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(PAS_COU_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )




EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_COU) %>%
  rename("Baseline"="PAS_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(PAS_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_COU)
  ) %>% 
  mutate(Diff= (PAS_COU-Baseline)/TIME_STUDY) %>% inner_join(PAS_COU_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



# ALL UMSARS Total Year 2

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==2) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(PAS_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(PAS_COU),
    sd=sd(PAS_COU),
    median=median(PAS_COU),
    Q1 = quantile(PAS_COU, 0.25),
    Q3 = quantile(PAS_COU, 0.75),
    min=min(PAS_COU),
    max=max(PAS_COU),
    n=n()
  )


# Change from baseline

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_COU) %>%
  rename("Baseline"="PAS_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(PAS_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_COU)
  ) %>%  inner_join(PAS_COU_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((PAS_COU-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_COU) %>%
  rename("Baseline"="PAS_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(PAS_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_COU)
  ) %>% 
  mutate(Diff= (PAS_COU-Baseline)) %>% inner_join(PAS_COU_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_COU) %>%
  rename("Baseline"="PAS_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(PAS_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_COU)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(PAS_COU_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_COU) %>%
  rename("Baseline"="PAS_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(PAS_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_COU)
  ) %>% 
  mutate(Diff= (PAS_COU-Baseline)/TIME_STUDY) %>% inner_join(PAS_COU_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


# ALL UMSARS Total Year 3

EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==3) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(PAS_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(PAS_COU),
    sd=sd(PAS_COU),
    median=median(PAS_COU),
    Q1 = quantile(PAS_COU, 0.25),
    Q3 = quantile(PAS_COU, 0.75),
    min=min(PAS_COU),
    max=max(PAS_COU),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_COU) %>%
  rename("Baseline"="PAS_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(PAS_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_COU)
  ) %>% group_by(DIAGNIV) %>% inner_join(PAS_COU_Baseline_Pats) %>%
  summarise(mean= mean((PAS_COU-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_COU) %>%
  rename("Baseline"="PAS_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(PAS_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_COU)
  ) %>% 
  mutate(Diff= (PAS_COU-Baseline)) %>% inner_join(PAS_COU_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% 
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_COU) %>%
  rename("Baseline"="PAS_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(PAS_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_COU)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(PAS_COU_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_COU) %>%
  rename("Baseline"="PAS_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(PAS_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_COU)
  ) %>% 
  mutate(Diff= (PAS_COU-Baseline)/TIME_STUDY) %>% inner_join(PAS_COU_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )

# -----------------------------
# Inputs UMSARS 3 SBP Standing ----------------------------------

AllMSA_Pop_Baseline_671 <- fread("Source/AllMSA_Pop_Baseline_671.txt")
AllMSA_Pop_BaselineYear1_410 <- fread("Source/AllMSA_Pop_BaselineYear1_410.txt")
AllMSA_Pop_BaselineYear1Year2_245 <- fread("Source/AllMSA_Pop_BaselineYear1Year2_245.txt")
AllMSA_Pop_BaselineYear1Year2Year3Plus_158 <- fread("Source/AllMSA_Pop_BaselineYear1Year2Year3Plus_158.txt")

EarlyCT_Pop_Baseline_319 <- fread("Source/EarlyCT_Pop_Baseline_319.txt")
EarlyCT_Pop_BaselineYear1_208 <- fread("Source/EarlyCT_Pop_BaselineYear1_208.txt")
EarlyCT_Pop_BaselineYear1Year2_134 <- fread("Source/EarlyCT_Pop_BaselineYear1Year2_134.txt")
EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 <- fread( "Source/EarlyCT_Pop_BaselineYear1Year2Year3Plus_99.txt")


dataCohorteManaged <- readRDS("Source/dataCohorteManaged.rds")

dataCohorteManaged <- dataCohorteManaged %>% group_by(NUM) %>% mutate(TIME_STUDY = ifelse( is.na(TIME_STUDY), 0, TIME_STUDY)) %>%
  mutate(Year= ifelse(TIME_STUDY==0, 0,
                      ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                             ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                    ifelse(TIME_STUDY>=2.5 ,3, NA))))) 

# SBP <- dataCohorteManaged %>% 
#   select(NUM, DATECONSULT, TIME_STUDY, Year, UMSARS3_ORTHO, PAS_COU, PAS_1MN:PAS_10MN, FC_COU:FC_10MN, deltaPAS)

SBP <- dataCohorteManaged %>%
  select(NUM, DATECONSULT, TIME_STUDY, Year, UMSARS3_ORTHO, PAS_COU, PAS_1MN:PAS_10MN, deltaPAS)

SBP <- SBP %>% filter(!is.na(deltaPAS))
SBP <-  gather(SBP, measure_PAS, PAS_DEB, PAS_1MN:PAS_10MN)
SBP <-  SBP %>% filter(deltaPAS == PAS_DEB-PAS_COU)

# SBP <- gather(SBP, measure_FC, FC_DEB, FC_1MN:FC_10MN)

SBP$measure_PAS <- substr(SBP$measure_PAS, start = 5, stop=nchar(SBP$measure_PAS))
SBP$measure_PAS <- substr(SBP$measure_PAS, start = 1, stop=nchar(SBP$measure_PAS)-2)
unique(SBP$measure_PAS)

# SBP$measure_FC <- substr(SBP$measure_FC, start = 4, stop=nchar(SBP$measure_FC))
# SBP$measure_FC <- substr(SBP$measure_FC, start = 1, stop=nchar(SBP$measure_FC)-2)
# unique(SBP$measure_FC)

# SBP <- SBP %>% filter(measure_PAS==measure_FC)

# SBP <- SBP %>% select(-c(measure_PAS, measure_FC)) %>% distinct()
SBP <- SBP %>% select(-c(measure_PAS)) %>% distinct()
SBP <-  SBP %>% ungroup()
mean(SBP$PAS_COU); mean(SBP$PAS_DEB); mean(SBP$deltaPAS); 
#mean(SBP$FC_COU); mean(SBP$FC_DEB); 
#sum(is.na(SBP$FC_COU))
#sum(is.na(SBP$FC_DEB))

# All we need for 
#orthos 
#PAS COU 
#PAS DEB 
#delta PAS
#FC DEB 
#FC COU

PAS_DEB_Baseline_Pats <- SBP %>% filter(Year==0) %>% filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM) %>% distinct()

# ----------------
# Overall MSA Entire -------------------------------

# ALL UMSARS Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(SBP) %>% filter(Year==0) %>% 
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(PAS_DEB_Baseline_Pats) %>%
  summarise(
    mean=mean(PAS_DEB),
    sd=sd(PAS_DEB),
    median=median(PAS_DEB),
    Q1 = quantile(PAS_DEB, 0.25),
    Q3 = quantile(PAS_DEB, 0.75),
    min=min(PAS_DEB),
    max=max(PAS_DEB),
    n=n()
  )



# ALL UMSARS Total Year 1

AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==1) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(PAS_DEB_Baseline_Pats) %>%
  summarise(
    mean=mean(PAS_DEB),
    sd=sd(PAS_DEB),
    median=median(PAS_DEB),
    Q1 = quantile(PAS_DEB, 0.25),
    Q3 = quantile(PAS_DEB, 0.75),
    min=min(PAS_DEB),
    max=max(PAS_DEB),
    n=n()
  )

# Change from baseline

AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>%  inner_join(PAS_DEB_Baseline_Pats) %>%
  summarise(mean= mean((PAS_DEB-Baseline)/TIME_STUDY)) 



AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>%  inner_join(PAS_DEB_Baseline_Pats) %>%
  mutate(Diff= (PAS_DEB-Baseline)) %>% 
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )




AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>%  inner_join(PAS_DEB_Baseline_Pats) %>%
  mutate(Diff= TIME_STUDY*12) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>%  inner_join(PAS_DEB_Baseline_Pats) %>%
  mutate(Diff= (PAS_DEB-Baseline)/TIME_STUDY) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



# ALL UMSARS Total Year 2

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==2) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(PAS_DEB_Baseline_Pats) %>%
  summarise(
    mean=mean(PAS_DEB),
    sd=sd(PAS_DEB),
    median=median(PAS_DEB),
    Q1 = quantile(PAS_DEB, 0.25),
    Q3 = quantile(PAS_DEB, 0.75),
    min=min(PAS_DEB),
    max=max(PAS_DEB),
    n=n()
  )


# Change from baseline

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>% 
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>%  inner_join(PAS_DEB_Baseline_Pats) %>%
  summarise(mean= mean((PAS_DEB-Baseline)/TIME_STUDY)) 



AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>%  inner_join(PAS_DEB_Baseline_Pats) %>%
  mutate(Diff= (PAS_DEB-Baseline)) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>%  inner_join(PAS_DEB_Baseline_Pats) %>%
  mutate(Diff= TIME_STUDY*12) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
  mutate(Diff= (PAS_DEB-Baseline)/TIME_STUDY) %>% inner_join(PAS_DEB_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


# ALL UMSARS Total Year 3

AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==3) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(PAS_DEB_Baseline_Pats) %>%
  summarise(
    mean=mean(PAS_DEB),
    sd=sd(PAS_DEB),
    median=median(PAS_DEB),
    Q1 = quantile(PAS_DEB, 0.25),
    Q3 = quantile(PAS_DEB, 0.75),
    min=min(PAS_DEB),
    max=max(PAS_DEB),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>%  inner_join(PAS_DEB_Baseline_Pats) %>%
  summarise(mean= mean((PAS_DEB-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>%  
  mutate(Diff= (PAS_DEB-Baseline)) %>% inner_join(PAS_DEB_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(PAS_DEB_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
  mutate(Diff= (PAS_DEB-Baseline)/TIME_STUDY) %>% inner_join(PAS_DEB_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )

# --------------


# By MSA P vs MSA C -----------------
# ALL UMSARS Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(PAS_DEB_Baseline_Pats) %>%
  summarise(
    mean=mean(PAS_DEB),
    sd=sd(PAS_DEB),
    median=median(PAS_DEB),
    Q1 = quantile(PAS_DEB, 0.25),
    Q3 = quantile(PAS_DEB, 0.75),
    min=min(PAS_DEB),
    max=max(PAS_DEB),
    n=n()
  )



# ALL UMSARS Total Year 1

AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==1) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(PAS_DEB_Baseline_Pats) %>%
  summarise(
    mean=mean(PAS_DEB),
    sd=sd(PAS_DEB),
    median=median(PAS_DEB),
    Q1 = quantile(PAS_DEB, 0.25),
    Q3 = quantile(PAS_DEB, 0.75),
    min=min(PAS_DEB),
    max=max(PAS_DEB),
    n=n()
  )

# Change from baseline

AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>%  inner_join(PAS_DEB_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((PAS_DEB-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
  mutate(Diff= (PAS_DEB-Baseline)) %>% inner_join(PAS_DEB_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(PAS_DEB_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
  mutate(Diff= (PAS_DEB-Baseline)/TIME_STUDY) %>% inner_join(PAS_DEB_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



# ALL UMSARS Total Year 2

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==2) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(PAS_DEB_Baseline_Pats) %>%
  summarise(
    mean=mean(PAS_DEB),
    sd=sd(PAS_DEB),
    median=median(PAS_DEB),
    Q1 = quantile(PAS_DEB, 0.25),
    Q3 = quantile(PAS_DEB, 0.75),
    min=min(PAS_DEB),
    max=max(PAS_DEB),
    n=n()
  )


# Change from baseline

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>%  inner_join(PAS_DEB_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((PAS_DEB-Baseline)/TIME_STUDY)) 



AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
  mutate(Diff= (PAS_DEB-Baseline)) %>% inner_join(PAS_DEB_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>%  inner_join(PAS_DEB_Baseline_Pats) %>%
  mutate(Diff= TIME_STUDY*12) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
  mutate(Diff= (PAS_DEB-Baseline)/TIME_STUDY) %>% inner_join(PAS_DEB_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


# ALL UMSARS Total Year 3

AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==3) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(PAS_DEB_Baseline_Pats) %>%
  summarise(
    mean=mean(PAS_DEB),
    sd=sd(PAS_DEB),
    median=median(PAS_DEB),
    Q1 = quantile(PAS_DEB, 0.25),
    Q3 = quantile(PAS_DEB, 0.75),
    min=min(PAS_DEB),
    max=max(PAS_DEB),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% group_by(DIAG) %>% inner_join(PAS_DEB_Baseline_Pats) %>%
  summarise(mean= mean((PAS_DEB-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>%  inner_join(PAS_DEB_Baseline_Pats) %>%
  mutate(Diff= (PAS_DEB-Baseline)) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(PAS_DEB_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
  mutate(Diff= (PAS_DEB-Baseline)/TIME_STUDY) %>% inner_join(PAS_DEB_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



# ------------
# By Possible vs probable --------------------

# ALL UMSARS Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(PAS_DEB_Baseline_Pats) %>%
  summarise(
    mean=mean(PAS_DEB),
    sd=sd(PAS_DEB),
    median=median(PAS_DEB),
    Q1 = quantile(PAS_DEB, 0.25),
    Q3 = quantile(PAS_DEB, 0.75),
    min=min(PAS_DEB),
    max=max(PAS_DEB),
    n=n()
  )



# ALL UMSARS Total Year 1

AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==1) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(PAS_DEB_Baseline_Pats) %>%
  summarise(
    mean=mean(PAS_DEB),
    sd=sd(PAS_DEB),
    median=median(PAS_DEB),
    Q1 = quantile(PAS_DEB, 0.25),
    Q3 = quantile(PAS_DEB, 0.75),
    min=min(PAS_DEB),
    max=max(PAS_DEB),
    n=n()
  )

# Change from baseline


AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>%  inner_join(PAS_DEB_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((PAS_DEB-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
  mutate(Diff= (PAS_DEB-Baseline)) %>% inner_join(PAS_DEB_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(PAS_DEB_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
  mutate(Diff= (PAS_DEB-Baseline)/TIME_STUDY) %>% inner_join(PAS_DEB_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



# ALL UMSARS Total Year 2

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==2) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(PAS_DEB_Baseline_Pats) %>%
  summarise(
    mean=mean(PAS_DEB),
    sd=sd(PAS_DEB),
    median=median(PAS_DEB),
    Q1 = quantile(PAS_DEB, 0.25),
    Q3 = quantile(PAS_DEB, 0.75),
    min=min(PAS_DEB),
    max=max(PAS_DEB),
    n=n()
  )


# Change from baseline

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>%  inner_join(PAS_DEB_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((PAS_DEB-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
  mutate(Diff= (PAS_DEB-Baseline)) %>% inner_join(PAS_DEB_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(PAS_DEB_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
  mutate(Diff= (PAS_DEB-Baseline)/TIME_STUDY) %>% inner_join(PAS_DEB_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


# ALL UMSARS Total Year 3

AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==3) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(PAS_DEB_Baseline_Pats) %>%
  summarise(
    mean=mean(PAS_DEB),
    sd=sd(PAS_DEB),
    median=median(PAS_DEB),
    Q1 = quantile(PAS_DEB, 0.25),
    Q3 = quantile(PAS_DEB, 0.75),
    min=min(PAS_DEB),
    max=max(PAS_DEB),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% group_by(DIAGNIV) %>% inner_join(PAS_DEB_Baseline_Pats) %>%
  summarise(mean= mean((PAS_DEB-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
  mutate(Diff= (PAS_DEB-Baseline)) %>% inner_join(PAS_DEB_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(PAS_DEB_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
  mutate(Diff= (PAS_DEB-Baseline)/TIME_STUDY) %>% inner_join(PAS_DEB_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )

# -----------------------------
# Overall Early CT  -------------------------------

# ALL UMSARS Total Year 0 


EarlyCT_Pop_Baseline_319 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(PAS_DEB_Baseline_Pats) %>%
  summarise(
    mean=mean(PAS_DEB),
    sd=sd(PAS_DEB),
    median=median(PAS_DEB),
    Q1 = quantile(PAS_DEB, 0.25),
    Q3 = quantile(PAS_DEB, 0.75),
    min=min(PAS_DEB),
    max=max(PAS_DEB),
    n=n()
  )



# ALL UMSARS Total Year 1

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==1) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(PAS_DEB_Baseline_Pats) %>%
  summarise(
    mean=mean(PAS_DEB),
    sd=sd(PAS_DEB),
    median=median(PAS_DEB),
    Q1 = quantile(PAS_DEB, 0.25),
    Q3 = quantile(PAS_DEB, 0.75),
    min=min(PAS_DEB),
    max=max(PAS_DEB),
    n=n()
  )

# Change from baseline

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>%  inner_join(PAS_DEB_Baseline_Pats) %>%
  summarise(mean= mean((PAS_DEB-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
  mutate(Diff= (PAS_DEB-Baseline)) %>% inner_join(PAS_DEB_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(PAS_DEB_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
  mutate(Diff= (PAS_DEB-Baseline)/TIME_STUDY) %>% inner_join(PAS_DEB_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



# ALL UMSARS Total Year 2

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==2) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(PAS_DEB_Baseline_Pats) %>%
  summarise(
    mean=mean(PAS_DEB),
    sd=sd(PAS_DEB),
    median=median(PAS_DEB),
    Q1 = quantile(PAS_DEB, 0.25),
    Q3 = quantile(PAS_DEB, 0.75),
    min=min(PAS_DEB),
    max=max(PAS_DEB),
    n=n()
  )


# Change from baseline

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>%  inner_join(PAS_DEB_Baseline_Pats) %>%
  summarise(mean= mean((PAS_DEB-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
  mutate(Diff= (PAS_DEB-Baseline)) %>% inner_join(PAS_DEB_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(PAS_DEB_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
  mutate(Diff= (PAS_DEB-Baseline)/TIME_STUDY) %>% inner_join(PAS_DEB_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


# ALL UMSARS Total Year 3

EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==3) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(PAS_DEB_Baseline_Pats) %>%
  summarise(
    mean=mean(PAS_DEB),
    sd=sd(PAS_DEB),
    median=median(PAS_DEB),
    Q1 = quantile(PAS_DEB, 0.25),
    Q3 = quantile(PAS_DEB, 0.75),
    min=min(PAS_DEB),
    max=max(PAS_DEB),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>%  inner_join(PAS_DEB_Baseline_Pats) %>%
  summarise(mean= mean((PAS_DEB-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
  mutate(Diff= (PAS_DEB-Baseline)) %>% inner_join(PAS_DEB_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(PAS_DEB_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
  mutate(Diff= (PAS_DEB-Baseline)/TIME_STUDY) %>% inner_join(PAS_DEB_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )

# --------------


# By MSA P vs MSA C -----------------
# ALL UMSARS Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(PAS_DEB_Baseline_Pats) %>%
  summarise(
    mean=mean(PAS_DEB),
    sd=sd(PAS_DEB),
    median=median(PAS_DEB),
    Q1 = quantile(PAS_DEB, 0.25),
    Q3 = quantile(PAS_DEB, 0.75),
    min=min(PAS_DEB),
    max=max(PAS_DEB),
    n=n()
  )



# ALL UMSARS Total Year 1

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==1) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(PAS_DEB_Baseline_Pats) %>%
  summarise(
    mean=mean(PAS_DEB),
    sd=sd(PAS_DEB),
    median=median(PAS_DEB),
    Q1 = quantile(PAS_DEB, 0.25),
    Q3 = quantile(PAS_DEB, 0.75),
    min=min(PAS_DEB),
    max=max(PAS_DEB),
    n=n()
  )

# Change from baseline

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>%  inner_join(PAS_DEB_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((PAS_DEB-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
  mutate(Diff= (PAS_DEB-Baseline)) %>% inner_join(PAS_DEB_Baseline_Pats) %>%
  group_by(DIAG) %>% 
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(PAS_DEB_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
  mutate(Diff= (PAS_DEB-Baseline)/TIME_STUDY) %>% inner_join(PAS_DEB_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



# ALL UMSARS Total Year 2

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==2) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(PAS_DEB_Baseline_Pats) %>%
  summarise(
    mean=mean(PAS_DEB),
    sd=sd(PAS_DEB),
    median=median(PAS_DEB),
    Q1 = quantile(PAS_DEB, 0.25),
    Q3 = quantile(PAS_DEB, 0.75),
    min=min(PAS_DEB),
    max=max(PAS_DEB),
    n=n()
  )


# Change from baseline

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% inner_join(PAS_DEB_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((PAS_DEB-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
  mutate(Diff= (PAS_DEB-Baseline)) %>% inner_join(PAS_DEB_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(PAS_DEB_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
  mutate(Diff= (PAS_DEB-Baseline)/TIME_STUDY) %>% inner_join(PAS_DEB_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


# ALL UMSARS Total Year 3

EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==3) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(PAS_DEB_Baseline_Pats) %>%
  summarise(
    mean=mean(PAS_DEB),
    sd=sd(PAS_DEB),
    median=median(PAS_DEB),
    Q1 = quantile(PAS_DEB, 0.25),
    Q3 = quantile(PAS_DEB, 0.75),
    min=min(PAS_DEB),
    max=max(PAS_DEB),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% group_by(DIAG) %>% inner_join(PAS_DEB_Baseline_Pats) %>%
  summarise(mean= mean((PAS_DEB-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
  mutate(Diff= (PAS_DEB-Baseline)) %>% inner_join(PAS_DEB_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(PAS_DEB_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
  mutate(Diff= (PAS_DEB-Baseline)/TIME_STUDY) %>% inner_join(PAS_DEB_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



# ------------
# By Possible vs probable --------------------

# ALL UMSARS Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(PAS_DEB_Baseline_Pats) %>%
  summarise(
    mean=mean(PAS_DEB),
    sd=sd(PAS_DEB),
    median=median(PAS_DEB),
    Q1 = quantile(PAS_DEB, 0.25),
    Q3 = quantile(PAS_DEB, 0.75),
    min=min(PAS_DEB),
    max=max(PAS_DEB),
    n=n()
  )



# ALL UMSARS Total Year 1

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==1) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(PAS_DEB_Baseline_Pats) %>%
  summarise(
    mean=mean(PAS_DEB),
    sd=sd(PAS_DEB),
    median=median(PAS_DEB),
    Q1 = quantile(PAS_DEB, 0.25),
    Q3 = quantile(PAS_DEB, 0.75),
    min=min(PAS_DEB),
    max=max(PAS_DEB),
    n=n()
  )

# Change from baseline


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>%  inner_join(PAS_DEB_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((PAS_DEB-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
  mutate(Diff= (PAS_DEB-Baseline)) %>% inner_join(PAS_DEB_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )




EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(PAS_DEB_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )




EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
  mutate(Diff= (PAS_DEB-Baseline)/TIME_STUDY) %>% inner_join(PAS_DEB_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



# ALL UMSARS Total Year 2

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==2) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(PAS_DEB_Baseline_Pats) %>%
  summarise(
    mean=mean(PAS_DEB),
    sd=sd(PAS_DEB),
    median=median(PAS_DEB),
    Q1 = quantile(PAS_DEB, 0.25),
    Q3 = quantile(PAS_DEB, 0.75),
    min=min(PAS_DEB),
    max=max(PAS_DEB),
    n=n()
  )


# Change from baseline

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>%  inner_join(PAS_DEB_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((PAS_DEB-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
  mutate(Diff= (PAS_DEB-Baseline)) %>% inner_join(PAS_DEB_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(PAS_DEB_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
  mutate(Diff= (PAS_DEB-Baseline)/TIME_STUDY) %>% inner_join(PAS_DEB_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


# ALL UMSARS Total Year 3

EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==3) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(PAS_DEB_Baseline_Pats) %>%
  summarise(
    mean=mean(PAS_DEB),
    sd=sd(PAS_DEB),
    median=median(PAS_DEB),
    Q1 = quantile(PAS_DEB, 0.25),
    Q3 = quantile(PAS_DEB, 0.75),
    min=min(PAS_DEB),
    max=max(PAS_DEB),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% group_by(DIAGNIV) %>% inner_join(PAS_DEB_Baseline_Pats) %>%
  summarise(mean= mean((PAS_DEB-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
  mutate(Diff= (PAS_DEB-Baseline)) %>% inner_join(PAS_DEB_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% 
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(PAS_DEB_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
  mutate(Diff= (PAS_DEB-Baseline)/TIME_STUDY) %>% inner_join(PAS_DEB_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )

# -----------------------------
# Inputs UMSARS 3 DBP Supine ----------------------------------

AllMSA_Pop_Baseline_671 <- fread("Source/AllMSA_Pop_Baseline_671.txt")
AllMSA_Pop_BaselineYear1_410 <- fread("Source/AllMSA_Pop_BaselineYear1_410.txt")
AllMSA_Pop_BaselineYear1Year2_245 <- fread("Source/AllMSA_Pop_BaselineYear1Year2_245.txt")
AllMSA_Pop_BaselineYear1Year2Year3Plus_158 <- fread("Source/AllMSA_Pop_BaselineYear1Year2Year3Plus_158.txt")

EarlyCT_Pop_Baseline_319 <- fread("Source/EarlyCT_Pop_Baseline_319.txt")
EarlyCT_Pop_BaselineYear1_208 <- fread("Source/EarlyCT_Pop_BaselineYear1_208.txt")
EarlyCT_Pop_BaselineYear1Year2_134 <- fread("Source/EarlyCT_Pop_BaselineYear1Year2_134.txt")
EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 <- fread( "Source/EarlyCT_Pop_BaselineYear1Year2Year3Plus_99.txt")


dataCohorteManaged <- readRDS("Source/dataCohorteManaged.rds")

dataCohorteManaged <- dataCohorteManaged %>% group_by(NUM) %>% mutate(TIME_STUDY = ifelse( is.na(TIME_STUDY), 0, TIME_STUDY)) %>%
  mutate(Year= ifelse(TIME_STUDY==0, 0,
                      ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                             ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                    ifelse(TIME_STUDY>=2.5 ,3, NA))))) 

DBP <- dataCohorteManaged %>%
  select(NUM, DATECONSULT, TIME_STUDY, Year, UMSARS3_ORTHO, PAD_COU, PAD_1MN:PAD_10MN, deltaPAD)

DBP <- DBP %>% filter(!is.na(deltaPAD))
DBP <-  gather(DBP, measure_PAD, PAD_DEB, PAD_1MN:PAD_10MN)
DBP <-  DBP %>% filter(deltaPAD == PAD_DEB-PAD_COU)


DBP$measure_PAD <- substr(DBP$measure_PAD, start = 5, stop=nchar(DBP$measure_PAD))
DBP$measure_PAD <- substr(DBP$measure_PAD, start = 1, stop=nchar(DBP$measure_PAD)-2)
unique(DBP$measure_PAD)

DBP <- DBP %>% select(-c(measure_PAD)) %>% distinct()
DBP <-  DBP %>% ungroup()
mean(DBP$PAD_COU); mean(DBP$PAD_DEB); mean(DBP$deltaPAD); 


#orthos 
#PAD COU 
#PAD DEB 
#delta PAD
#FC COU

PAD_COU_Baseline_Pats <- DBP %>% filter(Year==0) %>% filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM) %>% distinct()

# ----------------
# Overall MSA Entire -------------------------------

# ALL UMSARS Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(DBP) %>% filter(Year==0) %>% 
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(PAD_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(PAD_COU),
    sd=sd(PAD_COU),
    median=median(PAD_COU),
    Q1 = quantile(PAD_COU, 0.25),
    Q3 = quantile(PAD_COU, 0.75),
    min=min(PAD_COU),
    max=max(PAD_COU),
    n=n()
  )



# ALL UMSARS Total Year 1

AllMSA_Pop_BaselineYear1_410 %>% inner_join(DBP) %>% filter(Year==1) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(PAD_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(PAD_COU),
    sd=sd(PAD_COU),
    median=median(PAD_COU),
    Q1 = quantile(PAD_COU, 0.25),
    Q3 = quantile(PAD_COU, 0.75),
    min=min(PAD_COU),
    max=max(PAD_COU),
    n=n()
  )

# Change from baseline

AllMSA_Pop_BaselineYear1_410 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_COU) %>%
  rename("Baseline"="PAD_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(DBP) %>% filter(Year==1) %>%
      filter(!is.na(PAD_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_COU)
  ) %>%  inner_join(PAD_COU_Baseline_Pats) %>%
  summarise(mean= mean((PAD_COU-Baseline)/TIME_STUDY)) 



AllMSA_Pop_BaselineYear1_410 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_COU) %>%
  rename("Baseline"="PAD_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(DBP) %>% filter(Year==1) %>%
      filter(!is.na(PAD_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_COU)
  ) %>%  inner_join(PAD_COU_Baseline_Pats) %>%
  mutate(Diff= (PAD_COU-Baseline)) %>% 
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )




AllMSA_Pop_BaselineYear1_410 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_COU) %>%
  rename("Baseline"="PAD_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(DBP) %>% filter(Year==1) %>%
      filter(!is.na(PAD_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_COU)
  ) %>%  inner_join(PAD_COU_Baseline_Pats) %>%
  mutate(Diff= TIME_STUDY*12) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



AllMSA_Pop_BaselineYear1_410 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_COU) %>%
  rename("Baseline"="PAD_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(DBP) %>% filter(Year==1) %>%
      filter(!is.na(PAD_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_COU)
  ) %>%  inner_join(PAD_COU_Baseline_Pats) %>%
  mutate(Diff= (PAD_COU-Baseline)/TIME_STUDY) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



# ALL UMSARS Total Year 2

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(DBP) %>% filter(Year==2) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(PAD_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(PAD_COU),
    sd=sd(PAD_COU),
    median=median(PAD_COU),
    Q1 = quantile(PAD_COU, 0.25),
    Q3 = quantile(PAD_COU, 0.75),
    min=min(PAD_COU),
    max=max(PAD_COU),
    n=n()
  )


# Change from baseline

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_COU) %>%
  rename("Baseline"="PAD_COU") %>% 
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(DBP) %>% filter(Year==2) %>%
      filter(!is.na(PAD_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_COU)
  ) %>%  inner_join(PAD_COU_Baseline_Pats) %>%
  summarise(mean= mean((PAD_COU-Baseline)/TIME_STUDY)) 



AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_COU) %>%
  rename("Baseline"="PAD_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(DBP) %>% filter(Year==2) %>%
      filter(!is.na(PAD_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_COU)
  ) %>%  inner_join(PAD_COU_Baseline_Pats) %>%
  mutate(Diff= (PAD_COU-Baseline)) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_COU) %>%
  rename("Baseline"="PAD_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(DBP) %>% filter(Year==2) %>%
      filter(!is.na(PAD_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_COU)
  ) %>%  inner_join(PAD_COU_Baseline_Pats) %>%
  mutate(Diff= TIME_STUDY*12) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_COU) %>%
  rename("Baseline"="PAD_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(DBP) %>% filter(Year==2) %>%
      filter(!is.na(PAD_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_COU)
  ) %>% 
  mutate(Diff= (PAD_COU-Baseline)/TIME_STUDY) %>% inner_join(PAD_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


# ALL UMSARS Total Year 3

AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(DBP) %>% filter(Year==3) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(PAD_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(PAD_COU),
    sd=sd(PAD_COU),
    median=median(PAD_COU),
    Q1 = quantile(PAD_COU, 0.25),
    Q3 = quantile(PAD_COU, 0.75),
    min=min(PAD_COU),
    max=max(PAD_COU),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_COU) %>%
  rename("Baseline"="PAD_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(DBP) %>% filter(Year==3) %>%
      filter(!is.na(PAD_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_COU)
  ) %>%  inner_join(PAD_COU_Baseline_Pats) %>%
  summarise(mean= mean((PAD_COU-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_COU) %>%
  rename("Baseline"="PAD_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(DBP) %>% filter(Year==3) %>%
      filter(!is.na(PAD_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_COU)
  ) %>%  
  mutate(Diff= (PAD_COU-Baseline)) %>% inner_join(PAD_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_COU) %>%
  rename("Baseline"="PAD_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(DBP) %>% filter(Year==3) %>%
      filter(!is.na(PAD_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_COU)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(PAD_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_COU) %>%
  rename("Baseline"="PAD_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(DBP) %>% filter(Year==3) %>%
      filter(!is.na(PAD_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_COU)
  ) %>% 
  mutate(Diff= (PAD_COU-Baseline)/TIME_STUDY) %>% inner_join(PAD_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )

# --------------


# By MSA P vs MSA C -----------------
# ALL UMSARS Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(PAD_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(PAD_COU),
    sd=sd(PAD_COU),
    median=median(PAD_COU),
    Q1 = quantile(PAD_COU, 0.25),
    Q3 = quantile(PAD_COU, 0.75),
    min=min(PAD_COU),
    max=max(PAD_COU),
    n=n()
  )



# ALL UMSARS Total Year 1

AllMSA_Pop_BaselineYear1_410 %>% inner_join(DBP) %>% filter(Year==1) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(PAD_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(PAD_COU),
    sd=sd(PAD_COU),
    median=median(PAD_COU),
    Q1 = quantile(PAD_COU, 0.25),
    Q3 = quantile(PAD_COU, 0.75),
    min=min(PAD_COU),
    max=max(PAD_COU),
    n=n()
  )

# Change from baseline

AllMSA_Pop_BaselineYear1_410 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_COU) %>%
  rename("Baseline"="PAD_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(DBP) %>% filter(Year==1) %>%
      filter(!is.na(PAD_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_COU)
  ) %>%  inner_join(PAD_COU_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((PAD_COU-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1_410 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_COU) %>%
  rename("Baseline"="PAD_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(DBP) %>% filter(Year==1) %>%
      filter(!is.na(PAD_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_COU)
  ) %>% 
  mutate(Diff= (PAD_COU-Baseline)) %>% inner_join(PAD_COU_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



AllMSA_Pop_BaselineYear1_410 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_COU) %>%
  rename("Baseline"="PAD_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(DBP) %>% filter(Year==1) %>%
      filter(!is.na(PAD_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_COU)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(PAD_COU_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



AllMSA_Pop_BaselineYear1_410 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_COU) %>%
  rename("Baseline"="PAD_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(DBP) %>% filter(Year==1) %>%
      filter(!is.na(PAD_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_COU)
  ) %>% 
  mutate(Diff= (PAD_COU-Baseline)/TIME_STUDY) %>% inner_join(PAD_COU_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



# ALL UMSARS Total Year 2

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(DBP) %>% filter(Year==2) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(PAD_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(PAD_COU),
    sd=sd(PAD_COU),
    median=median(PAD_COU),
    Q1 = quantile(PAD_COU, 0.25),
    Q3 = quantile(PAD_COU, 0.75),
    min=min(PAD_COU),
    max=max(PAD_COU),
    n=n()
  )


# Change from baseline

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_COU) %>%
  rename("Baseline"="PAD_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(DBP) %>% filter(Year==2) %>%
      filter(!is.na(PAD_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_COU)
  ) %>%  inner_join(PAD_COU_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((PAD_COU-Baseline)/TIME_STUDY)) 



AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_COU) %>%
  rename("Baseline"="PAD_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(DBP) %>% filter(Year==2) %>%
      filter(!is.na(PAD_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_COU)
  ) %>% 
  mutate(Diff= (PAD_COU-Baseline)) %>% inner_join(PAD_COU_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_COU) %>%
  rename("Baseline"="PAD_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(DBP) %>% filter(Year==2) %>%
      filter(!is.na(PAD_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_COU)
  ) %>%  inner_join(PAD_COU_Baseline_Pats) %>%
  mutate(Diff= TIME_STUDY*12) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_COU) %>%
  rename("Baseline"="PAD_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(DBP) %>% filter(Year==2) %>%
      filter(!is.na(PAD_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_COU)
  ) %>% 
  mutate(Diff= (PAD_COU-Baseline)/TIME_STUDY) %>% inner_join(PAD_COU_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


# ALL UMSARS Total Year 3

AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(DBP) %>% filter(Year==3) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(PAD_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(PAD_COU),
    sd=sd(PAD_COU),
    median=median(PAD_COU),
    Q1 = quantile(PAD_COU, 0.25),
    Q3 = quantile(PAD_COU, 0.75),
    min=min(PAD_COU),
    max=max(PAD_COU),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_COU) %>%
  rename("Baseline"="PAD_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(DBP) %>% filter(Year==3) %>%
      filter(!is.na(PAD_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_COU)
  ) %>% group_by(DIAG) %>% inner_join(PAD_COU_Baseline_Pats) %>%
  summarise(mean= mean((PAD_COU-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_COU) %>%
  rename("Baseline"="PAD_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(DBP) %>% filter(Year==3) %>%
      filter(!is.na(PAD_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_COU)
  ) %>%  inner_join(PAD_COU_Baseline_Pats) %>%
  mutate(Diff= (PAD_COU-Baseline)) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_COU) %>%
  rename("Baseline"="PAD_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(DBP) %>% filter(Year==3) %>%
      filter(!is.na(PAD_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_COU)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(PAD_COU_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_COU) %>%
  rename("Baseline"="PAD_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(DBP) %>% filter(Year==3) %>%
      filter(!is.na(PAD_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_COU)
  ) %>% 
  mutate(Diff= (PAD_COU-Baseline)/TIME_STUDY) %>% inner_join(PAD_COU_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



# ------------
# By Possible vs probable --------------------

# ALL UMSARS Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(PAD_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(PAD_COU),
    sd=sd(PAD_COU),
    median=median(PAD_COU),
    Q1 = quantile(PAD_COU, 0.25),
    Q3 = quantile(PAD_COU, 0.75),
    min=min(PAD_COU),
    max=max(PAD_COU),
    n=n()
  )



# ALL UMSARS Total Year 1

AllMSA_Pop_BaselineYear1_410 %>% inner_join(DBP) %>% filter(Year==1) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(PAD_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(PAD_COU),
    sd=sd(PAD_COU),
    median=median(PAD_COU),
    Q1 = quantile(PAD_COU, 0.25),
    Q3 = quantile(PAD_COU, 0.75),
    min=min(PAD_COU),
    max=max(PAD_COU),
    n=n()
  )

# Change from baseline


AllMSA_Pop_BaselineYear1_410 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_COU) %>%
  rename("Baseline"="PAD_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(DBP) %>% filter(Year==1) %>%
      filter(!is.na(PAD_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_COU)
  ) %>%  inner_join(PAD_COU_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((PAD_COU-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1_410 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_COU) %>%
  rename("Baseline"="PAD_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(DBP) %>% filter(Year==1) %>%
      filter(!is.na(PAD_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_COU)
  ) %>% 
  mutate(Diff= (PAD_COU-Baseline)) %>% inner_join(PAD_COU_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1_410 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_COU) %>%
  rename("Baseline"="PAD_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(DBP) %>% filter(Year==1) %>%
      filter(!is.na(PAD_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_COU)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(PAD_COU_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1_410 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_COU) %>%
  rename("Baseline"="PAD_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(DBP) %>% filter(Year==1) %>%
      filter(!is.na(PAD_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_COU)
  ) %>% 
  mutate(Diff= (PAD_COU-Baseline)/TIME_STUDY) %>% inner_join(PAD_COU_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



# ALL UMSARS Total Year 2

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(DBP) %>% filter(Year==2) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(PAD_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(PAD_COU),
    sd=sd(PAD_COU),
    median=median(PAD_COU),
    Q1 = quantile(PAD_COU, 0.25),
    Q3 = quantile(PAD_COU, 0.75),
    min=min(PAD_COU),
    max=max(PAD_COU),
    n=n()
  )


# Change from baseline

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_COU) %>%
  rename("Baseline"="PAD_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(DBP) %>% filter(Year==2) %>%
      filter(!is.na(PAD_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_COU)
  ) %>%  inner_join(PAD_COU_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((PAD_COU-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_COU) %>%
  rename("Baseline"="PAD_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(DBP) %>% filter(Year==2) %>%
      filter(!is.na(PAD_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_COU)
  ) %>% 
  mutate(Diff= (PAD_COU-Baseline)) %>% inner_join(PAD_COU_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_COU) %>%
  rename("Baseline"="PAD_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(DBP) %>% filter(Year==2) %>%
      filter(!is.na(PAD_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_COU)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(PAD_COU_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_COU) %>%
  rename("Baseline"="PAD_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(DBP) %>% filter(Year==2) %>%
      filter(!is.na(PAD_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_COU)
  ) %>% 
  mutate(Diff= (PAD_COU-Baseline)/TIME_STUDY) %>% inner_join(PAD_COU_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


# ALL UMSARS Total Year 3

AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(DBP) %>% filter(Year==3) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(PAD_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(PAD_COU),
    sd=sd(PAD_COU),
    median=median(PAD_COU),
    Q1 = quantile(PAD_COU, 0.25),
    Q3 = quantile(PAD_COU, 0.75),
    min=min(PAD_COU),
    max=max(PAD_COU),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_COU) %>%
  rename("Baseline"="PAD_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(DBP) %>% filter(Year==3) %>%
      filter(!is.na(PAD_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_COU)
  ) %>% group_by(DIAGNIV) %>% inner_join(PAD_COU_Baseline_Pats) %>%
  summarise(mean= mean((PAD_COU-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_COU) %>%
  rename("Baseline"="PAD_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(DBP) %>% filter(Year==3) %>%
      filter(!is.na(PAD_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_COU)
  ) %>% 
  mutate(Diff= (PAD_COU-Baseline)) %>% inner_join(PAD_COU_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_COU) %>%
  rename("Baseline"="PAD_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(DBP) %>% filter(Year==3) %>%
      filter(!is.na(PAD_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_COU)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(PAD_COU_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_COU) %>%
  rename("Baseline"="PAD_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(DBP) %>% filter(Year==3) %>%
      filter(!is.na(PAD_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_COU)
  ) %>% 
  mutate(Diff= (PAD_COU-Baseline)/TIME_STUDY) %>% inner_join(PAD_COU_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )

# -----------------------------
# Overall Early CT  -------------------------------

# ALL UMSARS Total Year 0 


EarlyCT_Pop_Baseline_319 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(PAD_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(PAD_COU),
    sd=sd(PAD_COU),
    median=median(PAD_COU),
    Q1 = quantile(PAD_COU, 0.25),
    Q3 = quantile(PAD_COU, 0.75),
    min=min(PAD_COU),
    max=max(PAD_COU),
    n=n()
  )



# ALL UMSARS Total Year 1

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(DBP) %>% filter(Year==1) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(PAD_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(PAD_COU),
    sd=sd(PAD_COU),
    median=median(PAD_COU),
    Q1 = quantile(PAD_COU, 0.25),
    Q3 = quantile(PAD_COU, 0.75),
    min=min(PAD_COU),
    max=max(PAD_COU),
    n=n()
  )

# Change from baseline

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_COU) %>%
  rename("Baseline"="PAD_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(DBP) %>% filter(Year==1) %>%
      filter(!is.na(PAD_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_COU)
  ) %>%  inner_join(PAD_COU_Baseline_Pats) %>%
  summarise(mean= mean((PAD_COU-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_COU) %>%
  rename("Baseline"="PAD_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(DBP) %>% filter(Year==1) %>%
      filter(!is.na(PAD_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_COU)
  ) %>% 
  mutate(Diff= (PAD_COU-Baseline)) %>% inner_join(PAD_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_COU) %>%
  rename("Baseline"="PAD_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(DBP) %>% filter(Year==1) %>%
      filter(!is.na(PAD_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_COU)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(PAD_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_COU) %>%
  rename("Baseline"="PAD_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(DBP) %>% filter(Year==1) %>%
      filter(!is.na(PAD_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_COU)
  ) %>% 
  mutate(Diff= (PAD_COU-Baseline)/TIME_STUDY) %>% inner_join(PAD_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



# ALL UMSARS Total Year 2

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(DBP) %>% filter(Year==2) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(PAD_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(PAD_COU),
    sd=sd(PAD_COU),
    median=median(PAD_COU),
    Q1 = quantile(PAD_COU, 0.25),
    Q3 = quantile(PAD_COU, 0.75),
    min=min(PAD_COU),
    max=max(PAD_COU),
    n=n()
  )


# Change from baseline

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_COU) %>%
  rename("Baseline"="PAD_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(DBP) %>% filter(Year==2) %>%
      filter(!is.na(PAD_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_COU)
  ) %>%  inner_join(PAD_COU_Baseline_Pats) %>%
  summarise(mean= mean((PAD_COU-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_COU) %>%
  rename("Baseline"="PAD_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(DBP) %>% filter(Year==2) %>%
      filter(!is.na(PAD_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_COU)
  ) %>% 
  mutate(Diff= (PAD_COU-Baseline)) %>% inner_join(PAD_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_COU) %>%
  rename("Baseline"="PAD_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(DBP) %>% filter(Year==2) %>%
      filter(!is.na(PAD_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_COU)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(PAD_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_COU) %>%
  rename("Baseline"="PAD_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(DBP) %>% filter(Year==2) %>%
      filter(!is.na(PAD_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_COU)
  ) %>% 
  mutate(Diff= (PAD_COU-Baseline)/TIME_STUDY) %>% inner_join(PAD_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


# ALL UMSARS Total Year 3

EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(DBP) %>% filter(Year==3) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(PAD_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(PAD_COU),
    sd=sd(PAD_COU),
    median=median(PAD_COU),
    Q1 = quantile(PAD_COU, 0.25),
    Q3 = quantile(PAD_COU, 0.75),
    min=min(PAD_COU),
    max=max(PAD_COU),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_COU) %>%
  rename("Baseline"="PAD_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(DBP) %>% filter(Year==3) %>%
      filter(!is.na(PAD_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_COU)
  ) %>%  inner_join(PAD_COU_Baseline_Pats) %>%
  summarise(mean= mean((PAD_COU-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_COU) %>%
  rename("Baseline"="PAD_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(DBP) %>% filter(Year==3) %>%
      filter(!is.na(PAD_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_COU)
  ) %>% 
  mutate(Diff= (PAD_COU-Baseline)) %>% inner_join(PAD_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_COU) %>%
  rename("Baseline"="PAD_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(DBP) %>% filter(Year==3) %>%
      filter(!is.na(PAD_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_COU)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(PAD_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_COU) %>%
  rename("Baseline"="PAD_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(DBP) %>% filter(Year==3) %>%
      filter(!is.na(PAD_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_COU)
  ) %>% 
  mutate(Diff= (PAD_COU-Baseline)/TIME_STUDY) %>% inner_join(PAD_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )

# --------------


# By MSA P vs MSA C -----------------
# ALL UMSARS Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(PAD_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(PAD_COU),
    sd=sd(PAD_COU),
    median=median(PAD_COU),
    Q1 = quantile(PAD_COU, 0.25),
    Q3 = quantile(PAD_COU, 0.75),
    min=min(PAD_COU),
    max=max(PAD_COU),
    n=n()
  )



# ALL UMSARS Total Year 1

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(DBP) %>% filter(Year==1) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(PAD_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(PAD_COU),
    sd=sd(PAD_COU),
    median=median(PAD_COU),
    Q1 = quantile(PAD_COU, 0.25),
    Q3 = quantile(PAD_COU, 0.75),
    min=min(PAD_COU),
    max=max(PAD_COU),
    n=n()
  )

# Change from baseline

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_COU) %>%
  rename("Baseline"="PAD_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(DBP) %>% filter(Year==1) %>%
      filter(!is.na(PAD_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_COU)
  ) %>%  inner_join(PAD_COU_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((PAD_COU-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_COU) %>%
  rename("Baseline"="PAD_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(DBP) %>% filter(Year==1) %>%
      filter(!is.na(PAD_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_COU)
  ) %>% 
  mutate(Diff= (PAD_COU-Baseline)) %>% inner_join(PAD_COU_Baseline_Pats) %>%
  group_by(DIAG) %>% 
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_COU) %>%
  rename("Baseline"="PAD_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(DBP) %>% filter(Year==1) %>%
      filter(!is.na(PAD_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_COU)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(PAD_COU_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_COU) %>%
  rename("Baseline"="PAD_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(DBP) %>% filter(Year==1) %>%
      filter(!is.na(PAD_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_COU)
  ) %>% 
  mutate(Diff= (PAD_COU-Baseline)/TIME_STUDY) %>% inner_join(PAD_COU_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



# ALL UMSARS Total Year 2

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(DBP) %>% filter(Year==2) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(PAD_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(PAD_COU),
    sd=sd(PAD_COU),
    median=median(PAD_COU),
    Q1 = quantile(PAD_COU, 0.25),
    Q3 = quantile(PAD_COU, 0.75),
    min=min(PAD_COU),
    max=max(PAD_COU),
    n=n()
  )


# Change from baseline

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_COU) %>%
  rename("Baseline"="PAD_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(DBP) %>% filter(Year==2) %>%
      filter(!is.na(PAD_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_COU)
  ) %>% inner_join(PAD_COU_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((PAD_COU-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_COU) %>%
  rename("Baseline"="PAD_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(DBP) %>% filter(Year==2) %>%
      filter(!is.na(PAD_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_COU)
  ) %>% 
  mutate(Diff= (PAD_COU-Baseline)) %>% inner_join(PAD_COU_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_COU) %>%
  rename("Baseline"="PAD_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(DBP) %>% filter(Year==2) %>%
      filter(!is.na(PAD_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_COU)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(PAD_COU_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_COU) %>%
  rename("Baseline"="PAD_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(DBP) %>% filter(Year==2) %>%
      filter(!is.na(PAD_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_COU)
  ) %>% 
  mutate(Diff= (PAD_COU-Baseline)/TIME_STUDY) %>% inner_join(PAD_COU_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


# ALL UMSARS Total Year 3

EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(DBP) %>% filter(Year==3) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(PAD_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(PAD_COU),
    sd=sd(PAD_COU),
    median=median(PAD_COU),
    Q1 = quantile(PAD_COU, 0.25),
    Q3 = quantile(PAD_COU, 0.75),
    min=min(PAD_COU),
    max=max(PAD_COU),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_COU) %>%
  rename("Baseline"="PAD_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(DBP) %>% filter(Year==3) %>%
      filter(!is.na(PAD_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_COU)
  ) %>% group_by(DIAG) %>% inner_join(PAD_COU_Baseline_Pats) %>%
  summarise(mean= mean((PAD_COU-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_COU) %>%
  rename("Baseline"="PAD_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(DBP) %>% filter(Year==3) %>%
      filter(!is.na(PAD_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_COU)
  ) %>% 
  mutate(Diff= (PAD_COU-Baseline)) %>% inner_join(PAD_COU_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_COU) %>%
  rename("Baseline"="PAD_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(DBP) %>% filter(Year==3) %>%
      filter(!is.na(PAD_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_COU)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(PAD_COU_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_COU) %>%
  rename("Baseline"="PAD_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(DBP) %>% filter(Year==3) %>%
      filter(!is.na(PAD_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_COU)
  ) %>% 
  mutate(Diff= (PAD_COU-Baseline)/TIME_STUDY) %>% inner_join(PAD_COU_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



# ------------
# By Possible vs probable --------------------

# ALL UMSARS Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(PAD_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(PAD_COU),
    sd=sd(PAD_COU),
    median=median(PAD_COU),
    Q1 = quantile(PAD_COU, 0.25),
    Q3 = quantile(PAD_COU, 0.75),
    min=min(PAD_COU),
    max=max(PAD_COU),
    n=n()
  )



# ALL UMSARS Total Year 1

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(DBP) %>% filter(Year==1) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(PAD_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(PAD_COU),
    sd=sd(PAD_COU),
    median=median(PAD_COU),
    Q1 = quantile(PAD_COU, 0.25),
    Q3 = quantile(PAD_COU, 0.75),
    min=min(PAD_COU),
    max=max(PAD_COU),
    n=n()
  )

# Change from baseline


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_COU) %>%
  rename("Baseline"="PAD_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(DBP) %>% filter(Year==1) %>%
      filter(!is.na(PAD_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_COU)
  ) %>%  inner_join(PAD_COU_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((PAD_COU-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_COU) %>%
  rename("Baseline"="PAD_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(DBP) %>% filter(Year==1) %>%
      filter(!is.na(PAD_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_COU)
  ) %>% 
  mutate(Diff= (PAD_COU-Baseline)) %>% inner_join(PAD_COU_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )




EarlyCT_Pop_BaselineYear1_208 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_COU) %>%
  rename("Baseline"="PAD_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(DBP) %>% filter(Year==1) %>%
      filter(!is.na(PAD_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_COU)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(PAD_COU_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )




EarlyCT_Pop_BaselineYear1_208 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_COU) %>%
  rename("Baseline"="PAD_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(DBP) %>% filter(Year==1) %>%
      filter(!is.na(PAD_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_COU)
  ) %>% 
  mutate(Diff= (PAD_COU-Baseline)/TIME_STUDY) %>% inner_join(PAD_COU_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



# ALL UMSARS Total Year 2

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(DBP) %>% filter(Year==2) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(PAD_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(PAD_COU),
    sd=sd(PAD_COU),
    median=median(PAD_COU),
    Q1 = quantile(PAD_COU, 0.25),
    Q3 = quantile(PAD_COU, 0.75),
    min=min(PAD_COU),
    max=max(PAD_COU),
    n=n()
  )


# Change from baseline

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_COU) %>%
  rename("Baseline"="PAD_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(DBP) %>% filter(Year==2) %>%
      filter(!is.na(PAD_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_COU)
  ) %>%  inner_join(PAD_COU_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((PAD_COU-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_COU) %>%
  rename("Baseline"="PAD_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(DBP) %>% filter(Year==2) %>%
      filter(!is.na(PAD_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_COU)
  ) %>% 
  mutate(Diff= (PAD_COU-Baseline)) %>% inner_join(PAD_COU_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_COU) %>%
  rename("Baseline"="PAD_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(DBP) %>% filter(Year==2) %>%
      filter(!is.na(PAD_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_COU)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(PAD_COU_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_COU) %>%
  rename("Baseline"="PAD_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(DBP) %>% filter(Year==2) %>%
      filter(!is.na(PAD_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_COU)
  ) %>% 
  mutate(Diff= (PAD_COU-Baseline)/TIME_STUDY) %>% inner_join(PAD_COU_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


# ALL UMSARS Total Year 3

EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(DBP) %>% filter(Year==3) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(PAD_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(PAD_COU),
    sd=sd(PAD_COU),
    median=median(PAD_COU),
    Q1 = quantile(PAD_COU, 0.25),
    Q3 = quantile(PAD_COU, 0.75),
    min=min(PAD_COU),
    max=max(PAD_COU),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_COU) %>%
  rename("Baseline"="PAD_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(DBP) %>% filter(Year==3) %>%
      filter(!is.na(PAD_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_COU)
  ) %>% group_by(DIAGNIV) %>% inner_join(PAD_COU_Baseline_Pats) %>%
  summarise(mean= mean((PAD_COU-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_COU) %>%
  rename("Baseline"="PAD_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(DBP) %>% filter(Year==3) %>%
      filter(!is.na(PAD_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_COU)
  ) %>% 
  mutate(Diff= (PAD_COU-Baseline)) %>% inner_join(PAD_COU_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% 
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_COU) %>%
  rename("Baseline"="PAD_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(DBP) %>% filter(Year==3) %>%
      filter(!is.na(PAD_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_COU)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(PAD_COU_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_COU) %>%
  rename("Baseline"="PAD_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(DBP) %>% filter(Year==3) %>%
      filter(!is.na(PAD_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_COU)
  ) %>% 
  mutate(Diff= (PAD_COU-Baseline)/TIME_STUDY) %>% inner_join(PAD_COU_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )

# -----------------------------
# Inputs UMSARS 3 DBP Standing ----------------------------------

AllMSA_Pop_Baseline_671 <- fread("Source/AllMSA_Pop_Baseline_671.txt")
AllMSA_Pop_BaselineYear1_410 <- fread("Source/AllMSA_Pop_BaselineYear1_410.txt")
AllMSA_Pop_BaselineYear1Year2_245 <- fread("Source/AllMSA_Pop_BaselineYear1Year2_245.txt")
AllMSA_Pop_BaselineYear1Year2Year3Plus_158 <- fread("Source/AllMSA_Pop_BaselineYear1Year2Year3Plus_158.txt")

EarlyCT_Pop_Baseline_319 <- fread("Source/EarlyCT_Pop_Baseline_319.txt")
EarlyCT_Pop_BaselineYear1_208 <- fread("Source/EarlyCT_Pop_BaselineYear1_208.txt")
EarlyCT_Pop_BaselineYear1Year2_134 <- fread("Source/EarlyCT_Pop_BaselineYear1Year2_134.txt")
EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 <- fread( "Source/EarlyCT_Pop_BaselineYear1Year2Year3Plus_99.txt")


dataCohorteManaged <- readRDS("Source/dataCohorteManaged.rds")

dataCohorteManaged <- dataCohorteManaged %>% group_by(NUM) %>% mutate(TIME_STUDY = ifelse( is.na(TIME_STUDY), 0, TIME_STUDY)) %>%
  mutate(Year= ifelse(TIME_STUDY==0, 0,
                      ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                             ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                    ifelse(TIME_STUDY>=2.5 ,3, NA))))) 

DBP <- dataCohorteManaged %>%
  select(NUM, DATECONSULT, TIME_STUDY, Year, UMSARS3_ORTHO, PAD_COU, PAD_1MN:PAD_10MN, deltaPAD)

DBP <- DBP %>% filter(!is.na(deltaPAD))
DBP <-  gather(DBP, measure_PAD, PAD_DEB, PAD_1MN:PAD_10MN)
DBP <-  DBP %>% filter(deltaPAD == PAD_DEB-PAD_COU)


DBP$measure_PAD <- substr(DBP$measure_PAD, start = 5, stop=nchar(DBP$measure_PAD))
DBP$measure_PAD <- substr(DBP$measure_PAD, start = 1, stop=nchar(DBP$measure_PAD)-2)
unique(DBP$measure_PAD)

DBP <- DBP %>% select(-c(measure_PAD)) %>% distinct()
DBP <-  DBP %>% ungroup()
mean(DBP$PAD_COU); mean(DBP$PAD_DEB); mean(DBP$deltaPAD); 


#orthos 
#PAD COU 
#PAD DEB 
#delta PAD
#FC DEB

PAD_DEB_Baseline_Pats <- DBP %>% filter(Year==0) %>% filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM) %>% distinct()

# ----------------
# Overall MSA Entire -------------------------------

# ALL UMSARS Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(DBP) %>% filter(Year==0) %>% 
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(PAD_DEB_Baseline_Pats) %>%
  summarise(
    mean=mean(PAD_DEB),
    sd=sd(PAD_DEB),
    median=median(PAD_DEB),
    Q1 = quantile(PAD_DEB, 0.25),
    Q3 = quantile(PAD_DEB, 0.75),
    min=min(PAD_DEB),
    max=max(PAD_DEB),
    n=n()
  )



# ALL UMSARS Total Year 1

AllMSA_Pop_BaselineYear1_410 %>% inner_join(DBP) %>% filter(Year==1) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(PAD_DEB_Baseline_Pats) %>%
  summarise(
    mean=mean(PAD_DEB),
    sd=sd(PAD_DEB),
    median=median(PAD_DEB),
    Q1 = quantile(PAD_DEB, 0.25),
    Q3 = quantile(PAD_DEB, 0.75),
    min=min(PAD_DEB),
    max=max(PAD_DEB),
    n=n()
  )

# Change from baseline

AllMSA_Pop_BaselineYear1_410 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_DEB) %>%
  rename("Baseline"="PAD_DEB") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(DBP) %>% filter(Year==1) %>%
      filter(!is.na(PAD_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_DEB)
  ) %>%  inner_join(PAD_DEB_Baseline_Pats) %>%
  summarise(mean= mean((PAD_DEB-Baseline)/TIME_STUDY)) 



AllMSA_Pop_BaselineYear1_410 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_DEB) %>%
  rename("Baseline"="PAD_DEB") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(DBP) %>% filter(Year==1) %>%
      filter(!is.na(PAD_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_DEB)
  ) %>%  inner_join(PAD_DEB_Baseline_Pats) %>%
  mutate(Diff= (PAD_DEB-Baseline)) %>% 
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )




AllMSA_Pop_BaselineYear1_410 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_DEB) %>%
  rename("Baseline"="PAD_DEB") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(DBP) %>% filter(Year==1) %>%
      filter(!is.na(PAD_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_DEB)
  ) %>%  inner_join(PAD_DEB_Baseline_Pats) %>%
  mutate(Diff= TIME_STUDY*12) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



AllMSA_Pop_BaselineYear1_410 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_DEB) %>%
  rename("Baseline"="PAD_DEB") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(DBP) %>% filter(Year==1) %>%
      filter(!is.na(PAD_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_DEB)
  ) %>%  inner_join(PAD_DEB_Baseline_Pats) %>%
  mutate(Diff= (PAD_DEB-Baseline)/TIME_STUDY) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



# ALL UMSARS Total Year 2

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(DBP) %>% filter(Year==2) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(PAD_DEB_Baseline_Pats) %>%
  summarise(
    mean=mean(PAD_DEB),
    sd=sd(PAD_DEB),
    median=median(PAD_DEB),
    Q1 = quantile(PAD_DEB, 0.25),
    Q3 = quantile(PAD_DEB, 0.75),
    min=min(PAD_DEB),
    max=max(PAD_DEB),
    n=n()
  )


# Change from baseline

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_DEB) %>%
  rename("Baseline"="PAD_DEB") %>% 
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(DBP) %>% filter(Year==2) %>%
      filter(!is.na(PAD_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_DEB)
  ) %>%  inner_join(PAD_DEB_Baseline_Pats) %>%
  summarise(mean= mean((PAD_DEB-Baseline)/TIME_STUDY)) 



AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_DEB) %>%
  rename("Baseline"="PAD_DEB") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(DBP) %>% filter(Year==2) %>%
      filter(!is.na(PAD_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_DEB)
  ) %>%  inner_join(PAD_DEB_Baseline_Pats) %>%
  mutate(Diff= (PAD_DEB-Baseline)) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_DEB) %>%
  rename("Baseline"="PAD_DEB") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(DBP) %>% filter(Year==2) %>%
      filter(!is.na(PAD_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_DEB)
  ) %>%  inner_join(PAD_DEB_Baseline_Pats) %>%
  mutate(Diff= TIME_STUDY*12) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_DEB) %>%
  rename("Baseline"="PAD_DEB") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(DBP) %>% filter(Year==2) %>%
      filter(!is.na(PAD_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_DEB)
  ) %>% 
  mutate(Diff= (PAD_DEB-Baseline)/TIME_STUDY) %>% inner_join(PAD_DEB_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


# ALL UMSARS Total Year 3

AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(DBP) %>% filter(Year==3) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(PAD_DEB_Baseline_Pats) %>%
  summarise(
    mean=mean(PAD_DEB),
    sd=sd(PAD_DEB),
    median=median(PAD_DEB),
    Q1 = quantile(PAD_DEB, 0.25),
    Q3 = quantile(PAD_DEB, 0.75),
    min=min(PAD_DEB),
    max=max(PAD_DEB),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_DEB) %>%
  rename("Baseline"="PAD_DEB") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(DBP) %>% filter(Year==3) %>%
      filter(!is.na(PAD_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_DEB)
  ) %>%  inner_join(PAD_DEB_Baseline_Pats) %>%
  summarise(mean= mean((PAD_DEB-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_DEB) %>%
  rename("Baseline"="PAD_DEB") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(DBP) %>% filter(Year==3) %>%
      filter(!is.na(PAD_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_DEB)
  ) %>%  
  mutate(Diff= (PAD_DEB-Baseline)) %>% inner_join(PAD_DEB_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_DEB) %>%
  rename("Baseline"="PAD_DEB") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(DBP) %>% filter(Year==3) %>%
      filter(!is.na(PAD_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_DEB)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(PAD_DEB_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_DEB) %>%
  rename("Baseline"="PAD_DEB") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(DBP) %>% filter(Year==3) %>%
      filter(!is.na(PAD_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_DEB)
  ) %>% 
  mutate(Diff= (PAD_DEB-Baseline)/TIME_STUDY) %>% inner_join(PAD_DEB_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )

# --------------


# By MSA P vs MSA C -----------------
# ALL UMSARS Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(PAD_DEB_Baseline_Pats) %>%
  summarise(
    mean=mean(PAD_DEB),
    sd=sd(PAD_DEB),
    median=median(PAD_DEB),
    Q1 = quantile(PAD_DEB, 0.25),
    Q3 = quantile(PAD_DEB, 0.75),
    min=min(PAD_DEB),
    max=max(PAD_DEB),
    n=n()
  )



# ALL UMSARS Total Year 1

AllMSA_Pop_BaselineYear1_410 %>% inner_join(DBP) %>% filter(Year==1) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(PAD_DEB_Baseline_Pats) %>%
  summarise(
    mean=mean(PAD_DEB),
    sd=sd(PAD_DEB),
    median=median(PAD_DEB),
    Q1 = quantile(PAD_DEB, 0.25),
    Q3 = quantile(PAD_DEB, 0.75),
    min=min(PAD_DEB),
    max=max(PAD_DEB),
    n=n()
  )

# Change from baseline

AllMSA_Pop_BaselineYear1_410 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_DEB) %>%
  rename("Baseline"="PAD_DEB") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(DBP) %>% filter(Year==1) %>%
      filter(!is.na(PAD_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_DEB)
  ) %>%  inner_join(PAD_DEB_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((PAD_DEB-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1_410 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_DEB) %>%
  rename("Baseline"="PAD_DEB") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(DBP) %>% filter(Year==1) %>%
      filter(!is.na(PAD_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_DEB)
  ) %>% 
  mutate(Diff= (PAD_DEB-Baseline)) %>% inner_join(PAD_DEB_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



AllMSA_Pop_BaselineYear1_410 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_DEB) %>%
  rename("Baseline"="PAD_DEB") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(DBP) %>% filter(Year==1) %>%
      filter(!is.na(PAD_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_DEB)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(PAD_DEB_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



AllMSA_Pop_BaselineYear1_410 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_DEB) %>%
  rename("Baseline"="PAD_DEB") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(DBP) %>% filter(Year==1) %>%
      filter(!is.na(PAD_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_DEB)
  ) %>% 
  mutate(Diff= (PAD_DEB-Baseline)/TIME_STUDY) %>% inner_join(PAD_DEB_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



# ALL UMSARS Total Year 2

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(DBP) %>% filter(Year==2) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(PAD_DEB_Baseline_Pats) %>%
  summarise(
    mean=mean(PAD_DEB),
    sd=sd(PAD_DEB),
    median=median(PAD_DEB),
    Q1 = quantile(PAD_DEB, 0.25),
    Q3 = quantile(PAD_DEB, 0.75),
    min=min(PAD_DEB),
    max=max(PAD_DEB),
    n=n()
  )


# Change from baseline

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_DEB) %>%
  rename("Baseline"="PAD_DEB") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(DBP) %>% filter(Year==2) %>%
      filter(!is.na(PAD_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_DEB)
  ) %>%  inner_join(PAD_DEB_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((PAD_DEB-Baseline)/TIME_STUDY)) 



AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_DEB) %>%
  rename("Baseline"="PAD_DEB") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(DBP) %>% filter(Year==2) %>%
      filter(!is.na(PAD_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_DEB)
  ) %>% 
  mutate(Diff= (PAD_DEB-Baseline)) %>% inner_join(PAD_DEB_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_DEB) %>%
  rename("Baseline"="PAD_DEB") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(DBP) %>% filter(Year==2) %>%
      filter(!is.na(PAD_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_DEB)
  ) %>%  inner_join(PAD_DEB_Baseline_Pats) %>%
  mutate(Diff= TIME_STUDY*12) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_DEB) %>%
  rename("Baseline"="PAD_DEB") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(DBP) %>% filter(Year==2) %>%
      filter(!is.na(PAD_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_DEB)
  ) %>% 
  mutate(Diff= (PAD_DEB-Baseline)/TIME_STUDY) %>% inner_join(PAD_DEB_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


# ALL UMSARS Total Year 3

AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(DBP) %>% filter(Year==3) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(PAD_DEB_Baseline_Pats) %>%
  summarise(
    mean=mean(PAD_DEB),
    sd=sd(PAD_DEB),
    median=median(PAD_DEB),
    Q1 = quantile(PAD_DEB, 0.25),
    Q3 = quantile(PAD_DEB, 0.75),
    min=min(PAD_DEB),
    max=max(PAD_DEB),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_DEB) %>%
  rename("Baseline"="PAD_DEB") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(DBP) %>% filter(Year==3) %>%
      filter(!is.na(PAD_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_DEB)
  ) %>% group_by(DIAG) %>% inner_join(PAD_DEB_Baseline_Pats) %>%
  summarise(mean= mean((PAD_DEB-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_DEB) %>%
  rename("Baseline"="PAD_DEB") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(DBP) %>% filter(Year==3) %>%
      filter(!is.na(PAD_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_DEB)
  ) %>%  inner_join(PAD_DEB_Baseline_Pats) %>%
  mutate(Diff= (PAD_DEB-Baseline)) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_DEB) %>%
  rename("Baseline"="PAD_DEB") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(DBP) %>% filter(Year==3) %>%
      filter(!is.na(PAD_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_DEB)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(PAD_DEB_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_DEB) %>%
  rename("Baseline"="PAD_DEB") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(DBP) %>% filter(Year==3) %>%
      filter(!is.na(PAD_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_DEB)
  ) %>% 
  mutate(Diff= (PAD_DEB-Baseline)/TIME_STUDY) %>% inner_join(PAD_DEB_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



# ------------
# By Possible vs probable --------------------

# ALL UMSARS Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(PAD_DEB_Baseline_Pats) %>%
  summarise(
    mean=mean(PAD_DEB),
    sd=sd(PAD_DEB),
    median=median(PAD_DEB),
    Q1 = quantile(PAD_DEB, 0.25),
    Q3 = quantile(PAD_DEB, 0.75),
    min=min(PAD_DEB),
    max=max(PAD_DEB),
    n=n()
  )



# ALL UMSARS Total Year 1

AllMSA_Pop_BaselineYear1_410 %>% inner_join(DBP) %>% filter(Year==1) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(PAD_DEB_Baseline_Pats) %>%
  summarise(
    mean=mean(PAD_DEB),
    sd=sd(PAD_DEB),
    median=median(PAD_DEB),
    Q1 = quantile(PAD_DEB, 0.25),
    Q3 = quantile(PAD_DEB, 0.75),
    min=min(PAD_DEB),
    max=max(PAD_DEB),
    n=n()
  )

# Change from baseline


AllMSA_Pop_BaselineYear1_410 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_DEB) %>%
  rename("Baseline"="PAD_DEB") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(DBP) %>% filter(Year==1) %>%
      filter(!is.na(PAD_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_DEB)
  ) %>%  inner_join(PAD_DEB_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((PAD_DEB-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1_410 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_DEB) %>%
  rename("Baseline"="PAD_DEB") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(DBP) %>% filter(Year==1) %>%
      filter(!is.na(PAD_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_DEB)
  ) %>% 
  mutate(Diff= (PAD_DEB-Baseline)) %>% inner_join(PAD_DEB_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1_410 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_DEB) %>%
  rename("Baseline"="PAD_DEB") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(DBP) %>% filter(Year==1) %>%
      filter(!is.na(PAD_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_DEB)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(PAD_DEB_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1_410 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_DEB) %>%
  rename("Baseline"="PAD_DEB") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(DBP) %>% filter(Year==1) %>%
      filter(!is.na(PAD_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_DEB)
  ) %>% 
  mutate(Diff= (PAD_DEB-Baseline)/TIME_STUDY) %>% inner_join(PAD_DEB_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



# ALL UMSARS Total Year 2

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(DBP) %>% filter(Year==2) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(PAD_DEB_Baseline_Pats) %>%
  summarise(
    mean=mean(PAD_DEB),
    sd=sd(PAD_DEB),
    median=median(PAD_DEB),
    Q1 = quantile(PAD_DEB, 0.25),
    Q3 = quantile(PAD_DEB, 0.75),
    min=min(PAD_DEB),
    max=max(PAD_DEB),
    n=n()
  )


# Change from baseline

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_DEB) %>%
  rename("Baseline"="PAD_DEB") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(DBP) %>% filter(Year==2) %>%
      filter(!is.na(PAD_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_DEB)
  ) %>%  inner_join(PAD_DEB_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((PAD_DEB-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_DEB) %>%
  rename("Baseline"="PAD_DEB") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(DBP) %>% filter(Year==2) %>%
      filter(!is.na(PAD_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_DEB)
  ) %>% 
  mutate(Diff= (PAD_DEB-Baseline)) %>% inner_join(PAD_DEB_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_DEB) %>%
  rename("Baseline"="PAD_DEB") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(DBP) %>% filter(Year==2) %>%
      filter(!is.na(PAD_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_DEB)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(PAD_DEB_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_DEB) %>%
  rename("Baseline"="PAD_DEB") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(DBP) %>% filter(Year==2) %>%
      filter(!is.na(PAD_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_DEB)
  ) %>% 
  mutate(Diff= (PAD_DEB-Baseline)/TIME_STUDY) %>% inner_join(PAD_DEB_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


# ALL UMSARS Total Year 3

AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(DBP) %>% filter(Year==3) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(PAD_DEB_Baseline_Pats) %>%
  summarise(
    mean=mean(PAD_DEB),
    sd=sd(PAD_DEB),
    median=median(PAD_DEB),
    Q1 = quantile(PAD_DEB, 0.25),
    Q3 = quantile(PAD_DEB, 0.75),
    min=min(PAD_DEB),
    max=max(PAD_DEB),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_DEB) %>%
  rename("Baseline"="PAD_DEB") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(DBP) %>% filter(Year==3) %>%
      filter(!is.na(PAD_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_DEB)
  ) %>% group_by(DIAGNIV) %>% inner_join(PAD_DEB_Baseline_Pats) %>%
  summarise(mean= mean((PAD_DEB-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_DEB) %>%
  rename("Baseline"="PAD_DEB") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(DBP) %>% filter(Year==3) %>%
      filter(!is.na(PAD_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_DEB)
  ) %>% 
  mutate(Diff= (PAD_DEB-Baseline)) %>% inner_join(PAD_DEB_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_DEB) %>%
  rename("Baseline"="PAD_DEB") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(DBP) %>% filter(Year==3) %>%
      filter(!is.na(PAD_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_DEB)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(PAD_DEB_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_DEB) %>%
  rename("Baseline"="PAD_DEB") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(DBP) %>% filter(Year==3) %>%
      filter(!is.na(PAD_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_DEB)
  ) %>% 
  mutate(Diff= (PAD_DEB-Baseline)/TIME_STUDY) %>% inner_join(PAD_DEB_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )

# -----------------------------
# Overall Early CT  -------------------------------

# ALL UMSARS Total Year 0 


EarlyCT_Pop_Baseline_319 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(PAD_DEB_Baseline_Pats) %>%
  summarise(
    mean=mean(PAD_DEB),
    sd=sd(PAD_DEB),
    median=median(PAD_DEB),
    Q1 = quantile(PAD_DEB, 0.25),
    Q3 = quantile(PAD_DEB, 0.75),
    min=min(PAD_DEB),
    max=max(PAD_DEB),
    n=n()
  )



# ALL UMSARS Total Year 1

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(DBP) %>% filter(Year==1) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(PAD_DEB_Baseline_Pats) %>%
  summarise(
    mean=mean(PAD_DEB),
    sd=sd(PAD_DEB),
    median=median(PAD_DEB),
    Q1 = quantile(PAD_DEB, 0.25),
    Q3 = quantile(PAD_DEB, 0.75),
    min=min(PAD_DEB),
    max=max(PAD_DEB),
    n=n()
  )

# Change from baseline

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_DEB) %>%
  rename("Baseline"="PAD_DEB") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(DBP) %>% filter(Year==1) %>%
      filter(!is.na(PAD_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_DEB)
  ) %>%  inner_join(PAD_DEB_Baseline_Pats) %>%
  summarise(mean= mean((PAD_DEB-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_DEB) %>%
  rename("Baseline"="PAD_DEB") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(DBP) %>% filter(Year==1) %>%
      filter(!is.na(PAD_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_DEB)
  ) %>% 
  mutate(Diff= (PAD_DEB-Baseline)) %>% inner_join(PAD_DEB_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_DEB) %>%
  rename("Baseline"="PAD_DEB") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(DBP) %>% filter(Year==1) %>%
      filter(!is.na(PAD_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_DEB)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(PAD_DEB_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_DEB) %>%
  rename("Baseline"="PAD_DEB") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(DBP) %>% filter(Year==1) %>%
      filter(!is.na(PAD_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_DEB)
  ) %>% 
  mutate(Diff= (PAD_DEB-Baseline)/TIME_STUDY) %>% inner_join(PAD_DEB_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



# ALL UMSARS Total Year 2

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(DBP) %>% filter(Year==2) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(PAD_DEB_Baseline_Pats) %>%
  summarise(
    mean=mean(PAD_DEB),
    sd=sd(PAD_DEB),
    median=median(PAD_DEB),
    Q1 = quantile(PAD_DEB, 0.25),
    Q3 = quantile(PAD_DEB, 0.75),
    min=min(PAD_DEB),
    max=max(PAD_DEB),
    n=n()
  )


# Change from baseline

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_DEB) %>%
  rename("Baseline"="PAD_DEB") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(DBP) %>% filter(Year==2) %>%
      filter(!is.na(PAD_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_DEB)
  ) %>%  inner_join(PAD_DEB_Baseline_Pats) %>%
  summarise(mean= mean((PAD_DEB-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_DEB) %>%
  rename("Baseline"="PAD_DEB") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(DBP) %>% filter(Year==2) %>%
      filter(!is.na(PAD_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_DEB)
  ) %>% 
  mutate(Diff= (PAD_DEB-Baseline)) %>% inner_join(PAD_DEB_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_DEB) %>%
  rename("Baseline"="PAD_DEB") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(DBP) %>% filter(Year==2) %>%
      filter(!is.na(PAD_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_DEB)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(PAD_DEB_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_DEB) %>%
  rename("Baseline"="PAD_DEB") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(DBP) %>% filter(Year==2) %>%
      filter(!is.na(PAD_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_DEB)
  ) %>% 
  mutate(Diff= (PAD_DEB-Baseline)/TIME_STUDY) %>% inner_join(PAD_DEB_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


# ALL UMSARS Total Year 3

EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(DBP) %>% filter(Year==3) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(PAD_DEB_Baseline_Pats) %>%
  summarise(
    mean=mean(PAD_DEB),
    sd=sd(PAD_DEB),
    median=median(PAD_DEB),
    Q1 = quantile(PAD_DEB, 0.25),
    Q3 = quantile(PAD_DEB, 0.75),
    min=min(PAD_DEB),
    max=max(PAD_DEB),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_DEB) %>%
  rename("Baseline"="PAD_DEB") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(DBP) %>% filter(Year==3) %>%
      filter(!is.na(PAD_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_DEB)
  ) %>%  inner_join(PAD_DEB_Baseline_Pats) %>%
  summarise(mean= mean((PAD_DEB-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_DEB) %>%
  rename("Baseline"="PAD_DEB") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(DBP) %>% filter(Year==3) %>%
      filter(!is.na(PAD_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_DEB)
  ) %>% 
  mutate(Diff= (PAD_DEB-Baseline)) %>% inner_join(PAD_DEB_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_DEB) %>%
  rename("Baseline"="PAD_DEB") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(DBP) %>% filter(Year==3) %>%
      filter(!is.na(PAD_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_DEB)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(PAD_DEB_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_DEB) %>%
  rename("Baseline"="PAD_DEB") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(DBP) %>% filter(Year==3) %>%
      filter(!is.na(PAD_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_DEB)
  ) %>% 
  mutate(Diff= (PAD_DEB-Baseline)/TIME_STUDY) %>% inner_join(PAD_DEB_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )

# --------------


# By MSA P vs MSA C -----------------
# ALL UMSARS Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(PAD_DEB_Baseline_Pats) %>%
  summarise(
    mean=mean(PAD_DEB),
    sd=sd(PAD_DEB),
    median=median(PAD_DEB),
    Q1 = quantile(PAD_DEB, 0.25),
    Q3 = quantile(PAD_DEB, 0.75),
    min=min(PAD_DEB),
    max=max(PAD_DEB),
    n=n()
  )



# ALL UMSARS Total Year 1

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(DBP) %>% filter(Year==1) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(PAD_DEB_Baseline_Pats) %>%
  summarise(
    mean=mean(PAD_DEB),
    sd=sd(PAD_DEB),
    median=median(PAD_DEB),
    Q1 = quantile(PAD_DEB, 0.25),
    Q3 = quantile(PAD_DEB, 0.75),
    min=min(PAD_DEB),
    max=max(PAD_DEB),
    n=n()
  )

# Change from baseline

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_DEB) %>%
  rename("Baseline"="PAD_DEB") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(DBP) %>% filter(Year==1) %>%
      filter(!is.na(PAD_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_DEB)
  ) %>%  inner_join(PAD_DEB_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((PAD_DEB-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_DEB) %>%
  rename("Baseline"="PAD_DEB") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(DBP) %>% filter(Year==1) %>%
      filter(!is.na(PAD_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_DEB)
  ) %>% 
  mutate(Diff= (PAD_DEB-Baseline)) %>% inner_join(PAD_DEB_Baseline_Pats) %>%
  group_by(DIAG) %>% 
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_DEB) %>%
  rename("Baseline"="PAD_DEB") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(DBP) %>% filter(Year==1) %>%
      filter(!is.na(PAD_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_DEB)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(PAD_DEB_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_DEB) %>%
  rename("Baseline"="PAD_DEB") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(DBP) %>% filter(Year==1) %>%
      filter(!is.na(PAD_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_DEB)
  ) %>% 
  mutate(Diff= (PAD_DEB-Baseline)/TIME_STUDY) %>% inner_join(PAD_DEB_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



# ALL UMSARS Total Year 2

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(DBP) %>% filter(Year==2) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(PAD_DEB_Baseline_Pats) %>%
  summarise(
    mean=mean(PAD_DEB),
    sd=sd(PAD_DEB),
    median=median(PAD_DEB),
    Q1 = quantile(PAD_DEB, 0.25),
    Q3 = quantile(PAD_DEB, 0.75),
    min=min(PAD_DEB),
    max=max(PAD_DEB),
    n=n()
  )


# Change from baseline

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_DEB) %>%
  rename("Baseline"="PAD_DEB") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(DBP) %>% filter(Year==2) %>%
      filter(!is.na(PAD_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_DEB)
  ) %>% inner_join(PAD_DEB_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((PAD_DEB-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_DEB) %>%
  rename("Baseline"="PAD_DEB") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(DBP) %>% filter(Year==2) %>%
      filter(!is.na(PAD_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_DEB)
  ) %>% 
  mutate(Diff= (PAD_DEB-Baseline)) %>% inner_join(PAD_DEB_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_DEB) %>%
  rename("Baseline"="PAD_DEB") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(DBP) %>% filter(Year==2) %>%
      filter(!is.na(PAD_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_DEB)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(PAD_DEB_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_DEB) %>%
  rename("Baseline"="PAD_DEB") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(DBP) %>% filter(Year==2) %>%
      filter(!is.na(PAD_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_DEB)
  ) %>% 
  mutate(Diff= (PAD_DEB-Baseline)/TIME_STUDY) %>% inner_join(PAD_DEB_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


# ALL UMSARS Total Year 3

EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(DBP) %>% filter(Year==3) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(PAD_DEB_Baseline_Pats) %>%
  summarise(
    mean=mean(PAD_DEB),
    sd=sd(PAD_DEB),
    median=median(PAD_DEB),
    Q1 = quantile(PAD_DEB, 0.25),
    Q3 = quantile(PAD_DEB, 0.75),
    min=min(PAD_DEB),
    max=max(PAD_DEB),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_DEB) %>%
  rename("Baseline"="PAD_DEB") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(DBP) %>% filter(Year==3) %>%
      filter(!is.na(PAD_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_DEB)
  ) %>% group_by(DIAG) %>% inner_join(PAD_DEB_Baseline_Pats) %>%
  summarise(mean= mean((PAD_DEB-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_DEB) %>%
  rename("Baseline"="PAD_DEB") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(DBP) %>% filter(Year==3) %>%
      filter(!is.na(PAD_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_DEB)
  ) %>% 
  mutate(Diff= (PAD_DEB-Baseline)) %>% inner_join(PAD_DEB_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_DEB) %>%
  rename("Baseline"="PAD_DEB") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(DBP) %>% filter(Year==3) %>%
      filter(!is.na(PAD_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_DEB)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(PAD_DEB_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_DEB) %>%
  rename("Baseline"="PAD_DEB") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(DBP) %>% filter(Year==3) %>%
      filter(!is.na(PAD_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_DEB)
  ) %>% 
  mutate(Diff= (PAD_DEB-Baseline)/TIME_STUDY) %>% inner_join(PAD_DEB_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



# ------------
# By Possible vs probable --------------------

# ALL UMSARS Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(PAD_DEB_Baseline_Pats) %>%
  summarise(
    mean=mean(PAD_DEB),
    sd=sd(PAD_DEB),
    median=median(PAD_DEB),
    Q1 = quantile(PAD_DEB, 0.25),
    Q3 = quantile(PAD_DEB, 0.75),
    min=min(PAD_DEB),
    max=max(PAD_DEB),
    n=n()
  )



# ALL UMSARS Total Year 1

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(DBP) %>% filter(Year==1) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(PAD_DEB_Baseline_Pats) %>%
  summarise(
    mean=mean(PAD_DEB),
    sd=sd(PAD_DEB),
    median=median(PAD_DEB),
    Q1 = quantile(PAD_DEB, 0.25),
    Q3 = quantile(PAD_DEB, 0.75),
    min=min(PAD_DEB),
    max=max(PAD_DEB),
    n=n()
  )

# Change from baseline


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_DEB) %>%
  rename("Baseline"="PAD_DEB") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(DBP) %>% filter(Year==1) %>%
      filter(!is.na(PAD_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_DEB)
  ) %>%  inner_join(PAD_DEB_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((PAD_DEB-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_DEB) %>%
  rename("Baseline"="PAD_DEB") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(DBP) %>% filter(Year==1) %>%
      filter(!is.na(PAD_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_DEB)
  ) %>% 
  mutate(Diff= (PAD_DEB-Baseline)) %>% inner_join(PAD_DEB_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )




EarlyCT_Pop_BaselineYear1_208 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_DEB) %>%
  rename("Baseline"="PAD_DEB") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(DBP) %>% filter(Year==1) %>%
      filter(!is.na(PAD_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_DEB)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(PAD_DEB_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )




EarlyCT_Pop_BaselineYear1_208 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_DEB) %>%
  rename("Baseline"="PAD_DEB") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(DBP) %>% filter(Year==1) %>%
      filter(!is.na(PAD_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_DEB)
  ) %>% 
  mutate(Diff= (PAD_DEB-Baseline)/TIME_STUDY) %>% inner_join(PAD_DEB_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



# ALL UMSARS Total Year 2

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(DBP) %>% filter(Year==2) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(PAD_DEB_Baseline_Pats) %>%
  summarise(
    mean=mean(PAD_DEB),
    sd=sd(PAD_DEB),
    median=median(PAD_DEB),
    Q1 = quantile(PAD_DEB, 0.25),
    Q3 = quantile(PAD_DEB, 0.75),
    min=min(PAD_DEB),
    max=max(PAD_DEB),
    n=n()
  )


# Change from baseline

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_DEB) %>%
  rename("Baseline"="PAD_DEB") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(DBP) %>% filter(Year==2) %>%
      filter(!is.na(PAD_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_DEB)
  ) %>%  inner_join(PAD_DEB_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((PAD_DEB-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_DEB) %>%
  rename("Baseline"="PAD_DEB") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(DBP) %>% filter(Year==2) %>%
      filter(!is.na(PAD_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_DEB)
  ) %>% 
  mutate(Diff= (PAD_DEB-Baseline)) %>% inner_join(PAD_DEB_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_DEB) %>%
  rename("Baseline"="PAD_DEB") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(DBP) %>% filter(Year==2) %>%
      filter(!is.na(PAD_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_DEB)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(PAD_DEB_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_DEB) %>%
  rename("Baseline"="PAD_DEB") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(DBP) %>% filter(Year==2) %>%
      filter(!is.na(PAD_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_DEB)
  ) %>% 
  mutate(Diff= (PAD_DEB-Baseline)/TIME_STUDY) %>% inner_join(PAD_DEB_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


# ALL UMSARS Total Year 3

EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(DBP) %>% filter(Year==3) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(PAD_DEB_Baseline_Pats) %>%
  summarise(
    mean=mean(PAD_DEB),
    sd=sd(PAD_DEB),
    median=median(PAD_DEB),
    Q1 = quantile(PAD_DEB, 0.25),
    Q3 = quantile(PAD_DEB, 0.75),
    min=min(PAD_DEB),
    max=max(PAD_DEB),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_DEB) %>%
  rename("Baseline"="PAD_DEB") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(DBP) %>% filter(Year==3) %>%
      filter(!is.na(PAD_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_DEB)
  ) %>% group_by(DIAGNIV) %>% inner_join(PAD_DEB_Baseline_Pats) %>%
  summarise(mean= mean((PAD_DEB-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_DEB) %>%
  rename("Baseline"="PAD_DEB") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(DBP) %>% filter(Year==3) %>%
      filter(!is.na(PAD_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_DEB)
  ) %>% 
  mutate(Diff= (PAD_DEB-Baseline)) %>% inner_join(PAD_DEB_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% 
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_DEB) %>%
  rename("Baseline"="PAD_DEB") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(DBP) %>% filter(Year==3) %>%
      filter(!is.na(PAD_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_DEB)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(PAD_DEB_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(PAD_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAD_DEB) %>%
  rename("Baseline"="PAD_DEB") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(DBP) %>% filter(Year==3) %>%
      filter(!is.na(PAD_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAD_DEB)
  ) %>% 
  mutate(Diff= (PAD_DEB-Baseline)/TIME_STUDY) %>% inner_join(PAD_DEB_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )

# -----------------------------
# Inputs UMSARS 3 SBP MAX Delta ----------------------------------

AllMSA_Pop_Baseline_671 <- fread("Source/AllMSA_Pop_Baseline_671.txt")
AllMSA_Pop_BaselineYear1_410 <- fread("Source/AllMSA_Pop_BaselineYear1_410.txt")
AllMSA_Pop_BaselineYear1Year2_245 <- fread("Source/AllMSA_Pop_BaselineYear1Year2_245.txt")
AllMSA_Pop_BaselineYear1Year2Year3Plus_158 <- fread("Source/AllMSA_Pop_BaselineYear1Year2Year3Plus_158.txt")

EarlyCT_Pop_Baseline_319 <- fread("Source/EarlyCT_Pop_Baseline_319.txt")
EarlyCT_Pop_BaselineYear1_208 <- fread("Source/EarlyCT_Pop_BaselineYear1_208.txt")
EarlyCT_Pop_BaselineYear1Year2_134 <- fread("Source/EarlyCT_Pop_BaselineYear1Year2_134.txt")
EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 <- fread( "Source/EarlyCT_Pop_BaselineYear1Year2Year3Plus_99.txt")


dataCohorteManaged <- readRDS("Source/dataCohorteManaged.rds")

dataCohorteManaged <- dataCohorteManaged %>% group_by(NUM) %>% mutate(TIME_STUDY = ifelse( is.na(TIME_STUDY), 0, TIME_STUDY)) %>%
  mutate(Year= ifelse(TIME_STUDY==0, 0,
                      ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                             ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                    ifelse(TIME_STUDY>=2.5 ,3, NA))))) 

SBP <- dataCohorteManaged %>%
  select(NUM, DATECONSULT, TIME_STUDY, Year, UMSARS3_ORTHO, PAS_COU, PAS_1MN:PAS_10MN, deltaPAS)

SBP <- SBP %>% filter(!is.na(deltaPAS))
SBP <-  gather(SBP, measure_PAS, PAS_DEB, PAS_1MN:PAS_10MN)
SBP <-  SBP %>% filter(deltaPAS == PAS_DEB-PAS_COU)


SBP$measure_PAS <- substr(SBP$measure_PAS, start = 5, stop=nchar(SBP$measure_PAS))
SBP$measure_PAS <- substr(SBP$measure_PAS, start = 1, stop=nchar(SBP$measure_PAS)-2)
unique(SBP$measure_PAS)

SBP <- SBP %>% select(-c(measure_PAS)) %>% distinct()
SBP <-  SBP %>% ungroup()
mean(SBP$PAS_COU); mean(SBP$PAS_DEB); mean(SBP$deltaPAS); 


PAS_Delta_Baseline_Pats <- SBP %>% filter(Year==0) %>% filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM) %>% distinct()

# ----------------
# Overall MSA Entire -------------------------------

# ALL UMSARS Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(SBP) %>% filter(Year==0) %>% 
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(PAS_Delta_Baseline_Pats) %>%
  summarise(
    mean=mean(deltaPAS),
    sd=sd(deltaPAS),
    median=median(deltaPAS),
    Q1 = quantile(deltaPAS, 0.25),
    Q3 = quantile(deltaPAS, 0.75),
    min=min(deltaPAS),
    max=max(deltaPAS),
    n=n()
  )



# ALL UMSARS Total Year 1

AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==1) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(PAS_Delta_Baseline_Pats) %>%
  summarise(
    mean=mean(deltaPAS),
    sd=sd(deltaPAS),
    median=median(deltaPAS),
    Q1 = quantile(deltaPAS, 0.25),
    Q3 = quantile(deltaPAS, 0.75),
    min=min(deltaPAS),
    max=max(deltaPAS),
    n=n()
  )

# Change from baseline

AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAS) %>%
  rename("Baseline"="deltaPAS") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(deltaPAS)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAS)
  ) %>%  inner_join(PAS_Delta_Baseline_Pats) %>%
  summarise(mean= mean((deltaPAS-Baseline)/TIME_STUDY)) 



AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAS) %>%
  rename("Baseline"="deltaPAS") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(deltaPAS)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAS)
  ) %>%  inner_join(PAS_Delta_Baseline_Pats) %>%
  mutate(Diff= (deltaPAS-Baseline)) %>% 
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )




AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAS) %>%
  rename("Baseline"="deltaPAS") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(deltaPAS)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAS)
  ) %>%  inner_join(PAS_Delta_Baseline_Pats) %>%
  mutate(Diff= TIME_STUDY*12) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAS) %>%
  rename("Baseline"="deltaPAS") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(deltaPAS)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAS)
  ) %>%  inner_join(PAS_Delta_Baseline_Pats) %>%
  mutate(Diff= (deltaPAS-Baseline)/TIME_STUDY) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



# ALL UMSARS Total Year 2

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==2) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(PAS_Delta_Baseline_Pats) %>%
  summarise(
    mean=mean(deltaPAS),
    sd=sd(deltaPAS),
    median=median(deltaPAS),
    Q1 = quantile(deltaPAS, 0.25),
    Q3 = quantile(deltaPAS, 0.75),
    min=min(deltaPAS),
    max=max(deltaPAS),
    n=n()
  )


# Change from baseline

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAS) %>%
  rename("Baseline"="deltaPAS") %>% 
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(deltaPAS)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAS)
  ) %>%  inner_join(PAS_Delta_Baseline_Pats) %>%
  summarise(mean= mean((deltaPAS-Baseline)/TIME_STUDY)) 



AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAS) %>%
  rename("Baseline"="deltaPAS") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(deltaPAS)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAS)
  ) %>%  inner_join(PAS_Delta_Baseline_Pats) %>%
  mutate(Diff= (deltaPAS-Baseline)) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAS) %>%
  rename("Baseline"="deltaPAS") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(deltaPAS)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAS)
  ) %>%  inner_join(PAS_Delta_Baseline_Pats) %>%
  mutate(Diff= TIME_STUDY*12) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAS) %>%
  rename("Baseline"="deltaPAS") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(deltaPAS)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAS)
  ) %>% 
  mutate(Diff= (deltaPAS-Baseline)/TIME_STUDY) %>% inner_join(PAS_Delta_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


# ALL UMSARS Total Year 3

AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==3) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(PAS_Delta_Baseline_Pats) %>%
  summarise(
    mean=mean(deltaPAS),
    sd=sd(deltaPAS),
    median=median(deltaPAS),
    Q1 = quantile(deltaPAS, 0.25),
    Q3 = quantile(deltaPAS, 0.75),
    min=min(deltaPAS),
    max=max(deltaPAS),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAS) %>%
  rename("Baseline"="deltaPAS") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(deltaPAS)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAS)
  ) %>%  inner_join(PAS_Delta_Baseline_Pats) %>%
  summarise(mean= mean((deltaPAS-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAS) %>%
  rename("Baseline"="deltaPAS") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(deltaPAS)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAS)
  ) %>%  
  mutate(Diff= (deltaPAS-Baseline)) %>% inner_join(PAS_Delta_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAS) %>%
  rename("Baseline"="deltaPAS") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(deltaPAS)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAS)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(PAS_Delta_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAS) %>%
  rename("Baseline"="deltaPAS") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(deltaPAS)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAS)
  ) %>% 
  mutate(Diff= (deltaPAS-Baseline)/TIME_STUDY) %>% inner_join(PAS_Delta_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )

# --------------


# By MSA P vs MSA C -----------------
# ALL UMSARS Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(PAS_Delta_Baseline_Pats) %>%
  summarise(
    mean=mean(deltaPAS),
    sd=sd(deltaPAS),
    median=median(deltaPAS),
    Q1 = quantile(deltaPAS, 0.25),
    Q3 = quantile(deltaPAS, 0.75),
    min=min(deltaPAS),
    max=max(deltaPAS),
    n=n()
  )



# ALL UMSARS Total Year 1

AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==1) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(PAS_Delta_Baseline_Pats) %>%
  summarise(
    mean=mean(deltaPAS),
    sd=sd(deltaPAS),
    median=median(deltaPAS),
    Q1 = quantile(deltaPAS, 0.25),
    Q3 = quantile(deltaPAS, 0.75),
    min=min(deltaPAS),
    max=max(deltaPAS),
    n=n()
  )

# Change from baseline

AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAS) %>%
  rename("Baseline"="deltaPAS") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(deltaPAS)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAS)
  ) %>%  inner_join(PAS_Delta_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((deltaPAS-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAS) %>%
  rename("Baseline"="deltaPAS") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(deltaPAS)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAS)
  ) %>% 
  mutate(Diff= (deltaPAS-Baseline)) %>% inner_join(PAS_Delta_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAS) %>%
  rename("Baseline"="deltaPAS") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(deltaPAS)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAS)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(PAS_Delta_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAS) %>%
  rename("Baseline"="deltaPAS") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(deltaPAS)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAS)
  ) %>% 
  mutate(Diff= (deltaPAS-Baseline)/TIME_STUDY) %>% inner_join(PAS_Delta_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



# ALL UMSARS Total Year 2

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==2) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(PAS_Delta_Baseline_Pats) %>%
  summarise(
    mean=mean(deltaPAS),
    sd=sd(deltaPAS),
    median=median(deltaPAS),
    Q1 = quantile(deltaPAS, 0.25),
    Q3 = quantile(deltaPAS, 0.75),
    min=min(deltaPAS),
    max=max(deltaPAS),
    n=n()
  )


# Change from baseline

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAS) %>%
  rename("Baseline"="deltaPAS") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(deltaPAS)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAS)
  ) %>%  inner_join(PAS_Delta_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((deltaPAS-Baseline)/TIME_STUDY)) 



AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAS) %>%
  rename("Baseline"="deltaPAS") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(deltaPAS)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAS)
  ) %>% 
  mutate(Diff= (deltaPAS-Baseline)) %>% inner_join(PAS_Delta_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAS) %>%
  rename("Baseline"="deltaPAS") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(deltaPAS)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAS)
  ) %>%  inner_join(PAS_Delta_Baseline_Pats) %>%
  mutate(Diff= TIME_STUDY*12) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAS) %>%
  rename("Baseline"="deltaPAS") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(deltaPAS)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAS)
  ) %>% 
  mutate(Diff= (deltaPAS-Baseline)/TIME_STUDY) %>% inner_join(PAS_Delta_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


# ALL UMSARS Total Year 3

AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==3) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(PAS_Delta_Baseline_Pats) %>%
  summarise(
    mean=mean(deltaPAS),
    sd=sd(deltaPAS),
    median=median(deltaPAS),
    Q1 = quantile(deltaPAS, 0.25),
    Q3 = quantile(deltaPAS, 0.75),
    min=min(deltaPAS),
    max=max(deltaPAS),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAS) %>%
  rename("Baseline"="deltaPAS") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(deltaPAS)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAS)
  ) %>% group_by(DIAG) %>% inner_join(PAS_Delta_Baseline_Pats) %>%
  summarise(mean= mean((deltaPAS-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAS) %>%
  rename("Baseline"="deltaPAS") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(deltaPAS)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAS)
  ) %>%  inner_join(PAS_Delta_Baseline_Pats) %>%
  mutate(Diff= (deltaPAS-Baseline)) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAS) %>%
  rename("Baseline"="deltaPAS") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(deltaPAS)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAS)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(PAS_Delta_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAS) %>%
  rename("Baseline"="deltaPAS") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(deltaPAS)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAS)
  ) %>% 
  mutate(Diff= (deltaPAS-Baseline)/TIME_STUDY) %>% inner_join(PAS_Delta_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



# ------------
# By Possible vs probable --------------------

# ALL UMSARS Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(PAS_Delta_Baseline_Pats) %>%
  summarise(
    mean=mean(deltaPAS),
    sd=sd(deltaPAS),
    median=median(deltaPAS),
    Q1 = quantile(deltaPAS, 0.25),
    Q3 = quantile(deltaPAS, 0.75),
    min=min(deltaPAS),
    max=max(deltaPAS),
    n=n()
  )



# ALL UMSARS Total Year 1

AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==1) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(PAS_Delta_Baseline_Pats) %>%
  summarise(
    mean=mean(deltaPAS),
    sd=sd(deltaPAS),
    median=median(deltaPAS),
    Q1 = quantile(deltaPAS, 0.25),
    Q3 = quantile(deltaPAS, 0.75),
    min=min(deltaPAS),
    max=max(deltaPAS),
    n=n()
  )

# Change from baseline


AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAS) %>%
  rename("Baseline"="deltaPAS") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(deltaPAS)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAS)
  ) %>%  inner_join(PAS_Delta_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((deltaPAS-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAS) %>%
  rename("Baseline"="deltaPAS") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(deltaPAS)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAS)
  ) %>% 
  mutate(Diff= (deltaPAS-Baseline)) %>% inner_join(PAS_Delta_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAS) %>%
  rename("Baseline"="deltaPAS") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(deltaPAS)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAS)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(PAS_Delta_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAS) %>%
  rename("Baseline"="deltaPAS") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(deltaPAS)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAS)
  ) %>% 
  mutate(Diff= (deltaPAS-Baseline)/TIME_STUDY) %>% inner_join(PAS_Delta_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



# ALL UMSARS Total Year 2

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==2) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(PAS_Delta_Baseline_Pats) %>%
  summarise(
    mean=mean(deltaPAS),
    sd=sd(deltaPAS),
    median=median(deltaPAS),
    Q1 = quantile(deltaPAS, 0.25),
    Q3 = quantile(deltaPAS, 0.75),
    min=min(deltaPAS),
    max=max(deltaPAS),
    n=n()
  )


# Change from baseline

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAS) %>%
  rename("Baseline"="deltaPAS") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(deltaPAS)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAS)
  ) %>%  inner_join(PAS_Delta_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((deltaPAS-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAS) %>%
  rename("Baseline"="deltaPAS") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(deltaPAS)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAS)
  ) %>% 
  mutate(Diff= (deltaPAS-Baseline)) %>% inner_join(PAS_Delta_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAS) %>%
  rename("Baseline"="deltaPAS") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(deltaPAS)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAS)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(PAS_Delta_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAS) %>%
  rename("Baseline"="deltaPAS") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(deltaPAS)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAS)
  ) %>% 
  mutate(Diff= (deltaPAS-Baseline)/TIME_STUDY) %>% inner_join(PAS_Delta_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


# ALL UMSARS Total Year 3

AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==3) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(PAS_Delta_Baseline_Pats) %>%
  summarise(
    mean=mean(deltaPAS),
    sd=sd(deltaPAS),
    median=median(deltaPAS),
    Q1 = quantile(deltaPAS, 0.25),
    Q3 = quantile(deltaPAS, 0.75),
    min=min(deltaPAS),
    max=max(deltaPAS),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAS) %>%
  rename("Baseline"="deltaPAS") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(deltaPAS)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAS)
  ) %>% group_by(DIAGNIV) %>% inner_join(PAS_Delta_Baseline_Pats) %>%
  summarise(mean= mean((deltaPAS-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAS) %>%
  rename("Baseline"="deltaPAS") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(deltaPAS)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAS)
  ) %>% 
  mutate(Diff= (deltaPAS-Baseline)) %>% inner_join(PAS_Delta_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAS) %>%
  rename("Baseline"="deltaPAS") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(deltaPAS)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAS)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(PAS_Delta_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAS) %>%
  rename("Baseline"="deltaPAS") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(deltaPAS)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAS)
  ) %>% 
  mutate(Diff= (deltaPAS-Baseline)/TIME_STUDY) %>% inner_join(PAS_Delta_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )

# -----------------------------
# Overall Early CT  -------------------------------

# ALL UMSARS Total Year 0 


EarlyCT_Pop_Baseline_319 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(PAS_Delta_Baseline_Pats) %>%
  summarise(
    mean=mean(deltaPAS),
    sd=sd(deltaPAS),
    median=median(deltaPAS),
    Q1 = quantile(deltaPAS, 0.25),
    Q3 = quantile(deltaPAS, 0.75),
    min=min(deltaPAS),
    max=max(deltaPAS),
    n=n()
  )



# ALL UMSARS Total Year 1

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==1) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(PAS_Delta_Baseline_Pats) %>%
  summarise(
    mean=mean(deltaPAS),
    sd=sd(deltaPAS),
    median=median(deltaPAS),
    Q1 = quantile(deltaPAS, 0.25),
    Q3 = quantile(deltaPAS, 0.75),
    min=min(deltaPAS),
    max=max(deltaPAS),
    n=n()
  )

# Change from baseline

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAS) %>%
  rename("Baseline"="deltaPAS") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(deltaPAS)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAS)
  ) %>%  inner_join(PAS_Delta_Baseline_Pats) %>%
  summarise(mean= mean((deltaPAS-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAS) %>%
  rename("Baseline"="deltaPAS") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(deltaPAS)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAS)
  ) %>% 
  mutate(Diff= (deltaPAS-Baseline)) %>% inner_join(PAS_Delta_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAS) %>%
  rename("Baseline"="deltaPAS") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(deltaPAS)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAS)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(PAS_Delta_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAS) %>%
  rename("Baseline"="deltaPAS") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(deltaPAS)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAS)
  ) %>% 
  mutate(Diff= (deltaPAS-Baseline)/TIME_STUDY) %>% inner_join(PAS_Delta_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



# ALL UMSARS Total Year 2

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==2) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(PAS_Delta_Baseline_Pats) %>%
  summarise(
    mean=mean(deltaPAS),
    sd=sd(deltaPAS),
    median=median(deltaPAS),
    Q1 = quantile(deltaPAS, 0.25),
    Q3 = quantile(deltaPAS, 0.75),
    min=min(deltaPAS),
    max=max(deltaPAS),
    n=n()
  )


# Change from baseline

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAS) %>%
  rename("Baseline"="deltaPAS") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(deltaPAS)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAS)
  ) %>%  inner_join(PAS_Delta_Baseline_Pats) %>%
  summarise(mean= mean((deltaPAS-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAS) %>%
  rename("Baseline"="deltaPAS") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(deltaPAS)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAS)
  ) %>% 
  mutate(Diff= (deltaPAS-Baseline)) %>% inner_join(PAS_Delta_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAS) %>%
  rename("Baseline"="deltaPAS") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(deltaPAS)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAS)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(PAS_Delta_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAS) %>%
  rename("Baseline"="deltaPAS") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(deltaPAS)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAS)
  ) %>% 
  mutate(Diff= (deltaPAS-Baseline)/TIME_STUDY) %>% inner_join(PAS_Delta_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


# ALL UMSARS Total Year 3

EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==3) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(PAS_Delta_Baseline_Pats) %>%
  summarise(
    mean=mean(deltaPAS),
    sd=sd(deltaPAS),
    median=median(deltaPAS),
    Q1 = quantile(deltaPAS, 0.25),
    Q3 = quantile(deltaPAS, 0.75),
    min=min(deltaPAS),
    max=max(deltaPAS),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAS) %>%
  rename("Baseline"="deltaPAS") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(deltaPAS)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAS)
  ) %>%  inner_join(PAS_Delta_Baseline_Pats) %>%
  summarise(mean= mean((deltaPAS-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAS) %>%
  rename("Baseline"="deltaPAS") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(deltaPAS)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAS)
  ) %>% 
  mutate(Diff= (deltaPAS-Baseline)) %>% inner_join(PAS_Delta_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAS) %>%
  rename("Baseline"="deltaPAS") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(deltaPAS)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAS)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(PAS_Delta_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAS) %>%
  rename("Baseline"="deltaPAS") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(deltaPAS)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAS)
  ) %>% 
  mutate(Diff= (deltaPAS-Baseline)/TIME_STUDY) %>% inner_join(PAS_Delta_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )

# --------------


# By MSA P vs MSA C -----------------
# ALL UMSARS Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(PAS_Delta_Baseline_Pats) %>%
  summarise(
    mean=mean(deltaPAS),
    sd=sd(deltaPAS),
    median=median(deltaPAS),
    Q1 = quantile(deltaPAS, 0.25),
    Q3 = quantile(deltaPAS, 0.75),
    min=min(deltaPAS),
    max=max(deltaPAS),
    n=n()
  )



# ALL UMSARS Total Year 1

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==1) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(PAS_Delta_Baseline_Pats) %>%
  summarise(
    mean=mean(deltaPAS),
    sd=sd(deltaPAS),
    median=median(deltaPAS),
    Q1 = quantile(deltaPAS, 0.25),
    Q3 = quantile(deltaPAS, 0.75),
    min=min(deltaPAS),
    max=max(deltaPAS),
    n=n()
  )

# Change from baseline

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAS) %>%
  rename("Baseline"="deltaPAS") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(deltaPAS)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAS)
  ) %>%  inner_join(PAS_Delta_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((deltaPAS-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAS) %>%
  rename("Baseline"="deltaPAS") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(deltaPAS)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAS)
  ) %>% 
  mutate(Diff= (deltaPAS-Baseline)) %>% inner_join(PAS_Delta_Baseline_Pats) %>%
  group_by(DIAG) %>% 
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAS) %>%
  rename("Baseline"="deltaPAS") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(deltaPAS)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAS)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(PAS_Delta_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAS) %>%
  rename("Baseline"="deltaPAS") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(deltaPAS)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAS)
  ) %>% 
  mutate(Diff= (deltaPAS-Baseline)/TIME_STUDY) %>% inner_join(PAS_Delta_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



# ALL UMSARS Total Year 2

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==2) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(PAS_Delta_Baseline_Pats) %>%
  summarise(
    mean=mean(deltaPAS),
    sd=sd(deltaPAS),
    median=median(deltaPAS),
    Q1 = quantile(deltaPAS, 0.25),
    Q3 = quantile(deltaPAS, 0.75),
    min=min(deltaPAS),
    max=max(deltaPAS),
    n=n()
  )


# Change from baseline

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAS) %>%
  rename("Baseline"="deltaPAS") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(deltaPAS)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAS)
  ) %>% inner_join(PAS_Delta_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((deltaPAS-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAS) %>%
  rename("Baseline"="deltaPAS") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(deltaPAS)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAS)
  ) %>% 
  mutate(Diff= (deltaPAS-Baseline)) %>% inner_join(PAS_Delta_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAS) %>%
  rename("Baseline"="deltaPAS") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(deltaPAS)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAS)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(PAS_Delta_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAS) %>%
  rename("Baseline"="deltaPAS") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(deltaPAS)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAS)
  ) %>% 
  mutate(Diff= (deltaPAS-Baseline)/TIME_STUDY) %>% inner_join(PAS_Delta_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


# ALL UMSARS Total Year 3

EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==3) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(PAS_Delta_Baseline_Pats) %>%
  summarise(
    mean=mean(deltaPAS),
    sd=sd(deltaPAS),
    median=median(deltaPAS),
    Q1 = quantile(deltaPAS, 0.25),
    Q3 = quantile(deltaPAS, 0.75),
    min=min(deltaPAS),
    max=max(deltaPAS),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAS) %>%
  rename("Baseline"="deltaPAS") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(deltaPAS)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAS)
  ) %>% group_by(DIAG) %>% inner_join(PAS_Delta_Baseline_Pats) %>%
  summarise(mean= mean((deltaPAS-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAS) %>%
  rename("Baseline"="deltaPAS") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(deltaPAS)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAS)
  ) %>% 
  mutate(Diff= (deltaPAS-Baseline)) %>% inner_join(PAS_Delta_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAS) %>%
  rename("Baseline"="deltaPAS") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(deltaPAS)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAS)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(PAS_Delta_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAS) %>%
  rename("Baseline"="deltaPAS") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(deltaPAS)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAS)
  ) %>% 
  mutate(Diff= (deltaPAS-Baseline)/TIME_STUDY) %>% inner_join(PAS_Delta_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



# ------------
# By Possible vs probable --------------------

# ALL UMSARS Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(PAS_Delta_Baseline_Pats) %>%
  summarise(
    mean=mean(deltaPAS),
    sd=sd(deltaPAS),
    median=median(deltaPAS),
    Q1 = quantile(deltaPAS, 0.25),
    Q3 = quantile(deltaPAS, 0.75),
    min=min(deltaPAS),
    max=max(deltaPAS),
    n=n()
  )



# ALL UMSARS Total Year 1

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==1) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(PAS_Delta_Baseline_Pats) %>%
  summarise(
    mean=mean(deltaPAS),
    sd=sd(deltaPAS),
    median=median(deltaPAS),
    Q1 = quantile(deltaPAS, 0.25),
    Q3 = quantile(deltaPAS, 0.75),
    min=min(deltaPAS),
    max=max(deltaPAS),
    n=n()
  )

# Change from baseline


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAS) %>%
  rename("Baseline"="deltaPAS") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(deltaPAS)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAS)
  ) %>%  inner_join(PAS_Delta_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((deltaPAS-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAS) %>%
  rename("Baseline"="deltaPAS") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(deltaPAS)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAS)
  ) %>% 
  mutate(Diff= (deltaPAS-Baseline)) %>% inner_join(PAS_Delta_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )




EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAS) %>%
  rename("Baseline"="deltaPAS") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(deltaPAS)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAS)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(PAS_Delta_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )




EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAS) %>%
  rename("Baseline"="deltaPAS") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(deltaPAS)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAS)
  ) %>% 
  mutate(Diff= (deltaPAS-Baseline)/TIME_STUDY) %>% inner_join(PAS_Delta_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



# ALL UMSARS Total Year 2

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==2) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(PAS_Delta_Baseline_Pats) %>%
  summarise(
    mean=mean(deltaPAS),
    sd=sd(deltaPAS),
    median=median(deltaPAS),
    Q1 = quantile(deltaPAS, 0.25),
    Q3 = quantile(deltaPAS, 0.75),
    min=min(deltaPAS),
    max=max(deltaPAS),
    n=n()
  )


# Change from baseline

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAS) %>%
  rename("Baseline"="deltaPAS") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(deltaPAS)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAS)
  ) %>%  inner_join(PAS_Delta_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((deltaPAS-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAS) %>%
  rename("Baseline"="deltaPAS") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(deltaPAS)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAS)
  ) %>% 
  mutate(Diff= (deltaPAS-Baseline)) %>% inner_join(PAS_Delta_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAS) %>%
  rename("Baseline"="deltaPAS") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(deltaPAS)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAS)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(PAS_Delta_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAS) %>%
  rename("Baseline"="deltaPAS") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(deltaPAS)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAS)
  ) %>% 
  mutate(Diff= (deltaPAS-Baseline)/TIME_STUDY) %>% inner_join(PAS_Delta_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


# ALL UMSARS Total Year 3

EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==3) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(PAS_Delta_Baseline_Pats) %>%
  summarise(
    mean=mean(deltaPAS),
    sd=sd(deltaPAS),
    median=median(deltaPAS),
    Q1 = quantile(deltaPAS, 0.25),
    Q3 = quantile(deltaPAS, 0.75),
    min=min(deltaPAS),
    max=max(deltaPAS),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAS) %>%
  rename("Baseline"="deltaPAS") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(deltaPAS)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAS)
  ) %>% group_by(DIAGNIV) %>% inner_join(PAS_Delta_Baseline_Pats) %>%
  summarise(mean= mean((deltaPAS-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAS) %>%
  rename("Baseline"="deltaPAS") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(deltaPAS)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAS)
  ) %>% 
  mutate(Diff= (deltaPAS-Baseline)) %>% inner_join(PAS_Delta_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% 
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAS) %>%
  rename("Baseline"="deltaPAS") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(deltaPAS)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAS)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(PAS_Delta_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAS)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAS) %>%
  rename("Baseline"="deltaPAS") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(deltaPAS)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAS)
  ) %>% 
  mutate(Diff= (deltaPAS-Baseline)/TIME_STUDY) %>% inner_join(PAS_Delta_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )

# -----------------------------
# Inputs UMSARS 3 DBP MAX Delta ----------------------------------

AllMSA_Pop_Baseline_671 <- fread("Source/AllMSA_Pop_Baseline_671.txt")
AllMSA_Pop_BaselineYear1_410 <- fread("Source/AllMSA_Pop_BaselineYear1_410.txt")
AllMSA_Pop_BaselineYear1Year2_245 <- fread("Source/AllMSA_Pop_BaselineYear1Year2_245.txt")
AllMSA_Pop_BaselineYear1Year2Year3Plus_158 <- fread("Source/AllMSA_Pop_BaselineYear1Year2Year3Plus_158.txt")

EarlyCT_Pop_Baseline_319 <- fread("Source/EarlyCT_Pop_Baseline_319.txt")
EarlyCT_Pop_BaselineYear1_208 <- fread("Source/EarlyCT_Pop_BaselineYear1_208.txt")
EarlyCT_Pop_BaselineYear1Year2_134 <- fread("Source/EarlyCT_Pop_BaselineYear1Year2_134.txt")
EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 <- fread( "Source/EarlyCT_Pop_BaselineYear1Year2Year3Plus_99.txt")


dataCohorteManaged <- readRDS("Source/dataCohorteManaged.rds")

dataCohorteManaged <- dataCohorteManaged %>% group_by(NUM) %>% mutate(TIME_STUDY = ifelse( is.na(TIME_STUDY), 0, TIME_STUDY)) %>%
  mutate(Year= ifelse(TIME_STUDY==0, 0,
                      ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                             ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                    ifelse(TIME_STUDY>=2.5 ,3, NA))))) 

DBP <- dataCohorteManaged %>%
  select(NUM, DATECONSULT, TIME_STUDY, Year, UMSARS3_ORTHO, PAD_COU, PAD_1MN:PAD_10MN, deltaPAD)

DBP <- DBP %>% filter(!is.na(deltaPAD))
DBP <-  gather(DBP, measure_PAD, PAD_DEB, PAD_1MN:PAD_10MN)
DBP <-  DBP %>% filter(deltaPAD == PAD_DEB-PAD_COU)


DBP$measure_PAD <- substr(DBP$measure_PAD, start = 5, stop=nchar(DBP$measure_PAD))
DBP$measure_PAD <- substr(DBP$measure_PAD, start = 1, stop=nchar(DBP$measure_PAD)-2)
unique(DBP$measure_PAD)

DBP <- DBP %>% select(-c(measure_PAD)) %>% distinct()
DBP <-  DBP %>% ungroup()
mean(DBP$PAD_COU); mean(DBP$PAD_DEB); mean(DBP$deltaPAD); 


PAD_Delta_Baseline_Pats <- DBP %>% filter(Year==0) %>% filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM) %>% distinct()

# ----------------
# Overall MSA Entire -------------------------------

# ALL UMSARS Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(DBP) %>% filter(Year==0) %>% 
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(PAD_Delta_Baseline_Pats) %>%
  summarise(
    mean=mean(deltaPAD),
    sd=sd(deltaPAD),
    median=median(deltaPAD),
    Q1 = quantile(deltaPAD, 0.25),
    Q3 = quantile(deltaPAD, 0.75),
    min=min(deltaPAD),
    max=max(deltaPAD),
    n=n()
  )



# ALL UMSARS Total Year 1

AllMSA_Pop_BaselineYear1_410 %>% inner_join(DBP) %>% filter(Year==1) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(PAD_Delta_Baseline_Pats) %>%
  summarise(
    mean=mean(deltaPAD),
    sd=sd(deltaPAD),
    median=median(deltaPAD),
    Q1 = quantile(deltaPAD, 0.25),
    Q3 = quantile(deltaPAD, 0.75),
    min=min(deltaPAD),
    max=max(deltaPAD),
    n=n()
  )

# Change from baseline

AllMSA_Pop_BaselineYear1_410 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAD) %>%
  rename("Baseline"="deltaPAD") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(DBP) %>% filter(Year==1) %>%
      filter(!is.na(deltaPAD)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAD)
  ) %>%  inner_join(PAD_Delta_Baseline_Pats) %>%
  summarise(mean= mean((deltaPAD-Baseline)/TIME_STUDY)) 



AllMSA_Pop_BaselineYear1_410 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAD) %>%
  rename("Baseline"="deltaPAD") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(DBP) %>% filter(Year==1) %>%
      filter(!is.na(deltaPAD)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAD)
  ) %>%  inner_join(PAD_Delta_Baseline_Pats) %>%
  mutate(Diff= (deltaPAD-Baseline)) %>% 
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )




AllMSA_Pop_BaselineYear1_410 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAD) %>%
  rename("Baseline"="deltaPAD") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(DBP) %>% filter(Year==1) %>%
      filter(!is.na(deltaPAD)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAD)
  ) %>%  inner_join(PAD_Delta_Baseline_Pats) %>%
  mutate(Diff= TIME_STUDY*12) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



AllMSA_Pop_BaselineYear1_410 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAD) %>%
  rename("Baseline"="deltaPAD") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(DBP) %>% filter(Year==1) %>%
      filter(!is.na(deltaPAD)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAD)
  ) %>%  inner_join(PAD_Delta_Baseline_Pats) %>%
  mutate(Diff= (deltaPAD-Baseline)/TIME_STUDY) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



# ALL UMSARS Total Year 2

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(DBP) %>% filter(Year==2) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(PAD_Delta_Baseline_Pats) %>%
  summarise(
    mean=mean(deltaPAD),
    sd=sd(deltaPAD),
    median=median(deltaPAD),
    Q1 = quantile(deltaPAD, 0.25),
    Q3 = quantile(deltaPAD, 0.75),
    min=min(deltaPAD),
    max=max(deltaPAD),
    n=n()
  )


# Change from baseline

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAD) %>%
  rename("Baseline"="deltaPAD") %>% 
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(DBP) %>% filter(Year==2) %>%
      filter(!is.na(deltaPAD)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAD)
  ) %>%  inner_join(PAD_Delta_Baseline_Pats) %>%
  summarise(mean= mean((deltaPAD-Baseline)/TIME_STUDY)) 



AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAD) %>%
  rename("Baseline"="deltaPAD") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(DBP) %>% filter(Year==2) %>%
      filter(!is.na(deltaPAD)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAD)
  ) %>%  inner_join(PAD_Delta_Baseline_Pats) %>%
  mutate(Diff= (deltaPAD-Baseline)) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAD) %>%
  rename("Baseline"="deltaPAD") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(DBP) %>% filter(Year==2) %>%
      filter(!is.na(deltaPAD)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAD)
  ) %>%  inner_join(PAD_Delta_Baseline_Pats) %>%
  mutate(Diff= TIME_STUDY*12) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAD) %>%
  rename("Baseline"="deltaPAD") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(DBP) %>% filter(Year==2) %>%
      filter(!is.na(deltaPAD)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAD)
  ) %>% 
  mutate(Diff= (deltaPAD-Baseline)/TIME_STUDY) %>% inner_join(PAD_Delta_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


# ALL UMSARS Total Year 3

AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(DBP) %>% filter(Year==3) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(PAD_Delta_Baseline_Pats) %>%
  summarise(
    mean=mean(deltaPAD),
    sd=sd(deltaPAD),
    median=median(deltaPAD),
    Q1 = quantile(deltaPAD, 0.25),
    Q3 = quantile(deltaPAD, 0.75),
    min=min(deltaPAD),
    max=max(deltaPAD),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAD) %>%
  rename("Baseline"="deltaPAD") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(DBP) %>% filter(Year==3) %>%
      filter(!is.na(deltaPAD)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAD)
  ) %>%  inner_join(PAD_Delta_Baseline_Pats) %>%
  summarise(mean= mean((deltaPAD-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAD) %>%
  rename("Baseline"="deltaPAD") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(DBP) %>% filter(Year==3) %>%
      filter(!is.na(deltaPAD)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAD)
  ) %>%  
  mutate(Diff= (deltaPAD-Baseline)) %>% inner_join(PAD_Delta_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAD) %>%
  rename("Baseline"="deltaPAD") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(DBP) %>% filter(Year==3) %>%
      filter(!is.na(deltaPAD)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAD)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(PAD_Delta_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAD) %>%
  rename("Baseline"="deltaPAD") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(DBP) %>% filter(Year==3) %>%
      filter(!is.na(deltaPAD)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAD)
  ) %>% 
  mutate(Diff= (deltaPAD-Baseline)/TIME_STUDY) %>% inner_join(PAD_Delta_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )

# --------------


# By MSA P vs MSA C -----------------
# ALL UMSARS Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(PAD_Delta_Baseline_Pats) %>%
  summarise(
    mean=mean(deltaPAD),
    sd=sd(deltaPAD),
    median=median(deltaPAD),
    Q1 = quantile(deltaPAD, 0.25),
    Q3 = quantile(deltaPAD, 0.75),
    min=min(deltaPAD),
    max=max(deltaPAD),
    n=n()
  )



# ALL UMSARS Total Year 1

AllMSA_Pop_BaselineYear1_410 %>% inner_join(DBP) %>% filter(Year==1) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(PAD_Delta_Baseline_Pats) %>%
  summarise(
    mean=mean(deltaPAD),
    sd=sd(deltaPAD),
    median=median(deltaPAD),
    Q1 = quantile(deltaPAD, 0.25),
    Q3 = quantile(deltaPAD, 0.75),
    min=min(deltaPAD),
    max=max(deltaPAD),
    n=n()
  )

# Change from baseline

AllMSA_Pop_BaselineYear1_410 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAD) %>%
  rename("Baseline"="deltaPAD") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(DBP) %>% filter(Year==1) %>%
      filter(!is.na(deltaPAD)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAD)
  ) %>%  inner_join(PAD_Delta_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((deltaPAD-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1_410 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAD) %>%
  rename("Baseline"="deltaPAD") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(DBP) %>% filter(Year==1) %>%
      filter(!is.na(deltaPAD)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAD)
  ) %>% 
  mutate(Diff= (deltaPAD-Baseline)) %>% inner_join(PAD_Delta_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



AllMSA_Pop_BaselineYear1_410 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAD) %>%
  rename("Baseline"="deltaPAD") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(DBP) %>% filter(Year==1) %>%
      filter(!is.na(deltaPAD)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAD)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(PAD_Delta_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



AllMSA_Pop_BaselineYear1_410 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAD) %>%
  rename("Baseline"="deltaPAD") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(DBP) %>% filter(Year==1) %>%
      filter(!is.na(deltaPAD)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAD)
  ) %>% 
  mutate(Diff= (deltaPAD-Baseline)/TIME_STUDY) %>% inner_join(PAD_Delta_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



# ALL UMSARS Total Year 2

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(DBP) %>% filter(Year==2) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(PAD_Delta_Baseline_Pats) %>%
  summarise(
    mean=mean(deltaPAD),
    sd=sd(deltaPAD),
    median=median(deltaPAD),
    Q1 = quantile(deltaPAD, 0.25),
    Q3 = quantile(deltaPAD, 0.75),
    min=min(deltaPAD),
    max=max(deltaPAD),
    n=n()
  )


# Change from baseline

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAD) %>%
  rename("Baseline"="deltaPAD") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(DBP) %>% filter(Year==2) %>%
      filter(!is.na(deltaPAD)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAD)
  ) %>%  inner_join(PAD_Delta_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((deltaPAD-Baseline)/TIME_STUDY)) 



AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAD) %>%
  rename("Baseline"="deltaPAD") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(DBP) %>% filter(Year==2) %>%
      filter(!is.na(deltaPAD)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAD)
  ) %>% 
  mutate(Diff= (deltaPAD-Baseline)) %>% inner_join(PAD_Delta_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAD) %>%
  rename("Baseline"="deltaPAD") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(DBP) %>% filter(Year==2) %>%
      filter(!is.na(deltaPAD)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAD)
  ) %>%  inner_join(PAD_Delta_Baseline_Pats) %>%
  mutate(Diff= TIME_STUDY*12) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAD) %>%
  rename("Baseline"="deltaPAD") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(DBP) %>% filter(Year==2) %>%
      filter(!is.na(deltaPAD)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAD)
  ) %>% 
  mutate(Diff= (deltaPAD-Baseline)/TIME_STUDY) %>% inner_join(PAD_Delta_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


# ALL UMSARS Total Year 3

AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(DBP) %>% filter(Year==3) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(PAD_Delta_Baseline_Pats) %>%
  summarise(
    mean=mean(deltaPAD),
    sd=sd(deltaPAD),
    median=median(deltaPAD),
    Q1 = quantile(deltaPAD, 0.25),
    Q3 = quantile(deltaPAD, 0.75),
    min=min(deltaPAD),
    max=max(deltaPAD),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAD) %>%
  rename("Baseline"="deltaPAD") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(DBP) %>% filter(Year==3) %>%
      filter(!is.na(deltaPAD)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAD)
  ) %>% group_by(DIAG) %>% inner_join(PAD_Delta_Baseline_Pats) %>%
  summarise(mean= mean((deltaPAD-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAD) %>%
  rename("Baseline"="deltaPAD") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(DBP) %>% filter(Year==3) %>%
      filter(!is.na(deltaPAD)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAD)
  ) %>%  inner_join(PAD_Delta_Baseline_Pats) %>%
  mutate(Diff= (deltaPAD-Baseline)) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAD) %>%
  rename("Baseline"="deltaPAD") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(DBP) %>% filter(Year==3) %>%
      filter(!is.na(deltaPAD)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAD)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(PAD_Delta_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAD) %>%
  rename("Baseline"="deltaPAD") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(DBP) %>% filter(Year==3) %>%
      filter(!is.na(deltaPAD)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAD)
  ) %>% 
  mutate(Diff= (deltaPAD-Baseline)/TIME_STUDY) %>% inner_join(PAD_Delta_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



# ------------
# By Possible vs probable --------------------

# ALL UMSARS Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(PAD_Delta_Baseline_Pats) %>%
  summarise(
    mean=mean(deltaPAD),
    sd=sd(deltaPAD),
    median=median(deltaPAD),
    Q1 = quantile(deltaPAD, 0.25),
    Q3 = quantile(deltaPAD, 0.75),
    min=min(deltaPAD),
    max=max(deltaPAD),
    n=n()
  )



# ALL UMSARS Total Year 1

AllMSA_Pop_BaselineYear1_410 %>% inner_join(DBP) %>% filter(Year==1) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(PAD_Delta_Baseline_Pats) %>%
  summarise(
    mean=mean(deltaPAD),
    sd=sd(deltaPAD),
    median=median(deltaPAD),
    Q1 = quantile(deltaPAD, 0.25),
    Q3 = quantile(deltaPAD, 0.75),
    min=min(deltaPAD),
    max=max(deltaPAD),
    n=n()
  )

# Change from baseline


AllMSA_Pop_BaselineYear1_410 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAD) %>%
  rename("Baseline"="deltaPAD") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(DBP) %>% filter(Year==1) %>%
      filter(!is.na(deltaPAD)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAD)
  ) %>%  inner_join(PAD_Delta_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((deltaPAD-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1_410 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAD) %>%
  rename("Baseline"="deltaPAD") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(DBP) %>% filter(Year==1) %>%
      filter(!is.na(deltaPAD)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAD)
  ) %>% 
  mutate(Diff= (deltaPAD-Baseline)) %>% inner_join(PAD_Delta_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1_410 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAD) %>%
  rename("Baseline"="deltaPAD") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(DBP) %>% filter(Year==1) %>%
      filter(!is.na(deltaPAD)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAD)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(PAD_Delta_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1_410 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAD) %>%
  rename("Baseline"="deltaPAD") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(DBP) %>% filter(Year==1) %>%
      filter(!is.na(deltaPAD)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAD)
  ) %>% 
  mutate(Diff= (deltaPAD-Baseline)/TIME_STUDY) %>% inner_join(PAD_Delta_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



# ALL UMSARS Total Year 2

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(DBP) %>% filter(Year==2) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(PAD_Delta_Baseline_Pats) %>%
  summarise(
    mean=mean(deltaPAD),
    sd=sd(deltaPAD),
    median=median(deltaPAD),
    Q1 = quantile(deltaPAD, 0.25),
    Q3 = quantile(deltaPAD, 0.75),
    min=min(deltaPAD),
    max=max(deltaPAD),
    n=n()
  )


# Change from baseline

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAD) %>%
  rename("Baseline"="deltaPAD") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(DBP) %>% filter(Year==2) %>%
      filter(!is.na(deltaPAD)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAD)
  ) %>%  inner_join(PAD_Delta_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((deltaPAD-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAD) %>%
  rename("Baseline"="deltaPAD") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(DBP) %>% filter(Year==2) %>%
      filter(!is.na(deltaPAD)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAD)
  ) %>% 
  mutate(Diff= (deltaPAD-Baseline)) %>% inner_join(PAD_Delta_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAD) %>%
  rename("Baseline"="deltaPAD") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(DBP) %>% filter(Year==2) %>%
      filter(!is.na(deltaPAD)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAD)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(PAD_Delta_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAD) %>%
  rename("Baseline"="deltaPAD") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(DBP) %>% filter(Year==2) %>%
      filter(!is.na(deltaPAD)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAD)
  ) %>% 
  mutate(Diff= (deltaPAD-Baseline)/TIME_STUDY) %>% inner_join(PAD_Delta_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


# ALL UMSARS Total Year 3

AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(DBP) %>% filter(Year==3) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(PAD_Delta_Baseline_Pats) %>%
  summarise(
    mean=mean(deltaPAD),
    sd=sd(deltaPAD),
    median=median(deltaPAD),
    Q1 = quantile(deltaPAD, 0.25),
    Q3 = quantile(deltaPAD, 0.75),
    min=min(deltaPAD),
    max=max(deltaPAD),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAD) %>%
  rename("Baseline"="deltaPAD") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(DBP) %>% filter(Year==3) %>%
      filter(!is.na(deltaPAD)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAD)
  ) %>% group_by(DIAGNIV) %>% inner_join(PAD_Delta_Baseline_Pats) %>%
  summarise(mean= mean((deltaPAD-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAD) %>%
  rename("Baseline"="deltaPAD") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(DBP) %>% filter(Year==3) %>%
      filter(!is.na(deltaPAD)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAD)
  ) %>% 
  mutate(Diff= (deltaPAD-Baseline)) %>% inner_join(PAD_Delta_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAD) %>%
  rename("Baseline"="deltaPAD") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(DBP) %>% filter(Year==3) %>%
      filter(!is.na(deltaPAD)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAD)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(PAD_Delta_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAD) %>%
  rename("Baseline"="deltaPAD") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(DBP) %>% filter(Year==3) %>%
      filter(!is.na(deltaPAD)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAD)
  ) %>% 
  mutate(Diff= (deltaPAD-Baseline)/TIME_STUDY) %>% inner_join(PAD_Delta_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )

# -----------------------------
# Overall Early CT  -------------------------------

# ALL UMSARS Total Year 0 


EarlyCT_Pop_Baseline_319 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(PAD_Delta_Baseline_Pats) %>%
  summarise(
    mean=mean(deltaPAD),
    sd=sd(deltaPAD),
    median=median(deltaPAD),
    Q1 = quantile(deltaPAD, 0.25),
    Q3 = quantile(deltaPAD, 0.75),
    min=min(deltaPAD),
    max=max(deltaPAD),
    n=n()
  )



# ALL UMSARS Total Year 1

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(DBP) %>% filter(Year==1) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(PAD_Delta_Baseline_Pats) %>%
  summarise(
    mean=mean(deltaPAD),
    sd=sd(deltaPAD),
    median=median(deltaPAD),
    Q1 = quantile(deltaPAD, 0.25),
    Q3 = quantile(deltaPAD, 0.75),
    min=min(deltaPAD),
    max=max(deltaPAD),
    n=n()
  )

# Change from baseline

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAD) %>%
  rename("Baseline"="deltaPAD") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(DBP) %>% filter(Year==1) %>%
      filter(!is.na(deltaPAD)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAD)
  ) %>%  inner_join(PAD_Delta_Baseline_Pats) %>%
  summarise(mean= mean((deltaPAD-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAD) %>%
  rename("Baseline"="deltaPAD") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(DBP) %>% filter(Year==1) %>%
      filter(!is.na(deltaPAD)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAD)
  ) %>% 
  mutate(Diff= (deltaPAD-Baseline)) %>% inner_join(PAD_Delta_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAD) %>%
  rename("Baseline"="deltaPAD") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(DBP) %>% filter(Year==1) %>%
      filter(!is.na(deltaPAD)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAD)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(PAD_Delta_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAD) %>%
  rename("Baseline"="deltaPAD") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(DBP) %>% filter(Year==1) %>%
      filter(!is.na(deltaPAD)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAD)
  ) %>% 
  mutate(Diff= (deltaPAD-Baseline)/TIME_STUDY) %>% inner_join(PAD_Delta_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



# ALL UMSARS Total Year 2

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(DBP) %>% filter(Year==2) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(PAD_Delta_Baseline_Pats) %>%
  summarise(
    mean=mean(deltaPAD),
    sd=sd(deltaPAD),
    median=median(deltaPAD),
    Q1 = quantile(deltaPAD, 0.25),
    Q3 = quantile(deltaPAD, 0.75),
    min=min(deltaPAD),
    max=max(deltaPAD),
    n=n()
  )


# Change from baseline

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAD) %>%
  rename("Baseline"="deltaPAD") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(DBP) %>% filter(Year==2) %>%
      filter(!is.na(deltaPAD)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAD)
  ) %>%  inner_join(PAD_Delta_Baseline_Pats) %>%
  summarise(mean= mean((deltaPAD-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAD) %>%
  rename("Baseline"="deltaPAD") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(DBP) %>% filter(Year==2) %>%
      filter(!is.na(deltaPAD)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAD)
  ) %>% 
  mutate(Diff= (deltaPAD-Baseline)) %>% inner_join(PAD_Delta_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAD) %>%
  rename("Baseline"="deltaPAD") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(DBP) %>% filter(Year==2) %>%
      filter(!is.na(deltaPAD)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAD)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(PAD_Delta_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAD) %>%
  rename("Baseline"="deltaPAD") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(DBP) %>% filter(Year==2) %>%
      filter(!is.na(deltaPAD)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAD)
  ) %>% 
  mutate(Diff= (deltaPAD-Baseline)/TIME_STUDY) %>% inner_join(PAD_Delta_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


# ALL UMSARS Total Year 3

EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(DBP) %>% filter(Year==3) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(PAD_Delta_Baseline_Pats) %>%
  summarise(
    mean=mean(deltaPAD),
    sd=sd(deltaPAD),
    median=median(deltaPAD),
    Q1 = quantile(deltaPAD, 0.25),
    Q3 = quantile(deltaPAD, 0.75),
    min=min(deltaPAD),
    max=max(deltaPAD),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAD) %>%
  rename("Baseline"="deltaPAD") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(DBP) %>% filter(Year==3) %>%
      filter(!is.na(deltaPAD)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAD)
  ) %>%  inner_join(PAD_Delta_Baseline_Pats) %>%
  summarise(mean= mean((deltaPAD-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAD) %>%
  rename("Baseline"="deltaPAD") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(DBP) %>% filter(Year==3) %>%
      filter(!is.na(deltaPAD)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAD)
  ) %>% 
  mutate(Diff= (deltaPAD-Baseline)) %>% inner_join(PAD_Delta_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAD) %>%
  rename("Baseline"="deltaPAD") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(DBP) %>% filter(Year==3) %>%
      filter(!is.na(deltaPAD)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAD)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(PAD_Delta_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAD) %>%
  rename("Baseline"="deltaPAD") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(DBP) %>% filter(Year==3) %>%
      filter(!is.na(deltaPAD)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAD)
  ) %>% 
  mutate(Diff= (deltaPAD-Baseline)/TIME_STUDY) %>% inner_join(PAD_Delta_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )

# --------------


# By MSA P vs MSA C -----------------
# ALL UMSARS Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(PAD_Delta_Baseline_Pats) %>%
  summarise(
    mean=mean(deltaPAD),
    sd=sd(deltaPAD),
    median=median(deltaPAD),
    Q1 = quantile(deltaPAD, 0.25),
    Q3 = quantile(deltaPAD, 0.75),
    min=min(deltaPAD),
    max=max(deltaPAD),
    n=n()
  )



# ALL UMSARS Total Year 1

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(DBP) %>% filter(Year==1) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(PAD_Delta_Baseline_Pats) %>%
  summarise(
    mean=mean(deltaPAD),
    sd=sd(deltaPAD),
    median=median(deltaPAD),
    Q1 = quantile(deltaPAD, 0.25),
    Q3 = quantile(deltaPAD, 0.75),
    min=min(deltaPAD),
    max=max(deltaPAD),
    n=n()
  )

# Change from baseline

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAD) %>%
  rename("Baseline"="deltaPAD") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(DBP) %>% filter(Year==1) %>%
      filter(!is.na(deltaPAD)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAD)
  ) %>%  inner_join(PAD_Delta_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((deltaPAD-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAD) %>%
  rename("Baseline"="deltaPAD") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(DBP) %>% filter(Year==1) %>%
      filter(!is.na(deltaPAD)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAD)
  ) %>% 
  mutate(Diff= (deltaPAD-Baseline)) %>% inner_join(PAD_Delta_Baseline_Pats) %>%
  group_by(DIAG) %>% 
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAD) %>%
  rename("Baseline"="deltaPAD") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(DBP) %>% filter(Year==1) %>%
      filter(!is.na(deltaPAD)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAD)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(PAD_Delta_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAD) %>%
  rename("Baseline"="deltaPAD") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(DBP) %>% filter(Year==1) %>%
      filter(!is.na(deltaPAD)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAD)
  ) %>% 
  mutate(Diff= (deltaPAD-Baseline)/TIME_STUDY) %>% inner_join(PAD_Delta_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



# ALL UMSARS Total Year 2

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(DBP) %>% filter(Year==2) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(PAD_Delta_Baseline_Pats) %>%
  summarise(
    mean=mean(deltaPAD),
    sd=sd(deltaPAD),
    median=median(deltaPAD),
    Q1 = quantile(deltaPAD, 0.25),
    Q3 = quantile(deltaPAD, 0.75),
    min=min(deltaPAD),
    max=max(deltaPAD),
    n=n()
  )


# Change from baseline

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAD) %>%
  rename("Baseline"="deltaPAD") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(DBP) %>% filter(Year==2) %>%
      filter(!is.na(deltaPAD)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAD)
  ) %>% inner_join(PAD_Delta_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((deltaPAD-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAD) %>%
  rename("Baseline"="deltaPAD") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(DBP) %>% filter(Year==2) %>%
      filter(!is.na(deltaPAD)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAD)
  ) %>% 
  mutate(Diff= (deltaPAD-Baseline)) %>% inner_join(PAD_Delta_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAD) %>%
  rename("Baseline"="deltaPAD") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(DBP) %>% filter(Year==2) %>%
      filter(!is.na(deltaPAD)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAD)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(PAD_Delta_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAD) %>%
  rename("Baseline"="deltaPAD") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(DBP) %>% filter(Year==2) %>%
      filter(!is.na(deltaPAD)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAD)
  ) %>% 
  mutate(Diff= (deltaPAD-Baseline)/TIME_STUDY) %>% inner_join(PAD_Delta_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


# ALL UMSARS Total Year 3

EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(DBP) %>% filter(Year==3) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(PAD_Delta_Baseline_Pats) %>%
  summarise(
    mean=mean(deltaPAD),
    sd=sd(deltaPAD),
    median=median(deltaPAD),
    Q1 = quantile(deltaPAD, 0.25),
    Q3 = quantile(deltaPAD, 0.75),
    min=min(deltaPAD),
    max=max(deltaPAD),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAD) %>%
  rename("Baseline"="deltaPAD") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(DBP) %>% filter(Year==3) %>%
      filter(!is.na(deltaPAD)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAD)
  ) %>% group_by(DIAG) %>% inner_join(PAD_Delta_Baseline_Pats) %>%
  summarise(mean= mean((deltaPAD-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAD) %>%
  rename("Baseline"="deltaPAD") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(DBP) %>% filter(Year==3) %>%
      filter(!is.na(deltaPAD)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAD)
  ) %>% 
  mutate(Diff= (deltaPAD-Baseline)) %>% inner_join(PAD_Delta_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAD) %>%
  rename("Baseline"="deltaPAD") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(DBP) %>% filter(Year==3) %>%
      filter(!is.na(deltaPAD)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAD)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(PAD_Delta_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAD) %>%
  rename("Baseline"="deltaPAD") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(DBP) %>% filter(Year==3) %>%
      filter(!is.na(deltaPAD)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAD)
  ) %>% 
  mutate(Diff= (deltaPAD-Baseline)/TIME_STUDY) %>% inner_join(PAD_Delta_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



# ------------
# By Possible vs probable --------------------

# ALL UMSARS Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(PAD_Delta_Baseline_Pats) %>%
  summarise(
    mean=mean(deltaPAD),
    sd=sd(deltaPAD),
    median=median(deltaPAD),
    Q1 = quantile(deltaPAD, 0.25),
    Q3 = quantile(deltaPAD, 0.75),
    min=min(deltaPAD),
    max=max(deltaPAD),
    n=n()
  )



# ALL UMSARS Total Year 1

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(DBP) %>% filter(Year==1) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(PAD_Delta_Baseline_Pats) %>%
  summarise(
    mean=mean(deltaPAD),
    sd=sd(deltaPAD),
    median=median(deltaPAD),
    Q1 = quantile(deltaPAD, 0.25),
    Q3 = quantile(deltaPAD, 0.75),
    min=min(deltaPAD),
    max=max(deltaPAD),
    n=n()
  )

# Change from baseline


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAD) %>%
  rename("Baseline"="deltaPAD") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(DBP) %>% filter(Year==1) %>%
      filter(!is.na(deltaPAD)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAD)
  ) %>%  inner_join(PAD_Delta_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((deltaPAD-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAD) %>%
  rename("Baseline"="deltaPAD") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(DBP) %>% filter(Year==1) %>%
      filter(!is.na(deltaPAD)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAD)
  ) %>% 
  mutate(Diff= (deltaPAD-Baseline)) %>% inner_join(PAD_Delta_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )




EarlyCT_Pop_BaselineYear1_208 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAD) %>%
  rename("Baseline"="deltaPAD") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(DBP) %>% filter(Year==1) %>%
      filter(!is.na(deltaPAD)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAD)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(PAD_Delta_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )




EarlyCT_Pop_BaselineYear1_208 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAD) %>%
  rename("Baseline"="deltaPAD") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(DBP) %>% filter(Year==1) %>%
      filter(!is.na(deltaPAD)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAD)
  ) %>% 
  mutate(Diff= (deltaPAD-Baseline)/TIME_STUDY) %>% inner_join(PAD_Delta_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



# ALL UMSARS Total Year 2

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(DBP) %>% filter(Year==2) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(PAD_Delta_Baseline_Pats) %>%
  summarise(
    mean=mean(deltaPAD),
    sd=sd(deltaPAD),
    median=median(deltaPAD),
    Q1 = quantile(deltaPAD, 0.25),
    Q3 = quantile(deltaPAD, 0.75),
    min=min(deltaPAD),
    max=max(deltaPAD),
    n=n()
  )


# Change from baseline

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAD) %>%
  rename("Baseline"="deltaPAD") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(DBP) %>% filter(Year==2) %>%
      filter(!is.na(deltaPAD)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAD)
  ) %>%  inner_join(PAD_Delta_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((deltaPAD-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAD) %>%
  rename("Baseline"="deltaPAD") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(DBP) %>% filter(Year==2) %>%
      filter(!is.na(deltaPAD)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAD)
  ) %>% 
  mutate(Diff= (deltaPAD-Baseline)) %>% inner_join(PAD_Delta_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAD) %>%
  rename("Baseline"="deltaPAD") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(DBP) %>% filter(Year==2) %>%
      filter(!is.na(deltaPAD)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAD)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(PAD_Delta_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAD) %>%
  rename("Baseline"="deltaPAD") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(DBP) %>% filter(Year==2) %>%
      filter(!is.na(deltaPAD)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAD)
  ) %>% 
  mutate(Diff= (deltaPAD-Baseline)/TIME_STUDY) %>% inner_join(PAD_Delta_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


# ALL UMSARS Total Year 3

EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(DBP) %>% filter(Year==3) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(PAD_Delta_Baseline_Pats) %>%
  summarise(
    mean=mean(deltaPAD),
    sd=sd(deltaPAD),
    median=median(deltaPAD),
    Q1 = quantile(deltaPAD, 0.25),
    Q3 = quantile(deltaPAD, 0.75),
    min=min(deltaPAD),
    max=max(deltaPAD),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAD) %>%
  rename("Baseline"="deltaPAD") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(DBP) %>% filter(Year==3) %>%
      filter(!is.na(deltaPAD)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAD)
  ) %>% group_by(DIAGNIV) %>% inner_join(PAD_Delta_Baseline_Pats) %>%
  summarise(mean= mean((deltaPAD-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAD) %>%
  rename("Baseline"="deltaPAD") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(DBP) %>% filter(Year==3) %>%
      filter(!is.na(deltaPAD)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAD)
  ) %>% 
  mutate(Diff= (deltaPAD-Baseline)) %>% inner_join(PAD_Delta_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% 
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAD) %>%
  rename("Baseline"="deltaPAD") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(DBP) %>% filter(Year==3) %>%
      filter(!is.na(deltaPAD)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAD)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(PAD_Delta_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(DBP) %>% filter(Year==0) %>%
  filter(!is.na(deltaPAD)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  deltaPAD) %>%
  rename("Baseline"="deltaPAD") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(DBP) %>% filter(Year==3) %>%
      filter(!is.na(deltaPAD)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, deltaPAD)
  ) %>% 
  mutate(Diff= (deltaPAD-Baseline)/TIME_STUDY) %>% inner_join(PAD_Delta_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )

# -----------------------------
# Inputs UMSARS 3 HR Supine ----------------------------------

AllMSA_Pop_Baseline_671 <- fread("Source/AllMSA_Pop_Baseline_671.txt")
AllMSA_Pop_BaselineYear1_410 <- fread("Source/AllMSA_Pop_BaselineYear1_410.txt")
AllMSA_Pop_BaselineYear1Year2_245 <- fread("Source/AllMSA_Pop_BaselineYear1Year2_245.txt")
AllMSA_Pop_BaselineYear1Year2Year3Plus_158 <- fread("Source/AllMSA_Pop_BaselineYear1Year2Year3Plus_158.txt")

EarlyCT_Pop_Baseline_319 <- fread("Source/EarlyCT_Pop_Baseline_319.txt")
EarlyCT_Pop_BaselineYear1_208 <- fread("Source/EarlyCT_Pop_BaselineYear1_208.txt")
EarlyCT_Pop_BaselineYear1Year2_134 <- fread("Source/EarlyCT_Pop_BaselineYear1Year2_134.txt")
EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 <- fread( "Source/EarlyCT_Pop_BaselineYear1Year2Year3Plus_99.txt")


dataCohorteManaged <- readRDS("Source/dataCohorteManaged.rds")

dataCohorteManaged <- dataCohorteManaged %>% group_by(NUM) %>% mutate(TIME_STUDY = ifelse( is.na(TIME_STUDY), 0, TIME_STUDY)) %>%
  mutate(Year= ifelse(TIME_STUDY==0, 0,
                      ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                             ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                    ifelse(TIME_STUDY>=2.5 ,3, NA))))) 

SBP <- dataCohorteManaged %>% 
  select(NUM, DATECONSULT, TIME_STUDY, Year, UMSARS3_ORTHO, PAS_COU, PAS_1MN:PAS_10MN, FC_COU:FC_10MN, deltaPAS)

SBP <- SBP %>% filter(!is.na(deltaPAS))
SBP <-  gather(SBP, measure_PAS, PAS_DEB, PAS_1MN:PAS_10MN)
SBP <-  SBP %>% filter(deltaPAS == PAS_DEB-PAS_COU)

SBP <- gather(SBP, measure_FC, FC_DEB, FC_1MN:FC_10MN)

SBP$measure_PAS <- substr(SBP$measure_PAS, start = 5, stop=nchar(SBP$measure_PAS))
SBP$measure_PAS <- substr(SBP$measure_PAS, start = 1, stop=nchar(SBP$measure_PAS)-2)
unique(SBP$measure_PAS)

SBP$measure_FC <- substr(SBP$measure_FC, start = 4, stop=nchar(SBP$measure_FC))
SBP$measure_FC <- substr(SBP$measure_FC, start = 1, stop=nchar(SBP$measure_FC)-2)
unique(SBP$measure_FC)

SBP <- SBP %>% filter(measure_PAS==measure_FC)

SBP <- SBP %>% select(-c(measure_PAS, measure_FC)) %>% distinct()
SBP <-  SBP %>% ungroup()
mean(SBP$PAS_COU); mean(SBP$PAS_DEB); mean(SBP$deltaPAS); 
mean(SBP$FC_COU); mean(SBP$FC_DEB); 
length(unique(SBP$NUM))
sum(is.na(SBP$FC_COU))
sum(is.na(SBP$FC_DEB))

# All we need for 
#orthos 
#PAS COU 
#PAS DEB 
#delta PAS
#FC DEB 
#FC COU

FC_COU_Baseline_Pats <- SBP %>% filter(Year==0) %>% filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM) %>% distinct()

# ----------------
# Overall MSA Entire -------------------------------

# ALL UMSARS Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(SBP) %>% filter(Year==0) %>%  
  filter(!is.na(FC_COU)) %>% 
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%   inner_join(FC_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(FC_COU),
    sd=sd(FC_COU),
    median=median(FC_COU),
    Q1 = quantile(FC_COU, 0.25),
    Q3 = quantile(FC_COU, 0.75),
    min=min(FC_COU),
    max=max(FC_COU),
    n=n()
  )



# ALL UMSARS Total Year 1

AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==1) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(FC_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(FC_COU),
    sd=sd(FC_COU),
    median=median(FC_COU),
    Q1 = quantile(FC_COU, 0.25),
    Q3 = quantile(FC_COU, 0.75),
    min=min(FC_COU),
    max=max(FC_COU),
    n=n()
  )

# Change from baseline

AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  FC_COU) %>%
  rename("Baseline"="FC_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(FC_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, FC_COU)
  ) %>%  inner_join(FC_COU_Baseline_Pats) %>%
  summarise(mean= mean((FC_COU-Baseline)/TIME_STUDY)) 



AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  FC_COU) %>%
  rename("Baseline"="FC_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(FC_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, FC_COU)
  ) %>%  inner_join(FC_COU_Baseline_Pats) %>%
  mutate(Diff= (FC_COU-Baseline)) %>% 
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )




AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  FC_COU) %>%
  rename("Baseline"="FC_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(FC_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, FC_COU)
  ) %>%  inner_join(FC_COU_Baseline_Pats) %>%
  mutate(Diff= TIME_STUDY*12) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  FC_COU) %>%
  rename("Baseline"="FC_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(FC_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, FC_COU)
  ) %>%  inner_join(FC_COU_Baseline_Pats) %>%
  mutate(Diff= (FC_COU-Baseline)/TIME_STUDY) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



# ALL UMSARS Total Year 2

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==2) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(FC_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(FC_COU),
    sd=sd(FC_COU),
    median=median(FC_COU),
    Q1 = quantile(FC_COU, 0.25),
    Q3 = quantile(FC_COU, 0.75),
    min=min(FC_COU),
    max=max(FC_COU),
    n=n()
  )


# Change from baseline

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  FC_COU) %>%
  rename("Baseline"="FC_COU") %>% 
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(FC_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, FC_COU)
  ) %>%  inner_join(FC_COU_Baseline_Pats) %>%
  summarise(mean= mean((FC_COU-Baseline)/TIME_STUDY)) 



AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  FC_COU) %>%
  rename("Baseline"="FC_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(FC_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, FC_COU)
  ) %>%  inner_join(FC_COU_Baseline_Pats) %>%
  mutate(Diff= (FC_COU-Baseline)) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  FC_COU) %>%
  rename("Baseline"="FC_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(FC_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, FC_COU)
  ) %>%  inner_join(FC_COU_Baseline_Pats) %>%
  mutate(Diff= TIME_STUDY*12) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  FC_COU) %>%
  rename("Baseline"="FC_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(FC_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, FC_COU)
  ) %>% 
  mutate(Diff= (FC_COU-Baseline)/TIME_STUDY) %>% inner_join(FC_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


# ALL UMSARS Total Year 3

AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==3) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(FC_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(FC_COU),
    sd=sd(FC_COU),
    median=median(FC_COU),
    Q1 = quantile(FC_COU, 0.25),
    Q3 = quantile(FC_COU, 0.75),
    min=min(FC_COU),
    max=max(FC_COU),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  FC_COU) %>%
  rename("Baseline"="FC_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(FC_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, FC_COU)
  ) %>%  inner_join(FC_COU_Baseline_Pats) %>%
  summarise(mean= mean((FC_COU-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  FC_COU) %>%
  rename("Baseline"="FC_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(FC_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, FC_COU)
  ) %>%  
  mutate(Diff= (FC_COU-Baseline)) %>% inner_join(FC_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  FC_COU) %>%
  rename("Baseline"="FC_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(FC_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, FC_COU)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(FC_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  FC_COU) %>%
  rename("Baseline"="FC_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(FC_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, FC_COU)
  ) %>% 
  mutate(Diff= (FC_COU-Baseline)/TIME_STUDY) %>% inner_join(FC_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )

# --------------


# By MSA P vs MSA C -----------------
# ALL UMSARS Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(FC_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(FC_COU),
    sd=sd(FC_COU),
    median=median(FC_COU),
    Q1 = quantile(FC_COU, 0.25),
    Q3 = quantile(FC_COU, 0.75),
    min=min(FC_COU),
    max=max(FC_COU),
    n=n()
  )



# ALL UMSARS Total Year 1

AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==1) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(FC_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(FC_COU),
    sd=sd(FC_COU),
    median=median(FC_COU),
    Q1 = quantile(FC_COU, 0.25),
    Q3 = quantile(FC_COU, 0.75),
    min=min(FC_COU),
    max=max(FC_COU),
    n=n()
  )

# Change from baseline

AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  FC_COU) %>%
  rename("Baseline"="FC_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(FC_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, FC_COU)
  ) %>%  inner_join(FC_COU_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((FC_COU-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  FC_COU) %>%
  rename("Baseline"="FC_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(FC_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, FC_COU)
  ) %>% 
  mutate(Diff= (FC_COU-Baseline)) %>% inner_join(FC_COU_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  FC_COU) %>%
  rename("Baseline"="FC_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(FC_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, FC_COU)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(FC_COU_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  FC_COU) %>%
  rename("Baseline"="FC_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(FC_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, FC_COU)
  ) %>% 
  mutate(Diff= (FC_COU-Baseline)/TIME_STUDY) %>% inner_join(FC_COU_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



# ALL UMSARS Total Year 2

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==2) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(FC_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(FC_COU),
    sd=sd(FC_COU),
    median=median(FC_COU),
    Q1 = quantile(FC_COU, 0.25),
    Q3 = quantile(FC_COU, 0.75),
    min=min(FC_COU),
    max=max(FC_COU),
    n=n()
  )


# Change from baseline

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  FC_COU) %>%
  rename("Baseline"="FC_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(FC_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, FC_COU)
  ) %>%  inner_join(FC_COU_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((FC_COU-Baseline)/TIME_STUDY)) 



AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  FC_COU) %>%
  rename("Baseline"="FC_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(FC_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, FC_COU)
  ) %>% 
  mutate(Diff= (FC_COU-Baseline)) %>% inner_join(FC_COU_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  FC_COU) %>%
  rename("Baseline"="FC_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(FC_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, FC_COU)
  ) %>%  inner_join(FC_COU_Baseline_Pats) %>%
  mutate(Diff= TIME_STUDY*12) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  FC_COU) %>%
  rename("Baseline"="FC_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(FC_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, FC_COU)
  ) %>% 
  mutate(Diff= (FC_COU-Baseline)/TIME_STUDY) %>% inner_join(FC_COU_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


# ALL UMSARS Total Year 3

AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==3) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(FC_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(FC_COU),
    sd=sd(FC_COU),
    median=median(FC_COU),
    Q1 = quantile(FC_COU, 0.25),
    Q3 = quantile(FC_COU, 0.75),
    min=min(FC_COU),
    max=max(FC_COU),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  FC_COU) %>%
  rename("Baseline"="FC_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(FC_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, FC_COU)
  ) %>% group_by(DIAG) %>% inner_join(FC_COU_Baseline_Pats) %>%
  summarise(mean= mean((FC_COU-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  FC_COU) %>%
  rename("Baseline"="FC_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(FC_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, FC_COU)
  ) %>%  inner_join(FC_COU_Baseline_Pats) %>%
  mutate(Diff= (FC_COU-Baseline)) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  FC_COU) %>%
  rename("Baseline"="FC_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(FC_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, FC_COU)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(FC_COU_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  FC_COU) %>%
  rename("Baseline"="FC_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(FC_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, FC_COU)
  ) %>% 
  mutate(Diff= (FC_COU-Baseline)/TIME_STUDY) %>% inner_join(FC_COU_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



# ------------
# By Possible vs probable --------------------

# ALL UMSARS Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(FC_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(FC_COU),
    sd=sd(FC_COU),
    median=median(FC_COU),
    Q1 = quantile(FC_COU, 0.25),
    Q3 = quantile(FC_COU, 0.75),
    min=min(FC_COU),
    max=max(FC_COU),
    n=n()
  )



# ALL UMSARS Total Year 1

AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==1) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(FC_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(FC_COU),
    sd=sd(FC_COU),
    median=median(FC_COU),
    Q1 = quantile(FC_COU, 0.25),
    Q3 = quantile(FC_COU, 0.75),
    min=min(FC_COU),
    max=max(FC_COU),
    n=n()
  )

# Change from baseline


AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  FC_COU) %>%
  rename("Baseline"="FC_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(FC_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, FC_COU)
  ) %>%  inner_join(FC_COU_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((FC_COU-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  FC_COU) %>%
  rename("Baseline"="FC_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(FC_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, FC_COU)
  ) %>% 
  mutate(Diff= (FC_COU-Baseline)) %>% inner_join(FC_COU_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  FC_COU) %>%
  rename("Baseline"="FC_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(FC_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, FC_COU)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(FC_COU_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  FC_COU) %>%
  rename("Baseline"="FC_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(FC_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, FC_COU)
  ) %>% 
  mutate(Diff= (FC_COU-Baseline)/TIME_STUDY) %>% inner_join(FC_COU_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



# ALL UMSARS Total Year 2

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==2) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(FC_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(FC_COU),
    sd=sd(FC_COU),
    median=median(FC_COU),
    Q1 = quantile(FC_COU, 0.25),
    Q3 = quantile(FC_COU, 0.75),
    min=min(FC_COU),
    max=max(FC_COU),
    n=n()
  )


# Change from baseline

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  FC_COU) %>%
  rename("Baseline"="FC_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(FC_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, FC_COU)
  ) %>%  inner_join(FC_COU_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((FC_COU-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  FC_COU) %>%
  rename("Baseline"="FC_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(FC_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, FC_COU)
  ) %>% 
  mutate(Diff= (FC_COU-Baseline)) %>% inner_join(FC_COU_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  FC_COU) %>%
  rename("Baseline"="FC_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(FC_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, FC_COU)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(FC_COU_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  FC_COU) %>%
  rename("Baseline"="FC_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(FC_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, FC_COU)
  ) %>% 
  mutate(Diff= (FC_COU-Baseline)/TIME_STUDY) %>% inner_join(FC_COU_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


# ALL UMSARS Total Year 3

AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==3) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(FC_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(FC_COU),
    sd=sd(FC_COU),
    median=median(FC_COU),
    Q1 = quantile(FC_COU, 0.25),
    Q3 = quantile(FC_COU, 0.75),
    min=min(FC_COU),
    max=max(FC_COU),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  FC_COU) %>%
  rename("Baseline"="FC_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(FC_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, FC_COU)
  ) %>% group_by(DIAGNIV) %>% inner_join(FC_COU_Baseline_Pats) %>%
  summarise(mean= mean((FC_COU-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  FC_COU) %>%
  rename("Baseline"="FC_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(FC_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, FC_COU)
  ) %>% 
  mutate(Diff= (FC_COU-Baseline)) %>% inner_join(FC_COU_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  FC_COU) %>%
  rename("Baseline"="FC_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(FC_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, FC_COU)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(FC_COU_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  FC_COU) %>%
  rename("Baseline"="FC_COU") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(FC_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, FC_COU)
  ) %>% 
  mutate(Diff= (FC_COU-Baseline)/TIME_STUDY) %>% inner_join(FC_COU_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )

# -----------------------------
# Overall Early CT  -------------------------------

# ALL UMSARS Total Year 0 


EarlyCT_Pop_Baseline_319 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(FC_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(FC_COU),
    sd=sd(FC_COU),
    median=median(FC_COU),
    Q1 = quantile(FC_COU, 0.25),
    Q3 = quantile(FC_COU, 0.75),
    min=min(FC_COU),
    max=max(FC_COU),
    n=n()
  )



# ALL UMSARS Total Year 1

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==1) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(FC_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(FC_COU),
    sd=sd(FC_COU),
    median=median(FC_COU),
    Q1 = quantile(FC_COU, 0.25),
    Q3 = quantile(FC_COU, 0.75),
    min=min(FC_COU),
    max=max(FC_COU),
    n=n()
  )

# Change from baseline

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  FC_COU) %>%
  rename("Baseline"="FC_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(FC_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, FC_COU)
  ) %>%  inner_join(FC_COU_Baseline_Pats) %>%
  summarise(mean= mean((FC_COU-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  FC_COU) %>%
  rename("Baseline"="FC_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(FC_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, FC_COU)
  ) %>% 
  mutate(Diff= (FC_COU-Baseline)) %>% inner_join(FC_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  FC_COU) %>%
  rename("Baseline"="FC_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(FC_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, FC_COU)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(FC_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  FC_COU) %>%
  rename("Baseline"="FC_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(FC_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, FC_COU)
  ) %>% 
  mutate(Diff= (FC_COU-Baseline)/TIME_STUDY) %>% inner_join(FC_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



# ALL UMSARS Total Year 2

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==2) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(FC_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(FC_COU),
    sd=sd(FC_COU),
    median=median(FC_COU),
    Q1 = quantile(FC_COU, 0.25),
    Q3 = quantile(FC_COU, 0.75),
    min=min(FC_COU),
    max=max(FC_COU),
    n=n()
  )


# Change from baseline

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  FC_COU) %>%
  rename("Baseline"="FC_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(FC_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, FC_COU)
  ) %>%  inner_join(FC_COU_Baseline_Pats) %>%
  summarise(mean= mean((FC_COU-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  FC_COU) %>%
  rename("Baseline"="FC_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(FC_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, FC_COU)
  ) %>% 
  mutate(Diff= (FC_COU-Baseline)) %>% inner_join(FC_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  FC_COU) %>%
  rename("Baseline"="FC_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(FC_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, FC_COU)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(FC_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  FC_COU) %>%
  rename("Baseline"="FC_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(FC_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, FC_COU)
  ) %>% 
  mutate(Diff= (FC_COU-Baseline)/TIME_STUDY) %>% inner_join(FC_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


# ALL UMSARS Total Year 3

EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==3) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(FC_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(FC_COU),
    sd=sd(FC_COU),
    median=median(FC_COU),
    Q1 = quantile(FC_COU, 0.25),
    Q3 = quantile(FC_COU, 0.75),
    min=min(FC_COU),
    max=max(FC_COU),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  FC_COU) %>%
  rename("Baseline"="FC_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(FC_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, FC_COU)
  ) %>%  inner_join(FC_COU_Baseline_Pats) %>%
  summarise(mean= mean((FC_COU-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  FC_COU) %>%
  rename("Baseline"="FC_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(FC_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, FC_COU)
  ) %>% 
  mutate(Diff= (FC_COU-Baseline)) %>% inner_join(FC_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  FC_COU) %>%
  rename("Baseline"="FC_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(FC_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, FC_COU)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(FC_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  FC_COU) %>%
  rename("Baseline"="FC_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(FC_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, FC_COU)
  ) %>% 
  mutate(Diff= (FC_COU-Baseline)/TIME_STUDY) %>% inner_join(FC_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )

# --------------


# By MSA P vs MSA C -----------------
# ALL UMSARS Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(FC_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(FC_COU),
    sd=sd(FC_COU),
    median=median(FC_COU),
    Q1 = quantile(FC_COU, 0.25),
    Q3 = quantile(FC_COU, 0.75),
    min=min(FC_COU),
    max=max(FC_COU),
    n=n()
  )



# ALL UMSARS Total Year 1

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==1) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(FC_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(FC_COU),
    sd=sd(FC_COU),
    median=median(FC_COU),
    Q1 = quantile(FC_COU, 0.25),
    Q3 = quantile(FC_COU, 0.75),
    min=min(FC_COU),
    max=max(FC_COU),
    n=n()
  )

# Change from baseline

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  FC_COU) %>%
  rename("Baseline"="FC_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(FC_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, FC_COU)
  ) %>%  inner_join(FC_COU_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((FC_COU-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  FC_COU) %>%
  rename("Baseline"="FC_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(FC_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, FC_COU)
  ) %>% 
  mutate(Diff= (FC_COU-Baseline)) %>% inner_join(FC_COU_Baseline_Pats) %>%
  group_by(DIAG) %>% 
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  FC_COU) %>%
  rename("Baseline"="FC_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(FC_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, FC_COU)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(FC_COU_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  FC_COU) %>%
  rename("Baseline"="FC_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(FC_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, FC_COU)
  ) %>% 
  mutate(Diff= (FC_COU-Baseline)/TIME_STUDY) %>% inner_join(FC_COU_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



# ALL UMSARS Total Year 2

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==2) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(FC_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(FC_COU),
    sd=sd(FC_COU),
    median=median(FC_COU),
    Q1 = quantile(FC_COU, 0.25),
    Q3 = quantile(FC_COU, 0.75),
    min=min(FC_COU),
    max=max(FC_COU),
    n=n()
  )


# Change from baseline

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  FC_COU) %>%
  rename("Baseline"="FC_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(FC_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, FC_COU)
  ) %>% inner_join(FC_COU_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((FC_COU-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  FC_COU) %>%
  rename("Baseline"="FC_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(FC_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, FC_COU)
  ) %>% 
  mutate(Diff= (FC_COU-Baseline)) %>% inner_join(FC_COU_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  FC_COU) %>%
  rename("Baseline"="FC_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(FC_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, FC_COU)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(FC_COU_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  FC_COU) %>%
  rename("Baseline"="FC_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(FC_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, FC_COU)
  ) %>% 
  mutate(Diff= (FC_COU-Baseline)/TIME_STUDY) %>% inner_join(FC_COU_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


# ALL UMSARS Total Year 3

EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==3) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(FC_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(FC_COU),
    sd=sd(FC_COU),
    median=median(FC_COU),
    Q1 = quantile(FC_COU, 0.25),
    Q3 = quantile(FC_COU, 0.75),
    min=min(FC_COU),
    max=max(FC_COU),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  FC_COU) %>%
  rename("Baseline"="FC_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(FC_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, FC_COU)
  ) %>% group_by(DIAG) %>% inner_join(FC_COU_Baseline_Pats) %>%
  summarise(mean= mean((FC_COU-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  FC_COU) %>%
  rename("Baseline"="FC_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(FC_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, FC_COU)
  ) %>% 
  mutate(Diff= (FC_COU-Baseline)) %>% inner_join(FC_COU_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  FC_COU) %>%
  rename("Baseline"="FC_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(FC_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, FC_COU)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(FC_COU_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  FC_COU) %>%
  rename("Baseline"="FC_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(FC_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, FC_COU)
  ) %>% 
  mutate(Diff= (FC_COU-Baseline)/TIME_STUDY) %>% inner_join(FC_COU_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



# ------------
# By Possible vs probable --------------------

# ALL UMSARS Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(FC_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(FC_COU),
    sd=sd(FC_COU),
    median=median(FC_COU),
    Q1 = quantile(FC_COU, 0.25),
    Q3 = quantile(FC_COU, 0.75),
    min=min(FC_COU),
    max=max(FC_COU),
    n=n()
  )



# ALL UMSARS Total Year 1

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==1) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(FC_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(FC_COU),
    sd=sd(FC_COU),
    median=median(FC_COU),
    Q1 = quantile(FC_COU, 0.25),
    Q3 = quantile(FC_COU, 0.75),
    min=min(FC_COU),
    max=max(FC_COU),
    n=n()
  )

# Change from baseline


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  FC_COU) %>%
  rename("Baseline"="FC_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(FC_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, FC_COU)
  ) %>%  inner_join(FC_COU_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((FC_COU-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  FC_COU) %>%
  rename("Baseline"="FC_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(FC_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, FC_COU)
  ) %>% 
  mutate(Diff= (FC_COU-Baseline)) %>% inner_join(FC_COU_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )




EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  FC_COU) %>%
  rename("Baseline"="FC_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(FC_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, FC_COU)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(FC_COU_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )




EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  FC_COU) %>%
  rename("Baseline"="FC_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(FC_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, FC_COU)
  ) %>% 
  mutate(Diff= (FC_COU-Baseline)/TIME_STUDY) %>% inner_join(FC_COU_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



# ALL UMSARS Total Year 2

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==2) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(FC_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(FC_COU),
    sd=sd(FC_COU),
    median=median(FC_COU),
    Q1 = quantile(FC_COU, 0.25),
    Q3 = quantile(FC_COU, 0.75),
    min=min(FC_COU),
    max=max(FC_COU),
    n=n()
  )


# Change from baseline

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  FC_COU) %>%
  rename("Baseline"="FC_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(FC_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, FC_COU)
  ) %>%  inner_join(FC_COU_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((FC_COU-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  FC_COU) %>%
  rename("Baseline"="FC_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(FC_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, FC_COU)
  ) %>% 
  mutate(Diff= (FC_COU-Baseline)) %>% inner_join(FC_COU_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  FC_COU) %>%
  rename("Baseline"="FC_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(FC_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, FC_COU)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(FC_COU_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  FC_COU) %>%
  rename("Baseline"="FC_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(FC_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, FC_COU)
  ) %>% 
  mutate(Diff= (FC_COU-Baseline)/TIME_STUDY) %>% inner_join(FC_COU_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


# ALL UMSARS Total Year 3

EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==3) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(FC_COU_Baseline_Pats) %>%
  summarise(
    mean=mean(FC_COU),
    sd=sd(FC_COU),
    median=median(FC_COU),
    Q1 = quantile(FC_COU, 0.25),
    Q3 = quantile(FC_COU, 0.75),
    min=min(FC_COU),
    max=max(FC_COU),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  FC_COU) %>%
  rename("Baseline"="FC_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(FC_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, FC_COU)
  ) %>% group_by(DIAGNIV) %>% inner_join(FC_COU_Baseline_Pats) %>%
  summarise(mean= mean((FC_COU-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  FC_COU) %>%
  rename("Baseline"="FC_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(FC_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, FC_COU)
  ) %>% 
  mutate(Diff= (FC_COU-Baseline)) %>% inner_join(FC_COU_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% 
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  FC_COU) %>%
  rename("Baseline"="FC_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(FC_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, FC_COU)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(FC_COU_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(FC_COU)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  FC_COU) %>%
  rename("Baseline"="FC_COU") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(FC_COU)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, FC_COU)
  ) %>% 
  mutate(Diff= (FC_COU-Baseline)/TIME_STUDY) %>% inner_join(FC_COU_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )

# -----------------------------