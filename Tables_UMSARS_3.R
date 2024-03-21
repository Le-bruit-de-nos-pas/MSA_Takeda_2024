
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

# All we need for 
#orthos 
#PAS COU 
#PAS DEB 
#delta PAS

# ----------------
# Overall MSA Entire -------------------------------

# ALL UMSARS Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
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
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
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
  left_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
  summarise(mean= mean((PAS_DEB-Baseline)/TIME_STUDY)) 



AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  left_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
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
  left_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
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
  left_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
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
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
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
  left_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
  summarise(mean= mean((PAS_DEB-Baseline)/TIME_STUDY)) 



AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  left_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
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
  left_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
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
  left_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
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


# ALL UMSARS Total Year 3

AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==3) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
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
  left_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
  summarise(mean= mean((PAS_DEB-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  left_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
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


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  left_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
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


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  left_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
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

# --------------


# By MSA P vs MSA C -----------------
# ALL UMSARS Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
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
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
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
  left_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
  group_by(DIAG) %>% summarise(mean= mean((PAS_DEB-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  left_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
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



AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  left_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
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



AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  left_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
  mutate(Diff= (PAS_DEB-Baseline)/TIME_STUDY) %>%
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
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
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
  left_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
  group_by(DIAG) %>% summarise(mean= mean((PAS_DEB-Baseline)/TIME_STUDY)) 



AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  left_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
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



AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  left_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
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
  left_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
  mutate(Diff= (PAS_DEB-Baseline)/TIME_STUDY) %>%
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
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
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
  left_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% group_by(DIAG) %>%
  summarise(mean= mean((PAS_DEB-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  left_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
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
  left_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
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


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  left_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
  mutate(Diff= (PAS_DEB-Baseline)/TIME_STUDY) %>%
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
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
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
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
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
  left_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
  group_by(DIAGNIV) %>% summarise(mean= mean((PAS_DEB-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  left_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
  mutate(Diff= (PAS_DEB-Baseline)) %>%
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
  left_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>%
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
  left_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
  mutate(Diff= (PAS_DEB-Baseline)/TIME_STUDY) %>%
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
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
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
  left_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
  group_by(DIAGNIV) %>% summarise(mean= mean((PAS_DEB-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  left_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
  mutate(Diff= (PAS_DEB-Baseline)) %>%
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
  left_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>%
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
  left_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
  mutate(Diff= (PAS_DEB-Baseline)/TIME_STUDY) %>%
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
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
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
  left_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% group_by(DIAGNIV) %>%
  summarise(mean= mean((PAS_DEB-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  left_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
  mutate(Diff= (PAS_DEB-Baseline)) %>%
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
  left_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>%
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
  left_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
  mutate(Diff= (PAS_DEB-Baseline)/TIME_STUDY) %>%
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
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
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
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
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
  left_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
  summarise(mean= mean((PAS_DEB-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  left_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
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


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  left_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
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


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  left_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
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

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==2) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
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
  left_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
  summarise(mean= mean((PAS_DEB-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  left_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
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



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  left_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
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


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  left_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
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


# ALL UMSARS Total Year 3

EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==3) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
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
  left_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
  summarise(mean= mean((PAS_DEB-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  left_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
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



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  left_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
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



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  left_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
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

# --------------


# By MSA P vs MSA C -----------------
# ALL UMSARS Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
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
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
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
  left_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
  group_by(DIAG) %>% summarise(mean= mean((PAS_DEB-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  left_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
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



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  left_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
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



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  left_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
  mutate(Diff= (PAS_DEB-Baseline)/TIME_STUDY) %>%
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
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
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
  left_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
  group_by(DIAG) %>% summarise(mean= mean((PAS_DEB-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  left_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
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


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  left_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
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


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  left_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
  mutate(Diff= (PAS_DEB-Baseline)/TIME_STUDY) %>%
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
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
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
  left_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% group_by(DIAG) %>%
  summarise(mean= mean((PAS_DEB-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  left_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
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



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  left_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
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



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  left_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
  mutate(Diff= (PAS_DEB-Baseline)/TIME_STUDY) %>%
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
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
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
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
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
  left_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
  group_by(DIAGNIV) %>% summarise(mean= mean((PAS_DEB-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  left_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
  mutate(Diff= (PAS_DEB-Baseline)) %>%
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
  left_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>%
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
  left_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SBP) %>% filter(Year==1) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
  mutate(Diff= (PAS_DEB-Baseline)/TIME_STUDY) %>%
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
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
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
  left_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
  group_by(DIAGNIV) %>% summarise(mean= mean((PAS_DEB-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  left_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
  mutate(Diff= (PAS_DEB-Baseline)) %>%
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
  left_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>%
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
  left_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SBP) %>% filter(Year==2) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
  mutate(Diff= (PAS_DEB-Baseline)/TIME_STUDY) %>%
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
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
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
  left_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% group_by(DIAGNIV) %>%
  summarise(mean= mean((PAS_DEB-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==0) %>%
  filter(!is.na(PAS_DEB)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  PAS_DEB) %>%
  rename("Baseline"="PAS_DEB") %>%
  left_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
  mutate(Diff= (PAS_DEB-Baseline)) %>%
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
  left_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>%
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
  left_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SBP) %>% filter(Year==3) %>%
      filter(!is.na(PAS_DEB)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, PAS_DEB)
  ) %>% 
  mutate(Diff= (PAS_DEB-Baseline)/TIME_STUDY) %>%
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