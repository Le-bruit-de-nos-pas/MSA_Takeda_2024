

library(tidyverse)
library(data.table)
options(scipen = 999)

# Inputs SCOPA AUT ----------------------------------

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


SCOPA <- dataCohorteManaged %>% 
  select(NUM, DATECONSULT, TIME_STUDY, Year, SCOPA_scoretot, SCOPA_nbdm)

SCOPA <- SCOPA %>% filter(SCOPA_scoretot!=0) %>% 
  mutate(factor=SCOPA_nbdm/25) %>% 
  mutate(factor=1-factor) %>%
  mutate(SCOPA_scoretot=SCOPA_scoretot/factor)

SCOPA_Baseline_Pats <- SCOPA %>% filter(Year==0) %>% filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM) %>% distinct() %>%
  inner_join(AllMSA_Pop_Baseline_671)


# ----------------
# Overall MSA Entire -------------------------------

# ALL SCOPA Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(SCOPA) %>% filter(Year==0) %>% 
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_scoretot),
    sd=sd(SCOPA_scoretot),
    median=median(SCOPA_scoretot),
    Q1 = quantile(SCOPA_scoretot, 0.25),
    Q3 = quantile(SCOPA_scoretot, 0.75),
    min=min(SCOPA_scoretot),
    max=max(SCOPA_scoretot),
    n=n()
  )



# ALL SCOPA Total Year 1

AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_scoretot),
    sd=sd(SCOPA_scoretot),
    median=median(SCOPA_scoretot),
    Q1 = quantile(SCOPA_scoretot, 0.25),
    Q3 = quantile(SCOPA_scoretot, 0.75),
    min=min(SCOPA_scoretot),
    max=max(SCOPA_scoretot),
    n=n()
  )

# Change from baseline

AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_scoretot) %>%
  rename("Baseline"="SCOPA_scoretot") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_scoretot)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_scoretot)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  summarise(mean= mean((SCOPA_scoretot-Baseline)/TIME_STUDY)) 



AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_scoretot) %>%
  rename("Baseline"="SCOPA_scoretot") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_scoretot)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_scoretot)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  mutate(Diff= (SCOPA_scoretot-Baseline)) %>% 
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )




AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_scoretot) %>%
  rename("Baseline"="SCOPA_scoretot") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_scoretot)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_scoretot)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
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



AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_scoretot) %>%
  rename("Baseline"="SCOPA_scoretot") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_scoretot)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_scoretot)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  mutate(Diff= (SCOPA_scoretot-Baseline)/TIME_STUDY) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



# ALL SCOPA Total Year 2

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_scoretot),
    sd=sd(SCOPA_scoretot),
    median=median(SCOPA_scoretot),
    Q1 = quantile(SCOPA_scoretot, 0.25),
    Q3 = quantile(SCOPA_scoretot, 0.75),
    min=min(SCOPA_scoretot),
    max=max(SCOPA_scoretot),
    n=n()
  )


# Change from baseline

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_scoretot) %>%
  rename("Baseline"="SCOPA_scoretot") %>% 
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_scoretot)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_scoretot)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  summarise(mean= mean((SCOPA_scoretot-Baseline)/TIME_STUDY)) 



AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_scoretot) %>%
  rename("Baseline"="SCOPA_scoretot") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_scoretot)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_scoretot)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  mutate(Diff= (SCOPA_scoretot-Baseline)) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_scoretot) %>%
  rename("Baseline"="SCOPA_scoretot") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_scoretot)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_scoretot)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
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


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_scoretot) %>%
  rename("Baseline"="SCOPA_scoretot") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_scoretot)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_scoretot)
  ) %>% 
  mutate(Diff= (SCOPA_scoretot-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


# ALL SCOPA Total Year 3

AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_scoretot),
    sd=sd(SCOPA_scoretot),
    median=median(SCOPA_scoretot),
    Q1 = quantile(SCOPA_scoretot, 0.25),
    Q3 = quantile(SCOPA_scoretot, 0.75),
    min=min(SCOPA_scoretot),
    max=max(SCOPA_scoretot),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_scoretot) %>%
  rename("Baseline"="SCOPA_scoretot") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_scoretot)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_scoretot)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  summarise(mean= mean((SCOPA_scoretot-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_scoretot) %>%
  rename("Baseline"="SCOPA_scoretot") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_scoretot)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_scoretot)
  ) %>%  
  mutate(Diff= (SCOPA_scoretot-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_scoretot) %>%
  rename("Baseline"="SCOPA_scoretot") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_scoretot)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_scoretot)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_scoretot) %>%
  rename("Baseline"="SCOPA_scoretot") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_scoretot)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_scoretot)
  ) %>% 
  mutate(Diff= (SCOPA_scoretot-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
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
# ALL SCOPA Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_scoretot),
    sd=sd(SCOPA_scoretot),
    median=median(SCOPA_scoretot),
    Q1 = quantile(SCOPA_scoretot, 0.25),
    Q3 = quantile(SCOPA_scoretot, 0.75),
    min=min(SCOPA_scoretot),
    max=max(SCOPA_scoretot),
    n=n()
  )



# ALL SCOPA Total Year 1

AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_scoretot),
    sd=sd(SCOPA_scoretot),
    median=median(SCOPA_scoretot),
    Q1 = quantile(SCOPA_scoretot, 0.25),
    Q3 = quantile(SCOPA_scoretot, 0.75),
    min=min(SCOPA_scoretot),
    max=max(SCOPA_scoretot),
    n=n()
  )

# Change from baseline

AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_scoretot) %>%
  rename("Baseline"="SCOPA_scoretot") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_scoretot)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_scoretot)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((SCOPA_scoretot-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_scoretot) %>%
  rename("Baseline"="SCOPA_scoretot") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_scoretot)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_scoretot)
  ) %>% 
  mutate(Diff= (SCOPA_scoretot-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
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



AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_scoretot) %>%
  rename("Baseline"="SCOPA_scoretot") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_scoretot)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_scoretot)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(SCOPA_Baseline_Pats) %>%
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



AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_scoretot) %>%
  rename("Baseline"="SCOPA_scoretot") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_scoretot)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_scoretot)
  ) %>% 
  mutate(Diff= (SCOPA_scoretot-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
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



# ALL SCOPA Total Year 2

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_scoretot),
    sd=sd(SCOPA_scoretot),
    median=median(SCOPA_scoretot),
    Q1 = quantile(SCOPA_scoretot, 0.25),
    Q3 = quantile(SCOPA_scoretot, 0.75),
    min=min(SCOPA_scoretot),
    max=max(SCOPA_scoretot),
    n=n()
  )


# Change from baseline

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_scoretot) %>%
  rename("Baseline"="SCOPA_scoretot") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_scoretot)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_scoretot)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((SCOPA_scoretot-Baseline)/TIME_STUDY)) 



AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_scoretot) %>%
  rename("Baseline"="SCOPA_scoretot") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_scoretot)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_scoretot)
  ) %>% 
  mutate(Diff= (SCOPA_scoretot-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
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



AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_scoretot) %>%
  rename("Baseline"="SCOPA_scoretot") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_scoretot)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_scoretot)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
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


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_scoretot) %>%
  rename("Baseline"="SCOPA_scoretot") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_scoretot)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_scoretot)
  ) %>% 
  mutate(Diff= (SCOPA_scoretot-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
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


# ALL SCOPA Total Year 3

AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_scoretot),
    sd=sd(SCOPA_scoretot),
    median=median(SCOPA_scoretot),
    Q1 = quantile(SCOPA_scoretot, 0.25),
    Q3 = quantile(SCOPA_scoretot, 0.75),
    min=min(SCOPA_scoretot),
    max=max(SCOPA_scoretot),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_scoretot) %>%
  rename("Baseline"="SCOPA_scoretot") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_scoretot)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_scoretot)
  ) %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(mean= mean((SCOPA_scoretot-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_scoretot) %>%
  rename("Baseline"="SCOPA_scoretot") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_scoretot)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_scoretot)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  mutate(Diff= (SCOPA_scoretot-Baseline)) %>%
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


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_scoretot) %>%
  rename("Baseline"="SCOPA_scoretot") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_scoretot)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_scoretot)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(SCOPA_Baseline_Pats) %>%
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


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_scoretot) %>%
  rename("Baseline"="SCOPA_scoretot") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_scoretot)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_scoretot)
  ) %>% 
  mutate(Diff= (SCOPA_scoretot-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
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

# ALL SCOPA Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_scoretot),
    sd=sd(SCOPA_scoretot),
    median=median(SCOPA_scoretot),
    Q1 = quantile(SCOPA_scoretot, 0.25),
    Q3 = quantile(SCOPA_scoretot, 0.75),
    min=min(SCOPA_scoretot),
    max=max(SCOPA_scoretot),
    n=n()
  )



# ALL SCOPA Total Year 1

AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_scoretot),
    sd=sd(SCOPA_scoretot),
    median=median(SCOPA_scoretot),
    Q1 = quantile(SCOPA_scoretot, 0.25),
    Q3 = quantile(SCOPA_scoretot, 0.75),
    min=min(SCOPA_scoretot),
    max=max(SCOPA_scoretot),
    n=n()
  )

# Change from baseline


AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_scoretot) %>%
  rename("Baseline"="SCOPA_scoretot") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_scoretot)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_scoretot)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((SCOPA_scoretot-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_scoretot) %>%
  rename("Baseline"="SCOPA_scoretot") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_scoretot)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_scoretot)
  ) %>% 
  mutate(Diff= (SCOPA_scoretot-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
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


AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_scoretot) %>%
  rename("Baseline"="SCOPA_scoretot") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_scoretot)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_scoretot)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(SCOPA_Baseline_Pats) %>%
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


AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_scoretot) %>%
  rename("Baseline"="SCOPA_scoretot") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_scoretot)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_scoretot)
  ) %>% 
  mutate(Diff= (SCOPA_scoretot-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
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



# ALL SCOPA Total Year 2

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_scoretot),
    sd=sd(SCOPA_scoretot),
    median=median(SCOPA_scoretot),
    Q1 = quantile(SCOPA_scoretot, 0.25),
    Q3 = quantile(SCOPA_scoretot, 0.75),
    min=min(SCOPA_scoretot),
    max=max(SCOPA_scoretot),
    n=n()
  )


# Change from baseline

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_scoretot) %>%
  rename("Baseline"="SCOPA_scoretot") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_scoretot)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_scoretot)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((SCOPA_scoretot-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_scoretot) %>%
  rename("Baseline"="SCOPA_scoretot") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_scoretot)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_scoretot)
  ) %>% 
  mutate(Diff= (SCOPA_scoretot-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
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


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_scoretot) %>%
  rename("Baseline"="SCOPA_scoretot") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_scoretot)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_scoretot)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(SCOPA_Baseline_Pats) %>%
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


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_scoretot) %>%
  rename("Baseline"="SCOPA_scoretot") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_scoretot)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_scoretot)
  ) %>% 
  mutate(Diff= (SCOPA_scoretot-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
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


# ALL SCOPA Total Year 3

AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_scoretot),
    sd=sd(SCOPA_scoretot),
    median=median(SCOPA_scoretot),
    Q1 = quantile(SCOPA_scoretot, 0.25),
    Q3 = quantile(SCOPA_scoretot, 0.75),
    min=min(SCOPA_scoretot),
    max=max(SCOPA_scoretot),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_scoretot) %>%
  rename("Baseline"="SCOPA_scoretot") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_scoretot)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_scoretot)
  ) %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(mean= mean((SCOPA_scoretot-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_scoretot) %>%
  rename("Baseline"="SCOPA_scoretot") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_scoretot)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_scoretot)
  ) %>% 
  mutate(Diff= (SCOPA_scoretot-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
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


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_scoretot) %>%
  rename("Baseline"="SCOPA_scoretot") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_scoretot)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_scoretot)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(SCOPA_Baseline_Pats) %>%
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



AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_scoretot) %>%
  rename("Baseline"="SCOPA_scoretot") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_scoretot)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_scoretot)
  ) %>% 
  mutate(Diff= (SCOPA_scoretot-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
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

# ALL SCOPA Total Year 0 


EarlyCT_Pop_Baseline_319 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_scoretot),
    sd=sd(SCOPA_scoretot),
    median=median(SCOPA_scoretot),
    Q1 = quantile(SCOPA_scoretot, 0.25),
    Q3 = quantile(SCOPA_scoretot, 0.75),
    min=min(SCOPA_scoretot),
    max=max(SCOPA_scoretot),
    n=n()
  )



# ALL SCOPA Total Year 1

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_scoretot),
    sd=sd(SCOPA_scoretot),
    median=median(SCOPA_scoretot),
    Q1 = quantile(SCOPA_scoretot, 0.25),
    Q3 = quantile(SCOPA_scoretot, 0.75),
    min=min(SCOPA_scoretot),
    max=max(SCOPA_scoretot),
    n=n()
  )

# Change from baseline

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_scoretot) %>%
  rename("Baseline"="SCOPA_scoretot") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_scoretot)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_scoretot)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  summarise(mean= mean((SCOPA_scoretot-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_scoretot) %>%
  rename("Baseline"="SCOPA_scoretot") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_scoretot)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_scoretot)
  ) %>% 
  mutate(Diff= (SCOPA_scoretot-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_scoretot) %>%
  rename("Baseline"="SCOPA_scoretot") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_scoretot)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_scoretot)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_scoretot) %>%
  rename("Baseline"="SCOPA_scoretot") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_scoretot)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_scoretot)
  ) %>% 
  mutate(Diff= (SCOPA_scoretot-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



# ALL SCOPA Total Year 2

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_scoretot),
    sd=sd(SCOPA_scoretot),
    median=median(SCOPA_scoretot),
    Q1 = quantile(SCOPA_scoretot, 0.25),
    Q3 = quantile(SCOPA_scoretot, 0.75),
    min=min(SCOPA_scoretot),
    max=max(SCOPA_scoretot),
    n=n()
  )


# Change from baseline

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_scoretot) %>%
  rename("Baseline"="SCOPA_scoretot") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_scoretot)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_scoretot)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  summarise(mean= mean((SCOPA_scoretot-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_scoretot) %>%
  rename("Baseline"="SCOPA_scoretot") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_scoretot)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_scoretot)
  ) %>% 
  mutate(Diff= (SCOPA_scoretot-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_scoretot) %>%
  rename("Baseline"="SCOPA_scoretot") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_scoretot)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_scoretot)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_scoretot) %>%
  rename("Baseline"="SCOPA_scoretot") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_scoretot)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_scoretot)
  ) %>% 
  mutate(Diff= (SCOPA_scoretot-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


# ALL SCOPA Total Year 3

EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_scoretot),
    sd=sd(SCOPA_scoretot),
    median=median(SCOPA_scoretot),
    Q1 = quantile(SCOPA_scoretot, 0.25),
    Q3 = quantile(SCOPA_scoretot, 0.75),
    min=min(SCOPA_scoretot),
    max=max(SCOPA_scoretot),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_scoretot) %>%
  rename("Baseline"="SCOPA_scoretot") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_scoretot)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_scoretot)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  summarise(mean= mean((SCOPA_scoretot-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_scoretot) %>%
  rename("Baseline"="SCOPA_scoretot") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_scoretot)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_scoretot)
  ) %>% 
  mutate(Diff= (SCOPA_scoretot-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_scoretot) %>%
  rename("Baseline"="SCOPA_scoretot") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_scoretot)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_scoretot)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_scoretot) %>%
  rename("Baseline"="SCOPA_scoretot") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_scoretot)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_scoretot)
  ) %>% 
  mutate(Diff= (SCOPA_scoretot-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
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
# ALL SCOPA Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_scoretot),
    sd=sd(SCOPA_scoretot),
    median=median(SCOPA_scoretot),
    Q1 = quantile(SCOPA_scoretot, 0.25),
    Q3 = quantile(SCOPA_scoretot, 0.75),
    min=min(SCOPA_scoretot),
    max=max(SCOPA_scoretot),
    n=n()
  )



# ALL SCOPA Total Year 1

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_scoretot),
    sd=sd(SCOPA_scoretot),
    median=median(SCOPA_scoretot),
    Q1 = quantile(SCOPA_scoretot, 0.25),
    Q3 = quantile(SCOPA_scoretot, 0.75),
    min=min(SCOPA_scoretot),
    max=max(SCOPA_scoretot),
    n=n()
  )

# Change from baseline

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_scoretot) %>%
  rename("Baseline"="SCOPA_scoretot") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_scoretot)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_scoretot)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((SCOPA_scoretot-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_scoretot) %>%
  rename("Baseline"="SCOPA_scoretot") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_scoretot)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_scoretot)
  ) %>% 
  mutate(Diff= (SCOPA_scoretot-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
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



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_scoretot) %>%
  rename("Baseline"="SCOPA_scoretot") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_scoretot)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_scoretot)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(SCOPA_Baseline_Pats) %>%
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



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_scoretot) %>%
  rename("Baseline"="SCOPA_scoretot") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_scoretot)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_scoretot)
  ) %>% 
  mutate(Diff= (SCOPA_scoretot-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
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



# ALL SCOPA Total Year 2

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_scoretot),
    sd=sd(SCOPA_scoretot),
    median=median(SCOPA_scoretot),
    Q1 = quantile(SCOPA_scoretot, 0.25),
    Q3 = quantile(SCOPA_scoretot, 0.75),
    min=min(SCOPA_scoretot),
    max=max(SCOPA_scoretot),
    n=n()
  )


# Change from baseline

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_scoretot) %>%
  rename("Baseline"="SCOPA_scoretot") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_scoretot)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_scoretot)
  ) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((SCOPA_scoretot-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_scoretot) %>%
  rename("Baseline"="SCOPA_scoretot") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_scoretot)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_scoretot)
  ) %>% 
  mutate(Diff= (SCOPA_scoretot-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
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


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_scoretot) %>%
  rename("Baseline"="SCOPA_scoretot") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_scoretot)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_scoretot)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(SCOPA_Baseline_Pats) %>%
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


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_scoretot) %>%
  rename("Baseline"="SCOPA_scoretot") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_scoretot)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_scoretot)
  ) %>% 
  mutate(Diff= (SCOPA_scoretot-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
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


# ALL SCOPA Total Year 3

EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_scoretot),
    sd=sd(SCOPA_scoretot),
    median=median(SCOPA_scoretot),
    Q1 = quantile(SCOPA_scoretot, 0.25),
    Q3 = quantile(SCOPA_scoretot, 0.75),
    min=min(SCOPA_scoretot),
    max=max(SCOPA_scoretot),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_scoretot) %>%
  rename("Baseline"="SCOPA_scoretot") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_scoretot)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_scoretot)
  ) %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(mean= mean((SCOPA_scoretot-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_scoretot) %>%
  rename("Baseline"="SCOPA_scoretot") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_scoretot)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_scoretot)
  ) %>% 
  mutate(Diff= (SCOPA_scoretot-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
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



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_scoretot) %>%
  rename("Baseline"="SCOPA_scoretot") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_scoretot)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_scoretot)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(SCOPA_Baseline_Pats) %>%
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



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_scoretot) %>%
  rename("Baseline"="SCOPA_scoretot") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_scoretot)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_scoretot)
  ) %>% 
  mutate(Diff= (SCOPA_scoretot-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
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

# ALL SCOPA Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_scoretot),
    sd=sd(SCOPA_scoretot),
    median=median(SCOPA_scoretot),
    Q1 = quantile(SCOPA_scoretot, 0.25),
    Q3 = quantile(SCOPA_scoretot, 0.75),
    min=min(SCOPA_scoretot),
    max=max(SCOPA_scoretot),
    n=n()
  )



# ALL SCOPA Total Year 1

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_scoretot),
    sd=sd(SCOPA_scoretot),
    median=median(SCOPA_scoretot),
    Q1 = quantile(SCOPA_scoretot, 0.25),
    Q3 = quantile(SCOPA_scoretot, 0.75),
    min=min(SCOPA_scoretot),
    max=max(SCOPA_scoretot),
    n=n()
  )

# Change from baseline


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_scoretot) %>%
  rename("Baseline"="SCOPA_scoretot") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_scoretot)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_scoretot)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((SCOPA_scoretot-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_scoretot) %>%
  rename("Baseline"="SCOPA_scoretot") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_scoretot)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_scoretot)
  ) %>% 
  mutate(Diff= (SCOPA_scoretot-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
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




EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_scoretot) %>%
  rename("Baseline"="SCOPA_scoretot") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_scoretot)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_scoretot)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(SCOPA_Baseline_Pats) %>%
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




EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_scoretot) %>%
  rename("Baseline"="SCOPA_scoretot") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_scoretot)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_scoretot)
  ) %>% 
  mutate(Diff= (SCOPA_scoretot-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
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



# ALL SCOPA Total Year 2

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_scoretot),
    sd=sd(SCOPA_scoretot),
    median=median(SCOPA_scoretot),
    Q1 = quantile(SCOPA_scoretot, 0.25),
    Q3 = quantile(SCOPA_scoretot, 0.75),
    min=min(SCOPA_scoretot),
    max=max(SCOPA_scoretot),
    n=n()
  )


# Change from baseline

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_scoretot) %>%
  rename("Baseline"="SCOPA_scoretot") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_scoretot)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_scoretot)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((SCOPA_scoretot-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_scoretot) %>%
  rename("Baseline"="SCOPA_scoretot") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_scoretot)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_scoretot)
  ) %>% 
  mutate(Diff= (SCOPA_scoretot-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
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



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_scoretot) %>%
  rename("Baseline"="SCOPA_scoretot") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_scoretot)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_scoretot)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(SCOPA_Baseline_Pats) %>%
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



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_scoretot) %>%
  rename("Baseline"="SCOPA_scoretot") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_scoretot)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_scoretot)
  ) %>% 
  mutate(Diff= (SCOPA_scoretot-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
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


# ALL SCOPA Total Year 3

EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_scoretot),
    sd=sd(SCOPA_scoretot),
    median=median(SCOPA_scoretot),
    Q1 = quantile(SCOPA_scoretot, 0.25),
    Q3 = quantile(SCOPA_scoretot, 0.75),
    min=min(SCOPA_scoretot),
    max=max(SCOPA_scoretot),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_scoretot) %>%
  rename("Baseline"="SCOPA_scoretot") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_scoretot)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_scoretot)
  ) %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(mean= mean((SCOPA_scoretot-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_scoretot) %>%
  rename("Baseline"="SCOPA_scoretot") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_scoretot)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_scoretot)
  ) %>% 
  mutate(Diff= (SCOPA_scoretot-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
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



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_scoretot) %>%
  rename("Baseline"="SCOPA_scoretot") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_scoretot)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_scoretot)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(SCOPA_Baseline_Pats) %>%
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



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_scoretot) %>%
  rename("Baseline"="SCOPA_scoretot") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_scoretot)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_scoretot)
  ) %>% 
  mutate(Diff= (SCOPA_scoretot-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
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


# Student T-tests for deltas in SCOPA AUT Prob vs Poss, MSA-C vs MSA-P ---------------------------------------
library(rstatix)

# Inputs SCOPA Total ----------------------------------

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


SCOPA <- dataCohorteManaged %>% 
  select(NUM, DATECONSULT, TIME_STUDY, Year, SCOPA_scoretot, SCOPA_nbdm)

SCOPA <- SCOPA %>% filter(SCOPA_scoretot!=0) %>% 
  mutate(factor=SCOPA_nbdm/25) %>% 
  mutate(factor=1-factor) %>%
  mutate(SCOPA_scoretot=SCOPA_scoretot/factor)

SCOPA_Baseline_Pats <- SCOPA %>% filter(Year==0) %>% filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM) %>% distinct() %>%
  inner_join(AllMSA_Pop_Baseline_671)


# ----------------
# Overall MSA Entire -------------------------------

# ALL UMSARS Total Year 1 

AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==0) %>% inner_join(SCOPA_Baseline_Pats) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_scoretot) %>%
  rename("Baseline"="SCOPA_scoretot") %>%
  left_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_scoretot)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_scoretot)
  ) %>% 
  mutate(Diff= (SCOPA_scoretot-Baseline))  %>%
  #t_test(Diff~DIAG, detailed = F, var.equal = TRUE) 
  t_test(Diff~DIAGNIV, detailed = F, var.equal = TRUE) 


# ALL UMSARS Total Year 2 

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==0) %>% inner_join(SCOPA_Baseline_Pats) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_scoretot) %>%
  rename("Baseline"="SCOPA_scoretot") %>%
  left_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_scoretot)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_scoretot)
  ) %>% 
  mutate(Diff= (SCOPA_scoretot-Baseline))  %>%
  t_test(Diff~DIAG, var.equal = TRUE) 
  #t_test(Diff~DIAGNIV,  var.equal = TRUE) 


# ALL UMSARS Total Year 3

AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==0) %>% inner_join(SCOPA_Baseline_Pats) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_scoretot) %>%
  rename("Baseline"="SCOPA_scoretot") %>%
  left_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_scoretot)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_scoretot)
  ) %>% 
  mutate(Diff= (SCOPA_scoretot-Baseline))   %>%
  #t_test(Diff~DIAG, var.equal = TRUE) 
 t_test(Diff~DIAGNIV,  var.equal = TRUE) 

# -----------------------------
# Overall Early CT  -------------------------------

# ALL UMSARS Total Year 1


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>% inner_join(SCOPA_Baseline_Pats) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_scoretot) %>%
  rename("Baseline"="SCOPA_scoretot") %>%
  left_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_scoretot)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_scoretot)
  ) %>% 
  mutate(Diff= (SCOPA_scoretot-Baseline))  %>%
  t_test(Diff~DIAG, var.equal = TRUE) 
  #t_test(Diff~DIAGNIV,  var.equal = TRUE) 




# ALL UMSARS Total Year 2


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>% inner_join(SCOPA_Baseline_Pats) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_scoretot) %>%
  rename("Baseline"="SCOPA_scoretot") %>%
  left_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_scoretot)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_scoretot)
  ) %>% 
  mutate(Diff= (SCOPA_scoretot-Baseline))  %>%
  #t_test(Diff~DIAG, var.equal = TRUE) 
  t_test(Diff~DIAGNIV,  var.equal = TRUE) 

# ALL UMSARS Total Year 3


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>% inner_join(SCOPA_Baseline_Pats) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_scoretot) %>%
  rename("Baseline"="SCOPA_scoretot") %>%
  left_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_scoretot)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_scoretot)
  ) %>% 
  mutate(Diff= (SCOPA_scoretot-Baseline))  %>%
  #t_test(Diff~DIAG, var.equal = TRUE) 
  t_test(Diff~DIAGNIV,  var.equal = TRUE) 

# --------------


# Correlation between Delta SCOPA AUT & Delta FDA-modified UMSARS ------------------

# Inputs SCOPA Total ----------------------------------

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


SCOPA <- dataCohorteManaged %>% 
  select(NUM, DATECONSULT, TIME_STUDY, Year, SCOPA_scoretot, SCOPA_nbdm)

SCOPA <- SCOPA %>% filter(SCOPA_scoretot!=0) %>% 
  mutate(factor=SCOPA_nbdm/25) %>% 
  mutate(factor=1-factor) %>%
  mutate(SCOPA_scoretot=SCOPA_scoretot/factor)

SCOPA_Baseline_Pats <- SCOPA %>% filter(Year==0) %>% filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM) %>% distinct() %>%
  inner_join(AllMSA_Pop_Baseline_671)



UMSARS1 <- dataCohorteManaged %>% 
  select(NUM, DATECONSULT, TIME_STUDY, Year, UMSARS1_1:UMSARS1_TOT)


UMSARS1$UMSARS1_TOT_v2 <- rowSums(UMSARS1[, 5:16], na.rm = TRUE)

UMSARS1$missing_na <- rowSums(is.na(UMSARS1[, 5:16]))

UMSARS1 <-  UMSARS1 %>%  select(NUM, TIME_STUDY, Year, UMSARS1_TOT, UMSARS1_11)

UMSARS1 <- UMSARS1 %>% mutate(UMSARS1_TOT_FDA=UMSARS1_TOT-UMSARS1_11)


# ----------------
# Overall MSA Entire -------------------------------

# Year 1

AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==0) %>% inner_join(SCOPA_Baseline_Pats) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_scoretot) %>%
  rename("Baseline"="SCOPA_scoretot") %>%
  left_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_scoretot)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_scoretot)
  ) %>% 
  mutate(Diff= (SCOPA_scoretot-Baseline))  %>% select(NUM, DIAG, DIAGNIV, Diff) %>% rename("SCOPA_Diff"="Diff") %>% drop_na() %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
      filter(!is.na(UMSARS1_TOT_FDA)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  UMSARS1_TOT_FDA) %>%
      rename("Baseline"="UMSARS1_TOT_FDA") %>%
      left_join(
        AllMSA_Pop_BaselineYear1_410 %>% inner_join(UMSARS1) %>% filter(Year==1) %>%
          filter(!is.na(UMSARS1_TOT_FDA)) %>%
          mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
          group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
          group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
          group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, UMSARS1_TOT_FDA)
      ) %>% 
      mutate(Diff= (UMSARS1_TOT_FDA-Baseline))  %>% select(NUM, DIAG, DIAGNIV, Diff)
  ) %>% # summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))
  # group_by(DIAG) %>% summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))
  group_by(DIAGNIV) %>% summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))




# Year 2 

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==0) %>% inner_join(SCOPA_Baseline_Pats) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_scoretot) %>%
  rename("Baseline"="SCOPA_scoretot") %>%
  left_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_scoretot)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_scoretot)
  ) %>% 
  mutate(Diff= (SCOPA_scoretot-Baseline))  %>% select(NUM, DIAG, DIAGNIV, Diff) %>% rename("SCOPA_Diff"="Diff") %>% drop_na()  %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
      filter(!is.na(UMSARS1_TOT_FDA)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  UMSARS1_TOT_FDA) %>%
      rename("Baseline"="UMSARS1_TOT_FDA") %>%
      left_join(
        AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(UMSARS1) %>% filter(Year==2) %>%
          filter(!is.na(UMSARS1_TOT_FDA)) %>%
          mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
          group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
          group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
          group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, UMSARS1_TOT_FDA)
      ) %>% 
      mutate(Diff= (UMSARS1_TOT_FDA-Baseline)) %>% select(NUM, DIAG, DIAGNIV, Diff)
  )  %>% # summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))
  # group_by(DIAG) %>% summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))
  group_by(DIAGNIV) %>% summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))




# Year 3

AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==0) %>% inner_join(SCOPA_Baseline_Pats) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_scoretot) %>%
  rename("Baseline"="SCOPA_scoretot") %>%
  left_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_scoretot)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_scoretot)
  ) %>% 
  mutate(Diff= (SCOPA_scoretot-Baseline))  %>% select(NUM, DIAG, DIAGNIV, Diff) %>% rename("SCOPA_Diff"="Diff") %>% drop_na() %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
      filter(!is.na(UMSARS1_TOT_FDA)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  UMSARS1_TOT_FDA) %>%
      rename("Baseline"="UMSARS1_TOT_FDA") %>%
      left_join(
        AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(UMSARS1) %>% filter(Year==3) %>%
          filter(!is.na(UMSARS1_TOT_FDA)) %>%
          mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
          group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
          group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
          group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, UMSARS1_TOT_FDA)
      ) %>% 
      mutate(Diff= (UMSARS1_TOT_FDA-Baseline)) %>% select(NUM, DIAG, DIAGNIV, Diff)
  )  %>% # summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))
  #group_by(DIAG) %>% summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))
  group_by(DIAGNIV) %>% summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))


# -----------------------------
# Overall Early CT  -------------------------------

# Year 1


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>% inner_join(SCOPA_Baseline_Pats) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_scoretot) %>%
  rename("Baseline"="SCOPA_scoretot") %>%
  left_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_scoretot)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_scoretot)
  ) %>% 
  mutate(Diff= (SCOPA_scoretot-Baseline))  %>% select(NUM, DIAG, DIAGNIV, Diff) %>% rename("SCOPA_Diff"="Diff") %>% drop_na() %>%
  inner_join(
    
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
      filter(!is.na(UMSARS1_TOT_FDA)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  UMSARS1_TOT_FDA) %>%
      rename("Baseline"="UMSARS1_TOT_FDA") %>%
      left_join(
        EarlyCT_Pop_BaselineYear1_208 %>% inner_join(UMSARS1) %>% filter(Year==1) %>%
          filter(!is.na(UMSARS1_TOT_FDA)) %>%
          mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
          group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
          group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
          group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, UMSARS1_TOT_FDA)
      ) %>% 
      mutate(Diff= (UMSARS1_TOT_FDA-Baseline))  %>% select(NUM, DIAG, DIAGNIV, Diff)
  )  %>% #summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))
  #group_by(DIAG) %>% summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))
  group_by(DIAGNIV) %>% summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))





# Year 2


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>% inner_join(SCOPA_Baseline_Pats) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_scoretot) %>%
  rename("Baseline"="SCOPA_scoretot") %>%
  left_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_scoretot)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_scoretot)
  ) %>% 
  mutate(Diff= (SCOPA_scoretot-Baseline))  %>% select(NUM, DIAG, DIAGNIV, Diff) %>% rename("SCOPA_Diff"="Diff") %>% drop_na() %>%
  inner_join(
    
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
      filter(!is.na(UMSARS1_TOT_FDA)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  UMSARS1_TOT_FDA) %>%
      rename("Baseline"="UMSARS1_TOT_FDA") %>%
      left_join(
        EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(UMSARS1) %>% filter(Year==2) %>%
          filter(!is.na(UMSARS1_TOT_FDA)) %>%
          mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
          group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
          group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
          group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, UMSARS1_TOT_FDA)
      ) %>% 
      mutate(Diff= (UMSARS1_TOT_FDA-Baseline))   %>% select(NUM, DIAG, DIAGNIV, Diff)
  )  %>% # summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))
  # group_by(DIAG) %>% summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))
  group_by(DIAGNIV) %>% summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))


# Year 3


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>% inner_join(SCOPA_Baseline_Pats) %>%
  filter(!is.na(SCOPA_scoretot)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_scoretot) %>%
  rename("Baseline"="SCOPA_scoretot") %>%
  left_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_scoretot)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_scoretot)
  ) %>% 
  mutate(Diff= (SCOPA_scoretot-Baseline))  %>% select(NUM, DIAG, DIAGNIV, Diff) %>% rename("SCOPA_Diff"="Diff") %>% drop_na() %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
      filter(!is.na(UMSARS1_TOT_FDA)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  UMSARS1_TOT_FDA) %>%
      rename("Baseline"="UMSARS1_TOT_FDA") %>%
      left_join(
        EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(UMSARS1) %>% filter(Year==3) %>%
          filter(!is.na(UMSARS1_TOT_FDA)) %>%
          mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
          group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
          group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
          group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, UMSARS1_TOT_FDA)
      ) %>% 
      mutate(Diff= (UMSARS1_TOT_FDA-Baseline)) %>% select(NUM, DIAG, DIAGNIV, Diff)
  )  %>% # summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))
  #group_by(DIAG) %>% summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))
  group_by(DIAGNIV) %>% summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))


# --------------



# Inputs COMPASS31  ----------------------------------

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


# ----------------
# Overall MSA Entire -------------------------------

# ALL COMPASS Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(COMPASS) %>% filter(Year==0) %>% 
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(DomainTotals),
    sd=sd(DomainTotals),
    median=median(DomainTotals),
    Q1 = quantile(DomainTotals, 0.25),
    Q3 = quantile(DomainTotals, 0.75),
    min=min(DomainTotals),
    max=max(DomainTotals),
    n=n()
  )



# ALL COMPASS Total Year 1

AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(DomainTotals),
    sd=sd(DomainTotals),
    median=median(DomainTotals),
    Q1 = quantile(DomainTotals, 0.25),
    Q3 = quantile(DomainTotals, 0.75),
    min=min(DomainTotals),
    max=max(DomainTotals),
    n=n()
  )

# Change from baseline

AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  DomainTotals) %>%
  rename("Baseline"="DomainTotals") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(DomainTotals)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, DomainTotals)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  summarise(mean= mean((DomainTotals-Baseline)/TIME_STUDY)) 



AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  DomainTotals) %>%
  rename("Baseline"="DomainTotals") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(DomainTotals)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, DomainTotals)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  mutate(Diff= (DomainTotals-Baseline)) %>% 
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )




AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  DomainTotals) %>%
  rename("Baseline"="DomainTotals") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(DomainTotals)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, DomainTotals)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
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



AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  DomainTotals) %>%
  rename("Baseline"="DomainTotals") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(DomainTotals)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, DomainTotals)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  mutate(Diff= (DomainTotals-Baseline)/TIME_STUDY) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



# ALL COMPASS Total Year 2

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(DomainTotals),
    sd=sd(DomainTotals),
    median=median(DomainTotals),
    Q1 = quantile(DomainTotals, 0.25),
    Q3 = quantile(DomainTotals, 0.75),
    min=min(DomainTotals),
    max=max(DomainTotals),
    n=n()
  )


# Change from baseline

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  DomainTotals) %>%
  rename("Baseline"="DomainTotals") %>% 
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(DomainTotals)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, DomainTotals)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  summarise(mean= mean((DomainTotals-Baseline)/TIME_STUDY)) 



AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  DomainTotals) %>%
  rename("Baseline"="DomainTotals") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(DomainTotals)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, DomainTotals)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  mutate(Diff= (DomainTotals-Baseline)) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  DomainTotals) %>%
  rename("Baseline"="DomainTotals") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(DomainTotals)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, DomainTotals)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
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


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  DomainTotals) %>%
  rename("Baseline"="DomainTotals") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(DomainTotals)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, DomainTotals)
  ) %>% 
  mutate(Diff= (DomainTotals-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


# ALL COMPASS Total Year 3

AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(DomainTotals),
    sd=sd(DomainTotals),
    median=median(DomainTotals),
    Q1 = quantile(DomainTotals, 0.25),
    Q3 = quantile(DomainTotals, 0.75),
    min=min(DomainTotals),
    max=max(DomainTotals),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  DomainTotals) %>%
  rename("Baseline"="DomainTotals") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(DomainTotals)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, DomainTotals)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  summarise(mean= mean((DomainTotals-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  DomainTotals) %>%
  rename("Baseline"="DomainTotals") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(DomainTotals)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, DomainTotals)
  ) %>%  
  mutate(Diff= (DomainTotals-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  DomainTotals) %>%
  rename("Baseline"="DomainTotals") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(DomainTotals)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, DomainTotals)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  DomainTotals) %>%
  rename("Baseline"="DomainTotals") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(DomainTotals)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, DomainTotals)
  ) %>% 
  mutate(Diff= (DomainTotals-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
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
# ALL COMPASS Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(DomainTotals),
    sd=sd(DomainTotals),
    median=median(DomainTotals),
    Q1 = quantile(DomainTotals, 0.25),
    Q3 = quantile(DomainTotals, 0.75),
    min=min(DomainTotals),
    max=max(DomainTotals),
    n=n()
  )



# ALL COMPASS Total Year 1

AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(DomainTotals),
    sd=sd(DomainTotals),
    median=median(DomainTotals),
    Q1 = quantile(DomainTotals, 0.25),
    Q3 = quantile(DomainTotals, 0.75),
    min=min(DomainTotals),
    max=max(DomainTotals),
    n=n()
  )

# Change from baseline

AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  DomainTotals) %>%
  rename("Baseline"="DomainTotals") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(DomainTotals)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, DomainTotals)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((DomainTotals-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  DomainTotals) %>%
  rename("Baseline"="DomainTotals") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(DomainTotals)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, DomainTotals)
  ) %>% 
  mutate(Diff= (DomainTotals-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  DomainTotals) %>%
  rename("Baseline"="DomainTotals") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(DomainTotals)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, DomainTotals)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  DomainTotals) %>%
  rename("Baseline"="DomainTotals") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(DomainTotals)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, DomainTotals)
  ) %>% 
  mutate(Diff= (DomainTotals-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



# ALL COMPASS Total Year 2

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(DomainTotals),
    sd=sd(DomainTotals),
    median=median(DomainTotals),
    Q1 = quantile(DomainTotals, 0.25),
    Q3 = quantile(DomainTotals, 0.75),
    min=min(DomainTotals),
    max=max(DomainTotals),
    n=n()
  )


# Change from baseline

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  DomainTotals) %>%
  rename("Baseline"="DomainTotals") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(DomainTotals)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, DomainTotals)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((DomainTotals-Baseline)/TIME_STUDY)) 



AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  DomainTotals) %>%
  rename("Baseline"="DomainTotals") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(DomainTotals)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, DomainTotals)
  ) %>% 
  mutate(Diff= (DomainTotals-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  DomainTotals) %>%
  rename("Baseline"="DomainTotals") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(DomainTotals)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, DomainTotals)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
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


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  DomainTotals) %>%
  rename("Baseline"="DomainTotals") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(DomainTotals)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, DomainTotals)
  ) %>% 
  mutate(Diff= (DomainTotals-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
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


# ALL COMPASS Total Year 3

AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(DomainTotals),
    sd=sd(DomainTotals),
    median=median(DomainTotals),
    Q1 = quantile(DomainTotals, 0.25),
    Q3 = quantile(DomainTotals, 0.75),
    min=min(DomainTotals),
    max=max(DomainTotals),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  DomainTotals) %>%
  rename("Baseline"="DomainTotals") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(DomainTotals)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, DomainTotals)
  ) %>% group_by(DIAG) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(mean= mean((DomainTotals-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  DomainTotals) %>%
  rename("Baseline"="DomainTotals") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(DomainTotals)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, DomainTotals)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  mutate(Diff= (DomainTotals-Baseline)) %>%
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


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  DomainTotals) %>%
  rename("Baseline"="DomainTotals") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(DomainTotals)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, DomainTotals)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
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


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  DomainTotals) %>%
  rename("Baseline"="DomainTotals") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(DomainTotals)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, DomainTotals)
  ) %>% 
  mutate(Diff= (DomainTotals-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
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

# ALL COMPASS Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(DomainTotals),
    sd=sd(DomainTotals),
    median=median(DomainTotals),
    Q1 = quantile(DomainTotals, 0.25),
    Q3 = quantile(DomainTotals, 0.75),
    min=min(DomainTotals),
    max=max(DomainTotals),
    n=n()
  )



# ALL COMPASS Total Year 1

AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(DomainTotals),
    sd=sd(DomainTotals),
    median=median(DomainTotals),
    Q1 = quantile(DomainTotals, 0.25),
    Q3 = quantile(DomainTotals, 0.75),
    min=min(DomainTotals),
    max=max(DomainTotals),
    n=n()
  )

# Change from baseline


AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  DomainTotals) %>%
  rename("Baseline"="DomainTotals") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(DomainTotals)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, DomainTotals)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((DomainTotals-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  DomainTotals) %>%
  rename("Baseline"="DomainTotals") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(DomainTotals)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, DomainTotals)
  ) %>% 
  mutate(Diff= (DomainTotals-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
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


AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  DomainTotals) %>%
  rename("Baseline"="DomainTotals") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(DomainTotals)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, DomainTotals)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
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


AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  DomainTotals) %>%
  rename("Baseline"="DomainTotals") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(DomainTotals)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, DomainTotals)
  ) %>% 
  mutate(Diff= (DomainTotals-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



# ALL COMPASS Total Year 2

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(DomainTotals),
    sd=sd(DomainTotals),
    median=median(DomainTotals),
    Q1 = quantile(DomainTotals, 0.25),
    Q3 = quantile(DomainTotals, 0.75),
    min=min(DomainTotals),
    max=max(DomainTotals),
    n=n()
  )


# Change from baseline

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  DomainTotals) %>%
  rename("Baseline"="DomainTotals") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(DomainTotals)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, DomainTotals)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((DomainTotals-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  DomainTotals) %>%
  rename("Baseline"="DomainTotals") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(DomainTotals)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, DomainTotals)
  ) %>% 
  mutate(Diff= (DomainTotals-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
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


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  DomainTotals) %>%
  rename("Baseline"="DomainTotals") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(DomainTotals)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, DomainTotals)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
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


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  DomainTotals) %>%
  rename("Baseline"="DomainTotals") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(DomainTotals)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, DomainTotals)
  ) %>% 
  mutate(Diff= (DomainTotals-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
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


# ALL COMPASS Total Year 3

AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(DomainTotals),
    sd=sd(DomainTotals),
    median=median(DomainTotals),
    Q1 = quantile(DomainTotals, 0.25),
    Q3 = quantile(DomainTotals, 0.75),
    min=min(DomainTotals),
    max=max(DomainTotals),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  DomainTotals) %>%
  rename("Baseline"="DomainTotals") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(DomainTotals)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, DomainTotals)
  ) %>% group_by(DIAGNIV) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(mean= mean((DomainTotals-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  DomainTotals) %>%
  rename("Baseline"="DomainTotals") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(DomainTotals)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, DomainTotals)
  ) %>% 
  mutate(Diff= (DomainTotals-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
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


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  DomainTotals) %>%
  rename("Baseline"="DomainTotals") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(DomainTotals)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, DomainTotals)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  DomainTotals) %>%
  rename("Baseline"="DomainTotals") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(DomainTotals)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, DomainTotals)
  ) %>% 
  mutate(Diff= (DomainTotals-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
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

# ALL COMPASS Total Year 0 


EarlyCT_Pop_Baseline_319 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(DomainTotals),
    sd=sd(DomainTotals),
    median=median(DomainTotals),
    Q1 = quantile(DomainTotals, 0.25),
    Q3 = quantile(DomainTotals, 0.75),
    min=min(DomainTotals),
    max=max(DomainTotals),
    n=n()
  )



# ALL COMPASS Total Year 1

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(DomainTotals),
    sd=sd(DomainTotals),
    median=median(DomainTotals),
    Q1 = quantile(DomainTotals, 0.25),
    Q3 = quantile(DomainTotals, 0.75),
    min=min(DomainTotals),
    max=max(DomainTotals),
    n=n()
  )

# Change from baseline

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  DomainTotals) %>%
  rename("Baseline"="DomainTotals") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(DomainTotals)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, DomainTotals)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  summarise(mean= mean((DomainTotals-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  DomainTotals) %>%
  rename("Baseline"="DomainTotals") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(DomainTotals)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, DomainTotals)
  ) %>% 
  mutate(Diff= (DomainTotals-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  DomainTotals) %>%
  rename("Baseline"="DomainTotals") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(DomainTotals)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, DomainTotals)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  DomainTotals) %>%
  rename("Baseline"="DomainTotals") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(DomainTotals)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, DomainTotals)
  ) %>% 
  mutate(Diff= (DomainTotals-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



# ALL COMPASS Total Year 2

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(DomainTotals),
    sd=sd(DomainTotals),
    median=median(DomainTotals),
    Q1 = quantile(DomainTotals, 0.25),
    Q3 = quantile(DomainTotals, 0.75),
    min=min(DomainTotals),
    max=max(DomainTotals),
    n=n()
  )


# Change from baseline

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  DomainTotals) %>%
  rename("Baseline"="DomainTotals") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(DomainTotals)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, DomainTotals)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  summarise(mean= mean((DomainTotals-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  DomainTotals) %>%
  rename("Baseline"="DomainTotals") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(DomainTotals)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, DomainTotals)
  ) %>% 
  mutate(Diff= (DomainTotals-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  DomainTotals) %>%
  rename("Baseline"="DomainTotals") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(DomainTotals)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, DomainTotals)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  DomainTotals) %>%
  rename("Baseline"="DomainTotals") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(DomainTotals)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, DomainTotals)
  ) %>% 
  mutate(Diff= (DomainTotals-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


# ALL COMPASS Total Year 3

EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(DomainTotals),
    sd=sd(DomainTotals),
    median=median(DomainTotals),
    Q1 = quantile(DomainTotals, 0.25),
    Q3 = quantile(DomainTotals, 0.75),
    min=min(DomainTotals),
    max=max(DomainTotals),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  DomainTotals) %>%
  rename("Baseline"="DomainTotals") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(DomainTotals)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, DomainTotals)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  summarise(mean= mean((DomainTotals-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  DomainTotals) %>%
  rename("Baseline"="DomainTotals") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(DomainTotals)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, DomainTotals)
  ) %>% 
  mutate(Diff= (DomainTotals-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  DomainTotals) %>%
  rename("Baseline"="DomainTotals") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(DomainTotals)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, DomainTotals)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  DomainTotals) %>%
  rename("Baseline"="DomainTotals") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(DomainTotals)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, DomainTotals)
  ) %>% 
  mutate(Diff= (DomainTotals-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
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
# ALL COMPASS Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(DomainTotals),
    sd=sd(DomainTotals),
    median=median(DomainTotals),
    Q1 = quantile(DomainTotals, 0.25),
    Q3 = quantile(DomainTotals, 0.75),
    min=min(DomainTotals),
    max=max(DomainTotals),
    n=n()
  )



# ALL COMPASS Total Year 1

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(DomainTotals),
    sd=sd(DomainTotals),
    median=median(DomainTotals),
    Q1 = quantile(DomainTotals, 0.25),
    Q3 = quantile(DomainTotals, 0.75),
    min=min(DomainTotals),
    max=max(DomainTotals),
    n=n()
  )

# Change from baseline

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  DomainTotals) %>%
  rename("Baseline"="DomainTotals") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(DomainTotals)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, DomainTotals)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((DomainTotals-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  DomainTotals) %>%
  rename("Baseline"="DomainTotals") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(DomainTotals)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, DomainTotals)
  ) %>% 
  mutate(Diff= (DomainTotals-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  DomainTotals) %>%
  rename("Baseline"="DomainTotals") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(DomainTotals)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, DomainTotals)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  DomainTotals) %>%
  rename("Baseline"="DomainTotals") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(DomainTotals)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, DomainTotals)
  ) %>% 
  mutate(Diff= (DomainTotals-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



# ALL COMPASS Total Year 2

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(DomainTotals),
    sd=sd(DomainTotals),
    median=median(DomainTotals),
    Q1 = quantile(DomainTotals, 0.25),
    Q3 = quantile(DomainTotals, 0.75),
    min=min(DomainTotals),
    max=max(DomainTotals),
    n=n()
  )


# Change from baseline

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  DomainTotals) %>%
  rename("Baseline"="DomainTotals") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(DomainTotals)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, DomainTotals)
  ) %>% inner_join(COMPASS_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((DomainTotals-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  DomainTotals) %>%
  rename("Baseline"="DomainTotals") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(DomainTotals)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, DomainTotals)
  ) %>% 
  mutate(Diff= (DomainTotals-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
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


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  DomainTotals) %>%
  rename("Baseline"="DomainTotals") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(DomainTotals)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, DomainTotals)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
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


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  DomainTotals) %>%
  rename("Baseline"="DomainTotals") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(DomainTotals)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, DomainTotals)
  ) %>% 
  mutate(Diff= (DomainTotals-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
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


# ALL COMPASS Total Year 3

EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(DomainTotals),
    sd=sd(DomainTotals),
    median=median(DomainTotals),
    Q1 = quantile(DomainTotals, 0.25),
    Q3 = quantile(DomainTotals, 0.75),
    min=min(DomainTotals),
    max=max(DomainTotals),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  DomainTotals) %>%
  rename("Baseline"="DomainTotals") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(DomainTotals)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, DomainTotals)
  ) %>% group_by(DIAG) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(mean= mean((DomainTotals-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  DomainTotals) %>%
  rename("Baseline"="DomainTotals") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(DomainTotals)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, DomainTotals)
  ) %>% 
  mutate(Diff= (DomainTotals-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  DomainTotals) %>%
  rename("Baseline"="DomainTotals") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(DomainTotals)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, DomainTotals)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  DomainTotals) %>%
  rename("Baseline"="DomainTotals") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(DomainTotals)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, DomainTotals)
  ) %>% 
  mutate(Diff= (DomainTotals-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
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

# ALL COMPASS Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(DomainTotals),
    sd=sd(DomainTotals),
    median=median(DomainTotals),
    Q1 = quantile(DomainTotals, 0.25),
    Q3 = quantile(DomainTotals, 0.75),
    min=min(DomainTotals),
    max=max(DomainTotals),
    n=n()
  )



# ALL COMPASS Total Year 1

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(DomainTotals),
    sd=sd(DomainTotals),
    median=median(DomainTotals),
    Q1 = quantile(DomainTotals, 0.25),
    Q3 = quantile(DomainTotals, 0.75),
    min=min(DomainTotals),
    max=max(DomainTotals),
    n=n()
  )

# Change from baseline


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  DomainTotals) %>%
  rename("Baseline"="DomainTotals") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(DomainTotals)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, DomainTotals)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((DomainTotals-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  DomainTotals) %>%
  rename("Baseline"="DomainTotals") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(DomainTotals)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, DomainTotals)
  ) %>% 
  mutate(Diff= (DomainTotals-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
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




EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  DomainTotals) %>%
  rename("Baseline"="DomainTotals") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(DomainTotals)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, DomainTotals)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
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




EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  DomainTotals) %>%
  rename("Baseline"="DomainTotals") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(DomainTotals)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, DomainTotals)
  ) %>% 
  mutate(Diff= (DomainTotals-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



# ALL COMPASS Total Year 2

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(DomainTotals),
    sd=sd(DomainTotals),
    median=median(DomainTotals),
    Q1 = quantile(DomainTotals, 0.25),
    Q3 = quantile(DomainTotals, 0.75),
    min=min(DomainTotals),
    max=max(DomainTotals),
    n=n()
  )


# Change from baseline

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  DomainTotals) %>%
  rename("Baseline"="DomainTotals") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(DomainTotals)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, DomainTotals)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((DomainTotals-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  DomainTotals) %>%
  rename("Baseline"="DomainTotals") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(DomainTotals)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, DomainTotals)
  ) %>% 
  mutate(Diff= (DomainTotals-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  DomainTotals) %>%
  rename("Baseline"="DomainTotals") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(DomainTotals)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, DomainTotals)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  DomainTotals) %>%
  rename("Baseline"="DomainTotals") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(DomainTotals)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, DomainTotals)
  ) %>% 
  mutate(Diff= (DomainTotals-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
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


# ALL COMPASS Total Year 3

EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(DomainTotals),
    sd=sd(DomainTotals),
    median=median(DomainTotals),
    Q1 = quantile(DomainTotals, 0.25),
    Q3 = quantile(DomainTotals, 0.75),
    min=min(DomainTotals),
    max=max(DomainTotals),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  DomainTotals) %>%
  rename("Baseline"="DomainTotals") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(DomainTotals)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, DomainTotals)
  ) %>% group_by(DIAGNIV) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(mean= mean((DomainTotals-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  DomainTotals) %>%
  rename("Baseline"="DomainTotals") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(DomainTotals)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, DomainTotals)
  ) %>% 
  mutate(Diff= (DomainTotals-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  DomainTotals) %>%
  rename("Baseline"="DomainTotals") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(DomainTotals)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, DomainTotals)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  DomainTotals) %>%
  rename("Baseline"="DomainTotals") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(DomainTotals)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, DomainTotals)
  ) %>% 
  mutate(Diff= (DomainTotals-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
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


# Inputs COMPASS31 Orthostatic ----------------------------------

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


COMPASS <- COMPASS %>% select(NUM, DATECONSULT, TIME_STUDY, Year, DomainTotals, Vasomotor, Secretomotor, Gastrointestinal, Bladder, Pupillomotor, Orthostatic)

range(COMPASS$Orthostatic)
range(COMPASS$Orthostatic)

COMPASS <- COMPASS[COMPASS$DomainTotals>0,]

COMPASS_Baseline_Pats <- COMPASS %>% filter(Year==0) %>% filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM) %>% distinct() %>%
  inner_join(AllMSA_Pop_Baseline_671)


# ----------------
# Overall MSA Entire -------------------------------

# ALL COMPASS Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(COMPASS) %>% filter(Year==0) %>% 
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Orthostatic),
    sd=sd(Orthostatic),
    median=median(Orthostatic),
    Q1 = quantile(Orthostatic, 0.25),
    Q3 = quantile(Orthostatic, 0.75),
    min=min(Orthostatic),
    max=max(Orthostatic),
    n=n()
  )



# ALL COMPASS Total Year 1

AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Orthostatic),
    sd=sd(Orthostatic),
    median=median(Orthostatic),
    Q1 = quantile(Orthostatic, 0.25),
    Q3 = quantile(Orthostatic, 0.75),
    min=min(Orthostatic),
    max=max(Orthostatic),
    n=n()
  )

# Change from baseline

AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Orthostatic) %>%
  rename("Baseline"="Orthostatic") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Orthostatic)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Orthostatic)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  summarise(mean= mean((Orthostatic-Baseline)/TIME_STUDY)) 



AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Orthostatic) %>%
  rename("Baseline"="Orthostatic") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Orthostatic)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Orthostatic)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  mutate(Diff= (Orthostatic-Baseline)) %>% 
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )




AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Orthostatic) %>%
  rename("Baseline"="Orthostatic") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Orthostatic)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Orthostatic)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
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



AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Orthostatic) %>%
  rename("Baseline"="Orthostatic") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Orthostatic)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Orthostatic)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  mutate(Diff= (Orthostatic-Baseline)/TIME_STUDY) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



# ALL COMPASS Total Year 2

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Orthostatic),
    sd=sd(Orthostatic),
    median=median(Orthostatic),
    Q1 = quantile(Orthostatic, 0.25),
    Q3 = quantile(Orthostatic, 0.75),
    min=min(Orthostatic),
    max=max(Orthostatic),
    n=n()
  )


# Change from baseline

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Orthostatic) %>%
  rename("Baseline"="Orthostatic") %>% 
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Orthostatic)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Orthostatic)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  summarise(mean= mean((Orthostatic-Baseline)/TIME_STUDY)) 



AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Orthostatic) %>%
  rename("Baseline"="Orthostatic") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Orthostatic)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Orthostatic)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  mutate(Diff= (Orthostatic-Baseline)) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Orthostatic) %>%
  rename("Baseline"="Orthostatic") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Orthostatic)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Orthostatic)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
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


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Orthostatic) %>%
  rename("Baseline"="Orthostatic") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Orthostatic)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Orthostatic)
  ) %>% 
  mutate(Diff= (Orthostatic-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


# ALL COMPASS Total Year 3

AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Orthostatic),
    sd=sd(Orthostatic),
    median=median(Orthostatic),
    Q1 = quantile(Orthostatic, 0.25),
    Q3 = quantile(Orthostatic, 0.75),
    min=min(Orthostatic),
    max=max(Orthostatic),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Orthostatic) %>%
  rename("Baseline"="Orthostatic") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Orthostatic)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Orthostatic)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  summarise(mean= mean((Orthostatic-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Orthostatic) %>%
  rename("Baseline"="Orthostatic") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Orthostatic)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Orthostatic)
  ) %>%  
  mutate(Diff= (Orthostatic-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Orthostatic) %>%
  rename("Baseline"="Orthostatic") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Orthostatic)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Orthostatic)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Orthostatic) %>%
  rename("Baseline"="Orthostatic") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Orthostatic)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Orthostatic)
  ) %>% 
  mutate(Diff= (Orthostatic-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
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
# ALL COMPASS Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Orthostatic),
    sd=sd(Orthostatic),
    median=median(Orthostatic),
    Q1 = quantile(Orthostatic, 0.25),
    Q3 = quantile(Orthostatic, 0.75),
    min=min(Orthostatic),
    max=max(Orthostatic),
    n=n()
  )



# ALL COMPASS Total Year 1

AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Orthostatic),
    sd=sd(Orthostatic),
    median=median(Orthostatic),
    Q1 = quantile(Orthostatic, 0.25),
    Q3 = quantile(Orthostatic, 0.75),
    min=min(Orthostatic),
    max=max(Orthostatic),
    n=n()
  )

# Change from baseline

AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Orthostatic) %>%
  rename("Baseline"="Orthostatic") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Orthostatic)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Orthostatic)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((Orthostatic-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Orthostatic) %>%
  rename("Baseline"="Orthostatic") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Orthostatic)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Orthostatic)
  ) %>% 
  mutate(Diff= (Orthostatic-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Orthostatic) %>%
  rename("Baseline"="Orthostatic") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Orthostatic)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Orthostatic)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Orthostatic) %>%
  rename("Baseline"="Orthostatic") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Orthostatic)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Orthostatic)
  ) %>% 
  mutate(Diff= (Orthostatic-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



# ALL COMPASS Total Year 2

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Orthostatic),
    sd=sd(Orthostatic),
    median=median(Orthostatic),
    Q1 = quantile(Orthostatic, 0.25),
    Q3 = quantile(Orthostatic, 0.75),
    min=min(Orthostatic),
    max=max(Orthostatic),
    n=n()
  )


# Change from baseline

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Orthostatic) %>%
  rename("Baseline"="Orthostatic") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Orthostatic)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Orthostatic)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((Orthostatic-Baseline)/TIME_STUDY)) 



AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Orthostatic) %>%
  rename("Baseline"="Orthostatic") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Orthostatic)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Orthostatic)
  ) %>% 
  mutate(Diff= (Orthostatic-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Orthostatic) %>%
  rename("Baseline"="Orthostatic") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Orthostatic)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Orthostatic)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
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


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Orthostatic) %>%
  rename("Baseline"="Orthostatic") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Orthostatic)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Orthostatic)
  ) %>% 
  mutate(Diff= (Orthostatic-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
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


# ALL COMPASS Total Year 3

AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Orthostatic),
    sd=sd(Orthostatic),
    median=median(Orthostatic),
    Q1 = quantile(Orthostatic, 0.25),
    Q3 = quantile(Orthostatic, 0.75),
    min=min(Orthostatic),
    max=max(Orthostatic),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Orthostatic) %>%
  rename("Baseline"="Orthostatic") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Orthostatic)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Orthostatic)
  ) %>% group_by(DIAG) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(mean= mean((Orthostatic-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Orthostatic) %>%
  rename("Baseline"="Orthostatic") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Orthostatic)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Orthostatic)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  mutate(Diff= (Orthostatic-Baseline)) %>%
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


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Orthostatic) %>%
  rename("Baseline"="Orthostatic") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Orthostatic)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Orthostatic)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
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


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Orthostatic) %>%
  rename("Baseline"="Orthostatic") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Orthostatic)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Orthostatic)
  ) %>% 
  mutate(Diff= (Orthostatic-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
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

# ALL COMPASS Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Orthostatic),
    sd=sd(Orthostatic),
    median=median(Orthostatic),
    Q1 = quantile(Orthostatic, 0.25),
    Q3 = quantile(Orthostatic, 0.75),
    min=min(Orthostatic),
    max=max(Orthostatic),
    n=n()
  )



# ALL COMPASS Total Year 1

AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Orthostatic),
    sd=sd(Orthostatic),
    median=median(Orthostatic),
    Q1 = quantile(Orthostatic, 0.25),
    Q3 = quantile(Orthostatic, 0.75),
    min=min(Orthostatic),
    max=max(Orthostatic),
    n=n()
  )

# Change from baseline


AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Orthostatic) %>%
  rename("Baseline"="Orthostatic") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Orthostatic)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Orthostatic)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((Orthostatic-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Orthostatic) %>%
  rename("Baseline"="Orthostatic") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Orthostatic)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Orthostatic)
  ) %>% 
  mutate(Diff= (Orthostatic-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
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


AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Orthostatic) %>%
  rename("Baseline"="Orthostatic") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Orthostatic)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Orthostatic)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
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


AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Orthostatic) %>%
  rename("Baseline"="Orthostatic") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Orthostatic)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Orthostatic)
  ) %>% 
  mutate(Diff= (Orthostatic-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



# ALL COMPASS Total Year 2

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Orthostatic),
    sd=sd(Orthostatic),
    median=median(Orthostatic),
    Q1 = quantile(Orthostatic, 0.25),
    Q3 = quantile(Orthostatic, 0.75),
    min=min(Orthostatic),
    max=max(Orthostatic),
    n=n()
  )


# Change from baseline

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Orthostatic) %>%
  rename("Baseline"="Orthostatic") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Orthostatic)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Orthostatic)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((Orthostatic-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Orthostatic) %>%
  rename("Baseline"="Orthostatic") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Orthostatic)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Orthostatic)
  ) %>% 
  mutate(Diff= (Orthostatic-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
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


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Orthostatic) %>%
  rename("Baseline"="Orthostatic") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Orthostatic)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Orthostatic)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
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


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Orthostatic) %>%
  rename("Baseline"="Orthostatic") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Orthostatic)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Orthostatic)
  ) %>% 
  mutate(Diff= (Orthostatic-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
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


# ALL COMPASS Total Year 3

AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Orthostatic),
    sd=sd(Orthostatic),
    median=median(Orthostatic),
    Q1 = quantile(Orthostatic, 0.25),
    Q3 = quantile(Orthostatic, 0.75),
    min=min(Orthostatic),
    max=max(Orthostatic),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Orthostatic) %>%
  rename("Baseline"="Orthostatic") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Orthostatic)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Orthostatic)
  ) %>% group_by(DIAGNIV) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(mean= mean((Orthostatic-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Orthostatic) %>%
  rename("Baseline"="Orthostatic") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Orthostatic)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Orthostatic)
  ) %>% 
  mutate(Diff= (Orthostatic-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
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


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Orthostatic) %>%
  rename("Baseline"="Orthostatic") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Orthostatic)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Orthostatic)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Orthostatic) %>%
  rename("Baseline"="Orthostatic") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Orthostatic)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Orthostatic)
  ) %>% 
  mutate(Diff= (Orthostatic-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
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

# ALL COMPASS Total Year 0 


EarlyCT_Pop_Baseline_319 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Orthostatic),
    sd=sd(Orthostatic),
    median=median(Orthostatic),
    Q1 = quantile(Orthostatic, 0.25),
    Q3 = quantile(Orthostatic, 0.75),
    min=min(Orthostatic),
    max=max(Orthostatic),
    n=n()
  )



# ALL COMPASS Total Year 1

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Orthostatic),
    sd=sd(Orthostatic),
    median=median(Orthostatic),
    Q1 = quantile(Orthostatic, 0.25),
    Q3 = quantile(Orthostatic, 0.75),
    min=min(Orthostatic),
    max=max(Orthostatic),
    n=n()
  )

# Change from baseline

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Orthostatic) %>%
  rename("Baseline"="Orthostatic") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Orthostatic)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Orthostatic)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  summarise(mean= mean((Orthostatic-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Orthostatic) %>%
  rename("Baseline"="Orthostatic") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Orthostatic)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Orthostatic)
  ) %>% 
  mutate(Diff= (Orthostatic-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Orthostatic) %>%
  rename("Baseline"="Orthostatic") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Orthostatic)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Orthostatic)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Orthostatic) %>%
  rename("Baseline"="Orthostatic") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Orthostatic)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Orthostatic)
  ) %>% 
  mutate(Diff= (Orthostatic-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



# ALL COMPASS Total Year 2

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Orthostatic),
    sd=sd(Orthostatic),
    median=median(Orthostatic),
    Q1 = quantile(Orthostatic, 0.25),
    Q3 = quantile(Orthostatic, 0.75),
    min=min(Orthostatic),
    max=max(Orthostatic),
    n=n()
  )


# Change from baseline

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Orthostatic) %>%
  rename("Baseline"="Orthostatic") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Orthostatic)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Orthostatic)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  summarise(mean= mean((Orthostatic-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Orthostatic) %>%
  rename("Baseline"="Orthostatic") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Orthostatic)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Orthostatic)
  ) %>% 
  mutate(Diff= (Orthostatic-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Orthostatic) %>%
  rename("Baseline"="Orthostatic") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Orthostatic)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Orthostatic)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Orthostatic) %>%
  rename("Baseline"="Orthostatic") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Orthostatic)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Orthostatic)
  ) %>% 
  mutate(Diff= (Orthostatic-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


# ALL COMPASS Total Year 3

EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Orthostatic),
    sd=sd(Orthostatic),
    median=median(Orthostatic),
    Q1 = quantile(Orthostatic, 0.25),
    Q3 = quantile(Orthostatic, 0.75),
    min=min(Orthostatic),
    max=max(Orthostatic),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Orthostatic) %>%
  rename("Baseline"="Orthostatic") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Orthostatic)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Orthostatic)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  summarise(mean= mean((Orthostatic-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Orthostatic) %>%
  rename("Baseline"="Orthostatic") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Orthostatic)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Orthostatic)
  ) %>% 
  mutate(Diff= (Orthostatic-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Orthostatic) %>%
  rename("Baseline"="Orthostatic") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Orthostatic)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Orthostatic)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Orthostatic) %>%
  rename("Baseline"="Orthostatic") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Orthostatic)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Orthostatic)
  ) %>% 
  mutate(Diff= (Orthostatic-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
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
# ALL COMPASS Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Orthostatic),
    sd=sd(Orthostatic),
    median=median(Orthostatic),
    Q1 = quantile(Orthostatic, 0.25),
    Q3 = quantile(Orthostatic, 0.75),
    min=min(Orthostatic),
    max=max(Orthostatic),
    n=n()
  )



# ALL COMPASS Total Year 1

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Orthostatic),
    sd=sd(Orthostatic),
    median=median(Orthostatic),
    Q1 = quantile(Orthostatic, 0.25),
    Q3 = quantile(Orthostatic, 0.75),
    min=min(Orthostatic),
    max=max(Orthostatic),
    n=n()
  )

# Change from baseline

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Orthostatic) %>%
  rename("Baseline"="Orthostatic") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Orthostatic)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Orthostatic)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((Orthostatic-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Orthostatic) %>%
  rename("Baseline"="Orthostatic") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Orthostatic)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Orthostatic)
  ) %>% 
  mutate(Diff= (Orthostatic-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Orthostatic) %>%
  rename("Baseline"="Orthostatic") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Orthostatic)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Orthostatic)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Orthostatic) %>%
  rename("Baseline"="Orthostatic") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Orthostatic)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Orthostatic)
  ) %>% 
  mutate(Diff= (Orthostatic-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



# ALL COMPASS Total Year 2

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Orthostatic),
    sd=sd(Orthostatic),
    median=median(Orthostatic),
    Q1 = quantile(Orthostatic, 0.25),
    Q3 = quantile(Orthostatic, 0.75),
    min=min(Orthostatic),
    max=max(Orthostatic),
    n=n()
  )


# Change from baseline

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Orthostatic) %>%
  rename("Baseline"="Orthostatic") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Orthostatic)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Orthostatic)
  ) %>% inner_join(COMPASS_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((Orthostatic-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Orthostatic) %>%
  rename("Baseline"="Orthostatic") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Orthostatic)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Orthostatic)
  ) %>% 
  mutate(Diff= (Orthostatic-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
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


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Orthostatic) %>%
  rename("Baseline"="Orthostatic") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Orthostatic)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Orthostatic)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
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


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Orthostatic) %>%
  rename("Baseline"="Orthostatic") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Orthostatic)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Orthostatic)
  ) %>% 
  mutate(Diff= (Orthostatic-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
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


# ALL COMPASS Total Year 3

EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Orthostatic),
    sd=sd(Orthostatic),
    median=median(Orthostatic),
    Q1 = quantile(Orthostatic, 0.25),
    Q3 = quantile(Orthostatic, 0.75),
    min=min(Orthostatic),
    max=max(Orthostatic),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Orthostatic) %>%
  rename("Baseline"="Orthostatic") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Orthostatic)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Orthostatic)
  ) %>% group_by(DIAG) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(mean= mean((Orthostatic-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Orthostatic) %>%
  rename("Baseline"="Orthostatic") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Orthostatic)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Orthostatic)
  ) %>% 
  mutate(Diff= (Orthostatic-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Orthostatic) %>%
  rename("Baseline"="Orthostatic") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Orthostatic)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Orthostatic)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Orthostatic) %>%
  rename("Baseline"="Orthostatic") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Orthostatic)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Orthostatic)
  ) %>% 
  mutate(Diff= (Orthostatic-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
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

# ALL COMPASS Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Orthostatic),
    sd=sd(Orthostatic),
    median=median(Orthostatic),
    Q1 = quantile(Orthostatic, 0.25),
    Q3 = quantile(Orthostatic, 0.75),
    min=min(Orthostatic),
    max=max(Orthostatic),
    n=n()
  )



# ALL COMPASS Total Year 1

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Orthostatic),
    sd=sd(Orthostatic),
    median=median(Orthostatic),
    Q1 = quantile(Orthostatic, 0.25),
    Q3 = quantile(Orthostatic, 0.75),
    min=min(Orthostatic),
    max=max(Orthostatic),
    n=n()
  )

# Change from baseline


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Orthostatic) %>%
  rename("Baseline"="Orthostatic") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Orthostatic)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Orthostatic)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((Orthostatic-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Orthostatic) %>%
  rename("Baseline"="Orthostatic") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Orthostatic)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Orthostatic)
  ) %>% 
  mutate(Diff= (Orthostatic-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
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




EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Orthostatic) %>%
  rename("Baseline"="Orthostatic") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Orthostatic)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Orthostatic)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
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




EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Orthostatic) %>%
  rename("Baseline"="Orthostatic") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Orthostatic)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Orthostatic)
  ) %>% 
  mutate(Diff= (Orthostatic-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



# ALL COMPASS Total Year 2

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Orthostatic),
    sd=sd(Orthostatic),
    median=median(Orthostatic),
    Q1 = quantile(Orthostatic, 0.25),
    Q3 = quantile(Orthostatic, 0.75),
    min=min(Orthostatic),
    max=max(Orthostatic),
    n=n()
  )


# Change from baseline

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Orthostatic) %>%
  rename("Baseline"="Orthostatic") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Orthostatic)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Orthostatic)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((Orthostatic-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Orthostatic) %>%
  rename("Baseline"="Orthostatic") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Orthostatic)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Orthostatic)
  ) %>% 
  mutate(Diff= (Orthostatic-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Orthostatic) %>%
  rename("Baseline"="Orthostatic") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Orthostatic)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Orthostatic)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Orthostatic) %>%
  rename("Baseline"="Orthostatic") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Orthostatic)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Orthostatic)
  ) %>% 
  mutate(Diff= (Orthostatic-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
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


# ALL COMPASS Total Year 3

EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Orthostatic),
    sd=sd(Orthostatic),
    median=median(Orthostatic),
    Q1 = quantile(Orthostatic, 0.25),
    Q3 = quantile(Orthostatic, 0.75),
    min=min(Orthostatic),
    max=max(Orthostatic),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Orthostatic) %>%
  rename("Baseline"="Orthostatic") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Orthostatic)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Orthostatic)
  ) %>% group_by(DIAGNIV) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(mean= mean((Orthostatic-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Orthostatic) %>%
  rename("Baseline"="Orthostatic") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Orthostatic)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Orthostatic)
  ) %>% 
  mutate(Diff= (Orthostatic-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Orthostatic) %>%
  rename("Baseline"="Orthostatic") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Orthostatic)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Orthostatic)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Orthostatic)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Orthostatic) %>%
  rename("Baseline"="Orthostatic") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Orthostatic)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Orthostatic)
  ) %>% 
  mutate(Diff= (Orthostatic-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
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


# Inputs COMPASS31 Vasomotor ----------------------------------

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


COMPASS <- COMPASS %>% select(NUM, DATECONSULT, TIME_STUDY, Year, DomainTotals, Vasomotor, Secretomotor, Gastrointestinal, Bladder, Pupillomotor, Orthostatic)

range(COMPASS$Orthostatic)
range(COMPASS$Orthostatic)

COMPASS <- COMPASS[COMPASS$DomainTotals>0,]

COMPASS_Baseline_Pats <- COMPASS %>% filter(Year==0) %>% filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM) %>% distinct() %>%
  inner_join(AllMSA_Pop_Baseline_671)


# ----------------
# Overall MSA Entire -------------------------------

# ALL COMPASS Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(COMPASS) %>% filter(Year==0) %>% 
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Vasomotor),
    sd=sd(Vasomotor),
    median=median(Vasomotor),
    Q1 = quantile(Vasomotor, 0.25),
    Q3 = quantile(Vasomotor, 0.75),
    min=min(Vasomotor),
    max=max(Vasomotor),
    n=n()
  )



# ALL COMPASS Total Year 1

AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Vasomotor),
    sd=sd(Vasomotor),
    median=median(Vasomotor),
    Q1 = quantile(Vasomotor, 0.25),
    Q3 = quantile(Vasomotor, 0.75),
    min=min(Vasomotor),
    max=max(Vasomotor),
    n=n()
  )

# Change from baseline

AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Vasomotor) %>%
  rename("Baseline"="Vasomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Vasomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Vasomotor)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  summarise(mean= mean((Vasomotor-Baseline)/TIME_STUDY)) 



AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Vasomotor) %>%
  rename("Baseline"="Vasomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Vasomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Vasomotor)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  mutate(Diff= (Vasomotor-Baseline)) %>% 
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )




AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Vasomotor) %>%
  rename("Baseline"="Vasomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Vasomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Vasomotor)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
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



AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Vasomotor) %>%
  rename("Baseline"="Vasomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Vasomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Vasomotor)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  mutate(Diff= (Vasomotor-Baseline)/TIME_STUDY) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



# ALL COMPASS Total Year 2

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Vasomotor),
    sd=sd(Vasomotor),
    median=median(Vasomotor),
    Q1 = quantile(Vasomotor, 0.25),
    Q3 = quantile(Vasomotor, 0.75),
    min=min(Vasomotor),
    max=max(Vasomotor),
    n=n()
  )


# Change from baseline

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Vasomotor) %>%
  rename("Baseline"="Vasomotor") %>% 
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Vasomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Vasomotor)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  summarise(mean= mean((Vasomotor-Baseline)/TIME_STUDY)) 



AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Vasomotor) %>%
  rename("Baseline"="Vasomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Vasomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Vasomotor)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  mutate(Diff= (Vasomotor-Baseline)) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Vasomotor) %>%
  rename("Baseline"="Vasomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Vasomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Vasomotor)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
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


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Vasomotor) %>%
  rename("Baseline"="Vasomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Vasomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Vasomotor)
  ) %>% 
  mutate(Diff= (Vasomotor-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


# ALL COMPASS Total Year 3

AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Vasomotor),
    sd=sd(Vasomotor),
    median=median(Vasomotor),
    Q1 = quantile(Vasomotor, 0.25),
    Q3 = quantile(Vasomotor, 0.75),
    min=min(Vasomotor),
    max=max(Vasomotor),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Vasomotor) %>%
  rename("Baseline"="Vasomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Vasomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Vasomotor)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  summarise(mean= mean((Vasomotor-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Vasomotor) %>%
  rename("Baseline"="Vasomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Vasomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Vasomotor)
  ) %>%  
  mutate(Diff= (Vasomotor-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Vasomotor) %>%
  rename("Baseline"="Vasomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Vasomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Vasomotor)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Vasomotor) %>%
  rename("Baseline"="Vasomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Vasomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Vasomotor)
  ) %>% 
  mutate(Diff= (Vasomotor-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
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
# ALL COMPASS Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Vasomotor),
    sd=sd(Vasomotor),
    median=median(Vasomotor),
    Q1 = quantile(Vasomotor, 0.25),
    Q3 = quantile(Vasomotor, 0.75),
    min=min(Vasomotor),
    max=max(Vasomotor),
    n=n()
  )



# ALL COMPASS Total Year 1

AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Vasomotor),
    sd=sd(Vasomotor),
    median=median(Vasomotor),
    Q1 = quantile(Vasomotor, 0.25),
    Q3 = quantile(Vasomotor, 0.75),
    min=min(Vasomotor),
    max=max(Vasomotor),
    n=n()
  )

# Change from baseline

AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Vasomotor) %>%
  rename("Baseline"="Vasomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Vasomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Vasomotor)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((Vasomotor-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Vasomotor) %>%
  rename("Baseline"="Vasomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Vasomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Vasomotor)
  ) %>% 
  mutate(Diff= (Vasomotor-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Vasomotor) %>%
  rename("Baseline"="Vasomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Vasomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Vasomotor)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Vasomotor) %>%
  rename("Baseline"="Vasomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Vasomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Vasomotor)
  ) %>% 
  mutate(Diff= (Vasomotor-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



# ALL COMPASS Total Year 2

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Vasomotor),
    sd=sd(Vasomotor),
    median=median(Vasomotor),
    Q1 = quantile(Vasomotor, 0.25),
    Q3 = quantile(Vasomotor, 0.75),
    min=min(Vasomotor),
    max=max(Vasomotor),
    n=n()
  )


# Change from baseline

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Vasomotor) %>%
  rename("Baseline"="Vasomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Vasomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Vasomotor)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((Vasomotor-Baseline)/TIME_STUDY)) 



AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Vasomotor) %>%
  rename("Baseline"="Vasomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Vasomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Vasomotor)
  ) %>% 
  mutate(Diff= (Vasomotor-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Vasomotor) %>%
  rename("Baseline"="Vasomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Vasomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Vasomotor)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
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


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Vasomotor) %>%
  rename("Baseline"="Vasomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Vasomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Vasomotor)
  ) %>% 
  mutate(Diff= (Vasomotor-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
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


# ALL COMPASS Total Year 3

AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Vasomotor),
    sd=sd(Vasomotor),
    median=median(Vasomotor),
    Q1 = quantile(Vasomotor, 0.25),
    Q3 = quantile(Vasomotor, 0.75),
    min=min(Vasomotor),
    max=max(Vasomotor),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Vasomotor) %>%
  rename("Baseline"="Vasomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Vasomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Vasomotor)
  ) %>% group_by(DIAG) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(mean= mean((Vasomotor-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Vasomotor) %>%
  rename("Baseline"="Vasomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Vasomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Vasomotor)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  mutate(Diff= (Vasomotor-Baseline)) %>%
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


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Vasomotor) %>%
  rename("Baseline"="Vasomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Vasomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Vasomotor)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
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


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Vasomotor) %>%
  rename("Baseline"="Vasomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Vasomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Vasomotor)
  ) %>% 
  mutate(Diff= (Vasomotor-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
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

# ALL COMPASS Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Vasomotor),
    sd=sd(Vasomotor),
    median=median(Vasomotor),
    Q1 = quantile(Vasomotor, 0.25),
    Q3 = quantile(Vasomotor, 0.75),
    min=min(Vasomotor),
    max=max(Vasomotor),
    n=n()
  )



# ALL COMPASS Total Year 1

AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Vasomotor),
    sd=sd(Vasomotor),
    median=median(Vasomotor),
    Q1 = quantile(Vasomotor, 0.25),
    Q3 = quantile(Vasomotor, 0.75),
    min=min(Vasomotor),
    max=max(Vasomotor),
    n=n()
  )

# Change from baseline


AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Vasomotor) %>%
  rename("Baseline"="Vasomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Vasomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Vasomotor)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((Vasomotor-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Vasomotor) %>%
  rename("Baseline"="Vasomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Vasomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Vasomotor)
  ) %>% 
  mutate(Diff= (Vasomotor-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
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


AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Vasomotor) %>%
  rename("Baseline"="Vasomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Vasomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Vasomotor)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
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


AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Vasomotor) %>%
  rename("Baseline"="Vasomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Vasomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Vasomotor)
  ) %>% 
  mutate(Diff= (Vasomotor-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



# ALL COMPASS Total Year 2

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Vasomotor),
    sd=sd(Vasomotor),
    median=median(Vasomotor),
    Q1 = quantile(Vasomotor, 0.25),
    Q3 = quantile(Vasomotor, 0.75),
    min=min(Vasomotor),
    max=max(Vasomotor),
    n=n()
  )


# Change from baseline

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Vasomotor) %>%
  rename("Baseline"="Vasomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Vasomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Vasomotor)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((Vasomotor-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Vasomotor) %>%
  rename("Baseline"="Vasomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Vasomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Vasomotor)
  ) %>% 
  mutate(Diff= (Vasomotor-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
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


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Vasomotor) %>%
  rename("Baseline"="Vasomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Vasomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Vasomotor)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
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


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Vasomotor) %>%
  rename("Baseline"="Vasomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Vasomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Vasomotor)
  ) %>% 
  mutate(Diff= (Vasomotor-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
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


# ALL COMPASS Total Year 3

AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Vasomotor),
    sd=sd(Vasomotor),
    median=median(Vasomotor),
    Q1 = quantile(Vasomotor, 0.25),
    Q3 = quantile(Vasomotor, 0.75),
    min=min(Vasomotor),
    max=max(Vasomotor),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Vasomotor) %>%
  rename("Baseline"="Vasomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Vasomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Vasomotor)
  ) %>% group_by(DIAGNIV) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(mean= mean((Vasomotor-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Vasomotor) %>%
  rename("Baseline"="Vasomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Vasomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Vasomotor)
  ) %>% 
  mutate(Diff= (Vasomotor-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
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


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Vasomotor) %>%
  rename("Baseline"="Vasomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Vasomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Vasomotor)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Vasomotor) %>%
  rename("Baseline"="Vasomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Vasomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Vasomotor)
  ) %>% 
  mutate(Diff= (Vasomotor-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
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

# ALL COMPASS Total Year 0 


EarlyCT_Pop_Baseline_319 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Vasomotor),
    sd=sd(Vasomotor),
    median=median(Vasomotor),
    Q1 = quantile(Vasomotor, 0.25),
    Q3 = quantile(Vasomotor, 0.75),
    min=min(Vasomotor),
    max=max(Vasomotor),
    n=n()
  )



# ALL COMPASS Total Year 1

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Vasomotor),
    sd=sd(Vasomotor),
    median=median(Vasomotor),
    Q1 = quantile(Vasomotor, 0.25),
    Q3 = quantile(Vasomotor, 0.75),
    min=min(Vasomotor),
    max=max(Vasomotor),
    n=n()
  )

# Change from baseline

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Vasomotor) %>%
  rename("Baseline"="Vasomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Vasomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Vasomotor)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  summarise(mean= mean((Vasomotor-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Vasomotor) %>%
  rename("Baseline"="Vasomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Vasomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Vasomotor)
  ) %>% 
  mutate(Diff= (Vasomotor-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Vasomotor) %>%
  rename("Baseline"="Vasomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Vasomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Vasomotor)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Vasomotor) %>%
  rename("Baseline"="Vasomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Vasomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Vasomotor)
  ) %>% 
  mutate(Diff= (Vasomotor-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



# ALL COMPASS Total Year 2

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Vasomotor),
    sd=sd(Vasomotor),
    median=median(Vasomotor),
    Q1 = quantile(Vasomotor, 0.25),
    Q3 = quantile(Vasomotor, 0.75),
    min=min(Vasomotor),
    max=max(Vasomotor),
    n=n()
  )


# Change from baseline

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Vasomotor) %>%
  rename("Baseline"="Vasomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Vasomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Vasomotor)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  summarise(mean= mean((Vasomotor-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Vasomotor) %>%
  rename("Baseline"="Vasomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Vasomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Vasomotor)
  ) %>% 
  mutate(Diff= (Vasomotor-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Vasomotor) %>%
  rename("Baseline"="Vasomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Vasomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Vasomotor)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Vasomotor) %>%
  rename("Baseline"="Vasomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Vasomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Vasomotor)
  ) %>% 
  mutate(Diff= (Vasomotor-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


# ALL COMPASS Total Year 3

EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Vasomotor),
    sd=sd(Vasomotor),
    median=median(Vasomotor),
    Q1 = quantile(Vasomotor, 0.25),
    Q3 = quantile(Vasomotor, 0.75),
    min=min(Vasomotor),
    max=max(Vasomotor),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Vasomotor) %>%
  rename("Baseline"="Vasomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Vasomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Vasomotor)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  summarise(mean= mean((Vasomotor-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Vasomotor) %>%
  rename("Baseline"="Vasomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Vasomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Vasomotor)
  ) %>% 
  mutate(Diff= (Vasomotor-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Vasomotor) %>%
  rename("Baseline"="Vasomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Vasomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Vasomotor)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Vasomotor) %>%
  rename("Baseline"="Vasomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Vasomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Vasomotor)
  ) %>% 
  mutate(Diff= (Vasomotor-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
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
# ALL COMPASS Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Vasomotor),
    sd=sd(Vasomotor),
    median=median(Vasomotor),
    Q1 = quantile(Vasomotor, 0.25),
    Q3 = quantile(Vasomotor, 0.75),
    min=min(Vasomotor),
    max=max(Vasomotor),
    n=n()
  )



# ALL COMPASS Total Year 1

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Vasomotor),
    sd=sd(Vasomotor),
    median=median(Vasomotor),
    Q1 = quantile(Vasomotor, 0.25),
    Q3 = quantile(Vasomotor, 0.75),
    min=min(Vasomotor),
    max=max(Vasomotor),
    n=n()
  )

# Change from baseline

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Vasomotor) %>%
  rename("Baseline"="Vasomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Vasomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Vasomotor)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((Vasomotor-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Vasomotor) %>%
  rename("Baseline"="Vasomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Vasomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Vasomotor)
  ) %>% 
  mutate(Diff= (Vasomotor-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Vasomotor) %>%
  rename("Baseline"="Vasomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Vasomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Vasomotor)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Vasomotor) %>%
  rename("Baseline"="Vasomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Vasomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Vasomotor)
  ) %>% 
  mutate(Diff= (Vasomotor-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



# ALL COMPASS Total Year 2

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Vasomotor),
    sd=sd(Vasomotor),
    median=median(Vasomotor),
    Q1 = quantile(Vasomotor, 0.25),
    Q3 = quantile(Vasomotor, 0.75),
    min=min(Vasomotor),
    max=max(Vasomotor),
    n=n()
  )


# Change from baseline

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Vasomotor) %>%
  rename("Baseline"="Vasomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Vasomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Vasomotor)
  ) %>% inner_join(COMPASS_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((Vasomotor-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Vasomotor) %>%
  rename("Baseline"="Vasomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Vasomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Vasomotor)
  ) %>% 
  mutate(Diff= (Vasomotor-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
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


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Vasomotor) %>%
  rename("Baseline"="Vasomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Vasomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Vasomotor)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
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


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Vasomotor) %>%
  rename("Baseline"="Vasomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Vasomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Vasomotor)
  ) %>% 
  mutate(Diff= (Vasomotor-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
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


# ALL COMPASS Total Year 3

EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Vasomotor),
    sd=sd(Vasomotor),
    median=median(Vasomotor),
    Q1 = quantile(Vasomotor, 0.25),
    Q3 = quantile(Vasomotor, 0.75),
    min=min(Vasomotor),
    max=max(Vasomotor),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Vasomotor) %>%
  rename("Baseline"="Vasomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Vasomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Vasomotor)
  ) %>% group_by(DIAG) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(mean= mean((Vasomotor-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Vasomotor) %>%
  rename("Baseline"="Vasomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Vasomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Vasomotor)
  ) %>% 
  mutate(Diff= (Vasomotor-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Vasomotor) %>%
  rename("Baseline"="Vasomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Vasomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Vasomotor)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Vasomotor) %>%
  rename("Baseline"="Vasomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Vasomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Vasomotor)
  ) %>% 
  mutate(Diff= (Vasomotor-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
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

# ALL COMPASS Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Vasomotor),
    sd=sd(Vasomotor),
    median=median(Vasomotor),
    Q1 = quantile(Vasomotor, 0.25),
    Q3 = quantile(Vasomotor, 0.75),
    min=min(Vasomotor),
    max=max(Vasomotor),
    n=n()
  )



# ALL COMPASS Total Year 1

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Vasomotor),
    sd=sd(Vasomotor),
    median=median(Vasomotor),
    Q1 = quantile(Vasomotor, 0.25),
    Q3 = quantile(Vasomotor, 0.75),
    min=min(Vasomotor),
    max=max(Vasomotor),
    n=n()
  )

# Change from baseline


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Vasomotor) %>%
  rename("Baseline"="Vasomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Vasomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Vasomotor)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((Vasomotor-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Vasomotor) %>%
  rename("Baseline"="Vasomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Vasomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Vasomotor)
  ) %>% 
  mutate(Diff= (Vasomotor-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
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




EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Vasomotor) %>%
  rename("Baseline"="Vasomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Vasomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Vasomotor)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
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




EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Vasomotor) %>%
  rename("Baseline"="Vasomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Vasomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Vasomotor)
  ) %>% 
  mutate(Diff= (Vasomotor-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



# ALL COMPASS Total Year 2

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Vasomotor),
    sd=sd(Vasomotor),
    median=median(Vasomotor),
    Q1 = quantile(Vasomotor, 0.25),
    Q3 = quantile(Vasomotor, 0.75),
    min=min(Vasomotor),
    max=max(Vasomotor),
    n=n()
  )


# Change from baseline

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Vasomotor) %>%
  rename("Baseline"="Vasomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Vasomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Vasomotor)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((Vasomotor-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Vasomotor) %>%
  rename("Baseline"="Vasomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Vasomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Vasomotor)
  ) %>% 
  mutate(Diff= (Vasomotor-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Vasomotor) %>%
  rename("Baseline"="Vasomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Vasomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Vasomotor)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Vasomotor) %>%
  rename("Baseline"="Vasomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Vasomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Vasomotor)
  ) %>% 
  mutate(Diff= (Vasomotor-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
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


# ALL COMPASS Total Year 3

EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Vasomotor),
    sd=sd(Vasomotor),
    median=median(Vasomotor),
    Q1 = quantile(Vasomotor, 0.25),
    Q3 = quantile(Vasomotor, 0.75),
    min=min(Vasomotor),
    max=max(Vasomotor),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Vasomotor) %>%
  rename("Baseline"="Vasomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Vasomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Vasomotor)
  ) %>% group_by(DIAGNIV) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(mean= mean((Vasomotor-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Vasomotor) %>%
  rename("Baseline"="Vasomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Vasomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Vasomotor)
  ) %>% 
  mutate(Diff= (Vasomotor-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Vasomotor) %>%
  rename("Baseline"="Vasomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Vasomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Vasomotor)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Vasomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Vasomotor) %>%
  rename("Baseline"="Vasomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Vasomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Vasomotor)
  ) %>% 
  mutate(Diff= (Vasomotor-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
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


# Inputs COMPASS31 Secretomotor ----------------------------------

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


COMPASS <- COMPASS %>% select(NUM, DATECONSULT, TIME_STUDY, Year, DomainTotals, Vasomotor, Secretomotor, Gastrointestinal, Bladder, Pupillomotor, Orthostatic)

range(COMPASS$Secretomotor)

COMPASS <- COMPASS[COMPASS$DomainTotals>0,]

COMPASS_Baseline_Pats <- COMPASS %>% filter(Year==0) %>% filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM) %>% distinct() %>%
  inner_join(AllMSA_Pop_Baseline_671)


# ----------------
# Overall MSA Entire -------------------------------

# ALL COMPASS Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(COMPASS) %>% filter(Year==0) %>% 
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Secretomotor),
    sd=sd(Secretomotor),
    median=median(Secretomotor),
    Q1 = quantile(Secretomotor, 0.25),
    Q3 = quantile(Secretomotor, 0.75),
    min=min(Secretomotor),
    max=max(Secretomotor),
    n=n()
  )



# ALL COMPASS Total Year 1

AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Secretomotor),
    sd=sd(Secretomotor),
    median=median(Secretomotor),
    Q1 = quantile(Secretomotor, 0.25),
    Q3 = quantile(Secretomotor, 0.75),
    min=min(Secretomotor),
    max=max(Secretomotor),
    n=n()
  )

# Change from baseline

AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Secretomotor) %>%
  rename("Baseline"="Secretomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Secretomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Secretomotor)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  summarise(mean= mean((Secretomotor-Baseline)/TIME_STUDY)) 



AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Secretomotor) %>%
  rename("Baseline"="Secretomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Secretomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Secretomotor)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  mutate(Diff= (Secretomotor-Baseline)) %>% 
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )




AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Secretomotor) %>%
  rename("Baseline"="Secretomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Secretomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Secretomotor)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
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



AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Secretomotor) %>%
  rename("Baseline"="Secretomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Secretomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Secretomotor)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  mutate(Diff= (Secretomotor-Baseline)/TIME_STUDY) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



# ALL COMPASS Total Year 2

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Secretomotor),
    sd=sd(Secretomotor),
    median=median(Secretomotor),
    Q1 = quantile(Secretomotor, 0.25),
    Q3 = quantile(Secretomotor, 0.75),
    min=min(Secretomotor),
    max=max(Secretomotor),
    n=n()
  )


# Change from baseline

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Secretomotor) %>%
  rename("Baseline"="Secretomotor") %>% 
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Secretomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Secretomotor)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  summarise(mean= mean((Secretomotor-Baseline)/TIME_STUDY)) 



AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Secretomotor) %>%
  rename("Baseline"="Secretomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Secretomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Secretomotor)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  mutate(Diff= (Secretomotor-Baseline)) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Secretomotor) %>%
  rename("Baseline"="Secretomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Secretomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Secretomotor)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
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


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Secretomotor) %>%
  rename("Baseline"="Secretomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Secretomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Secretomotor)
  ) %>% 
  mutate(Diff= (Secretomotor-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


# ALL COMPASS Total Year 3

AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Secretomotor),
    sd=sd(Secretomotor),
    median=median(Secretomotor),
    Q1 = quantile(Secretomotor, 0.25),
    Q3 = quantile(Secretomotor, 0.75),
    min=min(Secretomotor),
    max=max(Secretomotor),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Secretomotor) %>%
  rename("Baseline"="Secretomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Secretomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Secretomotor)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  summarise(mean= mean((Secretomotor-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Secretomotor) %>%
  rename("Baseline"="Secretomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Secretomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Secretomotor)
  ) %>%  
  mutate(Diff= (Secretomotor-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Secretomotor) %>%
  rename("Baseline"="Secretomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Secretomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Secretomotor)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Secretomotor) %>%
  rename("Baseline"="Secretomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Secretomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Secretomotor)
  ) %>% 
  mutate(Diff= (Secretomotor-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
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
# ALL COMPASS Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Secretomotor),
    sd=sd(Secretomotor),
    median=median(Secretomotor),
    Q1 = quantile(Secretomotor, 0.25),
    Q3 = quantile(Secretomotor, 0.75),
    min=min(Secretomotor),
    max=max(Secretomotor),
    n=n()
  )



# ALL COMPASS Total Year 1

AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Secretomotor),
    sd=sd(Secretomotor),
    median=median(Secretomotor),
    Q1 = quantile(Secretomotor, 0.25),
    Q3 = quantile(Secretomotor, 0.75),
    min=min(Secretomotor),
    max=max(Secretomotor),
    n=n()
  )

# Change from baseline

AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Secretomotor) %>%
  rename("Baseline"="Secretomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Secretomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Secretomotor)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((Secretomotor-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Secretomotor) %>%
  rename("Baseline"="Secretomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Secretomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Secretomotor)
  ) %>% 
  mutate(Diff= (Secretomotor-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Secretomotor) %>%
  rename("Baseline"="Secretomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Secretomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Secretomotor)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Secretomotor) %>%
  rename("Baseline"="Secretomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Secretomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Secretomotor)
  ) %>% 
  mutate(Diff= (Secretomotor-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



# ALL COMPASS Total Year 2

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Secretomotor),
    sd=sd(Secretomotor),
    median=median(Secretomotor),
    Q1 = quantile(Secretomotor, 0.25),
    Q3 = quantile(Secretomotor, 0.75),
    min=min(Secretomotor),
    max=max(Secretomotor),
    n=n()
  )


# Change from baseline

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Secretomotor) %>%
  rename("Baseline"="Secretomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Secretomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Secretomotor)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((Secretomotor-Baseline)/TIME_STUDY)) 



AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Secretomotor) %>%
  rename("Baseline"="Secretomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Secretomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Secretomotor)
  ) %>% 
  mutate(Diff= (Secretomotor-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Secretomotor) %>%
  rename("Baseline"="Secretomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Secretomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Secretomotor)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
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


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Secretomotor) %>%
  rename("Baseline"="Secretomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Secretomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Secretomotor)
  ) %>% 
  mutate(Diff= (Secretomotor-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
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


# ALL COMPASS Total Year 3

AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Secretomotor),
    sd=sd(Secretomotor),
    median=median(Secretomotor),
    Q1 = quantile(Secretomotor, 0.25),
    Q3 = quantile(Secretomotor, 0.75),
    min=min(Secretomotor),
    max=max(Secretomotor),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Secretomotor) %>%
  rename("Baseline"="Secretomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Secretomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Secretomotor)
  ) %>% group_by(DIAG) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(mean= mean((Secretomotor-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Secretomotor) %>%
  rename("Baseline"="Secretomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Secretomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Secretomotor)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  mutate(Diff= (Secretomotor-Baseline)) %>%
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


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Secretomotor) %>%
  rename("Baseline"="Secretomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Secretomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Secretomotor)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
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


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Secretomotor) %>%
  rename("Baseline"="Secretomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Secretomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Secretomotor)
  ) %>% 
  mutate(Diff= (Secretomotor-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
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

# ALL COMPASS Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Secretomotor),
    sd=sd(Secretomotor),
    median=median(Secretomotor),
    Q1 = quantile(Secretomotor, 0.25),
    Q3 = quantile(Secretomotor, 0.75),
    min=min(Secretomotor),
    max=max(Secretomotor),
    n=n()
  )



# ALL COMPASS Total Year 1

AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Secretomotor),
    sd=sd(Secretomotor),
    median=median(Secretomotor),
    Q1 = quantile(Secretomotor, 0.25),
    Q3 = quantile(Secretomotor, 0.75),
    min=min(Secretomotor),
    max=max(Secretomotor),
    n=n()
  )

# Change from baseline


AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Secretomotor) %>%
  rename("Baseline"="Secretomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Secretomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Secretomotor)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((Secretomotor-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Secretomotor) %>%
  rename("Baseline"="Secretomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Secretomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Secretomotor)
  ) %>% 
  mutate(Diff= (Secretomotor-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
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


AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Secretomotor) %>%
  rename("Baseline"="Secretomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Secretomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Secretomotor)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
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


AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Secretomotor) %>%
  rename("Baseline"="Secretomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Secretomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Secretomotor)
  ) %>% 
  mutate(Diff= (Secretomotor-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



# ALL COMPASS Total Year 2

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Secretomotor),
    sd=sd(Secretomotor),
    median=median(Secretomotor),
    Q1 = quantile(Secretomotor, 0.25),
    Q3 = quantile(Secretomotor, 0.75),
    min=min(Secretomotor),
    max=max(Secretomotor),
    n=n()
  )


# Change from baseline

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Secretomotor) %>%
  rename("Baseline"="Secretomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Secretomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Secretomotor)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((Secretomotor-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Secretomotor) %>%
  rename("Baseline"="Secretomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Secretomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Secretomotor)
  ) %>% 
  mutate(Diff= (Secretomotor-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
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


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Secretomotor) %>%
  rename("Baseline"="Secretomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Secretomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Secretomotor)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
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


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Secretomotor) %>%
  rename("Baseline"="Secretomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Secretomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Secretomotor)
  ) %>% 
  mutate(Diff= (Secretomotor-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
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


# ALL COMPASS Total Year 3

AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Secretomotor),
    sd=sd(Secretomotor),
    median=median(Secretomotor),
    Q1 = quantile(Secretomotor, 0.25),
    Q3 = quantile(Secretomotor, 0.75),
    min=min(Secretomotor),
    max=max(Secretomotor),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Secretomotor) %>%
  rename("Baseline"="Secretomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Secretomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Secretomotor)
  ) %>% group_by(DIAGNIV) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(mean= mean((Secretomotor-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Secretomotor) %>%
  rename("Baseline"="Secretomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Secretomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Secretomotor)
  ) %>% 
  mutate(Diff= (Secretomotor-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
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


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Secretomotor) %>%
  rename("Baseline"="Secretomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Secretomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Secretomotor)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Secretomotor) %>%
  rename("Baseline"="Secretomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Secretomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Secretomotor)
  ) %>% 
  mutate(Diff= (Secretomotor-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
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

# ALL COMPASS Total Year 0 


EarlyCT_Pop_Baseline_319 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Secretomotor),
    sd=sd(Secretomotor),
    median=median(Secretomotor),
    Q1 = quantile(Secretomotor, 0.25),
    Q3 = quantile(Secretomotor, 0.75),
    min=min(Secretomotor),
    max=max(Secretomotor),
    n=n()
  )



# ALL COMPASS Total Year 1

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Secretomotor),
    sd=sd(Secretomotor),
    median=median(Secretomotor),
    Q1 = quantile(Secretomotor, 0.25),
    Q3 = quantile(Secretomotor, 0.75),
    min=min(Secretomotor),
    max=max(Secretomotor),
    n=n()
  )

# Change from baseline

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Secretomotor) %>%
  rename("Baseline"="Secretomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Secretomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Secretomotor)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  summarise(mean= mean((Secretomotor-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Secretomotor) %>%
  rename("Baseline"="Secretomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Secretomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Secretomotor)
  ) %>% 
  mutate(Diff= (Secretomotor-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Secretomotor) %>%
  rename("Baseline"="Secretomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Secretomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Secretomotor)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Secretomotor) %>%
  rename("Baseline"="Secretomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Secretomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Secretomotor)
  ) %>% 
  mutate(Diff= (Secretomotor-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



# ALL COMPASS Total Year 2

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Secretomotor),
    sd=sd(Secretomotor),
    median=median(Secretomotor),
    Q1 = quantile(Secretomotor, 0.25),
    Q3 = quantile(Secretomotor, 0.75),
    min=min(Secretomotor),
    max=max(Secretomotor),
    n=n()
  )


# Change from baseline

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Secretomotor) %>%
  rename("Baseline"="Secretomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Secretomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Secretomotor)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  summarise(mean= mean((Secretomotor-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Secretomotor) %>%
  rename("Baseline"="Secretomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Secretomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Secretomotor)
  ) %>% 
  mutate(Diff= (Secretomotor-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Secretomotor) %>%
  rename("Baseline"="Secretomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Secretomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Secretomotor)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Secretomotor) %>%
  rename("Baseline"="Secretomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Secretomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Secretomotor)
  ) %>% 
  mutate(Diff= (Secretomotor-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


# ALL COMPASS Total Year 3

EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Secretomotor),
    sd=sd(Secretomotor),
    median=median(Secretomotor),
    Q1 = quantile(Secretomotor, 0.25),
    Q3 = quantile(Secretomotor, 0.75),
    min=min(Secretomotor),
    max=max(Secretomotor),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Secretomotor) %>%
  rename("Baseline"="Secretomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Secretomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Secretomotor)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  summarise(mean= mean((Secretomotor-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Secretomotor) %>%
  rename("Baseline"="Secretomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Secretomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Secretomotor)
  ) %>% 
  mutate(Diff= (Secretomotor-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Secretomotor) %>%
  rename("Baseline"="Secretomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Secretomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Secretomotor)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Secretomotor) %>%
  rename("Baseline"="Secretomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Secretomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Secretomotor)
  ) %>% 
  mutate(Diff= (Secretomotor-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
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
# ALL COMPASS Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Secretomotor),
    sd=sd(Secretomotor),
    median=median(Secretomotor),
    Q1 = quantile(Secretomotor, 0.25),
    Q3 = quantile(Secretomotor, 0.75),
    min=min(Secretomotor),
    max=max(Secretomotor),
    n=n()
  )



# ALL COMPASS Total Year 1

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Secretomotor),
    sd=sd(Secretomotor),
    median=median(Secretomotor),
    Q1 = quantile(Secretomotor, 0.25),
    Q3 = quantile(Secretomotor, 0.75),
    min=min(Secretomotor),
    max=max(Secretomotor),
    n=n()
  )

# Change from baseline

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Secretomotor) %>%
  rename("Baseline"="Secretomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Secretomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Secretomotor)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((Secretomotor-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Secretomotor) %>%
  rename("Baseline"="Secretomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Secretomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Secretomotor)
  ) %>% 
  mutate(Diff= (Secretomotor-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Secretomotor) %>%
  rename("Baseline"="Secretomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Secretomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Secretomotor)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Secretomotor) %>%
  rename("Baseline"="Secretomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Secretomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Secretomotor)
  ) %>% 
  mutate(Diff= (Secretomotor-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



# ALL COMPASS Total Year 2

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Secretomotor),
    sd=sd(Secretomotor),
    median=median(Secretomotor),
    Q1 = quantile(Secretomotor, 0.25),
    Q3 = quantile(Secretomotor, 0.75),
    min=min(Secretomotor),
    max=max(Secretomotor),
    n=n()
  )


# Change from baseline

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Secretomotor) %>%
  rename("Baseline"="Secretomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Secretomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Secretomotor)
  ) %>% inner_join(COMPASS_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((Secretomotor-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Secretomotor) %>%
  rename("Baseline"="Secretomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Secretomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Secretomotor)
  ) %>% 
  mutate(Diff= (Secretomotor-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
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


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Secretomotor) %>%
  rename("Baseline"="Secretomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Secretomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Secretomotor)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
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


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Secretomotor) %>%
  rename("Baseline"="Secretomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Secretomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Secretomotor)
  ) %>% 
  mutate(Diff= (Secretomotor-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
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


# ALL COMPASS Total Year 3

EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Secretomotor),
    sd=sd(Secretomotor),
    median=median(Secretomotor),
    Q1 = quantile(Secretomotor, 0.25),
    Q3 = quantile(Secretomotor, 0.75),
    min=min(Secretomotor),
    max=max(Secretomotor),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Secretomotor) %>%
  rename("Baseline"="Secretomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Secretomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Secretomotor)
  ) %>% group_by(DIAG) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(mean= mean((Secretomotor-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Secretomotor) %>%
  rename("Baseline"="Secretomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Secretomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Secretomotor)
  ) %>% 
  mutate(Diff= (Secretomotor-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Secretomotor) %>%
  rename("Baseline"="Secretomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Secretomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Secretomotor)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Secretomotor) %>%
  rename("Baseline"="Secretomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Secretomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Secretomotor)
  ) %>% 
  mutate(Diff= (Secretomotor-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
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

# ALL COMPASS Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Secretomotor),
    sd=sd(Secretomotor),
    median=median(Secretomotor),
    Q1 = quantile(Secretomotor, 0.25),
    Q3 = quantile(Secretomotor, 0.75),
    min=min(Secretomotor),
    max=max(Secretomotor),
    n=n()
  )



# ALL COMPASS Total Year 1

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Secretomotor),
    sd=sd(Secretomotor),
    median=median(Secretomotor),
    Q1 = quantile(Secretomotor, 0.25),
    Q3 = quantile(Secretomotor, 0.75),
    min=min(Secretomotor),
    max=max(Secretomotor),
    n=n()
  )

# Change from baseline


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Secretomotor) %>%
  rename("Baseline"="Secretomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Secretomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Secretomotor)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((Secretomotor-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Secretomotor) %>%
  rename("Baseline"="Secretomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Secretomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Secretomotor)
  ) %>% 
  mutate(Diff= (Secretomotor-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
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




EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Secretomotor) %>%
  rename("Baseline"="Secretomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Secretomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Secretomotor)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
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




EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Secretomotor) %>%
  rename("Baseline"="Secretomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Secretomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Secretomotor)
  ) %>% 
  mutate(Diff= (Secretomotor-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



# ALL COMPASS Total Year 2

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Secretomotor),
    sd=sd(Secretomotor),
    median=median(Secretomotor),
    Q1 = quantile(Secretomotor, 0.25),
    Q3 = quantile(Secretomotor, 0.75),
    min=min(Secretomotor),
    max=max(Secretomotor),
    n=n()
  )


# Change from baseline

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Secretomotor) %>%
  rename("Baseline"="Secretomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Secretomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Secretomotor)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((Secretomotor-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Secretomotor) %>%
  rename("Baseline"="Secretomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Secretomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Secretomotor)
  ) %>% 
  mutate(Diff= (Secretomotor-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Secretomotor) %>%
  rename("Baseline"="Secretomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Secretomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Secretomotor)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Secretomotor) %>%
  rename("Baseline"="Secretomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Secretomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Secretomotor)
  ) %>% 
  mutate(Diff= (Secretomotor-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
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


# ALL COMPASS Total Year 3

EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Secretomotor),
    sd=sd(Secretomotor),
    median=median(Secretomotor),
    Q1 = quantile(Secretomotor, 0.25),
    Q3 = quantile(Secretomotor, 0.75),
    min=min(Secretomotor),
    max=max(Secretomotor),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Secretomotor) %>%
  rename("Baseline"="Secretomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Secretomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Secretomotor)
  ) %>% group_by(DIAGNIV) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(mean= mean((Secretomotor-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Secretomotor) %>%
  rename("Baseline"="Secretomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Secretomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Secretomotor)
  ) %>% 
  mutate(Diff= (Secretomotor-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Secretomotor) %>%
  rename("Baseline"="Secretomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Secretomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Secretomotor)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Secretomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Secretomotor) %>%
  rename("Baseline"="Secretomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Secretomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Secretomotor)
  ) %>% 
  mutate(Diff= (Secretomotor-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
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


# Inputs COMPASS31 Gastrointestinal ----------------------------------

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


COMPASS <- COMPASS %>% select(NUM, DATECONSULT, TIME_STUDY, Year, DomainTotals, Vasomotor, Secretomotor, Gastrointestinal, Bladder, Pupillomotor, Orthostatic)

range(COMPASS$Gastrointestinal)

COMPASS <- COMPASS[COMPASS$DomainTotals>0,]

COMPASS_Baseline_Pats <- COMPASS %>% filter(Year==0) %>% filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM) %>% distinct() %>%
  inner_join(AllMSA_Pop_Baseline_671)


# ----------------
# Overall MSA Entire -------------------------------

# ALL COMPASS Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(COMPASS) %>% filter(Year==0) %>% 
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Gastrointestinal),
    sd=sd(Gastrointestinal),
    median=median(Gastrointestinal),
    Q1 = quantile(Gastrointestinal, 0.25),
    Q3 = quantile(Gastrointestinal, 0.75),
    min=min(Gastrointestinal),
    max=max(Gastrointestinal),
    n=n()
  )



# ALL COMPASS Total Year 1

AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Gastrointestinal),
    sd=sd(Gastrointestinal),
    median=median(Gastrointestinal),
    Q1 = quantile(Gastrointestinal, 0.25),
    Q3 = quantile(Gastrointestinal, 0.75),
    min=min(Gastrointestinal),
    max=max(Gastrointestinal),
    n=n()
  )

# Change from baseline

AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Gastrointestinal) %>%
  rename("Baseline"="Gastrointestinal") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Gastrointestinal)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Gastrointestinal)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  summarise(mean= mean((Gastrointestinal-Baseline)/TIME_STUDY)) 



AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Gastrointestinal) %>%
  rename("Baseline"="Gastrointestinal") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Gastrointestinal)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Gastrointestinal)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  mutate(Diff= (Gastrointestinal-Baseline)) %>% 
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )




AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Gastrointestinal) %>%
  rename("Baseline"="Gastrointestinal") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Gastrointestinal)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Gastrointestinal)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
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



AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Gastrointestinal) %>%
  rename("Baseline"="Gastrointestinal") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Gastrointestinal)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Gastrointestinal)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  mutate(Diff= (Gastrointestinal-Baseline)/TIME_STUDY) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



# ALL COMPASS Total Year 2

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Gastrointestinal),
    sd=sd(Gastrointestinal),
    median=median(Gastrointestinal),
    Q1 = quantile(Gastrointestinal, 0.25),
    Q3 = quantile(Gastrointestinal, 0.75),
    min=min(Gastrointestinal),
    max=max(Gastrointestinal),
    n=n()
  )


# Change from baseline

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Gastrointestinal) %>%
  rename("Baseline"="Gastrointestinal") %>% 
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Gastrointestinal)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Gastrointestinal)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  summarise(mean= mean((Gastrointestinal-Baseline)/TIME_STUDY)) 



AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Gastrointestinal) %>%
  rename("Baseline"="Gastrointestinal") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Gastrointestinal)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Gastrointestinal)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  mutate(Diff= (Gastrointestinal-Baseline)) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Gastrointestinal) %>%
  rename("Baseline"="Gastrointestinal") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Gastrointestinal)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Gastrointestinal)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
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


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Gastrointestinal) %>%
  rename("Baseline"="Gastrointestinal") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Gastrointestinal)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Gastrointestinal)
  ) %>% 
  mutate(Diff= (Gastrointestinal-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


# ALL COMPASS Total Year 3

AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Gastrointestinal),
    sd=sd(Gastrointestinal),
    median=median(Gastrointestinal),
    Q1 = quantile(Gastrointestinal, 0.25),
    Q3 = quantile(Gastrointestinal, 0.75),
    min=min(Gastrointestinal),
    max=max(Gastrointestinal),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Gastrointestinal) %>%
  rename("Baseline"="Gastrointestinal") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Gastrointestinal)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Gastrointestinal)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  summarise(mean= mean((Gastrointestinal-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Gastrointestinal) %>%
  rename("Baseline"="Gastrointestinal") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Gastrointestinal)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Gastrointestinal)
  ) %>%  
  mutate(Diff= (Gastrointestinal-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Gastrointestinal) %>%
  rename("Baseline"="Gastrointestinal") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Gastrointestinal)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Gastrointestinal)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Gastrointestinal) %>%
  rename("Baseline"="Gastrointestinal") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Gastrointestinal)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Gastrointestinal)
  ) %>% 
  mutate(Diff= (Gastrointestinal-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
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
# ALL COMPASS Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Gastrointestinal),
    sd=sd(Gastrointestinal),
    median=median(Gastrointestinal),
    Q1 = quantile(Gastrointestinal, 0.25),
    Q3 = quantile(Gastrointestinal, 0.75),
    min=min(Gastrointestinal),
    max=max(Gastrointestinal),
    n=n()
  )



# ALL COMPASS Total Year 1

AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Gastrointestinal),
    sd=sd(Gastrointestinal),
    median=median(Gastrointestinal),
    Q1 = quantile(Gastrointestinal, 0.25),
    Q3 = quantile(Gastrointestinal, 0.75),
    min=min(Gastrointestinal),
    max=max(Gastrointestinal),
    n=n()
  )

# Change from baseline

AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Gastrointestinal) %>%
  rename("Baseline"="Gastrointestinal") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Gastrointestinal)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Gastrointestinal)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((Gastrointestinal-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Gastrointestinal) %>%
  rename("Baseline"="Gastrointestinal") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Gastrointestinal)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Gastrointestinal)
  ) %>% 
  mutate(Diff= (Gastrointestinal-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Gastrointestinal) %>%
  rename("Baseline"="Gastrointestinal") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Gastrointestinal)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Gastrointestinal)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Gastrointestinal) %>%
  rename("Baseline"="Gastrointestinal") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Gastrointestinal)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Gastrointestinal)
  ) %>% 
  mutate(Diff= (Gastrointestinal-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



# ALL COMPASS Total Year 2

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Gastrointestinal),
    sd=sd(Gastrointestinal),
    median=median(Gastrointestinal),
    Q1 = quantile(Gastrointestinal, 0.25),
    Q3 = quantile(Gastrointestinal, 0.75),
    min=min(Gastrointestinal),
    max=max(Gastrointestinal),
    n=n()
  )


# Change from baseline

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Gastrointestinal) %>%
  rename("Baseline"="Gastrointestinal") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Gastrointestinal)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Gastrointestinal)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((Gastrointestinal-Baseline)/TIME_STUDY)) 



AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Gastrointestinal) %>%
  rename("Baseline"="Gastrointestinal") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Gastrointestinal)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Gastrointestinal)
  ) %>% 
  mutate(Diff= (Gastrointestinal-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Gastrointestinal) %>%
  rename("Baseline"="Gastrointestinal") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Gastrointestinal)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Gastrointestinal)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
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


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Gastrointestinal) %>%
  rename("Baseline"="Gastrointestinal") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Gastrointestinal)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Gastrointestinal)
  ) %>% 
  mutate(Diff= (Gastrointestinal-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
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


# ALL COMPASS Total Year 3

AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Gastrointestinal),
    sd=sd(Gastrointestinal),
    median=median(Gastrointestinal),
    Q1 = quantile(Gastrointestinal, 0.25),
    Q3 = quantile(Gastrointestinal, 0.75),
    min=min(Gastrointestinal),
    max=max(Gastrointestinal),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Gastrointestinal) %>%
  rename("Baseline"="Gastrointestinal") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Gastrointestinal)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Gastrointestinal)
  ) %>% group_by(DIAG) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(mean= mean((Gastrointestinal-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Gastrointestinal) %>%
  rename("Baseline"="Gastrointestinal") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Gastrointestinal)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Gastrointestinal)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  mutate(Diff= (Gastrointestinal-Baseline)) %>%
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


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Gastrointestinal) %>%
  rename("Baseline"="Gastrointestinal") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Gastrointestinal)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Gastrointestinal)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
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


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Gastrointestinal) %>%
  rename("Baseline"="Gastrointestinal") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Gastrointestinal)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Gastrointestinal)
  ) %>% 
  mutate(Diff= (Gastrointestinal-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
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

# ALL COMPASS Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Gastrointestinal),
    sd=sd(Gastrointestinal),
    median=median(Gastrointestinal),
    Q1 = quantile(Gastrointestinal, 0.25),
    Q3 = quantile(Gastrointestinal, 0.75),
    min=min(Gastrointestinal),
    max=max(Gastrointestinal),
    n=n()
  )



# ALL COMPASS Total Year 1

AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Gastrointestinal),
    sd=sd(Gastrointestinal),
    median=median(Gastrointestinal),
    Q1 = quantile(Gastrointestinal, 0.25),
    Q3 = quantile(Gastrointestinal, 0.75),
    min=min(Gastrointestinal),
    max=max(Gastrointestinal),
    n=n()
  )

# Change from baseline


AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Gastrointestinal) %>%
  rename("Baseline"="Gastrointestinal") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Gastrointestinal)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Gastrointestinal)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((Gastrointestinal-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Gastrointestinal) %>%
  rename("Baseline"="Gastrointestinal") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Gastrointestinal)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Gastrointestinal)
  ) %>% 
  mutate(Diff= (Gastrointestinal-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
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


AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Gastrointestinal) %>%
  rename("Baseline"="Gastrointestinal") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Gastrointestinal)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Gastrointestinal)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
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


AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Gastrointestinal) %>%
  rename("Baseline"="Gastrointestinal") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Gastrointestinal)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Gastrointestinal)
  ) %>% 
  mutate(Diff= (Gastrointestinal-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



# ALL COMPASS Total Year 2

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Gastrointestinal),
    sd=sd(Gastrointestinal),
    median=median(Gastrointestinal),
    Q1 = quantile(Gastrointestinal, 0.25),
    Q3 = quantile(Gastrointestinal, 0.75),
    min=min(Gastrointestinal),
    max=max(Gastrointestinal),
    n=n()
  )


# Change from baseline

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Gastrointestinal) %>%
  rename("Baseline"="Gastrointestinal") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Gastrointestinal)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Gastrointestinal)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((Gastrointestinal-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Gastrointestinal) %>%
  rename("Baseline"="Gastrointestinal") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Gastrointestinal)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Gastrointestinal)
  ) %>% 
  mutate(Diff= (Gastrointestinal-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
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


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Gastrointestinal) %>%
  rename("Baseline"="Gastrointestinal") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Gastrointestinal)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Gastrointestinal)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
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


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Gastrointestinal) %>%
  rename("Baseline"="Gastrointestinal") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Gastrointestinal)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Gastrointestinal)
  ) %>% 
  mutate(Diff= (Gastrointestinal-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
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


# ALL COMPASS Total Year 3

AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Gastrointestinal),
    sd=sd(Gastrointestinal),
    median=median(Gastrointestinal),
    Q1 = quantile(Gastrointestinal, 0.25),
    Q3 = quantile(Gastrointestinal, 0.75),
    min=min(Gastrointestinal),
    max=max(Gastrointestinal),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Gastrointestinal) %>%
  rename("Baseline"="Gastrointestinal") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Gastrointestinal)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Gastrointestinal)
  ) %>% group_by(DIAGNIV) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(mean= mean((Gastrointestinal-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Gastrointestinal) %>%
  rename("Baseline"="Gastrointestinal") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Gastrointestinal)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Gastrointestinal)
  ) %>% 
  mutate(Diff= (Gastrointestinal-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
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


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Gastrointestinal) %>%
  rename("Baseline"="Gastrointestinal") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Gastrointestinal)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Gastrointestinal)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Gastrointestinal) %>%
  rename("Baseline"="Gastrointestinal") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Gastrointestinal)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Gastrointestinal)
  ) %>% 
  mutate(Diff= (Gastrointestinal-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
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

# ALL COMPASS Total Year 0 


EarlyCT_Pop_Baseline_319 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Gastrointestinal),
    sd=sd(Gastrointestinal),
    median=median(Gastrointestinal),
    Q1 = quantile(Gastrointestinal, 0.25),
    Q3 = quantile(Gastrointestinal, 0.75),
    min=min(Gastrointestinal),
    max=max(Gastrointestinal),
    n=n()
  )



# ALL COMPASS Total Year 1

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Gastrointestinal),
    sd=sd(Gastrointestinal),
    median=median(Gastrointestinal),
    Q1 = quantile(Gastrointestinal, 0.25),
    Q3 = quantile(Gastrointestinal, 0.75),
    min=min(Gastrointestinal),
    max=max(Gastrointestinal),
    n=n()
  )

# Change from baseline

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Gastrointestinal) %>%
  rename("Baseline"="Gastrointestinal") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Gastrointestinal)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Gastrointestinal)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  summarise(mean= mean((Gastrointestinal-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Gastrointestinal) %>%
  rename("Baseline"="Gastrointestinal") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Gastrointestinal)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Gastrointestinal)
  ) %>% 
  mutate(Diff= (Gastrointestinal-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Gastrointestinal) %>%
  rename("Baseline"="Gastrointestinal") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Gastrointestinal)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Gastrointestinal)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Gastrointestinal) %>%
  rename("Baseline"="Gastrointestinal") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Gastrointestinal)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Gastrointestinal)
  ) %>% 
  mutate(Diff= (Gastrointestinal-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



# ALL COMPASS Total Year 2

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Gastrointestinal),
    sd=sd(Gastrointestinal),
    median=median(Gastrointestinal),
    Q1 = quantile(Gastrointestinal, 0.25),
    Q3 = quantile(Gastrointestinal, 0.75),
    min=min(Gastrointestinal),
    max=max(Gastrointestinal),
    n=n()
  )


# Change from baseline

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Gastrointestinal) %>%
  rename("Baseline"="Gastrointestinal") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Gastrointestinal)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Gastrointestinal)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  summarise(mean= mean((Gastrointestinal-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Gastrointestinal) %>%
  rename("Baseline"="Gastrointestinal") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Gastrointestinal)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Gastrointestinal)
  ) %>% 
  mutate(Diff= (Gastrointestinal-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Gastrointestinal) %>%
  rename("Baseline"="Gastrointestinal") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Gastrointestinal)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Gastrointestinal)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Gastrointestinal) %>%
  rename("Baseline"="Gastrointestinal") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Gastrointestinal)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Gastrointestinal)
  ) %>% 
  mutate(Diff= (Gastrointestinal-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


# ALL COMPASS Total Year 3

EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Gastrointestinal),
    sd=sd(Gastrointestinal),
    median=median(Gastrointestinal),
    Q1 = quantile(Gastrointestinal, 0.25),
    Q3 = quantile(Gastrointestinal, 0.75),
    min=min(Gastrointestinal),
    max=max(Gastrointestinal),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Gastrointestinal) %>%
  rename("Baseline"="Gastrointestinal") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Gastrointestinal)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Gastrointestinal)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  summarise(mean= mean((Gastrointestinal-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Gastrointestinal) %>%
  rename("Baseline"="Gastrointestinal") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Gastrointestinal)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Gastrointestinal)
  ) %>% 
  mutate(Diff= (Gastrointestinal-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Gastrointestinal) %>%
  rename("Baseline"="Gastrointestinal") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Gastrointestinal)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Gastrointestinal)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Gastrointestinal) %>%
  rename("Baseline"="Gastrointestinal") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Gastrointestinal)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Gastrointestinal)
  ) %>% 
  mutate(Diff= (Gastrointestinal-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
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
# ALL COMPASS Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Gastrointestinal),
    sd=sd(Gastrointestinal),
    median=median(Gastrointestinal),
    Q1 = quantile(Gastrointestinal, 0.25),
    Q3 = quantile(Gastrointestinal, 0.75),
    min=min(Gastrointestinal),
    max=max(Gastrointestinal),
    n=n()
  )



# ALL COMPASS Total Year 1

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Gastrointestinal),
    sd=sd(Gastrointestinal),
    median=median(Gastrointestinal),
    Q1 = quantile(Gastrointestinal, 0.25),
    Q3 = quantile(Gastrointestinal, 0.75),
    min=min(Gastrointestinal),
    max=max(Gastrointestinal),
    n=n()
  )

# Change from baseline

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Gastrointestinal) %>%
  rename("Baseline"="Gastrointestinal") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Gastrointestinal)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Gastrointestinal)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((Gastrointestinal-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Gastrointestinal) %>%
  rename("Baseline"="Gastrointestinal") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Gastrointestinal)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Gastrointestinal)
  ) %>% 
  mutate(Diff= (Gastrointestinal-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Gastrointestinal) %>%
  rename("Baseline"="Gastrointestinal") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Gastrointestinal)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Gastrointestinal)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Gastrointestinal) %>%
  rename("Baseline"="Gastrointestinal") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Gastrointestinal)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Gastrointestinal)
  ) %>% 
  mutate(Diff= (Gastrointestinal-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



# ALL COMPASS Total Year 2

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Gastrointestinal),
    sd=sd(Gastrointestinal),
    median=median(Gastrointestinal),
    Q1 = quantile(Gastrointestinal, 0.25),
    Q3 = quantile(Gastrointestinal, 0.75),
    min=min(Gastrointestinal),
    max=max(Gastrointestinal),
    n=n()
  )


# Change from baseline

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Gastrointestinal) %>%
  rename("Baseline"="Gastrointestinal") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Gastrointestinal)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Gastrointestinal)
  ) %>% inner_join(COMPASS_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((Gastrointestinal-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Gastrointestinal) %>%
  rename("Baseline"="Gastrointestinal") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Gastrointestinal)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Gastrointestinal)
  ) %>% 
  mutate(Diff= (Gastrointestinal-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
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


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Gastrointestinal) %>%
  rename("Baseline"="Gastrointestinal") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Gastrointestinal)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Gastrointestinal)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
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


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Gastrointestinal) %>%
  rename("Baseline"="Gastrointestinal") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Gastrointestinal)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Gastrointestinal)
  ) %>% 
  mutate(Diff= (Gastrointestinal-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
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


# ALL COMPASS Total Year 3

EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Gastrointestinal),
    sd=sd(Gastrointestinal),
    median=median(Gastrointestinal),
    Q1 = quantile(Gastrointestinal, 0.25),
    Q3 = quantile(Gastrointestinal, 0.75),
    min=min(Gastrointestinal),
    max=max(Gastrointestinal),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Gastrointestinal) %>%
  rename("Baseline"="Gastrointestinal") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Gastrointestinal)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Gastrointestinal)
  ) %>% group_by(DIAG) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(mean= mean((Gastrointestinal-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Gastrointestinal) %>%
  rename("Baseline"="Gastrointestinal") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Gastrointestinal)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Gastrointestinal)
  ) %>% 
  mutate(Diff= (Gastrointestinal-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Gastrointestinal) %>%
  rename("Baseline"="Gastrointestinal") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Gastrointestinal)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Gastrointestinal)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Gastrointestinal) %>%
  rename("Baseline"="Gastrointestinal") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Gastrointestinal)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Gastrointestinal)
  ) %>% 
  mutate(Diff= (Gastrointestinal-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
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

# ALL COMPASS Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Gastrointestinal),
    sd=sd(Gastrointestinal),
    median=median(Gastrointestinal),
    Q1 = quantile(Gastrointestinal, 0.25),
    Q3 = quantile(Gastrointestinal, 0.75),
    min=min(Gastrointestinal),
    max=max(Gastrointestinal),
    n=n()
  )



# ALL COMPASS Total Year 1

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Gastrointestinal),
    sd=sd(Gastrointestinal),
    median=median(Gastrointestinal),
    Q1 = quantile(Gastrointestinal, 0.25),
    Q3 = quantile(Gastrointestinal, 0.75),
    min=min(Gastrointestinal),
    max=max(Gastrointestinal),
    n=n()
  )

# Change from baseline


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Gastrointestinal) %>%
  rename("Baseline"="Gastrointestinal") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Gastrointestinal)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Gastrointestinal)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((Gastrointestinal-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Gastrointestinal) %>%
  rename("Baseline"="Gastrointestinal") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Gastrointestinal)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Gastrointestinal)
  ) %>% 
  mutate(Diff= (Gastrointestinal-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
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




EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Gastrointestinal) %>%
  rename("Baseline"="Gastrointestinal") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Gastrointestinal)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Gastrointestinal)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
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




EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Gastrointestinal) %>%
  rename("Baseline"="Gastrointestinal") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Gastrointestinal)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Gastrointestinal)
  ) %>% 
  mutate(Diff= (Gastrointestinal-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



# ALL COMPASS Total Year 2

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Gastrointestinal),
    sd=sd(Gastrointestinal),
    median=median(Gastrointestinal),
    Q1 = quantile(Gastrointestinal, 0.25),
    Q3 = quantile(Gastrointestinal, 0.75),
    min=min(Gastrointestinal),
    max=max(Gastrointestinal),
    n=n()
  )


# Change from baseline

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Gastrointestinal) %>%
  rename("Baseline"="Gastrointestinal") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Gastrointestinal)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Gastrointestinal)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((Gastrointestinal-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Gastrointestinal) %>%
  rename("Baseline"="Gastrointestinal") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Gastrointestinal)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Gastrointestinal)
  ) %>% 
  mutate(Diff= (Gastrointestinal-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Gastrointestinal) %>%
  rename("Baseline"="Gastrointestinal") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Gastrointestinal)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Gastrointestinal)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Gastrointestinal) %>%
  rename("Baseline"="Gastrointestinal") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Gastrointestinal)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Gastrointestinal)
  ) %>% 
  mutate(Diff= (Gastrointestinal-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
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


# ALL COMPASS Total Year 3

EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Gastrointestinal),
    sd=sd(Gastrointestinal),
    median=median(Gastrointestinal),
    Q1 = quantile(Gastrointestinal, 0.25),
    Q3 = quantile(Gastrointestinal, 0.75),
    min=min(Gastrointestinal),
    max=max(Gastrointestinal),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Gastrointestinal) %>%
  rename("Baseline"="Gastrointestinal") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Gastrointestinal)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Gastrointestinal)
  ) %>% group_by(DIAGNIV) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(mean= mean((Gastrointestinal-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Gastrointestinal) %>%
  rename("Baseline"="Gastrointestinal") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Gastrointestinal)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Gastrointestinal)
  ) %>% 
  mutate(Diff= (Gastrointestinal-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Gastrointestinal) %>%
  rename("Baseline"="Gastrointestinal") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Gastrointestinal)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Gastrointestinal)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Gastrointestinal)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Gastrointestinal) %>%
  rename("Baseline"="Gastrointestinal") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Gastrointestinal)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Gastrointestinal)
  ) %>% 
  mutate(Diff= (Gastrointestinal-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
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


# Inputs COMPASS31 Bladder ----------------------------------

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


COMPASS <- COMPASS %>% select(NUM, DATECONSULT, TIME_STUDY, Year, DomainTotals, Vasomotor, Secretomotor, Gastrointestinal, Bladder, Pupillomotor, Orthostatic)

range(COMPASS$Bladder)

COMPASS <- COMPASS[COMPASS$DomainTotals>0,]

COMPASS_Baseline_Pats <- COMPASS %>% filter(Year==0) %>% filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM) %>% distinct() %>%
  inner_join(AllMSA_Pop_Baseline_671)


# ----------------
# Overall MSA Entire -------------------------------

# ALL COMPASS Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(COMPASS) %>% filter(Year==0) %>% 
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Bladder),
    sd=sd(Bladder),
    median=median(Bladder),
    Q1 = quantile(Bladder, 0.25),
    Q3 = quantile(Bladder, 0.75),
    min=min(Bladder),
    max=max(Bladder),
    n=n()
  )



# ALL COMPASS Total Year 1

AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Bladder),
    sd=sd(Bladder),
    median=median(Bladder),
    Q1 = quantile(Bladder, 0.25),
    Q3 = quantile(Bladder, 0.75),
    min=min(Bladder),
    max=max(Bladder),
    n=n()
  )

# Change from baseline

AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Bladder) %>%
  rename("Baseline"="Bladder") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Bladder)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Bladder)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  summarise(mean= mean((Bladder-Baseline)/TIME_STUDY)) 



AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Bladder) %>%
  rename("Baseline"="Bladder") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Bladder)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Bladder)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  mutate(Diff= (Bladder-Baseline)) %>% 
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )




AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Bladder) %>%
  rename("Baseline"="Bladder") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Bladder)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Bladder)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
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



AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Bladder) %>%
  rename("Baseline"="Bladder") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Bladder)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Bladder)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  mutate(Diff= (Bladder-Baseline)/TIME_STUDY) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



# ALL COMPASS Total Year 2

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Bladder),
    sd=sd(Bladder),
    median=median(Bladder),
    Q1 = quantile(Bladder, 0.25),
    Q3 = quantile(Bladder, 0.75),
    min=min(Bladder),
    max=max(Bladder),
    n=n()
  )


# Change from baseline

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Bladder) %>%
  rename("Baseline"="Bladder") %>% 
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Bladder)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Bladder)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  summarise(mean= mean((Bladder-Baseline)/TIME_STUDY)) 



AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Bladder) %>%
  rename("Baseline"="Bladder") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Bladder)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Bladder)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  mutate(Diff= (Bladder-Baseline)) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Bladder) %>%
  rename("Baseline"="Bladder") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Bladder)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Bladder)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
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


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Bladder) %>%
  rename("Baseline"="Bladder") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Bladder)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Bladder)
  ) %>% 
  mutate(Diff= (Bladder-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


# ALL COMPASS Total Year 3

AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Bladder),
    sd=sd(Bladder),
    median=median(Bladder),
    Q1 = quantile(Bladder, 0.25),
    Q3 = quantile(Bladder, 0.75),
    min=min(Bladder),
    max=max(Bladder),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Bladder) %>%
  rename("Baseline"="Bladder") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Bladder)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Bladder)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  summarise(mean= mean((Bladder-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Bladder) %>%
  rename("Baseline"="Bladder") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Bladder)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Bladder)
  ) %>%  
  mutate(Diff= (Bladder-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Bladder) %>%
  rename("Baseline"="Bladder") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Bladder)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Bladder)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Bladder) %>%
  rename("Baseline"="Bladder") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Bladder)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Bladder)
  ) %>% 
  mutate(Diff= (Bladder-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
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
# ALL COMPASS Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Bladder),
    sd=sd(Bladder),
    median=median(Bladder),
    Q1 = quantile(Bladder, 0.25),
    Q3 = quantile(Bladder, 0.75),
    min=min(Bladder),
    max=max(Bladder),
    n=n()
  )



# ALL COMPASS Total Year 1

AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Bladder),
    sd=sd(Bladder),
    median=median(Bladder),
    Q1 = quantile(Bladder, 0.25),
    Q3 = quantile(Bladder, 0.75),
    min=min(Bladder),
    max=max(Bladder),
    n=n()
  )

# Change from baseline

AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Bladder) %>%
  rename("Baseline"="Bladder") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Bladder)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Bladder)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((Bladder-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Bladder) %>%
  rename("Baseline"="Bladder") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Bladder)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Bladder)
  ) %>% 
  mutate(Diff= (Bladder-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Bladder) %>%
  rename("Baseline"="Bladder") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Bladder)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Bladder)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Bladder) %>%
  rename("Baseline"="Bladder") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Bladder)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Bladder)
  ) %>% 
  mutate(Diff= (Bladder-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



# ALL COMPASS Total Year 2

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Bladder),
    sd=sd(Bladder),
    median=median(Bladder),
    Q1 = quantile(Bladder, 0.25),
    Q3 = quantile(Bladder, 0.75),
    min=min(Bladder),
    max=max(Bladder),
    n=n()
  )


# Change from baseline

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Bladder) %>%
  rename("Baseline"="Bladder") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Bladder)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Bladder)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((Bladder-Baseline)/TIME_STUDY)) 



AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Bladder) %>%
  rename("Baseline"="Bladder") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Bladder)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Bladder)
  ) %>% 
  mutate(Diff= (Bladder-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Bladder) %>%
  rename("Baseline"="Bladder") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Bladder)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Bladder)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
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


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Bladder) %>%
  rename("Baseline"="Bladder") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Bladder)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Bladder)
  ) %>% 
  mutate(Diff= (Bladder-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
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


# ALL COMPASS Total Year 3

AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Bladder),
    sd=sd(Bladder),
    median=median(Bladder),
    Q1 = quantile(Bladder, 0.25),
    Q3 = quantile(Bladder, 0.75),
    min=min(Bladder),
    max=max(Bladder),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Bladder) %>%
  rename("Baseline"="Bladder") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Bladder)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Bladder)
  ) %>% group_by(DIAG) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(mean= mean((Bladder-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Bladder) %>%
  rename("Baseline"="Bladder") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Bladder)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Bladder)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  mutate(Diff= (Bladder-Baseline)) %>%
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


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Bladder) %>%
  rename("Baseline"="Bladder") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Bladder)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Bladder)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
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


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Bladder) %>%
  rename("Baseline"="Bladder") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Bladder)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Bladder)
  ) %>% 
  mutate(Diff= (Bladder-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
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

# ALL COMPASS Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Bladder),
    sd=sd(Bladder),
    median=median(Bladder),
    Q1 = quantile(Bladder, 0.25),
    Q3 = quantile(Bladder, 0.75),
    min=min(Bladder),
    max=max(Bladder),
    n=n()
  )



# ALL COMPASS Total Year 1

AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Bladder),
    sd=sd(Bladder),
    median=median(Bladder),
    Q1 = quantile(Bladder, 0.25),
    Q3 = quantile(Bladder, 0.75),
    min=min(Bladder),
    max=max(Bladder),
    n=n()
  )

# Change from baseline


AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Bladder) %>%
  rename("Baseline"="Bladder") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Bladder)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Bladder)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((Bladder-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Bladder) %>%
  rename("Baseline"="Bladder") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Bladder)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Bladder)
  ) %>% 
  mutate(Diff= (Bladder-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
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


AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Bladder) %>%
  rename("Baseline"="Bladder") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Bladder)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Bladder)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
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


AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Bladder) %>%
  rename("Baseline"="Bladder") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Bladder)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Bladder)
  ) %>% 
  mutate(Diff= (Bladder-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



# ALL COMPASS Total Year 2

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Bladder),
    sd=sd(Bladder),
    median=median(Bladder),
    Q1 = quantile(Bladder, 0.25),
    Q3 = quantile(Bladder, 0.75),
    min=min(Bladder),
    max=max(Bladder),
    n=n()
  )


# Change from baseline

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Bladder) %>%
  rename("Baseline"="Bladder") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Bladder)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Bladder)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((Bladder-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Bladder) %>%
  rename("Baseline"="Bladder") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Bladder)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Bladder)
  ) %>% 
  mutate(Diff= (Bladder-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
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


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Bladder) %>%
  rename("Baseline"="Bladder") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Bladder)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Bladder)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
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


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Bladder) %>%
  rename("Baseline"="Bladder") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Bladder)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Bladder)
  ) %>% 
  mutate(Diff= (Bladder-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
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


# ALL COMPASS Total Year 3

AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Bladder),
    sd=sd(Bladder),
    median=median(Bladder),
    Q1 = quantile(Bladder, 0.25),
    Q3 = quantile(Bladder, 0.75),
    min=min(Bladder),
    max=max(Bladder),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Bladder) %>%
  rename("Baseline"="Bladder") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Bladder)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Bladder)
  ) %>% group_by(DIAGNIV) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(mean= mean((Bladder-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Bladder) %>%
  rename("Baseline"="Bladder") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Bladder)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Bladder)
  ) %>% 
  mutate(Diff= (Bladder-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
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


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Bladder) %>%
  rename("Baseline"="Bladder") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Bladder)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Bladder)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Bladder) %>%
  rename("Baseline"="Bladder") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Bladder)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Bladder)
  ) %>% 
  mutate(Diff= (Bladder-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
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

# ALL COMPASS Total Year 0 


EarlyCT_Pop_Baseline_319 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Bladder),
    sd=sd(Bladder),
    median=median(Bladder),
    Q1 = quantile(Bladder, 0.25),
    Q3 = quantile(Bladder, 0.75),
    min=min(Bladder),
    max=max(Bladder),
    n=n()
  )



# ALL COMPASS Total Year 1

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Bladder),
    sd=sd(Bladder),
    median=median(Bladder),
    Q1 = quantile(Bladder, 0.25),
    Q3 = quantile(Bladder, 0.75),
    min=min(Bladder),
    max=max(Bladder),
    n=n()
  )

# Change from baseline

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Bladder) %>%
  rename("Baseline"="Bladder") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Bladder)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Bladder)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  summarise(mean= mean((Bladder-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Bladder) %>%
  rename("Baseline"="Bladder") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Bladder)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Bladder)
  ) %>% 
  mutate(Diff= (Bladder-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Bladder) %>%
  rename("Baseline"="Bladder") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Bladder)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Bladder)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Bladder) %>%
  rename("Baseline"="Bladder") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Bladder)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Bladder)
  ) %>% 
  mutate(Diff= (Bladder-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



# ALL COMPASS Total Year 2

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Bladder),
    sd=sd(Bladder),
    median=median(Bladder),
    Q1 = quantile(Bladder, 0.25),
    Q3 = quantile(Bladder, 0.75),
    min=min(Bladder),
    max=max(Bladder),
    n=n()
  )


# Change from baseline

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Bladder) %>%
  rename("Baseline"="Bladder") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Bladder)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Bladder)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  summarise(mean= mean((Bladder-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Bladder) %>%
  rename("Baseline"="Bladder") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Bladder)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Bladder)
  ) %>% 
  mutate(Diff= (Bladder-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Bladder) %>%
  rename("Baseline"="Bladder") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Bladder)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Bladder)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Bladder) %>%
  rename("Baseline"="Bladder") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Bladder)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Bladder)
  ) %>% 
  mutate(Diff= (Bladder-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


# ALL COMPASS Total Year 3

EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Bladder),
    sd=sd(Bladder),
    median=median(Bladder),
    Q1 = quantile(Bladder, 0.25),
    Q3 = quantile(Bladder, 0.75),
    min=min(Bladder),
    max=max(Bladder),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Bladder) %>%
  rename("Baseline"="Bladder") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Bladder)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Bladder)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  summarise(mean= mean((Bladder-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Bladder) %>%
  rename("Baseline"="Bladder") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Bladder)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Bladder)
  ) %>% 
  mutate(Diff= (Bladder-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Bladder) %>%
  rename("Baseline"="Bladder") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Bladder)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Bladder)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Bladder) %>%
  rename("Baseline"="Bladder") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Bladder)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Bladder)
  ) %>% 
  mutate(Diff= (Bladder-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
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
# ALL COMPASS Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Bladder),
    sd=sd(Bladder),
    median=median(Bladder),
    Q1 = quantile(Bladder, 0.25),
    Q3 = quantile(Bladder, 0.75),
    min=min(Bladder),
    max=max(Bladder),
    n=n()
  )



# ALL COMPASS Total Year 1

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Bladder),
    sd=sd(Bladder),
    median=median(Bladder),
    Q1 = quantile(Bladder, 0.25),
    Q3 = quantile(Bladder, 0.75),
    min=min(Bladder),
    max=max(Bladder),
    n=n()
  )

# Change from baseline

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Bladder) %>%
  rename("Baseline"="Bladder") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Bladder)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Bladder)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((Bladder-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Bladder) %>%
  rename("Baseline"="Bladder") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Bladder)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Bladder)
  ) %>% 
  mutate(Diff= (Bladder-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Bladder) %>%
  rename("Baseline"="Bladder") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Bladder)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Bladder)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Bladder) %>%
  rename("Baseline"="Bladder") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Bladder)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Bladder)
  ) %>% 
  mutate(Diff= (Bladder-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



# ALL COMPASS Total Year 2

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Bladder),
    sd=sd(Bladder),
    median=median(Bladder),
    Q1 = quantile(Bladder, 0.25),
    Q3 = quantile(Bladder, 0.75),
    min=min(Bladder),
    max=max(Bladder),
    n=n()
  )


# Change from baseline

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Bladder) %>%
  rename("Baseline"="Bladder") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Bladder)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Bladder)
  ) %>% inner_join(COMPASS_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((Bladder-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Bladder) %>%
  rename("Baseline"="Bladder") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Bladder)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Bladder)
  ) %>% 
  mutate(Diff= (Bladder-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
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


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Bladder) %>%
  rename("Baseline"="Bladder") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Bladder)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Bladder)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
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


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Bladder) %>%
  rename("Baseline"="Bladder") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Bladder)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Bladder)
  ) %>% 
  mutate(Diff= (Bladder-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
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


# ALL COMPASS Total Year 3

EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Bladder),
    sd=sd(Bladder),
    median=median(Bladder),
    Q1 = quantile(Bladder, 0.25),
    Q3 = quantile(Bladder, 0.75),
    min=min(Bladder),
    max=max(Bladder),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Bladder) %>%
  rename("Baseline"="Bladder") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Bladder)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Bladder)
  ) %>% group_by(DIAG) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(mean= mean((Bladder-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Bladder) %>%
  rename("Baseline"="Bladder") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Bladder)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Bladder)
  ) %>% 
  mutate(Diff= (Bladder-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Bladder) %>%
  rename("Baseline"="Bladder") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Bladder)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Bladder)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Bladder) %>%
  rename("Baseline"="Bladder") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Bladder)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Bladder)
  ) %>% 
  mutate(Diff= (Bladder-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
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

# ALL COMPASS Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Bladder),
    sd=sd(Bladder),
    median=median(Bladder),
    Q1 = quantile(Bladder, 0.25),
    Q3 = quantile(Bladder, 0.75),
    min=min(Bladder),
    max=max(Bladder),
    n=n()
  )



# ALL COMPASS Total Year 1

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Bladder),
    sd=sd(Bladder),
    median=median(Bladder),
    Q1 = quantile(Bladder, 0.25),
    Q3 = quantile(Bladder, 0.75),
    min=min(Bladder),
    max=max(Bladder),
    n=n()
  )

# Change from baseline


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Bladder) %>%
  rename("Baseline"="Bladder") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Bladder)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Bladder)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((Bladder-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Bladder) %>%
  rename("Baseline"="Bladder") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Bladder)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Bladder)
  ) %>% 
  mutate(Diff= (Bladder-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
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




EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Bladder) %>%
  rename("Baseline"="Bladder") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Bladder)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Bladder)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
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




EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Bladder) %>%
  rename("Baseline"="Bladder") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Bladder)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Bladder)
  ) %>% 
  mutate(Diff= (Bladder-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



# ALL COMPASS Total Year 2

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Bladder),
    sd=sd(Bladder),
    median=median(Bladder),
    Q1 = quantile(Bladder, 0.25),
    Q3 = quantile(Bladder, 0.75),
    min=min(Bladder),
    max=max(Bladder),
    n=n()
  )


# Change from baseline

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Bladder) %>%
  rename("Baseline"="Bladder") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Bladder)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Bladder)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((Bladder-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Bladder) %>%
  rename("Baseline"="Bladder") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Bladder)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Bladder)
  ) %>% 
  mutate(Diff= (Bladder-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Bladder) %>%
  rename("Baseline"="Bladder") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Bladder)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Bladder)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Bladder) %>%
  rename("Baseline"="Bladder") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Bladder)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Bladder)
  ) %>% 
  mutate(Diff= (Bladder-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
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


# ALL COMPASS Total Year 3

EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Bladder),
    sd=sd(Bladder),
    median=median(Bladder),
    Q1 = quantile(Bladder, 0.25),
    Q3 = quantile(Bladder, 0.75),
    min=min(Bladder),
    max=max(Bladder),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Bladder) %>%
  rename("Baseline"="Bladder") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Bladder)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Bladder)
  ) %>% group_by(DIAGNIV) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(mean= mean((Bladder-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Bladder) %>%
  rename("Baseline"="Bladder") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Bladder)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Bladder)
  ) %>% 
  mutate(Diff= (Bladder-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Bladder) %>%
  rename("Baseline"="Bladder") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Bladder)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Bladder)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Bladder)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Bladder) %>%
  rename("Baseline"="Bladder") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Bladder)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Bladder)
  ) %>% 
  mutate(Diff= (Bladder-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
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


# Inputs COMPASS31 Pupillomotor ----------------------------------

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


COMPASS <- COMPASS %>% select(NUM, DATECONSULT, TIME_STUDY, Year, DomainTotals, Vasomotor, Secretomotor, Gastrointestinal, Bladder, Pupillomotor, Orthostatic)

range(COMPASS$Pupillomotor)

COMPASS <- COMPASS[COMPASS$DomainTotals>0,]

COMPASS_Baseline_Pats <- COMPASS %>% filter(Year==0) %>% filter(!is.na(DomainTotals)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM) %>% distinct() %>%
  inner_join(AllMSA_Pop_Baseline_671)


# ----------------
# Overall MSA Entire -------------------------------

# ALL COMPASS Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(COMPASS) %>% filter(Year==0) %>% 
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Pupillomotor),
    sd=sd(Pupillomotor),
    median=median(Pupillomotor),
    Q1 = quantile(Pupillomotor, 0.25),
    Q3 = quantile(Pupillomotor, 0.75),
    min=min(Pupillomotor),
    max=max(Pupillomotor),
    n=n()
  )



# ALL COMPASS Total Year 1

AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Pupillomotor),
    sd=sd(Pupillomotor),
    median=median(Pupillomotor),
    Q1 = quantile(Pupillomotor, 0.25),
    Q3 = quantile(Pupillomotor, 0.75),
    min=min(Pupillomotor),
    max=max(Pupillomotor),
    n=n()
  )

# Change from baseline

AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Pupillomotor) %>%
  rename("Baseline"="Pupillomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Pupillomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Pupillomotor)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  summarise(mean= mean((Pupillomotor-Baseline)/TIME_STUDY)) 



AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Pupillomotor) %>%
  rename("Baseline"="Pupillomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Pupillomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Pupillomotor)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  mutate(Diff= (Pupillomotor-Baseline)) %>% 
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )




AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Pupillomotor) %>%
  rename("Baseline"="Pupillomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Pupillomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Pupillomotor)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
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



AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Pupillomotor) %>%
  rename("Baseline"="Pupillomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Pupillomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Pupillomotor)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  mutate(Diff= (Pupillomotor-Baseline)/TIME_STUDY) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



# ALL COMPASS Total Year 2

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Pupillomotor),
    sd=sd(Pupillomotor),
    median=median(Pupillomotor),
    Q1 = quantile(Pupillomotor, 0.25),
    Q3 = quantile(Pupillomotor, 0.75),
    min=min(Pupillomotor),
    max=max(Pupillomotor),
    n=n()
  )


# Change from baseline

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Pupillomotor) %>%
  rename("Baseline"="Pupillomotor") %>% 
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Pupillomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Pupillomotor)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  summarise(mean= mean((Pupillomotor-Baseline)/TIME_STUDY)) 



AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Pupillomotor) %>%
  rename("Baseline"="Pupillomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Pupillomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Pupillomotor)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  mutate(Diff= (Pupillomotor-Baseline)) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Pupillomotor) %>%
  rename("Baseline"="Pupillomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Pupillomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Pupillomotor)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
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


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Pupillomotor) %>%
  rename("Baseline"="Pupillomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Pupillomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Pupillomotor)
  ) %>% 
  mutate(Diff= (Pupillomotor-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


# ALL COMPASS Total Year 3

AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Pupillomotor),
    sd=sd(Pupillomotor),
    median=median(Pupillomotor),
    Q1 = quantile(Pupillomotor, 0.25),
    Q3 = quantile(Pupillomotor, 0.75),
    min=min(Pupillomotor),
    max=max(Pupillomotor),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Pupillomotor) %>%
  rename("Baseline"="Pupillomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Pupillomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Pupillomotor)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  summarise(mean= mean((Pupillomotor-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Pupillomotor) %>%
  rename("Baseline"="Pupillomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Pupillomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Pupillomotor)
  ) %>%  
  mutate(Diff= (Pupillomotor-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Pupillomotor) %>%
  rename("Baseline"="Pupillomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Pupillomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Pupillomotor)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Pupillomotor) %>%
  rename("Baseline"="Pupillomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Pupillomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Pupillomotor)
  ) %>% 
  mutate(Diff= (Pupillomotor-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
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
# ALL COMPASS Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Pupillomotor),
    sd=sd(Pupillomotor),
    median=median(Pupillomotor),
    Q1 = quantile(Pupillomotor, 0.25),
    Q3 = quantile(Pupillomotor, 0.75),
    min=min(Pupillomotor),
    max=max(Pupillomotor),
    n=n()
  )



# ALL COMPASS Total Year 1

AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Pupillomotor),
    sd=sd(Pupillomotor),
    median=median(Pupillomotor),
    Q1 = quantile(Pupillomotor, 0.25),
    Q3 = quantile(Pupillomotor, 0.75),
    min=min(Pupillomotor),
    max=max(Pupillomotor),
    n=n()
  )

# Change from baseline

AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Pupillomotor) %>%
  rename("Baseline"="Pupillomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Pupillomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Pupillomotor)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((Pupillomotor-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Pupillomotor) %>%
  rename("Baseline"="Pupillomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Pupillomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Pupillomotor)
  ) %>% 
  mutate(Diff= (Pupillomotor-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Pupillomotor) %>%
  rename("Baseline"="Pupillomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Pupillomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Pupillomotor)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Pupillomotor) %>%
  rename("Baseline"="Pupillomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Pupillomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Pupillomotor)
  ) %>% 
  mutate(Diff= (Pupillomotor-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



# ALL COMPASS Total Year 2

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Pupillomotor),
    sd=sd(Pupillomotor),
    median=median(Pupillomotor),
    Q1 = quantile(Pupillomotor, 0.25),
    Q3 = quantile(Pupillomotor, 0.75),
    min=min(Pupillomotor),
    max=max(Pupillomotor),
    n=n()
  )


# Change from baseline

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Pupillomotor) %>%
  rename("Baseline"="Pupillomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Pupillomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Pupillomotor)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((Pupillomotor-Baseline)/TIME_STUDY)) 



AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Pupillomotor) %>%
  rename("Baseline"="Pupillomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Pupillomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Pupillomotor)
  ) %>% 
  mutate(Diff= (Pupillomotor-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Pupillomotor) %>%
  rename("Baseline"="Pupillomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Pupillomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Pupillomotor)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
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


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Pupillomotor) %>%
  rename("Baseline"="Pupillomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Pupillomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Pupillomotor)
  ) %>% 
  mutate(Diff= (Pupillomotor-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
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


# ALL COMPASS Total Year 3

AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Pupillomotor),
    sd=sd(Pupillomotor),
    median=median(Pupillomotor),
    Q1 = quantile(Pupillomotor, 0.25),
    Q3 = quantile(Pupillomotor, 0.75),
    min=min(Pupillomotor),
    max=max(Pupillomotor),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Pupillomotor) %>%
  rename("Baseline"="Pupillomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Pupillomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Pupillomotor)
  ) %>% group_by(DIAG) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(mean= mean((Pupillomotor-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Pupillomotor) %>%
  rename("Baseline"="Pupillomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Pupillomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Pupillomotor)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  mutate(Diff= (Pupillomotor-Baseline)) %>%
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


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Pupillomotor) %>%
  rename("Baseline"="Pupillomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Pupillomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Pupillomotor)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
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


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Pupillomotor) %>%
  rename("Baseline"="Pupillomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Pupillomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Pupillomotor)
  ) %>% 
  mutate(Diff= (Pupillomotor-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
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

# ALL COMPASS Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Pupillomotor),
    sd=sd(Pupillomotor),
    median=median(Pupillomotor),
    Q1 = quantile(Pupillomotor, 0.25),
    Q3 = quantile(Pupillomotor, 0.75),
    min=min(Pupillomotor),
    max=max(Pupillomotor),
    n=n()
  )



# ALL COMPASS Total Year 1

AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Pupillomotor),
    sd=sd(Pupillomotor),
    median=median(Pupillomotor),
    Q1 = quantile(Pupillomotor, 0.25),
    Q3 = quantile(Pupillomotor, 0.75),
    min=min(Pupillomotor),
    max=max(Pupillomotor),
    n=n()
  )

# Change from baseline


AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Pupillomotor) %>%
  rename("Baseline"="Pupillomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Pupillomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Pupillomotor)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((Pupillomotor-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Pupillomotor) %>%
  rename("Baseline"="Pupillomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Pupillomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Pupillomotor)
  ) %>% 
  mutate(Diff= (Pupillomotor-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
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


AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Pupillomotor) %>%
  rename("Baseline"="Pupillomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Pupillomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Pupillomotor)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
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


AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Pupillomotor) %>%
  rename("Baseline"="Pupillomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Pupillomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Pupillomotor)
  ) %>% 
  mutate(Diff= (Pupillomotor-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



# ALL COMPASS Total Year 2

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Pupillomotor),
    sd=sd(Pupillomotor),
    median=median(Pupillomotor),
    Q1 = quantile(Pupillomotor, 0.25),
    Q3 = quantile(Pupillomotor, 0.75),
    min=min(Pupillomotor),
    max=max(Pupillomotor),
    n=n()
  )


# Change from baseline

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Pupillomotor) %>%
  rename("Baseline"="Pupillomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Pupillomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Pupillomotor)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((Pupillomotor-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Pupillomotor) %>%
  rename("Baseline"="Pupillomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Pupillomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Pupillomotor)
  ) %>% 
  mutate(Diff= (Pupillomotor-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
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


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Pupillomotor) %>%
  rename("Baseline"="Pupillomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Pupillomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Pupillomotor)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
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


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Pupillomotor) %>%
  rename("Baseline"="Pupillomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Pupillomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Pupillomotor)
  ) %>% 
  mutate(Diff= (Pupillomotor-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
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


# ALL COMPASS Total Year 3

AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Pupillomotor),
    sd=sd(Pupillomotor),
    median=median(Pupillomotor),
    Q1 = quantile(Pupillomotor, 0.25),
    Q3 = quantile(Pupillomotor, 0.75),
    min=min(Pupillomotor),
    max=max(Pupillomotor),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Pupillomotor) %>%
  rename("Baseline"="Pupillomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Pupillomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Pupillomotor)
  ) %>% group_by(DIAGNIV) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(mean= mean((Pupillomotor-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Pupillomotor) %>%
  rename("Baseline"="Pupillomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Pupillomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Pupillomotor)
  ) %>% 
  mutate(Diff= (Pupillomotor-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
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


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Pupillomotor) %>%
  rename("Baseline"="Pupillomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Pupillomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Pupillomotor)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Pupillomotor) %>%
  rename("Baseline"="Pupillomotor") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Pupillomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Pupillomotor)
  ) %>% 
  mutate(Diff= (Pupillomotor-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
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

# ALL COMPASS Total Year 0 


EarlyCT_Pop_Baseline_319 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Pupillomotor),
    sd=sd(Pupillomotor),
    median=median(Pupillomotor),
    Q1 = quantile(Pupillomotor, 0.25),
    Q3 = quantile(Pupillomotor, 0.75),
    min=min(Pupillomotor),
    max=max(Pupillomotor),
    n=n()
  )



# ALL COMPASS Total Year 1

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Pupillomotor),
    sd=sd(Pupillomotor),
    median=median(Pupillomotor),
    Q1 = quantile(Pupillomotor, 0.25),
    Q3 = quantile(Pupillomotor, 0.75),
    min=min(Pupillomotor),
    max=max(Pupillomotor),
    n=n()
  )

# Change from baseline

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Pupillomotor) %>%
  rename("Baseline"="Pupillomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Pupillomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Pupillomotor)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  summarise(mean= mean((Pupillomotor-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Pupillomotor) %>%
  rename("Baseline"="Pupillomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Pupillomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Pupillomotor)
  ) %>% 
  mutate(Diff= (Pupillomotor-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Pupillomotor) %>%
  rename("Baseline"="Pupillomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Pupillomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Pupillomotor)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Pupillomotor) %>%
  rename("Baseline"="Pupillomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Pupillomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Pupillomotor)
  ) %>% 
  mutate(Diff= (Pupillomotor-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



# ALL COMPASS Total Year 2

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Pupillomotor),
    sd=sd(Pupillomotor),
    median=median(Pupillomotor),
    Q1 = quantile(Pupillomotor, 0.25),
    Q3 = quantile(Pupillomotor, 0.75),
    min=min(Pupillomotor),
    max=max(Pupillomotor),
    n=n()
  )


# Change from baseline

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Pupillomotor) %>%
  rename("Baseline"="Pupillomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Pupillomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Pupillomotor)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  summarise(mean= mean((Pupillomotor-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Pupillomotor) %>%
  rename("Baseline"="Pupillomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Pupillomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Pupillomotor)
  ) %>% 
  mutate(Diff= (Pupillomotor-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Pupillomotor) %>%
  rename("Baseline"="Pupillomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Pupillomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Pupillomotor)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Pupillomotor) %>%
  rename("Baseline"="Pupillomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Pupillomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Pupillomotor)
  ) %>% 
  mutate(Diff= (Pupillomotor-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


# ALL COMPASS Total Year 3

EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Pupillomotor),
    sd=sd(Pupillomotor),
    median=median(Pupillomotor),
    Q1 = quantile(Pupillomotor, 0.25),
    Q3 = quantile(Pupillomotor, 0.75),
    min=min(Pupillomotor),
    max=max(Pupillomotor),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Pupillomotor) %>%
  rename("Baseline"="Pupillomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Pupillomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Pupillomotor)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  summarise(mean= mean((Pupillomotor-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Pupillomotor) %>%
  rename("Baseline"="Pupillomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Pupillomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Pupillomotor)
  ) %>% 
  mutate(Diff= (Pupillomotor-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Pupillomotor) %>%
  rename("Baseline"="Pupillomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Pupillomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Pupillomotor)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Pupillomotor) %>%
  rename("Baseline"="Pupillomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Pupillomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Pupillomotor)
  ) %>% 
  mutate(Diff= (Pupillomotor-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
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
# ALL COMPASS Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Pupillomotor),
    sd=sd(Pupillomotor),
    median=median(Pupillomotor),
    Q1 = quantile(Pupillomotor, 0.25),
    Q3 = quantile(Pupillomotor, 0.75),
    min=min(Pupillomotor),
    max=max(Pupillomotor),
    n=n()
  )



# ALL COMPASS Total Year 1

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Pupillomotor),
    sd=sd(Pupillomotor),
    median=median(Pupillomotor),
    Q1 = quantile(Pupillomotor, 0.25),
    Q3 = quantile(Pupillomotor, 0.75),
    min=min(Pupillomotor),
    max=max(Pupillomotor),
    n=n()
  )

# Change from baseline

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Pupillomotor) %>%
  rename("Baseline"="Pupillomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Pupillomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Pupillomotor)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((Pupillomotor-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Pupillomotor) %>%
  rename("Baseline"="Pupillomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Pupillomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Pupillomotor)
  ) %>% 
  mutate(Diff= (Pupillomotor-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Pupillomotor) %>%
  rename("Baseline"="Pupillomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Pupillomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Pupillomotor)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Pupillomotor) %>%
  rename("Baseline"="Pupillomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Pupillomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Pupillomotor)
  ) %>% 
  mutate(Diff= (Pupillomotor-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



# ALL COMPASS Total Year 2

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Pupillomotor),
    sd=sd(Pupillomotor),
    median=median(Pupillomotor),
    Q1 = quantile(Pupillomotor, 0.25),
    Q3 = quantile(Pupillomotor, 0.75),
    min=min(Pupillomotor),
    max=max(Pupillomotor),
    n=n()
  )


# Change from baseline

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Pupillomotor) %>%
  rename("Baseline"="Pupillomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Pupillomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Pupillomotor)
  ) %>% inner_join(COMPASS_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((Pupillomotor-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Pupillomotor) %>%
  rename("Baseline"="Pupillomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Pupillomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Pupillomotor)
  ) %>% 
  mutate(Diff= (Pupillomotor-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
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


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Pupillomotor) %>%
  rename("Baseline"="Pupillomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Pupillomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Pupillomotor)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
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


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Pupillomotor) %>%
  rename("Baseline"="Pupillomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Pupillomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Pupillomotor)
  ) %>% 
  mutate(Diff= (Pupillomotor-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
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


# ALL COMPASS Total Year 3

EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Pupillomotor),
    sd=sd(Pupillomotor),
    median=median(Pupillomotor),
    Q1 = quantile(Pupillomotor, 0.25),
    Q3 = quantile(Pupillomotor, 0.75),
    min=min(Pupillomotor),
    max=max(Pupillomotor),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Pupillomotor) %>%
  rename("Baseline"="Pupillomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Pupillomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Pupillomotor)
  ) %>% group_by(DIAG) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(mean= mean((Pupillomotor-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Pupillomotor) %>%
  rename("Baseline"="Pupillomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Pupillomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Pupillomotor)
  ) %>% 
  mutate(Diff= (Pupillomotor-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Pupillomotor) %>%
  rename("Baseline"="Pupillomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Pupillomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Pupillomotor)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Pupillomotor) %>%
  rename("Baseline"="Pupillomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Pupillomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Pupillomotor)
  ) %>% 
  mutate(Diff= (Pupillomotor-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
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

# ALL COMPASS Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Pupillomotor),
    sd=sd(Pupillomotor),
    median=median(Pupillomotor),
    Q1 = quantile(Pupillomotor, 0.25),
    Q3 = quantile(Pupillomotor, 0.75),
    min=min(Pupillomotor),
    max=max(Pupillomotor),
    n=n()
  )



# ALL COMPASS Total Year 1

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Pupillomotor),
    sd=sd(Pupillomotor),
    median=median(Pupillomotor),
    Q1 = quantile(Pupillomotor, 0.25),
    Q3 = quantile(Pupillomotor, 0.75),
    min=min(Pupillomotor),
    max=max(Pupillomotor),
    n=n()
  )

# Change from baseline


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Pupillomotor) %>%
  rename("Baseline"="Pupillomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Pupillomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Pupillomotor)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((Pupillomotor-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Pupillomotor) %>%
  rename("Baseline"="Pupillomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Pupillomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Pupillomotor)
  ) %>% 
  mutate(Diff= (Pupillomotor-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
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




EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Pupillomotor) %>%
  rename("Baseline"="Pupillomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Pupillomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Pupillomotor)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
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




EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Pupillomotor) %>%
  rename("Baseline"="Pupillomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==1) %>%
      filter(!is.na(Pupillomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Pupillomotor)
  ) %>% 
  mutate(Diff= (Pupillomotor-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



# ALL COMPASS Total Year 2

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Pupillomotor),
    sd=sd(Pupillomotor),
    median=median(Pupillomotor),
    Q1 = quantile(Pupillomotor, 0.25),
    Q3 = quantile(Pupillomotor, 0.75),
    min=min(Pupillomotor),
    max=max(Pupillomotor),
    n=n()
  )


# Change from baseline

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Pupillomotor) %>%
  rename("Baseline"="Pupillomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Pupillomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Pupillomotor)
  ) %>%  inner_join(COMPASS_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((Pupillomotor-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Pupillomotor) %>%
  rename("Baseline"="Pupillomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Pupillomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Pupillomotor)
  ) %>% 
  mutate(Diff= (Pupillomotor-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Pupillomotor) %>%
  rename("Baseline"="Pupillomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Pupillomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Pupillomotor)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Pupillomotor) %>%
  rename("Baseline"="Pupillomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==2) %>%
      filter(!is.na(Pupillomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Pupillomotor)
  ) %>% 
  mutate(Diff= (Pupillomotor-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
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


# ALL COMPASS Total Year 3

EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(
    mean=mean(Pupillomotor),
    sd=sd(Pupillomotor),
    median=median(Pupillomotor),
    Q1 = quantile(Pupillomotor, 0.25),
    Q3 = quantile(Pupillomotor, 0.75),
    min=min(Pupillomotor),
    max=max(Pupillomotor),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Pupillomotor) %>%
  rename("Baseline"="Pupillomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Pupillomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Pupillomotor)
  ) %>% group_by(DIAGNIV) %>% inner_join(COMPASS_Baseline_Pats) %>%
  summarise(mean= mean((Pupillomotor-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Pupillomotor) %>%
  rename("Baseline"="Pupillomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Pupillomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Pupillomotor)
  ) %>% 
  mutate(Diff= (Pupillomotor-Baseline)) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Pupillomotor) %>%
  rename("Baseline"="Pupillomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Pupillomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Pupillomotor)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(COMPASS_Baseline_Pats) %>%
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



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  filter(!is.na(Pupillomotor)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  Pupillomotor) %>%
  rename("Baseline"="Pupillomotor") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==3) %>%
      filter(!is.na(Pupillomotor)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, Pupillomotor)
  ) %>% 
  mutate(Diff= (Pupillomotor-Baseline)/TIME_STUDY) %>% inner_join(COMPASS_Baseline_Pats) %>%
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

