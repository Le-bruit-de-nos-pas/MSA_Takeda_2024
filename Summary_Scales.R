
library(tidyverse)
library(data.table)
options(scipen = 999)

# Inputs UMSARS 1 Total ----------------------------------

AllMSA_Pop_Baseline_671 <- fread("Source/AllMSA_Pop_Baseline_671.txt")
AllMSA_Pop_BaselineYear1_410 <- fread("Source/AllMSA_Pop_BaselineYear1_410.txt")
AllMSA_Pop_BaselineYear1Year2_245 <- fread("Source/AllMSA_Pop_BaselineYear1Year2_245.txt")
AllMSA_Pop_BaselineYear1Year2Year3Plus_158 <- fread("Source/AllMSA_Pop_BaselineYear1Year2Year3Plus_158.txt")

EarlyCT_Pop_Baseline_319 <- fread("Source/EarlyCT_Pop_Baseline_319.txt")
EarlyCT_Pop_BaselineYear1_208 <- fread("Source/EarlyCT_Pop_BaselineYear1_208.txt")
EarlyCT_Pop_BaselineYear1Year2_134 <- fread("Source/EarlyCT_Pop_BaselineYear1Year2_134.txt")
EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 <- fread( "Source/EarlyCT_Pop_BaselineYear1Year2Year3Plus_99.txt")

# UMSARS 1 Total Score 

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

UMSARS1 <-  UMSARS1 %>%  select(NUM, TIME_STUDY, Year, UMSARS1_TOT)

# ----------------
# Overall MSA Entire -------------------------------

# ALL UMSARS Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
  filter(!is.na(UMSARS1_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(UMSARS1_TOT),
    sd=sd(UMSARS1_TOT),
    median=median(UMSARS1_TOT),
    Q1 = quantile(UMSARS1_TOT, 0.25),
    Q3 = quantile(UMSARS1_TOT, 0.75),
    min=min(UMSARS1_TOT),
    max=max(UMSARS1_TOT),
    n=n()
  )



# ALL UMSARS Total Year 1

AllMSA_Pop_BaselineYear1_410 %>% inner_join(UMSARS1) %>% filter(Year==1) %>%
  filter(!is.na(UMSARS1_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(UMSARS1_TOT),
    sd=sd(UMSARS1_TOT),
    median=median(UMSARS1_TOT),
    Q1 = quantile(UMSARS1_TOT, 0.25),
    Q3 = quantile(UMSARS1_TOT, 0.75),
    min=min(UMSARS1_TOT),
    max=max(UMSARS1_TOT),
    n=n()
  )

# Change from baseline

AllMSA_Pop_BaselineYear1_410 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
  filter(!is.na(UMSARS1_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  UMSARS1_TOT) %>%
  rename("Baseline"="UMSARS1_TOT") %>%
  left_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(UMSARS1) %>% filter(Year==1) %>%
      filter(!is.na(UMSARS1_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, UMSARS1_TOT)
  ) %>% 
  summarise(mean= mean((UMSARS1_TOT-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1_410 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
  filter(!is.na(UMSARS1_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  UMSARS1_TOT) %>%
  rename("Baseline"="UMSARS1_TOT") %>%
  left_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(UMSARS1) %>% filter(Year==1) %>%
      filter(!is.na(UMSARS1_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, UMSARS1_TOT)
  ) %>% 
  mutate(Diff= (UMSARS1_TOT-Baseline)/TIME_STUDY)  %>%
  summarise(
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

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(UMSARS1) %>% filter(Year==2) %>%
  filter(!is.na(UMSARS1_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(UMSARS1_TOT),
    sd=sd(UMSARS1_TOT),
    median=median(UMSARS1_TOT),
    Q1 = quantile(UMSARS1_TOT, 0.25),
    Q3 = quantile(UMSARS1_TOT, 0.75),
    min=min(UMSARS1_TOT),
    max=max(UMSARS1_TOT),
    n=n()
  )


# Change from baseline

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
  filter(!is.na(UMSARS1_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  UMSARS1_TOT) %>%
  rename("Baseline"="UMSARS1_TOT") %>%
  left_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(UMSARS1) %>% filter(Year==2) %>%
      filter(!is.na(UMSARS1_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, UMSARS1_TOT)
  ) %>% 
  summarise(mean= mean((UMSARS1_TOT-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
  filter(!is.na(UMSARS1_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  UMSARS1_TOT) %>%
  rename("Baseline"="UMSARS1_TOT") %>%
  left_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(UMSARS1) %>% filter(Year==2) %>%
      filter(!is.na(UMSARS1_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, UMSARS1_TOT)
  ) %>% 
  mutate(Diff= (UMSARS1_TOT-Baseline)/TIME_STUDY)  %>%
  summarise(
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

AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(UMSARS1) %>% filter(Year==3) %>%
  filter(!is.na(UMSARS1_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(UMSARS1_TOT),
    sd=sd(UMSARS1_TOT),
    median=median(UMSARS1_TOT),
    Q1 = quantile(UMSARS1_TOT, 0.25),
    Q3 = quantile(UMSARS1_TOT, 0.75),
    min=min(UMSARS1_TOT),
    max=max(UMSARS1_TOT),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
  filter(!is.na(UMSARS1_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  UMSARS1_TOT) %>%
  rename("Baseline"="UMSARS1_TOT") %>%
  left_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(UMSARS1) %>% filter(Year==3) %>%
      filter(!is.na(UMSARS1_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, UMSARS1_TOT)
  ) %>% 
  summarise(mean= mean((UMSARS1_TOT-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
  filter(!is.na(UMSARS1_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  UMSARS1_TOT) %>%
  rename("Baseline"="UMSARS1_TOT") %>%
  left_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(UMSARS1) %>% filter(Year==3) %>%
      filter(!is.na(UMSARS1_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, UMSARS1_TOT)
  ) %>% 
  mutate(Diff= (UMSARS1_TOT-Baseline)/TIME_STUDY)  %>%
  summarise(
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

AllMSA_Pop_Baseline_671 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
  filter(!is.na(UMSARS1_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(UMSARS1_TOT),
    sd=sd(UMSARS1_TOT),
    median=median(UMSARS1_TOT),
    Q1 = quantile(UMSARS1_TOT, 0.25),
    Q3 = quantile(UMSARS1_TOT, 0.75),
    min=min(UMSARS1_TOT),
    max=max(UMSARS1_TOT),
    n=n()
  )



# ALL UMSARS Total Year 1

AllMSA_Pop_BaselineYear1_410 %>% inner_join(UMSARS1) %>% filter(Year==1) %>%
  filter(!is.na(UMSARS1_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(UMSARS1_TOT),
    sd=sd(UMSARS1_TOT),
    median=median(UMSARS1_TOT),
    Q1 = quantile(UMSARS1_TOT, 0.25),
    Q3 = quantile(UMSARS1_TOT, 0.75),
    min=min(UMSARS1_TOT),
    max=max(UMSARS1_TOT),
    n=n()
  )

# Change from baseline

AllMSA_Pop_BaselineYear1_410 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
  filter(!is.na(UMSARS1_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  UMSARS1_TOT) %>%
  rename("Baseline"="UMSARS1_TOT") %>%
  left_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(UMSARS1) %>% filter(Year==1) %>%
      filter(!is.na(UMSARS1_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, UMSARS1_TOT)
  ) %>% 
  group_by(DIAG) %>% summarise(mean= mean((UMSARS1_TOT-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1_410 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
  filter(!is.na(UMSARS1_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  UMSARS1_TOT) %>%
  rename("Baseline"="UMSARS1_TOT") %>%
  left_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(UMSARS1) %>% filter(Year==1) %>%
      filter(!is.na(UMSARS1_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, UMSARS1_TOT)
  ) %>% 
  mutate(Diff= (UMSARS1_TOT-Baseline)/TIME_STUDY)  %>%
  group_by(DIAG) %>%
  summarise(
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

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(UMSARS1) %>% filter(Year==2) %>%
  filter(!is.na(UMSARS1_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(UMSARS1_TOT),
    sd=sd(UMSARS1_TOT),
    median=median(UMSARS1_TOT),
    Q1 = quantile(UMSARS1_TOT, 0.25),
    Q3 = quantile(UMSARS1_TOT, 0.75),
    min=min(UMSARS1_TOT),
    max=max(UMSARS1_TOT),
    n=n()
  )


# Change from baseline

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
  filter(!is.na(UMSARS1_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  UMSARS1_TOT) %>%
  rename("Baseline"="UMSARS1_TOT") %>%
  left_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(UMSARS1) %>% filter(Year==2) %>%
      filter(!is.na(UMSARS1_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, UMSARS1_TOT)
  ) %>% 
  group_by(DIAG) %>% summarise(mean= mean((UMSARS1_TOT-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
  filter(!is.na(UMSARS1_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  UMSARS1_TOT) %>%
  rename("Baseline"="UMSARS1_TOT") %>%
  left_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(UMSARS1) %>% filter(Year==2) %>%
      filter(!is.na(UMSARS1_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, UMSARS1_TOT)
  ) %>% 
  mutate(Diff= (UMSARS1_TOT-Baseline)/TIME_STUDY)  %>%
  group_by(DIAG) %>%
  summarise(
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

AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(UMSARS1) %>% filter(Year==3) %>%
  filter(!is.na(UMSARS1_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(UMSARS1_TOT),
    sd=sd(UMSARS1_TOT),
    median=median(UMSARS1_TOT),
    Q1 = quantile(UMSARS1_TOT, 0.25),
    Q3 = quantile(UMSARS1_TOT, 0.75),
    min=min(UMSARS1_TOT),
    max=max(UMSARS1_TOT),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
  filter(!is.na(UMSARS1_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  UMSARS1_TOT) %>%
  rename("Baseline"="UMSARS1_TOT") %>%
  left_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(UMSARS1) %>% filter(Year==3) %>%
      filter(!is.na(UMSARS1_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, UMSARS1_TOT)
  ) %>% group_by(DIAG) %>%
  summarise(mean= mean((UMSARS1_TOT-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
  filter(!is.na(UMSARS1_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  UMSARS1_TOT) %>%
  rename("Baseline"="UMSARS1_TOT") %>%
  left_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(UMSARS1) %>% filter(Year==3) %>%
      filter(!is.na(UMSARS1_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, UMSARS1_TOT)
  ) %>% 
  mutate(Diff= (UMSARS1_TOT-Baseline)/TIME_STUDY)  %>%
  group_by(DIAG) %>%
  summarise(
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

AllMSA_Pop_Baseline_671 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
  filter(!is.na(UMSARS1_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(UMSARS1_TOT),
    sd=sd(UMSARS1_TOT),
    median=median(UMSARS1_TOT),
    Q1 = quantile(UMSARS1_TOT, 0.25),
    Q3 = quantile(UMSARS1_TOT, 0.75),
    min=min(UMSARS1_TOT),
    max=max(UMSARS1_TOT),
    n=n()
  )



# ALL UMSARS Total Year 1

AllMSA_Pop_BaselineYear1_410 %>% inner_join(UMSARS1) %>% filter(Year==1) %>%
  filter(!is.na(UMSARS1_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(UMSARS1_TOT),
    sd=sd(UMSARS1_TOT),
    median=median(UMSARS1_TOT),
    Q1 = quantile(UMSARS1_TOT, 0.25),
    Q3 = quantile(UMSARS1_TOT, 0.75),
    min=min(UMSARS1_TOT),
    max=max(UMSARS1_TOT),
    n=n()
  )

# Change from baseline


AllMSA_Pop_BaselineYear1_410 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
  filter(!is.na(UMSARS1_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  UMSARS1_TOT) %>%
  rename("Baseline"="UMSARS1_TOT") %>%
  left_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(UMSARS1) %>% filter(Year==1) %>%
      filter(!is.na(UMSARS1_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, UMSARS1_TOT)
  ) %>% 
  group_by(DIAGNIV) %>% summarise(mean= mean((UMSARS1_TOT-Baseline)/TIME_STUDY)) 

AllMSA_Pop_BaselineYear1_410 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
  filter(!is.na(UMSARS1_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  UMSARS1_TOT) %>%
  rename("Baseline"="UMSARS1_TOT") %>%
  left_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(UMSARS1) %>% filter(Year==1) %>%
      filter(!is.na(UMSARS1_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, UMSARS1_TOT)
  ) %>% 
  mutate(Diff= (UMSARS1_TOT-Baseline)/TIME_STUDY)  %>%
  group_by(DIAGNIV) %>%
  summarise(
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

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(UMSARS1) %>% filter(Year==2) %>%
  filter(!is.na(UMSARS1_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(UMSARS1_TOT),
    sd=sd(UMSARS1_TOT),
    median=median(UMSARS1_TOT),
    Q1 = quantile(UMSARS1_TOT, 0.25),
    Q3 = quantile(UMSARS1_TOT, 0.75),
    min=min(UMSARS1_TOT),
    max=max(UMSARS1_TOT),
    n=n()
  )


# Change from baseline

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
  filter(!is.na(UMSARS1_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  UMSARS1_TOT) %>%
  rename("Baseline"="UMSARS1_TOT") %>%
  left_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(UMSARS1) %>% filter(Year==2) %>%
      filter(!is.na(UMSARS1_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, UMSARS1_TOT)
  ) %>% 
  group_by(DIAGNIV) %>% summarise(mean= mean((UMSARS1_TOT-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
  filter(!is.na(UMSARS1_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  UMSARS1_TOT) %>%
  rename("Baseline"="UMSARS1_TOT") %>%
  left_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(UMSARS1) %>% filter(Year==2) %>%
      filter(!is.na(UMSARS1_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, UMSARS1_TOT)
  ) %>% 
  mutate(Diff= (UMSARS1_TOT-Baseline)/TIME_STUDY)  %>%
  group_by(DIAGNIV) %>%
  summarise(
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

AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(UMSARS1) %>% filter(Year==3) %>%
  filter(!is.na(UMSARS1_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(UMSARS1_TOT),
    sd=sd(UMSARS1_TOT),
    median=median(UMSARS1_TOT),
    Q1 = quantile(UMSARS1_TOT, 0.25),
    Q3 = quantile(UMSARS1_TOT, 0.75),
    min=min(UMSARS1_TOT),
    max=max(UMSARS1_TOT),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
  filter(!is.na(UMSARS1_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  UMSARS1_TOT) %>%
  rename("Baseline"="UMSARS1_TOT") %>%
  left_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(UMSARS1) %>% filter(Year==3) %>%
      filter(!is.na(UMSARS1_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, UMSARS1_TOT)
  ) %>% group_by(DIAGNIV) %>%
  summarise(mean= mean((UMSARS1_TOT-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
  filter(!is.na(UMSARS1_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  UMSARS1_TOT) %>%
  rename("Baseline"="UMSARS1_TOT") %>%
  left_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(UMSARS1) %>% filter(Year==3) %>%
      filter(!is.na(UMSARS1_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, UMSARS1_TOT)
  ) %>% 
  mutate(Diff= (UMSARS1_TOT-Baseline)/TIME_STUDY)  %>%
  group_by(DIAGNIV) %>%
  summarise(
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


EarlyCT_Pop_Baseline_319 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
  filter(!is.na(UMSARS1_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(UMSARS1_TOT),
    sd=sd(UMSARS1_TOT),
    median=median(UMSARS1_TOT),
    Q1 = quantile(UMSARS1_TOT, 0.25),
    Q3 = quantile(UMSARS1_TOT, 0.75),
    min=min(UMSARS1_TOT),
    max=max(UMSARS1_TOT),
    n=n()
  )



# ALL UMSARS Total Year 1

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(UMSARS1) %>% filter(Year==1) %>%
  filter(!is.na(UMSARS1_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(UMSARS1_TOT),
    sd=sd(UMSARS1_TOT),
    median=median(UMSARS1_TOT),
    Q1 = quantile(UMSARS1_TOT, 0.25),
    Q3 = quantile(UMSARS1_TOT, 0.75),
    min=min(UMSARS1_TOT),
    max=max(UMSARS1_TOT),
    n=n()
  )

# Change from baseline

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
  filter(!is.na(UMSARS1_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  UMSARS1_TOT) %>%
  rename("Baseline"="UMSARS1_TOT") %>%
  left_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(UMSARS1) %>% filter(Year==1) %>%
      filter(!is.na(UMSARS1_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, UMSARS1_TOT)
  ) %>% 
  summarise(mean= mean((UMSARS1_TOT-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
  filter(!is.na(UMSARS1_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  UMSARS1_TOT) %>%
  rename("Baseline"="UMSARS1_TOT") %>%
  left_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(UMSARS1) %>% filter(Year==1) %>%
      filter(!is.na(UMSARS1_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, UMSARS1_TOT)
  ) %>% 
  mutate(Diff= (UMSARS1_TOT-Baseline)/TIME_STUDY)  %>%
  summarise(
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

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(UMSARS1) %>% filter(Year==2) %>%
  filter(!is.na(UMSARS1_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(UMSARS1_TOT),
    sd=sd(UMSARS1_TOT),
    median=median(UMSARS1_TOT),
    Q1 = quantile(UMSARS1_TOT, 0.25),
    Q3 = quantile(UMSARS1_TOT, 0.75),
    min=min(UMSARS1_TOT),
    max=max(UMSARS1_TOT),
    n=n()
  )


# Change from baseline

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
  filter(!is.na(UMSARS1_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  UMSARS1_TOT) %>%
  rename("Baseline"="UMSARS1_TOT") %>%
  left_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(UMSARS1) %>% filter(Year==2) %>%
      filter(!is.na(UMSARS1_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, UMSARS1_TOT)
  ) %>% 
  summarise(mean= mean((UMSARS1_TOT-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
  filter(!is.na(UMSARS1_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  UMSARS1_TOT) %>%
  rename("Baseline"="UMSARS1_TOT") %>%
  left_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(UMSARS1) %>% filter(Year==2) %>%
      filter(!is.na(UMSARS1_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, UMSARS1_TOT)
  ) %>% 
  mutate(Diff= (UMSARS1_TOT-Baseline)/TIME_STUDY)  %>%
  summarise(
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

EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(UMSARS1) %>% filter(Year==3) %>%
  filter(!is.na(UMSARS1_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(UMSARS1_TOT),
    sd=sd(UMSARS1_TOT),
    median=median(UMSARS1_TOT),
    Q1 = quantile(UMSARS1_TOT, 0.25),
    Q3 = quantile(UMSARS1_TOT, 0.75),
    min=min(UMSARS1_TOT),
    max=max(UMSARS1_TOT),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
  filter(!is.na(UMSARS1_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  UMSARS1_TOT) %>%
  rename("Baseline"="UMSARS1_TOT") %>%
  left_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(UMSARS1) %>% filter(Year==3) %>%
      filter(!is.na(UMSARS1_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, UMSARS1_TOT)
  ) %>% 
  summarise(mean= mean((UMSARS1_TOT-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
  filter(!is.na(UMSARS1_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  UMSARS1_TOT) %>%
  rename("Baseline"="UMSARS1_TOT") %>%
  left_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(UMSARS1) %>% filter(Year==3) %>%
      filter(!is.na(UMSARS1_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, UMSARS1_TOT)
  ) %>% 
  mutate(Diff= (UMSARS1_TOT-Baseline)/TIME_STUDY)  %>%
  summarise(
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

EarlyCT_Pop_Baseline_319 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
  filter(!is.na(UMSARS1_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(UMSARS1_TOT),
    sd=sd(UMSARS1_TOT),
    median=median(UMSARS1_TOT),
    Q1 = quantile(UMSARS1_TOT, 0.25),
    Q3 = quantile(UMSARS1_TOT, 0.75),
    min=min(UMSARS1_TOT),
    max=max(UMSARS1_TOT),
    n=n()
  )



# ALL UMSARS Total Year 1

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(UMSARS1) %>% filter(Year==1) %>%
  filter(!is.na(UMSARS1_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(UMSARS1_TOT),
    sd=sd(UMSARS1_TOT),
    median=median(UMSARS1_TOT),
    Q1 = quantile(UMSARS1_TOT, 0.25),
    Q3 = quantile(UMSARS1_TOT, 0.75),
    min=min(UMSARS1_TOT),
    max=max(UMSARS1_TOT),
    n=n()
  )

# Change from baseline

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
  filter(!is.na(UMSARS1_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  UMSARS1_TOT) %>%
  rename("Baseline"="UMSARS1_TOT") %>%
  left_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(UMSARS1) %>% filter(Year==1) %>%
      filter(!is.na(UMSARS1_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, UMSARS1_TOT)
  ) %>% 
  group_by(DIAG) %>% summarise(mean= mean((UMSARS1_TOT-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
  filter(!is.na(UMSARS1_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  UMSARS1_TOT) %>%
  rename("Baseline"="UMSARS1_TOT") %>%
  left_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(UMSARS1) %>% filter(Year==1) %>%
      filter(!is.na(UMSARS1_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, UMSARS1_TOT)
  ) %>% 
  mutate(Diff= (UMSARS1_TOT-Baseline)/TIME_STUDY)  %>%
  group_by(DIAG) %>%
  summarise(
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

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(UMSARS1) %>% filter(Year==2) %>%
  filter(!is.na(UMSARS1_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(UMSARS1_TOT),
    sd=sd(UMSARS1_TOT),
    median=median(UMSARS1_TOT),
    Q1 = quantile(UMSARS1_TOT, 0.25),
    Q3 = quantile(UMSARS1_TOT, 0.75),
    min=min(UMSARS1_TOT),
    max=max(UMSARS1_TOT),
    n=n()
  )


# Change from baseline

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
  filter(!is.na(UMSARS1_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  UMSARS1_TOT) %>%
  rename("Baseline"="UMSARS1_TOT") %>%
  left_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(UMSARS1) %>% filter(Year==2) %>%
      filter(!is.na(UMSARS1_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, UMSARS1_TOT)
  ) %>% 
  group_by(DIAG) %>% summarise(mean= mean((UMSARS1_TOT-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
  filter(!is.na(UMSARS1_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  UMSARS1_TOT) %>%
  rename("Baseline"="UMSARS1_TOT") %>%
  left_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(UMSARS1) %>% filter(Year==2) %>%
      filter(!is.na(UMSARS1_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, UMSARS1_TOT)
  ) %>% 
  mutate(Diff= (UMSARS1_TOT-Baseline)/TIME_STUDY)  %>%
  group_by(DIAG) %>%
  summarise(
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

EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(UMSARS1) %>% filter(Year==3) %>%
  filter(!is.na(UMSARS1_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(UMSARS1_TOT),
    sd=sd(UMSARS1_TOT),
    median=median(UMSARS1_TOT),
    Q1 = quantile(UMSARS1_TOT, 0.25),
    Q3 = quantile(UMSARS1_TOT, 0.75),
    min=min(UMSARS1_TOT),
    max=max(UMSARS1_TOT),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
  filter(!is.na(UMSARS1_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  UMSARS1_TOT) %>%
  rename("Baseline"="UMSARS1_TOT") %>%
  left_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(UMSARS1) %>% filter(Year==3) %>%
      filter(!is.na(UMSARS1_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, UMSARS1_TOT)
  ) %>% group_by(DIAG) %>%
  summarise(mean= mean((UMSARS1_TOT-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
  filter(!is.na(UMSARS1_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  UMSARS1_TOT) %>%
  rename("Baseline"="UMSARS1_TOT") %>%
  left_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(UMSARS1) %>% filter(Year==3) %>%
      filter(!is.na(UMSARS1_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, UMSARS1_TOT)
  ) %>% 
  mutate(Diff= (UMSARS1_TOT-Baseline)/TIME_STUDY)  %>%
  group_by(DIAG) %>%
  summarise(
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

EarlyCT_Pop_Baseline_319 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
  filter(!is.na(UMSARS1_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(UMSARS1_TOT),
    sd=sd(UMSARS1_TOT),
    median=median(UMSARS1_TOT),
    Q1 = quantile(UMSARS1_TOT, 0.25),
    Q3 = quantile(UMSARS1_TOT, 0.75),
    min=min(UMSARS1_TOT),
    max=max(UMSARS1_TOT),
    n=n()
  )



# ALL UMSARS Total Year 1

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(UMSARS1) %>% filter(Year==1) %>%
  filter(!is.na(UMSARS1_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(UMSARS1_TOT),
    sd=sd(UMSARS1_TOT),
    median=median(UMSARS1_TOT),
    Q1 = quantile(UMSARS1_TOT, 0.25),
    Q3 = quantile(UMSARS1_TOT, 0.75),
    min=min(UMSARS1_TOT),
    max=max(UMSARS1_TOT),
    n=n()
  )

# Change from baseline


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
  filter(!is.na(UMSARS1_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  UMSARS1_TOT) %>%
  rename("Baseline"="UMSARS1_TOT") %>%
  left_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(UMSARS1) %>% filter(Year==1) %>%
      filter(!is.na(UMSARS1_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, UMSARS1_TOT)
  ) %>% 
  group_by(DIAGNIV) %>% summarise(mean= mean((UMSARS1_TOT-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
  filter(!is.na(UMSARS1_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  UMSARS1_TOT) %>%
  rename("Baseline"="UMSARS1_TOT") %>%
  left_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(UMSARS1) %>% filter(Year==1) %>%
      filter(!is.na(UMSARS1_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, UMSARS1_TOT)
  ) %>% 
  mutate(Diff= (UMSARS1_TOT-Baseline)/TIME_STUDY)  %>%
  group_by(DIAGNIV) %>%
  summarise(
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

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(UMSARS1) %>% filter(Year==2) %>%
  filter(!is.na(UMSARS1_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(UMSARS1_TOT),
    sd=sd(UMSARS1_TOT),
    median=median(UMSARS1_TOT),
    Q1 = quantile(UMSARS1_TOT, 0.25),
    Q3 = quantile(UMSARS1_TOT, 0.75),
    min=min(UMSARS1_TOT),
    max=max(UMSARS1_TOT),
    n=n()
  )


# Change from baseline

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
  filter(!is.na(UMSARS1_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  UMSARS1_TOT) %>%
  rename("Baseline"="UMSARS1_TOT") %>%
  left_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(UMSARS1) %>% filter(Year==2) %>%
      filter(!is.na(UMSARS1_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, UMSARS1_TOT)
  ) %>% 
  group_by(DIAGNIV) %>% summarise(mean= mean((UMSARS1_TOT-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
  filter(!is.na(UMSARS1_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  UMSARS1_TOT) %>%
  rename("Baseline"="UMSARS1_TOT") %>%
  left_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(UMSARS1) %>% filter(Year==2) %>%
      filter(!is.na(UMSARS1_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, UMSARS1_TOT)
  ) %>% 
  mutate(Diff= (UMSARS1_TOT-Baseline)/TIME_STUDY)  %>%
  group_by(DIAGNIV) %>%
  summarise(
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

EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(UMSARS1) %>% filter(Year==3) %>%
  filter(!is.na(UMSARS1_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(UMSARS1_TOT),
    sd=sd(UMSARS1_TOT),
    median=median(UMSARS1_TOT),
    Q1 = quantile(UMSARS1_TOT, 0.25),
    Q3 = quantile(UMSARS1_TOT, 0.75),
    min=min(UMSARS1_TOT),
    max=max(UMSARS1_TOT),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
  filter(!is.na(UMSARS1_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  UMSARS1_TOT) %>%
  rename("Baseline"="UMSARS1_TOT") %>%
  left_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(UMSARS1) %>% filter(Year==3) %>%
      filter(!is.na(UMSARS1_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, UMSARS1_TOT)
  ) %>% group_by(DIAGNIV) %>%
  summarise(mean= mean((UMSARS1_TOT-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
  filter(!is.na(UMSARS1_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  UMSARS1_TOT) %>%
  rename("Baseline"="UMSARS1_TOT") %>%
  left_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(UMSARS1) %>% filter(Year==3) %>%
      filter(!is.na(UMSARS1_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, UMSARS1_TOT)
  ) %>% 
  mutate(Diff= (UMSARS1_TOT-Baseline)/TIME_STUDY)  %>%
  group_by(DIAGNIV) %>%
  summarise(
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

# Inputs UMSARS 1 FDA-modified exc. #11 ----------------------------------

AllMSA_Pop_Baseline_671 <- fread("Source/AllMSA_Pop_Baseline_671.txt")
AllMSA_Pop_BaselineYear1_410 <- fread("Source/AllMSA_Pop_BaselineYear1_410.txt")
AllMSA_Pop_BaselineYear1Year2_245 <- fread("Source/AllMSA_Pop_BaselineYear1Year2_245.txt")
AllMSA_Pop_BaselineYear1Year2Year3Plus_158 <- fread("Source/AllMSA_Pop_BaselineYear1Year2Year3Plus_158.txt")

EarlyCT_Pop_Baseline_319 <- fread("Source/EarlyCT_Pop_Baseline_319.txt")
EarlyCT_Pop_BaselineYear1_208 <- fread("Source/EarlyCT_Pop_BaselineYear1_208.txt")
EarlyCT_Pop_BaselineYear1Year2_134 <- fread("Source/EarlyCT_Pop_BaselineYear1Year2_134.txt")
EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 <- fread( "Source/EarlyCT_Pop_BaselineYear1Year2Year3Plus_99.txt")

# UMSARS 1 DA-modified exc. #11

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

# ----------------
# Overall MSA Entire -------------------------------

# ALL UMSARS Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
  filter(!is.na(UMSARS1_TOT_FDA)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(UMSARS1_TOT_FDA),
    sd=sd(UMSARS1_TOT_FDA),
    median=median(UMSARS1_TOT_FDA),
    Q1 = quantile(UMSARS1_TOT_FDA, 0.25),
    Q3 = quantile(UMSARS1_TOT_FDA, 0.75),
    min=min(UMSARS1_TOT_FDA),
    max=max(UMSARS1_TOT_FDA),
    n=n()
  )



# ALL UMSARS Total Year 1

AllMSA_Pop_BaselineYear1_410 %>% inner_join(UMSARS1) %>% filter(Year==1) %>%
  filter(!is.na(UMSARS1_TOT_FDA)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(UMSARS1_TOT_FDA),
    sd=sd(UMSARS1_TOT_FDA),
    median=median(UMSARS1_TOT_FDA),
    Q1 = quantile(UMSARS1_TOT_FDA, 0.25),
    Q3 = quantile(UMSARS1_TOT_FDA, 0.75),
    min=min(UMSARS1_TOT_FDA),
    max=max(UMSARS1_TOT_FDA),
    n=n()
  )

# Change from baseline

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
  summarise(mean= mean((UMSARS1_TOT_FDA-Baseline)/TIME_STUDY)) 


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
  mutate(Diff= (UMSARS1_TOT_FDA-Baseline)/TIME_STUDY) %>%
  summarise(
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

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(UMSARS1) %>% filter(Year==2) %>%
  filter(!is.na(UMSARS1_TOT_FDA)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(UMSARS1_TOT_FDA),
    sd=sd(UMSARS1_TOT_FDA),
    median=median(UMSARS1_TOT_FDA),
    Q1 = quantile(UMSARS1_TOT_FDA, 0.25),
    Q3 = quantile(UMSARS1_TOT_FDA, 0.75),
    min=min(UMSARS1_TOT_FDA),
    max=max(UMSARS1_TOT_FDA),
    n=n()
  )


# Change from baseline

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
  summarise(mean= mean((UMSARS1_TOT_FDA-Baseline)/TIME_STUDY)) 


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
  mutate(Diff= (UMSARS1_TOT_FDA-Baseline)/TIME_STUDY) %>%
  summarise(
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

AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(UMSARS1) %>% filter(Year==3) %>%
  filter(!is.na(UMSARS1_TOT_FDA)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(UMSARS1_TOT_FDA),
    sd=sd(UMSARS1_TOT_FDA),
    median=median(UMSARS1_TOT_FDA),
    Q1 = quantile(UMSARS1_TOT_FDA, 0.25),
    Q3 = quantile(UMSARS1_TOT_FDA, 0.75),
    min=min(UMSARS1_TOT_FDA),
    max=max(UMSARS1_TOT_FDA),
    n=n()
  )


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
  summarise(mean= mean((UMSARS1_TOT_FDA-Baseline)/TIME_STUDY)) 


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
  mutate(Diff= (UMSARS1_TOT_FDA-Baseline)/TIME_STUDY) %>%
  summarise(
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

AllMSA_Pop_Baseline_671 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
  filter(!is.na(UMSARS1_TOT_FDA)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(UMSARS1_TOT_FDA),
    sd=sd(UMSARS1_TOT_FDA),
    median=median(UMSARS1_TOT_FDA),
    Q1 = quantile(UMSARS1_TOT_FDA, 0.25),
    Q3 = quantile(UMSARS1_TOT_FDA, 0.75),
    min=min(UMSARS1_TOT_FDA),
    max=max(UMSARS1_TOT_FDA),
    n=n()
  )



# ALL UMSARS Total Year 1

AllMSA_Pop_BaselineYear1_410 %>% inner_join(UMSARS1) %>% filter(Year==1) %>%
  filter(!is.na(UMSARS1_TOT_FDA)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(UMSARS1_TOT_FDA),
    sd=sd(UMSARS1_TOT_FDA),
    median=median(UMSARS1_TOT_FDA),
    Q1 = quantile(UMSARS1_TOT_FDA, 0.25),
    Q3 = quantile(UMSARS1_TOT_FDA, 0.75),
    min=min(UMSARS1_TOT_FDA),
    max=max(UMSARS1_TOT_FDA),
    n=n()
  )

# Change from baseline

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
  group_by(DIAG) %>% summarise(mean= mean((UMSARS1_TOT_FDA-Baseline)/TIME_STUDY)) 


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
  mutate(Diff= (UMSARS1_TOT_FDA-Baseline)/TIME_STUDY) %>%
  group_by(DIAG) %>%
  summarise(
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

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(UMSARS1) %>% filter(Year==2) %>%
  filter(!is.na(UMSARS1_TOT_FDA)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(UMSARS1_TOT_FDA),
    sd=sd(UMSARS1_TOT_FDA),
    median=median(UMSARS1_TOT_FDA),
    Q1 = quantile(UMSARS1_TOT_FDA, 0.25),
    Q3 = quantile(UMSARS1_TOT_FDA, 0.75),
    min=min(UMSARS1_TOT_FDA),
    max=max(UMSARS1_TOT_FDA),
    n=n()
  )


# Change from baseline

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
  group_by(DIAG) %>% summarise(mean= mean((UMSARS1_TOT_FDA-Baseline)/TIME_STUDY)) 


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
  mutate(Diff= (UMSARS1_TOT_FDA-Baseline)/TIME_STUDY) %>%
  group_by(DIAG) %>%
  summarise(
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

AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(UMSARS1) %>% filter(Year==3) %>%
  filter(!is.na(UMSARS1_TOT_FDA)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(UMSARS1_TOT_FDA),
    sd=sd(UMSARS1_TOT_FDA),
    median=median(UMSARS1_TOT_FDA),
    Q1 = quantile(UMSARS1_TOT_FDA, 0.25),
    Q3 = quantile(UMSARS1_TOT_FDA, 0.75),
    min=min(UMSARS1_TOT_FDA),
    max=max(UMSARS1_TOT_FDA),
    n=n()
  )


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
  ) %>% group_by(DIAG) %>%
  summarise(mean= mean((UMSARS1_TOT_FDA-Baseline)/TIME_STUDY)) 


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
  mutate(Diff= (UMSARS1_TOT_FDA-Baseline)/TIME_STUDY) %>%
  group_by(DIAG) %>%
  summarise(
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

AllMSA_Pop_Baseline_671 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
  filter(!is.na(UMSARS1_TOT_FDA)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(UMSARS1_TOT_FDA),
    sd=sd(UMSARS1_TOT_FDA),
    median=median(UMSARS1_TOT_FDA),
    Q1 = quantile(UMSARS1_TOT_FDA, 0.25),
    Q3 = quantile(UMSARS1_TOT_FDA, 0.75),
    min=min(UMSARS1_TOT_FDA),
    max=max(UMSARS1_TOT_FDA),
    n=n()
  )



# ALL UMSARS Total Year 1

AllMSA_Pop_BaselineYear1_410 %>% inner_join(UMSARS1) %>% filter(Year==1) %>%
  filter(!is.na(UMSARS1_TOT_FDA)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(UMSARS1_TOT_FDA),
    sd=sd(UMSARS1_TOT_FDA),
    median=median(UMSARS1_TOT_FDA),
    Q1 = quantile(UMSARS1_TOT_FDA, 0.25),
    Q3 = quantile(UMSARS1_TOT_FDA, 0.75),
    min=min(UMSARS1_TOT_FDA),
    max=max(UMSARS1_TOT_FDA),
    n=n()
  )

# Change from baseline


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
  group_by(DIAGNIV) %>% summarise(mean= mean((UMSARS1_TOT_FDA-Baseline)/TIME_STUDY)) 


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
  mutate(Diff= (UMSARS1_TOT_FDA-Baseline)/TIME_STUDY) %>%
  group_by(DIAGNIV) %>%
  summarise(
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

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(UMSARS1) %>% filter(Year==2) %>%
  filter(!is.na(UMSARS1_TOT_FDA)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(UMSARS1_TOT_FDA),
    sd=sd(UMSARS1_TOT_FDA),
    median=median(UMSARS1_TOT_FDA),
    Q1 = quantile(UMSARS1_TOT_FDA, 0.25),
    Q3 = quantile(UMSARS1_TOT_FDA, 0.75),
    min=min(UMSARS1_TOT_FDA),
    max=max(UMSARS1_TOT_FDA),
    n=n()
  )


# Change from baseline

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
  group_by(DIAGNIV) %>% summarise(mean= mean((UMSARS1_TOT_FDA-Baseline)/TIME_STUDY)) 


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
  mutate(Diff= (UMSARS1_TOT_FDA-Baseline)/TIME_STUDY) %>%
  group_by(DIAGNIV) %>%
  summarise(
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

AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(UMSARS1) %>% filter(Year==3) %>%
  filter(!is.na(UMSARS1_TOT_FDA)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(UMSARS1_TOT_FDA),
    sd=sd(UMSARS1_TOT_FDA),
    median=median(UMSARS1_TOT_FDA),
    Q1 = quantile(UMSARS1_TOT_FDA, 0.25),
    Q3 = quantile(UMSARS1_TOT_FDA, 0.75),
    min=min(UMSARS1_TOT_FDA),
    max=max(UMSARS1_TOT_FDA),
    n=n()
  )


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
  ) %>% group_by(DIAGNIV) %>%
  summarise(mean= mean((UMSARS1_TOT_FDA-Baseline)/TIME_STUDY)) 


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
  mutate(Diff= (UMSARS1_TOT_FDA-Baseline)/TIME_STUDY) %>%
  group_by(DIAGNIV) %>%
  summarise(
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


EarlyCT_Pop_Baseline_319 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
  filter(!is.na(UMSARS1_TOT_FDA)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(UMSARS1_TOT_FDA),
    sd=sd(UMSARS1_TOT_FDA),
    median=median(UMSARS1_TOT_FDA),
    Q1 = quantile(UMSARS1_TOT_FDA, 0.25),
    Q3 = quantile(UMSARS1_TOT_FDA, 0.75),
    min=min(UMSARS1_TOT_FDA),
    max=max(UMSARS1_TOT_FDA),
    n=n()
  )



# ALL UMSARS Total Year 1

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(UMSARS1) %>% filter(Year==1) %>%
  filter(!is.na(UMSARS1_TOT_FDA)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(UMSARS1_TOT_FDA),
    sd=sd(UMSARS1_TOT_FDA),
    median=median(UMSARS1_TOT_FDA),
    Q1 = quantile(UMSARS1_TOT_FDA, 0.25),
    Q3 = quantile(UMSARS1_TOT_FDA, 0.75),
    min=min(UMSARS1_TOT_FDA),
    max=max(UMSARS1_TOT_FDA),
    n=n()
  )

# Change from baseline

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
  summarise(mean= mean((UMSARS1_TOT_FDA-Baseline)/TIME_STUDY)) 


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
  mutate(Diff= (UMSARS1_TOT_FDA-Baseline)/TIME_STUDY) %>%
  summarise(
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

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(UMSARS1) %>% filter(Year==2) %>%
  filter(!is.na(UMSARS1_TOT_FDA)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(UMSARS1_TOT_FDA),
    sd=sd(UMSARS1_TOT_FDA),
    median=median(UMSARS1_TOT_FDA),
    Q1 = quantile(UMSARS1_TOT_FDA, 0.25),
    Q3 = quantile(UMSARS1_TOT_FDA, 0.75),
    min=min(UMSARS1_TOT_FDA),
    max=max(UMSARS1_TOT_FDA),
    n=n()
  )


# Change from baseline

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
  summarise(mean= mean((UMSARS1_TOT_FDA-Baseline)/TIME_STUDY)) 


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
  mutate(Diff= (UMSARS1_TOT_FDA-Baseline)/TIME_STUDY) %>%
  summarise(
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

EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(UMSARS1) %>% filter(Year==3) %>%
  filter(!is.na(UMSARS1_TOT_FDA)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(UMSARS1_TOT_FDA),
    sd=sd(UMSARS1_TOT_FDA),
    median=median(UMSARS1_TOT_FDA),
    Q1 = quantile(UMSARS1_TOT_FDA, 0.25),
    Q3 = quantile(UMSARS1_TOT_FDA, 0.75),
    min=min(UMSARS1_TOT_FDA),
    max=max(UMSARS1_TOT_FDA),
    n=n()
  )


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
  summarise(mean= mean((UMSARS1_TOT_FDA-Baseline)/TIME_STUDY)) 


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
  mutate(Diff= (UMSARS1_TOT_FDA-Baseline)/TIME_STUDY) %>%
  summarise(
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

EarlyCT_Pop_Baseline_319 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
  filter(!is.na(UMSARS1_TOT_FDA)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(UMSARS1_TOT_FDA),
    sd=sd(UMSARS1_TOT_FDA),
    median=median(UMSARS1_TOT_FDA),
    Q1 = quantile(UMSARS1_TOT_FDA, 0.25),
    Q3 = quantile(UMSARS1_TOT_FDA, 0.75),
    min=min(UMSARS1_TOT_FDA),
    max=max(UMSARS1_TOT_FDA),
    n=n()
  )



# ALL UMSARS Total Year 1

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(UMSARS1) %>% filter(Year==1) %>%
  filter(!is.na(UMSARS1_TOT_FDA)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(UMSARS1_TOT_FDA),
    sd=sd(UMSARS1_TOT_FDA),
    median=median(UMSARS1_TOT_FDA),
    Q1 = quantile(UMSARS1_TOT_FDA, 0.25),
    Q3 = quantile(UMSARS1_TOT_FDA, 0.75),
    min=min(UMSARS1_TOT_FDA),
    max=max(UMSARS1_TOT_FDA),
    n=n()
  )

# Change from baseline

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
  group_by(DIAG) %>% summarise(mean= mean((UMSARS1_TOT_FDA-Baseline)/TIME_STUDY)) 


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
  mutate(Diff= (UMSARS1_TOT_FDA-Baseline)/TIME_STUDY) %>%
  group_by(DIAG) %>%
  summarise(
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

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(UMSARS1) %>% filter(Year==2) %>%
  filter(!is.na(UMSARS1_TOT_FDA)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(UMSARS1_TOT_FDA),
    sd=sd(UMSARS1_TOT_FDA),
    median=median(UMSARS1_TOT_FDA),
    Q1 = quantile(UMSARS1_TOT_FDA, 0.25),
    Q3 = quantile(UMSARS1_TOT_FDA, 0.75),
    min=min(UMSARS1_TOT_FDA),
    max=max(UMSARS1_TOT_FDA),
    n=n()
  )


# Change from baseline

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
  group_by(DIAG) %>% summarise(mean= mean((UMSARS1_TOT_FDA-Baseline)/TIME_STUDY)) 


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
  mutate(Diff= (UMSARS1_TOT_FDA-Baseline)/TIME_STUDY) %>%
  group_by(DIAG) %>%
  summarise(
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

EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(UMSARS1) %>% filter(Year==3) %>%
  filter(!is.na(UMSARS1_TOT_FDA)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>%
  summarise(
    mean=mean(UMSARS1_TOT_FDA),
    sd=sd(UMSARS1_TOT_FDA),
    median=median(UMSARS1_TOT_FDA),
    Q1 = quantile(UMSARS1_TOT_FDA, 0.25),
    Q3 = quantile(UMSARS1_TOT_FDA, 0.75),
    min=min(UMSARS1_TOT_FDA),
    max=max(UMSARS1_TOT_FDA),
    n=n()
  )


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
  ) %>% group_by(DIAG) %>%
  summarise(mean= mean((UMSARS1_TOT_FDA-Baseline)/TIME_STUDY)) 


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
  mutate(Diff= (UMSARS1_TOT_FDA-Baseline)/TIME_STUDY) %>%
  group_by(DIAG) %>%
  summarise(
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

EarlyCT_Pop_Baseline_319 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
  filter(!is.na(UMSARS1_TOT_FDA)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(UMSARS1_TOT_FDA),
    sd=sd(UMSARS1_TOT_FDA),
    median=median(UMSARS1_TOT_FDA),
    Q1 = quantile(UMSARS1_TOT_FDA, 0.25),
    Q3 = quantile(UMSARS1_TOT_FDA, 0.75),
    min=min(UMSARS1_TOT_FDA),
    max=max(UMSARS1_TOT_FDA),
    n=n()
  )



# ALL UMSARS Total Year 1

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(UMSARS1) %>% filter(Year==1) %>%
  filter(!is.na(UMSARS1_TOT_FDA)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(UMSARS1_TOT_FDA),
    sd=sd(UMSARS1_TOT_FDA),
    median=median(UMSARS1_TOT_FDA),
    Q1 = quantile(UMSARS1_TOT_FDA, 0.25),
    Q3 = quantile(UMSARS1_TOT_FDA, 0.75),
    min=min(UMSARS1_TOT_FDA),
    max=max(UMSARS1_TOT_FDA),
    n=n()
  )

# Change from baseline


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
  group_by(DIAGNIV) %>% summarise(mean= mean((UMSARS1_TOT_FDA-Baseline)/TIME_STUDY)) 



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
  mutate(Diff= (UMSARS1_TOT_FDA-Baseline)/TIME_STUDY) %>%
  group_by(DIAGNIV) %>%
  summarise(
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

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(UMSARS1) %>% filter(Year==2) %>%
  filter(!is.na(UMSARS1_TOT_FDA)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(UMSARS1_TOT_FDA),
    sd=sd(UMSARS1_TOT_FDA),
    median=median(UMSARS1_TOT_FDA),
    Q1 = quantile(UMSARS1_TOT_FDA, 0.25),
    Q3 = quantile(UMSARS1_TOT_FDA, 0.75),
    min=min(UMSARS1_TOT_FDA),
    max=max(UMSARS1_TOT_FDA),
    n=n()
  )


# Change from baseline

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
  group_by(DIAGNIV) %>% summarise(mean= mean((UMSARS1_TOT_FDA-Baseline)/TIME_STUDY)) 



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
  mutate(Diff= (UMSARS1_TOT_FDA-Baseline)/TIME_STUDY) %>%
  group_by(DIAGNIV) %>%
  summarise(
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

EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(UMSARS1) %>% filter(Year==3) %>%
  filter(!is.na(UMSARS1_TOT_FDA)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>%
  summarise(
    mean=mean(UMSARS1_TOT_FDA),
    sd=sd(UMSARS1_TOT_FDA),
    median=median(UMSARS1_TOT_FDA),
    Q1 = quantile(UMSARS1_TOT_FDA, 0.25),
    Q3 = quantile(UMSARS1_TOT_FDA, 0.75),
    min=min(UMSARS1_TOT_FDA),
    max=max(UMSARS1_TOT_FDA),
    n=n()
  )


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
  ) %>% group_by(DIAGNIV) %>%
  summarise(mean= mean((UMSARS1_TOT_FDA-Baseline)/TIME_STUDY)) 


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
  mutate(Diff= (UMSARS1_TOT_FDA-Baseline)/TIME_STUDY) %>%
  group_by(DIAGNIV) %>%
  summarise(
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