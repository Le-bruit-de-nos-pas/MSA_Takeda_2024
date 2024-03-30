

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

