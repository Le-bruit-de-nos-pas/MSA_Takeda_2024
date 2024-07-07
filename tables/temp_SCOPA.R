# Inputs SCOPA AUT V2 ----------------------------------


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
  select(NUM, DATECONSULT, TIME_STUDY, Year) %>% rename("DATE"="DATECONSULT")




ams_scopa_bx_2015_16 <- haven::read_sas("Source/ams_scopa_bx_2015_16.sas7bdat")
names(ams_scopa_bx_2015_16)
ams_scopa_bx_2015_16 <- ams_scopa_bx_2015_16 %>% filter(!is.na(DATE))

ams_scopa_bx_2015_16[is.na(ams_scopa_bx_2015_16)] <- 0

mean(ams_scopa_bx_2015_16$SCOPA_TOT, na.rm=T)

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

length(unique(ams_scopa_bx_2015_16$NUM))


SCOPA <- ams_scopa_bx_2015_16 %>% inner_join(SCOPA) 

SCOPA <- SCOPA %>% filter(SCOPA_TOT!=0) 

length(unique(SCOPA$NUM))


SCOPA_Baseline_Pats <- SCOPA %>% filter(Year==0) %>% filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM) %>% distinct() %>%
  inner_join(AllMSA_Pop_Baseline_671)



# ----------------
# Overall MSA Entire V2 -------------------------------

# ALL SCOPA Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(SCOPA) %>% filter(Year==0) %>% 
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_TOT),
    sd=sd(SCOPA_TOT),
    median=median(SCOPA_TOT),
    Q1 = quantile(SCOPA_TOT, 0.25),
    Q3 = quantile(SCOPA_TOT, 0.75),
    min=min(SCOPA_TOT),
    max=max(SCOPA_TOT),
    n=n()
  )



# ALL SCOPA Total Year 1

AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_TOT),
    sd=sd(SCOPA_TOT),
    median=median(SCOPA_TOT),
    Q1 = quantile(SCOPA_TOT, 0.25),
    Q3 = quantile(SCOPA_TOT, 0.75),
    min=min(SCOPA_TOT),
    max=max(SCOPA_TOT),
    n=n()
  )

# Change from baseline

AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  summarise(mean= mean((SCOPA_TOT-Baseline)/TIME_STUDY)) 



AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  mutate(Diff= (SCOPA_TOT-Baseline)) %>% 
  summarise(
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
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
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
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  mutate(Diff= (SCOPA_TOT-Baseline)/TIME_STUDY) %>%
  summarise(
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
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_TOT),
    sd=sd(SCOPA_TOT),
    median=median(SCOPA_TOT),
    Q1 = quantile(SCOPA_TOT, 0.25),
    Q3 = quantile(SCOPA_TOT, 0.75),
    min=min(SCOPA_TOT),
    max=max(SCOPA_TOT),
    n=n()
  )


# Change from baseline

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>% 
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  summarise(mean= mean((SCOPA_TOT-Baseline)/TIME_STUDY)) 



AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  mutate(Diff= (SCOPA_TOT-Baseline)) %>%
  summarise(
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
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
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
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
  ) %>% 
  mutate(Diff= (SCOPA_TOT-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
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
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_TOT),
    sd=sd(SCOPA_TOT),
    median=median(SCOPA_TOT),
    Q1 = quantile(SCOPA_TOT, 0.25),
    Q3 = quantile(SCOPA_TOT, 0.75),
    min=min(SCOPA_TOT),
    max=max(SCOPA_TOT),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  summarise(mean= mean((SCOPA_TOT-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
  ) %>%  
  mutate(Diff= (SCOPA_TOT-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
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
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
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
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
  ) %>% 
  mutate(Diff= (SCOPA_TOT-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
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


# By MSA P vs MSA C V2 -----------------
# ALL SCOPA Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_TOT),
    sd=sd(SCOPA_TOT),
    median=median(SCOPA_TOT),
    Q1 = quantile(SCOPA_TOT, 0.25),
    Q3 = quantile(SCOPA_TOT, 0.75),
    min=min(SCOPA_TOT),
    max=max(SCOPA_TOT),
    n=n()
  )



# ALL SCOPA Total Year 1

AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_TOT),
    sd=sd(SCOPA_TOT),
    median=median(SCOPA_TOT),
    Q1 = quantile(SCOPA_TOT, 0.25),
    Q3 = quantile(SCOPA_TOT, 0.75),
    min=min(SCOPA_TOT),
    max=max(SCOPA_TOT),
    n=n()
  )

# Change from baseline

AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((SCOPA_TOT-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
  ) %>% 
  mutate(Diff= (SCOPA_TOT-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
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
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
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
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
  ) %>% 
  mutate(Diff= (SCOPA_TOT-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
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
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_TOT),
    sd=sd(SCOPA_TOT),
    median=median(SCOPA_TOT),
    Q1 = quantile(SCOPA_TOT, 0.25),
    Q3 = quantile(SCOPA_TOT, 0.75),
    min=min(SCOPA_TOT),
    max=max(SCOPA_TOT),
    n=n()
  )


# Change from baseline

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((SCOPA_TOT-Baseline)/TIME_STUDY)) 



AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
  ) %>% 
  mutate(Diff= (SCOPA_TOT-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
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
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
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
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
  ) %>% 
  mutate(Diff= (SCOPA_TOT-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
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
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_TOT),
    sd=sd(SCOPA_TOT),
    median=median(SCOPA_TOT),
    Q1 = quantile(SCOPA_TOT, 0.25),
    Q3 = quantile(SCOPA_TOT, 0.75),
    min=min(SCOPA_TOT),
    max=max(SCOPA_TOT),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
  ) %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(mean= mean((SCOPA_TOT-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  mutate(Diff= (SCOPA_TOT-Baseline)) %>%
  group_by(DIAG) %>%
  summarise(
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
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
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
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
  ) %>% 
  mutate(Diff= (SCOPA_TOT-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
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
# By Possible vs probable V2 --------------------

# ALL SCOPA Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_TOT),
    sd=sd(SCOPA_TOT),
    median=median(SCOPA_TOT),
    Q1 = quantile(SCOPA_TOT, 0.25),
    Q3 = quantile(SCOPA_TOT, 0.75),
    min=min(SCOPA_TOT),
    max=max(SCOPA_TOT),
    n=n()
  )



# ALL SCOPA Total Year 1

AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_TOT),
    sd=sd(SCOPA_TOT),
    median=median(SCOPA_TOT),
    Q1 = quantile(SCOPA_TOT, 0.25),
    Q3 = quantile(SCOPA_TOT, 0.75),
    min=min(SCOPA_TOT),
    max=max(SCOPA_TOT),
    n=n()
  )

# Change from baseline


AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((SCOPA_TOT-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
  ) %>% 
  mutate(Diff= (SCOPA_TOT-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
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
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
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
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
  ) %>% 
  mutate(Diff= (SCOPA_TOT-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
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
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_TOT),
    sd=sd(SCOPA_TOT),
    median=median(SCOPA_TOT),
    Q1 = quantile(SCOPA_TOT, 0.25),
    Q3 = quantile(SCOPA_TOT, 0.75),
    min=min(SCOPA_TOT),
    max=max(SCOPA_TOT),
    n=n()
  )


# Change from baseline

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((SCOPA_TOT-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
  ) %>% 
  mutate(Diff= (SCOPA_TOT-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
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
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
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
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
  ) %>% 
  mutate(Diff= (SCOPA_TOT-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
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
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_TOT),
    sd=sd(SCOPA_TOT),
    median=median(SCOPA_TOT),
    Q1 = quantile(SCOPA_TOT, 0.25),
    Q3 = quantile(SCOPA_TOT, 0.75),
    min=min(SCOPA_TOT),
    max=max(SCOPA_TOT),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
  ) %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(mean= mean((SCOPA_TOT-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
  ) %>% 
  mutate(Diff= (SCOPA_TOT-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
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
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
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
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
  ) %>% 
  mutate(Diff= (SCOPA_TOT-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
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
# Overall Early CT V2  -------------------------------

# ALL SCOPA Total Year 0 


EarlyCT_Pop_Baseline_319 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_TOT),
    sd=sd(SCOPA_TOT),
    median=median(SCOPA_TOT),
    Q1 = quantile(SCOPA_TOT, 0.25),
    Q3 = quantile(SCOPA_TOT, 0.75),
    min=min(SCOPA_TOT),
    max=max(SCOPA_TOT),
    n=n()
  )



# ALL SCOPA Total Year 1

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_TOT),
    sd=sd(SCOPA_TOT),
    median=median(SCOPA_TOT),
    Q1 = quantile(SCOPA_TOT, 0.25),
    Q3 = quantile(SCOPA_TOT, 0.75),
    min=min(SCOPA_TOT),
    max=max(SCOPA_TOT),
    n=n()
  )

# Change from baseline

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  summarise(mean= mean((SCOPA_TOT-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
  ) %>% 
  mutate(Diff= (SCOPA_TOT-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
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
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
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
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
  ) %>% 
  mutate(Diff= (SCOPA_TOT-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
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
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_TOT),
    sd=sd(SCOPA_TOT),
    median=median(SCOPA_TOT),
    Q1 = quantile(SCOPA_TOT, 0.25),
    Q3 = quantile(SCOPA_TOT, 0.75),
    min=min(SCOPA_TOT),
    max=max(SCOPA_TOT),
    n=n()
  )


# Change from baseline

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  summarise(mean= mean((SCOPA_TOT-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
  ) %>% 
  mutate(Diff= (SCOPA_TOT-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
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
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
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
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
  ) %>% 
  mutate(Diff= (SCOPA_TOT-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
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
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_TOT),
    sd=sd(SCOPA_TOT),
    median=median(SCOPA_TOT),
    Q1 = quantile(SCOPA_TOT, 0.25),
    Q3 = quantile(SCOPA_TOT, 0.75),
    min=min(SCOPA_TOT),
    max=max(SCOPA_TOT),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  summarise(mean= mean((SCOPA_TOT-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
  ) %>% 
  mutate(Diff= (SCOPA_TOT-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
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
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
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
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
  ) %>% 
  mutate(Diff= (SCOPA_TOT-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
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


# By MSA P vs MSA C V2 -----------------
# ALL SCOPA Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_TOT),
    sd=sd(SCOPA_TOT),
    median=median(SCOPA_TOT),
    Q1 = quantile(SCOPA_TOT, 0.25),
    Q3 = quantile(SCOPA_TOT, 0.75),
    min=min(SCOPA_TOT),
    max=max(SCOPA_TOT),
    n=n()
  )



# ALL SCOPA Total Year 1

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_TOT),
    sd=sd(SCOPA_TOT),
    median=median(SCOPA_TOT),
    Q1 = quantile(SCOPA_TOT, 0.25),
    Q3 = quantile(SCOPA_TOT, 0.75),
    min=min(SCOPA_TOT),
    max=max(SCOPA_TOT),
    n=n()
  )

# Change from baseline

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((SCOPA_TOT-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
  ) %>% 
  mutate(Diff= (SCOPA_TOT-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>% 
  summarise(
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
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
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
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
  ) %>% 
  mutate(Diff= (SCOPA_TOT-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
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
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_TOT),
    sd=sd(SCOPA_TOT),
    median=median(SCOPA_TOT),
    Q1 = quantile(SCOPA_TOT, 0.25),
    Q3 = quantile(SCOPA_TOT, 0.75),
    min=min(SCOPA_TOT),
    max=max(SCOPA_TOT),
    n=n()
  )


# Change from baseline

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
  ) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((SCOPA_TOT-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
  ) %>% 
  mutate(Diff= (SCOPA_TOT-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
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
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
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
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
  ) %>% 
  mutate(Diff= (SCOPA_TOT-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
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
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_TOT),
    sd=sd(SCOPA_TOT),
    median=median(SCOPA_TOT),
    Q1 = quantile(SCOPA_TOT, 0.25),
    Q3 = quantile(SCOPA_TOT, 0.75),
    min=min(SCOPA_TOT),
    max=max(SCOPA_TOT),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
  ) %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(mean= mean((SCOPA_TOT-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
  ) %>% 
  mutate(Diff= (SCOPA_TOT-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
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
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
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
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
  ) %>% 
  mutate(Diff= (SCOPA_TOT-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
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
# By Possible vs probable V2 --------------------

# ALL SCOPA Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_TOT),
    sd=sd(SCOPA_TOT),
    median=median(SCOPA_TOT),
    Q1 = quantile(SCOPA_TOT, 0.25),
    Q3 = quantile(SCOPA_TOT, 0.75),
    min=min(SCOPA_TOT),
    max=max(SCOPA_TOT),
    n=n()
  )



# ALL SCOPA Total Year 1

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_TOT),
    sd=sd(SCOPA_TOT),
    median=median(SCOPA_TOT),
    Q1 = quantile(SCOPA_TOT, 0.25),
    Q3 = quantile(SCOPA_TOT, 0.75),
    min=min(SCOPA_TOT),
    max=max(SCOPA_TOT),
    n=n()
  )

# Change from baseline


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((SCOPA_TOT-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
  ) %>% 
  mutate(Diff= (SCOPA_TOT-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
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
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
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
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
  ) %>% 
  mutate(Diff= (SCOPA_TOT-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
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
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_TOT),
    sd=sd(SCOPA_TOT),
    median=median(SCOPA_TOT),
    Q1 = quantile(SCOPA_TOT, 0.25),
    Q3 = quantile(SCOPA_TOT, 0.75),
    min=min(SCOPA_TOT),
    max=max(SCOPA_TOT),
    n=n()
  )


# Change from baseline

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((SCOPA_TOT-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
  ) %>% 
  mutate(Diff= (SCOPA_TOT-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
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
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
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
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
  ) %>% 
  mutate(Diff= (SCOPA_TOT-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
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
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_TOT),
    sd=sd(SCOPA_TOT),
    median=median(SCOPA_TOT),
    Q1 = quantile(SCOPA_TOT, 0.25),
    Q3 = quantile(SCOPA_TOT, 0.75),
    min=min(SCOPA_TOT),
    max=max(SCOPA_TOT),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
  ) %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(mean= mean((SCOPA_TOT-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
  ) %>% 
  mutate(Diff= (SCOPA_TOT-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% 
  summarise(
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
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
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
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
  ) %>% 
  mutate(Diff= (SCOPA_TOT-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
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


# Student T-tests for deltas in SCOPA AUT Prob vs Poss, MSA-C vs MSA-P V2 ---------------------------------------
library(rstatix)

# Inputs SCOPA Total V2 ----------------------------------


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
  select(NUM, DATECONSULT, TIME_STUDY, Year) %>% rename("DATE"="DATECONSULT")




ams_scopa_bx_2015_16 <- haven::read_sas("Source/ams_scopa_bx_2015_16.sas7bdat")
names(ams_scopa_bx_2015_16)
ams_scopa_bx_2015_16 <- ams_scopa_bx_2015_16 %>% filter(!is.na(DATE))

ams_scopa_bx_2015_16[is.na(ams_scopa_bx_2015_16)] <- 0

mean(ams_scopa_bx_2015_16$SCOPA_TOT, na.rm=T)

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

length(unique(ams_scopa_bx_2015_16$NUM))


SCOPA <- ams_scopa_bx_2015_16 %>% inner_join(SCOPA) 

SCOPA <- SCOPA %>% filter(SCOPA_TOT!=0) 

length(unique(SCOPA$NUM))


SCOPA_Baseline_Pats <- SCOPA %>% filter(Year==0) %>% filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM) %>% distinct() %>%
  inner_join(AllMSA_Pop_Baseline_671)



# ----------------
# Overall MSA Entire V2 -------------------------------

# ALL UMSARS Total Year 1 

AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==0) %>% inner_join(SCOPA_Baseline_Pats) %>%
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>%
  left_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
  ) %>% 
  mutate(Diff= (SCOPA_TOT-Baseline))  %>%
  t_test(Diff~DIAG, detailed = F, var.equal = TRUE) 
  #t_test(Diff~DIAGNIV, detailed = F, var.equal = TRUE) 


# ALL UMSARS Total Year 2 

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==0) %>% inner_join(SCOPA_Baseline_Pats) %>%
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>%
  left_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
  ) %>% 
  mutate(Diff= (SCOPA_TOT-Baseline))  %>%
  #t_test(Diff~DIAG, var.equal = TRUE) 
t_test(Diff~DIAGNIV,  var.equal = TRUE) 


# ALL UMSARS Total Year 3

AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==0) %>% inner_join(SCOPA_Baseline_Pats) %>%
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>%
  left_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
  ) %>% 
  mutate(Diff= (SCOPA_TOT-Baseline))   %>%
  t_test(Diff~DIAG, var.equal = TRUE) 
  #t_test(Diff~DIAGNIV,  var.equal = TRUE) 

# -----------------------------
# Overall Early CT V2  -------------------------------

# ALL UMSARS Total Year 1


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>% inner_join(SCOPA_Baseline_Pats) %>%
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>%
  left_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
  ) %>% 
  mutate(Diff= (SCOPA_TOT-Baseline))  %>%
  #t_test(Diff~DIAG, var.equal = TRUE) 
t_test(Diff~DIAGNIV,  var.equal = TRUE) 




# ALL UMSARS Total Year 2


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>% inner_join(SCOPA_Baseline_Pats) %>%
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>%
  left_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
  ) %>% 
  mutate(Diff= (SCOPA_TOT-Baseline))  %>%
  t_test(Diff~DIAG, var.equal = TRUE) 
  #t_test(Diff~DIAGNIV,  var.equal = TRUE) 

# ALL UMSARS Total Year 3


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>% inner_join(SCOPA_Baseline_Pats) %>%
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>%
  left_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
  ) %>% 
  mutate(Diff= (SCOPA_TOT-Baseline))  %>%
  t_test(Diff~DIAG, var.equal = TRUE) 
   #t_test(Diff~DIAGNIV,  var.equal = TRUE) 

# --------------


# Correlation between Delta SCOPA AUT & Delta FDA-modified UMSARS V2 ------------------

# Inputs SCOPA Total V2 ----------------------------------


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
  select(NUM, DATECONSULT, TIME_STUDY, Year) %>% rename("DATE"="DATECONSULT")




ams_scopa_bx_2015_16 <- haven::read_sas("Source/ams_scopa_bx_2015_16.sas7bdat")
names(ams_scopa_bx_2015_16)
ams_scopa_bx_2015_16 <- ams_scopa_bx_2015_16 %>% filter(!is.na(DATE))

ams_scopa_bx_2015_16[is.na(ams_scopa_bx_2015_16)] <- 0

mean(ams_scopa_bx_2015_16$SCOPA_TOT, na.rm=T)

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

length(unique(ams_scopa_bx_2015_16$NUM))


SCOPA <- ams_scopa_bx_2015_16 %>% inner_join(SCOPA) 

SCOPA <- SCOPA %>% filter(SCOPA_TOT!=0) 

length(unique(SCOPA$NUM))


SCOPA_Baseline_Pats <- SCOPA %>% filter(Year==0) %>% filter(!is.na(SCOPA_TOT)) %>%
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
# Overall MSA Entire V2 -------------------------------

# Year 1

AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==0) %>% inner_join(SCOPA_Baseline_Pats) %>%
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>%
  left_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
  ) %>% 
  mutate(Diff= (SCOPA_TOT-Baseline))  %>% select(NUM, DIAG, DIAGNIV, Diff) %>% rename("SCOPA_Diff"="Diff") %>% drop_na() %>%
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
  ) %>%  summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))
  # group_by(DIAG) %>% summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))
  #group_by(DIAGNIV) %>% summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))




# Year 2 

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==0) %>% inner_join(SCOPA_Baseline_Pats) %>%
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>%
  left_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
  ) %>% 
  mutate(Diff= (SCOPA_TOT-Baseline))  %>% select(NUM, DIAG, DIAGNIV, Diff) %>% rename("SCOPA_Diff"="Diff") %>% drop_na()  %>%
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
  )  %>%  summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))
   #group_by(DIAG) %>% summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))
  #group_by(DIAGNIV) %>% summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))




# Year 3

AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==0) %>% inner_join(SCOPA_Baseline_Pats) %>%
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>%
  left_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
  ) %>% 
  mutate(Diff= (SCOPA_TOT-Baseline))  %>% select(NUM, DIAG, DIAGNIV, Diff) %>% rename("SCOPA_Diff"="Diff") %>% drop_na() %>%
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
  )  %>%  summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))
  #group_by(DIAG) %>% summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))
  #group_by(DIAGNIV) %>% summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))


# -----------------------------
# Overall Early CT V2  -------------------------------

# Year 1


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>% inner_join(SCOPA_Baseline_Pats) %>%
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>%
  left_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
  ) %>% 
  mutate(Diff= (SCOPA_TOT-Baseline))  %>% select(NUM, DIAG, DIAGNIV, Diff) %>% rename("SCOPA_Diff"="Diff") %>% drop_na() %>%
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
  )  %>% summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))
  #group_by(DIAG) %>% summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))
  #group_by(DIAGNIV) %>% summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))





# Year 2


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>% inner_join(SCOPA_Baseline_Pats) %>%
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>%
  left_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
  ) %>% 
  mutate(Diff= (SCOPA_TOT-Baseline))  %>% select(NUM, DIAG, DIAGNIV, Diff) %>% rename("SCOPA_Diff"="Diff") %>% drop_na() %>%
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
  )  %>%  summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))
   #group_by(DIAG) %>% summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))
  #group_by(DIAGNIV) %>% summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))


# Year 3


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>% inner_join(SCOPA_Baseline_Pats) %>%
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
  rename("Baseline"="SCOPA_TOT") %>%
  left_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
  ) %>% 
  mutate(Diff= (SCOPA_TOT-Baseline))  %>% select(NUM, DIAG, DIAGNIV, Diff) %>% rename("SCOPA_Diff"="Diff") %>% drop_na() %>%
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
  )  %>%  summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))
  #group_by(DIAG) %>% summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))
  #group_by(DIAGNIV) %>% summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))


# --------------



# Inputs SCOPA AUT V2 DIGESTIVE ----------------------------------


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
  select(NUM, DATECONSULT, TIME_STUDY, Year) %>% rename("DATE"="DATECONSULT")




ams_scopa_bx_2015_16 <- haven::read_sas("Source/ams_scopa_bx_2015_16.sas7bdat")
names(ams_scopa_bx_2015_16)
ams_scopa_bx_2015_16 <- ams_scopa_bx_2015_16 %>% filter(!is.na(DATE))

ams_scopa_bx_2015_16[is.na(ams_scopa_bx_2015_16)] <- 0

mean(ams_scopa_bx_2015_16$SCOPA_TOT, na.rm=T)

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

length(unique(ams_scopa_bx_2015_16$NUM))


SCOPA <- ams_scopa_bx_2015_16 %>% inner_join(SCOPA) 

SCOPA <- SCOPA %>% filter(SCOPA_TOT!=0) 

length(unique(SCOPA$NUM))


SCOPA_Baseline_Pats <- SCOPA %>% filter(Year==0) %>% filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM) %>% distinct() %>%
  inner_join(AllMSA_Pop_Baseline_671)



# ----------------
# Overall MSA Entire V2 DIGESTIVE -------------------------------

# ALL SCOPA Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(SCOPA) %>% filter(Year==0) %>% 
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_DIG),
    sd=sd(SCOPA_DIG),
    median=median(SCOPA_DIG),
    Q1 = quantile(SCOPA_DIG, 0.25),
    Q3 = quantile(SCOPA_DIG, 0.75),
    min=min(SCOPA_DIG),
    max=max(SCOPA_DIG),
    n=n()
  )



# ALL SCOPA Total Year 1

AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_DIG),
    sd=sd(SCOPA_DIG),
    median=median(SCOPA_DIG),
    Q1 = quantile(SCOPA_DIG, 0.25),
    Q3 = quantile(SCOPA_DIG, 0.75),
    min=min(SCOPA_DIG),
    max=max(SCOPA_DIG),
    n=n()
  )

# Change from baseline

AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
  rename("Baseline"="SCOPA_DIG") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_DIG)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  summarise(mean= mean((SCOPA_DIG-Baseline)/TIME_STUDY)) 



AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
  rename("Baseline"="SCOPA_DIG") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_DIG)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  mutate(Diff= (SCOPA_DIG-Baseline)) %>% 
  summarise(
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
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
  rename("Baseline"="SCOPA_DIG") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_DIG)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
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
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
  rename("Baseline"="SCOPA_DIG") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_DIG)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  mutate(Diff= (SCOPA_DIG-Baseline)/TIME_STUDY) %>%
  summarise(
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
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_DIG),
    sd=sd(SCOPA_DIG),
    median=median(SCOPA_DIG),
    Q1 = quantile(SCOPA_DIG, 0.25),
    Q3 = quantile(SCOPA_DIG, 0.75),
    min=min(SCOPA_DIG),
    max=max(SCOPA_DIG),
    n=n()
  )


# Change from baseline

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
  rename("Baseline"="SCOPA_DIG") %>% 
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_DIG)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  summarise(mean= mean((SCOPA_DIG-Baseline)/TIME_STUDY)) 



AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
  rename("Baseline"="SCOPA_DIG") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_DIG)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  mutate(Diff= (SCOPA_DIG-Baseline)) %>%
  summarise(
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
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
  rename("Baseline"="SCOPA_DIG") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_DIG)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
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
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
  rename("Baseline"="SCOPA_DIG") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_DIG)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
  ) %>% 
  mutate(Diff= (SCOPA_DIG-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
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
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_DIG),
    sd=sd(SCOPA_DIG),
    median=median(SCOPA_DIG),
    Q1 = quantile(SCOPA_DIG, 0.25),
    Q3 = quantile(SCOPA_DIG, 0.75),
    min=min(SCOPA_DIG),
    max=max(SCOPA_DIG),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
  rename("Baseline"="SCOPA_DIG") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_DIG)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  summarise(mean= mean((SCOPA_DIG-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
  rename("Baseline"="SCOPA_DIG") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_DIG)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
  ) %>%  
  mutate(Diff= (SCOPA_DIG-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
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
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
  rename("Baseline"="SCOPA_DIG") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_DIG)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
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
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
  rename("Baseline"="SCOPA_DIG") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_DIG)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
  ) %>% 
  mutate(Diff= (SCOPA_DIG-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
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


# By MSA P vs MSA C V2 DIGESTIVE -----------------
# ALL SCOPA Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_DIG),
    sd=sd(SCOPA_DIG),
    median=median(SCOPA_DIG),
    Q1 = quantile(SCOPA_DIG, 0.25),
    Q3 = quantile(SCOPA_DIG, 0.75),
    min=min(SCOPA_DIG),
    max=max(SCOPA_DIG),
    n=n()
  )



# ALL SCOPA Total Year 1

AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_DIG),
    sd=sd(SCOPA_DIG),
    median=median(SCOPA_DIG),
    Q1 = quantile(SCOPA_DIG, 0.25),
    Q3 = quantile(SCOPA_DIG, 0.75),
    min=min(SCOPA_DIG),
    max=max(SCOPA_DIG),
    n=n()
  )

# Change from baseline

AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
  rename("Baseline"="SCOPA_DIG") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_DIG)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((SCOPA_DIG-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
  rename("Baseline"="SCOPA_DIG") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_DIG)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
  ) %>% 
  mutate(Diff= (SCOPA_DIG-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
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
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
  rename("Baseline"="SCOPA_DIG") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_DIG)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
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
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
  rename("Baseline"="SCOPA_DIG") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_DIG)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
  ) %>% 
  mutate(Diff= (SCOPA_DIG-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
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
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_DIG),
    sd=sd(SCOPA_DIG),
    median=median(SCOPA_DIG),
    Q1 = quantile(SCOPA_DIG, 0.25),
    Q3 = quantile(SCOPA_DIG, 0.75),
    min=min(SCOPA_DIG),
    max=max(SCOPA_DIG),
    n=n()
  )


# Change from baseline

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
  rename("Baseline"="SCOPA_DIG") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_DIG)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((SCOPA_DIG-Baseline)/TIME_STUDY)) 



AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
  rename("Baseline"="SCOPA_DIG") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_DIG)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
  ) %>% 
  mutate(Diff= (SCOPA_DIG-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
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
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
  rename("Baseline"="SCOPA_DIG") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_DIG)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
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
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
  rename("Baseline"="SCOPA_DIG") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_DIG)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
  ) %>% 
  mutate(Diff= (SCOPA_DIG-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
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
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_DIG),
    sd=sd(SCOPA_DIG),
    median=median(SCOPA_DIG),
    Q1 = quantile(SCOPA_DIG, 0.25),
    Q3 = quantile(SCOPA_DIG, 0.75),
    min=min(SCOPA_DIG),
    max=max(SCOPA_DIG),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
  rename("Baseline"="SCOPA_DIG") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_DIG)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
  ) %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(mean= mean((SCOPA_DIG-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
  rename("Baseline"="SCOPA_DIG") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_DIG)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  mutate(Diff= (SCOPA_DIG-Baseline)) %>%
  group_by(DIAG) %>%
  summarise(
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
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
  rename("Baseline"="SCOPA_DIG") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_DIG)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
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
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
  rename("Baseline"="SCOPA_DIG") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_DIG)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
  ) %>% 
  mutate(Diff= (SCOPA_DIG-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
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
# By Possible vs probable V2 DIGESTIVE --------------------

# ALL SCOPA Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_DIG),
    sd=sd(SCOPA_DIG),
    median=median(SCOPA_DIG),
    Q1 = quantile(SCOPA_DIG, 0.25),
    Q3 = quantile(SCOPA_DIG, 0.75),
    min=min(SCOPA_DIG),
    max=max(SCOPA_DIG),
    n=n()
  )



# ALL SCOPA Total Year 1

AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_DIG),
    sd=sd(SCOPA_DIG),
    median=median(SCOPA_DIG),
    Q1 = quantile(SCOPA_DIG, 0.25),
    Q3 = quantile(SCOPA_DIG, 0.75),
    min=min(SCOPA_DIG),
    max=max(SCOPA_DIG),
    n=n()
  )

# Change from baseline


AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
  rename("Baseline"="SCOPA_DIG") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_DIG)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((SCOPA_DIG-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
  rename("Baseline"="SCOPA_DIG") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_DIG)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
  ) %>% 
  mutate(Diff= (SCOPA_DIG-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
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
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
  rename("Baseline"="SCOPA_DIG") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_DIG)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
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
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
  rename("Baseline"="SCOPA_DIG") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_DIG)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
  ) %>% 
  mutate(Diff= (SCOPA_DIG-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
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
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_DIG),
    sd=sd(SCOPA_DIG),
    median=median(SCOPA_DIG),
    Q1 = quantile(SCOPA_DIG, 0.25),
    Q3 = quantile(SCOPA_DIG, 0.75),
    min=min(SCOPA_DIG),
    max=max(SCOPA_DIG),
    n=n()
  )


# Change from baseline

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
  rename("Baseline"="SCOPA_DIG") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_DIG)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((SCOPA_DIG-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
  rename("Baseline"="SCOPA_DIG") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_DIG)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
  ) %>% 
  mutate(Diff= (SCOPA_DIG-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
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
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
  rename("Baseline"="SCOPA_DIG") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_DIG)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
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
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
  rename("Baseline"="SCOPA_DIG") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_DIG)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
  ) %>% 
  mutate(Diff= (SCOPA_DIG-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
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
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_DIG),
    sd=sd(SCOPA_DIG),
    median=median(SCOPA_DIG),
    Q1 = quantile(SCOPA_DIG, 0.25),
    Q3 = quantile(SCOPA_DIG, 0.75),
    min=min(SCOPA_DIG),
    max=max(SCOPA_DIG),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
  rename("Baseline"="SCOPA_DIG") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_DIG)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
  ) %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(mean= mean((SCOPA_DIG-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
  rename("Baseline"="SCOPA_DIG") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_DIG)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
  ) %>% 
  mutate(Diff= (SCOPA_DIG-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
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
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
  rename("Baseline"="SCOPA_DIG") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_DIG)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
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
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
  rename("Baseline"="SCOPA_DIG") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_DIG)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
  ) %>% 
  mutate(Diff= (SCOPA_DIG-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
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
# Overall Early CT V2 DIGESTIVE  -------------------------------

# ALL SCOPA Total Year 0 


EarlyCT_Pop_Baseline_319 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_DIG),
    sd=sd(SCOPA_DIG),
    median=median(SCOPA_DIG),
    Q1 = quantile(SCOPA_DIG, 0.25),
    Q3 = quantile(SCOPA_DIG, 0.75),
    min=min(SCOPA_DIG),
    max=max(SCOPA_DIG),
    n=n()
  )



# ALL SCOPA Total Year 1

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_DIG),
    sd=sd(SCOPA_DIG),
    median=median(SCOPA_DIG),
    Q1 = quantile(SCOPA_DIG, 0.25),
    Q3 = quantile(SCOPA_DIG, 0.75),
    min=min(SCOPA_DIG),
    max=max(SCOPA_DIG),
    n=n()
  )

# Change from baseline

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
  rename("Baseline"="SCOPA_DIG") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_DIG)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  summarise(mean= mean((SCOPA_DIG-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
  rename("Baseline"="SCOPA_DIG") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_DIG)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
  ) %>% 
  mutate(Diff= (SCOPA_DIG-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
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
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
  rename("Baseline"="SCOPA_DIG") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_DIG)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
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
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
  rename("Baseline"="SCOPA_DIG") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_DIG)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
  ) %>% 
  mutate(Diff= (SCOPA_DIG-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
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
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_DIG),
    sd=sd(SCOPA_DIG),
    median=median(SCOPA_DIG),
    Q1 = quantile(SCOPA_DIG, 0.25),
    Q3 = quantile(SCOPA_DIG, 0.75),
    min=min(SCOPA_DIG),
    max=max(SCOPA_DIG),
    n=n()
  )


# Change from baseline

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
  rename("Baseline"="SCOPA_DIG") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_DIG)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  summarise(mean= mean((SCOPA_DIG-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
  rename("Baseline"="SCOPA_DIG") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_DIG)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
  ) %>% 
  mutate(Diff= (SCOPA_DIG-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
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
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
  rename("Baseline"="SCOPA_DIG") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_DIG)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
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
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
  rename("Baseline"="SCOPA_DIG") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_DIG)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
  ) %>% 
  mutate(Diff= (SCOPA_DIG-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
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
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_DIG),
    sd=sd(SCOPA_DIG),
    median=median(SCOPA_DIG),
    Q1 = quantile(SCOPA_DIG, 0.25),
    Q3 = quantile(SCOPA_DIG, 0.75),
    min=min(SCOPA_DIG),
    max=max(SCOPA_DIG),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
  rename("Baseline"="SCOPA_DIG") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_DIG)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  summarise(mean= mean((SCOPA_DIG-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
  rename("Baseline"="SCOPA_DIG") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_DIG)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
  ) %>% 
  mutate(Diff= (SCOPA_DIG-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
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
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
  rename("Baseline"="SCOPA_DIG") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_DIG)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
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
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
  rename("Baseline"="SCOPA_DIG") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_DIG)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
  ) %>% 
  mutate(Diff= (SCOPA_DIG-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
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


# By MSA P vs MSA C V2 DIGESTIVE -----------------
# ALL SCOPA Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_DIG),
    sd=sd(SCOPA_DIG),
    median=median(SCOPA_DIG),
    Q1 = quantile(SCOPA_DIG, 0.25),
    Q3 = quantile(SCOPA_DIG, 0.75),
    min=min(SCOPA_DIG),
    max=max(SCOPA_DIG),
    n=n()
  )



# ALL SCOPA Total Year 1

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_DIG),
    sd=sd(SCOPA_DIG),
    median=median(SCOPA_DIG),
    Q1 = quantile(SCOPA_DIG, 0.25),
    Q3 = quantile(SCOPA_DIG, 0.75),
    min=min(SCOPA_DIG),
    max=max(SCOPA_DIG),
    n=n()
  )

# Change from baseline

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
  rename("Baseline"="SCOPA_DIG") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_DIG)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((SCOPA_DIG-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
  rename("Baseline"="SCOPA_DIG") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_DIG)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
  ) %>% 
  mutate(Diff= (SCOPA_DIG-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>% 
  summarise(
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
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
  rename("Baseline"="SCOPA_DIG") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_DIG)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
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
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
  rename("Baseline"="SCOPA_DIG") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_DIG)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
  ) %>% 
  mutate(Diff= (SCOPA_DIG-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
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
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_DIG),
    sd=sd(SCOPA_DIG),
    median=median(SCOPA_DIG),
    Q1 = quantile(SCOPA_DIG, 0.25),
    Q3 = quantile(SCOPA_DIG, 0.75),
    min=min(SCOPA_DIG),
    max=max(SCOPA_DIG),
    n=n()
  )


# Change from baseline

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
  rename("Baseline"="SCOPA_DIG") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_DIG)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
  ) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((SCOPA_DIG-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
  rename("Baseline"="SCOPA_DIG") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_DIG)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
  ) %>% 
  mutate(Diff= (SCOPA_DIG-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
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
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
  rename("Baseline"="SCOPA_DIG") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_DIG)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
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
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
  rename("Baseline"="SCOPA_DIG") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_DIG)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
  ) %>% 
  mutate(Diff= (SCOPA_DIG-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
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
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_DIG),
    sd=sd(SCOPA_DIG),
    median=median(SCOPA_DIG),
    Q1 = quantile(SCOPA_DIG, 0.25),
    Q3 = quantile(SCOPA_DIG, 0.75),
    min=min(SCOPA_DIG),
    max=max(SCOPA_DIG),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
  rename("Baseline"="SCOPA_DIG") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_DIG)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
  ) %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(mean= mean((SCOPA_DIG-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
  rename("Baseline"="SCOPA_DIG") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_DIG)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
  ) %>% 
  mutate(Diff= (SCOPA_DIG-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
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
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
  rename("Baseline"="SCOPA_DIG") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_DIG)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
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
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
  rename("Baseline"="SCOPA_DIG") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_DIG)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
  ) %>% 
  mutate(Diff= (SCOPA_DIG-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
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
# By Possible vs probable V2 DIGESTIVE --------------------

# ALL SCOPA Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_DIG),
    sd=sd(SCOPA_DIG),
    median=median(SCOPA_DIG),
    Q1 = quantile(SCOPA_DIG, 0.25),
    Q3 = quantile(SCOPA_DIG, 0.75),
    min=min(SCOPA_DIG),
    max=max(SCOPA_DIG),
    n=n()
  )



# ALL SCOPA Total Year 1

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_DIG),
    sd=sd(SCOPA_DIG),
    median=median(SCOPA_DIG),
    Q1 = quantile(SCOPA_DIG, 0.25),
    Q3 = quantile(SCOPA_DIG, 0.75),
    min=min(SCOPA_DIG),
    max=max(SCOPA_DIG),
    n=n()
  )

# Change from baseline


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
  rename("Baseline"="SCOPA_DIG") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_DIG)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((SCOPA_DIG-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
  rename("Baseline"="SCOPA_DIG") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_DIG)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
  ) %>% 
  mutate(Diff= (SCOPA_DIG-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
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
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
  rename("Baseline"="SCOPA_DIG") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_DIG)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
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
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
  rename("Baseline"="SCOPA_DIG") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_DIG)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
  ) %>% 
  mutate(Diff= (SCOPA_DIG-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
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
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_DIG),
    sd=sd(SCOPA_DIG),
    median=median(SCOPA_DIG),
    Q1 = quantile(SCOPA_DIG, 0.25),
    Q3 = quantile(SCOPA_DIG, 0.75),
    min=min(SCOPA_DIG),
    max=max(SCOPA_DIG),
    n=n()
  )


# Change from baseline

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
  rename("Baseline"="SCOPA_DIG") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_DIG)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((SCOPA_DIG-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
  rename("Baseline"="SCOPA_DIG") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_DIG)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
  ) %>% 
  mutate(Diff= (SCOPA_DIG-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
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
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
  rename("Baseline"="SCOPA_DIG") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_DIG)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
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
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
  rename("Baseline"="SCOPA_DIG") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_DIG)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
  ) %>% 
  mutate(Diff= (SCOPA_DIG-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
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
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_DIG),
    sd=sd(SCOPA_DIG),
    median=median(SCOPA_DIG),
    Q1 = quantile(SCOPA_DIG, 0.25),
    Q3 = quantile(SCOPA_DIG, 0.75),
    min=min(SCOPA_DIG),
    max=max(SCOPA_DIG),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
  rename("Baseline"="SCOPA_DIG") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_DIG)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
  ) %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(mean= mean((SCOPA_DIG-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
  rename("Baseline"="SCOPA_DIG") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_DIG)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
  ) %>% 
  mutate(Diff= (SCOPA_DIG-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% 
  summarise(
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
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
  rename("Baseline"="SCOPA_DIG") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_DIG)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
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
  filter(!is.na(SCOPA_DIG)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
  rename("Baseline"="SCOPA_DIG") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_DIG)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
  ) %>% 
  mutate(Diff= (SCOPA_DIG-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
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


# Inputs SCOPA AUT V2 URINARY ----------------------------------


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
  select(NUM, DATECONSULT, TIME_STUDY, Year) %>% rename("DATE"="DATECONSULT")




ams_scopa_bx_2015_16 <- haven::read_sas("Source/ams_scopa_bx_2015_16.sas7bdat")
names(ams_scopa_bx_2015_16)
ams_scopa_bx_2015_16 <- ams_scopa_bx_2015_16 %>% filter(!is.na(DATE))

ams_scopa_bx_2015_16[is.na(ams_scopa_bx_2015_16)] <- 0

mean(ams_scopa_bx_2015_16$SCOPA_TOT, na.rm=T)

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

length(unique(ams_scopa_bx_2015_16$NUM))


SCOPA <- ams_scopa_bx_2015_16 %>% inner_join(SCOPA) 

SCOPA <- SCOPA %>% filter(SCOPA_TOT!=0) 

length(unique(SCOPA$NUM))


SCOPA_Baseline_Pats <- SCOPA %>% filter(Year==0) %>% filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM) %>% distinct() %>%
  inner_join(AllMSA_Pop_Baseline_671)



# ----------------
# Overall MSA Entire V2 URINARY -------------------------------

# ALL SCOPA Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(SCOPA) %>% filter(Year==0) %>% 
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_URI),
    sd=sd(SCOPA_URI),
    median=median(SCOPA_URI),
    Q1 = quantile(SCOPA_URI, 0.25),
    Q3 = quantile(SCOPA_URI, 0.75),
    min=min(SCOPA_URI),
    max=max(SCOPA_URI),
    n=n()
  )



# ALL SCOPA Total Year 1

AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_URI),
    sd=sd(SCOPA_URI),
    median=median(SCOPA_URI),
    Q1 = quantile(SCOPA_URI, 0.25),
    Q3 = quantile(SCOPA_URI, 0.75),
    min=min(SCOPA_URI),
    max=max(SCOPA_URI),
    n=n()
  )

# Change from baseline

AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
  rename("Baseline"="SCOPA_URI") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_URI)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  summarise(mean= mean((SCOPA_URI-Baseline)/TIME_STUDY)) 



AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
  rename("Baseline"="SCOPA_URI") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_URI)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  mutate(Diff= (SCOPA_URI-Baseline)) %>% 
  summarise(
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
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
  rename("Baseline"="SCOPA_URI") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_URI)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
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
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
  rename("Baseline"="SCOPA_URI") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_URI)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  mutate(Diff= (SCOPA_URI-Baseline)/TIME_STUDY) %>%
  summarise(
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
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_URI),
    sd=sd(SCOPA_URI),
    median=median(SCOPA_URI),
    Q1 = quantile(SCOPA_URI, 0.25),
    Q3 = quantile(SCOPA_URI, 0.75),
    min=min(SCOPA_URI),
    max=max(SCOPA_URI),
    n=n()
  )


# Change from baseline

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
  rename("Baseline"="SCOPA_URI") %>% 
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_URI)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  summarise(mean= mean((SCOPA_URI-Baseline)/TIME_STUDY)) 



AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
  rename("Baseline"="SCOPA_URI") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_URI)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  mutate(Diff= (SCOPA_URI-Baseline)) %>%
  summarise(
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
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
  rename("Baseline"="SCOPA_URI") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_URI)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
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
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
  rename("Baseline"="SCOPA_URI") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_URI)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
  ) %>% 
  mutate(Diff= (SCOPA_URI-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
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
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_URI),
    sd=sd(SCOPA_URI),
    median=median(SCOPA_URI),
    Q1 = quantile(SCOPA_URI, 0.25),
    Q3 = quantile(SCOPA_URI, 0.75),
    min=min(SCOPA_URI),
    max=max(SCOPA_URI),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
  rename("Baseline"="SCOPA_URI") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_URI)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  summarise(mean= mean((SCOPA_URI-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
  rename("Baseline"="SCOPA_URI") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_URI)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
  ) %>%  
  mutate(Diff= (SCOPA_URI-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
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
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
  rename("Baseline"="SCOPA_URI") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_URI)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
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
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
  rename("Baseline"="SCOPA_URI") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_URI)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
  ) %>% 
  mutate(Diff= (SCOPA_URI-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
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


# By MSA P vs MSA C V2 URINARY -----------------
# ALL SCOPA Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_URI),
    sd=sd(SCOPA_URI),
    median=median(SCOPA_URI),
    Q1 = quantile(SCOPA_URI, 0.25),
    Q3 = quantile(SCOPA_URI, 0.75),
    min=min(SCOPA_URI),
    max=max(SCOPA_URI),
    n=n()
  )



# ALL SCOPA Total Year 1

AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_URI),
    sd=sd(SCOPA_URI),
    median=median(SCOPA_URI),
    Q1 = quantile(SCOPA_URI, 0.25),
    Q3 = quantile(SCOPA_URI, 0.75),
    min=min(SCOPA_URI),
    max=max(SCOPA_URI),
    n=n()
  )

# Change from baseline

AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
  rename("Baseline"="SCOPA_URI") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_URI)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((SCOPA_URI-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
  rename("Baseline"="SCOPA_URI") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_URI)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
  ) %>% 
  mutate(Diff= (SCOPA_URI-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
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
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
  rename("Baseline"="SCOPA_URI") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_URI)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
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
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
  rename("Baseline"="SCOPA_URI") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_URI)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
  ) %>% 
  mutate(Diff= (SCOPA_URI-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
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
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_URI),
    sd=sd(SCOPA_URI),
    median=median(SCOPA_URI),
    Q1 = quantile(SCOPA_URI, 0.25),
    Q3 = quantile(SCOPA_URI, 0.75),
    min=min(SCOPA_URI),
    max=max(SCOPA_URI),
    n=n()
  )


# Change from baseline

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
  rename("Baseline"="SCOPA_URI") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_URI)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((SCOPA_URI-Baseline)/TIME_STUDY)) 



AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
  rename("Baseline"="SCOPA_URI") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_URI)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
  ) %>% 
  mutate(Diff= (SCOPA_URI-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
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
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
  rename("Baseline"="SCOPA_URI") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_URI)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
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
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
  rename("Baseline"="SCOPA_URI") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_URI)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
  ) %>% 
  mutate(Diff= (SCOPA_URI-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
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
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_URI),
    sd=sd(SCOPA_URI),
    median=median(SCOPA_URI),
    Q1 = quantile(SCOPA_URI, 0.25),
    Q3 = quantile(SCOPA_URI, 0.75),
    min=min(SCOPA_URI),
    max=max(SCOPA_URI),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
  rename("Baseline"="SCOPA_URI") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_URI)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
  ) %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(mean= mean((SCOPA_URI-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
  rename("Baseline"="SCOPA_URI") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_URI)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  mutate(Diff= (SCOPA_URI-Baseline)) %>%
  group_by(DIAG) %>%
  summarise(
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
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
  rename("Baseline"="SCOPA_URI") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_URI)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
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
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
  rename("Baseline"="SCOPA_URI") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_URI)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
  ) %>% 
  mutate(Diff= (SCOPA_URI-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
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
# By Possible vs probable V2 URINARY --------------------

# ALL SCOPA Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_URI),
    sd=sd(SCOPA_URI),
    median=median(SCOPA_URI),
    Q1 = quantile(SCOPA_URI, 0.25),
    Q3 = quantile(SCOPA_URI, 0.75),
    min=min(SCOPA_URI),
    max=max(SCOPA_URI),
    n=n()
  )



# ALL SCOPA Total Year 1

AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_URI),
    sd=sd(SCOPA_URI),
    median=median(SCOPA_URI),
    Q1 = quantile(SCOPA_URI, 0.25),
    Q3 = quantile(SCOPA_URI, 0.75),
    min=min(SCOPA_URI),
    max=max(SCOPA_URI),
    n=n()
  )

# Change from baseline


AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
  rename("Baseline"="SCOPA_URI") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_URI)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((SCOPA_URI-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
  rename("Baseline"="SCOPA_URI") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_URI)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
  ) %>% 
  mutate(Diff= (SCOPA_URI-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
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
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
  rename("Baseline"="SCOPA_URI") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_URI)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
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
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
  rename("Baseline"="SCOPA_URI") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_URI)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
  ) %>% 
  mutate(Diff= (SCOPA_URI-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
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
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_URI),
    sd=sd(SCOPA_URI),
    median=median(SCOPA_URI),
    Q1 = quantile(SCOPA_URI, 0.25),
    Q3 = quantile(SCOPA_URI, 0.75),
    min=min(SCOPA_URI),
    max=max(SCOPA_URI),
    n=n()
  )


# Change from baseline

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
  rename("Baseline"="SCOPA_URI") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_URI)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((SCOPA_URI-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
  rename("Baseline"="SCOPA_URI") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_URI)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
  ) %>% 
  mutate(Diff= (SCOPA_URI-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
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
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
  rename("Baseline"="SCOPA_URI") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_URI)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
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
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
  rename("Baseline"="SCOPA_URI") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_URI)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
  ) %>% 
  mutate(Diff= (SCOPA_URI-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
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
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_URI),
    sd=sd(SCOPA_URI),
    median=median(SCOPA_URI),
    Q1 = quantile(SCOPA_URI, 0.25),
    Q3 = quantile(SCOPA_URI, 0.75),
    min=min(SCOPA_URI),
    max=max(SCOPA_URI),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
  rename("Baseline"="SCOPA_URI") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_URI)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
  ) %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(mean= mean((SCOPA_URI-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
  rename("Baseline"="SCOPA_URI") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_URI)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
  ) %>% 
  mutate(Diff= (SCOPA_URI-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
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
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
  rename("Baseline"="SCOPA_URI") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_URI)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
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
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
  rename("Baseline"="SCOPA_URI") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_URI)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
  ) %>% 
  mutate(Diff= (SCOPA_URI-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
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
# Overall Early CT V2 URINARY  -------------------------------

# ALL SCOPA Total Year 0 


EarlyCT_Pop_Baseline_319 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_URI),
    sd=sd(SCOPA_URI),
    median=median(SCOPA_URI),
    Q1 = quantile(SCOPA_URI, 0.25),
    Q3 = quantile(SCOPA_URI, 0.75),
    min=min(SCOPA_URI),
    max=max(SCOPA_URI),
    n=n()
  )



# ALL SCOPA Total Year 1

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_URI),
    sd=sd(SCOPA_URI),
    median=median(SCOPA_URI),
    Q1 = quantile(SCOPA_URI, 0.25),
    Q3 = quantile(SCOPA_URI, 0.75),
    min=min(SCOPA_URI),
    max=max(SCOPA_URI),
    n=n()
  )

# Change from baseline

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
  rename("Baseline"="SCOPA_URI") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_URI)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  summarise(mean= mean((SCOPA_URI-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
  rename("Baseline"="SCOPA_URI") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_URI)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
  ) %>% 
  mutate(Diff= (SCOPA_URI-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
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
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
  rename("Baseline"="SCOPA_URI") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_URI)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
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
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
  rename("Baseline"="SCOPA_URI") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_URI)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
  ) %>% 
  mutate(Diff= (SCOPA_URI-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
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
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_URI),
    sd=sd(SCOPA_URI),
    median=median(SCOPA_URI),
    Q1 = quantile(SCOPA_URI, 0.25),
    Q3 = quantile(SCOPA_URI, 0.75),
    min=min(SCOPA_URI),
    max=max(SCOPA_URI),
    n=n()
  )


# Change from baseline

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
  rename("Baseline"="SCOPA_URI") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_URI)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  summarise(mean= mean((SCOPA_URI-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
  rename("Baseline"="SCOPA_URI") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_URI)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
  ) %>% 
  mutate(Diff= (SCOPA_URI-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
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
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
  rename("Baseline"="SCOPA_URI") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_URI)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
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
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
  rename("Baseline"="SCOPA_URI") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_URI)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
  ) %>% 
  mutate(Diff= (SCOPA_URI-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
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
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_URI),
    sd=sd(SCOPA_URI),
    median=median(SCOPA_URI),
    Q1 = quantile(SCOPA_URI, 0.25),
    Q3 = quantile(SCOPA_URI, 0.75),
    min=min(SCOPA_URI),
    max=max(SCOPA_URI),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
  rename("Baseline"="SCOPA_URI") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_URI)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  summarise(mean= mean((SCOPA_URI-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
  rename("Baseline"="SCOPA_URI") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_URI)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
  ) %>% 
  mutate(Diff= (SCOPA_URI-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
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
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
  rename("Baseline"="SCOPA_URI") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_URI)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
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
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
  rename("Baseline"="SCOPA_URI") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_URI)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
  ) %>% 
  mutate(Diff= (SCOPA_URI-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
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


# By MSA P vs MSA C V2 URINARY -----------------
# ALL SCOPA Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_URI),
    sd=sd(SCOPA_URI),
    median=median(SCOPA_URI),
    Q1 = quantile(SCOPA_URI, 0.25),
    Q3 = quantile(SCOPA_URI, 0.75),
    min=min(SCOPA_URI),
    max=max(SCOPA_URI),
    n=n()
  )



# ALL SCOPA Total Year 1

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_URI),
    sd=sd(SCOPA_URI),
    median=median(SCOPA_URI),
    Q1 = quantile(SCOPA_URI, 0.25),
    Q3 = quantile(SCOPA_URI, 0.75),
    min=min(SCOPA_URI),
    max=max(SCOPA_URI),
    n=n()
  )

# Change from baseline

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
  rename("Baseline"="SCOPA_URI") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_URI)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((SCOPA_URI-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
  rename("Baseline"="SCOPA_URI") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_URI)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
  ) %>% 
  mutate(Diff= (SCOPA_URI-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>% 
  summarise(
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
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
  rename("Baseline"="SCOPA_URI") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_URI)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
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
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
  rename("Baseline"="SCOPA_URI") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_URI)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
  ) %>% 
  mutate(Diff= (SCOPA_URI-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
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
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_URI),
    sd=sd(SCOPA_URI),
    median=median(SCOPA_URI),
    Q1 = quantile(SCOPA_URI, 0.25),
    Q3 = quantile(SCOPA_URI, 0.75),
    min=min(SCOPA_URI),
    max=max(SCOPA_URI),
    n=n()
  )


# Change from baseline

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
  rename("Baseline"="SCOPA_URI") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_URI)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
  ) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((SCOPA_URI-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
  rename("Baseline"="SCOPA_URI") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_URI)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
  ) %>% 
  mutate(Diff= (SCOPA_URI-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
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
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
  rename("Baseline"="SCOPA_URI") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_URI)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
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
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
  rename("Baseline"="SCOPA_URI") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_URI)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
  ) %>% 
  mutate(Diff= (SCOPA_URI-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
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
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_URI),
    sd=sd(SCOPA_URI),
    median=median(SCOPA_URI),
    Q1 = quantile(SCOPA_URI, 0.25),
    Q3 = quantile(SCOPA_URI, 0.75),
    min=min(SCOPA_URI),
    max=max(SCOPA_URI),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
  rename("Baseline"="SCOPA_URI") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_URI)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
  ) %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(mean= mean((SCOPA_URI-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
  rename("Baseline"="SCOPA_URI") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_URI)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
  ) %>% 
  mutate(Diff= (SCOPA_URI-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
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
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
  rename("Baseline"="SCOPA_URI") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_URI)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
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
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
  rename("Baseline"="SCOPA_URI") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_URI)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
  ) %>% 
  mutate(Diff= (SCOPA_URI-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
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
# By Possible vs probable V2 URINARY --------------------

# ALL SCOPA Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_URI),
    sd=sd(SCOPA_URI),
    median=median(SCOPA_URI),
    Q1 = quantile(SCOPA_URI, 0.25),
    Q3 = quantile(SCOPA_URI, 0.75),
    min=min(SCOPA_URI),
    max=max(SCOPA_URI),
    n=n()
  )



# ALL SCOPA Total Year 1

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_URI),
    sd=sd(SCOPA_URI),
    median=median(SCOPA_URI),
    Q1 = quantile(SCOPA_URI, 0.25),
    Q3 = quantile(SCOPA_URI, 0.75),
    min=min(SCOPA_URI),
    max=max(SCOPA_URI),
    n=n()
  )

# Change from baseline


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
  rename("Baseline"="SCOPA_URI") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_URI)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((SCOPA_URI-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
  rename("Baseline"="SCOPA_URI") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_URI)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
  ) %>% 
  mutate(Diff= (SCOPA_URI-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
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
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
  rename("Baseline"="SCOPA_URI") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_URI)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
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
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
  rename("Baseline"="SCOPA_URI") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_URI)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
  ) %>% 
  mutate(Diff= (SCOPA_URI-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
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
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_URI),
    sd=sd(SCOPA_URI),
    median=median(SCOPA_URI),
    Q1 = quantile(SCOPA_URI, 0.25),
    Q3 = quantile(SCOPA_URI, 0.75),
    min=min(SCOPA_URI),
    max=max(SCOPA_URI),
    n=n()
  )


# Change from baseline

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
  rename("Baseline"="SCOPA_URI") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_URI)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((SCOPA_URI-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
  rename("Baseline"="SCOPA_URI") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_URI)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
  ) %>% 
  mutate(Diff= (SCOPA_URI-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
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
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
  rename("Baseline"="SCOPA_URI") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_URI)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
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
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
  rename("Baseline"="SCOPA_URI") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_URI)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
  ) %>% 
  mutate(Diff= (SCOPA_URI-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
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
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_URI),
    sd=sd(SCOPA_URI),
    median=median(SCOPA_URI),
    Q1 = quantile(SCOPA_URI, 0.25),
    Q3 = quantile(SCOPA_URI, 0.75),
    min=min(SCOPA_URI),
    max=max(SCOPA_URI),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
  rename("Baseline"="SCOPA_URI") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_URI)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
  ) %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(mean= mean((SCOPA_URI-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
  rename("Baseline"="SCOPA_URI") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_URI)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
  ) %>% 
  mutate(Diff= (SCOPA_URI-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% 
  summarise(
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
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
  rename("Baseline"="SCOPA_URI") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_URI)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
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
  filter(!is.na(SCOPA_URI)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
  rename("Baseline"="SCOPA_URI") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_URI)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
  ) %>% 
  mutate(Diff= (SCOPA_URI-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
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


# Inputs SCOPA AUT V2 CARDIO ----------------------------------


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
  select(NUM, DATECONSULT, TIME_STUDY, Year) %>% rename("DATE"="DATECONSULT")




ams_scopa_bx_2015_16 <- haven::read_sas("Source/ams_scopa_bx_2015_16.sas7bdat")
names(ams_scopa_bx_2015_16)
ams_scopa_bx_2015_16 <- ams_scopa_bx_2015_16 %>% filter(!is.na(DATE))

ams_scopa_bx_2015_16[is.na(ams_scopa_bx_2015_16)] <- 0

mean(ams_scopa_bx_2015_16$SCOPA_TOT, na.rm=T)

ams_scopa_bx_2015_16 <- ams_scopa_bx_2015_16 %>%
  mutate(SCOPA_DIG=SCOPA1+SCOPA2+SCOPA3+SCOPA4+SCOPA5+SCOPA6+SCOPA7) %>%
  mutate(SCOPA_URI=SCOPA8+SCOPA9+SCOPA10+SCOPA11+SCOPA12+SCOPA13) %>%
  mutate(SCOPA_CDV=SCOPA14+SCOPA15+SCOPA16) %>%
  mutate(SCOPA_THE=SCOPA17+SCOPA18+SCOPA19+SCOPA20) %>%
  mutate(SCOPA_PUP=SCOPA21) %>%
  mutate(SCOPA_SEX=SCOPA22H+SCOPA22F+SCOPA23F+SCOPA23H) %>%
  mutate(SCOPA_TOT=SCOPA_SEX+SCOPA_PUP+SCOPA_THE+SCOPA_CDV+SCOPA_CDV+SCOPA_DIG)

ams_scopa_bx_2015_16 <- ams_scopa_bx_2015_16 %>% 
  select(NUM, DATE, SCOPA_TOT, SCOPA_SEX, SCOPA_PUP, SCOPA_THE, SCOPA_URI, SCOPA_CDV, SCOPA_DIG)

length(unique(ams_scopa_bx_2015_16$NUM))


SCOPA <- ams_scopa_bx_2015_16 %>% inner_join(SCOPA) 

SCOPA <- SCOPA %>% filter(SCOPA_TOT!=0) 

length(unique(SCOPA$NUM))


SCOPA_Baseline_Pats <- SCOPA %>% filter(Year==0) %>% filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM) %>% distinct() %>%
  inner_join(AllMSA_Pop_Baseline_671)



# ----------------
# Overall MSA Entire V2 CARDIO -------------------------------

# ALL SCOPA Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(SCOPA) %>% filter(Year==0) %>% 
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_CDV),
    sd=sd(SCOPA_CDV),
    median=median(SCOPA_CDV),
    Q1 = quantile(SCOPA_CDV, 0.25),
    Q3 = quantile(SCOPA_CDV, 0.75),
    min=min(SCOPA_CDV),
    max=max(SCOPA_CDV),
    n=n()
  )



# ALL SCOPA Total Year 1

AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_CDV),
    sd=sd(SCOPA_CDV),
    median=median(SCOPA_CDV),
    Q1 = quantile(SCOPA_CDV, 0.25),
    Q3 = quantile(SCOPA_CDV, 0.75),
    min=min(SCOPA_CDV),
    max=max(SCOPA_CDV),
    n=n()
  )

# Change from baseline

AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
  rename("Baseline"="SCOPA_CDV") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_CDV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  summarise(mean= mean((SCOPA_CDV-Baseline)/TIME_STUDY)) 



AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
  rename("Baseline"="SCOPA_CDV") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_CDV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  mutate(Diff= (SCOPA_CDV-Baseline)) %>% 
  summarise(
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
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
  rename("Baseline"="SCOPA_CDV") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_CDV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
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
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
  rename("Baseline"="SCOPA_CDV") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_CDV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  mutate(Diff= (SCOPA_CDV-Baseline)/TIME_STUDY) %>%
  summarise(
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
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_CDV),
    sd=sd(SCOPA_CDV),
    median=median(SCOPA_CDV),
    Q1 = quantile(SCOPA_CDV, 0.25),
    Q3 = quantile(SCOPA_CDV, 0.75),
    min=min(SCOPA_CDV),
    max=max(SCOPA_CDV),
    n=n()
  )


# Change from baseline

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
  rename("Baseline"="SCOPA_CDV") %>% 
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_CDV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  summarise(mean= mean((SCOPA_CDV-Baseline)/TIME_STUDY)) 



AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
  rename("Baseline"="SCOPA_CDV") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_CDV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  mutate(Diff= (SCOPA_CDV-Baseline)) %>%
  summarise(
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
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
  rename("Baseline"="SCOPA_CDV") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_CDV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
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
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
  rename("Baseline"="SCOPA_CDV") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_CDV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
  ) %>% 
  mutate(Diff= (SCOPA_CDV-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
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
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_CDV),
    sd=sd(SCOPA_CDV),
    median=median(SCOPA_CDV),
    Q1 = quantile(SCOPA_CDV, 0.25),
    Q3 = quantile(SCOPA_CDV, 0.75),
    min=min(SCOPA_CDV),
    max=max(SCOPA_CDV),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
  rename("Baseline"="SCOPA_CDV") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_CDV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  summarise(mean= mean((SCOPA_CDV-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
  rename("Baseline"="SCOPA_CDV") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_CDV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
  ) %>%  
  mutate(Diff= (SCOPA_CDV-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
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
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
  rename("Baseline"="SCOPA_CDV") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_CDV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
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
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
  rename("Baseline"="SCOPA_CDV") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_CDV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
  ) %>% 
  mutate(Diff= (SCOPA_CDV-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
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


# By MSA P vs MSA C V2 CARDIO -----------------
# ALL SCOPA Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_CDV),
    sd=sd(SCOPA_CDV),
    median=median(SCOPA_CDV),
    Q1 = quantile(SCOPA_CDV, 0.25),
    Q3 = quantile(SCOPA_CDV, 0.75),
    min=min(SCOPA_CDV),
    max=max(SCOPA_CDV),
    n=n()
  )



# ALL SCOPA Total Year 1

AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_CDV),
    sd=sd(SCOPA_CDV),
    median=median(SCOPA_CDV),
    Q1 = quantile(SCOPA_CDV, 0.25),
    Q3 = quantile(SCOPA_CDV, 0.75),
    min=min(SCOPA_CDV),
    max=max(SCOPA_CDV),
    n=n()
  )

# Change from baseline

AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
  rename("Baseline"="SCOPA_CDV") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_CDV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((SCOPA_CDV-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
  rename("Baseline"="SCOPA_CDV") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_CDV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
  ) %>% 
  mutate(Diff= (SCOPA_CDV-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
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
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
  rename("Baseline"="SCOPA_CDV") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_CDV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
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
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
  rename("Baseline"="SCOPA_CDV") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_CDV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
  ) %>% 
  mutate(Diff= (SCOPA_CDV-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
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
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_CDV),
    sd=sd(SCOPA_CDV),
    median=median(SCOPA_CDV),
    Q1 = quantile(SCOPA_CDV, 0.25),
    Q3 = quantile(SCOPA_CDV, 0.75),
    min=min(SCOPA_CDV),
    max=max(SCOPA_CDV),
    n=n()
  )


# Change from baseline

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
  rename("Baseline"="SCOPA_CDV") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_CDV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((SCOPA_CDV-Baseline)/TIME_STUDY)) 



AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
  rename("Baseline"="SCOPA_CDV") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_CDV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
  ) %>% 
  mutate(Diff= (SCOPA_CDV-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
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
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
  rename("Baseline"="SCOPA_CDV") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_CDV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
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
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
  rename("Baseline"="SCOPA_CDV") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_CDV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
  ) %>% 
  mutate(Diff= (SCOPA_CDV-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
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
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_CDV),
    sd=sd(SCOPA_CDV),
    median=median(SCOPA_CDV),
    Q1 = quantile(SCOPA_CDV, 0.25),
    Q3 = quantile(SCOPA_CDV, 0.75),
    min=min(SCOPA_CDV),
    max=max(SCOPA_CDV),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
  rename("Baseline"="SCOPA_CDV") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_CDV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
  ) %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(mean= mean((SCOPA_CDV-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
  rename("Baseline"="SCOPA_CDV") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_CDV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  mutate(Diff= (SCOPA_CDV-Baseline)) %>%
  group_by(DIAG) %>%
  summarise(
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
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
  rename("Baseline"="SCOPA_CDV") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_CDV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
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
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
  rename("Baseline"="SCOPA_CDV") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_CDV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
  ) %>% 
  mutate(Diff= (SCOPA_CDV-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
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
# By Possible vs probable V2 CARDIO --------------------

# ALL SCOPA Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_CDV),
    sd=sd(SCOPA_CDV),
    median=median(SCOPA_CDV),
    Q1 = quantile(SCOPA_CDV, 0.25),
    Q3 = quantile(SCOPA_CDV, 0.75),
    min=min(SCOPA_CDV),
    max=max(SCOPA_CDV),
    n=n()
  )



# ALL SCOPA Total Year 1

AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_CDV),
    sd=sd(SCOPA_CDV),
    median=median(SCOPA_CDV),
    Q1 = quantile(SCOPA_CDV, 0.25),
    Q3 = quantile(SCOPA_CDV, 0.75),
    min=min(SCOPA_CDV),
    max=max(SCOPA_CDV),
    n=n()
  )

# Change from baseline


AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
  rename("Baseline"="SCOPA_CDV") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_CDV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((SCOPA_CDV-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
  rename("Baseline"="SCOPA_CDV") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_CDV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
  ) %>% 
  mutate(Diff= (SCOPA_CDV-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
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
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
  rename("Baseline"="SCOPA_CDV") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_CDV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
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
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
  rename("Baseline"="SCOPA_CDV") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_CDV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
  ) %>% 
  mutate(Diff= (SCOPA_CDV-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
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
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_CDV),
    sd=sd(SCOPA_CDV),
    median=median(SCOPA_CDV),
    Q1 = quantile(SCOPA_CDV, 0.25),
    Q3 = quantile(SCOPA_CDV, 0.75),
    min=min(SCOPA_CDV),
    max=max(SCOPA_CDV),
    n=n()
  )


# Change from baseline

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
  rename("Baseline"="SCOPA_CDV") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_CDV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((SCOPA_CDV-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
  rename("Baseline"="SCOPA_CDV") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_CDV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
  ) %>% 
  mutate(Diff= (SCOPA_CDV-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
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
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
  rename("Baseline"="SCOPA_CDV") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_CDV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
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
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
  rename("Baseline"="SCOPA_CDV") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_CDV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
  ) %>% 
  mutate(Diff= (SCOPA_CDV-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
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
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_CDV),
    sd=sd(SCOPA_CDV),
    median=median(SCOPA_CDV),
    Q1 = quantile(SCOPA_CDV, 0.25),
    Q3 = quantile(SCOPA_CDV, 0.75),
    min=min(SCOPA_CDV),
    max=max(SCOPA_CDV),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
  rename("Baseline"="SCOPA_CDV") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_CDV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
  ) %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(mean= mean((SCOPA_CDV-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
  rename("Baseline"="SCOPA_CDV") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_CDV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
  ) %>% 
  mutate(Diff= (SCOPA_CDV-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
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
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
  rename("Baseline"="SCOPA_CDV") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_CDV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
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
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
  rename("Baseline"="SCOPA_CDV") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_CDV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
  ) %>% 
  mutate(Diff= (SCOPA_CDV-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
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
# Overall Early CT V2 CARDIO  -------------------------------

# ALL SCOPA Total Year 0 


EarlyCT_Pop_Baseline_319 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_CDV),
    sd=sd(SCOPA_CDV),
    median=median(SCOPA_CDV),
    Q1 = quantile(SCOPA_CDV, 0.25),
    Q3 = quantile(SCOPA_CDV, 0.75),
    min=min(SCOPA_CDV),
    max=max(SCOPA_CDV),
    n=n()
  )



# ALL SCOPA Total Year 1

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_CDV),
    sd=sd(SCOPA_CDV),
    median=median(SCOPA_CDV),
    Q1 = quantile(SCOPA_CDV, 0.25),
    Q3 = quantile(SCOPA_CDV, 0.75),
    min=min(SCOPA_CDV),
    max=max(SCOPA_CDV),
    n=n()
  )

# Change from baseline

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
  rename("Baseline"="SCOPA_CDV") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_CDV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  summarise(mean= mean((SCOPA_CDV-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
  rename("Baseline"="SCOPA_CDV") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_CDV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
  ) %>% 
  mutate(Diff= (SCOPA_CDV-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
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
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
  rename("Baseline"="SCOPA_CDV") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_CDV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
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
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
  rename("Baseline"="SCOPA_CDV") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_CDV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
  ) %>% 
  mutate(Diff= (SCOPA_CDV-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
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
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_CDV),
    sd=sd(SCOPA_CDV),
    median=median(SCOPA_CDV),
    Q1 = quantile(SCOPA_CDV, 0.25),
    Q3 = quantile(SCOPA_CDV, 0.75),
    min=min(SCOPA_CDV),
    max=max(SCOPA_CDV),
    n=n()
  )


# Change from baseline

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
  rename("Baseline"="SCOPA_CDV") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_CDV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  summarise(mean= mean((SCOPA_CDV-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
  rename("Baseline"="SCOPA_CDV") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_CDV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
  ) %>% 
  mutate(Diff= (SCOPA_CDV-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
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
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
  rename("Baseline"="SCOPA_CDV") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_CDV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
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
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
  rename("Baseline"="SCOPA_CDV") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_CDV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
  ) %>% 
  mutate(Diff= (SCOPA_CDV-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
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
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_CDV),
    sd=sd(SCOPA_CDV),
    median=median(SCOPA_CDV),
    Q1 = quantile(SCOPA_CDV, 0.25),
    Q3 = quantile(SCOPA_CDV, 0.75),
    min=min(SCOPA_CDV),
    max=max(SCOPA_CDV),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
  rename("Baseline"="SCOPA_CDV") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_CDV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  summarise(mean= mean((SCOPA_CDV-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
  rename("Baseline"="SCOPA_CDV") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_CDV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
  ) %>% 
  mutate(Diff= (SCOPA_CDV-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
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
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
  rename("Baseline"="SCOPA_CDV") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_CDV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
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
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
  rename("Baseline"="SCOPA_CDV") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_CDV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
  ) %>% 
  mutate(Diff= (SCOPA_CDV-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
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


# By MSA P vs MSA C V2 CARDIO -----------------
# ALL SCOPA Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_CDV),
    sd=sd(SCOPA_CDV),
    median=median(SCOPA_CDV),
    Q1 = quantile(SCOPA_CDV, 0.25),
    Q3 = quantile(SCOPA_CDV, 0.75),
    min=min(SCOPA_CDV),
    max=max(SCOPA_CDV),
    n=n()
  )



# ALL SCOPA Total Year 1

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_CDV),
    sd=sd(SCOPA_CDV),
    median=median(SCOPA_CDV),
    Q1 = quantile(SCOPA_CDV, 0.25),
    Q3 = quantile(SCOPA_CDV, 0.75),
    min=min(SCOPA_CDV),
    max=max(SCOPA_CDV),
    n=n()
  )

# Change from baseline

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
  rename("Baseline"="SCOPA_CDV") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_CDV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((SCOPA_CDV-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
  rename("Baseline"="SCOPA_CDV") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_CDV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
  ) %>% 
  mutate(Diff= (SCOPA_CDV-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>% 
  summarise(
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
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
  rename("Baseline"="SCOPA_CDV") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_CDV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
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
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
  rename("Baseline"="SCOPA_CDV") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_CDV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
  ) %>% 
  mutate(Diff= (SCOPA_CDV-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
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
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_CDV),
    sd=sd(SCOPA_CDV),
    median=median(SCOPA_CDV),
    Q1 = quantile(SCOPA_CDV, 0.25),
    Q3 = quantile(SCOPA_CDV, 0.75),
    min=min(SCOPA_CDV),
    max=max(SCOPA_CDV),
    n=n()
  )


# Change from baseline

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
  rename("Baseline"="SCOPA_CDV") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_CDV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
  ) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((SCOPA_CDV-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
  rename("Baseline"="SCOPA_CDV") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_CDV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
  ) %>% 
  mutate(Diff= (SCOPA_CDV-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
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
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
  rename("Baseline"="SCOPA_CDV") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_CDV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
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
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
  rename("Baseline"="SCOPA_CDV") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_CDV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
  ) %>% 
  mutate(Diff= (SCOPA_CDV-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
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
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_CDV),
    sd=sd(SCOPA_CDV),
    median=median(SCOPA_CDV),
    Q1 = quantile(SCOPA_CDV, 0.25),
    Q3 = quantile(SCOPA_CDV, 0.75),
    min=min(SCOPA_CDV),
    max=max(SCOPA_CDV),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
  rename("Baseline"="SCOPA_CDV") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_CDV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
  ) %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(mean= mean((SCOPA_CDV-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
  rename("Baseline"="SCOPA_CDV") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_CDV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
  ) %>% 
  mutate(Diff= (SCOPA_CDV-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
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
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
  rename("Baseline"="SCOPA_CDV") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_CDV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
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
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
  rename("Baseline"="SCOPA_CDV") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_CDV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
  ) %>% 
  mutate(Diff= (SCOPA_CDV-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
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
# By Possible vs probable V2 CARDIO --------------------

# ALL SCOPA Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_CDV),
    sd=sd(SCOPA_CDV),
    median=median(SCOPA_CDV),
    Q1 = quantile(SCOPA_CDV, 0.25),
    Q3 = quantile(SCOPA_CDV, 0.75),
    min=min(SCOPA_CDV),
    max=max(SCOPA_CDV),
    n=n()
  )



# ALL SCOPA Total Year 1

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_CDV),
    sd=sd(SCOPA_CDV),
    median=median(SCOPA_CDV),
    Q1 = quantile(SCOPA_CDV, 0.25),
    Q3 = quantile(SCOPA_CDV, 0.75),
    min=min(SCOPA_CDV),
    max=max(SCOPA_CDV),
    n=n()
  )

# Change from baseline


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
  rename("Baseline"="SCOPA_CDV") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_CDV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((SCOPA_CDV-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
  rename("Baseline"="SCOPA_CDV") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_CDV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
  ) %>% 
  mutate(Diff= (SCOPA_CDV-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
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
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
  rename("Baseline"="SCOPA_CDV") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_CDV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
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
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
  rename("Baseline"="SCOPA_CDV") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_CDV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
  ) %>% 
  mutate(Diff= (SCOPA_CDV-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
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
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_CDV),
    sd=sd(SCOPA_CDV),
    median=median(SCOPA_CDV),
    Q1 = quantile(SCOPA_CDV, 0.25),
    Q3 = quantile(SCOPA_CDV, 0.75),
    min=min(SCOPA_CDV),
    max=max(SCOPA_CDV),
    n=n()
  )


# Change from baseline

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
  rename("Baseline"="SCOPA_CDV") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_CDV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((SCOPA_CDV-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
  rename("Baseline"="SCOPA_CDV") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_CDV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
  ) %>% 
  mutate(Diff= (SCOPA_CDV-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
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
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
  rename("Baseline"="SCOPA_CDV") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_CDV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
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
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
  rename("Baseline"="SCOPA_CDV") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_CDV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
  ) %>% 
  mutate(Diff= (SCOPA_CDV-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
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
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_CDV),
    sd=sd(SCOPA_CDV),
    median=median(SCOPA_CDV),
    Q1 = quantile(SCOPA_CDV, 0.25),
    Q3 = quantile(SCOPA_CDV, 0.75),
    min=min(SCOPA_CDV),
    max=max(SCOPA_CDV),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
  rename("Baseline"="SCOPA_CDV") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_CDV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
  ) %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(mean= mean((SCOPA_CDV-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
  rename("Baseline"="SCOPA_CDV") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_CDV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
  ) %>% 
  mutate(Diff= (SCOPA_CDV-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% 
  summarise(
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
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
  rename("Baseline"="SCOPA_CDV") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_CDV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
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
  filter(!is.na(SCOPA_CDV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
  rename("Baseline"="SCOPA_CDV") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_CDV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
  ) %>% 
  mutate(Diff= (SCOPA_CDV-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
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


# Inputs SCOPA AUT V2 THERMOREGULATORY  ----------------------------------


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
  select(NUM, DATECONSULT, TIME_STUDY, Year) %>% rename("DATE"="DATECONSULT")




ams_scopa_bx_2015_16 <- haven::read_sas("Source/ams_scopa_bx_2015_16.sas7bdat")
names(ams_scopa_bx_2015_16)
ams_scopa_bx_2015_16 <- ams_scopa_bx_2015_16 %>% filter(!is.na(DATE))

ams_scopa_bx_2015_16[is.na(ams_scopa_bx_2015_16)] <- 0

mean(ams_scopa_bx_2015_16$SCOPA_TOT, na.rm=T)

ams_scopa_bx_2015_16 <- ams_scopa_bx_2015_16 %>%
  mutate(SCOPA_DIG=SCOPA1+SCOPA2+SCOPA3+SCOPA4+SCOPA5+SCOPA6+SCOPA7) %>%
  mutate(SCOPA_URI=SCOPA8+SCOPA9+SCOPA10+SCOPA11+SCOPA12+SCOPA13) %>%
  mutate(SCOPA_CDV=SCOPA14+SCOPA15+SCOPA16) %>%
  mutate(SCOPA_THE=SCOPA17+SCOPA18+SCOPA19+SCOPA20) %>%
  mutate(SCOPA_PUP=SCOPA21) %>%
  mutate(SCOPA_SEX=SCOPA22H+SCOPA22F+SCOPA23F+SCOPA23H) %>%
  mutate(SCOPA_TOT=SCOPA_SEX+SCOPA_PUP+SCOPA_THE+SCOPA_THE+SCOPA_THE+SCOPA_DIG)

ams_scopa_bx_2015_16 <- ams_scopa_bx_2015_16 %>% 
  select(NUM, DATE, SCOPA_TOT, SCOPA_SEX, SCOPA_PUP, SCOPA_THE, SCOPA_URI, SCOPA_CDV, SCOPA_DIG)

length(unique(ams_scopa_bx_2015_16$NUM))


SCOPA <- ams_scopa_bx_2015_16 %>% inner_join(SCOPA) 

SCOPA <- SCOPA %>% filter(SCOPA_TOT!=0) 

length(unique(SCOPA$NUM))


SCOPA_Baseline_Pats <- SCOPA %>% filter(Year==0) %>% filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM) %>% distinct() %>%
  inner_join(AllMSA_Pop_Baseline_671)



# ----------------
# Overall MSA Entire V2 THERMOREGULATORY -------------------------------

# ALL SCOPA Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(SCOPA) %>% filter(Year==0) %>% 
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_THE),
    sd=sd(SCOPA_THE),
    median=median(SCOPA_THE),
    Q1 = quantile(SCOPA_THE, 0.25),
    Q3 = quantile(SCOPA_THE, 0.75),
    min=min(SCOPA_THE),
    max=max(SCOPA_THE),
    n=n()
  )



# ALL SCOPA Total Year 1

AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_THE),
    sd=sd(SCOPA_THE),
    median=median(SCOPA_THE),
    Q1 = quantile(SCOPA_THE, 0.25),
    Q3 = quantile(SCOPA_THE, 0.75),
    min=min(SCOPA_THE),
    max=max(SCOPA_THE),
    n=n()
  )

# Change from baseline

AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
  rename("Baseline"="SCOPA_THE") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_THE)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  summarise(mean= mean((SCOPA_THE-Baseline)/TIME_STUDY)) 



AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
  rename("Baseline"="SCOPA_THE") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_THE)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  mutate(Diff= (SCOPA_THE-Baseline)) %>% 
  summarise(
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
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
  rename("Baseline"="SCOPA_THE") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_THE)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
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
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
  rename("Baseline"="SCOPA_THE") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_THE)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  mutate(Diff= (SCOPA_THE-Baseline)/TIME_STUDY) %>%
  summarise(
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
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_THE),
    sd=sd(SCOPA_THE),
    median=median(SCOPA_THE),
    Q1 = quantile(SCOPA_THE, 0.25),
    Q3 = quantile(SCOPA_THE, 0.75),
    min=min(SCOPA_THE),
    max=max(SCOPA_THE),
    n=n()
  )


# Change from baseline

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
  rename("Baseline"="SCOPA_THE") %>% 
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_THE)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  summarise(mean= mean((SCOPA_THE-Baseline)/TIME_STUDY)) 



AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
  rename("Baseline"="SCOPA_THE") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_THE)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  mutate(Diff= (SCOPA_THE-Baseline)) %>%
  summarise(
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
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
  rename("Baseline"="SCOPA_THE") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_THE)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
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
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
  rename("Baseline"="SCOPA_THE") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_THE)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
  ) %>% 
  mutate(Diff= (SCOPA_THE-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
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
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_THE),
    sd=sd(SCOPA_THE),
    median=median(SCOPA_THE),
    Q1 = quantile(SCOPA_THE, 0.25),
    Q3 = quantile(SCOPA_THE, 0.75),
    min=min(SCOPA_THE),
    max=max(SCOPA_THE),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
  rename("Baseline"="SCOPA_THE") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_THE)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  summarise(mean= mean((SCOPA_THE-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
  rename("Baseline"="SCOPA_THE") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_THE)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
  ) %>%  
  mutate(Diff= (SCOPA_THE-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
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
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
  rename("Baseline"="SCOPA_THE") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_THE)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
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
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
  rename("Baseline"="SCOPA_THE") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_THE)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
  ) %>% 
  mutate(Diff= (SCOPA_THE-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
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


# By MSA P vs MSA C V2 THERMOREGULATORY -----------------
# ALL SCOPA Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_THE),
    sd=sd(SCOPA_THE),
    median=median(SCOPA_THE),
    Q1 = quantile(SCOPA_THE, 0.25),
    Q3 = quantile(SCOPA_THE, 0.75),
    min=min(SCOPA_THE),
    max=max(SCOPA_THE),
    n=n()
  )



# ALL SCOPA Total Year 1

AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_THE),
    sd=sd(SCOPA_THE),
    median=median(SCOPA_THE),
    Q1 = quantile(SCOPA_THE, 0.25),
    Q3 = quantile(SCOPA_THE, 0.75),
    min=min(SCOPA_THE),
    max=max(SCOPA_THE),
    n=n()
  )

# Change from baseline

AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
  rename("Baseline"="SCOPA_THE") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_THE)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((SCOPA_THE-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
  rename("Baseline"="SCOPA_THE") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_THE)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
  ) %>% 
  mutate(Diff= (SCOPA_THE-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
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
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
  rename("Baseline"="SCOPA_THE") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_THE)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
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
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
  rename("Baseline"="SCOPA_THE") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_THE)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
  ) %>% 
  mutate(Diff= (SCOPA_THE-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
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
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_THE),
    sd=sd(SCOPA_THE),
    median=median(SCOPA_THE),
    Q1 = quantile(SCOPA_THE, 0.25),
    Q3 = quantile(SCOPA_THE, 0.75),
    min=min(SCOPA_THE),
    max=max(SCOPA_THE),
    n=n()
  )


# Change from baseline

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
  rename("Baseline"="SCOPA_THE") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_THE)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((SCOPA_THE-Baseline)/TIME_STUDY)) 



AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
  rename("Baseline"="SCOPA_THE") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_THE)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
  ) %>% 
  mutate(Diff= (SCOPA_THE-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
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
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
  rename("Baseline"="SCOPA_THE") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_THE)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
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
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
  rename("Baseline"="SCOPA_THE") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_THE)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
  ) %>% 
  mutate(Diff= (SCOPA_THE-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
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
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_THE),
    sd=sd(SCOPA_THE),
    median=median(SCOPA_THE),
    Q1 = quantile(SCOPA_THE, 0.25),
    Q3 = quantile(SCOPA_THE, 0.75),
    min=min(SCOPA_THE),
    max=max(SCOPA_THE),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
  rename("Baseline"="SCOPA_THE") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_THE)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
  ) %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(mean= mean((SCOPA_THE-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
  rename("Baseline"="SCOPA_THE") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_THE)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  mutate(Diff= (SCOPA_THE-Baseline)) %>%
  group_by(DIAG) %>%
  summarise(
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
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
  rename("Baseline"="SCOPA_THE") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_THE)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
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
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
  rename("Baseline"="SCOPA_THE") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_THE)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
  ) %>% 
  mutate(Diff= (SCOPA_THE-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
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
# By Possible vs probable V2 THERMOREGULATORY --------------------

# ALL SCOPA Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_THE),
    sd=sd(SCOPA_THE),
    median=median(SCOPA_THE),
    Q1 = quantile(SCOPA_THE, 0.25),
    Q3 = quantile(SCOPA_THE, 0.75),
    min=min(SCOPA_THE),
    max=max(SCOPA_THE),
    n=n()
  )



# ALL SCOPA Total Year 1

AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_THE),
    sd=sd(SCOPA_THE),
    median=median(SCOPA_THE),
    Q1 = quantile(SCOPA_THE, 0.25),
    Q3 = quantile(SCOPA_THE, 0.75),
    min=min(SCOPA_THE),
    max=max(SCOPA_THE),
    n=n()
  )

# Change from baseline


AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
  rename("Baseline"="SCOPA_THE") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_THE)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((SCOPA_THE-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
  rename("Baseline"="SCOPA_THE") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_THE)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
  ) %>% 
  mutate(Diff= (SCOPA_THE-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
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
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
  rename("Baseline"="SCOPA_THE") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_THE)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
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
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
  rename("Baseline"="SCOPA_THE") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_THE)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
  ) %>% 
  mutate(Diff= (SCOPA_THE-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
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
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_THE),
    sd=sd(SCOPA_THE),
    median=median(SCOPA_THE),
    Q1 = quantile(SCOPA_THE, 0.25),
    Q3 = quantile(SCOPA_THE, 0.75),
    min=min(SCOPA_THE),
    max=max(SCOPA_THE),
    n=n()
  )


# Change from baseline

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
  rename("Baseline"="SCOPA_THE") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_THE)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((SCOPA_THE-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
  rename("Baseline"="SCOPA_THE") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_THE)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
  ) %>% 
  mutate(Diff= (SCOPA_THE-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
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
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
  rename("Baseline"="SCOPA_THE") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_THE)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
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
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
  rename("Baseline"="SCOPA_THE") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_THE)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
  ) %>% 
  mutate(Diff= (SCOPA_THE-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
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
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_THE),
    sd=sd(SCOPA_THE),
    median=median(SCOPA_THE),
    Q1 = quantile(SCOPA_THE, 0.25),
    Q3 = quantile(SCOPA_THE, 0.75),
    min=min(SCOPA_THE),
    max=max(SCOPA_THE),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
  rename("Baseline"="SCOPA_THE") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_THE)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
  ) %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(mean= mean((SCOPA_THE-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
  rename("Baseline"="SCOPA_THE") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_THE)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
  ) %>% 
  mutate(Diff= (SCOPA_THE-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
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
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
  rename("Baseline"="SCOPA_THE") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_THE)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
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
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
  rename("Baseline"="SCOPA_THE") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_THE)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
  ) %>% 
  mutate(Diff= (SCOPA_THE-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
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
# Overall Early CT V2 THERMOREGULATORY  -------------------------------

# ALL SCOPA Total Year 0 


EarlyCT_Pop_Baseline_319 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_THE),
    sd=sd(SCOPA_THE),
    median=median(SCOPA_THE),
    Q1 = quantile(SCOPA_THE, 0.25),
    Q3 = quantile(SCOPA_THE, 0.75),
    min=min(SCOPA_THE),
    max=max(SCOPA_THE),
    n=n()
  )



# ALL SCOPA Total Year 1

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_THE),
    sd=sd(SCOPA_THE),
    median=median(SCOPA_THE),
    Q1 = quantile(SCOPA_THE, 0.25),
    Q3 = quantile(SCOPA_THE, 0.75),
    min=min(SCOPA_THE),
    max=max(SCOPA_THE),
    n=n()
  )

# Change from baseline

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
  rename("Baseline"="SCOPA_THE") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_THE)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  summarise(mean= mean((SCOPA_THE-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
  rename("Baseline"="SCOPA_THE") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_THE)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
  ) %>% 
  mutate(Diff= (SCOPA_THE-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
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
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
  rename("Baseline"="SCOPA_THE") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_THE)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
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
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
  rename("Baseline"="SCOPA_THE") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_THE)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
  ) %>% 
  mutate(Diff= (SCOPA_THE-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
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
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_THE),
    sd=sd(SCOPA_THE),
    median=median(SCOPA_THE),
    Q1 = quantile(SCOPA_THE, 0.25),
    Q3 = quantile(SCOPA_THE, 0.75),
    min=min(SCOPA_THE),
    max=max(SCOPA_THE),
    n=n()
  )


# Change from baseline

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
  rename("Baseline"="SCOPA_THE") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_THE)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  summarise(mean= mean((SCOPA_THE-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
  rename("Baseline"="SCOPA_THE") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_THE)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
  ) %>% 
  mutate(Diff= (SCOPA_THE-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
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
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
  rename("Baseline"="SCOPA_THE") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_THE)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
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
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
  rename("Baseline"="SCOPA_THE") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_THE)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
  ) %>% 
  mutate(Diff= (SCOPA_THE-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
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
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_THE),
    sd=sd(SCOPA_THE),
    median=median(SCOPA_THE),
    Q1 = quantile(SCOPA_THE, 0.25),
    Q3 = quantile(SCOPA_THE, 0.75),
    min=min(SCOPA_THE),
    max=max(SCOPA_THE),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
  rename("Baseline"="SCOPA_THE") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_THE)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  summarise(mean= mean((SCOPA_THE-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
  rename("Baseline"="SCOPA_THE") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_THE)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
  ) %>% 
  mutate(Diff= (SCOPA_THE-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
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
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
  rename("Baseline"="SCOPA_THE") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_THE)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
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
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
  rename("Baseline"="SCOPA_THE") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_THE)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
  ) %>% 
  mutate(Diff= (SCOPA_THE-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
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


# By MSA P vs MSA C V2 THERMOREGULATORY -----------------
# ALL SCOPA Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_THE),
    sd=sd(SCOPA_THE),
    median=median(SCOPA_THE),
    Q1 = quantile(SCOPA_THE, 0.25),
    Q3 = quantile(SCOPA_THE, 0.75),
    min=min(SCOPA_THE),
    max=max(SCOPA_THE),
    n=n()
  )



# ALL SCOPA Total Year 1

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_THE),
    sd=sd(SCOPA_THE),
    median=median(SCOPA_THE),
    Q1 = quantile(SCOPA_THE, 0.25),
    Q3 = quantile(SCOPA_THE, 0.75),
    min=min(SCOPA_THE),
    max=max(SCOPA_THE),
    n=n()
  )

# Change from baseline

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
  rename("Baseline"="SCOPA_THE") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_THE)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((SCOPA_THE-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
  rename("Baseline"="SCOPA_THE") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_THE)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
  ) %>% 
  mutate(Diff= (SCOPA_THE-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>% 
  summarise(
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
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
  rename("Baseline"="SCOPA_THE") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_THE)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
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
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
  rename("Baseline"="SCOPA_THE") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_THE)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
  ) %>% 
  mutate(Diff= (SCOPA_THE-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
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
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_THE),
    sd=sd(SCOPA_THE),
    median=median(SCOPA_THE),
    Q1 = quantile(SCOPA_THE, 0.25),
    Q3 = quantile(SCOPA_THE, 0.75),
    min=min(SCOPA_THE),
    max=max(SCOPA_THE),
    n=n()
  )


# Change from baseline

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
  rename("Baseline"="SCOPA_THE") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_THE)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
  ) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((SCOPA_THE-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
  rename("Baseline"="SCOPA_THE") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_THE)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
  ) %>% 
  mutate(Diff= (SCOPA_THE-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
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
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
  rename("Baseline"="SCOPA_THE") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_THE)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
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
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
  rename("Baseline"="SCOPA_THE") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_THE)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
  ) %>% 
  mutate(Diff= (SCOPA_THE-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
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
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_THE),
    sd=sd(SCOPA_THE),
    median=median(SCOPA_THE),
    Q1 = quantile(SCOPA_THE, 0.25),
    Q3 = quantile(SCOPA_THE, 0.75),
    min=min(SCOPA_THE),
    max=max(SCOPA_THE),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
  rename("Baseline"="SCOPA_THE") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_THE)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
  ) %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(mean= mean((SCOPA_THE-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
  rename("Baseline"="SCOPA_THE") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_THE)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
  ) %>% 
  mutate(Diff= (SCOPA_THE-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
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
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
  rename("Baseline"="SCOPA_THE") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_THE)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
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
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
  rename("Baseline"="SCOPA_THE") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_THE)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
  ) %>% 
  mutate(Diff= (SCOPA_THE-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
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
# By Possible vs probable V2 THERMOREGULATORY --------------------

# ALL SCOPA Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_THE),
    sd=sd(SCOPA_THE),
    median=median(SCOPA_THE),
    Q1 = quantile(SCOPA_THE, 0.25),
    Q3 = quantile(SCOPA_THE, 0.75),
    min=min(SCOPA_THE),
    max=max(SCOPA_THE),
    n=n()
  )



# ALL SCOPA Total Year 1

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_THE),
    sd=sd(SCOPA_THE),
    median=median(SCOPA_THE),
    Q1 = quantile(SCOPA_THE, 0.25),
    Q3 = quantile(SCOPA_THE, 0.75),
    min=min(SCOPA_THE),
    max=max(SCOPA_THE),
    n=n()
  )

# Change from baseline


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
  rename("Baseline"="SCOPA_THE") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_THE)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((SCOPA_THE-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
  rename("Baseline"="SCOPA_THE") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_THE)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
  ) %>% 
  mutate(Diff= (SCOPA_THE-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
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
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
  rename("Baseline"="SCOPA_THE") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_THE)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
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
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
  rename("Baseline"="SCOPA_THE") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_THE)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
  ) %>% 
  mutate(Diff= (SCOPA_THE-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
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
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_THE),
    sd=sd(SCOPA_THE),
    median=median(SCOPA_THE),
    Q1 = quantile(SCOPA_THE, 0.25),
    Q3 = quantile(SCOPA_THE, 0.75),
    min=min(SCOPA_THE),
    max=max(SCOPA_THE),
    n=n()
  )


# Change from baseline

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
  rename("Baseline"="SCOPA_THE") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_THE)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((SCOPA_THE-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
  rename("Baseline"="SCOPA_THE") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_THE)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
  ) %>% 
  mutate(Diff= (SCOPA_THE-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
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
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
  rename("Baseline"="SCOPA_THE") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_THE)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
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
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
  rename("Baseline"="SCOPA_THE") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_THE)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
  ) %>% 
  mutate(Diff= (SCOPA_THE-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
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
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_THE),
    sd=sd(SCOPA_THE),
    median=median(SCOPA_THE),
    Q1 = quantile(SCOPA_THE, 0.25),
    Q3 = quantile(SCOPA_THE, 0.75),
    min=min(SCOPA_THE),
    max=max(SCOPA_THE),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
  rename("Baseline"="SCOPA_THE") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_THE)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
  ) %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(mean= mean((SCOPA_THE-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
  rename("Baseline"="SCOPA_THE") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_THE)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
  ) %>% 
  mutate(Diff= (SCOPA_THE-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% 
  summarise(
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
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
  rename("Baseline"="SCOPA_THE") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_THE)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
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
  filter(!is.na(SCOPA_THE)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
  rename("Baseline"="SCOPA_THE") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_THE)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
  ) %>% 
  mutate(Diff= (SCOPA_THE-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
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


# Inputs SCOPA AUT V2 Pupillomotor   ----------------------------------


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
  select(NUM, DATECONSULT, TIME_STUDY, Year) %>% rename("DATE"="DATECONSULT")




ams_scopa_bx_2015_16 <- haven::read_sas("Source/ams_scopa_bx_2015_16.sas7bdat")
names(ams_scopa_bx_2015_16)
ams_scopa_bx_2015_16 <- ams_scopa_bx_2015_16 %>% filter(!is.na(DATE))

ams_scopa_bx_2015_16[is.na(ams_scopa_bx_2015_16)] <- 0

mean(ams_scopa_bx_2015_16$SCOPA_TOT, na.rm=T)

ams_scopa_bx_2015_16 <- ams_scopa_bx_2015_16 %>%
  mutate(SCOPA_DIG=SCOPA1+SCOPA2+SCOPA3+SCOPA4+SCOPA5+SCOPA6+SCOPA7) %>%
  mutate(SCOPA_URI=SCOPA8+SCOPA9+SCOPA10+SCOPA11+SCOPA12+SCOPA13) %>%
  mutate(SCOPA_CDV=SCOPA14+SCOPA15+SCOPA16) %>%
  mutate(SCOPA_THE=SCOPA17+SCOPA18+SCOPA19+SCOPA20) %>%
  mutate(SCOPA_PUP=SCOPA21) %>%
  mutate(SCOPA_SEX=SCOPA22H+SCOPA22F+SCOPA23F+SCOPA23H) %>%
  mutate(SCOPA_TOT=SCOPA_SEX+SCOPA_PUP+SCOPA_PUP+SCOPA_PUP+SCOPA_PUP+SCOPA_DIG)

ams_scopa_bx_2015_16 <- ams_scopa_bx_2015_16 %>% 
  select(NUM, DATE, SCOPA_TOT, SCOPA_SEX, SCOPA_PUP, SCOPA_THE, SCOPA_URI, SCOPA_CDV, SCOPA_DIG)

length(unique(ams_scopa_bx_2015_16$NUM))


SCOPA <- ams_scopa_bx_2015_16 %>% inner_join(SCOPA) 

SCOPA <- SCOPA %>% filter(SCOPA_TOT!=0) 

length(unique(SCOPA$NUM))


SCOPA_Baseline_Pats <- SCOPA %>% filter(Year==0) %>% filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM) %>% distinct() %>%
  inner_join(AllMSA_Pop_Baseline_671)



# ----------------
# Overall MSA Entire V2	Pupillomotor  -------------------------------

# ALL SCOPA Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(SCOPA) %>% filter(Year==0) %>% 
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_PUP),
    sd=sd(SCOPA_PUP),
    median=median(SCOPA_PUP),
    Q1 = quantile(SCOPA_PUP, 0.25),
    Q3 = quantile(SCOPA_PUP, 0.75),
    min=min(SCOPA_PUP),
    max=max(SCOPA_PUP),
    n=n()
  )



# ALL SCOPA Total Year 1

AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_PUP),
    sd=sd(SCOPA_PUP),
    median=median(SCOPA_PUP),
    Q1 = quantile(SCOPA_PUP, 0.25),
    Q3 = quantile(SCOPA_PUP, 0.75),
    min=min(SCOPA_PUP),
    max=max(SCOPA_PUP),
    n=n()
  )

# Change from baseline

AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
  rename("Baseline"="SCOPA_PUP") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_PUP)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  summarise(mean= mean((SCOPA_PUP-Baseline)/TIME_STUDY)) 



AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
  rename("Baseline"="SCOPA_PUP") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_PUP)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  mutate(Diff= (SCOPA_PUP-Baseline)) %>% 
  summarise(
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
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
  rename("Baseline"="SCOPA_PUP") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_PUP)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
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
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
  rename("Baseline"="SCOPA_PUP") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_PUP)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  mutate(Diff= (SCOPA_PUP-Baseline)/TIME_STUDY) %>%
  summarise(
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
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_PUP),
    sd=sd(SCOPA_PUP),
    median=median(SCOPA_PUP),
    Q1 = quantile(SCOPA_PUP, 0.25),
    Q3 = quantile(SCOPA_PUP, 0.75),
    min=min(SCOPA_PUP),
    max=max(SCOPA_PUP),
    n=n()
  )


# Change from baseline

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
  rename("Baseline"="SCOPA_PUP") %>% 
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_PUP)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  summarise(mean= mean((SCOPA_PUP-Baseline)/TIME_STUDY)) 



AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
  rename("Baseline"="SCOPA_PUP") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_PUP)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  mutate(Diff= (SCOPA_PUP-Baseline)) %>%
  summarise(
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
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
  rename("Baseline"="SCOPA_PUP") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_PUP)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
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
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
  rename("Baseline"="SCOPA_PUP") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_PUP)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
  ) %>% 
  mutate(Diff= (SCOPA_PUP-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
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
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_PUP),
    sd=sd(SCOPA_PUP),
    median=median(SCOPA_PUP),
    Q1 = quantile(SCOPA_PUP, 0.25),
    Q3 = quantile(SCOPA_PUP, 0.75),
    min=min(SCOPA_PUP),
    max=max(SCOPA_PUP),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
  rename("Baseline"="SCOPA_PUP") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_PUP)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  summarise(mean= mean((SCOPA_PUP-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
  rename("Baseline"="SCOPA_PUP") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_PUP)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
  ) %>%  
  mutate(Diff= (SCOPA_PUP-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
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
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
  rename("Baseline"="SCOPA_PUP") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_PUP)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
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
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
  rename("Baseline"="SCOPA_PUP") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_PUP)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
  ) %>% 
  mutate(Diff= (SCOPA_PUP-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
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


# By MSA P vs MSA C V2 Pupillomotor -----------------
# ALL SCOPA Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_PUP),
    sd=sd(SCOPA_PUP),
    median=median(SCOPA_PUP),
    Q1 = quantile(SCOPA_PUP, 0.25),
    Q3 = quantile(SCOPA_PUP, 0.75),
    min=min(SCOPA_PUP),
    max=max(SCOPA_PUP),
    n=n()
  )



# ALL SCOPA Total Year 1

AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_PUP),
    sd=sd(SCOPA_PUP),
    median=median(SCOPA_PUP),
    Q1 = quantile(SCOPA_PUP, 0.25),
    Q3 = quantile(SCOPA_PUP, 0.75),
    min=min(SCOPA_PUP),
    max=max(SCOPA_PUP),
    n=n()
  )

# Change from baseline

AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
  rename("Baseline"="SCOPA_PUP") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_PUP)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((SCOPA_PUP-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
  rename("Baseline"="SCOPA_PUP") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_PUP)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
  ) %>% 
  mutate(Diff= (SCOPA_PUP-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
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
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
  rename("Baseline"="SCOPA_PUP") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_PUP)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
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
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
  rename("Baseline"="SCOPA_PUP") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_PUP)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
  ) %>% 
  mutate(Diff= (SCOPA_PUP-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
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
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_PUP),
    sd=sd(SCOPA_PUP),
    median=median(SCOPA_PUP),
    Q1 = quantile(SCOPA_PUP, 0.25),
    Q3 = quantile(SCOPA_PUP, 0.75),
    min=min(SCOPA_PUP),
    max=max(SCOPA_PUP),
    n=n()
  )


# Change from baseline

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
  rename("Baseline"="SCOPA_PUP") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_PUP)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((SCOPA_PUP-Baseline)/TIME_STUDY)) 



AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
  rename("Baseline"="SCOPA_PUP") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_PUP)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
  ) %>% 
  mutate(Diff= (SCOPA_PUP-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
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
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
  rename("Baseline"="SCOPA_PUP") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_PUP)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
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
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
  rename("Baseline"="SCOPA_PUP") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_PUP)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
  ) %>% 
  mutate(Diff= (SCOPA_PUP-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
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
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_PUP),
    sd=sd(SCOPA_PUP),
    median=median(SCOPA_PUP),
    Q1 = quantile(SCOPA_PUP, 0.25),
    Q3 = quantile(SCOPA_PUP, 0.75),
    min=min(SCOPA_PUP),
    max=max(SCOPA_PUP),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
  rename("Baseline"="SCOPA_PUP") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_PUP)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
  ) %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(mean= mean((SCOPA_PUP-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
  rename("Baseline"="SCOPA_PUP") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_PUP)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  mutate(Diff= (SCOPA_PUP-Baseline)) %>%
  group_by(DIAG) %>%
  summarise(
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
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
  rename("Baseline"="SCOPA_PUP") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_PUP)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
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
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
  rename("Baseline"="SCOPA_PUP") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_PUP)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
  ) %>% 
  mutate(Diff= (SCOPA_PUP-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
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
# By Possible vs probable V2 Pupillomotor --------------------

# ALL SCOPA Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_PUP),
    sd=sd(SCOPA_PUP),
    median=median(SCOPA_PUP),
    Q1 = quantile(SCOPA_PUP, 0.25),
    Q3 = quantile(SCOPA_PUP, 0.75),
    min=min(SCOPA_PUP),
    max=max(SCOPA_PUP),
    n=n()
  )



# ALL SCOPA Total Year 1

AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_PUP),
    sd=sd(SCOPA_PUP),
    median=median(SCOPA_PUP),
    Q1 = quantile(SCOPA_PUP, 0.25),
    Q3 = quantile(SCOPA_PUP, 0.75),
    min=min(SCOPA_PUP),
    max=max(SCOPA_PUP),
    n=n()
  )

# Change from baseline


AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
  rename("Baseline"="SCOPA_PUP") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_PUP)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((SCOPA_PUP-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
  rename("Baseline"="SCOPA_PUP") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_PUP)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
  ) %>% 
  mutate(Diff= (SCOPA_PUP-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
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
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
  rename("Baseline"="SCOPA_PUP") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_PUP)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
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
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
  rename("Baseline"="SCOPA_PUP") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_PUP)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
  ) %>% 
  mutate(Diff= (SCOPA_PUP-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
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
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_PUP),
    sd=sd(SCOPA_PUP),
    median=median(SCOPA_PUP),
    Q1 = quantile(SCOPA_PUP, 0.25),
    Q3 = quantile(SCOPA_PUP, 0.75),
    min=min(SCOPA_PUP),
    max=max(SCOPA_PUP),
    n=n()
  )


# Change from baseline

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
  rename("Baseline"="SCOPA_PUP") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_PUP)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((SCOPA_PUP-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
  rename("Baseline"="SCOPA_PUP") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_PUP)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
  ) %>% 
  mutate(Diff= (SCOPA_PUP-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
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
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
  rename("Baseline"="SCOPA_PUP") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_PUP)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
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
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
  rename("Baseline"="SCOPA_PUP") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_PUP)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
  ) %>% 
  mutate(Diff= (SCOPA_PUP-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
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
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_PUP),
    sd=sd(SCOPA_PUP),
    median=median(SCOPA_PUP),
    Q1 = quantile(SCOPA_PUP, 0.25),
    Q3 = quantile(SCOPA_PUP, 0.75),
    min=min(SCOPA_PUP),
    max=max(SCOPA_PUP),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
  rename("Baseline"="SCOPA_PUP") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_PUP)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
  ) %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(mean= mean((SCOPA_PUP-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
  rename("Baseline"="SCOPA_PUP") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_PUP)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
  ) %>% 
  mutate(Diff= (SCOPA_PUP-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
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
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
  rename("Baseline"="SCOPA_PUP") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_PUP)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
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
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
  rename("Baseline"="SCOPA_PUP") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_PUP)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
  ) %>% 
  mutate(Diff= (SCOPA_PUP-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
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
# Overall Early CT V2 Pupillomotor  -------------------------------

# ALL SCOPA Total Year 0 


EarlyCT_Pop_Baseline_319 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_PUP),
    sd=sd(SCOPA_PUP),
    median=median(SCOPA_PUP),
    Q1 = quantile(SCOPA_PUP, 0.25),
    Q3 = quantile(SCOPA_PUP, 0.75),
    min=min(SCOPA_PUP),
    max=max(SCOPA_PUP),
    n=n()
  )



# ALL SCOPA Total Year 1

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_PUP),
    sd=sd(SCOPA_PUP),
    median=median(SCOPA_PUP),
    Q1 = quantile(SCOPA_PUP, 0.25),
    Q3 = quantile(SCOPA_PUP, 0.75),
    min=min(SCOPA_PUP),
    max=max(SCOPA_PUP),
    n=n()
  )

# Change from baseline

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
  rename("Baseline"="SCOPA_PUP") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_PUP)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  summarise(mean= mean((SCOPA_PUP-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
  rename("Baseline"="SCOPA_PUP") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_PUP)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
  ) %>% 
  mutate(Diff= (SCOPA_PUP-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
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
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
  rename("Baseline"="SCOPA_PUP") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_PUP)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
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
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
  rename("Baseline"="SCOPA_PUP") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_PUP)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
  ) %>% 
  mutate(Diff= (SCOPA_PUP-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
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
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_PUP),
    sd=sd(SCOPA_PUP),
    median=median(SCOPA_PUP),
    Q1 = quantile(SCOPA_PUP, 0.25),
    Q3 = quantile(SCOPA_PUP, 0.75),
    min=min(SCOPA_PUP),
    max=max(SCOPA_PUP),
    n=n()
  )


# Change from baseline

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
  rename("Baseline"="SCOPA_PUP") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_PUP)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  summarise(mean= mean((SCOPA_PUP-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
  rename("Baseline"="SCOPA_PUP") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_PUP)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
  ) %>% 
  mutate(Diff= (SCOPA_PUP-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
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
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
  rename("Baseline"="SCOPA_PUP") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_PUP)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
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
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
  rename("Baseline"="SCOPA_PUP") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_PUP)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
  ) %>% 
  mutate(Diff= (SCOPA_PUP-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
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
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_PUP),
    sd=sd(SCOPA_PUP),
    median=median(SCOPA_PUP),
    Q1 = quantile(SCOPA_PUP, 0.25),
    Q3 = quantile(SCOPA_PUP, 0.75),
    min=min(SCOPA_PUP),
    max=max(SCOPA_PUP),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
  rename("Baseline"="SCOPA_PUP") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_PUP)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  summarise(mean= mean((SCOPA_PUP-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
  rename("Baseline"="SCOPA_PUP") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_PUP)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
  ) %>% 
  mutate(Diff= (SCOPA_PUP-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
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
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
  rename("Baseline"="SCOPA_PUP") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_PUP)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
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
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
  rename("Baseline"="SCOPA_PUP") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_PUP)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
  ) %>% 
  mutate(Diff= (SCOPA_PUP-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
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


# By MSA P vs MSA C V2 Pupillomotor -----------------
# ALL SCOPA Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_PUP),
    sd=sd(SCOPA_PUP),
    median=median(SCOPA_PUP),
    Q1 = quantile(SCOPA_PUP, 0.25),
    Q3 = quantile(SCOPA_PUP, 0.75),
    min=min(SCOPA_PUP),
    max=max(SCOPA_PUP),
    n=n()
  )



# ALL SCOPA Total Year 1

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_PUP),
    sd=sd(SCOPA_PUP),
    median=median(SCOPA_PUP),
    Q1 = quantile(SCOPA_PUP, 0.25),
    Q3 = quantile(SCOPA_PUP, 0.75),
    min=min(SCOPA_PUP),
    max=max(SCOPA_PUP),
    n=n()
  )

# Change from baseline

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
  rename("Baseline"="SCOPA_PUP") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_PUP)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((SCOPA_PUP-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
  rename("Baseline"="SCOPA_PUP") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_PUP)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
  ) %>% 
  mutate(Diff= (SCOPA_PUP-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>% 
  summarise(
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
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
  rename("Baseline"="SCOPA_PUP") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_PUP)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
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
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
  rename("Baseline"="SCOPA_PUP") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_PUP)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
  ) %>% 
  mutate(Diff= (SCOPA_PUP-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
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
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_PUP),
    sd=sd(SCOPA_PUP),
    median=median(SCOPA_PUP),
    Q1 = quantile(SCOPA_PUP, 0.25),
    Q3 = quantile(SCOPA_PUP, 0.75),
    min=min(SCOPA_PUP),
    max=max(SCOPA_PUP),
    n=n()
  )


# Change from baseline

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
  rename("Baseline"="SCOPA_PUP") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_PUP)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
  ) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((SCOPA_PUP-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
  rename("Baseline"="SCOPA_PUP") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_PUP)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
  ) %>% 
  mutate(Diff= (SCOPA_PUP-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
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
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
  rename("Baseline"="SCOPA_PUP") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_PUP)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
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
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
  rename("Baseline"="SCOPA_PUP") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_PUP)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
  ) %>% 
  mutate(Diff= (SCOPA_PUP-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
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
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_PUP),
    sd=sd(SCOPA_PUP),
    median=median(SCOPA_PUP),
    Q1 = quantile(SCOPA_PUP, 0.25),
    Q3 = quantile(SCOPA_PUP, 0.75),
    min=min(SCOPA_PUP),
    max=max(SCOPA_PUP),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
  rename("Baseline"="SCOPA_PUP") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_PUP)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
  ) %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(mean= mean((SCOPA_PUP-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
  rename("Baseline"="SCOPA_PUP") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_PUP)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
  ) %>% 
  mutate(Diff= (SCOPA_PUP-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
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
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
  rename("Baseline"="SCOPA_PUP") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_PUP)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
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
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
  rename("Baseline"="SCOPA_PUP") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_PUP)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
  ) %>% 
  mutate(Diff= (SCOPA_PUP-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
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
# By Possible vs probable V2 Pupillomotor --------------------

# ALL SCOPA Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_PUP),
    sd=sd(SCOPA_PUP),
    median=median(SCOPA_PUP),
    Q1 = quantile(SCOPA_PUP, 0.25),
    Q3 = quantile(SCOPA_PUP, 0.75),
    min=min(SCOPA_PUP),
    max=max(SCOPA_PUP),
    n=n()
  )



# ALL SCOPA Total Year 1

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_PUP),
    sd=sd(SCOPA_PUP),
    median=median(SCOPA_PUP),
    Q1 = quantile(SCOPA_PUP, 0.25),
    Q3 = quantile(SCOPA_PUP, 0.75),
    min=min(SCOPA_PUP),
    max=max(SCOPA_PUP),
    n=n()
  )

# Change from baseline


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
  rename("Baseline"="SCOPA_PUP") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_PUP)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((SCOPA_PUP-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
  rename("Baseline"="SCOPA_PUP") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_PUP)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
  ) %>% 
  mutate(Diff= (SCOPA_PUP-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
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
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
  rename("Baseline"="SCOPA_PUP") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_PUP)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
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
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
  rename("Baseline"="SCOPA_PUP") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_PUP)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
  ) %>% 
  mutate(Diff= (SCOPA_PUP-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
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
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_PUP),
    sd=sd(SCOPA_PUP),
    median=median(SCOPA_PUP),
    Q1 = quantile(SCOPA_PUP, 0.25),
    Q3 = quantile(SCOPA_PUP, 0.75),
    min=min(SCOPA_PUP),
    max=max(SCOPA_PUP),
    n=n()
  )


# Change from baseline

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
  rename("Baseline"="SCOPA_PUP") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_PUP)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((SCOPA_PUP-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
  rename("Baseline"="SCOPA_PUP") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_PUP)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
  ) %>% 
  mutate(Diff= (SCOPA_PUP-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
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
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
  rename("Baseline"="SCOPA_PUP") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_PUP)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
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
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
  rename("Baseline"="SCOPA_PUP") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_PUP)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
  ) %>% 
  mutate(Diff= (SCOPA_PUP-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
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
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_PUP),
    sd=sd(SCOPA_PUP),
    median=median(SCOPA_PUP),
    Q1 = quantile(SCOPA_PUP, 0.25),
    Q3 = quantile(SCOPA_PUP, 0.75),
    min=min(SCOPA_PUP),
    max=max(SCOPA_PUP),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
  rename("Baseline"="SCOPA_PUP") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_PUP)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
  ) %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(mean= mean((SCOPA_PUP-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
  rename("Baseline"="SCOPA_PUP") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_PUP)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
  ) %>% 
  mutate(Diff= (SCOPA_PUP-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% 
  summarise(
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
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
  rename("Baseline"="SCOPA_PUP") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_PUP)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
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
  filter(!is.na(SCOPA_PUP)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
  rename("Baseline"="SCOPA_PUP") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_PUP)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
  ) %>% 
  mutate(Diff= (SCOPA_PUP-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
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


# Inputs SCOPA AUT V2 Sexual   ----------------------------------


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
  select(NUM, DATECONSULT, TIME_STUDY, Year) %>% rename("DATE"="DATECONSULT")




ams_scopa_bx_2015_16 <- haven::read_sas("Source/ams_scopa_bx_2015_16.sas7bdat")
names(ams_scopa_bx_2015_16)
ams_scopa_bx_2015_16 <- ams_scopa_bx_2015_16 %>% filter(!is.na(DATE))

ams_scopa_bx_2015_16[is.na(ams_scopa_bx_2015_16)] <- 0

mean(ams_scopa_bx_2015_16$SCOPA_TOT, na.rm=T)

ams_scopa_bx_2015_16 <- ams_scopa_bx_2015_16 %>%
  mutate(SCOPA_DIG=SCOPA1+SCOPA2+SCOPA3+SCOPA4+SCOPA5+SCOPA6+SCOPA7) %>%
  mutate(SCOPA_URI=SCOPA8+SCOPA9+SCOPA10+SCOPA11+SCOPA12+SCOPA13) %>%
  mutate(SCOPA_CDV=SCOPA14+SCOPA15+SCOPA16) %>%
  mutate(SCOPA_THE=SCOPA17+SCOPA18+SCOPA19+SCOPA20) %>%
  mutate(SCOPA_SEX=SCOPA21) %>%
  mutate(SCOPA_PUP=SCOPA22H+SCOPA22F+SCOPA23F+SCOPA23H) %>%
  mutate(SCOPA_TOT=SCOPA_SEX+SCOPA_SEX+SCOPA_SEX+SCOPA_SEX+SCOPA_SEX+SCOPA_DIG)

ams_scopa_bx_2015_16 <- ams_scopa_bx_2015_16 %>% 
  select(NUM, DATE, SCOPA_TOT, SCOPA_PUP, SCOPA_SEX, SCOPA_THE, SCOPA_URI, SCOPA_CDV, SCOPA_DIG)

length(unique(ams_scopa_bx_2015_16$NUM))


SCOPA <- ams_scopa_bx_2015_16 %>% inner_join(SCOPA) 

SCOPA <- SCOPA %>% filter(SCOPA_TOT!=0) 

length(unique(SCOPA$NUM))


SCOPA_Baseline_Pats <- SCOPA %>% filter(Year==0) %>% filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM) %>% distinct() %>%
  inner_join(AllMSA_Pop_Baseline_671)



# ----------------
# Overall MSA Entire V2	Sexual  -------------------------------

# ALL SCOPA Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(SCOPA) %>% filter(Year==0) %>% 
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_SEX),
    sd=sd(SCOPA_SEX),
    median=median(SCOPA_SEX),
    Q1 = quantile(SCOPA_SEX, 0.25),
    Q3 = quantile(SCOPA_SEX, 0.75),
    min=min(SCOPA_SEX),
    max=max(SCOPA_SEX),
    n=n()
  )



# ALL SCOPA Total Year 1

AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_SEX),
    sd=sd(SCOPA_SEX),
    median=median(SCOPA_SEX),
    Q1 = quantile(SCOPA_SEX, 0.25),
    Q3 = quantile(SCOPA_SEX, 0.75),
    min=min(SCOPA_SEX),
    max=max(SCOPA_SEX),
    n=n()
  )

# Change from baseline

AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
  rename("Baseline"="SCOPA_SEX") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_SEX)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  summarise(mean= mean((SCOPA_SEX-Baseline)/TIME_STUDY)) 



AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
  rename("Baseline"="SCOPA_SEX") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_SEX)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  mutate(Diff= (SCOPA_SEX-Baseline)) %>% 
  summarise(
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
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
  rename("Baseline"="SCOPA_SEX") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_SEX)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
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
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
  rename("Baseline"="SCOPA_SEX") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_SEX)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  mutate(Diff= (SCOPA_SEX-Baseline)/TIME_STUDY) %>%
  summarise(
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
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_SEX),
    sd=sd(SCOPA_SEX),
    median=median(SCOPA_SEX),
    Q1 = quantile(SCOPA_SEX, 0.25),
    Q3 = quantile(SCOPA_SEX, 0.75),
    min=min(SCOPA_SEX),
    max=max(SCOPA_SEX),
    n=n()
  )


# Change from baseline

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
  rename("Baseline"="SCOPA_SEX") %>% 
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_SEX)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  summarise(mean= mean((SCOPA_SEX-Baseline)/TIME_STUDY)) 



AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
  rename("Baseline"="SCOPA_SEX") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_SEX)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  mutate(Diff= (SCOPA_SEX-Baseline)) %>%
  summarise(
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
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
  rename("Baseline"="SCOPA_SEX") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_SEX)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
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
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
  rename("Baseline"="SCOPA_SEX") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_SEX)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
  ) %>% 
  mutate(Diff= (SCOPA_SEX-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
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
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_SEX),
    sd=sd(SCOPA_SEX),
    median=median(SCOPA_SEX),
    Q1 = quantile(SCOPA_SEX, 0.25),
    Q3 = quantile(SCOPA_SEX, 0.75),
    min=min(SCOPA_SEX),
    max=max(SCOPA_SEX),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
  rename("Baseline"="SCOPA_SEX") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_SEX)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  summarise(mean= mean((SCOPA_SEX-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
  rename("Baseline"="SCOPA_SEX") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_SEX)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
  ) %>%  
  mutate(Diff= (SCOPA_SEX-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
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
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
  rename("Baseline"="SCOPA_SEX") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_SEX)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
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
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
  rename("Baseline"="SCOPA_SEX") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_SEX)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
  ) %>% 
  mutate(Diff= (SCOPA_SEX-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
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


# By MSA P vs MSA C V2 Sexual -----------------
# ALL SCOPA Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_SEX),
    sd=sd(SCOPA_SEX),
    median=median(SCOPA_SEX),
    Q1 = quantile(SCOPA_SEX, 0.25),
    Q3 = quantile(SCOPA_SEX, 0.75),
    min=min(SCOPA_SEX),
    max=max(SCOPA_SEX),
    n=n()
  )



# ALL SCOPA Total Year 1

AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_SEX),
    sd=sd(SCOPA_SEX),
    median=median(SCOPA_SEX),
    Q1 = quantile(SCOPA_SEX, 0.25),
    Q3 = quantile(SCOPA_SEX, 0.75),
    min=min(SCOPA_SEX),
    max=max(SCOPA_SEX),
    n=n()
  )

# Change from baseline

AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
  rename("Baseline"="SCOPA_SEX") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_SEX)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((SCOPA_SEX-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
  rename("Baseline"="SCOPA_SEX") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_SEX)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
  ) %>% 
  mutate(Diff= (SCOPA_SEX-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
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
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
  rename("Baseline"="SCOPA_SEX") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_SEX)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
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
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
  rename("Baseline"="SCOPA_SEX") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_SEX)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
  ) %>% 
  mutate(Diff= (SCOPA_SEX-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
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
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_SEX),
    sd=sd(SCOPA_SEX),
    median=median(SCOPA_SEX),
    Q1 = quantile(SCOPA_SEX, 0.25),
    Q3 = quantile(SCOPA_SEX, 0.75),
    min=min(SCOPA_SEX),
    max=max(SCOPA_SEX),
    n=n()
  )


# Change from baseline

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
  rename("Baseline"="SCOPA_SEX") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_SEX)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((SCOPA_SEX-Baseline)/TIME_STUDY)) 



AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
  rename("Baseline"="SCOPA_SEX") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_SEX)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
  ) %>% 
  mutate(Diff= (SCOPA_SEX-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
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
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
  rename("Baseline"="SCOPA_SEX") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_SEX)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
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
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
  rename("Baseline"="SCOPA_SEX") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_SEX)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
  ) %>% 
  mutate(Diff= (SCOPA_SEX-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
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
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_SEX),
    sd=sd(SCOPA_SEX),
    median=median(SCOPA_SEX),
    Q1 = quantile(SCOPA_SEX, 0.25),
    Q3 = quantile(SCOPA_SEX, 0.75),
    min=min(SCOPA_SEX),
    max=max(SCOPA_SEX),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
  rename("Baseline"="SCOPA_SEX") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_SEX)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
  ) %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(mean= mean((SCOPA_SEX-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
  rename("Baseline"="SCOPA_SEX") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_SEX)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  mutate(Diff= (SCOPA_SEX-Baseline)) %>%
  group_by(DIAG) %>%
  summarise(
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
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
  rename("Baseline"="SCOPA_SEX") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_SEX)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
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
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
  rename("Baseline"="SCOPA_SEX") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_SEX)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
  ) %>% 
  mutate(Diff= (SCOPA_SEX-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
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
# By Possible vs probable V2 Sexual --------------------

# ALL SCOPA Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_SEX),
    sd=sd(SCOPA_SEX),
    median=median(SCOPA_SEX),
    Q1 = quantile(SCOPA_SEX, 0.25),
    Q3 = quantile(SCOPA_SEX, 0.75),
    min=min(SCOPA_SEX),
    max=max(SCOPA_SEX),
    n=n()
  )



# ALL SCOPA Total Year 1

AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_SEX),
    sd=sd(SCOPA_SEX),
    median=median(SCOPA_SEX),
    Q1 = quantile(SCOPA_SEX, 0.25),
    Q3 = quantile(SCOPA_SEX, 0.75),
    min=min(SCOPA_SEX),
    max=max(SCOPA_SEX),
    n=n()
  )

# Change from baseline


AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
  rename("Baseline"="SCOPA_SEX") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_SEX)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((SCOPA_SEX-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
  rename("Baseline"="SCOPA_SEX") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_SEX)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
  ) %>% 
  mutate(Diff= (SCOPA_SEX-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
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
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
  rename("Baseline"="SCOPA_SEX") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_SEX)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
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
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
  rename("Baseline"="SCOPA_SEX") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_SEX)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
  ) %>% 
  mutate(Diff= (SCOPA_SEX-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
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
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_SEX),
    sd=sd(SCOPA_SEX),
    median=median(SCOPA_SEX),
    Q1 = quantile(SCOPA_SEX, 0.25),
    Q3 = quantile(SCOPA_SEX, 0.75),
    min=min(SCOPA_SEX),
    max=max(SCOPA_SEX),
    n=n()
  )


# Change from baseline

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
  rename("Baseline"="SCOPA_SEX") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_SEX)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((SCOPA_SEX-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
  rename("Baseline"="SCOPA_SEX") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_SEX)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
  ) %>% 
  mutate(Diff= (SCOPA_SEX-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
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
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
  rename("Baseline"="SCOPA_SEX") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_SEX)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
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
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
  rename("Baseline"="SCOPA_SEX") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_SEX)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
  ) %>% 
  mutate(Diff= (SCOPA_SEX-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
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
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_SEX),
    sd=sd(SCOPA_SEX),
    median=median(SCOPA_SEX),
    Q1 = quantile(SCOPA_SEX, 0.25),
    Q3 = quantile(SCOPA_SEX, 0.75),
    min=min(SCOPA_SEX),
    max=max(SCOPA_SEX),
    n=n()
  )


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
  rename("Baseline"="SCOPA_SEX") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_SEX)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
  ) %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(mean= mean((SCOPA_SEX-Baseline)/TIME_STUDY)) 


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
  rename("Baseline"="SCOPA_SEX") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_SEX)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
  ) %>% 
  mutate(Diff= (SCOPA_SEX-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
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
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
  rename("Baseline"="SCOPA_SEX") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_SEX)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
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
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
  rename("Baseline"="SCOPA_SEX") %>%
  inner_join(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_SEX)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
  ) %>% 
  mutate(Diff= (SCOPA_SEX-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
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
# Overall Early CT V2 Sexual  -------------------------------

# ALL SCOPA Total Year 0 


EarlyCT_Pop_Baseline_319 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_SEX),
    sd=sd(SCOPA_SEX),
    median=median(SCOPA_SEX),
    Q1 = quantile(SCOPA_SEX, 0.25),
    Q3 = quantile(SCOPA_SEX, 0.75),
    min=min(SCOPA_SEX),
    max=max(SCOPA_SEX),
    n=n()
  )



# ALL SCOPA Total Year 1

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_SEX),
    sd=sd(SCOPA_SEX),
    median=median(SCOPA_SEX),
    Q1 = quantile(SCOPA_SEX, 0.25),
    Q3 = quantile(SCOPA_SEX, 0.75),
    min=min(SCOPA_SEX),
    max=max(SCOPA_SEX),
    n=n()
  )

# Change from baseline

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
  rename("Baseline"="SCOPA_SEX") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_SEX)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  summarise(mean= mean((SCOPA_SEX-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
  rename("Baseline"="SCOPA_SEX") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_SEX)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
  ) %>% 
  mutate(Diff= (SCOPA_SEX-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
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
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
  rename("Baseline"="SCOPA_SEX") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_SEX)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
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
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
  rename("Baseline"="SCOPA_SEX") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_SEX)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
  ) %>% 
  mutate(Diff= (SCOPA_SEX-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
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
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_SEX),
    sd=sd(SCOPA_SEX),
    median=median(SCOPA_SEX),
    Q1 = quantile(SCOPA_SEX, 0.25),
    Q3 = quantile(SCOPA_SEX, 0.75),
    min=min(SCOPA_SEX),
    max=max(SCOPA_SEX),
    n=n()
  )


# Change from baseline

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
  rename("Baseline"="SCOPA_SEX") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_SEX)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  summarise(mean= mean((SCOPA_SEX-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
  rename("Baseline"="SCOPA_SEX") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_SEX)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
  ) %>% 
  mutate(Diff= (SCOPA_SEX-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
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
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
  rename("Baseline"="SCOPA_SEX") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_SEX)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
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
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
  rename("Baseline"="SCOPA_SEX") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_SEX)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
  ) %>% 
  mutate(Diff= (SCOPA_SEX-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
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
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_SEX),
    sd=sd(SCOPA_SEX),
    median=median(SCOPA_SEX),
    Q1 = quantile(SCOPA_SEX, 0.25),
    Q3 = quantile(SCOPA_SEX, 0.75),
    min=min(SCOPA_SEX),
    max=max(SCOPA_SEX),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
  rename("Baseline"="SCOPA_SEX") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_SEX)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  summarise(mean= mean((SCOPA_SEX-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
  rename("Baseline"="SCOPA_SEX") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_SEX)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
  ) %>% 
  mutate(Diff= (SCOPA_SEX-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
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
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
  rename("Baseline"="SCOPA_SEX") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_SEX)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
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
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
  rename("Baseline"="SCOPA_SEX") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_SEX)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
  ) %>% 
  mutate(Diff= (SCOPA_SEX-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
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


# By MSA P vs MSA C V2 Sexual -----------------
# ALL SCOPA Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_SEX),
    sd=sd(SCOPA_SEX),
    median=median(SCOPA_SEX),
    Q1 = quantile(SCOPA_SEX, 0.25),
    Q3 = quantile(SCOPA_SEX, 0.75),
    min=min(SCOPA_SEX),
    max=max(SCOPA_SEX),
    n=n()
  )



# ALL SCOPA Total Year 1

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_SEX),
    sd=sd(SCOPA_SEX),
    median=median(SCOPA_SEX),
    Q1 = quantile(SCOPA_SEX, 0.25),
    Q3 = quantile(SCOPA_SEX, 0.75),
    min=min(SCOPA_SEX),
    max=max(SCOPA_SEX),
    n=n()
  )

# Change from baseline

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
  rename("Baseline"="SCOPA_SEX") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_SEX)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((SCOPA_SEX-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
  rename("Baseline"="SCOPA_SEX") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_SEX)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
  ) %>% 
  mutate(Diff= (SCOPA_SEX-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>% 
  summarise(
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
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
  rename("Baseline"="SCOPA_SEX") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_SEX)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
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
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
  rename("Baseline"="SCOPA_SEX") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_SEX)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
  ) %>% 
  mutate(Diff= (SCOPA_SEX-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
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
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_SEX),
    sd=sd(SCOPA_SEX),
    median=median(SCOPA_SEX),
    Q1 = quantile(SCOPA_SEX, 0.25),
    Q3 = quantile(SCOPA_SEX, 0.75),
    min=min(SCOPA_SEX),
    max=max(SCOPA_SEX),
    n=n()
  )


# Change from baseline

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
  rename("Baseline"="SCOPA_SEX") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_SEX)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
  ) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((SCOPA_SEX-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
  rename("Baseline"="SCOPA_SEX") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_SEX)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
  ) %>% 
  mutate(Diff= (SCOPA_SEX-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
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
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
  rename("Baseline"="SCOPA_SEX") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_SEX)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
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
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
  rename("Baseline"="SCOPA_SEX") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_SEX)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
  ) %>% 
  mutate(Diff= (SCOPA_SEX-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
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
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_SEX),
    sd=sd(SCOPA_SEX),
    median=median(SCOPA_SEX),
    Q1 = quantile(SCOPA_SEX, 0.25),
    Q3 = quantile(SCOPA_SEX, 0.75),
    min=min(SCOPA_SEX),
    max=max(SCOPA_SEX),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
  rename("Baseline"="SCOPA_SEX") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_SEX)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
  ) %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(mean= mean((SCOPA_SEX-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
  rename("Baseline"="SCOPA_SEX") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_SEX)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
  ) %>% 
  mutate(Diff= (SCOPA_SEX-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
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
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
  rename("Baseline"="SCOPA_SEX") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_SEX)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
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
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
  rename("Baseline"="SCOPA_SEX") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_SEX)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
  ) %>% 
  mutate(Diff= (SCOPA_SEX-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )




# ---------
# By Possible vs probable V2 Sexual --------------------

# ALL SCOPA Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_SEX),
    sd=sd(SCOPA_SEX),
    median=median(SCOPA_SEX),
    Q1 = quantile(SCOPA_SEX, 0.25),
    Q3 = quantile(SCOPA_SEX, 0.75),
    min=min(SCOPA_SEX),
    max=max(SCOPA_SEX),
    n=n()
  )



# ALL SCOPA Total Year 1

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_SEX),
    sd=sd(SCOPA_SEX),
    median=median(SCOPA_SEX),
    Q1 = quantile(SCOPA_SEX, 0.25),
    Q3 = quantile(SCOPA_SEX, 0.75),
    min=min(SCOPA_SEX),
    max=max(SCOPA_SEX),
    n=n()
  )

# Change from baseline


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
  rename("Baseline"="SCOPA_SEX") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_SEX)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((SCOPA_SEX-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
  rename("Baseline"="SCOPA_SEX") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_SEX)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
  ) %>% 
  mutate(Diff= (SCOPA_SEX-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
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
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
  rename("Baseline"="SCOPA_SEX") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_SEX)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
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
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
  rename("Baseline"="SCOPA_SEX") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
      filter(!is.na(SCOPA_SEX)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
  ) %>% 
  mutate(Diff= (SCOPA_SEX-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
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
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_SEX),
    sd=sd(SCOPA_SEX),
    median=median(SCOPA_SEX),
    Q1 = quantile(SCOPA_SEX, 0.25),
    Q3 = quantile(SCOPA_SEX, 0.75),
    min=min(SCOPA_SEX),
    max=max(SCOPA_SEX),
    n=n()
  )


# Change from baseline

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
  rename("Baseline"="SCOPA_SEX") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_SEX)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
  ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((SCOPA_SEX-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
  rename("Baseline"="SCOPA_SEX") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_SEX)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
  ) %>% 
  mutate(Diff= (SCOPA_SEX-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
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
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
  rename("Baseline"="SCOPA_SEX") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_SEX)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
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
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
  rename("Baseline"="SCOPA_SEX") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
      filter(!is.na(SCOPA_SEX)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
  ) %>% 
  mutate(Diff= (SCOPA_SEX-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
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
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(
    mean=mean(SCOPA_SEX),
    sd=sd(SCOPA_SEX),
    median=median(SCOPA_SEX),
    Q1 = quantile(SCOPA_SEX, 0.25),
    Q3 = quantile(SCOPA_SEX, 0.75),
    min=min(SCOPA_SEX),
    max=max(SCOPA_SEX),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
  rename("Baseline"="SCOPA_SEX") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_SEX)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
  ) %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
  summarise(mean= mean((SCOPA_SEX-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
  rename("Baseline"="SCOPA_SEX") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_SEX)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
  ) %>% 
  mutate(Diff= (SCOPA_SEX-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% 
  summarise(
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
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
  rename("Baseline"="SCOPA_SEX") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_SEX)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
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
  filter(!is.na(SCOPA_SEX)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
  rename("Baseline"="SCOPA_SEX") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
      filter(!is.na(SCOPA_SEX)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
  ) %>% 
  mutate(Diff= (SCOPA_SEX-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
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

