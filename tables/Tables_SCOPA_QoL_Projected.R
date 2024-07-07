

library(tidyverse)
library(data.table)
options(scipen = 999)



# Inputs SCHARG   ----------------------------------


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

# ----------
# Overall Early CT  -------------------------------

# ALL SCHRAG Total Year 0 


EarlyCT_Pop_Baseline_319 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_TOT_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(SCHRAG_TOT_v2),
    sd=sd(SCHRAG_TOT_v2),
    median=median(SCHRAG_TOT_v2),
    Q1 = quantile(SCHRAG_TOT_v2, 0.25),
    Q3 = quantile(SCHRAG_TOT_v2, 0.75),
    min=min(SCHRAG_TOT_v2),
    max=max(SCHRAG_TOT_v2),
    n=n()
  )



# ALL SCHRAG Total Year 1

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
  filter(!is.na(SCHRAG_TOT_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(SCHRAG_TOT_v2),
    sd=sd(SCHRAG_TOT_v2),
    median=median(SCHRAG_TOT_v2),
    Q1 = quantile(SCHRAG_TOT_v2, 0.25),
    Q3 = quantile(SCHRAG_TOT_v2, 0.75),
    min=min(SCHRAG_TOT_v2),
    max=max(SCHRAG_TOT_v2),
    n=n()
  )

# Change from baseline

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_TOT_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_TOT_v2) %>%
  rename("Baseline"="SCHRAG_TOT_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
      filter(!is.na(SCHRAG_TOT_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_TOT_v2)
  ) %>%  inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(mean= mean((SCHRAG_TOT_v2-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_TOT_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_TOT_v2) %>%
  rename("Baseline"="SCHRAG_TOT_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
      filter(!is.na(SCHRAG_TOT_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_TOT_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_TOT_v2-Baseline)) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_TOT_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_TOT_v2) %>%
  rename("Baseline"="SCHRAG_TOT_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
      filter(!is.na(SCHRAG_TOT_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_TOT_v2)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_TOT_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_TOT_v2) %>%
  rename("Baseline"="SCHRAG_TOT_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
      filter(!is.na(SCHRAG_TOT_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_TOT_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_TOT_v2-Baseline)/TIME_STUDY) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



# ALL SCHRAG Total Year 2

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==2) %>%
  filter(!is.na(SCHRAG_TOT_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(SCHRAG_TOT_v2),
    sd=sd(SCHRAG_TOT_v2),
    median=median(SCHRAG_TOT_v2),
    Q1 = quantile(SCHRAG_TOT_v2, 0.25),
    Q3 = quantile(SCHRAG_TOT_v2, 0.75),
    min=min(SCHRAG_TOT_v2),
    max=max(SCHRAG_TOT_v2),
    n=n()
  )


# Change from baseline

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_TOT_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_TOT_v2) %>%
  rename("Baseline"="SCHRAG_TOT_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==2) %>%
      filter(!is.na(SCHRAG_TOT_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_TOT_v2)
  ) %>%  inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(mean= mean((SCHRAG_TOT_v2-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_TOT_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_TOT_v2) %>%
  rename("Baseline"="SCHRAG_TOT_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==2) %>%
      filter(!is.na(SCHRAG_TOT_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_TOT_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_TOT_v2-Baseline)) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_TOT_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_TOT_v2) %>%
  rename("Baseline"="SCHRAG_TOT_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==2) %>%
      filter(!is.na(SCHRAG_TOT_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_TOT_v2)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_TOT_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_TOT_v2) %>%
  rename("Baseline"="SCHRAG_TOT_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==2) %>%
      filter(!is.na(SCHRAG_TOT_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_TOT_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_TOT_v2-Baseline)/TIME_STUDY) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


# ALL SCHRAG Total Year 3

EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==3) %>%
  filter(!is.na(SCHRAG_TOT_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(SCHRAG_TOT_v2),
    sd=sd(SCHRAG_TOT_v2),
    median=median(SCHRAG_TOT_v2),
    Q1 = quantile(SCHRAG_TOT_v2, 0.25),
    Q3 = quantile(SCHRAG_TOT_v2, 0.75),
    min=min(SCHRAG_TOT_v2),
    max=max(SCHRAG_TOT_v2),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_TOT_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_TOT_v2) %>%
  rename("Baseline"="SCHRAG_TOT_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==3) %>%
      filter(!is.na(SCHRAG_TOT_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_TOT_v2)
  ) %>%  inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(mean= mean((SCHRAG_TOT_v2-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_TOT_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_TOT_v2) %>%
  rename("Baseline"="SCHRAG_TOT_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==3) %>%
      filter(!is.na(SCHRAG_TOT_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_TOT_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_TOT_v2-Baseline)) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_TOT_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_TOT_v2) %>%
  rename("Baseline"="SCHRAG_TOT_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==3) %>%
      filter(!is.na(SCHRAG_TOT_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_TOT_v2)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_TOT_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_TOT_v2) %>%
  rename("Baseline"="SCHRAG_TOT_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==3) %>%
      filter(!is.na(SCHRAG_TOT_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_TOT_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_TOT_v2-Baseline)/TIME_STUDY) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
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
# ALL SCHRAG Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_TOT_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(SCHRAG_TOT_v2),
    sd=sd(SCHRAG_TOT_v2),
    median=median(SCHRAG_TOT_v2),
    Q1 = quantile(SCHRAG_TOT_v2, 0.25),
    Q3 = quantile(SCHRAG_TOT_v2, 0.75),
    min=min(SCHRAG_TOT_v2),
    max=max(SCHRAG_TOT_v2),
    n=n()
  )



# ALL SCHRAG Total Year 1

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
  filter(!is.na(SCHRAG_TOT_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(SCHRAG_TOT_v2),
    sd=sd(SCHRAG_TOT_v2),
    median=median(SCHRAG_TOT_v2),
    Q1 = quantile(SCHRAG_TOT_v2, 0.25),
    Q3 = quantile(SCHRAG_TOT_v2, 0.75),
    min=min(SCHRAG_TOT_v2),
    max=max(SCHRAG_TOT_v2),
    n=n()
  )

# Change from baseline

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_TOT_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_TOT_v2) %>%
  rename("Baseline"="SCHRAG_TOT_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
      filter(!is.na(SCHRAG_TOT_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_TOT_v2)
  ) %>%  inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((SCHRAG_TOT_v2-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_TOT_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_TOT_v2) %>%
  rename("Baseline"="SCHRAG_TOT_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
      filter(!is.na(SCHRAG_TOT_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_TOT_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_TOT_v2-Baseline)) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAG) %>% 
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_TOT_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_TOT_v2) %>%
  rename("Baseline"="SCHRAG_TOT_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
      filter(!is.na(SCHRAG_TOT_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_TOT_v2)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_TOT_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_TOT_v2) %>%
  rename("Baseline"="SCHRAG_TOT_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
      filter(!is.na(SCHRAG_TOT_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_TOT_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_TOT_v2-Baseline)/TIME_STUDY) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



# ALL SCHRAG Total Year 2

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==2) %>%
  filter(!is.na(SCHRAG_TOT_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(SCHRAG_TOT_v2),
    sd=sd(SCHRAG_TOT_v2),
    median=median(SCHRAG_TOT_v2),
    Q1 = quantile(SCHRAG_TOT_v2, 0.25),
    Q3 = quantile(SCHRAG_TOT_v2, 0.75),
    min=min(SCHRAG_TOT_v2),
    max=max(SCHRAG_TOT_v2),
    n=n()
  )


# Change from baseline

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_TOT_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_TOT_v2) %>%
  rename("Baseline"="SCHRAG_TOT_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==2) %>%
      filter(!is.na(SCHRAG_TOT_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_TOT_v2)
  ) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((SCHRAG_TOT_v2-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_TOT_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_TOT_v2) %>%
  rename("Baseline"="SCHRAG_TOT_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==2) %>%
      filter(!is.na(SCHRAG_TOT_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_TOT_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_TOT_v2-Baseline)) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_TOT_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_TOT_v2) %>%
  rename("Baseline"="SCHRAG_TOT_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==2) %>%
      filter(!is.na(SCHRAG_TOT_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_TOT_v2)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_TOT_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_TOT_v2) %>%
  rename("Baseline"="SCHRAG_TOT_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==2) %>%
      filter(!is.na(SCHRAG_TOT_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_TOT_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_TOT_v2-Baseline)/TIME_STUDY) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


# ALL SCHRAG Total Year 3

EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==3) %>%
  filter(!is.na(SCHRAG_TOT_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(SCHRAG_TOT_v2),
    sd=sd(SCHRAG_TOT_v2),
    median=median(SCHRAG_TOT_v2),
    Q1 = quantile(SCHRAG_TOT_v2, 0.25),
    Q3 = quantile(SCHRAG_TOT_v2, 0.75),
    min=min(SCHRAG_TOT_v2),
    max=max(SCHRAG_TOT_v2),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_TOT_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_TOT_v2) %>%
  rename("Baseline"="SCHRAG_TOT_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==3) %>%
      filter(!is.na(SCHRAG_TOT_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_TOT_v2)
  ) %>% group_by(DIAG) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(mean= mean((SCHRAG_TOT_v2-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_TOT_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_TOT_v2) %>%
  rename("Baseline"="SCHRAG_TOT_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==3) %>%
      filter(!is.na(SCHRAG_TOT_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_TOT_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_TOT_v2-Baseline)) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_TOT_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_TOT_v2) %>%
  rename("Baseline"="SCHRAG_TOT_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==3) %>%
      filter(!is.na(SCHRAG_TOT_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_TOT_v2)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_TOT_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_TOT_v2) %>%
  rename("Baseline"="SCHRAG_TOT_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==3) %>%
      filter(!is.na(SCHRAG_TOT_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_TOT_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_TOT_v2-Baseline)/TIME_STUDY) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
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

# ALL SCHRAG Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_TOT_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(SCHRAG_TOT_v2),
    sd=sd(SCHRAG_TOT_v2),
    median=median(SCHRAG_TOT_v2),
    Q1 = quantile(SCHRAG_TOT_v2, 0.25),
    Q3 = quantile(SCHRAG_TOT_v2, 0.75),
    min=min(SCHRAG_TOT_v2),
    max=max(SCHRAG_TOT_v2),
    n=n()
  )



# ALL SCHRAG Total Year 1

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
  filter(!is.na(SCHRAG_TOT_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(SCHRAG_TOT_v2),
    sd=sd(SCHRAG_TOT_v2),
    median=median(SCHRAG_TOT_v2),
    Q1 = quantile(SCHRAG_TOT_v2, 0.25),
    Q3 = quantile(SCHRAG_TOT_v2, 0.75),
    min=min(SCHRAG_TOT_v2),
    max=max(SCHRAG_TOT_v2),
    n=n()
  )

# Change from baseline


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_TOT_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_TOT_v2) %>%
  rename("Baseline"="SCHRAG_TOT_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
      filter(!is.na(SCHRAG_TOT_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_TOT_v2)
  ) %>%  inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((SCHRAG_TOT_v2-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_TOT_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_TOT_v2) %>%
  rename("Baseline"="SCHRAG_TOT_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
      filter(!is.na(SCHRAG_TOT_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_TOT_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_TOT_v2-Baseline)) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )




EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_TOT_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_TOT_v2) %>%
  rename("Baseline"="SCHRAG_TOT_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
      filter(!is.na(SCHRAG_TOT_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_TOT_v2)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )




EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_TOT_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_TOT_v2) %>%
  rename("Baseline"="SCHRAG_TOT_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
      filter(!is.na(SCHRAG_TOT_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_TOT_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_TOT_v2-Baseline)/TIME_STUDY) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



# ALL SCHRAG Total Year 2

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==2) %>%
  filter(!is.na(SCHRAG_TOT_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(SCHRAG_TOT_v2),
    sd=sd(SCHRAG_TOT_v2),
    median=median(SCHRAG_TOT_v2),
    Q1 = quantile(SCHRAG_TOT_v2, 0.25),
    Q3 = quantile(SCHRAG_TOT_v2, 0.75),
    min=min(SCHRAG_TOT_v2),
    max=max(SCHRAG_TOT_v2),
    n=n()
  )


# Change from baseline

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_TOT_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_TOT_v2) %>%
  rename("Baseline"="SCHRAG_TOT_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==2) %>%
      filter(!is.na(SCHRAG_TOT_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_TOT_v2)
  ) %>%  inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((SCHRAG_TOT_v2-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_TOT_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_TOT_v2) %>%
  rename("Baseline"="SCHRAG_TOT_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==2) %>%
      filter(!is.na(SCHRAG_TOT_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_TOT_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_TOT_v2-Baseline)) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_TOT_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_TOT_v2) %>%
  rename("Baseline"="SCHRAG_TOT_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==2) %>%
      filter(!is.na(SCHRAG_TOT_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_TOT_v2)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_TOT_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_TOT_v2) %>%
  rename("Baseline"="SCHRAG_TOT_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==2) %>%
      filter(!is.na(SCHRAG_TOT_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_TOT_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_TOT_v2-Baseline)/TIME_STUDY) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


# ALL SCHRAG Total Year 3

EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==3) %>%
  filter(!is.na(SCHRAG_TOT_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(SCHRAG_TOT_v2),
    sd=sd(SCHRAG_TOT_v2),
    median=median(SCHRAG_TOT_v2),
    Q1 = quantile(SCHRAG_TOT_v2, 0.25),
    Q3 = quantile(SCHRAG_TOT_v2, 0.75),
    min=min(SCHRAG_TOT_v2),
    max=max(SCHRAG_TOT_v2),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_TOT_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_TOT_v2) %>%
  rename("Baseline"="SCHRAG_TOT_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==3) %>%
      filter(!is.na(SCHRAG_TOT_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_TOT_v2)
  ) %>% group_by(DIAGNIV) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(mean= mean((SCHRAG_TOT_v2-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_TOT_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_TOT_v2) %>%
  rename("Baseline"="SCHRAG_TOT_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==3) %>%
      filter(!is.na(SCHRAG_TOT_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_TOT_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_TOT_v2-Baseline)) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% 
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_TOT_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_TOT_v2) %>%
  rename("Baseline"="SCHRAG_TOT_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==3) %>%
      filter(!is.na(SCHRAG_TOT_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_TOT_v2)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_TOT_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_TOT_v2) %>%
  rename("Baseline"="SCHRAG_TOT_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==3) %>%
      filter(!is.na(SCHRAG_TOT_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_TOT_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_TOT_v2-Baseline)/TIME_STUDY) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
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


# Inputs Motor MSA QoL   ----------------------------------

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
 mutate(SCHRAG_MOTOR_v2/ ((14-motor_na)/14) ) %>%
  mutate(SCHRAG_NONMOTOR_v2/ ((14-non_motor_na)/14) ) %>%
  mutate(SCHRAG_EMOTIONAL_v2/ ((14-emotional_na)/14) ) %>%
  mutate(SCHRAG_TOT_v2=SCHRAG_MOTOR_v2+SCHRAG_NONMOTOR_v2+SCHRAG_EMOTIONAL_v2) 



# ----------------
# Overall Early CT  -------------------------------

# ALL SCHRAG Total Year 0 


EarlyCT_Pop_Baseline_319 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_MOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(SCHRAG_MOTOR_v2),
    sd=sd(SCHRAG_MOTOR_v2),
    median=median(SCHRAG_MOTOR_v2),
    Q1 = quantile(SCHRAG_MOTOR_v2, 0.25),
    Q3 = quantile(SCHRAG_MOTOR_v2, 0.75),
    min=min(SCHRAG_MOTOR_v2),
    max=max(SCHRAG_MOTOR_v2),
    n=n()
  )



# ALL SCHRAG Total Year 1

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
  filter(!is.na(SCHRAG_MOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(SCHRAG_MOTOR_v2),
    sd=sd(SCHRAG_MOTOR_v2),
    median=median(SCHRAG_MOTOR_v2),
    Q1 = quantile(SCHRAG_MOTOR_v2, 0.25),
    Q3 = quantile(SCHRAG_MOTOR_v2, 0.75),
    min=min(SCHRAG_MOTOR_v2),
    max=max(SCHRAG_MOTOR_v2),
    n=n()
  )

# Change from baseline

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_MOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_MOTOR_v2) %>%
  rename("Baseline"="SCHRAG_MOTOR_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
      filter(!is.na(SCHRAG_MOTOR_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_MOTOR_v2)
  ) %>%  inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(mean= mean((SCHRAG_MOTOR_v2-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_MOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_MOTOR_v2) %>%
  rename("Baseline"="SCHRAG_MOTOR_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
      filter(!is.na(SCHRAG_MOTOR_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_MOTOR_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_MOTOR_v2-Baseline)) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_MOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_MOTOR_v2) %>%
  rename("Baseline"="SCHRAG_MOTOR_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
      filter(!is.na(SCHRAG_MOTOR_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_MOTOR_v2)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_MOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_MOTOR_v2) %>%
  rename("Baseline"="SCHRAG_MOTOR_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
      filter(!is.na(SCHRAG_MOTOR_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_MOTOR_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_MOTOR_v2-Baseline)/TIME_STUDY) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



# ALL SCHRAG Total Year 2

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==2) %>%
  filter(!is.na(SCHRAG_MOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(SCHRAG_MOTOR_v2),
    sd=sd(SCHRAG_MOTOR_v2),
    median=median(SCHRAG_MOTOR_v2),
    Q1 = quantile(SCHRAG_MOTOR_v2, 0.25),
    Q3 = quantile(SCHRAG_MOTOR_v2, 0.75),
    min=min(SCHRAG_MOTOR_v2),
    max=max(SCHRAG_MOTOR_v2),
    n=n()
  )


# Change from baseline

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_MOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_MOTOR_v2) %>%
  rename("Baseline"="SCHRAG_MOTOR_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==2) %>%
      filter(!is.na(SCHRAG_MOTOR_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_MOTOR_v2)
  ) %>%  inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(mean= mean((SCHRAG_MOTOR_v2-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_MOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_MOTOR_v2) %>%
  rename("Baseline"="SCHRAG_MOTOR_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==2) %>%
      filter(!is.na(SCHRAG_MOTOR_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_MOTOR_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_MOTOR_v2-Baseline)) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_MOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_MOTOR_v2) %>%
  rename("Baseline"="SCHRAG_MOTOR_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==2) %>%
      filter(!is.na(SCHRAG_MOTOR_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_MOTOR_v2)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_MOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_MOTOR_v2) %>%
  rename("Baseline"="SCHRAG_MOTOR_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==2) %>%
      filter(!is.na(SCHRAG_MOTOR_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_MOTOR_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_MOTOR_v2-Baseline)/TIME_STUDY) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


# ALL SCHRAG Total Year 3

EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==3) %>%
  filter(!is.na(SCHRAG_MOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(SCHRAG_MOTOR_v2),
    sd=sd(SCHRAG_MOTOR_v2),
    median=median(SCHRAG_MOTOR_v2),
    Q1 = quantile(SCHRAG_MOTOR_v2, 0.25),
    Q3 = quantile(SCHRAG_MOTOR_v2, 0.75),
    min=min(SCHRAG_MOTOR_v2),
    max=max(SCHRAG_MOTOR_v2),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_MOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_MOTOR_v2) %>%
  rename("Baseline"="SCHRAG_MOTOR_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==3) %>%
      filter(!is.na(SCHRAG_MOTOR_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_MOTOR_v2)
  ) %>%  inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(mean= mean((SCHRAG_MOTOR_v2-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_MOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_MOTOR_v2) %>%
  rename("Baseline"="SCHRAG_MOTOR_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==3) %>%
      filter(!is.na(SCHRAG_MOTOR_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_MOTOR_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_MOTOR_v2-Baseline)) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_MOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_MOTOR_v2) %>%
  rename("Baseline"="SCHRAG_MOTOR_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==3) %>%
      filter(!is.na(SCHRAG_MOTOR_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_MOTOR_v2)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_MOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_MOTOR_v2) %>%
  rename("Baseline"="SCHRAG_MOTOR_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==3) %>%
      filter(!is.na(SCHRAG_MOTOR_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_MOTOR_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_MOTOR_v2-Baseline)/TIME_STUDY) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
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
# ALL SCHRAG Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_MOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(SCHRAG_MOTOR_v2),
    sd=sd(SCHRAG_MOTOR_v2),
    median=median(SCHRAG_MOTOR_v2),
    Q1 = quantile(SCHRAG_MOTOR_v2, 0.25),
    Q3 = quantile(SCHRAG_MOTOR_v2, 0.75),
    min=min(SCHRAG_MOTOR_v2),
    max=max(SCHRAG_MOTOR_v2),
    n=n()
  )



# ALL SCHRAG Total Year 1

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
  filter(!is.na(SCHRAG_MOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(SCHRAG_MOTOR_v2),
    sd=sd(SCHRAG_MOTOR_v2),
    median=median(SCHRAG_MOTOR_v2),
    Q1 = quantile(SCHRAG_MOTOR_v2, 0.25),
    Q3 = quantile(SCHRAG_MOTOR_v2, 0.75),
    min=min(SCHRAG_MOTOR_v2),
    max=max(SCHRAG_MOTOR_v2),
    n=n()
  )

# Change from baseline

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_MOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_MOTOR_v2) %>%
  rename("Baseline"="SCHRAG_MOTOR_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
      filter(!is.na(SCHRAG_MOTOR_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_MOTOR_v2)
  ) %>%  inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((SCHRAG_MOTOR_v2-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_MOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_MOTOR_v2) %>%
  rename("Baseline"="SCHRAG_MOTOR_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
      filter(!is.na(SCHRAG_MOTOR_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_MOTOR_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_MOTOR_v2-Baseline)) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAG) %>% 
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_MOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_MOTOR_v2) %>%
  rename("Baseline"="SCHRAG_MOTOR_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
      filter(!is.na(SCHRAG_MOTOR_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_MOTOR_v2)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_MOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_MOTOR_v2) %>%
  rename("Baseline"="SCHRAG_MOTOR_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
      filter(!is.na(SCHRAG_MOTOR_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_MOTOR_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_MOTOR_v2-Baseline)/TIME_STUDY) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



# ALL SCHRAG Total Year 2

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==2) %>%
  filter(!is.na(SCHRAG_MOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(SCHRAG_MOTOR_v2),
    sd=sd(SCHRAG_MOTOR_v2),
    median=median(SCHRAG_MOTOR_v2),
    Q1 = quantile(SCHRAG_MOTOR_v2, 0.25),
    Q3 = quantile(SCHRAG_MOTOR_v2, 0.75),
    min=min(SCHRAG_MOTOR_v2),
    max=max(SCHRAG_MOTOR_v2),
    n=n()
  )


# Change from baseline

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_MOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_MOTOR_v2) %>%
  rename("Baseline"="SCHRAG_MOTOR_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==2) %>%
      filter(!is.na(SCHRAG_MOTOR_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_MOTOR_v2)
  ) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((SCHRAG_MOTOR_v2-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_MOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_MOTOR_v2) %>%
  rename("Baseline"="SCHRAG_MOTOR_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==2) %>%
      filter(!is.na(SCHRAG_MOTOR_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_MOTOR_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_MOTOR_v2-Baseline)) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_MOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_MOTOR_v2) %>%
  rename("Baseline"="SCHRAG_MOTOR_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==2) %>%
      filter(!is.na(SCHRAG_MOTOR_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_MOTOR_v2)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_MOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_MOTOR_v2) %>%
  rename("Baseline"="SCHRAG_MOTOR_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==2) %>%
      filter(!is.na(SCHRAG_MOTOR_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_MOTOR_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_MOTOR_v2-Baseline)/TIME_STUDY) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


# ALL SCHRAG Total Year 3

EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==3) %>%
  filter(!is.na(SCHRAG_MOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(SCHRAG_MOTOR_v2),
    sd=sd(SCHRAG_MOTOR_v2),
    median=median(SCHRAG_MOTOR_v2),
    Q1 = quantile(SCHRAG_MOTOR_v2, 0.25),
    Q3 = quantile(SCHRAG_MOTOR_v2, 0.75),
    min=min(SCHRAG_MOTOR_v2),
    max=max(SCHRAG_MOTOR_v2),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_MOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_MOTOR_v2) %>%
  rename("Baseline"="SCHRAG_MOTOR_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==3) %>%
      filter(!is.na(SCHRAG_MOTOR_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_MOTOR_v2)
  ) %>% group_by(DIAG) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(mean= mean((SCHRAG_MOTOR_v2-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_MOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_MOTOR_v2) %>%
  rename("Baseline"="SCHRAG_MOTOR_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==3) %>%
      filter(!is.na(SCHRAG_MOTOR_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_MOTOR_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_MOTOR_v2-Baseline)) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_MOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_MOTOR_v2) %>%
  rename("Baseline"="SCHRAG_MOTOR_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==3) %>%
      filter(!is.na(SCHRAG_MOTOR_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_MOTOR_v2)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_MOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_MOTOR_v2) %>%
  rename("Baseline"="SCHRAG_MOTOR_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==3) %>%
      filter(!is.na(SCHRAG_MOTOR_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_MOTOR_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_MOTOR_v2-Baseline)/TIME_STUDY) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
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

# ALL SCHRAG Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_MOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(SCHRAG_MOTOR_v2),
    sd=sd(SCHRAG_MOTOR_v2),
    median=median(SCHRAG_MOTOR_v2),
    Q1 = quantile(SCHRAG_MOTOR_v2, 0.25),
    Q3 = quantile(SCHRAG_MOTOR_v2, 0.75),
    min=min(SCHRAG_MOTOR_v2),
    max=max(SCHRAG_MOTOR_v2),
    n=n()
  )



# ALL SCHRAG Total Year 1

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
  filter(!is.na(SCHRAG_MOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(SCHRAG_MOTOR_v2),
    sd=sd(SCHRAG_MOTOR_v2),
    median=median(SCHRAG_MOTOR_v2),
    Q1 = quantile(SCHRAG_MOTOR_v2, 0.25),
    Q3 = quantile(SCHRAG_MOTOR_v2, 0.75),
    min=min(SCHRAG_MOTOR_v2),
    max=max(SCHRAG_MOTOR_v2),
    n=n()
  )

# Change from baseline


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_MOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_MOTOR_v2) %>%
  rename("Baseline"="SCHRAG_MOTOR_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
      filter(!is.na(SCHRAG_MOTOR_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_MOTOR_v2)
  ) %>%  inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((SCHRAG_MOTOR_v2-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_MOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_MOTOR_v2) %>%
  rename("Baseline"="SCHRAG_MOTOR_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
      filter(!is.na(SCHRAG_MOTOR_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_MOTOR_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_MOTOR_v2-Baseline)) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )




EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_MOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_MOTOR_v2) %>%
  rename("Baseline"="SCHRAG_MOTOR_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
      filter(!is.na(SCHRAG_MOTOR_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_MOTOR_v2)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )




EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_MOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_MOTOR_v2) %>%
  rename("Baseline"="SCHRAG_MOTOR_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
      filter(!is.na(SCHRAG_MOTOR_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_MOTOR_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_MOTOR_v2-Baseline)/TIME_STUDY) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



# ALL SCHRAG Total Year 2

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==2) %>%
  filter(!is.na(SCHRAG_MOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(SCHRAG_MOTOR_v2),
    sd=sd(SCHRAG_MOTOR_v2),
    median=median(SCHRAG_MOTOR_v2),
    Q1 = quantile(SCHRAG_MOTOR_v2, 0.25),
    Q3 = quantile(SCHRAG_MOTOR_v2, 0.75),
    min=min(SCHRAG_MOTOR_v2),
    max=max(SCHRAG_MOTOR_v2),
    n=n()
  )


# Change from baseline

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_MOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_MOTOR_v2) %>%
  rename("Baseline"="SCHRAG_MOTOR_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==2) %>%
      filter(!is.na(SCHRAG_MOTOR_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_MOTOR_v2)
  ) %>%  inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((SCHRAG_MOTOR_v2-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_MOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_MOTOR_v2) %>%
  rename("Baseline"="SCHRAG_MOTOR_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==2) %>%
      filter(!is.na(SCHRAG_MOTOR_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_MOTOR_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_MOTOR_v2-Baseline)) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_MOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_MOTOR_v2) %>%
  rename("Baseline"="SCHRAG_MOTOR_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==2) %>%
      filter(!is.na(SCHRAG_MOTOR_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_MOTOR_v2)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_MOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_MOTOR_v2) %>%
  rename("Baseline"="SCHRAG_MOTOR_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==2) %>%
      filter(!is.na(SCHRAG_MOTOR_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_MOTOR_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_MOTOR_v2-Baseline)/TIME_STUDY) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


# ALL SCHRAG Total Year 3

EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==3) %>%
  filter(!is.na(SCHRAG_MOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(SCHRAG_MOTOR_v2),
    sd=sd(SCHRAG_MOTOR_v2),
    median=median(SCHRAG_MOTOR_v2),
    Q1 = quantile(SCHRAG_MOTOR_v2, 0.25),
    Q3 = quantile(SCHRAG_MOTOR_v2, 0.75),
    min=min(SCHRAG_MOTOR_v2),
    max=max(SCHRAG_MOTOR_v2),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_MOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_MOTOR_v2) %>%
  rename("Baseline"="SCHRAG_MOTOR_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==3) %>%
      filter(!is.na(SCHRAG_MOTOR_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_MOTOR_v2)
  ) %>% group_by(DIAGNIV) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(mean= mean((SCHRAG_MOTOR_v2-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_MOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_MOTOR_v2) %>%
  rename("Baseline"="SCHRAG_MOTOR_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==3) %>%
      filter(!is.na(SCHRAG_MOTOR_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_MOTOR_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_MOTOR_v2-Baseline)) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% 
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_MOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_MOTOR_v2) %>%
  rename("Baseline"="SCHRAG_MOTOR_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==3) %>%
      filter(!is.na(SCHRAG_MOTOR_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_MOTOR_v2)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_MOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_MOTOR_v2) %>%
  rename("Baseline"="SCHRAG_MOTOR_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==3) %>%
      filter(!is.na(SCHRAG_MOTOR_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_MOTOR_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_MOTOR_v2-Baseline)/TIME_STUDY) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
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


# Inputs non-Motor MSA QoL   ----------------------------------


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
 mutate(SCHRAG_MOTOR_v2/ ((14-motor_na)/14) ) %>%
  mutate(SCHRAG_NONMOTOR_v2/ ((14-non_motor_na)/14) ) %>%
  mutate(SCHRAG_EMOTIONAL_v2/ ((14-emotional_na)/14) ) %>%
  mutate(SCHRAG_TOT_v2=SCHRAG_MOTOR_v2+SCHRAG_NONMOTOR_v2+SCHRAG_EMOTIONAL_v2) 


# ----------------
# Overall Early CT  -------------------------------

# ALL SCHRAG Total Year 0 


EarlyCT_Pop_Baseline_319 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_NONMOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(SCHRAG_NONMOTOR_v2),
    sd=sd(SCHRAG_NONMOTOR_v2),
    median=median(SCHRAG_NONMOTOR_v2),
    Q1 = quantile(SCHRAG_NONMOTOR_v2, 0.25),
    Q3 = quantile(SCHRAG_NONMOTOR_v2, 0.75),
    min=min(SCHRAG_NONMOTOR_v2),
    max=max(SCHRAG_NONMOTOR_v2),
    n=n()
  )



# ALL SCHRAG Total Year 1

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
  filter(!is.na(SCHRAG_NONMOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(SCHRAG_NONMOTOR_v2),
    sd=sd(SCHRAG_NONMOTOR_v2),
    median=median(SCHRAG_NONMOTOR_v2),
    Q1 = quantile(SCHRAG_NONMOTOR_v2, 0.25),
    Q3 = quantile(SCHRAG_NONMOTOR_v2, 0.75),
    min=min(SCHRAG_NONMOTOR_v2),
    max=max(SCHRAG_NONMOTOR_v2),
    n=n()
  )

# Change from baseline

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_NONMOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_NONMOTOR_v2) %>%
  rename("Baseline"="SCHRAG_NONMOTOR_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
      filter(!is.na(SCHRAG_NONMOTOR_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_NONMOTOR_v2)
  ) %>%  inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(mean= mean((SCHRAG_NONMOTOR_v2-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_NONMOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_NONMOTOR_v2) %>%
  rename("Baseline"="SCHRAG_NONMOTOR_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
      filter(!is.na(SCHRAG_NONMOTOR_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_NONMOTOR_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_NONMOTOR_v2-Baseline)) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_NONMOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_NONMOTOR_v2) %>%
  rename("Baseline"="SCHRAG_NONMOTOR_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
      filter(!is.na(SCHRAG_NONMOTOR_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_NONMOTOR_v2)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_NONMOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_NONMOTOR_v2) %>%
  rename("Baseline"="SCHRAG_NONMOTOR_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
      filter(!is.na(SCHRAG_NONMOTOR_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_NONMOTOR_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_NONMOTOR_v2-Baseline)/TIME_STUDY) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



# ALL SCHRAG Total Year 2

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==2) %>%
  filter(!is.na(SCHRAG_NONMOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(SCHRAG_NONMOTOR_v2),
    sd=sd(SCHRAG_NONMOTOR_v2),
    median=median(SCHRAG_NONMOTOR_v2),
    Q1 = quantile(SCHRAG_NONMOTOR_v2, 0.25),
    Q3 = quantile(SCHRAG_NONMOTOR_v2, 0.75),
    min=min(SCHRAG_NONMOTOR_v2),
    max=max(SCHRAG_NONMOTOR_v2),
    n=n()
  )


# Change from baseline

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_NONMOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_NONMOTOR_v2) %>%
  rename("Baseline"="SCHRAG_NONMOTOR_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==2) %>%
      filter(!is.na(SCHRAG_NONMOTOR_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_NONMOTOR_v2)
  ) %>%  inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(mean= mean((SCHRAG_NONMOTOR_v2-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_NONMOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_NONMOTOR_v2) %>%
  rename("Baseline"="SCHRAG_NONMOTOR_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==2) %>%
      filter(!is.na(SCHRAG_NONMOTOR_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_NONMOTOR_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_NONMOTOR_v2-Baseline)) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_NONMOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_NONMOTOR_v2) %>%
  rename("Baseline"="SCHRAG_NONMOTOR_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==2) %>%
      filter(!is.na(SCHRAG_NONMOTOR_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_NONMOTOR_v2)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_NONMOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_NONMOTOR_v2) %>%
  rename("Baseline"="SCHRAG_NONMOTOR_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==2) %>%
      filter(!is.na(SCHRAG_NONMOTOR_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_NONMOTOR_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_NONMOTOR_v2-Baseline)/TIME_STUDY) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


# ALL SCHRAG Total Year 3

EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==3) %>%
  filter(!is.na(SCHRAG_NONMOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(SCHRAG_NONMOTOR_v2),
    sd=sd(SCHRAG_NONMOTOR_v2),
    median=median(SCHRAG_NONMOTOR_v2),
    Q1 = quantile(SCHRAG_NONMOTOR_v2, 0.25),
    Q3 = quantile(SCHRAG_NONMOTOR_v2, 0.75),
    min=min(SCHRAG_NONMOTOR_v2),
    max=max(SCHRAG_NONMOTOR_v2),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_NONMOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_NONMOTOR_v2) %>%
  rename("Baseline"="SCHRAG_NONMOTOR_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==3) %>%
      filter(!is.na(SCHRAG_NONMOTOR_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_NONMOTOR_v2)
  ) %>%  inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(mean= mean((SCHRAG_NONMOTOR_v2-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_NONMOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_NONMOTOR_v2) %>%
  rename("Baseline"="SCHRAG_NONMOTOR_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==3) %>%
      filter(!is.na(SCHRAG_NONMOTOR_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_NONMOTOR_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_NONMOTOR_v2-Baseline)) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_NONMOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_NONMOTOR_v2) %>%
  rename("Baseline"="SCHRAG_NONMOTOR_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==3) %>%
      filter(!is.na(SCHRAG_NONMOTOR_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_NONMOTOR_v2)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_NONMOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_NONMOTOR_v2) %>%
  rename("Baseline"="SCHRAG_NONMOTOR_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==3) %>%
      filter(!is.na(SCHRAG_NONMOTOR_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_NONMOTOR_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_NONMOTOR_v2-Baseline)/TIME_STUDY) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
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
# ALL SCHRAG Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_NONMOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(SCHRAG_NONMOTOR_v2),
    sd=sd(SCHRAG_NONMOTOR_v2),
    median=median(SCHRAG_NONMOTOR_v2),
    Q1 = quantile(SCHRAG_NONMOTOR_v2, 0.25),
    Q3 = quantile(SCHRAG_NONMOTOR_v2, 0.75),
    min=min(SCHRAG_NONMOTOR_v2),
    max=max(SCHRAG_NONMOTOR_v2),
    n=n()
  )



# ALL SCHRAG Total Year 1

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
  filter(!is.na(SCHRAG_NONMOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(SCHRAG_NONMOTOR_v2),
    sd=sd(SCHRAG_NONMOTOR_v2),
    median=median(SCHRAG_NONMOTOR_v2),
    Q1 = quantile(SCHRAG_NONMOTOR_v2, 0.25),
    Q3 = quantile(SCHRAG_NONMOTOR_v2, 0.75),
    min=min(SCHRAG_NONMOTOR_v2),
    max=max(SCHRAG_NONMOTOR_v2),
    n=n()
  )

# Change from baseline

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_NONMOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_NONMOTOR_v2) %>%
  rename("Baseline"="SCHRAG_NONMOTOR_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
      filter(!is.na(SCHRAG_NONMOTOR_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_NONMOTOR_v2)
  ) %>%  inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((SCHRAG_NONMOTOR_v2-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_NONMOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_NONMOTOR_v2) %>%
  rename("Baseline"="SCHRAG_NONMOTOR_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
      filter(!is.na(SCHRAG_NONMOTOR_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_NONMOTOR_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_NONMOTOR_v2-Baseline)) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAG) %>% 
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_NONMOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_NONMOTOR_v2) %>%
  rename("Baseline"="SCHRAG_NONMOTOR_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
      filter(!is.na(SCHRAG_NONMOTOR_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_NONMOTOR_v2)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_NONMOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_NONMOTOR_v2) %>%
  rename("Baseline"="SCHRAG_NONMOTOR_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
      filter(!is.na(SCHRAG_NONMOTOR_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_NONMOTOR_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_NONMOTOR_v2-Baseline)/TIME_STUDY) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



# ALL SCHRAG Total Year 2

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==2) %>%
  filter(!is.na(SCHRAG_NONMOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(SCHRAG_NONMOTOR_v2),
    sd=sd(SCHRAG_NONMOTOR_v2),
    median=median(SCHRAG_NONMOTOR_v2),
    Q1 = quantile(SCHRAG_NONMOTOR_v2, 0.25),
    Q3 = quantile(SCHRAG_NONMOTOR_v2, 0.75),
    min=min(SCHRAG_NONMOTOR_v2),
    max=max(SCHRAG_NONMOTOR_v2),
    n=n()
  )


# Change from baseline

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_NONMOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_NONMOTOR_v2) %>%
  rename("Baseline"="SCHRAG_NONMOTOR_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==2) %>%
      filter(!is.na(SCHRAG_NONMOTOR_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_NONMOTOR_v2)
  ) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((SCHRAG_NONMOTOR_v2-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_NONMOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_NONMOTOR_v2) %>%
  rename("Baseline"="SCHRAG_NONMOTOR_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==2) %>%
      filter(!is.na(SCHRAG_NONMOTOR_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_NONMOTOR_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_NONMOTOR_v2-Baseline)) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_NONMOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_NONMOTOR_v2) %>%
  rename("Baseline"="SCHRAG_NONMOTOR_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==2) %>%
      filter(!is.na(SCHRAG_NONMOTOR_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_NONMOTOR_v2)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_NONMOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_NONMOTOR_v2) %>%
  rename("Baseline"="SCHRAG_NONMOTOR_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==2) %>%
      filter(!is.na(SCHRAG_NONMOTOR_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_NONMOTOR_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_NONMOTOR_v2-Baseline)/TIME_STUDY) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


# ALL SCHRAG Total Year 3

EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==3) %>%
  filter(!is.na(SCHRAG_NONMOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(SCHRAG_NONMOTOR_v2),
    sd=sd(SCHRAG_NONMOTOR_v2),
    median=median(SCHRAG_NONMOTOR_v2),
    Q1 = quantile(SCHRAG_NONMOTOR_v2, 0.25),
    Q3 = quantile(SCHRAG_NONMOTOR_v2, 0.75),
    min=min(SCHRAG_NONMOTOR_v2),
    max=max(SCHRAG_NONMOTOR_v2),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_NONMOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_NONMOTOR_v2) %>%
  rename("Baseline"="SCHRAG_NONMOTOR_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==3) %>%
      filter(!is.na(SCHRAG_NONMOTOR_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_NONMOTOR_v2)
  ) %>% group_by(DIAG) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(mean= mean((SCHRAG_NONMOTOR_v2-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_NONMOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_NONMOTOR_v2) %>%
  rename("Baseline"="SCHRAG_NONMOTOR_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==3) %>%
      filter(!is.na(SCHRAG_NONMOTOR_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_NONMOTOR_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_NONMOTOR_v2-Baseline)) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_NONMOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_NONMOTOR_v2) %>%
  rename("Baseline"="SCHRAG_NONMOTOR_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==3) %>%
      filter(!is.na(SCHRAG_NONMOTOR_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_NONMOTOR_v2)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_NONMOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_NONMOTOR_v2) %>%
  rename("Baseline"="SCHRAG_NONMOTOR_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==3) %>%
      filter(!is.na(SCHRAG_NONMOTOR_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_NONMOTOR_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_NONMOTOR_v2-Baseline)/TIME_STUDY) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
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

# ALL SCHRAG Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_NONMOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(SCHRAG_NONMOTOR_v2),
    sd=sd(SCHRAG_NONMOTOR_v2),
    median=median(SCHRAG_NONMOTOR_v2),
    Q1 = quantile(SCHRAG_NONMOTOR_v2, 0.25),
    Q3 = quantile(SCHRAG_NONMOTOR_v2, 0.75),
    min=min(SCHRAG_NONMOTOR_v2),
    max=max(SCHRAG_NONMOTOR_v2),
    n=n()
  )



# ALL SCHRAG Total Year 1

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
  filter(!is.na(SCHRAG_NONMOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(SCHRAG_NONMOTOR_v2),
    sd=sd(SCHRAG_NONMOTOR_v2),
    median=median(SCHRAG_NONMOTOR_v2),
    Q1 = quantile(SCHRAG_NONMOTOR_v2, 0.25),
    Q3 = quantile(SCHRAG_NONMOTOR_v2, 0.75),
    min=min(SCHRAG_NONMOTOR_v2),
    max=max(SCHRAG_NONMOTOR_v2),
    n=n()
  )

# Change from baseline


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_NONMOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_NONMOTOR_v2) %>%
  rename("Baseline"="SCHRAG_NONMOTOR_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
      filter(!is.na(SCHRAG_NONMOTOR_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_NONMOTOR_v2)
  ) %>%  inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((SCHRAG_NONMOTOR_v2-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_NONMOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_NONMOTOR_v2) %>%
  rename("Baseline"="SCHRAG_NONMOTOR_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
      filter(!is.na(SCHRAG_NONMOTOR_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_NONMOTOR_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_NONMOTOR_v2-Baseline)) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )




EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_NONMOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_NONMOTOR_v2) %>%
  rename("Baseline"="SCHRAG_NONMOTOR_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
      filter(!is.na(SCHRAG_NONMOTOR_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_NONMOTOR_v2)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )




EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_NONMOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_NONMOTOR_v2) %>%
  rename("Baseline"="SCHRAG_NONMOTOR_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
      filter(!is.na(SCHRAG_NONMOTOR_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_NONMOTOR_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_NONMOTOR_v2-Baseline)/TIME_STUDY) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



# ALL SCHRAG Total Year 2

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==2) %>%
  filter(!is.na(SCHRAG_NONMOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(SCHRAG_NONMOTOR_v2),
    sd=sd(SCHRAG_NONMOTOR_v2),
    median=median(SCHRAG_NONMOTOR_v2),
    Q1 = quantile(SCHRAG_NONMOTOR_v2, 0.25),
    Q3 = quantile(SCHRAG_NONMOTOR_v2, 0.75),
    min=min(SCHRAG_NONMOTOR_v2),
    max=max(SCHRAG_NONMOTOR_v2),
    n=n()
  )


# Change from baseline

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_NONMOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_NONMOTOR_v2) %>%
  rename("Baseline"="SCHRAG_NONMOTOR_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==2) %>%
      filter(!is.na(SCHRAG_NONMOTOR_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_NONMOTOR_v2)
  ) %>%  inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((SCHRAG_NONMOTOR_v2-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_NONMOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_NONMOTOR_v2) %>%
  rename("Baseline"="SCHRAG_NONMOTOR_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==2) %>%
      filter(!is.na(SCHRAG_NONMOTOR_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_NONMOTOR_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_NONMOTOR_v2-Baseline)) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_NONMOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_NONMOTOR_v2) %>%
  rename("Baseline"="SCHRAG_NONMOTOR_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==2) %>%
      filter(!is.na(SCHRAG_NONMOTOR_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_NONMOTOR_v2)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_NONMOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_NONMOTOR_v2) %>%
  rename("Baseline"="SCHRAG_NONMOTOR_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==2) %>%
      filter(!is.na(SCHRAG_NONMOTOR_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_NONMOTOR_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_NONMOTOR_v2-Baseline)/TIME_STUDY) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


# ALL SCHRAG Total Year 3

EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==3) %>%
  filter(!is.na(SCHRAG_NONMOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(SCHRAG_NONMOTOR_v2),
    sd=sd(SCHRAG_NONMOTOR_v2),
    median=median(SCHRAG_NONMOTOR_v2),
    Q1 = quantile(SCHRAG_NONMOTOR_v2, 0.25),
    Q3 = quantile(SCHRAG_NONMOTOR_v2, 0.75),
    min=min(SCHRAG_NONMOTOR_v2),
    max=max(SCHRAG_NONMOTOR_v2),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_NONMOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_NONMOTOR_v2) %>%
  rename("Baseline"="SCHRAG_NONMOTOR_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==3) %>%
      filter(!is.na(SCHRAG_NONMOTOR_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_NONMOTOR_v2)
  ) %>% group_by(DIAGNIV) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(mean= mean((SCHRAG_NONMOTOR_v2-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_NONMOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_NONMOTOR_v2) %>%
  rename("Baseline"="SCHRAG_NONMOTOR_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==3) %>%
      filter(!is.na(SCHRAG_NONMOTOR_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_NONMOTOR_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_NONMOTOR_v2-Baseline)) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% 
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_NONMOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_NONMOTOR_v2) %>%
  rename("Baseline"="SCHRAG_NONMOTOR_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==3) %>%
      filter(!is.na(SCHRAG_NONMOTOR_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_NONMOTOR_v2)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_NONMOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_NONMOTOR_v2) %>%
  rename("Baseline"="SCHRAG_NONMOTOR_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==3) %>%
      filter(!is.na(SCHRAG_NONMOTOR_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_NONMOTOR_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_NONMOTOR_v2-Baseline)/TIME_STUDY) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
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


# Inputs Emotional MSA QoL   ----------------------------------

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
 mutate(SCHRAG_MOTOR_v2/ ((14-motor_na)/14) ) %>%
  mutate(SCHRAG_NONMOTOR_v2/ ((14-non_motor_na)/14) ) %>%
  mutate(SCHRAG_EMOTIONAL_v2/ ((14-emotional_na)/14) ) %>%
  mutate(SCHRAG_TOT_v2=SCHRAG_MOTOR_v2+SCHRAG_NONMOTOR_v2+SCHRAG_EMOTIONAL_v2) 



# ----------------
# Overall Early CT  -------------------------------

# ALL SCHRAG Total Year 0 


EarlyCT_Pop_Baseline_319 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_EMOTIONAL_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(SCHRAG_EMOTIONAL_v2),
    sd=sd(SCHRAG_EMOTIONAL_v2),
    median=median(SCHRAG_EMOTIONAL_v2),
    Q1 = quantile(SCHRAG_EMOTIONAL_v2, 0.25),
    Q3 = quantile(SCHRAG_EMOTIONAL_v2, 0.75),
    min=min(SCHRAG_EMOTIONAL_v2),
    max=max(SCHRAG_EMOTIONAL_v2),
    n=n()
  )



# ALL SCHRAG Total Year 1

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
  filter(!is.na(SCHRAG_EMOTIONAL_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(SCHRAG_EMOTIONAL_v2),
    sd=sd(SCHRAG_EMOTIONAL_v2),
    median=median(SCHRAG_EMOTIONAL_v2),
    Q1 = quantile(SCHRAG_EMOTIONAL_v2, 0.25),
    Q3 = quantile(SCHRAG_EMOTIONAL_v2, 0.75),
    min=min(SCHRAG_EMOTIONAL_v2),
    max=max(SCHRAG_EMOTIONAL_v2),
    n=n()
  )

# Change from baseline

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_EMOTIONAL_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_EMOTIONAL_v2) %>%
  rename("Baseline"="SCHRAG_EMOTIONAL_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
      filter(!is.na(SCHRAG_EMOTIONAL_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_EMOTIONAL_v2)
  ) %>%  inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(mean= mean((SCHRAG_EMOTIONAL_v2-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_EMOTIONAL_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_EMOTIONAL_v2) %>%
  rename("Baseline"="SCHRAG_EMOTIONAL_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
      filter(!is.na(SCHRAG_EMOTIONAL_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_EMOTIONAL_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_EMOTIONAL_v2-Baseline)) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_EMOTIONAL_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_EMOTIONAL_v2) %>%
  rename("Baseline"="SCHRAG_EMOTIONAL_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
      filter(!is.na(SCHRAG_EMOTIONAL_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_EMOTIONAL_v2)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_EMOTIONAL_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_EMOTIONAL_v2) %>%
  rename("Baseline"="SCHRAG_EMOTIONAL_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
      filter(!is.na(SCHRAG_EMOTIONAL_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_EMOTIONAL_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_EMOTIONAL_v2-Baseline)/TIME_STUDY) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



# ALL SCHRAG Total Year 2

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==2) %>%
  filter(!is.na(SCHRAG_EMOTIONAL_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(SCHRAG_EMOTIONAL_v2),
    sd=sd(SCHRAG_EMOTIONAL_v2),
    median=median(SCHRAG_EMOTIONAL_v2),
    Q1 = quantile(SCHRAG_EMOTIONAL_v2, 0.25),
    Q3 = quantile(SCHRAG_EMOTIONAL_v2, 0.75),
    min=min(SCHRAG_EMOTIONAL_v2),
    max=max(SCHRAG_EMOTIONAL_v2),
    n=n()
  )


# Change from baseline

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_EMOTIONAL_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_EMOTIONAL_v2) %>%
  rename("Baseline"="SCHRAG_EMOTIONAL_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==2) %>%
      filter(!is.na(SCHRAG_EMOTIONAL_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_EMOTIONAL_v2)
  ) %>%  inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(mean= mean((SCHRAG_EMOTIONAL_v2-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_EMOTIONAL_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_EMOTIONAL_v2) %>%
  rename("Baseline"="SCHRAG_EMOTIONAL_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==2) %>%
      filter(!is.na(SCHRAG_EMOTIONAL_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_EMOTIONAL_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_EMOTIONAL_v2-Baseline)) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_EMOTIONAL_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_EMOTIONAL_v2) %>%
  rename("Baseline"="SCHRAG_EMOTIONAL_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==2) %>%
      filter(!is.na(SCHRAG_EMOTIONAL_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_EMOTIONAL_v2)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_EMOTIONAL_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_EMOTIONAL_v2) %>%
  rename("Baseline"="SCHRAG_EMOTIONAL_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==2) %>%
      filter(!is.na(SCHRAG_EMOTIONAL_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_EMOTIONAL_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_EMOTIONAL_v2-Baseline)/TIME_STUDY) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


# ALL SCHRAG Total Year 3

EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==3) %>%
  filter(!is.na(SCHRAG_EMOTIONAL_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(SCHRAG_EMOTIONAL_v2),
    sd=sd(SCHRAG_EMOTIONAL_v2),
    median=median(SCHRAG_EMOTIONAL_v2),
    Q1 = quantile(SCHRAG_EMOTIONAL_v2, 0.25),
    Q3 = quantile(SCHRAG_EMOTIONAL_v2, 0.75),
    min=min(SCHRAG_EMOTIONAL_v2),
    max=max(SCHRAG_EMOTIONAL_v2),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_EMOTIONAL_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_EMOTIONAL_v2) %>%
  rename("Baseline"="SCHRAG_EMOTIONAL_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==3) %>%
      filter(!is.na(SCHRAG_EMOTIONAL_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_EMOTIONAL_v2)
  ) %>%  inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(mean= mean((SCHRAG_EMOTIONAL_v2-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_EMOTIONAL_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_EMOTIONAL_v2) %>%
  rename("Baseline"="SCHRAG_EMOTIONAL_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==3) %>%
      filter(!is.na(SCHRAG_EMOTIONAL_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_EMOTIONAL_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_EMOTIONAL_v2-Baseline)) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_EMOTIONAL_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_EMOTIONAL_v2) %>%
  rename("Baseline"="SCHRAG_EMOTIONAL_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==3) %>%
      filter(!is.na(SCHRAG_EMOTIONAL_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_EMOTIONAL_v2)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_EMOTIONAL_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_EMOTIONAL_v2) %>%
  rename("Baseline"="SCHRAG_EMOTIONAL_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==3) %>%
      filter(!is.na(SCHRAG_EMOTIONAL_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_EMOTIONAL_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_EMOTIONAL_v2-Baseline)/TIME_STUDY) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
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
# ALL SCHRAG Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_EMOTIONAL_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(SCHRAG_EMOTIONAL_v2),
    sd=sd(SCHRAG_EMOTIONAL_v2),
    median=median(SCHRAG_EMOTIONAL_v2),
    Q1 = quantile(SCHRAG_EMOTIONAL_v2, 0.25),
    Q3 = quantile(SCHRAG_EMOTIONAL_v2, 0.75),
    min=min(SCHRAG_EMOTIONAL_v2),
    max=max(SCHRAG_EMOTIONAL_v2),
    n=n()
  )



# ALL SCHRAG Total Year 1

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
  filter(!is.na(SCHRAG_EMOTIONAL_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(SCHRAG_EMOTIONAL_v2),
    sd=sd(SCHRAG_EMOTIONAL_v2),
    median=median(SCHRAG_EMOTIONAL_v2),
    Q1 = quantile(SCHRAG_EMOTIONAL_v2, 0.25),
    Q3 = quantile(SCHRAG_EMOTIONAL_v2, 0.75),
    min=min(SCHRAG_EMOTIONAL_v2),
    max=max(SCHRAG_EMOTIONAL_v2),
    n=n()
  )

# Change from baseline

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_EMOTIONAL_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_EMOTIONAL_v2) %>%
  rename("Baseline"="SCHRAG_EMOTIONAL_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
      filter(!is.na(SCHRAG_EMOTIONAL_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_EMOTIONAL_v2)
  ) %>%  inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((SCHRAG_EMOTIONAL_v2-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_EMOTIONAL_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_EMOTIONAL_v2) %>%
  rename("Baseline"="SCHRAG_EMOTIONAL_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
      filter(!is.na(SCHRAG_EMOTIONAL_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_EMOTIONAL_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_EMOTIONAL_v2-Baseline)) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAG) %>% 
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_EMOTIONAL_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_EMOTIONAL_v2) %>%
  rename("Baseline"="SCHRAG_EMOTIONAL_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
      filter(!is.na(SCHRAG_EMOTIONAL_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_EMOTIONAL_v2)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_EMOTIONAL_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_EMOTIONAL_v2) %>%
  rename("Baseline"="SCHRAG_EMOTIONAL_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
      filter(!is.na(SCHRAG_EMOTIONAL_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_EMOTIONAL_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_EMOTIONAL_v2-Baseline)/TIME_STUDY) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



# ALL SCHRAG Total Year 2

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==2) %>%
  filter(!is.na(SCHRAG_EMOTIONAL_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(SCHRAG_EMOTIONAL_v2),
    sd=sd(SCHRAG_EMOTIONAL_v2),
    median=median(SCHRAG_EMOTIONAL_v2),
    Q1 = quantile(SCHRAG_EMOTIONAL_v2, 0.25),
    Q3 = quantile(SCHRAG_EMOTIONAL_v2, 0.75),
    min=min(SCHRAG_EMOTIONAL_v2),
    max=max(SCHRAG_EMOTIONAL_v2),
    n=n()
  )


# Change from baseline

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_EMOTIONAL_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_EMOTIONAL_v2) %>%
  rename("Baseline"="SCHRAG_EMOTIONAL_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==2) %>%
      filter(!is.na(SCHRAG_EMOTIONAL_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_EMOTIONAL_v2)
  ) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((SCHRAG_EMOTIONAL_v2-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_EMOTIONAL_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_EMOTIONAL_v2) %>%
  rename("Baseline"="SCHRAG_EMOTIONAL_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==2) %>%
      filter(!is.na(SCHRAG_EMOTIONAL_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_EMOTIONAL_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_EMOTIONAL_v2-Baseline)) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_EMOTIONAL_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_EMOTIONAL_v2) %>%
  rename("Baseline"="SCHRAG_EMOTIONAL_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==2) %>%
      filter(!is.na(SCHRAG_EMOTIONAL_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_EMOTIONAL_v2)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_EMOTIONAL_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_EMOTIONAL_v2) %>%
  rename("Baseline"="SCHRAG_EMOTIONAL_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==2) %>%
      filter(!is.na(SCHRAG_EMOTIONAL_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_EMOTIONAL_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_EMOTIONAL_v2-Baseline)/TIME_STUDY) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


# ALL SCHRAG Total Year 3

EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==3) %>%
  filter(!is.na(SCHRAG_EMOTIONAL_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(SCHRAG_EMOTIONAL_v2),
    sd=sd(SCHRAG_EMOTIONAL_v2),
    median=median(SCHRAG_EMOTIONAL_v2),
    Q1 = quantile(SCHRAG_EMOTIONAL_v2, 0.25),
    Q3 = quantile(SCHRAG_EMOTIONAL_v2, 0.75),
    min=min(SCHRAG_EMOTIONAL_v2),
    max=max(SCHRAG_EMOTIONAL_v2),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_EMOTIONAL_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_EMOTIONAL_v2) %>%
  rename("Baseline"="SCHRAG_EMOTIONAL_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==3) %>%
      filter(!is.na(SCHRAG_EMOTIONAL_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_EMOTIONAL_v2)
  ) %>% group_by(DIAG) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(mean= mean((SCHRAG_EMOTIONAL_v2-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_EMOTIONAL_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_EMOTIONAL_v2) %>%
  rename("Baseline"="SCHRAG_EMOTIONAL_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==3) %>%
      filter(!is.na(SCHRAG_EMOTIONAL_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_EMOTIONAL_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_EMOTIONAL_v2-Baseline)) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_EMOTIONAL_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_EMOTIONAL_v2) %>%
  rename("Baseline"="SCHRAG_EMOTIONAL_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==3) %>%
      filter(!is.na(SCHRAG_EMOTIONAL_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_EMOTIONAL_v2)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_EMOTIONAL_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_EMOTIONAL_v2) %>%
  rename("Baseline"="SCHRAG_EMOTIONAL_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==3) %>%
      filter(!is.na(SCHRAG_EMOTIONAL_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_EMOTIONAL_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_EMOTIONAL_v2-Baseline)/TIME_STUDY) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
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

# ALL SCHRAG Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_EMOTIONAL_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(SCHRAG_EMOTIONAL_v2),
    sd=sd(SCHRAG_EMOTIONAL_v2),
    median=median(SCHRAG_EMOTIONAL_v2),
    Q1 = quantile(SCHRAG_EMOTIONAL_v2, 0.25),
    Q3 = quantile(SCHRAG_EMOTIONAL_v2, 0.75),
    min=min(SCHRAG_EMOTIONAL_v2),
    max=max(SCHRAG_EMOTIONAL_v2),
    n=n()
  )



# ALL SCHRAG Total Year 1

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
  filter(!is.na(SCHRAG_EMOTIONAL_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(SCHRAG_EMOTIONAL_v2),
    sd=sd(SCHRAG_EMOTIONAL_v2),
    median=median(SCHRAG_EMOTIONAL_v2),
    Q1 = quantile(SCHRAG_EMOTIONAL_v2, 0.25),
    Q3 = quantile(SCHRAG_EMOTIONAL_v2, 0.75),
    min=min(SCHRAG_EMOTIONAL_v2),
    max=max(SCHRAG_EMOTIONAL_v2),
    n=n()
  )

# Change from baseline


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_EMOTIONAL_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_EMOTIONAL_v2) %>%
  rename("Baseline"="SCHRAG_EMOTIONAL_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
      filter(!is.na(SCHRAG_EMOTIONAL_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_EMOTIONAL_v2)
  ) %>%  inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((SCHRAG_EMOTIONAL_v2-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_EMOTIONAL_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_EMOTIONAL_v2) %>%
  rename("Baseline"="SCHRAG_EMOTIONAL_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
      filter(!is.na(SCHRAG_EMOTIONAL_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_EMOTIONAL_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_EMOTIONAL_v2-Baseline)) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )




EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_EMOTIONAL_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_EMOTIONAL_v2) %>%
  rename("Baseline"="SCHRAG_EMOTIONAL_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
      filter(!is.na(SCHRAG_EMOTIONAL_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_EMOTIONAL_v2)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )




EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_EMOTIONAL_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_EMOTIONAL_v2) %>%
  rename("Baseline"="SCHRAG_EMOTIONAL_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
      filter(!is.na(SCHRAG_EMOTIONAL_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_EMOTIONAL_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_EMOTIONAL_v2-Baseline)/TIME_STUDY) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



# ALL SCHRAG Total Year 2

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==2) %>%
  filter(!is.na(SCHRAG_EMOTIONAL_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(SCHRAG_EMOTIONAL_v2),
    sd=sd(SCHRAG_EMOTIONAL_v2),
    median=median(SCHRAG_EMOTIONAL_v2),
    Q1 = quantile(SCHRAG_EMOTIONAL_v2, 0.25),
    Q3 = quantile(SCHRAG_EMOTIONAL_v2, 0.75),
    min=min(SCHRAG_EMOTIONAL_v2),
    max=max(SCHRAG_EMOTIONAL_v2),
    n=n()
  )


# Change from baseline

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_EMOTIONAL_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_EMOTIONAL_v2) %>%
  rename("Baseline"="SCHRAG_EMOTIONAL_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==2) %>%
      filter(!is.na(SCHRAG_EMOTIONAL_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_EMOTIONAL_v2)
  ) %>%  inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((SCHRAG_EMOTIONAL_v2-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_EMOTIONAL_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_EMOTIONAL_v2) %>%
  rename("Baseline"="SCHRAG_EMOTIONAL_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==2) %>%
      filter(!is.na(SCHRAG_EMOTIONAL_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_EMOTIONAL_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_EMOTIONAL_v2-Baseline)) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_EMOTIONAL_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_EMOTIONAL_v2) %>%
  rename("Baseline"="SCHRAG_EMOTIONAL_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==2) %>%
      filter(!is.na(SCHRAG_EMOTIONAL_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_EMOTIONAL_v2)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_EMOTIONAL_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_EMOTIONAL_v2) %>%
  rename("Baseline"="SCHRAG_EMOTIONAL_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==2) %>%
      filter(!is.na(SCHRAG_EMOTIONAL_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_EMOTIONAL_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_EMOTIONAL_v2-Baseline)/TIME_STUDY) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


# ALL SCHRAG Total Year 3

EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==3) %>%
  filter(!is.na(SCHRAG_EMOTIONAL_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(SCHRAG_EMOTIONAL_v2),
    sd=sd(SCHRAG_EMOTIONAL_v2),
    median=median(SCHRAG_EMOTIONAL_v2),
    Q1 = quantile(SCHRAG_EMOTIONAL_v2, 0.25),
    Q3 = quantile(SCHRAG_EMOTIONAL_v2, 0.75),
    min=min(SCHRAG_EMOTIONAL_v2),
    max=max(SCHRAG_EMOTIONAL_v2),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_EMOTIONAL_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_EMOTIONAL_v2) %>%
  rename("Baseline"="SCHRAG_EMOTIONAL_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==3) %>%
      filter(!is.na(SCHRAG_EMOTIONAL_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_EMOTIONAL_v2)
  ) %>% group_by(DIAGNIV) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(mean= mean((SCHRAG_EMOTIONAL_v2-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_EMOTIONAL_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_EMOTIONAL_v2) %>%
  rename("Baseline"="SCHRAG_EMOTIONAL_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==3) %>%
      filter(!is.na(SCHRAG_EMOTIONAL_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_EMOTIONAL_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_EMOTIONAL_v2-Baseline)) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% 
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_EMOTIONAL_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_EMOTIONAL_v2) %>%
  rename("Baseline"="SCHRAG_EMOTIONAL_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==3) %>%
      filter(!is.na(SCHRAG_EMOTIONAL_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_EMOTIONAL_v2)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_EMOTIONAL_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_EMOTIONAL_v2) %>%
  rename("Baseline"="SCHRAG_EMOTIONAL_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==3) %>%
      filter(!is.na(SCHRAG_EMOTIONAL_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_EMOTIONAL_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_EMOTIONAL_v2-Baseline)/TIME_STUDY) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
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


# Inputs VAS MSA QoL   ----------------------------------

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
 mutate(SCHRAG_MOTOR_v2/ ((14-motor_na)/14) ) %>%
  mutate(SCHRAG_NONMOTOR_v2/ ((14-non_motor_na)/14) ) %>%
  mutate(SCHRAG_EMOTIONAL_v2/ ((14-emotional_na)/14) ) %>%
  mutate(SCHRAG_TOT_v2=SCHRAG_MOTOR_v2+SCHRAG_NONMOTOR_v2+SCHRAG_EMOTIONAL_v2) 



# ----------------
# Overall Early CT  -------------------------------

# ALL SCHRAG Total Year 0 


EarlyCT_Pop_Baseline_319 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(ECHANQV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(ECHANQV),
    sd=sd(ECHANQV),
    median=median(ECHANQV),
    Q1 = quantile(ECHANQV, 0.25),
    Q3 = quantile(ECHANQV, 0.75),
    min=min(ECHANQV),
    max=max(ECHANQV),
    n=n()
  )



# ALL SCHRAG Total Year 1

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
  filter(!is.na(ECHANQV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(ECHANQV),
    sd=sd(ECHANQV),
    median=median(ECHANQV),
    Q1 = quantile(ECHANQV, 0.25),
    Q3 = quantile(ECHANQV, 0.75),
    min=min(ECHANQV),
    max=max(ECHANQV),
    n=n()
  )

# Change from baseline

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(ECHANQV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  ECHANQV) %>%
  rename("Baseline"="ECHANQV") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
      filter(!is.na(ECHANQV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, ECHANQV)
  ) %>%  inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(mean= mean((ECHANQV-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(ECHANQV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  ECHANQV) %>%
  rename("Baseline"="ECHANQV") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
      filter(!is.na(ECHANQV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, ECHANQV)
  ) %>% 
  mutate(Diff= (ECHANQV-Baseline)) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(ECHANQV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  ECHANQV) %>%
  rename("Baseline"="ECHANQV") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
      filter(!is.na(ECHANQV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, ECHANQV)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(ECHANQV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  ECHANQV) %>%
  rename("Baseline"="ECHANQV") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
      filter(!is.na(ECHANQV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, ECHANQV)
  ) %>% 
  mutate(Diff= (ECHANQV-Baseline)/TIME_STUDY) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



# ALL SCHRAG Total Year 2

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==2) %>%
  filter(!is.na(ECHANQV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(ECHANQV),
    sd=sd(ECHANQV),
    median=median(ECHANQV),
    Q1 = quantile(ECHANQV, 0.25),
    Q3 = quantile(ECHANQV, 0.75),
    min=min(ECHANQV),
    max=max(ECHANQV),
    n=n()
  )


# Change from baseline

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(ECHANQV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  ECHANQV) %>%
  rename("Baseline"="ECHANQV") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==2) %>%
      filter(!is.na(ECHANQV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, ECHANQV)
  ) %>%  inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(mean= mean((ECHANQV-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(ECHANQV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  ECHANQV) %>%
  rename("Baseline"="ECHANQV") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==2) %>%
      filter(!is.na(ECHANQV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, ECHANQV)
  ) %>% 
  mutate(Diff= (ECHANQV-Baseline)) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(ECHANQV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  ECHANQV) %>%
  rename("Baseline"="ECHANQV") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==2) %>%
      filter(!is.na(ECHANQV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, ECHANQV)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(ECHANQV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  ECHANQV) %>%
  rename("Baseline"="ECHANQV") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==2) %>%
      filter(!is.na(ECHANQV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, ECHANQV)
  ) %>% 
  mutate(Diff= (ECHANQV-Baseline)/TIME_STUDY) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


# ALL SCHRAG Total Year 3

EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==3) %>%
  filter(!is.na(ECHANQV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(ECHANQV),
    sd=sd(ECHANQV),
    median=median(ECHANQV),
    Q1 = quantile(ECHANQV, 0.25),
    Q3 = quantile(ECHANQV, 0.75),
    min=min(ECHANQV),
    max=max(ECHANQV),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(ECHANQV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  ECHANQV) %>%
  rename("Baseline"="ECHANQV") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==3) %>%
      filter(!is.na(ECHANQV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, ECHANQV)
  ) %>%  inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(mean= mean((ECHANQV-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(ECHANQV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  ECHANQV) %>%
  rename("Baseline"="ECHANQV") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==3) %>%
      filter(!is.na(ECHANQV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, ECHANQV)
  ) %>% 
  mutate(Diff= (ECHANQV-Baseline)) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(ECHANQV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  ECHANQV) %>%
  rename("Baseline"="ECHANQV") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==3) %>%
      filter(!is.na(ECHANQV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, ECHANQV)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(ECHANQV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  ECHANQV) %>%
  rename("Baseline"="ECHANQV") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==3) %>%
      filter(!is.na(ECHANQV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, ECHANQV)
  ) %>% 
  mutate(Diff= (ECHANQV-Baseline)/TIME_STUDY) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
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
# ALL SCHRAG Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(ECHANQV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(ECHANQV),
    sd=sd(ECHANQV),
    median=median(ECHANQV),
    Q1 = quantile(ECHANQV, 0.25),
    Q3 = quantile(ECHANQV, 0.75),
    min=min(ECHANQV),
    max=max(ECHANQV),
    n=n()
  )



# ALL SCHRAG Total Year 1

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
  filter(!is.na(ECHANQV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(ECHANQV),
    sd=sd(ECHANQV),
    median=median(ECHANQV),
    Q1 = quantile(ECHANQV, 0.25),
    Q3 = quantile(ECHANQV, 0.75),
    min=min(ECHANQV),
    max=max(ECHANQV),
    n=n()
  )

# Change from baseline

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(ECHANQV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  ECHANQV) %>%
  rename("Baseline"="ECHANQV") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
      filter(!is.na(ECHANQV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, ECHANQV)
  ) %>%  inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((ECHANQV-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(ECHANQV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  ECHANQV) %>%
  rename("Baseline"="ECHANQV") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
      filter(!is.na(ECHANQV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, ECHANQV)
  ) %>% 
  mutate(Diff= (ECHANQV-Baseline)) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAG) %>% 
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(ECHANQV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  ECHANQV) %>%
  rename("Baseline"="ECHANQV") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
      filter(!is.na(ECHANQV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, ECHANQV)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(ECHANQV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  ECHANQV) %>%
  rename("Baseline"="ECHANQV") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
      filter(!is.na(ECHANQV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, ECHANQV)
  ) %>% 
  mutate(Diff= (ECHANQV-Baseline)/TIME_STUDY) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



# ALL SCHRAG Total Year 2

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==2) %>%
  filter(!is.na(ECHANQV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(ECHANQV),
    sd=sd(ECHANQV),
    median=median(ECHANQV),
    Q1 = quantile(ECHANQV, 0.25),
    Q3 = quantile(ECHANQV, 0.75),
    min=min(ECHANQV),
    max=max(ECHANQV),
    n=n()
  )


# Change from baseline

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(ECHANQV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  ECHANQV) %>%
  rename("Baseline"="ECHANQV") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==2) %>%
      filter(!is.na(ECHANQV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, ECHANQV)
  ) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAG) %>% summarise(mean= mean((ECHANQV-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(ECHANQV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  ECHANQV) %>%
  rename("Baseline"="ECHANQV") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==2) %>%
      filter(!is.na(ECHANQV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, ECHANQV)
  ) %>% 
  mutate(Diff= (ECHANQV-Baseline)) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(ECHANQV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  ECHANQV) %>%
  rename("Baseline"="ECHANQV") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==2) %>%
      filter(!is.na(ECHANQV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, ECHANQV)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(ECHANQV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  ECHANQV) %>%
  rename("Baseline"="ECHANQV") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==2) %>%
      filter(!is.na(ECHANQV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, ECHANQV)
  ) %>% 
  mutate(Diff= (ECHANQV-Baseline)/TIME_STUDY) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


# ALL SCHRAG Total Year 3

EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==3) %>%
  filter(!is.na(ECHANQV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(ECHANQV),
    sd=sd(ECHANQV),
    median=median(ECHANQV),
    Q1 = quantile(ECHANQV, 0.25),
    Q3 = quantile(ECHANQV, 0.75),
    min=min(ECHANQV),
    max=max(ECHANQV),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(ECHANQV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  ECHANQV) %>%
  rename("Baseline"="ECHANQV") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==3) %>%
      filter(!is.na(ECHANQV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, ECHANQV)
  ) %>% group_by(DIAG) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(mean= mean((ECHANQV-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(ECHANQV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  ECHANQV) %>%
  rename("Baseline"="ECHANQV") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==3) %>%
      filter(!is.na(ECHANQV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, ECHANQV)
  ) %>% 
  mutate(Diff= (ECHANQV-Baseline)) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(ECHANQV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  ECHANQV) %>%
  rename("Baseline"="ECHANQV") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==3) %>%
      filter(!is.na(ECHANQV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, ECHANQV)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(ECHANQV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  ECHANQV) %>%
  rename("Baseline"="ECHANQV") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==3) %>%
      filter(!is.na(ECHANQV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, ECHANQV)
  ) %>% 
  mutate(Diff= (ECHANQV-Baseline)/TIME_STUDY) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAG) %>%
  summarise(
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

# ALL SCHRAG Total Year 0 

EarlyCT_Pop_Baseline_319 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(ECHANQV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(ECHANQV),
    sd=sd(ECHANQV),
    median=median(ECHANQV),
    Q1 = quantile(ECHANQV, 0.25),
    Q3 = quantile(ECHANQV, 0.75),
    min=min(ECHANQV),
    max=max(ECHANQV),
    n=n()
  )



# ALL SCHRAG Total Year 1

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
  filter(!is.na(ECHANQV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(ECHANQV),
    sd=sd(ECHANQV),
    median=median(ECHANQV),
    Q1 = quantile(ECHANQV, 0.25),
    Q3 = quantile(ECHANQV, 0.75),
    min=min(ECHANQV),
    max=max(ECHANQV),
    n=n()
  )

# Change from baseline


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(ECHANQV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  ECHANQV) %>%
  rename("Baseline"="ECHANQV") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
      filter(!is.na(ECHANQV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, ECHANQV)
  ) %>%  inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((ECHANQV-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(ECHANQV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  ECHANQV) %>%
  rename("Baseline"="ECHANQV") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
      filter(!is.na(ECHANQV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, ECHANQV)
  ) %>% 
  mutate(Diff= (ECHANQV-Baseline)) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )




EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(ECHANQV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  ECHANQV) %>%
  rename("Baseline"="ECHANQV") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
      filter(!is.na(ECHANQV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, ECHANQV)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )




EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(ECHANQV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  ECHANQV) %>%
  rename("Baseline"="ECHANQV") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
      filter(!is.na(ECHANQV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, ECHANQV)
  ) %>% 
  mutate(Diff= (ECHANQV-Baseline)/TIME_STUDY) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



# ALL SCHRAG Total Year 2

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==2) %>%
  filter(!is.na(ECHANQV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(ECHANQV),
    sd=sd(ECHANQV),
    median=median(ECHANQV),
    Q1 = quantile(ECHANQV, 0.25),
    Q3 = quantile(ECHANQV, 0.75),
    min=min(ECHANQV),
    max=max(ECHANQV),
    n=n()
  )


# Change from baseline

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(ECHANQV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  ECHANQV) %>%
  rename("Baseline"="ECHANQV") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==2) %>%
      filter(!is.na(ECHANQV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, ECHANQV)
  ) %>%  inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% summarise(mean= mean((ECHANQV-Baseline)/TIME_STUDY)) 



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(ECHANQV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  ECHANQV) %>%
  rename("Baseline"="ECHANQV") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==2) %>%
      filter(!is.na(ECHANQV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, ECHANQV)
  ) %>% 
  mutate(Diff= (ECHANQV-Baseline)) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(ECHANQV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  ECHANQV) %>%
  rename("Baseline"="ECHANQV") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==2) %>%
      filter(!is.na(ECHANQV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, ECHANQV)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(ECHANQV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  ECHANQV) %>%
  rename("Baseline"="ECHANQV") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==2) %>%
      filter(!is.na(ECHANQV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, ECHANQV)
  ) %>% 
  mutate(Diff= (ECHANQV-Baseline)/TIME_STUDY) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )


# ALL SCHRAG Total Year 3

EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==3) %>%
  filter(!is.na(ECHANQV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(
    mean=mean(ECHANQV),
    sd=sd(ECHANQV),
    median=median(ECHANQV),
    Q1 = quantile(ECHANQV, 0.25),
    Q3 = quantile(ECHANQV, 0.75),
    min=min(ECHANQV),
    max=max(ECHANQV),
    n=n()
  )


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(ECHANQV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  ECHANQV) %>%
  rename("Baseline"="ECHANQV") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==3) %>%
      filter(!is.na(ECHANQV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, ECHANQV)
  ) %>% group_by(DIAGNIV) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  summarise(mean= mean((ECHANQV-Baseline)/TIME_STUDY)) 


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(ECHANQV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  ECHANQV) %>%
  rename("Baseline"="ECHANQV") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==3) %>%
      filter(!is.na(ECHANQV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, ECHANQV)
  ) %>% 
  mutate(Diff= (ECHANQV-Baseline)) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAGNIV) %>% 
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(ECHANQV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  ECHANQV) %>%
  rename("Baseline"="ECHANQV") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==3) %>%
      filter(!is.na(ECHANQV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, ECHANQV)
  ) %>% 
  mutate(Diff= TIME_STUDY*12) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(Diff),
    sd=sd(Diff),
    median=median(Diff),
    Q1 = quantile(Diff, 0.25),
    Q3 = quantile(Diff, 0.75),
    min=min(Diff),
    max=max(Diff),
    n=n()
  )



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(ECHANQV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  ECHANQV) %>%
  rename("Baseline"="ECHANQV") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==3) %>%
      filter(!is.na(ECHANQV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, ECHANQV)
  ) %>% 
  mutate(Diff= (ECHANQV-Baseline)/TIME_STUDY) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  group_by(DIAGNIV) %>%
  summarise(
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



# Student T-tests for deltas in SCHRAG Prob vs Poss, MSA-C vs MSA-P ---------------------------------------
library(rstatix)

# Inputs SCHARG   ----------------------------------


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
 mutate(SCHRAG_MOTOR_v2/ ((14-motor_na)/14) ) %>%
  mutate(SCHRAG_NONMOTOR_v2/ ((14-non_motor_na)/14) ) %>%
  mutate(SCHRAG_EMOTIONAL_v2/ ((14-emotional_na)/14) ) %>%
  mutate(SCHRAG_TOT_v2=SCHRAG_MOTOR_v2+SCHRAG_NONMOTOR_v2+SCHRAG_EMOTIONAL_v2) 


# ----------
# SCHRAG_TOT_v2 -------------
# -----------
# Overall Early CT  -------------------------------

# ALL UMSARS Total Year 1


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==0) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  filter(!is.na(SCHRAG_TOT_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_TOT_v2) %>%
  rename("Baseline"="SCHRAG_TOT_v2") %>%
  left_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
      filter(!is.na(SCHRAG_TOT_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_TOT_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_TOT_v2-Baseline)/TIME_STUDY)  %>%
  t_test(Diff~DIAG, detailed = T,var.equal = TRUE) 
 #t_test(Diff~DIAGNIV, detailed = T, var.equal = TRUE) 




# ALL UMSARS Total Year 2


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==0) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  filter(!is.na(SCHRAG_TOT_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_TOT_v2) %>%
  rename("Baseline"="SCHRAG_TOT_v2") %>%
  left_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==2) %>%
      filter(!is.na(SCHRAG_TOT_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_TOT_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_TOT_v2-Baseline)/TIME_STUDY)  %>%
  t_test(Diff~DIAG, detailed = T,var.equal = TRUE) 
  #t_test(Diff~DIAGNIV,  detailed = T,var.equal = TRUE) 

# ALL UMSARS Total Year 3


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==0) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  filter(!is.na(SCHRAG_TOT_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_TOT_v2) %>%
  rename("Baseline"="SCHRAG_TOT_v2") %>%
  left_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==3) %>%
      filter(!is.na(SCHRAG_TOT_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_TOT_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_TOT_v2-Baseline)/TIME_STUDY)  %>%
  #t_test(Diff~DIAG, detailed = T,var.equal = TRUE) 
  t_test(Diff~DIAGNIV, detailed = T, var.equal = TRUE) 

# --------------


# SCHRAG_MOTOR_v2 -------------
# -----------
# Overall Early CT  -------------------------------

# ALL UMSARS Total Year 1


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==0) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  filter(!is.na(SCHRAG_MOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_MOTOR_v2) %>%
  rename("Baseline"="SCHRAG_MOTOR_v2") %>%
  left_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
      filter(!is.na(SCHRAG_MOTOR_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_MOTOR_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_MOTOR_v2-Baseline)/TIME_STUDY)  %>%
  t_test(Diff~DIAG, detailed = T, var.equal = TRUE) 
  #t_test(Diff~DIAGNIV, detailed = T, var.equal = TRUE) 




# ALL UMSARS Total Year 2


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==0) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  filter(!is.na(SCHRAG_MOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_MOTOR_v2) %>%
  rename("Baseline"="SCHRAG_MOTOR_v2") %>%
  left_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==2) %>%
      filter(!is.na(SCHRAG_MOTOR_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_MOTOR_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_MOTOR_v2-Baseline)/TIME_STUDY)  %>%
  t_test(Diff~DIAG, detailed = T, var.equal = TRUE) 
  #t_test(Diff~DIAGNIV,  detailed = T, var.equal = TRUE) 

# ALL UMSARS Total Year 3


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==0) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  filter(!is.na(SCHRAG_MOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_MOTOR_v2) %>%
  rename("Baseline"="SCHRAG_MOTOR_v2") %>%
  left_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==3) %>%
      filter(!is.na(SCHRAG_MOTOR_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_MOTOR_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_MOTOR_v2-Baseline)/TIME_STUDY)  %>%
  t_test(Diff~DIAG, detailed = T, var.equal = TRUE) 
  #t_test(Diff~DIAGNIV, detailed = T, var.equal = TRUE) 

# --------------


# SCHRAG_NONMOTOR_v2 -------------
# -----------
# Overall Early CT  -------------------------------

# ALL UMSARS Total Year 1


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==0) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  filter(!is.na(SCHRAG_NONMOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_NONMOTOR_v2) %>%
  rename("Baseline"="SCHRAG_NONMOTOR_v2") %>%
  left_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
      filter(!is.na(SCHRAG_NONMOTOR_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_NONMOTOR_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_NONMOTOR_v2-Baseline)/TIME_STUDY)  %>%
  #t_test(Diff~DIAG, detailed = T, var.equal = TRUE) 
  t_test(Diff~DIAGNIV,  detailed = T, var.equal = TRUE) 




# ALL UMSARS Total Year 2


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==0) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  filter(!is.na(SCHRAG_NONMOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_NONMOTOR_v2) %>%
  rename("Baseline"="SCHRAG_NONMOTOR_v2") %>%
  left_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==2) %>%
      filter(!is.na(SCHRAG_NONMOTOR_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_NONMOTOR_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_NONMOTOR_v2-Baseline)/TIME_STUDY)  %>%
  #t_test(Diff~DIAG, detailed = T, var.equal = TRUE) 
  t_test(Diff~DIAGNIV, detailed = T, var.equal = TRUE) 

# ALL UMSARS Total Year 3


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==0) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  filter(!is.na(SCHRAG_NONMOTOR_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_NONMOTOR_v2) %>%
  rename("Baseline"="SCHRAG_NONMOTOR_v2") %>%
  left_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==3) %>%
      filter(!is.na(SCHRAG_NONMOTOR_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_NONMOTOR_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_NONMOTOR_v2-Baseline)/TIME_STUDY)  %>%
  #t_test(Diff~DIAG, detailed = T, var.equal = TRUE) 
  t_test(Diff~DIAGNIV, detailed = T, var.equal = TRUE) 

# --------------


# SCHRAG_EMOTIONAL_v2 -------------
# -----------
# Overall Early CT  -------------------------------

# ALL UMSARS Total Year 1


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==0) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  filter(!is.na(SCHRAG_EMOTIONAL_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_EMOTIONAL_v2) %>%
  rename("Baseline"="SCHRAG_EMOTIONAL_v2") %>%
  left_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
      filter(!is.na(SCHRAG_EMOTIONAL_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_EMOTIONAL_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_EMOTIONAL_v2-Baseline)/TIME_STUDY)  %>%
   t_test(Diff~DIAG, detailed = T,var.equal = TRUE) 
  #t_test(Diff~DIAGNIV,  detailed = T,var.equal = TRUE) 




# ALL UMSARS Total Year 2


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==0) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  filter(!is.na(SCHRAG_EMOTIONAL_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_EMOTIONAL_v2) %>%
  rename("Baseline"="SCHRAG_EMOTIONAL_v2") %>%
  left_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==2) %>%
      filter(!is.na(SCHRAG_EMOTIONAL_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_EMOTIONAL_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_EMOTIONAL_v2-Baseline)/TIME_STUDY)  %>%
  t_test(Diff~DIAG, detailed = T,var.equal = TRUE) 
  #t_test(Diff~DIAGNIV,  detailed = T,var.equal = TRUE) 

# ALL UMSARS Total Year 3


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==0) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  filter(!is.na(SCHRAG_EMOTIONAL_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_EMOTIONAL_v2) %>%
  rename("Baseline"="SCHRAG_EMOTIONAL_v2") %>%
  left_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==3) %>%
      filter(!is.na(SCHRAG_EMOTIONAL_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_EMOTIONAL_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_EMOTIONAL_v2-Baseline)/TIME_STUDY)  %>%
  t_test(Diff~DIAG, detailed = T,var.equal = TRUE) 
  #t_test(Diff~DIAGNIV,detailed = T,  var.equal = TRUE) 

# --------------


# ECHANQV -------------
# -----------
# Overall Early CT  -------------------------------

# ALL UMSARS Total Year 1


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==0) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  filter(!is.na(ECHANQV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  ECHANQV) %>%
  rename("Baseline"="ECHANQV") %>%
  left_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
      filter(!is.na(ECHANQV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, ECHANQV)
  ) %>% 
  mutate(Diff= (ECHANQV-Baseline)/TIME_STUDY)  %>%
  t_test(Diff~DIAG,detailed = T, var.equal = TRUE) 
  #t_test(Diff~DIAGNIV,  detailed = T,var.equal = TRUE) 




# ALL UMSARS Total Year 2


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==0) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  filter(!is.na(ECHANQV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  ECHANQV) %>%
  rename("Baseline"="ECHANQV") %>%
  left_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==2) %>%
      filter(!is.na(ECHANQV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, ECHANQV)
  ) %>% 
  mutate(Diff= (ECHANQV-Baseline)/TIME_STUDY)  %>%
  #t_test(Diff~DIAG, detailed = T,var.equal = TRUE) 
  t_test(Diff~DIAGNIV, detailed = T, var.equal = TRUE) 

# ALL UMSARS Total Year 3


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==0) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  filter(!is.na(ECHANQV)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  ECHANQV) %>%
  rename("Baseline"="ECHANQV") %>%
  left_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==3) %>%
      filter(!is.na(ECHANQV)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, ECHANQV)
  ) %>% 
  mutate(Diff= (ECHANQV-Baseline)/TIME_STUDY)  %>%
  #t_test(Diff~DIAG, detailed = T,var.equal = TRUE) 
  t_test(Diff~DIAGNIV, detailed = T, var.equal = TRUE) 


# --------------


# Correlation between Delta SCHRAG & Delta FDA-modified UMSARS ------------------

# ------------
# Inputs SCHRAG  ----------------------------------


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
 mutate(SCHRAG_MOTOR_v2/ ((14-motor_na)/14) ) %>%
  mutate(SCHRAG_NONMOTOR_v2/ ((14-non_motor_na)/14) ) %>%
  mutate(SCHRAG_EMOTIONAL_v2/ ((14-emotional_na)/14) ) %>%
  mutate(SCHRAG_TOT_v2=SCHRAG_MOTOR_v2+SCHRAG_NONMOTOR_v2+SCHRAG_EMOTIONAL_v2) 




UMSARS1 <- dataCohorteManaged %>% 
  select(NUM, DATECONSULT, TIME_STUDY, Year, UMSARS1_1:UMSARS1_TOT)


UMSARS1$UMSARS1_TOT_v2 <- rowSums(UMSARS1[, 5:16], na.rm = TRUE)

UMSARS1$missing_na <- rowSums(is.na(UMSARS1[, 5:16]))

UMSARS1 <-  UMSARS1 %>%  select(NUM, TIME_STUDY, Year, UMSARS1_TOT, UMSARS1_11)

UMSARS1 <- UMSARS1 %>% mutate(UMSARS1_TOT_FDA=UMSARS1_TOT-UMSARS1_11)



# ----------------

# Overall Early CT  -------------------------------

# Year 1


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==0) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  filter(!is.na(SCHRAG_TOT_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_TOT_v2) %>%
  rename("Baseline"="SCHRAG_TOT_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
      filter(!is.na(SCHRAG_TOT_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_TOT_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_TOT_v2-Baseline)/TIME_STUDY)  %>% select(NUM, DIAG, DIAGNIV, Diff) %>% rename("SCHRAG_Diff"="Diff") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
      filter(!is.na(UMSARS1_TOT_FDA)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  UMSARS1_TOT_FDA) %>%
      rename("Baseline"="UMSARS1_TOT_FDA") %>%
      left_join(
        EarlyCT_Pop_BaselineYear1_208 %>% inner_join(UMSARS1) %>% filter(Year==1) %>%
          filter(!is.na(UMSARS1_TOT_FDA)) %>%
          mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
          group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
          group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
          group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, UMSARS1_TOT_FDA)
      ) %>% 
      mutate(Diff= (UMSARS1_TOT_FDA-Baseline)/TIME_STUDY)  %>% select(NUM, DIAG, DIAGNIV, Diff)
  )  %>% summarise(cor=cor(SCHRAG_Diff  , Diff, method="spearman"))
  #group_by(DIAG) %>% summarise(cor=cor(SCHRAG_Diff  , Diff, method="spearman"))
  #group_by(DIAGNIV) %>% summarise(cor=cor(SCHRAG_Diff  , Diff, method="spearman"))





# Year 2


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==0) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  filter(!is.na(SCHRAG_TOT_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_TOT_v2) %>%
  rename("Baseline"="SCHRAG_TOT_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==2) %>%
      filter(!is.na(SCHRAG_TOT_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_TOT_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_TOT_v2-Baseline)/TIME_STUDY)  %>% select(NUM, DIAG, DIAGNIV, Diff) %>% rename("SCHRAG_Diff"="Diff") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
      filter(!is.na(UMSARS1_TOT_FDA)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  UMSARS1_TOT_FDA) %>%
      rename("Baseline"="UMSARS1_TOT_FDA") %>%
      left_join(
        EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(UMSARS1) %>% filter(Year==2) %>%
          filter(!is.na(UMSARS1_TOT_FDA)) %>%
          mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
          group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
          group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
          group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, UMSARS1_TOT_FDA)
      ) %>% 
      mutate(Diff= (UMSARS1_TOT_FDA-Baseline)/TIME_STUDY)   %>% select(NUM, DIAG, DIAGNIV, Diff)
  )  %>% # summarise(cor=cor(SCHRAG_Diff  , Diff, method="spearman"))
  #group_by(DIAG) %>% summarise(cor=cor(SCHRAG_Diff  , Diff, method="spearman"))
  group_by(DIAGNIV) %>% summarise(cor=cor(SCHRAG_Diff  , Diff, method="spearman"))


# Year 3


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==0) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  filter(!is.na(SCHRAG_TOT_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_TOT_v2) %>%
  rename("Baseline"="SCHRAG_TOT_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==3) %>%
      filter(!is.na(SCHRAG_TOT_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_TOT_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_TOT_v2-Baseline)/TIME_STUDY)  %>% select(NUM, DIAG, DIAGNIV, Diff) %>% rename("SCHRAG_Diff"="Diff") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
      filter(!is.na(UMSARS1_TOT_FDA)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  UMSARS1_TOT_FDA) %>%
      rename("Baseline"="UMSARS1_TOT_FDA") %>%
      left_join(
        EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(UMSARS1) %>% filter(Year==3) %>%
          filter(!is.na(UMSARS1_TOT_FDA)) %>%
          mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
          group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
          group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
          group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, UMSARS1_TOT_FDA)
      ) %>% 
      mutate(Diff= (UMSARS1_TOT_FDA-Baseline)/TIME_STUDY) %>% select(NUM, DIAG, DIAGNIV, Diff)
  )  %>%  #summarise(cor=cor(SCHRAG_Diff  , Diff, method="spearman"))
#group_by(DIAG) %>% summarise(cor=cor(SCHRAG_Diff  , Diff, method="spearman"))
group_by(DIAGNIV) %>% summarise(cor=cor(SCHRAG_Diff  , Diff, method="spearman"))


# --------------



# Inputs SCHRAG Motor ----------------------------------


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
 mutate(SCHRAG_MOTOR_v2/ ((14-motor_na)/14) ) %>%
  mutate(SCHRAG_NONMOTOR_v2/ ((14-non_motor_na)/14) ) %>%
  mutate(SCHRAG_EMOTIONAL_v2/ ((14-emotional_na)/14) ) %>%
  mutate(SCHRAG_TOT_v2=SCHRAG_MOTOR_v2+SCHRAG_NONMOTOR_v2+SCHRAG_EMOTIONAL_v2) 




UMSARS1 <- dataCohorteManaged %>% 
  select(NUM, DATECONSULT, TIME_STUDY, Year, UMSARS1_1:UMSARS1_TOT)


UMSARS1$UMSARS1_TOT_v2 <- rowSums(UMSARS1[, 5:16], na.rm = TRUE)

UMSARS1$missing_na <- rowSums(is.na(UMSARS1[, 5:16]))

UMSARS1 <-  UMSARS1 %>%  select(NUM, TIME_STUDY, Year, UMSARS1_TOT, UMSARS1_11)

UMSARS1 <- UMSARS1 %>% mutate(UMSARS1_TOT_FDA=UMSARS1_TOT-UMSARS1_11)



# ----------------

# Overall Early CT  -------------------------------

# Year 1


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==0) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  filter(!is.na(SCHRAG_TOT_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_MOTOR_v2) %>%
  rename("Baseline"="SCHRAG_MOTOR_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
      filter(!is.na(SCHRAG_TOT_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_MOTOR_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_MOTOR_v2-Baseline)/TIME_STUDY)  %>% select(NUM, DIAG, DIAGNIV, Diff) %>% rename("SCHRAG_Diff"="Diff") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
      filter(!is.na(UMSARS1_TOT_FDA)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  UMSARS1_TOT_FDA) %>%
      rename("Baseline"="UMSARS1_TOT_FDA") %>%
      left_join(
        EarlyCT_Pop_BaselineYear1_208 %>% inner_join(UMSARS1) %>% filter(Year==1) %>%
          filter(!is.na(UMSARS1_TOT_FDA)) %>%
          mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
          group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
          group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
          group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, UMSARS1_TOT_FDA)
      ) %>% 
      mutate(Diff= (UMSARS1_TOT_FDA-Baseline)/TIME_STUDY)  %>% select(NUM, DIAG, DIAGNIV, Diff)
  )  %>% # summarise(cor=cor(SCHRAG_Diff  , Diff, method="spearman"))
  #group_by(DIAG) %>% summarise(cor=cor(SCHRAG_Diff  , Diff, method="spearman"))
 group_by(DIAGNIV) %>% summarise(cor=cor(SCHRAG_Diff  , Diff, method="spearman"))





# Year 2


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==0) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  filter(!is.na(SCHRAG_TOT_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_MOTOR_v2) %>%
  rename("Baseline"="SCHRAG_MOTOR_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==2) %>%
      filter(!is.na(SCHRAG_TOT_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_MOTOR_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_MOTOR_v2-Baseline)/TIME_STUDY)  %>% select(NUM, DIAG, DIAGNIV, Diff) %>% rename("SCHRAG_Diff"="Diff") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
      filter(!is.na(UMSARS1_TOT_FDA)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  UMSARS1_TOT_FDA) %>%
      rename("Baseline"="UMSARS1_TOT_FDA") %>%
      left_join(
        EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(UMSARS1) %>% filter(Year==2) %>%
          filter(!is.na(UMSARS1_TOT_FDA)) %>%
          mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
          group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
          group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
          group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, UMSARS1_TOT_FDA)
      ) %>% 
      mutate(Diff= (UMSARS1_TOT_FDA-Baseline)/TIME_STUDY)   %>% select(NUM, DIAG, DIAGNIV, Diff)
  )  %>%  summarise(cor=cor(SCHRAG_Diff  , Diff, method="spearman"))
  # group_by(DIAG) %>% summarise(cor=cor(SCHRAG_Diff  , Diff, method="spearman"))
  #group_by(DIAGNIV) %>% summarise(cor=cor(SCHRAG_Diff  , Diff, method="spearman"))


# Year 3


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==0) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  filter(!is.na(SCHRAG_TOT_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_MOTOR_v2) %>%
  rename("Baseline"="SCHRAG_MOTOR_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==3) %>%
      filter(!is.na(SCHRAG_TOT_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_MOTOR_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_MOTOR_v2-Baseline)/TIME_STUDY)  %>% select(NUM, DIAG, DIAGNIV, Diff) %>% rename("SCHRAG_Diff"="Diff") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
      filter(!is.na(UMSARS1_TOT_FDA)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  UMSARS1_TOT_FDA) %>%
      rename("Baseline"="UMSARS1_TOT_FDA") %>%
      left_join(
        EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(UMSARS1) %>% filter(Year==3) %>%
          filter(!is.na(UMSARS1_TOT_FDA)) %>%
          mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
          group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
          group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
          group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, UMSARS1_TOT_FDA)
      ) %>% 
      mutate(Diff= (UMSARS1_TOT_FDA-Baseline)/TIME_STUDY) %>% select(NUM, DIAG, DIAGNIV, Diff)
  )  %>% #  summarise(cor=cor(SCHRAG_Diff  , Diff, method="spearman"))
#group_by(DIAG) %>% summarise(cor=cor(SCHRAG_Diff  , Diff, method="spearman"))
group_by(DIAGNIV) %>% summarise(cor=cor(SCHRAG_Diff  , Diff, method="spearman"))


# --------------



# Inputs SCHRAG non-Motor ----------------------------------


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
 mutate(SCHRAG_MOTOR_v2/ ((14-motor_na)/14) ) %>%
  mutate(SCHRAG_NONMOTOR_v2/ ((14-non_motor_na)/14) ) %>%
  mutate(SCHRAG_EMOTIONAL_v2/ ((14-emotional_na)/14) ) %>%
  mutate(SCHRAG_TOT_v2=SCHRAG_MOTOR_v2+SCHRAG_NONMOTOR_v2+SCHRAG_EMOTIONAL_v2) 





UMSARS1 <- dataCohorteManaged %>% 
  select(NUM, DATECONSULT, TIME_STUDY, Year, UMSARS1_1:UMSARS1_TOT)


UMSARS1$UMSARS1_TOT_v2 <- rowSums(UMSARS1[, 5:16], na.rm = TRUE)

UMSARS1$missing_na <- rowSums(is.na(UMSARS1[, 5:16]))

UMSARS1 <-  UMSARS1 %>%  select(NUM, TIME_STUDY, Year, UMSARS1_TOT, UMSARS1_11)

UMSARS1 <- UMSARS1 %>% mutate(UMSARS1_TOT_FDA=UMSARS1_TOT-UMSARS1_11)



# ----------------

# Overall Early CT  -------------------------------

# Year 1


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==0) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  filter(!is.na(SCHRAG_TOT_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_NONMOTOR_v2) %>%
  rename("Baseline"="SCHRAG_NONMOTOR_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
      filter(!is.na(SCHRAG_TOT_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_NONMOTOR_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_NONMOTOR_v2-Baseline)/TIME_STUDY)  %>% select(NUM, DIAG, DIAGNIV, Diff) %>% rename("SCHRAG_Diff"="Diff") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
      filter(!is.na(UMSARS1_TOT_FDA)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  UMSARS1_TOT_FDA) %>%
      rename("Baseline"="UMSARS1_TOT_FDA") %>%
      left_join(
        EarlyCT_Pop_BaselineYear1_208 %>% inner_join(UMSARS1) %>% filter(Year==1) %>%
          filter(!is.na(UMSARS1_TOT_FDA)) %>%
          mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
          group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
          group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
          group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, UMSARS1_TOT_FDA)
      ) %>% 
      mutate(Diff= (UMSARS1_TOT_FDA-Baseline)/TIME_STUDY)  %>% select(NUM, DIAG, DIAGNIV, Diff)
  )  %>% # summarise(cor=cor(SCHRAG_Diff  , Diff, method="spearman"))
#group_by(DIAG) %>% summarise(cor=cor(SCHRAG_Diff  , Diff, method="spearman"))
group_by(DIAGNIV) %>% summarise(cor=cor(SCHRAG_Diff  , Diff, method="spearman"))





# Year 2


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==0) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  filter(!is.na(SCHRAG_TOT_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_NONMOTOR_v2) %>%
  rename("Baseline"="SCHRAG_NONMOTOR_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==2) %>%
      filter(!is.na(SCHRAG_TOT_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_NONMOTOR_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_NONMOTOR_v2-Baseline)/TIME_STUDY)  %>% select(NUM, DIAG, DIAGNIV, Diff) %>% rename("SCHRAG_Diff"="Diff") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
      filter(!is.na(UMSARS1_TOT_FDA)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  UMSARS1_TOT_FDA) %>%
      rename("Baseline"="UMSARS1_TOT_FDA") %>%
      left_join(
        EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(UMSARS1) %>% filter(Year==2) %>%
          filter(!is.na(UMSARS1_TOT_FDA)) %>%
          mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
          group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
          group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
          group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, UMSARS1_TOT_FDA)
      ) %>% 
      mutate(Diff= (UMSARS1_TOT_FDA-Baseline)/TIME_STUDY)   %>% select(NUM, DIAG, DIAGNIV, Diff)
  )  %>% # summarise(cor=cor(SCHRAG_Diff  , Diff, method="spearman"))
#group_by(DIAG) %>% summarise(cor=cor(SCHRAG_Diff  , Diff, method="spearman"))
group_by(DIAGNIV) %>% summarise(cor=cor(SCHRAG_Diff  , Diff, method="spearman"))


# Year 3


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==0) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  filter(!is.na(SCHRAG_TOT_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_NONMOTOR_v2) %>%
  rename("Baseline"="SCHRAG_NONMOTOR_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==3) %>%
      filter(!is.na(SCHRAG_TOT_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_NONMOTOR_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_NONMOTOR_v2-Baseline)/TIME_STUDY)  %>% select(NUM, DIAG, DIAGNIV, Diff) %>% rename("SCHRAG_Diff"="Diff") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
      filter(!is.na(UMSARS1_TOT_FDA)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  UMSARS1_TOT_FDA) %>%
      rename("Baseline"="UMSARS1_TOT_FDA") %>%
      left_join(
        EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(UMSARS1) %>% filter(Year==3) %>%
          filter(!is.na(UMSARS1_TOT_FDA)) %>%
          mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
          group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
          group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
          group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, UMSARS1_TOT_FDA)
      ) %>% 
      mutate(Diff= (UMSARS1_TOT_FDA-Baseline)/TIME_STUDY) %>% select(NUM, DIAG, DIAGNIV, Diff)
  )  %>%   summarise(cor=cor(SCHRAG_Diff  , Diff, method="spearman"))
  #group_by(DIAG) %>% summarise(cor=cor(SCHRAG_Diff  , Diff, method="spearman"))
  #group_by(DIAGNIV) %>% summarise(cor=cor(SCHRAG_Diff  , Diff, method="spearman"))


# --------------



# Inputs SCHRAG Emotional ----------------------------------


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
 mutate(SCHRAG_MOTOR_v2/ ((14-motor_na)/14) ) %>%
  mutate(SCHRAG_NONMOTOR_v2/ ((14-non_motor_na)/14) ) %>%
  mutate(SCHRAG_EMOTIONAL_v2/ ((14-emotional_na)/14) ) %>%
  mutate(SCHRAG_TOT_v2=SCHRAG_MOTOR_v2+SCHRAG_NONMOTOR_v2+SCHRAG_EMOTIONAL_v2) 




UMSARS1 <- dataCohorteManaged %>% 
  select(NUM, DATECONSULT, TIME_STUDY, Year, UMSARS1_1:UMSARS1_TOT)


UMSARS1$UMSARS1_TOT_v2 <- rowSums(UMSARS1[, 5:16], na.rm = TRUE)

UMSARS1$missing_na <- rowSums(is.na(UMSARS1[, 5:16]))

UMSARS1 <-  UMSARS1 %>%  select(NUM, TIME_STUDY, Year, UMSARS1_TOT, UMSARS1_11)

UMSARS1 <- UMSARS1 %>% mutate(UMSARS1_TOT_FDA=UMSARS1_TOT-UMSARS1_11)



# ----------------

# Overall Early CT  -------------------------------

# Year 1


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==0) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  filter(!is.na(SCHRAG_TOT_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_EMOTIONAL_v2) %>%
  rename("Baseline"="SCHRAG_EMOTIONAL_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
      filter(!is.na(SCHRAG_TOT_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_EMOTIONAL_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_EMOTIONAL_v2-Baseline)/TIME_STUDY)  %>% select(NUM, DIAG, DIAGNIV, Diff) %>% rename("SCHRAG_Diff"="Diff") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
      filter(!is.na(UMSARS1_TOT_FDA)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  UMSARS1_TOT_FDA) %>%
      rename("Baseline"="UMSARS1_TOT_FDA") %>%
      left_join(
        EarlyCT_Pop_BaselineYear1_208 %>% inner_join(UMSARS1) %>% filter(Year==1) %>%
          filter(!is.na(UMSARS1_TOT_FDA)) %>%
          mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
          group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
          group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
          group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, UMSARS1_TOT_FDA)
      ) %>% 
      mutate(Diff= (UMSARS1_TOT_FDA-Baseline)/TIME_STUDY)  %>% select(NUM, DIAG, DIAGNIV, Diff)
  )  %>%  summarise(cor=cor(SCHRAG_Diff  , Diff, method="spearman"))
  #group_by(DIAG) %>% summarise(cor=cor(SCHRAG_Diff  , Diff, method="spearman"))
  #group_by(DIAGNIV) %>% summarise(cor=cor(SCHRAG_Diff  , Diff, method="spearman"))





# Year 2


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==0) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  filter(!is.na(SCHRAG_TOT_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_EMOTIONAL_v2) %>%
  rename("Baseline"="SCHRAG_EMOTIONAL_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==2) %>%
      filter(!is.na(SCHRAG_TOT_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_EMOTIONAL_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_EMOTIONAL_v2-Baseline)/TIME_STUDY)  %>% select(NUM, DIAG, DIAGNIV, Diff) %>% rename("SCHRAG_Diff"="Diff") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
      filter(!is.na(UMSARS1_TOT_FDA)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  UMSARS1_TOT_FDA) %>%
      rename("Baseline"="UMSARS1_TOT_FDA") %>%
      left_join(
        EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(UMSARS1) %>% filter(Year==2) %>%
          filter(!is.na(UMSARS1_TOT_FDA)) %>%
          mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
          group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
          group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
          group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, UMSARS1_TOT_FDA)
      ) %>% 
      mutate(Diff= (UMSARS1_TOT_FDA-Baseline)/TIME_STUDY)   %>% select(NUM, DIAG, DIAGNIV, Diff)
  )  %>% summarise(cor=cor(SCHRAG_Diff  , Diff, method="spearman"))
  #group_by(DIAG) %>% summarise(cor=cor(SCHRAG_Diff  , Diff, method="spearman"))
  #group_by(DIAGNIV) %>% summarise(cor=cor(SCHRAG_Diff  , Diff, method="spearman"))


# Year 3


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==0) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  filter(!is.na(SCHRAG_TOT_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCHRAG_EMOTIONAL_v2) %>%
  rename("Baseline"="SCHRAG_EMOTIONAL_v2") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==3) %>%
      filter(!is.na(SCHRAG_TOT_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCHRAG_EMOTIONAL_v2)
  ) %>% 
  mutate(Diff= (SCHRAG_EMOTIONAL_v2-Baseline)/TIME_STUDY)  %>% select(NUM, DIAG, DIAGNIV, Diff) %>% rename("SCHRAG_Diff"="Diff") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
      filter(!is.na(UMSARS1_TOT_FDA)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  UMSARS1_TOT_FDA) %>%
      rename("Baseline"="UMSARS1_TOT_FDA") %>%
      left_join(
        EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(UMSARS1) %>% filter(Year==3) %>%
          filter(!is.na(UMSARS1_TOT_FDA)) %>%
          mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
          group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
          group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
          group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, UMSARS1_TOT_FDA)
      ) %>% 
      mutate(Diff= (UMSARS1_TOT_FDA-Baseline)/TIME_STUDY) %>% select(NUM, DIAG, DIAGNIV, Diff)
  )  %>%  # summarise(cor=cor(SCHRAG_Diff  , Diff, method="spearman"))
#group_by(DIAG) %>% summarise(cor=cor(SCHRAG_Diff  , Diff, method="spearman"))
#group_by(DIAGNIV) %>% summarise(cor=cor(SCHRAG_Diff  , Diff, method="spearman"))


# --------------



# Inputs SCHRAG VAS ----------------------------------


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
 mutate(SCHRAG_MOTOR_v2/ ((14-motor_na)/14) ) %>%
  mutate(SCHRAG_NONMOTOR_v2/ ((14-non_motor_na)/14) ) %>%
  mutate(SCHRAG_EMOTIONAL_v2/ ((14-emotional_na)/14) ) %>%
  mutate(SCHRAG_TOT_v2=SCHRAG_MOTOR_v2+SCHRAG_NONMOTOR_v2+SCHRAG_EMOTIONAL_v2) 



UMSARS1 <- dataCohorteManaged %>% 
  select(NUM, DATECONSULT, TIME_STUDY, Year, UMSARS1_1:UMSARS1_TOT)


UMSARS1$UMSARS1_TOT_v2 <- rowSums(UMSARS1[, 5:16], na.rm = TRUE)

UMSARS1$missing_na <- rowSums(is.na(UMSARS1[, 5:16]))

UMSARS1 <-  UMSARS1 %>%  select(NUM, TIME_STUDY, Year, UMSARS1_TOT, UMSARS1_11)

UMSARS1 <- UMSARS1 %>% mutate(UMSARS1_TOT_FDA=UMSARS1_TOT-UMSARS1_11)



# ----------------

# Overall Early CT  -------------------------------

# Year 1


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==0) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  filter(!is.na(SCHRAG_TOT_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  ECHANQV) %>%
  rename("Baseline"="ECHANQV") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
      filter(!is.na(SCHRAG_TOT_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, ECHANQV)
  ) %>% 
  mutate(Diff= (ECHANQV-Baseline)/TIME_STUDY)  %>% select(NUM, DIAG, DIAGNIV, Diff) %>% rename("SCHRAG_Diff"="Diff") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
      filter(!is.na(UMSARS1_TOT_FDA)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  UMSARS1_TOT_FDA) %>%
      rename("Baseline"="UMSARS1_TOT_FDA") %>%
      left_join(
        EarlyCT_Pop_BaselineYear1_208 %>% inner_join(UMSARS1) %>% filter(Year==1) %>%
          filter(!is.na(UMSARS1_TOT_FDA)) %>%
          mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
          group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
          group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
          group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, UMSARS1_TOT_FDA)
      ) %>% 
      mutate(Diff= (UMSARS1_TOT_FDA-Baseline)/TIME_STUDY)  %>% select(NUM, DIAG, DIAGNIV, Diff)
  )  %>% drop_na() %>%  # summarise(cor=cor(SCHRAG_Diff  , Diff, method="spearman"))
#group_by(DIAG) %>% summarise(cor=cor(SCHRAG_Diff  , Diff, method="spearman"))
group_by(DIAGNIV) %>% summarise(cor=cor(SCHRAG_Diff  , Diff, method="spearman"))





# Year 2


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==0) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  filter(!is.na(SCHRAG_TOT_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  ECHANQV) %>%
  rename("Baseline"="ECHANQV") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==2) %>%
      filter(!is.na(SCHRAG_TOT_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, ECHANQV)
  ) %>% 
  mutate(Diff= (ECHANQV-Baseline)/TIME_STUDY)  %>% select(NUM, DIAG, DIAGNIV, Diff) %>% rename("SCHRAG_Diff"="Diff") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
      filter(!is.na(UMSARS1_TOT_FDA)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  UMSARS1_TOT_FDA) %>%
      rename("Baseline"="UMSARS1_TOT_FDA") %>%
      left_join(
        EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(UMSARS1) %>% filter(Year==2) %>%
          filter(!is.na(UMSARS1_TOT_FDA)) %>%
          mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
          group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
          group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
          group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, UMSARS1_TOT_FDA)
      ) %>% 
      mutate(Diff= (UMSARS1_TOT_FDA-Baseline)/TIME_STUDY)   %>% select(NUM, DIAG, DIAGNIV, Diff)
  )  %>% drop_na() %>% # summarise(cor=cor(SCHRAG_Diff  , Diff, method="spearman"))
#group_by(DIAG) %>% summarise(cor=cor(SCHRAG_Diff  , Diff, method="spearman"))
group_by(DIAGNIV) %>% summarise(cor=cor(SCHRAG_Diff  , Diff, method="spearman"))


# Year 3


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==0) %>% inner_join(SCHRAG_Baseline_Pats) %>%
  filter(!is.na(SCHRAG_TOT_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  ECHANQV) %>%
  rename("Baseline"="ECHANQV") %>%
  left_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==3) %>%
      filter(!is.na(SCHRAG_TOT_v2)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, ECHANQV)
  ) %>% 
  mutate(Diff= (ECHANQV-Baseline)/TIME_STUDY)  %>% select(NUM, DIAG, DIAGNIV, Diff) %>% rename("SCHRAG_Diff"="Diff") %>%
  inner_join(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
      filter(!is.na(UMSARS1_TOT_FDA)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  UMSARS1_TOT_FDA) %>%
      rename("Baseline"="UMSARS1_TOT_FDA") %>%
      left_join(
        EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(UMSARS1) %>% filter(Year==3) %>%
          filter(!is.na(UMSARS1_TOT_FDA)) %>%
          mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
          group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
          group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
          group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, UMSARS1_TOT_FDA)
      ) %>% 
      mutate(Diff= (UMSARS1_TOT_FDA-Baseline)/TIME_STUDY) %>% select(NUM, DIAG, DIAGNIV, Diff)
  )  %>%  drop_na() %>%   summarise(cor=cor(SCHRAG_Diff  , Diff, method="spearman"))
  #group_by(DIAG) %>% summarise(cor=cor(SCHRAG_Diff  , Diff, method="spearman"))
  #group_by(DIAGNIV) %>% summarise(cor=cor(SCHRAG_Diff  , Diff, method="spearman"))


# --------------



# ---------------------





















# # Inputs SCOPA AUT V2 ----------------------------------
# 
# 
# AllMSA_Pop_Baseline_671 <- fread("Source/AllMSA_Pop_Baseline_671.txt")
# AllMSA_Pop_BaselineYear1_410 <- fread("Source/AllMSA_Pop_BaselineYear1_410.txt")
# AllMSA_Pop_BaselineYear1Year2_245 <- fread("Source/AllMSA_Pop_BaselineYear1Year2_245.txt")
# AllMSA_Pop_BaselineYear1Year2Year3Plus_158 <- fread("Source/AllMSA_Pop_BaselineYear1Year2Year3Plus_158.txt")
# 
# EarlyCT_Pop_Baseline_319 <- fread("Source/EarlyCT_Pop_Baseline_319.txt")
# EarlyCT_Pop_BaselineYear1_208 <- fread("Source/EarlyCT_Pop_BaselineYear1_208.txt")
# EarlyCT_Pop_BaselineYear1Year2_134 <- fread("Source/EarlyCT_Pop_BaselineYear1Year2_134.txt")
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 <- fread( "Source/EarlyCT_Pop_BaselineYear1Year2Year3Plus_99.txt")
# 
# dataCohorteManaged <- readRDS("Source/dataCohorteManaged.rds")
# 
# dataCohorteManaged <- dataCohorteManaged %>% group_by(NUM) %>% mutate(TIME_STUDY = ifelse( is.na(TIME_STUDY), 0, TIME_STUDY)) %>%
#   mutate(Year= ifelse(TIME_STUDY==0, 0,
#                       ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
#                              ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
#                                     ifelse(TIME_STUDY>=2.5 ,3, NA)))))
# 
# 
# SCOPA <- dataCohorteManaged %>%
#   select(NUM, DATECONSULT, TIME_STUDY, Year) %>% rename("DATE"="DATECONSULT")
# 
# 
# 
# 
# ams_scopa_bx_2015_16 <- haven::read_sas("Source/ams_scopa_bx_2015_16.sas7bdat")
# names(ams_scopa_bx_2015_16)
# ams_scopa_bx_2015_16 <- ams_scopa_bx_2015_16 %>% filter(!is.na(DATE))
# 
# 
# 
# 
# mean(ams_scopa_bx_2015_16$SCOPA_TOT, na.rm=T)
# 
# ams_scopa_bx_2015_16 <- ams_scopa_bx_2015_16 %>%
#     mutate(SCOPA_DIG = rowSums(select(., SCOPA1,SCOPA2,SCOPA3,SCOPA4,SCOPA5,SCOPA6,SCOPA7), na.rm = TRUE)) %>%
#     mutate(SCOPA_URI = rowSums(select(., SCOPA8,SCOPA9,SCOPA10,SCOPA11,SCOPA12,SCOPA13), na.rm = TRUE)) %>%
#     mutate(SCOPA_CDV = rowSums(select(., SCOPA14,SCOPA15,SCOPA16), na.rm = TRUE)) %>%
#     mutate(SCOPA_THE = rowSums(select(., SCOPA17,SCOPA18,SCOPA19,SCOPA20), na.rm = TRUE)) %>%
#     mutate(SCOPA_PUP = rowSums(select(., SCOPA21), na.rm = TRUE)) %>%
#     mutate(SCOPA_SEX = rowSums(select(., SCOPA22H,SCOPA22F,SCOPA23F,SCOPA23H), na.rm = TRUE)) %>%
#     mutate(SCOPA_TOT = rowSums(select(., SCOPA_SEX,SCOPA_PUP,SCOPA_THE,SCOPA_CDV,SCOPA_URI,SCOPA_DIG), na.rm = TRUE))
# 
# 
# columns_to_check <- c("SCOPA1", "SCOPA2", "SCOPA3",
#                       "SCOPA4", "SCOPA5", "SCOPA6",
#                       "SCOPA7", "SCOPA8", "SCOPA9",
#                       "SCOPA10", "SCOPA11", "SCOPA12",
#                       "SCOPA13", "SCOPA14", "SCOPA15",
#                       "SCOPA16", "SCOPA17", "SCOPA18",
#                       "SCOPA19", "SCOPA20", "SCOPA21",
#                       "SCOPA22H", "SCOPA22F", "SCOPA23F", "SCOPA23H"
#                       )
# 
# ams_scopa_bx_2015_16 <- ams_scopa_bx_2015_16 %>%
#   mutate(
#     number_na = rowSums(is.na(select(., all_of(columns_to_check))))
#   )
# 
# ams_scopa_bx_2015_16 <- ams_scopa_bx_2015_16 %>%
#   mutate(number_na=number_na-2) %>%
#   mutate(number_na=ifelse(number_na<0,0,number_na))
# 
# 
# names(ams_scopa_bx_2015_16)
# 
# ams_scopa_bx_2015_16$dig_na <- rowSums(is.na(ams_scopa_bx_2015_16[, 3:9])) # 7
# ams_scopa_bx_2015_16$uri_na <- rowSums(is.na(ams_scopa_bx_2015_16[, 11:16])) # 6
# ams_scopa_bx_2015_16$cdv_na <- rowSums(is.na(ams_scopa_bx_2015_16[, 18:20])) # 3
# ams_scopa_bx_2015_16$the_na <- rowSums(is.na(ams_scopa_bx_2015_16[, 22:25])) # 4
# ams_scopa_bx_2015_16$pup_na <- rowSums(is.na(ams_scopa_bx_2015_16[, 27:27])) # 1
# ams_scopa_bx_2015_16$sex_na <- rowSums(is.na(ams_scopa_bx_2015_16[, 29:32])) # 2
# 
# ams_scopa_bx_2015_16 <- ams_scopa_bx_2015_16 %>%
#   mutate(sex_na=sex_na-2) %>%
#   mutate(sex_na=ifelse(sex_na<0,0,sex_na))
# 
# 
# 
# range(ams_scopa_bx_2015_16$dig_na)
# range(ams_scopa_bx_2015_16$uri_na)
# range(ams_scopa_bx_2015_16$cdv_na)
# range(ams_scopa_bx_2015_16$the_na)
# range(ams_scopa_bx_2015_16$pup_na)
# range(ams_scopa_bx_2015_16$sex_na)
# 
# ams_scopa_bx_2015_16 <- ams_scopa_bx_2015_16 %>%
#   select(NUM, DATE, SCOPA_TOT, SCOPA_SEX, SCOPA_PUP, SCOPA_THE, SCOPA_CDV, SCOPA_URI, SCOPA_DIG,
#          dig_na, uri_na, cdv_na, the_na, pup_na, sex_na)
# 
# 
# 
# 
# ams_scopa_bx_2015_16 <- ams_scopa_bx_2015_16 %>%  ungroup() %>%
#  mutate(SCOPA_DIG = SCOPA_DIG / ((7-dig_na)/7) ) %>%
#   mutate(SCOPA_URI = SCOPA_URI / ((6-uri_na)/6) ) %>%
#   mutate(SCOPA_CDV = SCOPA_CDV / ((3-cdv_na)/3) ) %>%
#   mutate(SCOPA_THE = SCOPA_THE / ((4-the_na)/4) ) %>%
#   mutate(SCOPA_PUP = SCOPA_PUP/ ((1-pup_na)/1) ) %>%
#   mutate(SCOPA_SEX = SCOPA_SEX/ ((2-sex_na)/2) )
# 
# mean(ams_scopa_bx_2015_16$SCOPA_TOT)
# 
# 
# ams_scopa_bx_2015_16 <- ams_scopa_bx_2015_16 %>%
#     mutate(SCOPA_TOT = rowSums(select(., SCOPA_SEX,SCOPA_PUP,SCOPA_THE,SCOPA_CDV,SCOPA_URI,SCOPA_DIG), na.rm = TRUE))
# 
# ams_scopa_bx_2015_16 <- ams_scopa_bx_2015_16 %>%
#   select(NUM, DATE, SCOPA_TOT, SCOPA_SEX, SCOPA_PUP, SCOPA_THE, SCOPA_CDV, SCOPA_URI, SCOPA_DIG)
# 
# length(unique(ams_scopa_bx_2015_16$NUM))
# length(unique(SCOPA$NUM))
# 
# 
# SCOPA <- ams_scopa_bx_2015_16 %>% inner_join(SCOPA)
# 
# SCOPA <- SCOPA %>% filter(SCOPA_TOT!=0)
# 
# length(unique(SCOPA$NUM))
# 
# 
# SCOPA_Baseline_Pats <- SCOPA %>% filter(Year==0) %>% filter(!is.na(SCOPA_TOT)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM) %>% distinct() %>%
#   inner_join(AllMSA_Pop_Baseline_671)
# 
# 
# 
# # ----------------
# # Overall Early CT V2  -------------------------------
# 
# # ALL SCOPA Total Year 0
# 
# 
# EarlyCT_Pop_Baseline_319 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_TOT)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(SCOPA_TOT),
#     sd=sd(SCOPA_TOT),
#     median=median(SCOPA_TOT),
#     Q1 = quantile(SCOPA_TOT, 0.25),
#     Q3 = quantile(SCOPA_TOT, 0.75),
#     min=min(SCOPA_TOT),
#     max=max(SCOPA_TOT),
#     n=n()
#   )
# 
# 
# 
# # ALL SCOPA Total Year 1
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#   filter(!is.na(SCOPA_TOT)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(SCOPA_TOT),
#     sd=sd(SCOPA_TOT),
#     median=median(SCOPA_TOT),
#     Q1 = quantile(SCOPA_TOT, 0.25),
#     Q3 = quantile(SCOPA_TOT, 0.75),
#     min=min(SCOPA_TOT),
#     max=max(SCOPA_TOT),
#     n=n()
#   )
# 
# # Change from baseline
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_TOT)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
#   rename("Baseline"="SCOPA_TOT") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_TOT)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
#   ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(mean= mean((SCOPA_TOT-Baseline)/TIME_STUDY))
# 
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_TOT)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
#   rename("Baseline"="SCOPA_TOT") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_TOT)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
#   ) %>%
#   mutate(Diff= (SCOPA_TOT-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_TOT)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
#   rename("Baseline"="SCOPA_TOT") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_TOT)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
#   ) %>%
#   mutate(Diff= TIME_STUDY*12) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_TOT)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
#   rename("Baseline"="SCOPA_TOT") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_TOT)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
#   ) %>%
#   mutate(Diff= (SCOPA_TOT-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# # ALL SCOPA Total Year 2
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#   filter(!is.na(SCOPA_TOT)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(SCOPA_TOT),
#     sd=sd(SCOPA_TOT),
#     median=median(SCOPA_TOT),
#     Q1 = quantile(SCOPA_TOT, 0.25),
#     Q3 = quantile(SCOPA_TOT, 0.75),
#     min=min(SCOPA_TOT),
#     max=max(SCOPA_TOT),
#     n=n()
#   )
# 
# 
# # Change from baseline
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_TOT)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
#   rename("Baseline"="SCOPA_TOT") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_TOT)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
#   ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(mean= mean((SCOPA_TOT-Baseline)/TIME_STUDY))
# 
# 
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_TOT)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
#   rename("Baseline"="SCOPA_TOT") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_TOT)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
#   ) %>%
#   mutate(Diff= (SCOPA_TOT-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_TOT)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
#   rename("Baseline"="SCOPA_TOT") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_TOT)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
#   ) %>%
#   mutate(Diff= TIME_STUDY*12) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_TOT)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
#   rename("Baseline"="SCOPA_TOT") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_TOT)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
#   ) %>%
#   mutate(Diff= (SCOPA_TOT-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# # ALL SCOPA Total Year 3
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#   filter(!is.na(SCOPA_TOT)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(SCOPA_TOT),
#     sd=sd(SCOPA_TOT),
#     median=median(SCOPA_TOT),
#     Q1 = quantile(SCOPA_TOT, 0.25),
#     Q3 = quantile(SCOPA_TOT, 0.75),
#     min=min(SCOPA_TOT),
#     max=max(SCOPA_TOT),
#     n=n()
#   )
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_TOT)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
#   rename("Baseline"="SCOPA_TOT") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_TOT)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
#   ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(mean= mean((SCOPA_TOT-Baseline)/TIME_STUDY))
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_TOT)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
#   rename("Baseline"="SCOPA_TOT") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_TOT)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
#   ) %>%
#   mutate(Diff= (SCOPA_TOT-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_TOT)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
#   rename("Baseline"="SCOPA_TOT") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_TOT)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
#   ) %>%
#   mutate(Diff= TIME_STUDY*12) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_TOT)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
#   rename("Baseline"="SCOPA_TOT") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_TOT)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
#   ) %>%
#   mutate(Diff= (SCOPA_TOT-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# # --------------
# 
# 
# # By MSA P vs MSA C V2 -----------------
# # ALL SCOPA Total Year 0
# 
# EarlyCT_Pop_Baseline_319 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_TOT)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(SCOPA_TOT),
#     sd=sd(SCOPA_TOT),
#     median=median(SCOPA_TOT),
#     Q1 = quantile(SCOPA_TOT, 0.25),
#     Q3 = quantile(SCOPA_TOT, 0.75),
#     min=min(SCOPA_TOT),
#     max=max(SCOPA_TOT),
#     n=n()
#   )
# 
# 
# 
# # ALL SCOPA Total Year 1
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#   filter(!is.na(SCOPA_TOT)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(SCOPA_TOT),
#     sd=sd(SCOPA_TOT),
#     median=median(SCOPA_TOT),
#     Q1 = quantile(SCOPA_TOT, 0.25),
#     Q3 = quantile(SCOPA_TOT, 0.75),
#     min=min(SCOPA_TOT),
#     max=max(SCOPA_TOT),
#     n=n()
#   )
# 
# # Change from baseline
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_TOT)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
#   rename("Baseline"="SCOPA_TOT") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_TOT)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
#   ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAG) %>% summarise(mean= mean((SCOPA_TOT-Baseline)/TIME_STUDY))
# 
# 
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_TOT)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
#   rename("Baseline"="SCOPA_TOT") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_TOT)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
#   ) %>%
#   mutate(Diff= (SCOPA_TOT-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAG) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_TOT)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
#   rename("Baseline"="SCOPA_TOT") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_TOT)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
#   ) %>%
#   mutate(Diff= TIME_STUDY*12) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAG) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_TOT)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
#   rename("Baseline"="SCOPA_TOT") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_TOT)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
#   ) %>%
#   mutate(Diff= (SCOPA_TOT-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAG) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# # ALL SCOPA Total Year 2
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#   filter(!is.na(SCOPA_TOT)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(SCOPA_TOT),
#     sd=sd(SCOPA_TOT),
#     median=median(SCOPA_TOT),
#     Q1 = quantile(SCOPA_TOT, 0.25),
#     Q3 = quantile(SCOPA_TOT, 0.75),
#     min=min(SCOPA_TOT),
#     max=max(SCOPA_TOT),
#     n=n()
#   )
# 
# 
# # Change from baseline
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_TOT)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
#   rename("Baseline"="SCOPA_TOT") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_TOT)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
#   ) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAG) %>% summarise(mean= mean((SCOPA_TOT-Baseline)/TIME_STUDY))
# 
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_TOT)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
#   rename("Baseline"="SCOPA_TOT") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_TOT)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
#   ) %>%
#   mutate(Diff= (SCOPA_TOT-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAG) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_TOT)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
#   rename("Baseline"="SCOPA_TOT") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_TOT)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
#   ) %>%
#   mutate(Diff= TIME_STUDY*12) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAG) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_TOT)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
#   rename("Baseline"="SCOPA_TOT") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_TOT)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
#   ) %>%
#   mutate(Diff= (SCOPA_TOT-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAG) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# # ALL SCOPA Total Year 3
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#   filter(!is.na(SCOPA_TOT)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(SCOPA_TOT),
#     sd=sd(SCOPA_TOT),
#     median=median(SCOPA_TOT),
#     Q1 = quantile(SCOPA_TOT, 0.25),
#     Q3 = quantile(SCOPA_TOT, 0.75),
#     min=min(SCOPA_TOT),
#     max=max(SCOPA_TOT),
#     n=n()
#   )
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_TOT)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
#   rename("Baseline"="SCOPA_TOT") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_TOT)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
#   ) %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(mean= mean((SCOPA_TOT-Baseline)/TIME_STUDY))
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_TOT)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
#   rename("Baseline"="SCOPA_TOT") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_TOT)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
#   ) %>%
#   mutate(Diff= (SCOPA_TOT-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAG) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_TOT)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
#   rename("Baseline"="SCOPA_TOT") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_TOT)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
#   ) %>%
#   mutate(Diff= TIME_STUDY*12) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAG) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_TOT)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
#   rename("Baseline"="SCOPA_TOT") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_TOT)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
#   ) %>%
#   mutate(Diff= (SCOPA_TOT-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAG) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# # ------------
# # By Possible vs probable V2 --------------------
# 
# # ALL SCOPA Total Year 0
# 
# EarlyCT_Pop_Baseline_319 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_TOT)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(SCOPA_TOT),
#     sd=sd(SCOPA_TOT),
#     median=median(SCOPA_TOT),
#     Q1 = quantile(SCOPA_TOT, 0.25),
#     Q3 = quantile(SCOPA_TOT, 0.75),
#     min=min(SCOPA_TOT),
#     max=max(SCOPA_TOT),
#     n=n()
#   )
# 
# 
# 
# # ALL SCOPA Total Year 1
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#   filter(!is.na(SCOPA_TOT)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(SCOPA_TOT),
#     sd=sd(SCOPA_TOT),
#     median=median(SCOPA_TOT),
#     Q1 = quantile(SCOPA_TOT, 0.25),
#     Q3 = quantile(SCOPA_TOT, 0.75),
#     min=min(SCOPA_TOT),
#     max=max(SCOPA_TOT),
#     n=n()
#   )
# 
# # Change from baseline
# 
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_TOT)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
#   rename("Baseline"="SCOPA_TOT") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_TOT)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
#   ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAGNIV) %>% summarise(mean= mean((SCOPA_TOT-Baseline)/TIME_STUDY))
# 
# 
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_TOT)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
#   rename("Baseline"="SCOPA_TOT") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_TOT)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
#   ) %>%
#   mutate(Diff= (SCOPA_TOT-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAGNIV) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_TOT)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
#   rename("Baseline"="SCOPA_TOT") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_TOT)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
#   ) %>%
#   mutate(Diff= TIME_STUDY*12) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAGNIV) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_TOT)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
#   rename("Baseline"="SCOPA_TOT") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_TOT)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
#   ) %>%
#   mutate(Diff= (SCOPA_TOT-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAGNIV) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# # ALL SCOPA Total Year 2
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#   filter(!is.na(SCOPA_TOT)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(SCOPA_TOT),
#     sd=sd(SCOPA_TOT),
#     median=median(SCOPA_TOT),
#     Q1 = quantile(SCOPA_TOT, 0.25),
#     Q3 = quantile(SCOPA_TOT, 0.75),
#     min=min(SCOPA_TOT),
#     max=max(SCOPA_TOT),
#     n=n()
#   )
# 
# 
# # Change from baseline
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_TOT)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
#   rename("Baseline"="SCOPA_TOT") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_TOT)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
#   ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAGNIV) %>% summarise(mean= mean((SCOPA_TOT-Baseline)/TIME_STUDY))
# 
# 
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_TOT)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
#   rename("Baseline"="SCOPA_TOT") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_TOT)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
#   ) %>%
#   mutate(Diff= (SCOPA_TOT-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAGNIV) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_TOT)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
#   rename("Baseline"="SCOPA_TOT") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_TOT)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
#   ) %>%
#   mutate(Diff= TIME_STUDY*12) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAGNIV) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_TOT)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
#   rename("Baseline"="SCOPA_TOT") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_TOT)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
#   ) %>%
#   mutate(Diff= (SCOPA_TOT-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAGNIV) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# # ALL SCOPA Total Year 3
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#   filter(!is.na(SCOPA_TOT)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(SCOPA_TOT),
#     sd=sd(SCOPA_TOT),
#     median=median(SCOPA_TOT),
#     Q1 = quantile(SCOPA_TOT, 0.25),
#     Q3 = quantile(SCOPA_TOT, 0.75),
#     min=min(SCOPA_TOT),
#     max=max(SCOPA_TOT),
#     n=n()
#   )
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_TOT)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
#   rename("Baseline"="SCOPA_TOT") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_TOT)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
#   ) %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(mean= mean((SCOPA_TOT-Baseline)/TIME_STUDY))
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_TOT)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
#   rename("Baseline"="SCOPA_TOT") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_TOT)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
#   ) %>%
#   mutate(Diff= (SCOPA_TOT-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAGNIV) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_TOT)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
#   rename("Baseline"="SCOPA_TOT") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_TOT)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
#   ) %>%
#   mutate(Diff= TIME_STUDY*12) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAGNIV) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_TOT)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
#   rename("Baseline"="SCOPA_TOT") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_TOT)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
#   ) %>%
#   mutate(Diff= (SCOPA_TOT-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAGNIV) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# # -----------------------------
# 
# 
# # Student T-tests for deltas in SCOPA AUT Prob vs Poss, MSA-C vs MSA-P V2 ---------------------------------------
# library(rstatix)
# 
# # Inputs SCOPA Total V2 ----------------------------------
# 
# 
# AllMSA_Pop_Baseline_671 <- fread("Source/AllMSA_Pop_Baseline_671.txt")
# AllMSA_Pop_BaselineYear1_410 <- fread("Source/AllMSA_Pop_BaselineYear1_410.txt")
# AllMSA_Pop_BaselineYear1Year2_245 <- fread("Source/AllMSA_Pop_BaselineYear1Year2_245.txt")
# AllMSA_Pop_BaselineYear1Year2Year3Plus_158 <- fread("Source/AllMSA_Pop_BaselineYear1Year2Year3Plus_158.txt")
# 
# EarlyCT_Pop_Baseline_319 <- fread("Source/EarlyCT_Pop_Baseline_319.txt")
# EarlyCT_Pop_BaselineYear1_208 <- fread("Source/EarlyCT_Pop_BaselineYear1_208.txt")
# EarlyCT_Pop_BaselineYear1Year2_134 <- fread("Source/EarlyCT_Pop_BaselineYear1Year2_134.txt")
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 <- fread( "Source/EarlyCT_Pop_BaselineYear1Year2Year3Plus_99.txt")
# 
# dataCohorteManaged <- readRDS("Source/dataCohorteManaged.rds")
# 
# dataCohorteManaged <- dataCohorteManaged %>% group_by(NUM) %>% mutate(TIME_STUDY = ifelse( is.na(TIME_STUDY), 0, TIME_STUDY)) %>%
#   mutate(Year= ifelse(TIME_STUDY==0, 0,
#                       ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
#                              ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
#                                     ifelse(TIME_STUDY>=2.5 ,3, NA)))))
# 
# 
# SCOPA <- dataCohorteManaged %>%
#   select(NUM, DATECONSULT, TIME_STUDY, Year) %>% rename("DATE"="DATECONSULT")
# 
# 
# 
# 
# ams_scopa_bx_2015_16 <- haven::read_sas("Source/ams_scopa_bx_2015_16.sas7bdat")
# names(ams_scopa_bx_2015_16)
# ams_scopa_bx_2015_16 <- ams_scopa_bx_2015_16 %>% filter(!is.na(DATE))
# 
# ams_scopa_bx_2015_16[is.na(ams_scopa_bx_2015_16)] <- 0
# 
# mean(ams_scopa_bx_2015_16$SCOPA_TOT, na.rm=T)
# 
# ams_scopa_bx_2015_16 <- ams_scopa_bx_2015_16 %>%
#   mutate(SCOPA_DIG=SCOPA1+SCOPA2+SCOPA3+SCOPA4+SCOPA5+SCOPA6+SCOPA7) %>%
#   mutate(SCOPA_URI=SCOPA8+SCOPA9+SCOPA10+SCOPA11+SCOPA12+SCOPA13) %>%
#   mutate(SCOPA_CDV=SCOPA14+SCOPA15+SCOPA16) %>%
#   mutate(SCOPA_THE=SCOPA17+SCOPA18+SCOPA19+SCOPA20) %>%
#   mutate(SCOPA_PUP=SCOPA21) %>%
#   mutate(SCOPA_SEX=SCOPA22H+SCOPA22F+SCOPA23F+SCOPA23H) %>%
#   mutate(SCOPA_TOT=SCOPA_SEX+SCOPA_PUP+SCOPA_THE+SCOPA_CDV+SCOPA_URI+SCOPA_DIG)
# 
# ams_scopa_bx_2015_16 <- ams_scopa_bx_2015_16 %>%
#   select(NUM, DATE, SCOPA_TOT, SCOPA_SEX, SCOPA_PUP, SCOPA_THE, SCOPA_CDV, SCOPA_URI, SCOPA_DIG)
# 
# length(unique(ams_scopa_bx_2015_16$NUM))
# 
# 
# SCOPA <- ams_scopa_bx_2015_16 %>% inner_join(SCOPA)
# 
# SCOPA <- SCOPA %>% filter(SCOPA_TOT!=0)
# 
# length(unique(SCOPA$NUM))
# 
# 
# SCOPA_Baseline_Pats <- SCOPA %>% filter(Year==0) %>% filter(!is.na(SCOPA_TOT)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM) %>% distinct() %>%
#   inner_join(AllMSA_Pop_Baseline_671)
# 
# 
# 
# # ----------------
# # Overall Early CT V2  -------------------------------
# 
# # ALL UMSARS Total Year 1
# 
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   filter(!is.na(SCOPA_TOT)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
#   rename("Baseline"="SCOPA_TOT") %>%
#   left_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_TOT)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
#   ) %>%
#   mutate(Diff= (SCOPA_TOT-Baseline)/TIME_STUDY)  %>%
#   t_test(Diff~DIAG, detailed = T, var.equal = TRUE)
#   #t_test(Diff~DIAGNIV,  detailed = T, var.equal = TRUE)
# 
# 
# 
# 
# # ALL UMSARS Total Year 2
# 
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   filter(!is.na(SCOPA_TOT)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
#   rename("Baseline"="SCOPA_TOT") %>%
#   left_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_TOT)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
#   ) %>%
#   mutate(Diff= (SCOPA_TOT-Baseline)/TIME_STUDY)  %>%
#   #t_test(Diff~DIAG,  detailed = T, var.equal = TRUE)
#   t_test(Diff~DIAGNIV,   detailed = T, var.equal = TRUE)
# 
# # ALL UMSARS Total Year 3
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   filter(!is.na(SCOPA_TOT)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
#   rename("Baseline"="SCOPA_TOT") %>%
#   left_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_TOT)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
#   ) %>%
#   mutate(Diff= (SCOPA_TOT-Baseline)/TIME_STUDY)  %>%
#   #t_test(Diff~DIAG, detailed = T, var.equal = TRUE)
#  t_test(Diff~DIAGNIV,  detailed = T, var.equal = TRUE)
# 
# # --------------
# 
# 
# # Correlation between Delta SCOPA AUT & Delta FDA-modified UMSARS V2 ------------------
# 
# # Inputs SCOPA Total V2 ----------------------------------
# 
# 
# AllMSA_Pop_Baseline_671 <- fread("Source/AllMSA_Pop_Baseline_671.txt")
# AllMSA_Pop_BaselineYear1_410 <- fread("Source/AllMSA_Pop_BaselineYear1_410.txt")
# AllMSA_Pop_BaselineYear1Year2_245 <- fread("Source/AllMSA_Pop_BaselineYear1Year2_245.txt")
# AllMSA_Pop_BaselineYear1Year2Year3Plus_158 <- fread("Source/AllMSA_Pop_BaselineYear1Year2Year3Plus_158.txt")
# 
# EarlyCT_Pop_Baseline_319 <- fread("Source/EarlyCT_Pop_Baseline_319.txt")
# EarlyCT_Pop_BaselineYear1_208 <- fread("Source/EarlyCT_Pop_BaselineYear1_208.txt")
# EarlyCT_Pop_BaselineYear1Year2_134 <- fread("Source/EarlyCT_Pop_BaselineYear1Year2_134.txt")
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 <- fread( "Source/EarlyCT_Pop_BaselineYear1Year2Year3Plus_99.txt")
# 
# dataCohorteManaged <- readRDS("Source/dataCohorteManaged.rds")
# 
# dataCohorteManaged <- dataCohorteManaged %>% group_by(NUM) %>% mutate(TIME_STUDY = ifelse( is.na(TIME_STUDY), 0, TIME_STUDY)) %>%
#   mutate(Year= ifelse(TIME_STUDY==0, 0,
#                       ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
#                              ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
#                                     ifelse(TIME_STUDY>=2.5 ,3, NA)))))
# 
# 
# SCOPA <- dataCohorteManaged %>%
#   select(NUM, DATECONSULT, TIME_STUDY, Year) %>% rename("DATE"="DATECONSULT")
# 
# 
# 
# 
# ams_scopa_bx_2015_16 <- haven::read_sas("Source/ams_scopa_bx_2015_16.sas7bdat")
# names(ams_scopa_bx_2015_16)
# ams_scopa_bx_2015_16 <- ams_scopa_bx_2015_16 %>% filter(!is.na(DATE))
# 
# ams_scopa_bx_2015_16[is.na(ams_scopa_bx_2015_16)] <- 0
# 
# mean(ams_scopa_bx_2015_16$SCOPA_TOT, na.rm=T)
# 
# ams_scopa_bx_2015_16 <- ams_scopa_bx_2015_16 %>%
#   mutate(SCOPA_DIG=SCOPA1+SCOPA2+SCOPA3+SCOPA4+SCOPA5+SCOPA6+SCOPA7) %>%
#   mutate(SCOPA_URI=SCOPA8+SCOPA9+SCOPA10+SCOPA11+SCOPA12+SCOPA13) %>%
#   mutate(SCOPA_CDV=SCOPA14+SCOPA15+SCOPA16) %>%
#   mutate(SCOPA_THE=SCOPA17+SCOPA18+SCOPA19+SCOPA20) %>%
#   mutate(SCOPA_PUP=SCOPA21) %>%
#   mutate(SCOPA_SEX=SCOPA22H+SCOPA22F+SCOPA23F+SCOPA23H) %>%
#   mutate(SCOPA_TOT=SCOPA_SEX+SCOPA_PUP+SCOPA_THE+SCOPA_CDV+SCOPA_URI+SCOPA_DIG)
# 
# ams_scopa_bx_2015_16 <- ams_scopa_bx_2015_16 %>%
#   select(NUM, DATE, SCOPA_TOT, SCOPA_SEX, SCOPA_PUP, SCOPA_THE, SCOPA_CDV, SCOPA_URI, SCOPA_DIG)
# 
# length(unique(ams_scopa_bx_2015_16$NUM))
# 
# 
# SCOPA <- ams_scopa_bx_2015_16 %>% inner_join(SCOPA)
# 
# SCOPA <- SCOPA %>% filter(SCOPA_TOT!=0)
# 
# length(unique(SCOPA$NUM))
# 
# 
# SCOPA_Baseline_Pats <- SCOPA %>% filter(Year==0) %>% filter(!is.na(SCOPA_TOT)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM) %>% distinct() %>%
#   inner_join(AllMSA_Pop_Baseline_671)
# 
# 
# 
# 
# UMSARS1 <- dataCohorteManaged %>%
#   select(NUM, DATECONSULT, TIME_STUDY, Year, UMSARS1_1:UMSARS1_TOT)
# 
# 
# UMSARS1$UMSARS1_TOT_v2 <- rowSums(UMSARS1[, 5:16], na.rm = TRUE)
# 
# UMSARS1$missing_na <- rowSums(is.na(UMSARS1[, 5:16]))
# 
# UMSARS1 <-  UMSARS1 %>%  select(NUM, TIME_STUDY, Year, UMSARS1_TOT, UMSARS1_11)
# 
# UMSARS1 <- UMSARS1 %>% mutate(UMSARS1_TOT_FDA=UMSARS1_TOT-UMSARS1_11)
# 
# 
# # ----------------
# # Overall Early CT V2  -------------------------------
# 
# # Year 1
# 
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   filter(!is.na(SCOPA_TOT)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
#   rename("Baseline"="SCOPA_TOT") %>%
#   left_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_TOT)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
#   ) %>%
#   mutate(Diff= (SCOPA_TOT-Baseline))  %>% select(NUM, DIAG, DIAGNIV, Diff) %>% rename("SCOPA_Diff"="Diff") %>% drop_na() %>%
#   inner_join(
# 
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
#       filter(!is.na(UMSARS1_TOT_FDA)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  UMSARS1_TOT_FDA) %>%
#       rename("Baseline"="UMSARS1_TOT_FDA") %>%
#       left_join(
#         EarlyCT_Pop_BaselineYear1_208 %>% inner_join(UMSARS1) %>% filter(Year==1) %>%
#           filter(!is.na(UMSARS1_TOT_FDA)) %>%
#           mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#           group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#           group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#           group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, UMSARS1_TOT_FDA)
#       ) %>%
#       mutate(Diff= (UMSARS1_TOT_FDA-Baseline))  %>% select(NUM, DIAG, DIAGNIV, Diff)
#   )  %>% summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))
# #group_by(DIAG) %>% summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))
# #group_by(DIAGNIV) %>% summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))
# 
# 
# 
# 
# 
# # Year 2
# 
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   filter(!is.na(SCOPA_TOT)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
#   rename("Baseline"="SCOPA_TOT") %>%
#   left_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_TOT)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
#   ) %>%
#   mutate(Diff= (SCOPA_TOT-Baseline))  %>% select(NUM, DIAG, DIAGNIV, Diff) %>% rename("SCOPA_Diff"="Diff") %>% drop_na() %>%
#   inner_join(
# 
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
#       filter(!is.na(UMSARS1_TOT_FDA)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  UMSARS1_TOT_FDA) %>%
#       rename("Baseline"="UMSARS1_TOT_FDA") %>%
#       left_join(
#         EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(UMSARS1) %>% filter(Year==2) %>%
#           filter(!is.na(UMSARS1_TOT_FDA)) %>%
#           mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#           group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#           group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#           group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, UMSARS1_TOT_FDA)
#       ) %>%
#       mutate(Diff= (UMSARS1_TOT_FDA-Baseline))   %>% select(NUM, DIAG, DIAGNIV, Diff)
#   )  %>%  summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))
# #group_by(DIAG) %>% summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))
# #group_by(DIAGNIV) %>% summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))
# 
# 
# # Year 3
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   filter(!is.na(SCOPA_TOT)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_TOT) %>%
#   rename("Baseline"="SCOPA_TOT") %>%
#   left_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_TOT)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_TOT)
#   ) %>%
#   mutate(Diff= (SCOPA_TOT-Baseline))  %>% select(NUM, DIAG, DIAGNIV, Diff) %>% rename("SCOPA_Diff"="Diff") %>% drop_na() %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
#       filter(!is.na(UMSARS1_TOT_FDA)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  UMSARS1_TOT_FDA) %>%
#       rename("Baseline"="UMSARS1_TOT_FDA") %>%
#       left_join(
#         EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(UMSARS1) %>% filter(Year==3) %>%
#           filter(!is.na(UMSARS1_TOT_FDA)) %>%
#           mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#           group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#           group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#           group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, UMSARS1_TOT_FDA)
#       ) %>%
#       mutate(Diff= (UMSARS1_TOT_FDA-Baseline)) %>% select(NUM, DIAG, DIAGNIV, Diff)
#   )  %>%  summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))
# #group_by(DIAG) %>% summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))
# #group_by(DIAGNIV) %>% summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))
# 
# 
# # --------------
# 
# 
# 
# # Inputs SCOPA AUT V2 DIGESTIVE ----------------------------------
# 
# 
# AllMSA_Pop_Baseline_671 <- fread("Source/AllMSA_Pop_Baseline_671.txt")
# AllMSA_Pop_BaselineYear1_410 <- fread("Source/AllMSA_Pop_BaselineYear1_410.txt")
# AllMSA_Pop_BaselineYear1Year2_245 <- fread("Source/AllMSA_Pop_BaselineYear1Year2_245.txt")
# AllMSA_Pop_BaselineYear1Year2Year3Plus_158 <- fread("Source/AllMSA_Pop_BaselineYear1Year2Year3Plus_158.txt")
# 
# EarlyCT_Pop_Baseline_319 <- fread("Source/EarlyCT_Pop_Baseline_319.txt")
# EarlyCT_Pop_BaselineYear1_208 <- fread("Source/EarlyCT_Pop_BaselineYear1_208.txt")
# EarlyCT_Pop_BaselineYear1Year2_134 <- fread("Source/EarlyCT_Pop_BaselineYear1Year2_134.txt")
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 <- fread( "Source/EarlyCT_Pop_BaselineYear1Year2Year3Plus_99.txt")
# 
# dataCohorteManaged <- readRDS("Source/dataCohorteManaged.rds")
# 
# dataCohorteManaged <- dataCohorteManaged %>% group_by(NUM) %>% mutate(TIME_STUDY = ifelse( is.na(TIME_STUDY), 0, TIME_STUDY)) %>%
#   mutate(Year= ifelse(TIME_STUDY==0, 0,
#                       ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
#                              ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
#                                     ifelse(TIME_STUDY>=2.5 ,3, NA)))))
# 
# 
# SCOPA <- dataCohorteManaged %>%
#   select(NUM, DATECONSULT, TIME_STUDY, Year) %>% rename("DATE"="DATECONSULT")
# 
# 
# 
# 
# ams_scopa_bx_2015_16 <- haven::read_sas("Source/ams_scopa_bx_2015_16.sas7bdat")
# names(ams_scopa_bx_2015_16)
# ams_scopa_bx_2015_16 <- ams_scopa_bx_2015_16 %>% filter(!is.na(DATE))
# 
# ams_scopa_bx_2015_16[is.na(ams_scopa_bx_2015_16)] <- 0
# 
# mean(ams_scopa_bx_2015_16$SCOPA_TOT, na.rm=T)
# 
# ams_scopa_bx_2015_16 <- ams_scopa_bx_2015_16 %>%
#   mutate(SCOPA_DIG=SCOPA1+SCOPA2+SCOPA3+SCOPA4+SCOPA5+SCOPA6+SCOPA7) %>%
#   mutate(SCOPA_URI=SCOPA8+SCOPA9+SCOPA10+SCOPA11+SCOPA12+SCOPA13) %>%
#   mutate(SCOPA_CDV=SCOPA14+SCOPA15+SCOPA16) %>%
#   mutate(SCOPA_THE=SCOPA17+SCOPA18+SCOPA19+SCOPA20) %>%
#   mutate(SCOPA_PUP=SCOPA21) %>%
#   mutate(SCOPA_SEX=SCOPA22H+SCOPA22F+SCOPA23F+SCOPA23H) %>%
#   mutate(SCOPA_TOT=SCOPA_SEX+SCOPA_PUP+SCOPA_THE+SCOPA_CDV+SCOPA_URI+SCOPA_DIG)
# 
# ams_scopa_bx_2015_16 <- ams_scopa_bx_2015_16 %>%
#   select(NUM, DATE, SCOPA_TOT, SCOPA_SEX, SCOPA_PUP, SCOPA_THE, SCOPA_CDV, SCOPA_URI, SCOPA_DIG)
# 
# length(unique(ams_scopa_bx_2015_16$NUM))
# 
# 
# SCOPA <- ams_scopa_bx_2015_16 %>% inner_join(SCOPA)
# 
# SCOPA <- SCOPA %>% filter(SCOPA_TOT!=0)
# 
# length(unique(SCOPA$NUM))
# 
# 
# SCOPA_Baseline_Pats <- SCOPA %>% filter(Year==0) %>% filter(!is.na(SCOPA_TOT)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM) %>% distinct() %>%
#   inner_join(AllMSA_Pop_Baseline_671)
# 
# 
# 
# # ----------------
# # Overall Early CT V2 DIGESTIVE  -------------------------------
# 
# # ALL SCOPA Total Year 0
# 
# 
# EarlyCT_Pop_Baseline_319 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_DIG)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(SCOPA_DIG),
#     sd=sd(SCOPA_DIG),
#     median=median(SCOPA_DIG),
#     Q1 = quantile(SCOPA_DIG, 0.25),
#     Q3 = quantile(SCOPA_DIG, 0.75),
#     min=min(SCOPA_DIG),
#     max=max(SCOPA_DIG),
#     n=n()
#   )
# 
# 
# 
# # ALL SCOPA Total Year 1
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#   filter(!is.na(SCOPA_DIG)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(SCOPA_DIG),
#     sd=sd(SCOPA_DIG),
#     median=median(SCOPA_DIG),
#     Q1 = quantile(SCOPA_DIG, 0.25),
#     Q3 = quantile(SCOPA_DIG, 0.75),
#     min=min(SCOPA_DIG),
#     max=max(SCOPA_DIG),
#     n=n()
#   )
# 
# # Change from baseline
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_DIG)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
#   rename("Baseline"="SCOPA_DIG") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_DIG)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
#   ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(mean= mean((SCOPA_DIG-Baseline)/TIME_STUDY))
# 
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_DIG)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
#   rename("Baseline"="SCOPA_DIG") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_DIG)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
#   ) %>%
#   mutate(Diff= (SCOPA_DIG-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_DIG)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
#   rename("Baseline"="SCOPA_DIG") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_DIG)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
#   ) %>%
#   mutate(Diff= TIME_STUDY*12) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_DIG)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
#   rename("Baseline"="SCOPA_DIG") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_DIG)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
#   ) %>%
#   mutate(Diff= (SCOPA_DIG-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# # ALL SCOPA Total Year 2
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#   filter(!is.na(SCOPA_DIG)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(SCOPA_DIG),
#     sd=sd(SCOPA_DIG),
#     median=median(SCOPA_DIG),
#     Q1 = quantile(SCOPA_DIG, 0.25),
#     Q3 = quantile(SCOPA_DIG, 0.75),
#     min=min(SCOPA_DIG),
#     max=max(SCOPA_DIG),
#     n=n()
#   )
# 
# 
# # Change from baseline
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_DIG)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
#   rename("Baseline"="SCOPA_DIG") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_DIG)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
#   ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(mean= mean((SCOPA_DIG-Baseline)/TIME_STUDY))
# 
# 
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_DIG)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
#   rename("Baseline"="SCOPA_DIG") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_DIG)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
#   ) %>%
#   mutate(Diff= (SCOPA_DIG-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_DIG)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
#   rename("Baseline"="SCOPA_DIG") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_DIG)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
#   ) %>%
#   mutate(Diff= TIME_STUDY*12) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_DIG)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
#   rename("Baseline"="SCOPA_DIG") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_DIG)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
#   ) %>%
#   mutate(Diff= (SCOPA_DIG-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# # ALL SCOPA Total Year 3
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#   filter(!is.na(SCOPA_DIG)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(SCOPA_DIG),
#     sd=sd(SCOPA_DIG),
#     median=median(SCOPA_DIG),
#     Q1 = quantile(SCOPA_DIG, 0.25),
#     Q3 = quantile(SCOPA_DIG, 0.75),
#     min=min(SCOPA_DIG),
#     max=max(SCOPA_DIG),
#     n=n()
#   )
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_DIG)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
#   rename("Baseline"="SCOPA_DIG") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_DIG)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
#   ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(mean= mean((SCOPA_DIG-Baseline)/TIME_STUDY))
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_DIG)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
#   rename("Baseline"="SCOPA_DIG") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_DIG)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
#   ) %>%
#   mutate(Diff= (SCOPA_DIG-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_DIG)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
#   rename("Baseline"="SCOPA_DIG") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_DIG)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
#   ) %>%
#   mutate(Diff= TIME_STUDY*12) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_DIG)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
#   rename("Baseline"="SCOPA_DIG") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_DIG)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
#   ) %>%
#   mutate(Diff= (SCOPA_DIG-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# # --------------
# 
# 
# # By MSA P vs MSA C V2 DIGESTIVE -----------------
# # ALL SCOPA Total Year 0
# 
# EarlyCT_Pop_Baseline_319 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_DIG)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(SCOPA_DIG),
#     sd=sd(SCOPA_DIG),
#     median=median(SCOPA_DIG),
#     Q1 = quantile(SCOPA_DIG, 0.25),
#     Q3 = quantile(SCOPA_DIG, 0.75),
#     min=min(SCOPA_DIG),
#     max=max(SCOPA_DIG),
#     n=n()
#   )
# 
# 
# 
# # ALL SCOPA Total Year 1
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#   filter(!is.na(SCOPA_DIG)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(SCOPA_DIG),
#     sd=sd(SCOPA_DIG),
#     median=median(SCOPA_DIG),
#     Q1 = quantile(SCOPA_DIG, 0.25),
#     Q3 = quantile(SCOPA_DIG, 0.75),
#     min=min(SCOPA_DIG),
#     max=max(SCOPA_DIG),
#     n=n()
#   )
# 
# # Change from baseline
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_DIG)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
#   rename("Baseline"="SCOPA_DIG") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_DIG)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
#   ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAG) %>% summarise(mean= mean((SCOPA_DIG-Baseline)/TIME_STUDY))
# 
# 
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_DIG)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
#   rename("Baseline"="SCOPA_DIG") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_DIG)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
#   ) %>%
#   mutate(Diff= (SCOPA_DIG-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAG) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_DIG)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
#   rename("Baseline"="SCOPA_DIG") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_DIG)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
#   ) %>%
#   mutate(Diff= TIME_STUDY*12) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAG) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_DIG)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
#   rename("Baseline"="SCOPA_DIG") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_DIG)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
#   ) %>%
#   mutate(Diff= (SCOPA_DIG-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAG) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# # ALL SCOPA Total Year 2
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#   filter(!is.na(SCOPA_DIG)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(SCOPA_DIG),
#     sd=sd(SCOPA_DIG),
#     median=median(SCOPA_DIG),
#     Q1 = quantile(SCOPA_DIG, 0.25),
#     Q3 = quantile(SCOPA_DIG, 0.75),
#     min=min(SCOPA_DIG),
#     max=max(SCOPA_DIG),
#     n=n()
#   )
# 
# 
# # Change from baseline
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_DIG)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
#   rename("Baseline"="SCOPA_DIG") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_DIG)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
#   ) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAG) %>% summarise(mean= mean((SCOPA_DIG-Baseline)/TIME_STUDY))
# 
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_DIG)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
#   rename("Baseline"="SCOPA_DIG") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_DIG)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
#   ) %>%
#   mutate(Diff= (SCOPA_DIG-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAG) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_DIG)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
#   rename("Baseline"="SCOPA_DIG") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_DIG)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
#   ) %>%
#   mutate(Diff= TIME_STUDY*12) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAG) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_DIG)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
#   rename("Baseline"="SCOPA_DIG") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_DIG)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
#   ) %>%
#   mutate(Diff= (SCOPA_DIG-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAG) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# # ALL SCOPA Total Year 3
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#   filter(!is.na(SCOPA_DIG)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(SCOPA_DIG),
#     sd=sd(SCOPA_DIG),
#     median=median(SCOPA_DIG),
#     Q1 = quantile(SCOPA_DIG, 0.25),
#     Q3 = quantile(SCOPA_DIG, 0.75),
#     min=min(SCOPA_DIG),
#     max=max(SCOPA_DIG),
#     n=n()
#   )
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_DIG)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
#   rename("Baseline"="SCOPA_DIG") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_DIG)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
#   ) %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(mean= mean((SCOPA_DIG-Baseline)/TIME_STUDY))
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_DIG)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
#   rename("Baseline"="SCOPA_DIG") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_DIG)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
#   ) %>%
#   mutate(Diff= (SCOPA_DIG-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAG) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_DIG)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
#   rename("Baseline"="SCOPA_DIG") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_DIG)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
#   ) %>%
#   mutate(Diff= TIME_STUDY*12) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAG) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_DIG)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
#   rename("Baseline"="SCOPA_DIG") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_DIG)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
#   ) %>%
#   mutate(Diff= (SCOPA_DIG-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAG) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# # ------------
# # By Possible vs probable V2 DIGESTIVE --------------------
# 
# # ALL SCOPA Total Year 0
# 
# EarlyCT_Pop_Baseline_319 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_DIG)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(SCOPA_DIG),
#     sd=sd(SCOPA_DIG),
#     median=median(SCOPA_DIG),
#     Q1 = quantile(SCOPA_DIG, 0.25),
#     Q3 = quantile(SCOPA_DIG, 0.75),
#     min=min(SCOPA_DIG),
#     max=max(SCOPA_DIG),
#     n=n()
#   )
# 
# 
# 
# # ALL SCOPA Total Year 1
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#   filter(!is.na(SCOPA_DIG)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(SCOPA_DIG),
#     sd=sd(SCOPA_DIG),
#     median=median(SCOPA_DIG),
#     Q1 = quantile(SCOPA_DIG, 0.25),
#     Q3 = quantile(SCOPA_DIG, 0.75),
#     min=min(SCOPA_DIG),
#     max=max(SCOPA_DIG),
#     n=n()
#   )
# 
# # Change from baseline
# 
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_DIG)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
#   rename("Baseline"="SCOPA_DIG") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_DIG)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
#   ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAGNIV) %>% summarise(mean= mean((SCOPA_DIG-Baseline)/TIME_STUDY))
# 
# 
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_DIG)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
#   rename("Baseline"="SCOPA_DIG") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_DIG)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
#   ) %>%
#   mutate(Diff= (SCOPA_DIG-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAGNIV) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_DIG)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
#   rename("Baseline"="SCOPA_DIG") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_DIG)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
#   ) %>%
#   mutate(Diff= TIME_STUDY*12) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAGNIV) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_DIG)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
#   rename("Baseline"="SCOPA_DIG") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_DIG)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
#   ) %>%
#   mutate(Diff= (SCOPA_DIG-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAGNIV) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# # ALL SCOPA Total Year 2
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#   filter(!is.na(SCOPA_DIG)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(SCOPA_DIG),
#     sd=sd(SCOPA_DIG),
#     median=median(SCOPA_DIG),
#     Q1 = quantile(SCOPA_DIG, 0.25),
#     Q3 = quantile(SCOPA_DIG, 0.75),
#     min=min(SCOPA_DIG),
#     max=max(SCOPA_DIG),
#     n=n()
#   )
# 
# 
# # Change from baseline
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_DIG)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
#   rename("Baseline"="SCOPA_DIG") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_DIG)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
#   ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAGNIV) %>% summarise(mean= mean((SCOPA_DIG-Baseline)/TIME_STUDY))
# 
# 
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_DIG)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
#   rename("Baseline"="SCOPA_DIG") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_DIG)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
#   ) %>%
#   mutate(Diff= (SCOPA_DIG-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAGNIV) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_DIG)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
#   rename("Baseline"="SCOPA_DIG") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_DIG)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
#   ) %>%
#   mutate(Diff= TIME_STUDY*12) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAGNIV) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_DIG)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
#   rename("Baseline"="SCOPA_DIG") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_DIG)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
#   ) %>%
#   mutate(Diff= (SCOPA_DIG-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAGNIV) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# # ALL SCOPA Total Year 3
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#   filter(!is.na(SCOPA_DIG)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(SCOPA_DIG),
#     sd=sd(SCOPA_DIG),
#     median=median(SCOPA_DIG),
#     Q1 = quantile(SCOPA_DIG, 0.25),
#     Q3 = quantile(SCOPA_DIG, 0.75),
#     min=min(SCOPA_DIG),
#     max=max(SCOPA_DIG),
#     n=n()
#   )
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_DIG)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
#   rename("Baseline"="SCOPA_DIG") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_DIG)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
#   ) %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(mean= mean((SCOPA_DIG-Baseline)/TIME_STUDY))
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_DIG)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
#   rename("Baseline"="SCOPA_DIG") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_DIG)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
#   ) %>%
#   mutate(Diff= (SCOPA_DIG-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAGNIV) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_DIG)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
#   rename("Baseline"="SCOPA_DIG") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_DIG)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
#   ) %>%
#   mutate(Diff= TIME_STUDY*12) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAGNIV) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_DIG)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
#   rename("Baseline"="SCOPA_DIG") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_DIG)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
#   ) %>%
#   mutate(Diff= (SCOPA_DIG-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAGNIV) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# # -----------------------------
# 
# 
# # Inputs SCOPA AUT V2 URINARY ----------------------------------
# 
# 
# AllMSA_Pop_Baseline_671 <- fread("Source/AllMSA_Pop_Baseline_671.txt")
# AllMSA_Pop_BaselineYear1_410 <- fread("Source/AllMSA_Pop_BaselineYear1_410.txt")
# AllMSA_Pop_BaselineYear1Year2_245 <- fread("Source/AllMSA_Pop_BaselineYear1Year2_245.txt")
# AllMSA_Pop_BaselineYear1Year2Year3Plus_158 <- fread("Source/AllMSA_Pop_BaselineYear1Year2Year3Plus_158.txt")
# 
# EarlyCT_Pop_Baseline_319 <- fread("Source/EarlyCT_Pop_Baseline_319.txt")
# EarlyCT_Pop_BaselineYear1_208 <- fread("Source/EarlyCT_Pop_BaselineYear1_208.txt")
# EarlyCT_Pop_BaselineYear1Year2_134 <- fread("Source/EarlyCT_Pop_BaselineYear1Year2_134.txt")
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 <- fread( "Source/EarlyCT_Pop_BaselineYear1Year2Year3Plus_99.txt")
# 
# dataCohorteManaged <- readRDS("Source/dataCohorteManaged.rds")
# 
# dataCohorteManaged <- dataCohorteManaged %>% group_by(NUM) %>% mutate(TIME_STUDY = ifelse( is.na(TIME_STUDY), 0, TIME_STUDY)) %>%
#   mutate(Year= ifelse(TIME_STUDY==0, 0,
#                       ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
#                              ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
#                                     ifelse(TIME_STUDY>=2.5 ,3, NA)))))
# 
# 
# SCOPA <- dataCohorteManaged %>%
#   select(NUM, DATECONSULT, TIME_STUDY, Year) %>% rename("DATE"="DATECONSULT")
# 
# 
# 
# 
# ams_scopa_bx_2015_16 <- haven::read_sas("Source/ams_scopa_bx_2015_16.sas7bdat")
# names(ams_scopa_bx_2015_16)
# ams_scopa_bx_2015_16 <- ams_scopa_bx_2015_16 %>% filter(!is.na(DATE))
# 
# ams_scopa_bx_2015_16[is.na(ams_scopa_bx_2015_16)] <- 0
# 
# mean(ams_scopa_bx_2015_16$SCOPA_TOT, na.rm=T)
# 
# ams_scopa_bx_2015_16 <- ams_scopa_bx_2015_16 %>%
#   mutate(SCOPA_DIG=SCOPA1+SCOPA2+SCOPA3+SCOPA4+SCOPA5+SCOPA6+SCOPA7) %>%
#   mutate(SCOPA_URI=SCOPA8+SCOPA9+SCOPA10+SCOPA11+SCOPA12+SCOPA13) %>%
#   mutate(SCOPA_CDV=SCOPA14+SCOPA15+SCOPA16) %>%
#   mutate(SCOPA_THE=SCOPA17+SCOPA18+SCOPA19+SCOPA20) %>%
#   mutate(SCOPA_PUP=SCOPA21) %>%
#   mutate(SCOPA_SEX=SCOPA22H+SCOPA22F+SCOPA23F+SCOPA23H) %>%
#   mutate(SCOPA_TOT=SCOPA_SEX+SCOPA_PUP+SCOPA_THE+SCOPA_CDV+SCOPA_URI+SCOPA_DIG)
# 
# ams_scopa_bx_2015_16 <- ams_scopa_bx_2015_16 %>%
#   select(NUM, DATE, SCOPA_TOT, SCOPA_SEX, SCOPA_PUP, SCOPA_THE, SCOPA_CDV, SCOPA_URI, SCOPA_DIG)
# 
# length(unique(ams_scopa_bx_2015_16$NUM))
# 
# 
# SCOPA <- ams_scopa_bx_2015_16 %>% inner_join(SCOPA)
# 
# SCOPA <- SCOPA %>% filter(SCOPA_TOT!=0)
# 
# length(unique(SCOPA$NUM))
# 
# 
# SCOPA_Baseline_Pats <- SCOPA %>% filter(Year==0) %>% filter(!is.na(SCOPA_TOT)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM) %>% distinct() %>%
#   inner_join(AllMSA_Pop_Baseline_671)
# 
# 
# 
# # ----------------
# # Overall Early CT V2 URINARY  -------------------------------
# 
# # ALL SCOPA Total Year 0
# 
# 
# EarlyCT_Pop_Baseline_319 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_URI)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(SCOPA_URI),
#     sd=sd(SCOPA_URI),
#     median=median(SCOPA_URI),
#     Q1 = quantile(SCOPA_URI, 0.25),
#     Q3 = quantile(SCOPA_URI, 0.75),
#     min=min(SCOPA_URI),
#     max=max(SCOPA_URI),
#     n=n()
#   )
# 
# 
# 
# # ALL SCOPA Total Year 1
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#   filter(!is.na(SCOPA_URI)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(SCOPA_URI),
#     sd=sd(SCOPA_URI),
#     median=median(SCOPA_URI),
#     Q1 = quantile(SCOPA_URI, 0.25),
#     Q3 = quantile(SCOPA_URI, 0.75),
#     min=min(SCOPA_URI),
#     max=max(SCOPA_URI),
#     n=n()
#   )
# 
# # Change from baseline
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_URI)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
#   rename("Baseline"="SCOPA_URI") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_URI)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
#   ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(mean= mean((SCOPA_URI-Baseline)/TIME_STUDY))
# 
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_URI)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
#   rename("Baseline"="SCOPA_URI") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_URI)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
#   ) %>%
#   mutate(Diff= (SCOPA_URI-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_URI)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
#   rename("Baseline"="SCOPA_URI") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_URI)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
#   ) %>%
#   mutate(Diff= TIME_STUDY*12) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_URI)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
#   rename("Baseline"="SCOPA_URI") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_URI)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
#   ) %>%
#   mutate(Diff= (SCOPA_URI-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# # ALL SCOPA Total Year 2
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#   filter(!is.na(SCOPA_URI)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(SCOPA_URI),
#     sd=sd(SCOPA_URI),
#     median=median(SCOPA_URI),
#     Q1 = quantile(SCOPA_URI, 0.25),
#     Q3 = quantile(SCOPA_URI, 0.75),
#     min=min(SCOPA_URI),
#     max=max(SCOPA_URI),
#     n=n()
#   )
# 
# 
# # Change from baseline
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_URI)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
#   rename("Baseline"="SCOPA_URI") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_URI)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
#   ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(mean= mean((SCOPA_URI-Baseline)/TIME_STUDY))
# 
# 
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_URI)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
#   rename("Baseline"="SCOPA_URI") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_URI)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
#   ) %>%
#   mutate(Diff= (SCOPA_URI-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_URI)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
#   rename("Baseline"="SCOPA_URI") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_URI)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
#   ) %>%
#   mutate(Diff= TIME_STUDY*12) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_URI)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
#   rename("Baseline"="SCOPA_URI") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_URI)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
#   ) %>%
#   mutate(Diff= (SCOPA_URI-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# # ALL SCOPA Total Year 3
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#   filter(!is.na(SCOPA_URI)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(SCOPA_URI),
#     sd=sd(SCOPA_URI),
#     median=median(SCOPA_URI),
#     Q1 = quantile(SCOPA_URI, 0.25),
#     Q3 = quantile(SCOPA_URI, 0.75),
#     min=min(SCOPA_URI),
#     max=max(SCOPA_URI),
#     n=n()
#   )
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_URI)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
#   rename("Baseline"="SCOPA_URI") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_URI)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
#   ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(mean= mean((SCOPA_URI-Baseline)/TIME_STUDY))
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_URI)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
#   rename("Baseline"="SCOPA_URI") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_URI)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
#   ) %>%
#   mutate(Diff= (SCOPA_URI-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_URI)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
#   rename("Baseline"="SCOPA_URI") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_URI)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
#   ) %>%
#   mutate(Diff= TIME_STUDY*12) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_URI)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
#   rename("Baseline"="SCOPA_URI") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_URI)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
#   ) %>%
#   mutate(Diff= (SCOPA_URI-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# # --------------
# 
# 
# # By MSA P vs MSA C V2 URINARY -----------------
# # ALL SCOPA Total Year 0
# 
# EarlyCT_Pop_Baseline_319 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_URI)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(SCOPA_URI),
#     sd=sd(SCOPA_URI),
#     median=median(SCOPA_URI),
#     Q1 = quantile(SCOPA_URI, 0.25),
#     Q3 = quantile(SCOPA_URI, 0.75),
#     min=min(SCOPA_URI),
#     max=max(SCOPA_URI),
#     n=n()
#   )
# 
# 
# 
# # ALL SCOPA Total Year 1
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#   filter(!is.na(SCOPA_URI)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(SCOPA_URI),
#     sd=sd(SCOPA_URI),
#     median=median(SCOPA_URI),
#     Q1 = quantile(SCOPA_URI, 0.25),
#     Q3 = quantile(SCOPA_URI, 0.75),
#     min=min(SCOPA_URI),
#     max=max(SCOPA_URI),
#     n=n()
#   )
# 
# # Change from baseline
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_URI)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
#   rename("Baseline"="SCOPA_URI") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_URI)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
#   ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAG) %>% summarise(mean= mean((SCOPA_URI-Baseline)/TIME_STUDY))
# 
# 
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_URI)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
#   rename("Baseline"="SCOPA_URI") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_URI)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
#   ) %>%
#   mutate(Diff= (SCOPA_URI-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAG) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_URI)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
#   rename("Baseline"="SCOPA_URI") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_URI)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
#   ) %>%
#   mutate(Diff= TIME_STUDY*12) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAG) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_URI)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
#   rename("Baseline"="SCOPA_URI") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_URI)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
#   ) %>%
#   mutate(Diff= (SCOPA_URI-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAG) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# # ALL SCOPA Total Year 2
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#   filter(!is.na(SCOPA_URI)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(SCOPA_URI),
#     sd=sd(SCOPA_URI),
#     median=median(SCOPA_URI),
#     Q1 = quantile(SCOPA_URI, 0.25),
#     Q3 = quantile(SCOPA_URI, 0.75),
#     min=min(SCOPA_URI),
#     max=max(SCOPA_URI),
#     n=n()
#   )
# 
# 
# # Change from baseline
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_URI)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
#   rename("Baseline"="SCOPA_URI") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_URI)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
#   ) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAG) %>% summarise(mean= mean((SCOPA_URI-Baseline)/TIME_STUDY))
# 
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_URI)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
#   rename("Baseline"="SCOPA_URI") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_URI)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
#   ) %>%
#   mutate(Diff= (SCOPA_URI-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAG) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_URI)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
#   rename("Baseline"="SCOPA_URI") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_URI)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
#   ) %>%
#   mutate(Diff= TIME_STUDY*12) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAG) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_URI)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
#   rename("Baseline"="SCOPA_URI") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_URI)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
#   ) %>%
#   mutate(Diff= (SCOPA_URI-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAG) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# # ALL SCOPA Total Year 3
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#   filter(!is.na(SCOPA_URI)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(SCOPA_URI),
#     sd=sd(SCOPA_URI),
#     median=median(SCOPA_URI),
#     Q1 = quantile(SCOPA_URI, 0.25),
#     Q3 = quantile(SCOPA_URI, 0.75),
#     min=min(SCOPA_URI),
#     max=max(SCOPA_URI),
#     n=n()
#   )
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_URI)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
#   rename("Baseline"="SCOPA_URI") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_URI)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
#   ) %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(mean= mean((SCOPA_URI-Baseline)/TIME_STUDY))
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_URI)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
#   rename("Baseline"="SCOPA_URI") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_URI)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
#   ) %>%
#   mutate(Diff= (SCOPA_URI-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAG) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_URI)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
#   rename("Baseline"="SCOPA_URI") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_URI)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
#   ) %>%
#   mutate(Diff= TIME_STUDY*12) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAG) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_URI)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
#   rename("Baseline"="SCOPA_URI") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_URI)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
#   ) %>%
#   mutate(Diff= (SCOPA_URI-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAG) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# # ------------
# # By Possible vs probable V2 URINARY --------------------
# 
# # ALL SCOPA Total Year 0
# 
# EarlyCT_Pop_Baseline_319 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_URI)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(SCOPA_URI),
#     sd=sd(SCOPA_URI),
#     median=median(SCOPA_URI),
#     Q1 = quantile(SCOPA_URI, 0.25),
#     Q3 = quantile(SCOPA_URI, 0.75),
#     min=min(SCOPA_URI),
#     max=max(SCOPA_URI),
#     n=n()
#   )
# 
# 
# 
# # ALL SCOPA Total Year 1
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#   filter(!is.na(SCOPA_URI)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(SCOPA_URI),
#     sd=sd(SCOPA_URI),
#     median=median(SCOPA_URI),
#     Q1 = quantile(SCOPA_URI, 0.25),
#     Q3 = quantile(SCOPA_URI, 0.75),
#     min=min(SCOPA_URI),
#     max=max(SCOPA_URI),
#     n=n()
#   )
# 
# # Change from baseline
# 
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_URI)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
#   rename("Baseline"="SCOPA_URI") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_URI)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
#   ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAGNIV) %>% summarise(mean= mean((SCOPA_URI-Baseline)/TIME_STUDY))
# 
# 
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_URI)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
#   rename("Baseline"="SCOPA_URI") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_URI)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
#   ) %>%
#   mutate(Diff= (SCOPA_URI-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAGNIV) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_URI)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
#   rename("Baseline"="SCOPA_URI") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_URI)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
#   ) %>%
#   mutate(Diff= TIME_STUDY*12) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAGNIV) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_URI)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
#   rename("Baseline"="SCOPA_URI") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_URI)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
#   ) %>%
#   mutate(Diff= (SCOPA_URI-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAGNIV) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# # ALL SCOPA Total Year 2
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#   filter(!is.na(SCOPA_URI)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(SCOPA_URI),
#     sd=sd(SCOPA_URI),
#     median=median(SCOPA_URI),
#     Q1 = quantile(SCOPA_URI, 0.25),
#     Q3 = quantile(SCOPA_URI, 0.75),
#     min=min(SCOPA_URI),
#     max=max(SCOPA_URI),
#     n=n()
#   )
# 
# 
# # Change from baseline
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_URI)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
#   rename("Baseline"="SCOPA_URI") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_URI)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
#   ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAGNIV) %>% summarise(mean= mean((SCOPA_URI-Baseline)/TIME_STUDY))
# 
# 
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_URI)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
#   rename("Baseline"="SCOPA_URI") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_URI)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
#   ) %>%
#   mutate(Diff= (SCOPA_URI-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAGNIV) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_URI)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
#   rename("Baseline"="SCOPA_URI") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_URI)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
#   ) %>%
#   mutate(Diff= TIME_STUDY*12) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAGNIV) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_URI)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
#   rename("Baseline"="SCOPA_URI") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_URI)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
#   ) %>%
#   mutate(Diff= (SCOPA_URI-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAGNIV) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# # ALL SCOPA Total Year 3
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#   filter(!is.na(SCOPA_URI)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(SCOPA_URI),
#     sd=sd(SCOPA_URI),
#     median=median(SCOPA_URI),
#     Q1 = quantile(SCOPA_URI, 0.25),
#     Q3 = quantile(SCOPA_URI, 0.75),
#     min=min(SCOPA_URI),
#     max=max(SCOPA_URI),
#     n=n()
#   )
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_URI)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
#   rename("Baseline"="SCOPA_URI") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_URI)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
#   ) %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(mean= mean((SCOPA_URI-Baseline)/TIME_STUDY))
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_URI)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
#   rename("Baseline"="SCOPA_URI") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_URI)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
#   ) %>%
#   mutate(Diff= (SCOPA_URI-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAGNIV) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_URI)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
#   rename("Baseline"="SCOPA_URI") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_URI)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
#   ) %>%
#   mutate(Diff= TIME_STUDY*12) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAGNIV) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_URI)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
#   rename("Baseline"="SCOPA_URI") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_URI)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
#   ) %>%
#   mutate(Diff= (SCOPA_URI-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAGNIV) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# # -----------------------------
# 
# 
# # Inputs SCOPA AUT V2 CARDIO ----------------------------------
# 
# 
# AllMSA_Pop_Baseline_671 <- fread("Source/AllMSA_Pop_Baseline_671.txt")
# AllMSA_Pop_BaselineYear1_410 <- fread("Source/AllMSA_Pop_BaselineYear1_410.txt")
# AllMSA_Pop_BaselineYear1Year2_245 <- fread("Source/AllMSA_Pop_BaselineYear1Year2_245.txt")
# AllMSA_Pop_BaselineYear1Year2Year3Plus_158 <- fread("Source/AllMSA_Pop_BaselineYear1Year2Year3Plus_158.txt")
# 
# EarlyCT_Pop_Baseline_319 <- fread("Source/EarlyCT_Pop_Baseline_319.txt")
# EarlyCT_Pop_BaselineYear1_208 <- fread("Source/EarlyCT_Pop_BaselineYear1_208.txt")
# EarlyCT_Pop_BaselineYear1Year2_134 <- fread("Source/EarlyCT_Pop_BaselineYear1Year2_134.txt")
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 <- fread( "Source/EarlyCT_Pop_BaselineYear1Year2Year3Plus_99.txt")
# 
# dataCohorteManaged <- readRDS("Source/dataCohorteManaged.rds")
# 
# dataCohorteManaged <- dataCohorteManaged %>% group_by(NUM) %>% mutate(TIME_STUDY = ifelse( is.na(TIME_STUDY), 0, TIME_STUDY)) %>%
#   mutate(Year= ifelse(TIME_STUDY==0, 0,
#                       ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
#                              ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
#                                     ifelse(TIME_STUDY>=2.5 ,3, NA)))))
# 
# 
# SCOPA <- dataCohorteManaged %>%
#   select(NUM, DATECONSULT, TIME_STUDY, Year) %>% rename("DATE"="DATECONSULT")
# 
# 
# 
# 
# ams_scopa_bx_2015_16 <- haven::read_sas("Source/ams_scopa_bx_2015_16.sas7bdat")
# names(ams_scopa_bx_2015_16)
# ams_scopa_bx_2015_16 <- ams_scopa_bx_2015_16 %>% filter(!is.na(DATE))
# 
# ams_scopa_bx_2015_16[is.na(ams_scopa_bx_2015_16)] <- 0
# 
# mean(ams_scopa_bx_2015_16$SCOPA_TOT, na.rm=T)
# 
# ams_scopa_bx_2015_16 <- ams_scopa_bx_2015_16 %>%
#   mutate(SCOPA_DIG=SCOPA1+SCOPA2+SCOPA3+SCOPA4+SCOPA5+SCOPA6+SCOPA7) %>%
#   mutate(SCOPA_URI=SCOPA8+SCOPA9+SCOPA10+SCOPA11+SCOPA12+SCOPA13) %>%
#   mutate(SCOPA_CDV=SCOPA14+SCOPA15+SCOPA16) %>%
#   mutate(SCOPA_THE=SCOPA17+SCOPA18+SCOPA19+SCOPA20) %>%
#   mutate(SCOPA_PUP=SCOPA21) %>%
#   mutate(SCOPA_SEX=SCOPA22H+SCOPA22F+SCOPA23F+SCOPA23H) %>%
#   mutate(SCOPA_TOT=SCOPA_SEX+SCOPA_PUP+SCOPA_THE+SCOPA_CDV+SCOPA_CDV+SCOPA_DIG)
# 
# ams_scopa_bx_2015_16 <- ams_scopa_bx_2015_16 %>%
#   select(NUM, DATE, SCOPA_TOT, SCOPA_SEX, SCOPA_PUP, SCOPA_THE, SCOPA_URI, SCOPA_CDV, SCOPA_DIG)
# 
# length(unique(ams_scopa_bx_2015_16$NUM))
# 
# 
# SCOPA <- ams_scopa_bx_2015_16 %>% inner_join(SCOPA)
# 
# SCOPA <- SCOPA %>% filter(SCOPA_TOT!=0)
# 
# length(unique(SCOPA$NUM))
# 
# 
# SCOPA_Baseline_Pats <- SCOPA %>% filter(Year==0) %>% filter(!is.na(SCOPA_TOT)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM) %>% distinct() %>%
#   inner_join(AllMSA_Pop_Baseline_671)
# 
# 
# 
# # ----------------
# # Overall Early CT V2 CARDIO  -------------------------------
# 
# # ALL SCOPA Total Year 0
# 
# 
# EarlyCT_Pop_Baseline_319 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_CDV)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(SCOPA_CDV),
#     sd=sd(SCOPA_CDV),
#     median=median(SCOPA_CDV),
#     Q1 = quantile(SCOPA_CDV, 0.25),
#     Q3 = quantile(SCOPA_CDV, 0.75),
#     min=min(SCOPA_CDV),
#     max=max(SCOPA_CDV),
#     n=n()
#   )
# 
# 
# 
# # ALL SCOPA Total Year 1
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#   filter(!is.na(SCOPA_CDV)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(SCOPA_CDV),
#     sd=sd(SCOPA_CDV),
#     median=median(SCOPA_CDV),
#     Q1 = quantile(SCOPA_CDV, 0.25),
#     Q3 = quantile(SCOPA_CDV, 0.75),
#     min=min(SCOPA_CDV),
#     max=max(SCOPA_CDV),
#     n=n()
#   )
# 
# # Change from baseline
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_CDV)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
#   rename("Baseline"="SCOPA_CDV") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_CDV)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
#   ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(mean= mean((SCOPA_CDV-Baseline)/TIME_STUDY))
# 
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_CDV)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
#   rename("Baseline"="SCOPA_CDV") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_CDV)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
#   ) %>%
#   mutate(Diff= (SCOPA_CDV-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_CDV)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
#   rename("Baseline"="SCOPA_CDV") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_CDV)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
#   ) %>%
#   mutate(Diff= TIME_STUDY*12) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_CDV)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
#   rename("Baseline"="SCOPA_CDV") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_CDV)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
#   ) %>%
#   mutate(Diff= (SCOPA_CDV-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# # ALL SCOPA Total Year 2
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#   filter(!is.na(SCOPA_CDV)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(SCOPA_CDV),
#     sd=sd(SCOPA_CDV),
#     median=median(SCOPA_CDV),
#     Q1 = quantile(SCOPA_CDV, 0.25),
#     Q3 = quantile(SCOPA_CDV, 0.75),
#     min=min(SCOPA_CDV),
#     max=max(SCOPA_CDV),
#     n=n()
#   )
# 
# 
# # Change from baseline
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_CDV)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
#   rename("Baseline"="SCOPA_CDV") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_CDV)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
#   ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(mean= mean((SCOPA_CDV-Baseline)/TIME_STUDY))
# 
# 
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_CDV)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
#   rename("Baseline"="SCOPA_CDV") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_CDV)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
#   ) %>%
#   mutate(Diff= (SCOPA_CDV-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_CDV)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
#   rename("Baseline"="SCOPA_CDV") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_CDV)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
#   ) %>%
#   mutate(Diff= TIME_STUDY*12) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_CDV)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
#   rename("Baseline"="SCOPA_CDV") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_CDV)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
#   ) %>%
#   mutate(Diff= (SCOPA_CDV-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# # ALL SCOPA Total Year 3
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#   filter(!is.na(SCOPA_CDV)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(SCOPA_CDV),
#     sd=sd(SCOPA_CDV),
#     median=median(SCOPA_CDV),
#     Q1 = quantile(SCOPA_CDV, 0.25),
#     Q3 = quantile(SCOPA_CDV, 0.75),
#     min=min(SCOPA_CDV),
#     max=max(SCOPA_CDV),
#     n=n()
#   )
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_CDV)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
#   rename("Baseline"="SCOPA_CDV") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_CDV)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
#   ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(mean= mean((SCOPA_CDV-Baseline)/TIME_STUDY))
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_CDV)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
#   rename("Baseline"="SCOPA_CDV") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_CDV)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
#   ) %>%
#   mutate(Diff= (SCOPA_CDV-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_CDV)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
#   rename("Baseline"="SCOPA_CDV") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_CDV)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
#   ) %>%
#   mutate(Diff= TIME_STUDY*12) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_CDV)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
#   rename("Baseline"="SCOPA_CDV") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_CDV)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
#   ) %>%
#   mutate(Diff= (SCOPA_CDV-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# # --------------
# 
# 
# # By MSA P vs MSA C V2 CARDIO -----------------
# # ALL SCOPA Total Year 0
# 
# EarlyCT_Pop_Baseline_319 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_CDV)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(SCOPA_CDV),
#     sd=sd(SCOPA_CDV),
#     median=median(SCOPA_CDV),
#     Q1 = quantile(SCOPA_CDV, 0.25),
#     Q3 = quantile(SCOPA_CDV, 0.75),
#     min=min(SCOPA_CDV),
#     max=max(SCOPA_CDV),
#     n=n()
#   )
# 
# 
# 
# # ALL SCOPA Total Year 1
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#   filter(!is.na(SCOPA_CDV)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(SCOPA_CDV),
#     sd=sd(SCOPA_CDV),
#     median=median(SCOPA_CDV),
#     Q1 = quantile(SCOPA_CDV, 0.25),
#     Q3 = quantile(SCOPA_CDV, 0.75),
#     min=min(SCOPA_CDV),
#     max=max(SCOPA_CDV),
#     n=n()
#   )
# 
# # Change from baseline
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_CDV)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
#   rename("Baseline"="SCOPA_CDV") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_CDV)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
#   ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAG) %>% summarise(mean= mean((SCOPA_CDV-Baseline)/TIME_STUDY))
# 
# 
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_CDV)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
#   rename("Baseline"="SCOPA_CDV") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_CDV)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
#   ) %>%
#   mutate(Diff= (SCOPA_CDV-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAG) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_CDV)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
#   rename("Baseline"="SCOPA_CDV") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_CDV)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
#   ) %>%
#   mutate(Diff= TIME_STUDY*12) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAG) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_CDV)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
#   rename("Baseline"="SCOPA_CDV") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_CDV)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
#   ) %>%
#   mutate(Diff= (SCOPA_CDV-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAG) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# # ALL SCOPA Total Year 2
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#   filter(!is.na(SCOPA_CDV)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(SCOPA_CDV),
#     sd=sd(SCOPA_CDV),
#     median=median(SCOPA_CDV),
#     Q1 = quantile(SCOPA_CDV, 0.25),
#     Q3 = quantile(SCOPA_CDV, 0.75),
#     min=min(SCOPA_CDV),
#     max=max(SCOPA_CDV),
#     n=n()
#   )
# 
# 
# # Change from baseline
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_CDV)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
#   rename("Baseline"="SCOPA_CDV") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_CDV)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
#   ) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAG) %>% summarise(mean= mean((SCOPA_CDV-Baseline)/TIME_STUDY))
# 
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_CDV)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
#   rename("Baseline"="SCOPA_CDV") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_CDV)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
#   ) %>%
#   mutate(Diff= (SCOPA_CDV-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAG) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_CDV)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
#   rename("Baseline"="SCOPA_CDV") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_CDV)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
#   ) %>%
#   mutate(Diff= TIME_STUDY*12) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAG) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_CDV)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
#   rename("Baseline"="SCOPA_CDV") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_CDV)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
#   ) %>%
#   mutate(Diff= (SCOPA_CDV-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAG) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# # ALL SCOPA Total Year 3
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#   filter(!is.na(SCOPA_CDV)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(SCOPA_CDV),
#     sd=sd(SCOPA_CDV),
#     median=median(SCOPA_CDV),
#     Q1 = quantile(SCOPA_CDV, 0.25),
#     Q3 = quantile(SCOPA_CDV, 0.75),
#     min=min(SCOPA_CDV),
#     max=max(SCOPA_CDV),
#     n=n()
#   )
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_CDV)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
#   rename("Baseline"="SCOPA_CDV") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_CDV)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
#   ) %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(mean= mean((SCOPA_CDV-Baseline)/TIME_STUDY))
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_CDV)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
#   rename("Baseline"="SCOPA_CDV") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_CDV)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
#   ) %>%
#   mutate(Diff= (SCOPA_CDV-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAG) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_CDV)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
#   rename("Baseline"="SCOPA_CDV") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_CDV)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
#   ) %>%
#   mutate(Diff= TIME_STUDY*12) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAG) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_CDV)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
#   rename("Baseline"="SCOPA_CDV") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_CDV)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
#   ) %>%
#   mutate(Diff= (SCOPA_CDV-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAG) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# # ------------
# # By Possible vs probable V2 CARDIO --------------------
# 
# # ALL SCOPA Total Year 0
# 
# EarlyCT_Pop_Baseline_319 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_CDV)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(SCOPA_CDV),
#     sd=sd(SCOPA_CDV),
#     median=median(SCOPA_CDV),
#     Q1 = quantile(SCOPA_CDV, 0.25),
#     Q3 = quantile(SCOPA_CDV, 0.75),
#     min=min(SCOPA_CDV),
#     max=max(SCOPA_CDV),
#     n=n()
#   )
# 
# 
# 
# # ALL SCOPA Total Year 1
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#   filter(!is.na(SCOPA_CDV)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(SCOPA_CDV),
#     sd=sd(SCOPA_CDV),
#     median=median(SCOPA_CDV),
#     Q1 = quantile(SCOPA_CDV, 0.25),
#     Q3 = quantile(SCOPA_CDV, 0.75),
#     min=min(SCOPA_CDV),
#     max=max(SCOPA_CDV),
#     n=n()
#   )
# 
# # Change from baseline
# 
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_CDV)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
#   rename("Baseline"="SCOPA_CDV") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_CDV)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
#   ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAGNIV) %>% summarise(mean= mean((SCOPA_CDV-Baseline)/TIME_STUDY))
# 
# 
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_CDV)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
#   rename("Baseline"="SCOPA_CDV") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_CDV)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
#   ) %>%
#   mutate(Diff= (SCOPA_CDV-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAGNIV) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_CDV)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
#   rename("Baseline"="SCOPA_CDV") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_CDV)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
#   ) %>%
#   mutate(Diff= TIME_STUDY*12) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAGNIV) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_CDV)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
#   rename("Baseline"="SCOPA_CDV") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_CDV)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
#   ) %>%
#   mutate(Diff= (SCOPA_CDV-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAGNIV) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# # ALL SCOPA Total Year 2
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#   filter(!is.na(SCOPA_CDV)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(SCOPA_CDV),
#     sd=sd(SCOPA_CDV),
#     median=median(SCOPA_CDV),
#     Q1 = quantile(SCOPA_CDV, 0.25),
#     Q3 = quantile(SCOPA_CDV, 0.75),
#     min=min(SCOPA_CDV),
#     max=max(SCOPA_CDV),
#     n=n()
#   )
# 
# 
# # Change from baseline
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_CDV)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
#   rename("Baseline"="SCOPA_CDV") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_CDV)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
#   ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAGNIV) %>% summarise(mean= mean((SCOPA_CDV-Baseline)/TIME_STUDY))
# 
# 
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_CDV)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
#   rename("Baseline"="SCOPA_CDV") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_CDV)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
#   ) %>%
#   mutate(Diff= (SCOPA_CDV-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAGNIV) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_CDV)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
#   rename("Baseline"="SCOPA_CDV") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_CDV)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
#   ) %>%
#   mutate(Diff= TIME_STUDY*12) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAGNIV) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_CDV)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
#   rename("Baseline"="SCOPA_CDV") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_CDV)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
#   ) %>%
#   mutate(Diff= (SCOPA_CDV-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAGNIV) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# # ALL SCOPA Total Year 3
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#   filter(!is.na(SCOPA_CDV)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(SCOPA_CDV),
#     sd=sd(SCOPA_CDV),
#     median=median(SCOPA_CDV),
#     Q1 = quantile(SCOPA_CDV, 0.25),
#     Q3 = quantile(SCOPA_CDV, 0.75),
#     min=min(SCOPA_CDV),
#     max=max(SCOPA_CDV),
#     n=n()
#   )
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_CDV)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
#   rename("Baseline"="SCOPA_CDV") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_CDV)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
#   ) %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(mean= mean((SCOPA_CDV-Baseline)/TIME_STUDY))
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_CDV)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
#   rename("Baseline"="SCOPA_CDV") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_CDV)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
#   ) %>%
#   mutate(Diff= (SCOPA_CDV-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAGNIV) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_CDV)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
#   rename("Baseline"="SCOPA_CDV") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_CDV)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
#   ) %>%
#   mutate(Diff= TIME_STUDY*12) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAGNIV) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_CDV)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
#   rename("Baseline"="SCOPA_CDV") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_CDV)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
#   ) %>%
#   mutate(Diff= (SCOPA_CDV-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAGNIV) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# # -----------------------------
# 
# 
# # Inputs SCOPA AUT V2 THERMOREGULATORY  ----------------------------------
# 
# 
# AllMSA_Pop_Baseline_671 <- fread("Source/AllMSA_Pop_Baseline_671.txt")
# AllMSA_Pop_BaselineYear1_410 <- fread("Source/AllMSA_Pop_BaselineYear1_410.txt")
# AllMSA_Pop_BaselineYear1Year2_245 <- fread("Source/AllMSA_Pop_BaselineYear1Year2_245.txt")
# AllMSA_Pop_BaselineYear1Year2Year3Plus_158 <- fread("Source/AllMSA_Pop_BaselineYear1Year2Year3Plus_158.txt")
# 
# EarlyCT_Pop_Baseline_319 <- fread("Source/EarlyCT_Pop_Baseline_319.txt")
# EarlyCT_Pop_BaselineYear1_208 <- fread("Source/EarlyCT_Pop_BaselineYear1_208.txt")
# EarlyCT_Pop_BaselineYear1Year2_134 <- fread("Source/EarlyCT_Pop_BaselineYear1Year2_134.txt")
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 <- fread( "Source/EarlyCT_Pop_BaselineYear1Year2Year3Plus_99.txt")
# 
# dataCohorteManaged <- readRDS("Source/dataCohorteManaged.rds")
# 
# dataCohorteManaged <- dataCohorteManaged %>% group_by(NUM) %>% mutate(TIME_STUDY = ifelse( is.na(TIME_STUDY), 0, TIME_STUDY)) %>%
#   mutate(Year= ifelse(TIME_STUDY==0, 0,
#                       ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
#                              ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
#                                     ifelse(TIME_STUDY>=2.5 ,3, NA)))))
# 
# 
# SCOPA <- dataCohorteManaged %>%
#   select(NUM, DATECONSULT, TIME_STUDY, Year) %>% rename("DATE"="DATECONSULT")
# 
# 
# 
# 
# ams_scopa_bx_2015_16 <- haven::read_sas("Source/ams_scopa_bx_2015_16.sas7bdat")
# names(ams_scopa_bx_2015_16)
# ams_scopa_bx_2015_16 <- ams_scopa_bx_2015_16 %>% filter(!is.na(DATE))
# 
# ams_scopa_bx_2015_16[is.na(ams_scopa_bx_2015_16)] <- 0
# 
# mean(ams_scopa_bx_2015_16$SCOPA_TOT, na.rm=T)
# 
# ams_scopa_bx_2015_16 <- ams_scopa_bx_2015_16 %>%
#   mutate(SCOPA_DIG=SCOPA1+SCOPA2+SCOPA3+SCOPA4+SCOPA5+SCOPA6+SCOPA7) %>%
#   mutate(SCOPA_URI=SCOPA8+SCOPA9+SCOPA10+SCOPA11+SCOPA12+SCOPA13) %>%
#   mutate(SCOPA_CDV=SCOPA14+SCOPA15+SCOPA16) %>%
#   mutate(SCOPA_THE=SCOPA17+SCOPA18+SCOPA19+SCOPA20) %>%
#   mutate(SCOPA_PUP=SCOPA21) %>%
#   mutate(SCOPA_SEX=SCOPA22H+SCOPA22F+SCOPA23F+SCOPA23H) %>%
#   mutate(SCOPA_TOT=SCOPA_SEX+SCOPA_PUP+SCOPA_THE+SCOPA_THE+SCOPA_THE+SCOPA_DIG)
# 
# ams_scopa_bx_2015_16 <- ams_scopa_bx_2015_16 %>%
#   select(NUM, DATE, SCOPA_TOT, SCOPA_SEX, SCOPA_PUP, SCOPA_THE, SCOPA_URI, SCOPA_CDV, SCOPA_DIG)
# 
# length(unique(ams_scopa_bx_2015_16$NUM))
# 
# 
# SCOPA <- ams_scopa_bx_2015_16 %>% inner_join(SCOPA)
# 
# SCOPA <- SCOPA %>% filter(SCOPA_TOT!=0)
# 
# length(unique(SCOPA$NUM))
# 
# 
# SCOPA_Baseline_Pats <- SCOPA %>% filter(Year==0) %>% filter(!is.na(SCOPA_TOT)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM) %>% distinct() %>%
#   inner_join(AllMSA_Pop_Baseline_671)
# 
# 
# 
# # ----------------
# # Overall Early CT V2 THERMOREGULATORY  -------------------------------
# 
# # ALL SCOPA Total Year 0
# 
# 
# EarlyCT_Pop_Baseline_319 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_THE)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(SCOPA_THE),
#     sd=sd(SCOPA_THE),
#     median=median(SCOPA_THE),
#     Q1 = quantile(SCOPA_THE, 0.25),
#     Q3 = quantile(SCOPA_THE, 0.75),
#     min=min(SCOPA_THE),
#     max=max(SCOPA_THE),
#     n=n()
#   )
# 
# 
# 
# # ALL SCOPA Total Year 1
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#   filter(!is.na(SCOPA_THE)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(SCOPA_THE),
#     sd=sd(SCOPA_THE),
#     median=median(SCOPA_THE),
#     Q1 = quantile(SCOPA_THE, 0.25),
#     Q3 = quantile(SCOPA_THE, 0.75),
#     min=min(SCOPA_THE),
#     max=max(SCOPA_THE),
#     n=n()
#   )
# 
# # Change from baseline
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_THE)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
#   rename("Baseline"="SCOPA_THE") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_THE)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
#   ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(mean= mean((SCOPA_THE-Baseline)/TIME_STUDY))
# 
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_THE)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
#   rename("Baseline"="SCOPA_THE") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_THE)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
#   ) %>%
#   mutate(Diff= (SCOPA_THE-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_THE)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
#   rename("Baseline"="SCOPA_THE") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_THE)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
#   ) %>%
#   mutate(Diff= TIME_STUDY*12) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_THE)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
#   rename("Baseline"="SCOPA_THE") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_THE)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
#   ) %>%
#   mutate(Diff= (SCOPA_THE-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# # ALL SCOPA Total Year 2
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#   filter(!is.na(SCOPA_THE)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(SCOPA_THE),
#     sd=sd(SCOPA_THE),
#     median=median(SCOPA_THE),
#     Q1 = quantile(SCOPA_THE, 0.25),
#     Q3 = quantile(SCOPA_THE, 0.75),
#     min=min(SCOPA_THE),
#     max=max(SCOPA_THE),
#     n=n()
#   )
# 
# 
# # Change from baseline
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_THE)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
#   rename("Baseline"="SCOPA_THE") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_THE)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
#   ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(mean= mean((SCOPA_THE-Baseline)/TIME_STUDY))
# 
# 
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_THE)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
#   rename("Baseline"="SCOPA_THE") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_THE)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
#   ) %>%
#   mutate(Diff= (SCOPA_THE-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_THE)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
#   rename("Baseline"="SCOPA_THE") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_THE)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
#   ) %>%
#   mutate(Diff= TIME_STUDY*12) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_THE)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
#   rename("Baseline"="SCOPA_THE") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_THE)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
#   ) %>%
#   mutate(Diff= (SCOPA_THE-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# # ALL SCOPA Total Year 3
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#   filter(!is.na(SCOPA_THE)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(SCOPA_THE),
#     sd=sd(SCOPA_THE),
#     median=median(SCOPA_THE),
#     Q1 = quantile(SCOPA_THE, 0.25),
#     Q3 = quantile(SCOPA_THE, 0.75),
#     min=min(SCOPA_THE),
#     max=max(SCOPA_THE),
#     n=n()
#   )
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_THE)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
#   rename("Baseline"="SCOPA_THE") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_THE)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
#   ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(mean= mean((SCOPA_THE-Baseline)/TIME_STUDY))
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_THE)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
#   rename("Baseline"="SCOPA_THE") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_THE)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
#   ) %>%
#   mutate(Diff= (SCOPA_THE-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_THE)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
#   rename("Baseline"="SCOPA_THE") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_THE)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
#   ) %>%
#   mutate(Diff= TIME_STUDY*12) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_THE)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
#   rename("Baseline"="SCOPA_THE") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_THE)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
#   ) %>%
#   mutate(Diff= (SCOPA_THE-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# # --------------
# 
# 
# # By MSA P vs MSA C V2 THERMOREGULATORY -----------------
# # ALL SCOPA Total Year 0
# 
# EarlyCT_Pop_Baseline_319 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_THE)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(SCOPA_THE),
#     sd=sd(SCOPA_THE),
#     median=median(SCOPA_THE),
#     Q1 = quantile(SCOPA_THE, 0.25),
#     Q3 = quantile(SCOPA_THE, 0.75),
#     min=min(SCOPA_THE),
#     max=max(SCOPA_THE),
#     n=n()
#   )
# 
# 
# 
# # ALL SCOPA Total Year 1
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#   filter(!is.na(SCOPA_THE)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(SCOPA_THE),
#     sd=sd(SCOPA_THE),
#     median=median(SCOPA_THE),
#     Q1 = quantile(SCOPA_THE, 0.25),
#     Q3 = quantile(SCOPA_THE, 0.75),
#     min=min(SCOPA_THE),
#     max=max(SCOPA_THE),
#     n=n()
#   )
# 
# # Change from baseline
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_THE)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
#   rename("Baseline"="SCOPA_THE") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_THE)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
#   ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAG) %>% summarise(mean= mean((SCOPA_THE-Baseline)/TIME_STUDY))
# 
# 
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_THE)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
#   rename("Baseline"="SCOPA_THE") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_THE)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
#   ) %>%
#   mutate(Diff= (SCOPA_THE-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAG) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_THE)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
#   rename("Baseline"="SCOPA_THE") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_THE)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
#   ) %>%
#   mutate(Diff= TIME_STUDY*12) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAG) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_THE)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
#   rename("Baseline"="SCOPA_THE") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_THE)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
#   ) %>%
#   mutate(Diff= (SCOPA_THE-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAG) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# # ALL SCOPA Total Year 2
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#   filter(!is.na(SCOPA_THE)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(SCOPA_THE),
#     sd=sd(SCOPA_THE),
#     median=median(SCOPA_THE),
#     Q1 = quantile(SCOPA_THE, 0.25),
#     Q3 = quantile(SCOPA_THE, 0.75),
#     min=min(SCOPA_THE),
#     max=max(SCOPA_THE),
#     n=n()
#   )
# 
# 
# # Change from baseline
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_THE)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
#   rename("Baseline"="SCOPA_THE") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_THE)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
#   ) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAG) %>% summarise(mean= mean((SCOPA_THE-Baseline)/TIME_STUDY))
# 
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_THE)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
#   rename("Baseline"="SCOPA_THE") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_THE)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
#   ) %>%
#   mutate(Diff= (SCOPA_THE-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAG) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_THE)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
#   rename("Baseline"="SCOPA_THE") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_THE)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
#   ) %>%
#   mutate(Diff= TIME_STUDY*12) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAG) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_THE)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
#   rename("Baseline"="SCOPA_THE") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_THE)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
#   ) %>%
#   mutate(Diff= (SCOPA_THE-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAG) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# # ALL SCOPA Total Year 3
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#   filter(!is.na(SCOPA_THE)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(SCOPA_THE),
#     sd=sd(SCOPA_THE),
#     median=median(SCOPA_THE),
#     Q1 = quantile(SCOPA_THE, 0.25),
#     Q3 = quantile(SCOPA_THE, 0.75),
#     min=min(SCOPA_THE),
#     max=max(SCOPA_THE),
#     n=n()
#   )
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_THE)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
#   rename("Baseline"="SCOPA_THE") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_THE)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
#   ) %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(mean= mean((SCOPA_THE-Baseline)/TIME_STUDY))
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_THE)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
#   rename("Baseline"="SCOPA_THE") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_THE)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
#   ) %>%
#   mutate(Diff= (SCOPA_THE-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAG) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_THE)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
#   rename("Baseline"="SCOPA_THE") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_THE)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
#   ) %>%
#   mutate(Diff= TIME_STUDY*12) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAG) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_THE)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
#   rename("Baseline"="SCOPA_THE") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_THE)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
#   ) %>%
#   mutate(Diff= (SCOPA_THE-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAG) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# # ------------
# # By Possible vs probable V2 THERMOREGULATORY --------------------
# 
# # ALL SCOPA Total Year 0
# 
# EarlyCT_Pop_Baseline_319 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_THE)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(SCOPA_THE),
#     sd=sd(SCOPA_THE),
#     median=median(SCOPA_THE),
#     Q1 = quantile(SCOPA_THE, 0.25),
#     Q3 = quantile(SCOPA_THE, 0.75),
#     min=min(SCOPA_THE),
#     max=max(SCOPA_THE),
#     n=n()
#   )
# 
# 
# 
# # ALL SCOPA Total Year 1
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#   filter(!is.na(SCOPA_THE)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(SCOPA_THE),
#     sd=sd(SCOPA_THE),
#     median=median(SCOPA_THE),
#     Q1 = quantile(SCOPA_THE, 0.25),
#     Q3 = quantile(SCOPA_THE, 0.75),
#     min=min(SCOPA_THE),
#     max=max(SCOPA_THE),
#     n=n()
#   )
# 
# # Change from baseline
# 
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_THE)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
#   rename("Baseline"="SCOPA_THE") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_THE)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
#   ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAGNIV) %>% summarise(mean= mean((SCOPA_THE-Baseline)/TIME_STUDY))
# 
# 
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_THE)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
#   rename("Baseline"="SCOPA_THE") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_THE)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
#   ) %>%
#   mutate(Diff= (SCOPA_THE-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAGNIV) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_THE)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
#   rename("Baseline"="SCOPA_THE") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_THE)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
#   ) %>%
#   mutate(Diff= TIME_STUDY*12) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAGNIV) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_THE)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
#   rename("Baseline"="SCOPA_THE") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_THE)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
#   ) %>%
#   mutate(Diff= (SCOPA_THE-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAGNIV) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# # ALL SCOPA Total Year 2
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#   filter(!is.na(SCOPA_THE)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(SCOPA_THE),
#     sd=sd(SCOPA_THE),
#     median=median(SCOPA_THE),
#     Q1 = quantile(SCOPA_THE, 0.25),
#     Q3 = quantile(SCOPA_THE, 0.75),
#     min=min(SCOPA_THE),
#     max=max(SCOPA_THE),
#     n=n()
#   )
# 
# 
# # Change from baseline
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_THE)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
#   rename("Baseline"="SCOPA_THE") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_THE)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
#   ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAGNIV) %>% summarise(mean= mean((SCOPA_THE-Baseline)/TIME_STUDY))
# 
# 
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_THE)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
#   rename("Baseline"="SCOPA_THE") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_THE)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
#   ) %>%
#   mutate(Diff= (SCOPA_THE-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAGNIV) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_THE)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
#   rename("Baseline"="SCOPA_THE") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_THE)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
#   ) %>%
#   mutate(Diff= TIME_STUDY*12) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAGNIV) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_THE)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
#   rename("Baseline"="SCOPA_THE") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_THE)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
#   ) %>%
#   mutate(Diff= (SCOPA_THE-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAGNIV) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# # ALL SCOPA Total Year 3
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#   filter(!is.na(SCOPA_THE)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(SCOPA_THE),
#     sd=sd(SCOPA_THE),
#     median=median(SCOPA_THE),
#     Q1 = quantile(SCOPA_THE, 0.25),
#     Q3 = quantile(SCOPA_THE, 0.75),
#     min=min(SCOPA_THE),
#     max=max(SCOPA_THE),
#     n=n()
#   )
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_THE)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
#   rename("Baseline"="SCOPA_THE") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_THE)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
#   ) %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(mean= mean((SCOPA_THE-Baseline)/TIME_STUDY))
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_THE)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
#   rename("Baseline"="SCOPA_THE") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_THE)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
#   ) %>%
#   mutate(Diff= (SCOPA_THE-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAGNIV) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_THE)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
#   rename("Baseline"="SCOPA_THE") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_THE)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
#   ) %>%
#   mutate(Diff= TIME_STUDY*12) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAGNIV) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_THE)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
#   rename("Baseline"="SCOPA_THE") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_THE)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
#   ) %>%
#   mutate(Diff= (SCOPA_THE-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAGNIV) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# # -----------------------------
# 
# 
# # Inputs SCOPA AUT V2 Pupillomotor   ----------------------------------
# 
# 
# AllMSA_Pop_Baseline_671 <- fread("Source/AllMSA_Pop_Baseline_671.txt")
# AllMSA_Pop_BaselineYear1_410 <- fread("Source/AllMSA_Pop_BaselineYear1_410.txt")
# AllMSA_Pop_BaselineYear1Year2_245 <- fread("Source/AllMSA_Pop_BaselineYear1Year2_245.txt")
# AllMSA_Pop_BaselineYear1Year2Year3Plus_158 <- fread("Source/AllMSA_Pop_BaselineYear1Year2Year3Plus_158.txt")
# 
# EarlyCT_Pop_Baseline_319 <- fread("Source/EarlyCT_Pop_Baseline_319.txt")
# EarlyCT_Pop_BaselineYear1_208 <- fread("Source/EarlyCT_Pop_BaselineYear1_208.txt")
# EarlyCT_Pop_BaselineYear1Year2_134 <- fread("Source/EarlyCT_Pop_BaselineYear1Year2_134.txt")
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 <- fread( "Source/EarlyCT_Pop_BaselineYear1Year2Year3Plus_99.txt")
# 
# dataCohorteManaged <- readRDS("Source/dataCohorteManaged.rds")
# 
# dataCohorteManaged <- dataCohorteManaged %>% group_by(NUM) %>% mutate(TIME_STUDY = ifelse( is.na(TIME_STUDY), 0, TIME_STUDY)) %>%
#   mutate(Year= ifelse(TIME_STUDY==0, 0,
#                       ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
#                              ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
#                                     ifelse(TIME_STUDY>=2.5 ,3, NA)))))
# 
# 
# SCOPA <- dataCohorteManaged %>%
#   select(NUM, DATECONSULT, TIME_STUDY, Year) %>% rename("DATE"="DATECONSULT")
# 
# 
# 
# 
# ams_scopa_bx_2015_16 <- haven::read_sas("Source/ams_scopa_bx_2015_16.sas7bdat")
# names(ams_scopa_bx_2015_16)
# ams_scopa_bx_2015_16 <- ams_scopa_bx_2015_16 %>% filter(!is.na(DATE))
# 
# ams_scopa_bx_2015_16[is.na(ams_scopa_bx_2015_16)] <- 0
# 
# mean(ams_scopa_bx_2015_16$SCOPA_TOT, na.rm=T)
# 
# ams_scopa_bx_2015_16 <- ams_scopa_bx_2015_16 %>%
#   mutate(SCOPA_DIG=SCOPA1+SCOPA2+SCOPA3+SCOPA4+SCOPA5+SCOPA6+SCOPA7) %>%
#   mutate(SCOPA_URI=SCOPA8+SCOPA9+SCOPA10+SCOPA11+SCOPA12+SCOPA13) %>%
#   mutate(SCOPA_CDV=SCOPA14+SCOPA15+SCOPA16) %>%
#   mutate(SCOPA_THE=SCOPA17+SCOPA18+SCOPA19+SCOPA20) %>%
#   mutate(SCOPA_PUP=SCOPA21) %>%
#   mutate(SCOPA_SEX=SCOPA22H+SCOPA22F+SCOPA23F+SCOPA23H) %>%
#   mutate(SCOPA_TOT=SCOPA_SEX+SCOPA_PUP+SCOPA_PUP+SCOPA_PUP+SCOPA_PUP+SCOPA_DIG)
# 
# ams_scopa_bx_2015_16 <- ams_scopa_bx_2015_16 %>%
#   select(NUM, DATE, SCOPA_TOT, SCOPA_SEX, SCOPA_PUP, SCOPA_THE, SCOPA_URI, SCOPA_CDV, SCOPA_DIG)
# 
# length(unique(ams_scopa_bx_2015_16$NUM))
# 
# 
# SCOPA <- ams_scopa_bx_2015_16 %>% inner_join(SCOPA)
# 
# SCOPA <- SCOPA %>% filter(SCOPA_TOT!=0)
# 
# length(unique(SCOPA$NUM))
# 
# 
# SCOPA_Baseline_Pats <- SCOPA %>% filter(Year==0) %>% filter(!is.na(SCOPA_TOT)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM) %>% distinct() %>%
#   inner_join(AllMSA_Pop_Baseline_671)
# 
# 
# 
# # ----------------
# # Overall Early CT V2 Pupillomotor  -------------------------------
# 
# # ALL SCOPA Total Year 0
# 
# 
# EarlyCT_Pop_Baseline_319 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_PUP)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(SCOPA_PUP),
#     sd=sd(SCOPA_PUP),
#     median=median(SCOPA_PUP),
#     Q1 = quantile(SCOPA_PUP, 0.25),
#     Q3 = quantile(SCOPA_PUP, 0.75),
#     min=min(SCOPA_PUP),
#     max=max(SCOPA_PUP),
#     n=n()
#   )
# 
# 
# 
# # ALL SCOPA Total Year 1
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#   filter(!is.na(SCOPA_PUP)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(SCOPA_PUP),
#     sd=sd(SCOPA_PUP),
#     median=median(SCOPA_PUP),
#     Q1 = quantile(SCOPA_PUP, 0.25),
#     Q3 = quantile(SCOPA_PUP, 0.75),
#     min=min(SCOPA_PUP),
#     max=max(SCOPA_PUP),
#     n=n()
#   )
# 
# # Change from baseline
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_PUP)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
#   rename("Baseline"="SCOPA_PUP") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_PUP)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
#   ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(mean= mean((SCOPA_PUP-Baseline)/TIME_STUDY))
# 
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_PUP)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
#   rename("Baseline"="SCOPA_PUP") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_PUP)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
#   ) %>%
#   mutate(Diff= (SCOPA_PUP-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_PUP)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
#   rename("Baseline"="SCOPA_PUP") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_PUP)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
#   ) %>%
#   mutate(Diff= TIME_STUDY*12) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_PUP)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
#   rename("Baseline"="SCOPA_PUP") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_PUP)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
#   ) %>%
#   mutate(Diff= (SCOPA_PUP-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# # ALL SCOPA Total Year 2
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#   filter(!is.na(SCOPA_PUP)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(SCOPA_PUP),
#     sd=sd(SCOPA_PUP),
#     median=median(SCOPA_PUP),
#     Q1 = quantile(SCOPA_PUP, 0.25),
#     Q3 = quantile(SCOPA_PUP, 0.75),
#     min=min(SCOPA_PUP),
#     max=max(SCOPA_PUP),
#     n=n()
#   )
# 
# 
# # Change from baseline
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_PUP)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
#   rename("Baseline"="SCOPA_PUP") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_PUP)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
#   ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(mean= mean((SCOPA_PUP-Baseline)/TIME_STUDY))
# 
# 
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_PUP)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
#   rename("Baseline"="SCOPA_PUP") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_PUP)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
#   ) %>%
#   mutate(Diff= (SCOPA_PUP-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_PUP)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
#   rename("Baseline"="SCOPA_PUP") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_PUP)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
#   ) %>%
#   mutate(Diff= TIME_STUDY*12) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_PUP)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
#   rename("Baseline"="SCOPA_PUP") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_PUP)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
#   ) %>%
#   mutate(Diff= (SCOPA_PUP-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# # ALL SCOPA Total Year 3
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#   filter(!is.na(SCOPA_PUP)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(SCOPA_PUP),
#     sd=sd(SCOPA_PUP),
#     median=median(SCOPA_PUP),
#     Q1 = quantile(SCOPA_PUP, 0.25),
#     Q3 = quantile(SCOPA_PUP, 0.75),
#     min=min(SCOPA_PUP),
#     max=max(SCOPA_PUP),
#     n=n()
#   )
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_PUP)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
#   rename("Baseline"="SCOPA_PUP") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_PUP)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
#   ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(mean= mean((SCOPA_PUP-Baseline)/TIME_STUDY))
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_PUP)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
#   rename("Baseline"="SCOPA_PUP") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_PUP)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
#   ) %>%
#   mutate(Diff= (SCOPA_PUP-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_PUP)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
#   rename("Baseline"="SCOPA_PUP") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_PUP)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
#   ) %>%
#   mutate(Diff= TIME_STUDY*12) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_PUP)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
#   rename("Baseline"="SCOPA_PUP") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_PUP)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
#   ) %>%
#   mutate(Diff= (SCOPA_PUP-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# # --------------
# 
# 
# # By MSA P vs MSA C V2 Pupillomotor -----------------
# # ALL SCOPA Total Year 0
# 
# EarlyCT_Pop_Baseline_319 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_PUP)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(SCOPA_PUP),
#     sd=sd(SCOPA_PUP),
#     median=median(SCOPA_PUP),
#     Q1 = quantile(SCOPA_PUP, 0.25),
#     Q3 = quantile(SCOPA_PUP, 0.75),
#     min=min(SCOPA_PUP),
#     max=max(SCOPA_PUP),
#     n=n()
#   )
# 
# 
# 
# # ALL SCOPA Total Year 1
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#   filter(!is.na(SCOPA_PUP)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(SCOPA_PUP),
#     sd=sd(SCOPA_PUP),
#     median=median(SCOPA_PUP),
#     Q1 = quantile(SCOPA_PUP, 0.25),
#     Q3 = quantile(SCOPA_PUP, 0.75),
#     min=min(SCOPA_PUP),
#     max=max(SCOPA_PUP),
#     n=n()
#   )
# 
# # Change from baseline
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_PUP)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
#   rename("Baseline"="SCOPA_PUP") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_PUP)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
#   ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAG) %>% summarise(mean= mean((SCOPA_PUP-Baseline)/TIME_STUDY))
# 
# 
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_PUP)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
#   rename("Baseline"="SCOPA_PUP") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_PUP)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
#   ) %>%
#   mutate(Diff= (SCOPA_PUP-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAG) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_PUP)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
#   rename("Baseline"="SCOPA_PUP") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_PUP)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
#   ) %>%
#   mutate(Diff= TIME_STUDY*12) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAG) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_PUP)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
#   rename("Baseline"="SCOPA_PUP") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_PUP)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
#   ) %>%
#   mutate(Diff= (SCOPA_PUP-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAG) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# # ALL SCOPA Total Year 2
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#   filter(!is.na(SCOPA_PUP)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(SCOPA_PUP),
#     sd=sd(SCOPA_PUP),
#     median=median(SCOPA_PUP),
#     Q1 = quantile(SCOPA_PUP, 0.25),
#     Q3 = quantile(SCOPA_PUP, 0.75),
#     min=min(SCOPA_PUP),
#     max=max(SCOPA_PUP),
#     n=n()
#   )
# 
# 
# # Change from baseline
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_PUP)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
#   rename("Baseline"="SCOPA_PUP") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_PUP)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
#   ) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAG) %>% summarise(mean= mean((SCOPA_PUP-Baseline)/TIME_STUDY))
# 
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_PUP)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
#   rename("Baseline"="SCOPA_PUP") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_PUP)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
#   ) %>%
#   mutate(Diff= (SCOPA_PUP-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAG) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_PUP)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
#   rename("Baseline"="SCOPA_PUP") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_PUP)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
#   ) %>%
#   mutate(Diff= TIME_STUDY*12) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAG) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_PUP)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
#   rename("Baseline"="SCOPA_PUP") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_PUP)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
#   ) %>%
#   mutate(Diff= (SCOPA_PUP-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAG) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# # ALL SCOPA Total Year 3
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#   filter(!is.na(SCOPA_PUP)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(SCOPA_PUP),
#     sd=sd(SCOPA_PUP),
#     median=median(SCOPA_PUP),
#     Q1 = quantile(SCOPA_PUP, 0.25),
#     Q3 = quantile(SCOPA_PUP, 0.75),
#     min=min(SCOPA_PUP),
#     max=max(SCOPA_PUP),
#     n=n()
#   )
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_PUP)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
#   rename("Baseline"="SCOPA_PUP") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_PUP)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
#   ) %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(mean= mean((SCOPA_PUP-Baseline)/TIME_STUDY))
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_PUP)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
#   rename("Baseline"="SCOPA_PUP") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_PUP)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
#   ) %>%
#   mutate(Diff= (SCOPA_PUP-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAG) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_PUP)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
#   rename("Baseline"="SCOPA_PUP") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_PUP)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
#   ) %>%
#   mutate(Diff= TIME_STUDY*12) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAG) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_PUP)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
#   rename("Baseline"="SCOPA_PUP") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_PUP)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
#   ) %>%
#   mutate(Diff= (SCOPA_PUP-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAG) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# # ------------
# # By Possible vs probable V2 Pupillomotor --------------------
# 
# # ALL SCOPA Total Year 0
# 
# EarlyCT_Pop_Baseline_319 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_PUP)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(SCOPA_PUP),
#     sd=sd(SCOPA_PUP),
#     median=median(SCOPA_PUP),
#     Q1 = quantile(SCOPA_PUP, 0.25),
#     Q3 = quantile(SCOPA_PUP, 0.75),
#     min=min(SCOPA_PUP),
#     max=max(SCOPA_PUP),
#     n=n()
#   )
# 
# 
# 
# # ALL SCOPA Total Year 1
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#   filter(!is.na(SCOPA_PUP)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(SCOPA_PUP),
#     sd=sd(SCOPA_PUP),
#     median=median(SCOPA_PUP),
#     Q1 = quantile(SCOPA_PUP, 0.25),
#     Q3 = quantile(SCOPA_PUP, 0.75),
#     min=min(SCOPA_PUP),
#     max=max(SCOPA_PUP),
#     n=n()
#   )
# 
# # Change from baseline
# 
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_PUP)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
#   rename("Baseline"="SCOPA_PUP") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_PUP)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
#   ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAGNIV) %>% summarise(mean= mean((SCOPA_PUP-Baseline)/TIME_STUDY))
# 
# 
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_PUP)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
#   rename("Baseline"="SCOPA_PUP") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_PUP)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
#   ) %>%
#   mutate(Diff= (SCOPA_PUP-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAGNIV) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_PUP)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
#   rename("Baseline"="SCOPA_PUP") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_PUP)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
#   ) %>%
#   mutate(Diff= TIME_STUDY*12) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAGNIV) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_PUP)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
#   rename("Baseline"="SCOPA_PUP") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_PUP)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
#   ) %>%
#   mutate(Diff= (SCOPA_PUP-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAGNIV) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# # ALL SCOPA Total Year 2
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#   filter(!is.na(SCOPA_PUP)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(SCOPA_PUP),
#     sd=sd(SCOPA_PUP),
#     median=median(SCOPA_PUP),
#     Q1 = quantile(SCOPA_PUP, 0.25),
#     Q3 = quantile(SCOPA_PUP, 0.75),
#     min=min(SCOPA_PUP),
#     max=max(SCOPA_PUP),
#     n=n()
#   )
# 
# 
# # Change from baseline
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_PUP)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
#   rename("Baseline"="SCOPA_PUP") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_PUP)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
#   ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAGNIV) %>% summarise(mean= mean((SCOPA_PUP-Baseline)/TIME_STUDY))
# 
# 
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_PUP)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
#   rename("Baseline"="SCOPA_PUP") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_PUP)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
#   ) %>%
#   mutate(Diff= (SCOPA_PUP-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAGNIV) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_PUP)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
#   rename("Baseline"="SCOPA_PUP") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_PUP)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
#   ) %>%
#   mutate(Diff= TIME_STUDY*12) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAGNIV) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_PUP)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
#   rename("Baseline"="SCOPA_PUP") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_PUP)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
#   ) %>%
#   mutate(Diff= (SCOPA_PUP-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAGNIV) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# # ALL SCOPA Total Year 3
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#   filter(!is.na(SCOPA_PUP)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(SCOPA_PUP),
#     sd=sd(SCOPA_PUP),
#     median=median(SCOPA_PUP),
#     Q1 = quantile(SCOPA_PUP, 0.25),
#     Q3 = quantile(SCOPA_PUP, 0.75),
#     min=min(SCOPA_PUP),
#     max=max(SCOPA_PUP),
#     n=n()
#   )
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_PUP)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
#   rename("Baseline"="SCOPA_PUP") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_PUP)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
#   ) %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(mean= mean((SCOPA_PUP-Baseline)/TIME_STUDY))
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_PUP)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
#   rename("Baseline"="SCOPA_PUP") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_PUP)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
#   ) %>%
#   mutate(Diff= (SCOPA_PUP-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAGNIV) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_PUP)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
#   rename("Baseline"="SCOPA_PUP") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_PUP)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
#   ) %>%
#   mutate(Diff= TIME_STUDY*12) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAGNIV) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_PUP)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
#   rename("Baseline"="SCOPA_PUP") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_PUP)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
#   ) %>%
#   mutate(Diff= (SCOPA_PUP-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAGNIV) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# # -----------------------------
# 
# 
# # Inputs SCOPA AUT V2 Sexual   ----------------------------------
# 
# 
# AllMSA_Pop_Baseline_671 <- fread("Source/AllMSA_Pop_Baseline_671.txt")
# AllMSA_Pop_BaselineYear1_410 <- fread("Source/AllMSA_Pop_BaselineYear1_410.txt")
# AllMSA_Pop_BaselineYear1Year2_245 <- fread("Source/AllMSA_Pop_BaselineYear1Year2_245.txt")
# AllMSA_Pop_BaselineYear1Year2Year3Plus_158 <- fread("Source/AllMSA_Pop_BaselineYear1Year2Year3Plus_158.txt")
# 
# EarlyCT_Pop_Baseline_319 <- fread("Source/EarlyCT_Pop_Baseline_319.txt")
# EarlyCT_Pop_BaselineYear1_208 <- fread("Source/EarlyCT_Pop_BaselineYear1_208.txt")
# EarlyCT_Pop_BaselineYear1Year2_134 <- fread("Source/EarlyCT_Pop_BaselineYear1Year2_134.txt")
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 <- fread( "Source/EarlyCT_Pop_BaselineYear1Year2Year3Plus_99.txt")
# 
# dataCohorteManaged <- readRDS("Source/dataCohorteManaged.rds")
# 
# dataCohorteManaged <- dataCohorteManaged %>% group_by(NUM) %>% mutate(TIME_STUDY = ifelse( is.na(TIME_STUDY), 0, TIME_STUDY)) %>%
#   mutate(Year= ifelse(TIME_STUDY==0, 0,
#                       ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
#                              ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
#                                     ifelse(TIME_STUDY>=2.5 ,3, NA)))))
# 
# 
# SCOPA <- dataCohorteManaged %>%
#   select(NUM, DATECONSULT, TIME_STUDY, Year) %>% rename("DATE"="DATECONSULT")
# 
# 
# 
# 
# ams_scopa_bx_2015_16 <- haven::read_sas("Source/ams_scopa_bx_2015_16.sas7bdat")
# names(ams_scopa_bx_2015_16)
# ams_scopa_bx_2015_16 <- ams_scopa_bx_2015_16 %>% filter(!is.na(DATE))
# 
# ams_scopa_bx_2015_16[is.na(ams_scopa_bx_2015_16)] <- 0
# 
# mean(ams_scopa_bx_2015_16$SCOPA_TOT, na.rm=T)
# 
# ams_scopa_bx_2015_16 <- ams_scopa_bx_2015_16 %>%
#   mutate(SCOPA_DIG=SCOPA1+SCOPA2+SCOPA3+SCOPA4+SCOPA5+SCOPA6+SCOPA7) %>%
#   mutate(SCOPA_URI=SCOPA8+SCOPA9+SCOPA10+SCOPA11+SCOPA12+SCOPA13) %>%
#   mutate(SCOPA_CDV=SCOPA14+SCOPA15+SCOPA16) %>%
#   mutate(SCOPA_THE=SCOPA17+SCOPA18+SCOPA19+SCOPA20) %>%
#   mutate(SCOPA_SEX=SCOPA21) %>%
#   mutate(SCOPA_PUP=SCOPA22H+SCOPA22F+SCOPA23F+SCOPA23H) %>%
#   mutate(SCOPA_TOT=SCOPA_SEX+SCOPA_SEX+SCOPA_SEX+SCOPA_SEX+SCOPA_SEX+SCOPA_DIG)
# 
# ams_scopa_bx_2015_16 <- ams_scopa_bx_2015_16 %>%
#   select(NUM, DATE, SCOPA_TOT, SCOPA_PUP, SCOPA_SEX, SCOPA_THE, SCOPA_URI, SCOPA_CDV, SCOPA_DIG)
# 
# length(unique(ams_scopa_bx_2015_16$NUM))
# 
# 
# SCOPA <- ams_scopa_bx_2015_16 %>% inner_join(SCOPA)
# 
# SCOPA <- SCOPA %>% filter(SCOPA_TOT!=0)
# 
# length(unique(SCOPA$NUM))
# 
# 
# SCOPA_Baseline_Pats <- SCOPA %>% filter(Year==0) %>% filter(!is.na(SCOPA_TOT)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM) %>% distinct() %>%
#   inner_join(AllMSA_Pop_Baseline_671)
# 
# 
# 
# # ----------------
# # Overall Early CT V2 Sexual  -------------------------------
# 
# # ALL SCOPA Total Year 0
# 
# 
# EarlyCT_Pop_Baseline_319 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_SEX)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(SCOPA_SEX),
#     sd=sd(SCOPA_SEX),
#     median=median(SCOPA_SEX),
#     Q1 = quantile(SCOPA_SEX, 0.25),
#     Q3 = quantile(SCOPA_SEX, 0.75),
#     min=min(SCOPA_SEX),
#     max=max(SCOPA_SEX),
#     n=n()
#   )
# 
# 
# 
# # ALL SCOPA Total Year 1
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#   filter(!is.na(SCOPA_SEX)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(SCOPA_SEX),
#     sd=sd(SCOPA_SEX),
#     median=median(SCOPA_SEX),
#     Q1 = quantile(SCOPA_SEX, 0.25),
#     Q3 = quantile(SCOPA_SEX, 0.75),
#     min=min(SCOPA_SEX),
#     max=max(SCOPA_SEX),
#     n=n()
#   )
# 
# # Change from baseline
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_SEX)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
#   rename("Baseline"="SCOPA_SEX") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_SEX)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
#   ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(mean= mean((SCOPA_SEX-Baseline)/TIME_STUDY))
# 
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_SEX)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
#   rename("Baseline"="SCOPA_SEX") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_SEX)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
#   ) %>%
#   mutate(Diff= (SCOPA_SEX-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_SEX)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
#   rename("Baseline"="SCOPA_SEX") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_SEX)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
#   ) %>%
#   mutate(Diff= TIME_STUDY*12) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_SEX)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
#   rename("Baseline"="SCOPA_SEX") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_SEX)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
#   ) %>%
#   mutate(Diff= (SCOPA_SEX-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# # ALL SCOPA Total Year 2
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#   filter(!is.na(SCOPA_SEX)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(SCOPA_SEX),
#     sd=sd(SCOPA_SEX),
#     median=median(SCOPA_SEX),
#     Q1 = quantile(SCOPA_SEX, 0.25),
#     Q3 = quantile(SCOPA_SEX, 0.75),
#     min=min(SCOPA_SEX),
#     max=max(SCOPA_SEX),
#     n=n()
#   )
# 
# 
# # Change from baseline
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_SEX)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
#   rename("Baseline"="SCOPA_SEX") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_SEX)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
#   ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(mean= mean((SCOPA_SEX-Baseline)/TIME_STUDY))
# 
# 
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_SEX)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
#   rename("Baseline"="SCOPA_SEX") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_SEX)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
#   ) %>%
#   mutate(Diff= (SCOPA_SEX-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_SEX)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
#   rename("Baseline"="SCOPA_SEX") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_SEX)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
#   ) %>%
#   mutate(Diff= TIME_STUDY*12) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_SEX)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
#   rename("Baseline"="SCOPA_SEX") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_SEX)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
#   ) %>%
#   mutate(Diff= (SCOPA_SEX-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# # ALL SCOPA Total Year 3
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#   filter(!is.na(SCOPA_SEX)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(SCOPA_SEX),
#     sd=sd(SCOPA_SEX),
#     median=median(SCOPA_SEX),
#     Q1 = quantile(SCOPA_SEX, 0.25),
#     Q3 = quantile(SCOPA_SEX, 0.75),
#     min=min(SCOPA_SEX),
#     max=max(SCOPA_SEX),
#     n=n()
#   )
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_SEX)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
#   rename("Baseline"="SCOPA_SEX") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_SEX)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
#   ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(mean= mean((SCOPA_SEX-Baseline)/TIME_STUDY))
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_SEX)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
#   rename("Baseline"="SCOPA_SEX") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_SEX)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
#   ) %>%
#   mutate(Diff= (SCOPA_SEX-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_SEX)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
#   rename("Baseline"="SCOPA_SEX") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_SEX)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
#   ) %>%
#   mutate(Diff= TIME_STUDY*12) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_SEX)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
#   rename("Baseline"="SCOPA_SEX") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_SEX)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
#   ) %>%
#   mutate(Diff= (SCOPA_SEX-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# # --------------
# 
# 
# # By MSA P vs MSA C V2 Sexual -----------------
# # ALL SCOPA Total Year 0
# 
# EarlyCT_Pop_Baseline_319 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_SEX)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(SCOPA_SEX),
#     sd=sd(SCOPA_SEX),
#     median=median(SCOPA_SEX),
#     Q1 = quantile(SCOPA_SEX, 0.25),
#     Q3 = quantile(SCOPA_SEX, 0.75),
#     min=min(SCOPA_SEX),
#     max=max(SCOPA_SEX),
#     n=n()
#   )
# 
# 
# 
# # ALL SCOPA Total Year 1
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#   filter(!is.na(SCOPA_SEX)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(SCOPA_SEX),
#     sd=sd(SCOPA_SEX),
#     median=median(SCOPA_SEX),
#     Q1 = quantile(SCOPA_SEX, 0.25),
#     Q3 = quantile(SCOPA_SEX, 0.75),
#     min=min(SCOPA_SEX),
#     max=max(SCOPA_SEX),
#     n=n()
#   )
# 
# # Change from baseline
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_SEX)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
#   rename("Baseline"="SCOPA_SEX") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_SEX)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
#   ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAG) %>% summarise(mean= mean((SCOPA_SEX-Baseline)/TIME_STUDY))
# 
# 
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_SEX)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
#   rename("Baseline"="SCOPA_SEX") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_SEX)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
#   ) %>%
#   mutate(Diff= (SCOPA_SEX-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAG) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_SEX)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
#   rename("Baseline"="SCOPA_SEX") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_SEX)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
#   ) %>%
#   mutate(Diff= TIME_STUDY*12) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAG) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_SEX)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
#   rename("Baseline"="SCOPA_SEX") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_SEX)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
#   ) %>%
#   mutate(Diff= (SCOPA_SEX-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAG) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# # ALL SCOPA Total Year 2
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#   filter(!is.na(SCOPA_SEX)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(SCOPA_SEX),
#     sd=sd(SCOPA_SEX),
#     median=median(SCOPA_SEX),
#     Q1 = quantile(SCOPA_SEX, 0.25),
#     Q3 = quantile(SCOPA_SEX, 0.75),
#     min=min(SCOPA_SEX),
#     max=max(SCOPA_SEX),
#     n=n()
#   )
# 
# 
# # Change from baseline
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_SEX)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
#   rename("Baseline"="SCOPA_SEX") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_SEX)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
#   ) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAG) %>% summarise(mean= mean((SCOPA_SEX-Baseline)/TIME_STUDY))
# 
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_SEX)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
#   rename("Baseline"="SCOPA_SEX") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_SEX)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
#   ) %>%
#   mutate(Diff= (SCOPA_SEX-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAG) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_SEX)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
#   rename("Baseline"="SCOPA_SEX") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_SEX)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
#   ) %>%
#   mutate(Diff= TIME_STUDY*12) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAG) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_SEX)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
#   rename("Baseline"="SCOPA_SEX") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_SEX)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
#   ) %>%
#   mutate(Diff= (SCOPA_SEX-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAG) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# # ALL SCOPA Total Year 3
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#   filter(!is.na(SCOPA_SEX)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(SCOPA_SEX),
#     sd=sd(SCOPA_SEX),
#     median=median(SCOPA_SEX),
#     Q1 = quantile(SCOPA_SEX, 0.25),
#     Q3 = quantile(SCOPA_SEX, 0.75),
#     min=min(SCOPA_SEX),
#     max=max(SCOPA_SEX),
#     n=n()
#   )
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_SEX)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
#   rename("Baseline"="SCOPA_SEX") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_SEX)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
#   ) %>% group_by(DIAG) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(mean= mean((SCOPA_SEX-Baseline)/TIME_STUDY))
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_SEX)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
#   rename("Baseline"="SCOPA_SEX") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_SEX)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
#   ) %>%
#   mutate(Diff= (SCOPA_SEX-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAG) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_SEX)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
#   rename("Baseline"="SCOPA_SEX") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_SEX)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
#   ) %>%
#   mutate(Diff= TIME_STUDY*12) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAG) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_SEX)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
#   rename("Baseline"="SCOPA_SEX") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_SEX)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
#   ) %>%
#   mutate(Diff= (SCOPA_SEX-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAG) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# 
# # ---------
# # By Possible vs probable V2 Sexual --------------------
# 
# # ALL SCOPA Total Year 0
# 
# EarlyCT_Pop_Baseline_319 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_SEX)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(SCOPA_SEX),
#     sd=sd(SCOPA_SEX),
#     median=median(SCOPA_SEX),
#     Q1 = quantile(SCOPA_SEX, 0.25),
#     Q3 = quantile(SCOPA_SEX, 0.75),
#     min=min(SCOPA_SEX),
#     max=max(SCOPA_SEX),
#     n=n()
#   )
# 
# 
# 
# # ALL SCOPA Total Year 1
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#   filter(!is.na(SCOPA_SEX)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(SCOPA_SEX),
#     sd=sd(SCOPA_SEX),
#     median=median(SCOPA_SEX),
#     Q1 = quantile(SCOPA_SEX, 0.25),
#     Q3 = quantile(SCOPA_SEX, 0.75),
#     min=min(SCOPA_SEX),
#     max=max(SCOPA_SEX),
#     n=n()
#   )
# 
# # Change from baseline
# 
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_SEX)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
#   rename("Baseline"="SCOPA_SEX") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_SEX)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
#   ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAGNIV) %>% summarise(mean= mean((SCOPA_SEX-Baseline)/TIME_STUDY))
# 
# 
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_SEX)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
#   rename("Baseline"="SCOPA_SEX") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_SEX)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
#   ) %>%
#   mutate(Diff= (SCOPA_SEX-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAGNIV) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_SEX)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
#   rename("Baseline"="SCOPA_SEX") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_SEX)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
#   ) %>%
#   mutate(Diff= TIME_STUDY*12) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAGNIV) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_SEX)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
#   rename("Baseline"="SCOPA_SEX") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_SEX)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
#   ) %>%
#   mutate(Diff= (SCOPA_SEX-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAGNIV) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# # ALL SCOPA Total Year 2
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#   filter(!is.na(SCOPA_SEX)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(SCOPA_SEX),
#     sd=sd(SCOPA_SEX),
#     median=median(SCOPA_SEX),
#     Q1 = quantile(SCOPA_SEX, 0.25),
#     Q3 = quantile(SCOPA_SEX, 0.75),
#     min=min(SCOPA_SEX),
#     max=max(SCOPA_SEX),
#     n=n()
#   )
# 
# 
# # Change from baseline
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_SEX)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
#   rename("Baseline"="SCOPA_SEX") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_SEX)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
#   ) %>%  inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAGNIV) %>% summarise(mean= mean((SCOPA_SEX-Baseline)/TIME_STUDY))
# 
# 
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_SEX)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
#   rename("Baseline"="SCOPA_SEX") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_SEX)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
#   ) %>%
#   mutate(Diff= (SCOPA_SEX-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAGNIV) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_SEX)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
#   rename("Baseline"="SCOPA_SEX") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_SEX)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
#   ) %>%
#   mutate(Diff= TIME_STUDY*12) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAGNIV) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_SEX)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
#   rename("Baseline"="SCOPA_SEX") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_SEX)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
#   ) %>%
#   mutate(Diff= (SCOPA_SEX-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAGNIV) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# # ALL SCOPA Total Year 3
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#   filter(!is.na(SCOPA_SEX)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(
#     mean=mean(SCOPA_SEX),
#     sd=sd(SCOPA_SEX),
#     median=median(SCOPA_SEX),
#     Q1 = quantile(SCOPA_SEX, 0.25),
#     Q3 = quantile(SCOPA_SEX, 0.75),
#     min=min(SCOPA_SEX),
#     max=max(SCOPA_SEX),
#     n=n()
#   )
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_SEX)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
#   rename("Baseline"="SCOPA_SEX") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_SEX)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
#   ) %>% group_by(DIAGNIV) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   summarise(mean= mean((SCOPA_SEX-Baseline)/TIME_STUDY))
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_SEX)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
#   rename("Baseline"="SCOPA_SEX") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_SEX)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
#   ) %>%
#   mutate(Diff= (SCOPA_SEX-Baseline)) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAGNIV) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_SEX)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
#   rename("Baseline"="SCOPA_SEX") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_SEX)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
#   ) %>%
#   mutate(Diff= TIME_STUDY*12) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAGNIV) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
#   filter(!is.na(SCOPA_SEX)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
#   rename("Baseline"="SCOPA_SEX") %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_SEX)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
#   ) %>%
#   mutate(Diff= (SCOPA_SEX-Baseline)/TIME_STUDY) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   group_by(DIAGNIV) %>%
#   summarise(
#     mean=mean(Diff),
#     sd=sd(Diff),
#     median=median(Diff),
#     Q1 = quantile(Diff, 0.25),
#     Q3 = quantile(Diff, 0.75),
#     min=min(Diff),
#     max=max(Diff),
#     n=n()
#   )
# 
# # -----------------------------
# 
# 
# # Missing Correlations -----
# # -----
# # Correlation between Delta SCOPA AUT & Delta FDA-modified UMSARS V2 DIGESTIVE ------------------
# 
# # Inputs SCOPA Total V2 ----------------------------------
# 
# 
# AllMSA_Pop_Baseline_671 <- fread("Source/AllMSA_Pop_Baseline_671.txt")
# AllMSA_Pop_BaselineYear1_410 <- fread("Source/AllMSA_Pop_BaselineYear1_410.txt")
# AllMSA_Pop_BaselineYear1Year2_245 <- fread("Source/AllMSA_Pop_BaselineYear1Year2_245.txt")
# AllMSA_Pop_BaselineYear1Year2Year3Plus_158 <- fread("Source/AllMSA_Pop_BaselineYear1Year2Year3Plus_158.txt")
# 
# EarlyCT_Pop_Baseline_319 <- fread("Source/EarlyCT_Pop_Baseline_319.txt")
# EarlyCT_Pop_BaselineYear1_208 <- fread("Source/EarlyCT_Pop_BaselineYear1_208.txt")
# EarlyCT_Pop_BaselineYear1Year2_134 <- fread("Source/EarlyCT_Pop_BaselineYear1Year2_134.txt")
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 <- fread( "Source/EarlyCT_Pop_BaselineYear1Year2Year3Plus_99.txt")
# 
# dataCohorteManaged <- readRDS("Source/dataCohorteManaged.rds")
# 
# dataCohorteManaged <- dataCohorteManaged %>% group_by(NUM) %>% mutate(TIME_STUDY = ifelse( is.na(TIME_STUDY), 0, TIME_STUDY)) %>%
#   mutate(Year= ifelse(TIME_STUDY==0, 0,
#                       ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
#                              ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
#                                     ifelse(TIME_STUDY>=2.5 ,3, NA)))))
# 
# 
# SCOPA <- dataCohorteManaged %>%
#   select(NUM, DATECONSULT, TIME_STUDY, Year) %>% rename("DATE"="DATECONSULT")
# 
# 
# 
# 
# ams_scopa_bx_2015_16 <- haven::read_sas("Source/ams_scopa_bx_2015_16.sas7bdat")
# names(ams_scopa_bx_2015_16)
# ams_scopa_bx_2015_16 <- ams_scopa_bx_2015_16 %>% filter(!is.na(DATE))
# 
# ams_scopa_bx_2015_16[is.na(ams_scopa_bx_2015_16)] <- 0
# 
# mean(ams_scopa_bx_2015_16$SCOPA_TOT, na.rm=T)
# 
# ams_scopa_bx_2015_16 <- ams_scopa_bx_2015_16 %>%
#   mutate(SCOPA_DIG=SCOPA1+SCOPA2+SCOPA3+SCOPA4+SCOPA5+SCOPA6+SCOPA7) %>%
#   mutate(SCOPA_URI=SCOPA8+SCOPA9+SCOPA10+SCOPA11+SCOPA12+SCOPA13) %>%
#   mutate(SCOPA_CDV=SCOPA14+SCOPA15+SCOPA16) %>%
#   mutate(SCOPA_THE=SCOPA17+SCOPA18+SCOPA19+SCOPA20) %>%
#   mutate(SCOPA_PUP=SCOPA21) %>%
#   mutate(SCOPA_SEX=SCOPA22H+SCOPA22F+SCOPA23F+SCOPA23H) %>%
#   mutate(SCOPA_TOT=SCOPA_SEX+SCOPA_PUP+SCOPA_THE+SCOPA_CDV+SCOPA_URI+SCOPA_DIG)
# 
# ams_scopa_bx_2015_16 <- ams_scopa_bx_2015_16 %>%
#   select(NUM, DATE, SCOPA_TOT, SCOPA_SEX, SCOPA_PUP, SCOPA_THE, SCOPA_CDV, SCOPA_URI, SCOPA_DIG)
# 
# length(unique(ams_scopa_bx_2015_16$NUM))
# 
# 
# SCOPA <- ams_scopa_bx_2015_16 %>% inner_join(SCOPA)
# 
# SCOPA <- SCOPA %>% filter(SCOPA_TOT!=0)
# 
# length(unique(SCOPA$NUM))
# 
# 
# SCOPA_Baseline_Pats <- SCOPA %>% filter(Year==0) %>% filter(!is.na(SCOPA_TOT)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM) %>% distinct() %>%
#   inner_join(AllMSA_Pop_Baseline_671)
# 
# 
# 
# 
# UMSARS1 <- dataCohorteManaged %>%
#   select(NUM, DATECONSULT, TIME_STUDY, Year, UMSARS1_1:UMSARS1_TOT)
# 
# 
# UMSARS1$UMSARS1_TOT_v2 <- rowSums(UMSARS1[, 5:16], na.rm = TRUE)
# 
# UMSARS1$missing_na <- rowSums(is.na(UMSARS1[, 5:16]))
# 
# UMSARS1 <-  UMSARS1 %>%  select(NUM, TIME_STUDY, Year, UMSARS1_TOT, UMSARS1_11)
# 
# UMSARS1 <- UMSARS1 %>% mutate(UMSARS1_TOT_FDA=UMSARS1_TOT-UMSARS1_11)
# 
# names(SCOPA)
# 
# # ----------------
# # Overall Early CT V2  -------------------------------
# 
# # Year 1
# 
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   filter(!is.na(SCOPA_TOT)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
#   rename("Baseline"="SCOPA_DIG") %>%
#   left_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_TOT)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
#   ) %>%
#   mutate(Diff= (SCOPA_DIG-Baseline))  %>% select(NUM, DIAG, DIAGNIV, Diff) %>% rename("SCOPA_Diff"="Diff") %>% drop_na() %>%
#   inner_join(
# 
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
#       filter(!is.na(UMSARS1_TOT_FDA)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  UMSARS1_TOT_FDA) %>%
#       rename("Baseline"="UMSARS1_TOT_FDA") %>%
#       left_join(
#         EarlyCT_Pop_BaselineYear1_208 %>% inner_join(UMSARS1) %>% filter(Year==1) %>%
#           filter(!is.na(UMSARS1_TOT_FDA)) %>%
#           mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#           group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#           group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#           group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, UMSARS1_TOT_FDA)
#       ) %>%
#       mutate(Diff= (UMSARS1_TOT_FDA-Baseline))  %>% select(NUM, DIAG, DIAGNIV, Diff)
#   )  %>% #summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))
#   #group_by(DIAG) %>% summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))
#   group_by(DIAGNIV) %>% summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))
# 
# 
# 
# 
# 
# # Year 2
# 
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   filter(!is.na(SCOPA_TOT)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
#   rename("Baseline"="SCOPA_DIG") %>%
#   left_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_TOT)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
#   ) %>%
#   mutate(Diff= (SCOPA_DIG-Baseline))  %>% select(NUM, DIAG, DIAGNIV, Diff) %>% rename("SCOPA_Diff"="Diff") %>% drop_na() %>%
#   inner_join(
# 
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
#       filter(!is.na(UMSARS1_TOT_FDA)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  UMSARS1_TOT_FDA) %>%
#       rename("Baseline"="UMSARS1_TOT_FDA") %>%
#       left_join(
#         EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(UMSARS1) %>% filter(Year==2) %>%
#           filter(!is.na(UMSARS1_TOT_FDA)) %>%
#           mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#           group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#           group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#           group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, UMSARS1_TOT_FDA)
#       ) %>%
#       mutate(Diff= (UMSARS1_TOT_FDA-Baseline))   %>% select(NUM, DIAG, DIAGNIV, Diff)
#   )  %>% #  summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))
#   #group_by(DIAG) %>% summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))
#   group_by(DIAGNIV) %>% summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))
# 
# 
# # Year 3
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   filter(!is.na(SCOPA_TOT)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_DIG) %>%
#   rename("Baseline"="SCOPA_DIG") %>%
#   left_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_TOT)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_DIG)
#   ) %>%
#   mutate(Diff= (SCOPA_DIG-Baseline))  %>% select(NUM, DIAG, DIAGNIV, Diff) %>% rename("SCOPA_Diff"="Diff") %>% drop_na() %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
#       filter(!is.na(UMSARS1_TOT_FDA)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  UMSARS1_TOT_FDA) %>%
#       rename("Baseline"="UMSARS1_TOT_FDA") %>%
#       left_join(
#         EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(UMSARS1) %>% filter(Year==3) %>%
#           filter(!is.na(UMSARS1_TOT_FDA)) %>%
#           mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#           group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#           group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#           group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, UMSARS1_TOT_FDA)
#       ) %>%
#       mutate(Diff= (UMSARS1_TOT_FDA-Baseline)) %>% select(NUM, DIAG, DIAGNIV, Diff)
#   )  %>%#  summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))
#   #group_by(DIAG) %>% summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))
#   group_by(DIAGNIV) %>% summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))
# 
# 
# # --------------
# 
# 
# 
# # Correlation between Delta SCOPA AUT & Delta FDA-modified UMSARS V2 URINARY ------------------
# 
# # Inputs SCOPA Total V2 ----------------------------------
# 
# 
# AllMSA_Pop_Baseline_671 <- fread("Source/AllMSA_Pop_Baseline_671.txt")
# AllMSA_Pop_BaselineYear1_410 <- fread("Source/AllMSA_Pop_BaselineYear1_410.txt")
# AllMSA_Pop_BaselineYear1Year2_245 <- fread("Source/AllMSA_Pop_BaselineYear1Year2_245.txt")
# AllMSA_Pop_BaselineYear1Year2Year3Plus_158 <- fread("Source/AllMSA_Pop_BaselineYear1Year2Year3Plus_158.txt")
# 
# EarlyCT_Pop_Baseline_319 <- fread("Source/EarlyCT_Pop_Baseline_319.txt")
# EarlyCT_Pop_BaselineYear1_208 <- fread("Source/EarlyCT_Pop_BaselineYear1_208.txt")
# EarlyCT_Pop_BaselineYear1Year2_134 <- fread("Source/EarlyCT_Pop_BaselineYear1Year2_134.txt")
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 <- fread( "Source/EarlyCT_Pop_BaselineYear1Year2Year3Plus_99.txt")
# 
# dataCohorteManaged <- readRDS("Source/dataCohorteManaged.rds")
# 
# dataCohorteManaged <- dataCohorteManaged %>% group_by(NUM) %>% mutate(TIME_STUDY = ifelse( is.na(TIME_STUDY), 0, TIME_STUDY)) %>%
#   mutate(Year= ifelse(TIME_STUDY==0, 0,
#                       ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
#                              ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
#                                     ifelse(TIME_STUDY>=2.5 ,3, NA)))))
# 
# 
# SCOPA <- dataCohorteManaged %>%
#   select(NUM, DATECONSULT, TIME_STUDY, Year) %>% rename("DATE"="DATECONSULT")
# 
# 
# 
# 
# ams_scopa_bx_2015_16 <- haven::read_sas("Source/ams_scopa_bx_2015_16.sas7bdat")
# names(ams_scopa_bx_2015_16)
# ams_scopa_bx_2015_16 <- ams_scopa_bx_2015_16 %>% filter(!is.na(DATE))
# 
# ams_scopa_bx_2015_16[is.na(ams_scopa_bx_2015_16)] <- 0
# 
# mean(ams_scopa_bx_2015_16$SCOPA_TOT, na.rm=T)
# 
# ams_scopa_bx_2015_16 <- ams_scopa_bx_2015_16 %>%
#   mutate(SCOPA_DIG=SCOPA1+SCOPA2+SCOPA3+SCOPA4+SCOPA5+SCOPA6+SCOPA7) %>%
#   mutate(SCOPA_URI=SCOPA8+SCOPA9+SCOPA10+SCOPA11+SCOPA12+SCOPA13) %>%
#   mutate(SCOPA_CDV=SCOPA14+SCOPA15+SCOPA16) %>%
#   mutate(SCOPA_THE=SCOPA17+SCOPA18+SCOPA19+SCOPA20) %>%
#   mutate(SCOPA_PUP=SCOPA21) %>%
#   mutate(SCOPA_SEX=SCOPA22H+SCOPA22F+SCOPA23F+SCOPA23H) %>%
#   mutate(SCOPA_TOT=SCOPA_SEX+SCOPA_PUP+SCOPA_THE+SCOPA_CDV+SCOPA_URI+SCOPA_DIG)
# 
# ams_scopa_bx_2015_16 <- ams_scopa_bx_2015_16 %>%
#   select(NUM, DATE, SCOPA_TOT, SCOPA_SEX, SCOPA_PUP, SCOPA_THE, SCOPA_CDV, SCOPA_URI, SCOPA_DIG)
# 
# length(unique(ams_scopa_bx_2015_16$NUM))
# 
# 
# SCOPA <- ams_scopa_bx_2015_16 %>% inner_join(SCOPA)
# 
# SCOPA <- SCOPA %>% filter(SCOPA_TOT!=0)
# 
# length(unique(SCOPA$NUM))
# 
# 
# SCOPA_Baseline_Pats <- SCOPA %>% filter(Year==0) %>% filter(!is.na(SCOPA_TOT)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM) %>% distinct() %>%
#   inner_join(AllMSA_Pop_Baseline_671)
# 
# 
# 
# 
# UMSARS1 <- dataCohorteManaged %>%
#   select(NUM, DATECONSULT, TIME_STUDY, Year, UMSARS1_1:UMSARS1_TOT)
# 
# 
# UMSARS1$UMSARS1_TOT_v2 <- rowSums(UMSARS1[, 5:16], na.rm = TRUE)
# 
# UMSARS1$missing_na <- rowSums(is.na(UMSARS1[, 5:16]))
# 
# UMSARS1 <-  UMSARS1 %>%  select(NUM, TIME_STUDY, Year, UMSARS1_TOT, UMSARS1_11)
# 
# UMSARS1 <- UMSARS1 %>% mutate(UMSARS1_TOT_FDA=UMSARS1_TOT-UMSARS1_11)
# 
# names(SCOPA)
# 
# # ----------------
# # Overall Early CT V2  -------------------------------
# 
# # Year 1
# 
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   filter(!is.na(SCOPA_TOT)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
#   rename("Baseline"="SCOPA_URI") %>%
#   left_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_TOT)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
#   ) %>%
#   mutate(Diff= (SCOPA_URI-Baseline))  %>% select(NUM, DIAG, DIAGNIV, Diff) %>% rename("SCOPA_Diff"="Diff") %>% drop_na() %>%
#   inner_join(
# 
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
#       filter(!is.na(UMSARS1_TOT_FDA)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  UMSARS1_TOT_FDA) %>%
#       rename("Baseline"="UMSARS1_TOT_FDA") %>%
#       left_join(
#         EarlyCT_Pop_BaselineYear1_208 %>% inner_join(UMSARS1) %>% filter(Year==1) %>%
#           filter(!is.na(UMSARS1_TOT_FDA)) %>%
#           mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#           group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#           group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#           group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, UMSARS1_TOT_FDA)
#       ) %>%
#       mutate(Diff= (UMSARS1_TOT_FDA-Baseline))  %>% select(NUM, DIAG, DIAGNIV, Diff)
#   )  %>% summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))
# #group_by(DIAG) %>% summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))
# #group_by(DIAGNIV) %>% summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))
# 
# 
# 
# 
# 
# # Year 2
# 
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   filter(!is.na(SCOPA_TOT)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
#   rename("Baseline"="SCOPA_URI") %>%
#   left_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_TOT)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
#   ) %>%
#   mutate(Diff= (SCOPA_URI-Baseline))  %>% select(NUM, DIAG, DIAGNIV, Diff) %>% rename("SCOPA_Diff"="Diff") %>% drop_na() %>%
#   inner_join(
# 
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
#       filter(!is.na(UMSARS1_TOT_FDA)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  UMSARS1_TOT_FDA) %>%
#       rename("Baseline"="UMSARS1_TOT_FDA") %>%
#       left_join(
#         EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(UMSARS1) %>% filter(Year==2) %>%
#           filter(!is.na(UMSARS1_TOT_FDA)) %>%
#           mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#           group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#           group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#           group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, UMSARS1_TOT_FDA)
#       ) %>%
#       mutate(Diff= (UMSARS1_TOT_FDA-Baseline))   %>% select(NUM, DIAG, DIAGNIV, Diff)
#   )  %>%   summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))
# #group_by(DIAG) %>% summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))
# #group_by(DIAGNIV) %>% summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))
# 
# 
# # Year 3
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   filter(!is.na(SCOPA_TOT)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_URI) %>%
#   rename("Baseline"="SCOPA_URI") %>%
#   left_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_TOT)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_URI)
#   ) %>%
#   mutate(Diff= (SCOPA_URI-Baseline))  %>% select(NUM, DIAG, DIAGNIV, Diff) %>% rename("SCOPA_Diff"="Diff") %>% drop_na() %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
#       filter(!is.na(UMSARS1_TOT_FDA)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  UMSARS1_TOT_FDA) %>%
#       rename("Baseline"="UMSARS1_TOT_FDA") %>%
#       left_join(
#         EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(UMSARS1) %>% filter(Year==3) %>%
#           filter(!is.na(UMSARS1_TOT_FDA)) %>%
#           mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#           group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#           group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#           group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, UMSARS1_TOT_FDA)
#       ) %>%
#       mutate(Diff= (UMSARS1_TOT_FDA-Baseline)) %>% select(NUM, DIAG, DIAGNIV, Diff)
#   )  %>% summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))
# #group_by(DIAG) %>% summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))
# #group_by(DIAGNIV) %>% summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))
# 
# 
# # --------------
# 
# 
# 
# # Correlation between Delta SCOPA AUT & Delta FDA-modified UMSARS V2 CARDIOVASCULAR ------------------
# 
# # Inputs SCOPA Total V2 ----------------------------------
# 
# 
# AllMSA_Pop_Baseline_671 <- fread("Source/AllMSA_Pop_Baseline_671.txt")
# AllMSA_Pop_BaselineYear1_410 <- fread("Source/AllMSA_Pop_BaselineYear1_410.txt")
# AllMSA_Pop_BaselineYear1Year2_245 <- fread("Source/AllMSA_Pop_BaselineYear1Year2_245.txt")
# AllMSA_Pop_BaselineYear1Year2Year3Plus_158 <- fread("Source/AllMSA_Pop_BaselineYear1Year2Year3Plus_158.txt")
# 
# EarlyCT_Pop_Baseline_319 <- fread("Source/EarlyCT_Pop_Baseline_319.txt")
# EarlyCT_Pop_BaselineYear1_208 <- fread("Source/EarlyCT_Pop_BaselineYear1_208.txt")
# EarlyCT_Pop_BaselineYear1Year2_134 <- fread("Source/EarlyCT_Pop_BaselineYear1Year2_134.txt")
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 <- fread( "Source/EarlyCT_Pop_BaselineYear1Year2Year3Plus_99.txt")
# 
# dataCohorteManaged <- readRDS("Source/dataCohorteManaged.rds")
# 
# dataCohorteManaged <- dataCohorteManaged %>% group_by(NUM) %>% mutate(TIME_STUDY = ifelse( is.na(TIME_STUDY), 0, TIME_STUDY)) %>%
#   mutate(Year= ifelse(TIME_STUDY==0, 0,
#                       ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
#                              ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
#                                     ifelse(TIME_STUDY>=2.5 ,3, NA)))))
# 
# 
# SCOPA <- dataCohorteManaged %>%
#   select(NUM, DATECONSULT, TIME_STUDY, Year) %>% rename("DATE"="DATECONSULT")
# 
# 
# 
# 
# ams_scopa_bx_2015_16 <- haven::read_sas("Source/ams_scopa_bx_2015_16.sas7bdat")
# names(ams_scopa_bx_2015_16)
# ams_scopa_bx_2015_16 <- ams_scopa_bx_2015_16 %>% filter(!is.na(DATE))
# 
# ams_scopa_bx_2015_16[is.na(ams_scopa_bx_2015_16)] <- 0
# 
# mean(ams_scopa_bx_2015_16$SCOPA_TOT, na.rm=T)
# 
# ams_scopa_bx_2015_16 <- ams_scopa_bx_2015_16 %>%
#   mutate(SCOPA_DIG=SCOPA1+SCOPA2+SCOPA3+SCOPA4+SCOPA5+SCOPA6+SCOPA7) %>%
#   mutate(SCOPA_URI=SCOPA8+SCOPA9+SCOPA10+SCOPA11+SCOPA12+SCOPA13) %>%
#   mutate(SCOPA_CDV=SCOPA14+SCOPA15+SCOPA16) %>%
#   mutate(SCOPA_THE=SCOPA17+SCOPA18+SCOPA19+SCOPA20) %>%
#   mutate(SCOPA_PUP=SCOPA21) %>%
#   mutate(SCOPA_SEX=SCOPA22H+SCOPA22F+SCOPA23F+SCOPA23H) %>%
#   mutate(SCOPA_TOT=SCOPA_SEX+SCOPA_PUP+SCOPA_THE+SCOPA_CDV+SCOPA_URI+SCOPA_DIG)
# 
# ams_scopa_bx_2015_16 <- ams_scopa_bx_2015_16 %>%
#   select(NUM, DATE, SCOPA_TOT, SCOPA_SEX, SCOPA_PUP, SCOPA_THE, SCOPA_CDV, SCOPA_URI, SCOPA_DIG)
# 
# length(unique(ams_scopa_bx_2015_16$NUM))
# 
# 
# SCOPA <- ams_scopa_bx_2015_16 %>% inner_join(SCOPA)
# 
# SCOPA <- SCOPA %>% filter(SCOPA_TOT!=0)
# 
# length(unique(SCOPA$NUM))
# 
# 
# SCOPA_Baseline_Pats <- SCOPA %>% filter(Year==0) %>% filter(!is.na(SCOPA_TOT)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM) %>% distinct() %>%
#   inner_join(AllMSA_Pop_Baseline_671)
# 
# 
# 
# 
# UMSARS1 <- dataCohorteManaged %>%
#   select(NUM, DATECONSULT, TIME_STUDY, Year, UMSARS1_1:UMSARS1_TOT)
# 
# 
# UMSARS1$UMSARS1_TOT_v2 <- rowSums(UMSARS1[, 5:16], na.rm = TRUE)
# 
# UMSARS1$missing_na <- rowSums(is.na(UMSARS1[, 5:16]))
# 
# UMSARS1 <-  UMSARS1 %>%  select(NUM, TIME_STUDY, Year, UMSARS1_TOT, UMSARS1_11)
# 
# UMSARS1 <- UMSARS1 %>% mutate(UMSARS1_TOT_FDA=UMSARS1_TOT-UMSARS1_11)
# 
# names(SCOPA)
# 
# # ----------------
# # Overall Early CT V2  -------------------------------
# 
# # Year 1
# 
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   filter(!is.na(SCOPA_TOT)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
#   rename("Baseline"="SCOPA_CDV") %>%
#   left_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_TOT)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
#   ) %>%
#   mutate(Diff= (SCOPA_CDV-Baseline))  %>% select(NUM, DIAG, DIAGNIV, Diff) %>% rename("SCOPA_Diff"="Diff") %>% drop_na() %>%
#   inner_join(
# 
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
#       filter(!is.na(UMSARS1_TOT_FDA)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  UMSARS1_TOT_FDA) %>%
#       rename("Baseline"="UMSARS1_TOT_FDA") %>%
#       left_join(
#         EarlyCT_Pop_BaselineYear1_208 %>% inner_join(UMSARS1) %>% filter(Year==1) %>%
#           filter(!is.na(UMSARS1_TOT_FDA)) %>%
#           mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#           group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#           group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#           group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, UMSARS1_TOT_FDA)
#       ) %>%
#       mutate(Diff= (UMSARS1_TOT_FDA-Baseline))  %>% select(NUM, DIAG, DIAGNIV, Diff)
#   )  %>%# summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))
#   #group_by(DIAG) %>% summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))
#   group_by(DIAGNIV) %>% summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))
# 
# 
# 
# 
# 
# # Year 2
# 
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   filter(!is.na(SCOPA_TOT)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
#   rename("Baseline"="SCOPA_CDV") %>%
#   left_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_TOT)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
#   ) %>%
#   mutate(Diff= (SCOPA_CDV-Baseline))  %>% select(NUM, DIAG, DIAGNIV, Diff) %>% rename("SCOPA_Diff"="Diff") %>% drop_na() %>%
#   inner_join(
# 
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
#       filter(!is.na(UMSARS1_TOT_FDA)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  UMSARS1_TOT_FDA) %>%
#       rename("Baseline"="UMSARS1_TOT_FDA") %>%
#       left_join(
#         EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(UMSARS1) %>% filter(Year==2) %>%
#           filter(!is.na(UMSARS1_TOT_FDA)) %>%
#           mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#           group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#           group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#           group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, UMSARS1_TOT_FDA)
#       ) %>%
#       mutate(Diff= (UMSARS1_TOT_FDA-Baseline))   %>% select(NUM, DIAG, DIAGNIV, Diff)
#   )  %>%#   summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))
#   #group_by(DIAG) %>% summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))
#   group_by(DIAGNIV) %>% summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))
# 
# 
# # Year 3
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   filter(!is.na(SCOPA_TOT)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_CDV) %>%
#   rename("Baseline"="SCOPA_CDV") %>%
#   left_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_TOT)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_CDV)
#   ) %>%
#   mutate(Diff= (SCOPA_CDV-Baseline))  %>% select(NUM, DIAG, DIAGNIV, Diff) %>% rename("SCOPA_Diff"="Diff") %>% drop_na() %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
#       filter(!is.na(UMSARS1_TOT_FDA)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  UMSARS1_TOT_FDA) %>%
#       rename("Baseline"="UMSARS1_TOT_FDA") %>%
#       left_join(
#         EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(UMSARS1) %>% filter(Year==3) %>%
#           filter(!is.na(UMSARS1_TOT_FDA)) %>%
#           mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#           group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#           group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#           group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, UMSARS1_TOT_FDA)
#       ) %>%
#       mutate(Diff= (UMSARS1_TOT_FDA-Baseline)) %>% select(NUM, DIAG, DIAGNIV, Diff)
#   )  %>%# summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))
#   #group_by(DIAG) %>% summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))
#   group_by(DIAGNIV) %>% summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))
# 
# 
# # --------------
# 
# 
# 
# # Correlation between Delta SCOPA AUT & Delta FDA-modified UMSARS V2 THERMO ------------------
# 
# # Inputs SCOPA Total V2 ----------------------------------
# 
# 
# AllMSA_Pop_Baseline_671 <- fread("Source/AllMSA_Pop_Baseline_671.txt")
# AllMSA_Pop_BaselineYear1_410 <- fread("Source/AllMSA_Pop_BaselineYear1_410.txt")
# AllMSA_Pop_BaselineYear1Year2_245 <- fread("Source/AllMSA_Pop_BaselineYear1Year2_245.txt")
# AllMSA_Pop_BaselineYear1Year2Year3Plus_158 <- fread("Source/AllMSA_Pop_BaselineYear1Year2Year3Plus_158.txt")
# 
# EarlyCT_Pop_Baseline_319 <- fread("Source/EarlyCT_Pop_Baseline_319.txt")
# EarlyCT_Pop_BaselineYear1_208 <- fread("Source/EarlyCT_Pop_BaselineYear1_208.txt")
# EarlyCT_Pop_BaselineYear1Year2_134 <- fread("Source/EarlyCT_Pop_BaselineYear1Year2_134.txt")
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 <- fread( "Source/EarlyCT_Pop_BaselineYear1Year2Year3Plus_99.txt")
# 
# dataCohorteManaged <- readRDS("Source/dataCohorteManaged.rds")
# 
# dataCohorteManaged <- dataCohorteManaged %>% group_by(NUM) %>% mutate(TIME_STUDY = ifelse( is.na(TIME_STUDY), 0, TIME_STUDY)) %>%
#   mutate(Year= ifelse(TIME_STUDY==0, 0,
#                       ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
#                              ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
#                                     ifelse(TIME_STUDY>=2.5 ,3, NA)))))
# 
# 
# SCOPA <- dataCohorteManaged %>%
#   select(NUM, DATECONSULT, TIME_STUDY, Year) %>% rename("DATE"="DATECONSULT")
# 
# 
# 
# 
# ams_scopa_bx_2015_16 <- haven::read_sas("Source/ams_scopa_bx_2015_16.sas7bdat")
# names(ams_scopa_bx_2015_16)
# ams_scopa_bx_2015_16 <- ams_scopa_bx_2015_16 %>% filter(!is.na(DATE))
# 
# ams_scopa_bx_2015_16[is.na(ams_scopa_bx_2015_16)] <- 0
# 
# mean(ams_scopa_bx_2015_16$SCOPA_TOT, na.rm=T)
# 
# ams_scopa_bx_2015_16 <- ams_scopa_bx_2015_16 %>%
#   mutate(SCOPA_DIG=SCOPA1+SCOPA2+SCOPA3+SCOPA4+SCOPA5+SCOPA6+SCOPA7) %>%
#   mutate(SCOPA_URI=SCOPA8+SCOPA9+SCOPA10+SCOPA11+SCOPA12+SCOPA13) %>%
#   mutate(SCOPA_CDV=SCOPA14+SCOPA15+SCOPA16) %>%
#   mutate(SCOPA_THE=SCOPA17+SCOPA18+SCOPA19+SCOPA20) %>%
#   mutate(SCOPA_PUP=SCOPA21) %>%
#   mutate(SCOPA_SEX=SCOPA22H+SCOPA22F+SCOPA23F+SCOPA23H) %>%
#   mutate(SCOPA_TOT=SCOPA_SEX+SCOPA_PUP+SCOPA_THE+SCOPA_CDV+SCOPA_URI+SCOPA_DIG)
# 
# ams_scopa_bx_2015_16 <- ams_scopa_bx_2015_16 %>%
#   select(NUM, DATE, SCOPA_TOT, SCOPA_SEX, SCOPA_PUP, SCOPA_THE, SCOPA_CDV, SCOPA_URI, SCOPA_DIG)
# 
# length(unique(ams_scopa_bx_2015_16$NUM))
# 
# 
# SCOPA <- ams_scopa_bx_2015_16 %>% inner_join(SCOPA)
# 
# SCOPA <- SCOPA %>% filter(SCOPA_TOT!=0)
# 
# length(unique(SCOPA$NUM))
# 
# 
# SCOPA_Baseline_Pats <- SCOPA %>% filter(Year==0) %>% filter(!is.na(SCOPA_TOT)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM) %>% distinct() %>%
#   inner_join(AllMSA_Pop_Baseline_671)
# 
# 
# 
# 
# UMSARS1 <- dataCohorteManaged %>%
#   select(NUM, DATECONSULT, TIME_STUDY, Year, UMSARS1_1:UMSARS1_TOT)
# 
# 
# UMSARS1$UMSARS1_TOT_v2 <- rowSums(UMSARS1[, 5:16], na.rm = TRUE)
# 
# UMSARS1$missing_na <- rowSums(is.na(UMSARS1[, 5:16]))
# 
# UMSARS1 <-  UMSARS1 %>%  select(NUM, TIME_STUDY, Year, UMSARS1_TOT, UMSARS1_11)
# 
# UMSARS1 <- UMSARS1 %>% mutate(UMSARS1_TOT_FDA=UMSARS1_TOT-UMSARS1_11)
# 
# names(SCOPA)
# 
# # ----------------
# # Overall Early CT V2  -------------------------------
# 
# # Year 1
# 
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   filter(!is.na(SCOPA_TOT)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
#   rename("Baseline"="SCOPA_THE") %>%
#   left_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_TOT)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
#   ) %>%
#   mutate(Diff= (SCOPA_THE-Baseline))  %>% select(NUM, DIAG, DIAGNIV, Diff) %>% rename("SCOPA_Diff"="Diff") %>% drop_na() %>%
#   inner_join(
# 
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
#       filter(!is.na(UMSARS1_TOT_FDA)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  UMSARS1_TOT_FDA) %>%
#       rename("Baseline"="UMSARS1_TOT_FDA") %>%
#       left_join(
#         EarlyCT_Pop_BaselineYear1_208 %>% inner_join(UMSARS1) %>% filter(Year==1) %>%
#           filter(!is.na(UMSARS1_TOT_FDA)) %>%
#           mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#           group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#           group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#           group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, UMSARS1_TOT_FDA)
#       ) %>%
#       mutate(Diff= (UMSARS1_TOT_FDA-Baseline))  %>% select(NUM, DIAG, DIAGNIV, Diff)
#   )  %>% summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))
# #group_by(DIAG) %>% summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))
# #group_by(DIAGNIV) %>% summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))
# 
# 
# 
# 
# 
# # Year 2
# 
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   filter(!is.na(SCOPA_TOT)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
#   rename("Baseline"="SCOPA_THE") %>%
#   left_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_TOT)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
#   ) %>%
#   mutate(Diff= (SCOPA_THE-Baseline))  %>% select(NUM, DIAG, DIAGNIV, Diff) %>% rename("SCOPA_Diff"="Diff") %>% drop_na() %>%
#   inner_join(
# 
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
#       filter(!is.na(UMSARS1_TOT_FDA)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  UMSARS1_TOT_FDA) %>%
#       rename("Baseline"="UMSARS1_TOT_FDA") %>%
#       left_join(
#         EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(UMSARS1) %>% filter(Year==2) %>%
#           filter(!is.na(UMSARS1_TOT_FDA)) %>%
#           mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#           group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#           group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#           group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, UMSARS1_TOT_FDA)
#       ) %>%
#       mutate(Diff= (UMSARS1_TOT_FDA-Baseline))   %>% select(NUM, DIAG, DIAGNIV, Diff)
#   )  %>%   summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))
# #group_by(DIAG) %>% summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))
# #group_by(DIAGNIV) %>% summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))
# 
# 
# # Year 3
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   filter(!is.na(SCOPA_TOT)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_THE) %>%
#   rename("Baseline"="SCOPA_THE") %>%
#   left_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_TOT)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_THE)
#   ) %>%
#   mutate(Diff= (SCOPA_THE-Baseline))  %>% select(NUM, DIAG, DIAGNIV, Diff) %>% rename("SCOPA_Diff"="Diff") %>% drop_na() %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
#       filter(!is.na(UMSARS1_TOT_FDA)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  UMSARS1_TOT_FDA) %>%
#       rename("Baseline"="UMSARS1_TOT_FDA") %>%
#       left_join(
#         EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(UMSARS1) %>% filter(Year==3) %>%
#           filter(!is.na(UMSARS1_TOT_FDA)) %>%
#           mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#           group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#           group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#           group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, UMSARS1_TOT_FDA)
#       ) %>%
#       mutate(Diff= (UMSARS1_TOT_FDA-Baseline)) %>% select(NUM, DIAG, DIAGNIV, Diff)
#   )  %>% summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))
# #group_by(DIAG) %>% summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))
# #group_by(DIAGNIV) %>% summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))
# 
# 
# # --------------
# 
# 
# 
# # Correlation between Delta SCOPA AUT & Delta FDA-modified UMSARS V2 PUPPILO ------------------
# 
# # Inputs SCOPA Total V2 ----------------------------------
# 
# 
# AllMSA_Pop_Baseline_671 <- fread("Source/AllMSA_Pop_Baseline_671.txt")
# AllMSA_Pop_BaselineYear1_410 <- fread("Source/AllMSA_Pop_BaselineYear1_410.txt")
# AllMSA_Pop_BaselineYear1Year2_245 <- fread("Source/AllMSA_Pop_BaselineYear1Year2_245.txt")
# AllMSA_Pop_BaselineYear1Year2Year3Plus_158 <- fread("Source/AllMSA_Pop_BaselineYear1Year2Year3Plus_158.txt")
# 
# EarlyCT_Pop_Baseline_319 <- fread("Source/EarlyCT_Pop_Baseline_319.txt")
# EarlyCT_Pop_BaselineYear1_208 <- fread("Source/EarlyCT_Pop_BaselineYear1_208.txt")
# EarlyCT_Pop_BaselineYear1Year2_134 <- fread("Source/EarlyCT_Pop_BaselineYear1Year2_134.txt")
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 <- fread( "Source/EarlyCT_Pop_BaselineYear1Year2Year3Plus_99.txt")
# 
# dataCohorteManaged <- readRDS("Source/dataCohorteManaged.rds")
# 
# dataCohorteManaged <- dataCohorteManaged %>% group_by(NUM) %>% mutate(TIME_STUDY = ifelse( is.na(TIME_STUDY), 0, TIME_STUDY)) %>%
#   mutate(Year= ifelse(TIME_STUDY==0, 0,
#                       ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
#                              ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
#                                     ifelse(TIME_STUDY>=2.5 ,3, NA)))))
# 
# 
# SCOPA <- dataCohorteManaged %>%
#   select(NUM, DATECONSULT, TIME_STUDY, Year) %>% rename("DATE"="DATECONSULT")
# 
# 
# 
# 
# ams_scopa_bx_2015_16 <- haven::read_sas("Source/ams_scopa_bx_2015_16.sas7bdat")
# names(ams_scopa_bx_2015_16)
# ams_scopa_bx_2015_16 <- ams_scopa_bx_2015_16 %>% filter(!is.na(DATE))
# 
# ams_scopa_bx_2015_16[is.na(ams_scopa_bx_2015_16)] <- 0
# 
# mean(ams_scopa_bx_2015_16$SCOPA_TOT, na.rm=T)
# 
# ams_scopa_bx_2015_16 <- ams_scopa_bx_2015_16 %>%
#   mutate(SCOPA_DIG=SCOPA1+SCOPA2+SCOPA3+SCOPA4+SCOPA5+SCOPA6+SCOPA7) %>%
#   mutate(SCOPA_URI=SCOPA8+SCOPA9+SCOPA10+SCOPA11+SCOPA12+SCOPA13) %>%
#   mutate(SCOPA_CDV=SCOPA14+SCOPA15+SCOPA16) %>%
#   mutate(SCOPA_THE=SCOPA17+SCOPA18+SCOPA19+SCOPA20) %>%
#   mutate(SCOPA_PUP=SCOPA21) %>%
#   mutate(SCOPA_SEX=SCOPA22H+SCOPA22F+SCOPA23F+SCOPA23H) %>%
#   mutate(SCOPA_TOT=SCOPA_SEX+SCOPA_PUP+SCOPA_THE+SCOPA_CDV+SCOPA_URI+SCOPA_DIG)
# 
# ams_scopa_bx_2015_16 <- ams_scopa_bx_2015_16 %>%
#   select(NUM, DATE, SCOPA_TOT, SCOPA_SEX, SCOPA_PUP, SCOPA_THE, SCOPA_CDV, SCOPA_URI, SCOPA_DIG)
# 
# length(unique(ams_scopa_bx_2015_16$NUM))
# 
# 
# SCOPA <- ams_scopa_bx_2015_16 %>% inner_join(SCOPA)
# 
# SCOPA <- SCOPA %>% filter(SCOPA_TOT!=0)
# 
# length(unique(SCOPA$NUM))
# 
# 
# SCOPA_Baseline_Pats <- SCOPA %>% filter(Year==0) %>% filter(!is.na(SCOPA_TOT)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM) %>% distinct() %>%
#   inner_join(AllMSA_Pop_Baseline_671)
# 
# 
# 
# 
# UMSARS1 <- dataCohorteManaged %>%
#   select(NUM, DATECONSULT, TIME_STUDY, Year, UMSARS1_1:UMSARS1_TOT)
# 
# 
# UMSARS1$UMSARS1_TOT_v2 <- rowSums(UMSARS1[, 5:16], na.rm = TRUE)
# 
# UMSARS1$missing_na <- rowSums(is.na(UMSARS1[, 5:16]))
# 
# UMSARS1 <-  UMSARS1 %>%  select(NUM, TIME_STUDY, Year, UMSARS1_TOT, UMSARS1_11)
# 
# UMSARS1 <- UMSARS1 %>% mutate(UMSARS1_TOT_FDA=UMSARS1_TOT-UMSARS1_11)
# 
# names(SCOPA)
# 
# # ----------------
# # Overall Early CT V2  -------------------------------
# 
# # Year 1
# 
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   filter(!is.na(SCOPA_TOT)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
#   rename("Baseline"="SCOPA_PUP") %>%
#   left_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_TOT)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
#   ) %>%
#   mutate(Diff= (SCOPA_PUP-Baseline))  %>% select(NUM, DIAG, DIAGNIV, Diff) %>% rename("SCOPA_Diff"="Diff") %>% drop_na() %>%
#   inner_join(
# 
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
#       filter(!is.na(UMSARS1_TOT_FDA)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  UMSARS1_TOT_FDA) %>%
#       rename("Baseline"="UMSARS1_TOT_FDA") %>%
#       left_join(
#         EarlyCT_Pop_BaselineYear1_208 %>% inner_join(UMSARS1) %>% filter(Year==1) %>%
#           filter(!is.na(UMSARS1_TOT_FDA)) %>%
#           mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#           group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#           group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#           group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, UMSARS1_TOT_FDA)
#       ) %>%
#       mutate(Diff= (UMSARS1_TOT_FDA-Baseline))  %>% select(NUM, DIAG, DIAGNIV, Diff)
#   )  %>% #summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))
#   #group_by(DIAG) %>% summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))
#   group_by(DIAGNIV) %>% summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))
# 
# 
# 
# 
# 
# # Year 2
# 
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   filter(!is.na(SCOPA_TOT)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
#   rename("Baseline"="SCOPA_PUP") %>%
#   left_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_TOT)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
#   ) %>%
#   mutate(Diff= (SCOPA_PUP-Baseline))  %>% select(NUM, DIAG, DIAGNIV, Diff) %>% rename("SCOPA_Diff"="Diff") %>% drop_na() %>%
#   inner_join(
# 
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
#       filter(!is.na(UMSARS1_TOT_FDA)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  UMSARS1_TOT_FDA) %>%
#       rename("Baseline"="UMSARS1_TOT_FDA") %>%
#       left_join(
#         EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(UMSARS1) %>% filter(Year==2) %>%
#           filter(!is.na(UMSARS1_TOT_FDA)) %>%
#           mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#           group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#           group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#           group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, UMSARS1_TOT_FDA)
#       ) %>%
#       mutate(Diff= (UMSARS1_TOT_FDA-Baseline))   %>% select(NUM, DIAG, DIAGNIV, Diff)
#   )  %>%#   summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))
#   #group_by(DIAG) %>% summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))
#   group_by(DIAGNIV) %>% summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))
# 
# 
# # Year 3
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   filter(!is.na(SCOPA_TOT)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_PUP) %>%
#   rename("Baseline"="SCOPA_PUP") %>%
#   left_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_TOT)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_PUP)
#   ) %>%
#   mutate(Diff= (SCOPA_PUP-Baseline))  %>% select(NUM, DIAG, DIAGNIV, Diff) %>% rename("SCOPA_Diff"="Diff") %>% drop_na() %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
#       filter(!is.na(UMSARS1_TOT_FDA)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  UMSARS1_TOT_FDA) %>%
#       rename("Baseline"="UMSARS1_TOT_FDA") %>%
#       left_join(
#         EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(UMSARS1) %>% filter(Year==3) %>%
#           filter(!is.na(UMSARS1_TOT_FDA)) %>%
#           mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#           group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#           group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#           group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, UMSARS1_TOT_FDA)
#       ) %>%
#       mutate(Diff= (UMSARS1_TOT_FDA-Baseline)) %>% select(NUM, DIAG, DIAGNIV, Diff)
#   )  %>%# summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))
#   #group_by(DIAG) %>% summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))
#   group_by(DIAGNIV) %>% summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))
# 
# 
# # --------------
# 
# 
# 
# # Correlation between Delta SCOPA AUT & Delta FDA-modified UMSARS V2 SEXUAL ------------------
# 
# # Inputs SCOPA Total V2 ----------------------------------
# 
# 
# AllMSA_Pop_Baseline_671 <- fread("Source/AllMSA_Pop_Baseline_671.txt")
# AllMSA_Pop_BaselineYear1_410 <- fread("Source/AllMSA_Pop_BaselineYear1_410.txt")
# AllMSA_Pop_BaselineYear1Year2_245 <- fread("Source/AllMSA_Pop_BaselineYear1Year2_245.txt")
# AllMSA_Pop_BaselineYear1Year2Year3Plus_158 <- fread("Source/AllMSA_Pop_BaselineYear1Year2Year3Plus_158.txt")
# 
# EarlyCT_Pop_Baseline_319 <- fread("Source/EarlyCT_Pop_Baseline_319.txt")
# EarlyCT_Pop_BaselineYear1_208 <- fread("Source/EarlyCT_Pop_BaselineYear1_208.txt")
# EarlyCT_Pop_BaselineYear1Year2_134 <- fread("Source/EarlyCT_Pop_BaselineYear1Year2_134.txt")
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 <- fread( "Source/EarlyCT_Pop_BaselineYear1Year2Year3Plus_99.txt")
# 
# dataCohorteManaged <- readRDS("Source/dataCohorteManaged.rds")
# 
# dataCohorteManaged <- dataCohorteManaged %>% group_by(NUM) %>% mutate(TIME_STUDY = ifelse( is.na(TIME_STUDY), 0, TIME_STUDY)) %>%
#   mutate(Year= ifelse(TIME_STUDY==0, 0,
#                       ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
#                              ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
#                                     ifelse(TIME_STUDY>=2.5 ,3, NA)))))
# 
# 
# SCOPA <- dataCohorteManaged %>%
#   select(NUM, DATECONSULT, TIME_STUDY, Year) %>% rename("DATE"="DATECONSULT")
# 
# 
# 
# 
# ams_scopa_bx_2015_16 <- haven::read_sas("Source/ams_scopa_bx_2015_16.sas7bdat")
# names(ams_scopa_bx_2015_16)
# ams_scopa_bx_2015_16 <- ams_scopa_bx_2015_16 %>% filter(!is.na(DATE))
# 
# ams_scopa_bx_2015_16[is.na(ams_scopa_bx_2015_16)] <- 0
# 
# mean(ams_scopa_bx_2015_16$SCOPA_TOT, na.rm=T)
# 
# ams_scopa_bx_2015_16 <- ams_scopa_bx_2015_16 %>%
#   mutate(SCOPA_DIG=SCOPA1+SCOPA2+SCOPA3+SCOPA4+SCOPA5+SCOPA6+SCOPA7) %>%
#   mutate(SCOPA_URI=SCOPA8+SCOPA9+SCOPA10+SCOPA11+SCOPA12+SCOPA13) %>%
#   mutate(SCOPA_CDV=SCOPA14+SCOPA15+SCOPA16) %>%
#   mutate(SCOPA_THE=SCOPA17+SCOPA18+SCOPA19+SCOPA20) %>%
#   mutate(SCOPA_PUP=SCOPA21) %>%
#   mutate(SCOPA_SEX=SCOPA22H+SCOPA22F+SCOPA23F+SCOPA23H) %>%
#   mutate(SCOPA_TOT=SCOPA_SEX+SCOPA_PUP+SCOPA_THE+SCOPA_CDV+SCOPA_URI+SCOPA_DIG)
# 
# ams_scopa_bx_2015_16 <- ams_scopa_bx_2015_16 %>%
#   select(NUM, DATE, SCOPA_TOT, SCOPA_SEX, SCOPA_PUP, SCOPA_THE, SCOPA_CDV, SCOPA_URI, SCOPA_DIG)
# 
# length(unique(ams_scopa_bx_2015_16$NUM))
# 
# 
# SCOPA <- ams_scopa_bx_2015_16 %>% inner_join(SCOPA)
# 
# SCOPA <- SCOPA %>% filter(SCOPA_TOT!=0)
# 
# length(unique(SCOPA$NUM))
# 
# 
# SCOPA_Baseline_Pats <- SCOPA %>% filter(Year==0) %>% filter(!is.na(SCOPA_TOT)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM) %>% distinct() %>%
#   inner_join(AllMSA_Pop_Baseline_671)
# 
# 
# 
# 
# UMSARS1 <- dataCohorteManaged %>%
#   select(NUM, DATECONSULT, TIME_STUDY, Year, UMSARS1_1:UMSARS1_TOT)
# 
# 
# UMSARS1$UMSARS1_TOT_v2 <- rowSums(UMSARS1[, 5:16], na.rm = TRUE)
# 
# UMSARS1$missing_na <- rowSums(is.na(UMSARS1[, 5:16]))
# 
# UMSARS1 <-  UMSARS1 %>%  select(NUM, TIME_STUDY, Year, UMSARS1_TOT, UMSARS1_11)
# 
# UMSARS1 <- UMSARS1 %>% mutate(UMSARS1_TOT_FDA=UMSARS1_TOT-UMSARS1_11)
# 
# names(SCOPA)
# 
# # ----------------
# # Overall Early CT V2  -------------------------------
# 
# # Year 1
# 
# 
# EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==0) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   filter(!is.na(SCOPA_TOT)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
#   rename("Baseline"="SCOPA_SEX") %>%
#   left_join(
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
#       filter(!is.na(SCOPA_TOT)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
#   ) %>%
#   mutate(Diff= (SCOPA_SEX-Baseline))  %>% select(NUM, DIAG, DIAGNIV, Diff) %>% rename("SCOPA_Diff"="Diff") %>% drop_na() %>%
#   inner_join(
# 
#     EarlyCT_Pop_BaselineYear1_208 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
#       filter(!is.na(UMSARS1_TOT_FDA)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  UMSARS1_TOT_FDA) %>%
#       rename("Baseline"="UMSARS1_TOT_FDA") %>%
#       left_join(
#         EarlyCT_Pop_BaselineYear1_208 %>% inner_join(UMSARS1) %>% filter(Year==1) %>%
#           filter(!is.na(UMSARS1_TOT_FDA)) %>%
#           mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#           group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#           group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#           group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, UMSARS1_TOT_FDA)
#       ) %>%
#       mutate(Diff= (UMSARS1_TOT_FDA-Baseline))  %>% select(NUM, DIAG, DIAGNIV, Diff)
#   )  %>% summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))
# # group_by(DIAG) %>% summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))
# #group_by(DIAGNIV) %>% summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))
# 
# 
# 
# 
# 
# # Year 2
# 
# 
# EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==0) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   filter(!is.na(SCOPA_TOT)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
#   rename("Baseline"="SCOPA_SEX") %>%
#   left_join(
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
#       filter(!is.na(SCOPA_TOT)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
#   ) %>%
#   mutate(Diff= (SCOPA_SEX-Baseline))  %>% select(NUM, DIAG, DIAGNIV, Diff) %>% rename("SCOPA_Diff"="Diff") %>% drop_na() %>%
#   inner_join(
# 
#     EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
#       filter(!is.na(UMSARS1_TOT_FDA)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  UMSARS1_TOT_FDA) %>%
#       rename("Baseline"="UMSARS1_TOT_FDA") %>%
#       left_join(
#         EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(UMSARS1) %>% filter(Year==2) %>%
#           filter(!is.na(UMSARS1_TOT_FDA)) %>%
#           mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#           group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#           group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#           group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, UMSARS1_TOT_FDA)
#       ) %>%
#       mutate(Diff= (UMSARS1_TOT_FDA-Baseline))   %>% select(NUM, DIAG, DIAGNIV, Diff)
#   )  %>%   summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))
# #group_by(DIAG) %>% summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))
# #group_by(DIAGNIV) %>% summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))
# 
# 
# # Year 3
# 
# 
# EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==0) %>% inner_join(SCOPA_Baseline_Pats) %>%
#   filter(!is.na(SCOPA_TOT)) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  SCOPA_SEX) %>%
#   rename("Baseline"="SCOPA_SEX") %>%
#   left_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
#       filter(!is.na(SCOPA_TOT)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, SCOPA_SEX)
#   ) %>%
#   mutate(Diff= (SCOPA_SEX-Baseline))  %>% select(NUM, DIAG, DIAGNIV, Diff) %>% rename("SCOPA_Diff"="Diff") %>% drop_na() %>%
#   inner_join(
#     EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
#       filter(!is.na(UMSARS1_TOT_FDA)) %>%
#       mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#       group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#       group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#       group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV,  UMSARS1_TOT_FDA) %>%
#       rename("Baseline"="UMSARS1_TOT_FDA") %>%
#       left_join(
#         EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(UMSARS1) %>% filter(Year==3) %>%
#           filter(!is.na(UMSARS1_TOT_FDA)) %>%
#           mutate(Elapsed=abs(TIME_STUDY-Year)) %>%
#           group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#           group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#           group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM, DIAG, DIAGNIV, TIME_STUDY, UMSARS1_TOT_FDA)
#       ) %>%
#       mutate(Diff= (UMSARS1_TOT_FDA-Baseline)) %>% select(NUM, DIAG, DIAGNIV, Diff)
#   )  %>% summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))
# #group_by(DIAG) %>% summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))
# #group_by(DIAGNIV) %>% summarise(cor=cor(SCOPA_Diff  , Diff, method="spearman"))
# 
# 
# # --------------
# 
# 
