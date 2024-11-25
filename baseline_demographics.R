
library(tidyverse)
library(data.table)
options(scipen = 999)



# Pats to track -----------------------


FA_Takeda <- readxl::read_xlsx("Source/FA_Takeda.xlsx")
FA_Takeda$DateIRM <-  as.Date(FA_Takeda$DateIRM)
names(FA_Takeda)

Left_Putamen_FA <- FA_Takeda %>% select(NUM, DateIRM, FA_L_Puta)


Left_Putamen_FA <- Visits_zero %>% inner_join(Left_Putamen_FA) %>%
  mutate(TIME_STUDY=time_length(difftime(DateIRM, dateVisite0 ), "years")) %>%
  mutate(Year= ifelse(TIME_STUDY<=0.5, 0,
                      ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                             ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                    ifelse(TIME_STUDY>=2.5 ,3, NA))))) 

Left_Putamen_FA <- Left_Putamen_FA %>% 
  drop_na() %>% arrange(NUM, DateIRM) %>%
  group_by(NUM, DateIRM) %>% mutate(FA_L_Puta=mean(FA_L_Puta)) %>%
  distinct()

pats_to_track <- AllMSA_Pop_Baseline_671 %>% inner_join(Left_Putamen_FA) %>% filter(Year==0) %>%
  select(NUM) %>% distinct()


# ---------

# Inputs, cleanup ----------------------------------

AllMSA_Pop_Baseline_671 <- fread("Source/AllMSA_Pop_Baseline_671.txt")
EarlyCT_Pop_Baseline_319 <- fread("Source/EarlyCT_Pop_Baseline_319.txt")
dataCohorteManaged <- readRDS("Source/dataCohorteManaged.rds")


BaselineDem <- dataCohorteManaged  %>% 
  select(NUM, DATECONSULT, DATENAIS, SEXE, DATE_VISITE0, annee_vis0, ANDIAG, ANDEBSYMPT, AGE_VISITE0, DELAI_CHARGE) %>%
  mutate(AGE_DEBSYMPT=AGE_VISITE0-DELAI_CHARGE)  %>%
  inner_join(AllMSA_Pop_Baseline_671) 

# First visit, to replace missing visit 0s
BaselineDem <- BaselineDem %>% left_join(BaselineDem %>% group_by(NUM) %>% summarise(MIN_DATECONSULT=min(DATECONSULT))) 

# Distinct
BaselineDem <- BaselineDem %>% select(-DATECONSULT) %>% distinct()

# Year of symptom onset, min because some have different ones
BaselineDem <- BaselineDem %>% group_by(NUM) %>% mutate(ANDEBSYMPT=min(ANDEBSYMPT, na.rm=T))
  
# Year of first visit, min because some have different ones
BaselineDem <- BaselineDem %>% group_by(NUM) %>% mutate(annee_vis0=min(annee_vis0, na.rm=T))

# Year of first diagnosis, min because some have different ones
BaselineDem <- BaselineDem %>% group_by(NUM) %>% mutate(ANDIAG=min(ANDIAG, na.rm=T))

# Years elapsed from symptom to visit 
BaselineDem <- BaselineDem %>% group_by(NUM) %>% mutate(DELAI_CHARGE=annee_vis0-ANDEBSYMPT)

# Years elapsed from diagnosis to visit 

BaselineDem <- BaselineDem %>% group_by(NUM) %>% mutate(DELAI_DIAG=annee_vis0-ANDIAG)

# missing dates of first visit
BaselineDem <- BaselineDem %>% group_by(NUM) %>% mutate(DATE_VISITE0=if_else(is.na(DATE_VISITE0), MIN_DATECONSULT, DATE_VISITE0))

# age at first visit; calculate for missing ones
BaselineDem <- BaselineDem %>%  group_by(NUM) %>%
  mutate(AGE_VISITE0=if_else(is.na(AGE_VISITE0), as.numeric(DATE_VISITE0-DATENAIS)/365.25, AGE_VISITE0))

BaselineDem <- BaselineDem %>% group_by(NUM) %>%
  mutate(AGE_DEBSYMPT=if_else(is.na(AGE_DEBSYMPT), as.numeric(AGE_VISITE0-DELAI_CHARGE), AGE_DEBSYMPT))

BaselineDem <- BaselineDem %>% group_by(NUM) %>% mutate(AGE_DEBSYMPT=min(AGE_DEBSYMPT, na.rm=T))

names(BaselineDem)
range(BaselineDem$DATENAIS)
range(BaselineDem$DATE_VISITE0)
range(BaselineDem$annee_vis0)
range(BaselineDem$ANDEBSYMPT)
range(BaselineDem$DELAI_CHARGE)
range(BaselineDem$DELAI_DIAG)
range(BaselineDem$MIN_DATECONSULT)
range(BaselineDem$AGE_VISITE0)
range(BaselineDem$AGE_DEBSYMPT)

data.frame(
  AllMSA_Pop_Baseline_671 %>% 
  inner_join(BaselineDem) %>%
    distinct()  %>% 
  group_by(NUM) %>% count() %>% filter(n>1) %>%
  left_join(BaselineDem) 
  )

# --------------

# SUMMARY STATISTICS ---------------

# Overall -------------------

# EarlyCT 319


# AGE_VISITE0
AllMSA_Pop_Baseline_671  %>% inner_join(BaselineDem) %>% distinct() %>%
  inner_join(pats_to_track) %>%
  summarise(
    mean=mean(AGE_VISITE0),
    sd=sd(AGE_VISITE0),
    median=median(AGE_VISITE0),
    Q1 = quantile(AGE_VISITE0, 0.25),
    Q3 = quantile(AGE_VISITE0, 0.75),
    min=min(AGE_VISITE0),
    max=max(AGE_VISITE0),
    n=n()
  )


# AGE_DEBSYMPT
AllMSA_Pop_Baseline_671 %>% inner_join(BaselineDem) %>% distinct() %>%
  inner_join(pats_to_track) %>%
  summarise(
    mean=mean(AGE_DEBSYMPT),
    sd=sd(AGE_DEBSYMPT),
    median=median(AGE_DEBSYMPT),
    Q1 = quantile(AGE_DEBSYMPT, 0.25),
    Q3 = quantile(AGE_DEBSYMPT, 0.75),
    min=min(AGE_DEBSYMPT),
    max=max(AGE_DEBSYMPT),
    n=n()
  )


# DELAI_CHARGE
AllMSA_Pop_Baseline_671 %>% inner_join(BaselineDem) %>% distinct() %>%
  inner_join(pats_to_track) %>%
  summarise(
    mean=mean(DELAI_CHARGE),
    sd=sd(DELAI_CHARGE),
    median=median(DELAI_CHARGE),
    Q1 = quantile(DELAI_CHARGE, 0.25),
    Q3 = quantile(DELAI_CHARGE, 0.75),
    min=min(DELAI_CHARGE),
    max=max(DELAI_CHARGE),
    n=n()
  )

# DELAI_DIAG
AllMSA_Pop_Baseline_671 %>% inner_join(BaselineDem) %>% distinct() %>%
  inner_join(pats_to_track) %>%
  summarise(
    mean=mean(DELAI_DIAG),
    sd=sd(DELAI_DIAG),
    median=median(DELAI_DIAG),
    Q1 = quantile(DELAI_DIAG, 0.25),
    Q3 = quantile(DELAI_DIAG, 0.75),
    min=min(DELAI_DIAG),
    max=max(DELAI_DIAG),
    n=n()
  )


AllMSA_Pop_Baseline_671 %>% inner_join(BaselineDem) %>% 
  distinct() %>%
  inner_join(pats_to_track) %>%
  group_by(SEXE) %>% count()


# ALL MSA 671


# AGE_VISITE0
AllMSA_Pop_Baseline_671 %>% inner_join(BaselineDem) %>% distinct() %>%
  summarise(
    mean=mean(AGE_VISITE0),
    sd=sd(AGE_VISITE0),
    median=median(AGE_VISITE0),
    Q1 = quantile(AGE_VISITE0, 0.25),
    Q3 = quantile(AGE_VISITE0, 0.75),
    min=min(AGE_VISITE0),
    max=max(AGE_VISITE0),
    n=n()
  )


# AGE_DEBSYMPT
AllMSA_Pop_Baseline_671 %>% inner_join(BaselineDem) %>% distinct() %>%
  summarise(
    mean=mean(AGE_DEBSYMPT),
    sd=sd(AGE_DEBSYMPT),
    median=median(AGE_DEBSYMPT),
    Q1 = quantile(AGE_DEBSYMPT, 0.25),
    Q3 = quantile(AGE_DEBSYMPT, 0.75),
    min=min(AGE_DEBSYMPT),
    max=max(AGE_DEBSYMPT),
    n=n()
  )


# DELAI_CHARGE
AllMSA_Pop_Baseline_671 %>% inner_join(BaselineDem) %>% distinct() %>%
  summarise(
    mean=mean(DELAI_CHARGE),
    sd=sd(DELAI_CHARGE),
    median=median(DELAI_CHARGE),
    Q1 = quantile(DELAI_CHARGE, 0.25),
    Q3 = quantile(DELAI_CHARGE, 0.75),
    min=min(DELAI_CHARGE),
    max=max(DELAI_CHARGE),
    n=n()
  )


# DELAI_DIAG
AllMSA_Pop_Baseline_671 %>% inner_join(BaselineDem) %>% distinct() %>%
  summarise(
    mean=mean(DELAI_DIAG),
    sd=sd(DELAI_DIAG),
    median=median(DELAI_DIAG),
    Q1 = quantile(DELAI_DIAG, 0.25),
    Q3 = quantile(DELAI_DIAG, 0.75),
    min=min(DELAI_DIAG),
    max=max(DELAI_DIAG),
    n=n()
  )

AllMSA_Pop_Baseline_671 %>% inner_join(BaselineDem) %>% 
  distinct() %>%
  group_by(SEXE) %>% 
  count()

# ----------------------

# By DIAG TYPE ------------


# EarlyCT 319


# AGE_VISITE0
AllMSA_Pop_Baseline_671 %>% inner_join(BaselineDem) %>% distinct() %>%
  inner_join(pats_to_track) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(AGE_VISITE0),
    sd=sd(AGE_VISITE0),
    median=median(AGE_VISITE0),
    Q1 = quantile(AGE_VISITE0, 0.25),
    Q3 = quantile(AGE_VISITE0, 0.75),
    min=min(AGE_VISITE0),
    max=max(AGE_VISITE0),
    n=n()
  )


# AGE_DEBSYMPT
AllMSA_Pop_Baseline_671 %>% inner_join(BaselineDem) %>% distinct() %>%
  inner_join(pats_to_track) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(AGE_DEBSYMPT),
    sd=sd(AGE_DEBSYMPT),
    median=median(AGE_DEBSYMPT),
    Q1 = quantile(AGE_DEBSYMPT, 0.25),
    Q3 = quantile(AGE_DEBSYMPT, 0.75),
    min=min(AGE_DEBSYMPT),
    max=max(AGE_DEBSYMPT),
    n=n()
  )


# DELAI_CHARGE
AllMSA_Pop_Baseline_671 %>% inner_join(BaselineDem) %>% distinct() %>%
  inner_join(pats_to_track) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(DELAI_CHARGE),
    sd=sd(DELAI_CHARGE),
    median=median(DELAI_CHARGE),
    Q1 = quantile(DELAI_CHARGE, 0.25),
    Q3 = quantile(DELAI_CHARGE, 0.75),
    min=min(DELAI_CHARGE),
    max=max(DELAI_CHARGE),
    n=n()
  )


# DELAI_DIAG
AllMSA_Pop_Baseline_671 %>% inner_join(BaselineDem) %>% distinct() %>%
  inner_join(pats_to_track) %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(DELAI_DIAG),
    sd=sd(DELAI_DIAG),
    median=median(DELAI_DIAG),
    Q1 = quantile(DELAI_DIAG, 0.25),
    Q3 = quantile(DELAI_DIAG, 0.75),
    min=min(DELAI_DIAG),
    max=max(DELAI_DIAG),
    n=n()
  )


AllMSA_Pop_Baseline_671 %>% inner_join(BaselineDem) %>% 
  distinct() %>%
  inner_join(pats_to_track) %>%
  group_by(DIAG, SEXE) %>% count()


# ALL MSA 671


# AGE_VISITE0
AllMSA_Pop_Baseline_671 %>% inner_join(BaselineDem) %>% distinct() %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(AGE_VISITE0),
    sd=sd(AGE_VISITE0),
    median=median(AGE_VISITE0),
    Q1 = quantile(AGE_VISITE0, 0.25),
    Q3 = quantile(AGE_VISITE0, 0.75),
    min=min(AGE_VISITE0),
    max=max(AGE_VISITE0),
    n=n()
  )


# AGE_DEBSYMPT
AllMSA_Pop_Baseline_671 %>% inner_join(BaselineDem) %>% distinct() %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(AGE_DEBSYMPT),
    sd=sd(AGE_DEBSYMPT),
    median=median(AGE_DEBSYMPT),
    Q1 = quantile(AGE_DEBSYMPT, 0.25),
    Q3 = quantile(AGE_DEBSYMPT, 0.75),
    min=min(AGE_DEBSYMPT),
    max=max(AGE_DEBSYMPT),
    n=n()
  )


# DELAI_CHARGE
AllMSA_Pop_Baseline_671 %>% inner_join(BaselineDem) %>% distinct() %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(DELAI_CHARGE),
    sd=sd(DELAI_CHARGE),
    median=median(DELAI_CHARGE),
    Q1 = quantile(DELAI_CHARGE, 0.25),
    Q3 = quantile(DELAI_CHARGE, 0.75),
    min=min(DELAI_CHARGE),
    max=max(DELAI_CHARGE),
    n=n()
  )

# DELAI_DIAG
AllMSA_Pop_Baseline_671 %>% inner_join(BaselineDem) %>% distinct() %>%
  group_by(DIAG) %>%
  summarise(
    mean=mean(DELAI_DIAG),
    sd=sd(DELAI_DIAG),
    median=median(DELAI_DIAG),
    Q1 = quantile(DELAI_DIAG, 0.25),
    Q3 = quantile(DELAI_DIAG, 0.75),
    min=min(DELAI_DIAG),
    max=max(DELAI_DIAG),
    n=n()
  )


AllMSA_Pop_Baseline_671 %>% inner_join(BaselineDem) %>% 
  distinct() %>%
  group_by(DIAG, SEXE) %>% 
  count()


# --------------


# BY DIAG NIV -------------



# EarlyCT 319


# AGE_VISITE0
AllMSA_Pop_Baseline_671 %>% inner_join(BaselineDem) %>% distinct() %>%
  inner_join(pats_to_track) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(AGE_VISITE0),
    sd=sd(AGE_VISITE0),
    median=median(AGE_VISITE0),
    Q1 = quantile(AGE_VISITE0, 0.25),
    Q3 = quantile(AGE_VISITE0, 0.75),
    min=min(AGE_VISITE0),
    max=max(AGE_VISITE0),
    n=n()
  )


# AGE_DEBSYMPT
AllMSA_Pop_Baseline_671 %>% inner_join(BaselineDem) %>% distinct() %>%
  inner_join(pats_to_track) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(AGE_DEBSYMPT),
    sd=sd(AGE_DEBSYMPT),
    median=median(AGE_DEBSYMPT),
    Q1 = quantile(AGE_DEBSYMPT, 0.25),
    Q3 = quantile(AGE_DEBSYMPT, 0.75),
    min=min(AGE_DEBSYMPT),
    max=max(AGE_DEBSYMPT),
    n=n()
  )


# DELAI_CHARGE
AllMSA_Pop_Baseline_671 %>% inner_join(BaselineDem) %>% distinct() %>%
  inner_join(pats_to_track) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(DELAI_CHARGE),
    sd=sd(DELAI_CHARGE),
    median=median(DELAI_CHARGE),
    Q1 = quantile(DELAI_CHARGE, 0.25),
    Q3 = quantile(DELAI_CHARGE, 0.75),
    min=min(DELAI_CHARGE),
    max=max(DELAI_CHARGE),
    n=n()
  )



# DELAI_DIAG
AllMSA_Pop_Baseline_671 %>% inner_join(BaselineDem) %>% distinct() %>%
  inner_join(pats_to_track) %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(DELAI_DIAG),
    sd=sd(DELAI_DIAG),
    median=median(DELAI_DIAG),
    Q1 = quantile(DELAI_DIAG, 0.25),
    Q3 = quantile(DELAI_DIAG, 0.75),
    min=min(DELAI_DIAG),
    max=max(DELAI_DIAG),
    n=n()
  )


AllMSA_Pop_Baseline_671 %>% inner_join(BaselineDem) %>% 
  distinct() %>%
  inner_join(pats_to_track) %>%
  group_by(DIAGNIV, SEXE) %>% count()


# ALL MSA 671


# AGE_VISITE0
AllMSA_Pop_Baseline_671 %>% inner_join(BaselineDem) %>% distinct() %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(AGE_VISITE0),
    sd=sd(AGE_VISITE0),
    median=median(AGE_VISITE0),
    Q1 = quantile(AGE_VISITE0, 0.25),
    Q3 = quantile(AGE_VISITE0, 0.75),
    min=min(AGE_VISITE0),
    max=max(AGE_VISITE0),
    n=n()
  )


# AGE_DEBSYMPT
AllMSA_Pop_Baseline_671 %>% inner_join(BaselineDem) %>% distinct() %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(AGE_DEBSYMPT),
    sd=sd(AGE_DEBSYMPT),
    median=median(AGE_DEBSYMPT),
    Q1 = quantile(AGE_DEBSYMPT, 0.25),
    Q3 = quantile(AGE_DEBSYMPT, 0.75),
    min=min(AGE_DEBSYMPT),
    max=max(AGE_DEBSYMPT),
    n=n()
  )


# DELAI_CHARGE
AllMSA_Pop_Baseline_671 %>% inner_join(BaselineDem) %>% distinct() %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(DELAI_CHARGE),
    sd=sd(DELAI_CHARGE),
    median=median(DELAI_CHARGE),
    Q1 = quantile(DELAI_CHARGE, 0.25),
    Q3 = quantile(DELAI_CHARGE, 0.75),
    min=min(DELAI_CHARGE),
    max=max(DELAI_CHARGE),
    n=n()
  )



# DELAI_DIAG
AllMSA_Pop_Baseline_671 %>% inner_join(BaselineDem) %>% distinct() %>%
  group_by(DIAGNIV) %>%
  summarise(
    mean=mean(DELAI_DIAG),
    sd=sd(DELAI_DIAG),
    median=median(DELAI_DIAG),
    Q1 = quantile(DELAI_DIAG, 0.25),
    Q3 = quantile(DELAI_DIAG, 0.75),
    min=min(DELAI_DIAG),
    max=max(DELAI_DIAG),
    n=n()
  )


AllMSA_Pop_Baseline_671 %>% inner_join(BaselineDem) %>% 
  distinct() %>%
  group_by(DIAGNIV, SEXE) %>% 
  count()


# --------------
