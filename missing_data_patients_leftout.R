# Number of missing patients patients missing data --------
# -------

# Inputs MSA QoL   ----------------------------------

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

SCHRAG %>% select(SCHRAG_TOT_v2, SCHRAG_TOT, SCHRAG_MOTOR, SCHRAG_NONMOTOR, SCHRAG_EMOTIONAL, ECHANQV) %>% 
  mutate(SCHRAG_TOT=ifelse(is.na(SCHRAG_TOT),0,SCHRAG_TOT)) %>% ungroup() %>%
  filter(SCHRAG_TOT  !=0) %>% summarise(SCHRAG_TOT =mean(SCHRAG_TOT , na.rm=T))


SCHRAG %>% select(SCHRAG_TOT_v2, SCHRAG_TOT, SCHRAG_MOTOR, SCHRAG_NONMOTOR, SCHRAG_EMOTIONAL, ECHANQV) %>%
  mutate(SCHRAG_TOT=ifelse(is.na(SCHRAG_TOT),0,SCHRAG_TOT)) %>% ungroup() %>%
  mutate(SCHRAG_TOT=ifelse(SCHRAG_TOT==0, SCHRAG_TOT_v2, SCHRAG_TOT)) %>%
  select(-SCHRAG_TOT_v2) %>% filter(!is.na(SCHRAG_TOT) & is.na(SCHRAG_NONMOTOR))


SCHRAG$SCHRAG_TOT_v2 <- rowSums(SCHRAG[, 5:44], na.rm = TRUE)
SCHRAG$SCHRAG_MOTOR_v2 <- rowSums(SCHRAG[, 5:18], na.rm = TRUE)
SCHRAG$SCHRAG_NONMOTOR_v2 <- rowSums(SCHRAG[, 19:30], na.rm = TRUE)
SCHRAG$SCHRAG_EMOTIONAL_v2 <- rowSums(SCHRAG[, 31:44], na.rm = TRUE)

SCHRAG <- SCHRAG %>% select(-c(SCHRAG_TOT, SCHRAG_MOTOR, SCHRAG_NONMOTOR, SCHRAG_EMOTIONAL))

#SCHRAG <- SCHRAG %>% select(NUM, DATECONSULT, TIME_STUDY, Year, SCHRAG_TOT_v2, SCHRAG_MOTOR_v2, SCHRAG_NONMOTOR_v2, SCHRAG_EMOTIONAL_v2, ECHANQV)
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
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% select(NUM) %>% distinct() %>%
  inner_join(AllMSA_Pop_Baseline_671)





# ----------------
# Overall MSA Entire -------------------------------

# ALL SCHRAG Total Year 0 

AllMSA_Pop_Baseline_671 %>% inner_join(SCHRAG) %>% filter(Year==0) %>% 
  filter(!is.na(SCHRAG_TOT_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCHRAG_Baseline_Pats) %>%
  filter(number_na!=0) %>% filter(!is.na(number_na)) %>%
  summarise(
    mean=mean(number_na),
    sd=sd(number_na),
    median=median(number_na),
    Q1 = quantile(number_na, 0.25),
    Q3 = quantile(number_na, 0.75),
    min=min(number_na),
    max=max(number_na),
    n=n()
  )



# ALL SCHRAG Total Year 1

AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
  filter(!is.na(SCHRAG_TOT_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCHRAG_Baseline_Pats) %>%
  filter(number_na!=0) %>% filter(!is.na(number_na)) %>%
  summarise(
    mean=mean(number_na),
    sd=sd(number_na),
    median=median(number_na),
    Q1 = quantile(number_na, 0.25),
    Q3 = quantile(number_na, 0.75),
    min=min(number_na),
    max=max(number_na),
    n=n()
  )




# ALL SCHRAG Total Year 2

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCHRAG) %>% filter(Year==2) %>%
  filter(!is.na(SCHRAG_TOT_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCHRAG_Baseline_Pats) %>%
  filter(number_na!=0) %>% filter(!is.na(number_na)) %>%
  summarise(
    mean=mean(number_na),
    sd=sd(number_na),
    median=median(number_na),
    Q1 = quantile(number_na, 0.25),
    Q3 = quantile(number_na, 0.75),
    min=min(number_na),
    max=max(number_na),
    n=n()
  )





# ALL SCHRAG Total Year 3

AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCHRAG) %>% filter(Year==3) %>%
  filter(!is.na(SCHRAG_TOT_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCHRAG_Baseline_Pats) %>%
  filter(number_na!=0) %>% filter(!is.na(number_na)) %>%
  summarise(
    mean=mean(number_na),
    sd=sd(number_na),
    median=median(number_na),
    Q1 = quantile(number_na, 0.25),
    Q3 = quantile(number_na, 0.75),
    min=min(number_na),
    max=max(number_na),
    n=n()
  )

# -------------------
# Overall Early CT  -------------------------------

# ALL SCHRAG Total Year 0 


EarlyCT_Pop_Baseline_319 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  filter(!is.na(SCHRAG_TOT_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCHRAG_Baseline_Pats) %>%
  filter(number_na!=0) %>% filter(!is.na(number_na)) %>%
  summarise(
    mean=mean(number_na),
    sd=sd(number_na),
    median=median(number_na),
    Q1 = quantile(number_na, 0.25),
    Q3 = quantile(number_na, 0.75),
    min=min(number_na),
    max=max(number_na),
    n=n()
  )


# ALL SCHRAG Total Year 1

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==1) %>%
  filter(!is.na(SCHRAG_TOT_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCHRAG_Baseline_Pats) %>%
  filter(number_na!=0) %>% filter(!is.na(number_na)) %>%
  summarise(
    mean=mean(number_na),
    sd=sd(number_na),
    median=median(number_na),
    Q1 = quantile(number_na, 0.25),
    Q3 = quantile(number_na, 0.75),
    min=min(number_na),
    max=max(number_na),
    n=n()
  )



# ALL SCHRAG Total Year 2

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==2) %>%
  filter(!is.na(SCHRAG_TOT_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCHRAG_Baseline_Pats) %>%
  filter(number_na!=0) %>% filter(!is.na(number_na)) %>%
  summarise(
    mean=mean(number_na),
    sd=sd(number_na),
    median=median(number_na),
    Q1 = quantile(number_na, 0.25),
    Q3 = quantile(number_na, 0.75),
    min=min(number_na),
    max=max(number_na),
    n=n()
  )


# ALL SCHRAG Total Year 3

EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==3) %>%
  filter(!is.na(SCHRAG_TOT_v2)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCHRAG_Baseline_Pats) %>%
  filter(number_na!=0) %>% filter(!is.na(number_na)) %>%
  summarise(
    mean=mean(number_na),
    sd=sd(number_na),
    median=median(number_na),
    Q1 = quantile(number_na, 0.25),
    Q3 = quantile(number_na, 0.75),
    min=min(number_na),
    max=max(number_na),
    n=n()
  )


# --------------



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

#ams_scopa_bx_2015_16[is.na(ams_scopa_bx_2015_16)] <- 0

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



ams_scopa_bx_2015_16 <- ams_scopa_bx_2015_16 %>% 
  select(NUM, DATE, SCOPA_TOT, SCOPA_SEX, SCOPA_PUP, SCOPA_THE, SCOPA_CDV, SCOPA_URI, SCOPA_DIG, number_na)

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
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
filter(number_na!=0) %>% filter(!is.na(number_na)) %>%
  summarise(
    mean=mean(number_na),
    sd=sd(number_na),
    median=median(number_na),
    Q1 = quantile(number_na, 0.25),
    Q3 = quantile(number_na, 0.75),
    min=min(number_na),
    max=max(number_na),
    n=n()
  )




# ALL SCOPA Total Year 1

AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
filter(number_na!=0) %>% filter(!is.na(number_na)) %>%
  summarise(
    mean=mean(number_na),
    sd=sd(number_na),
    median=median(number_na),
    Q1 = quantile(number_na, 0.25),
    Q3 = quantile(number_na, 0.75),
    min=min(number_na),
    max=max(number_na),
    n=n()
  )





# ALL SCOPA Total Year 2

AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
filter(number_na!=0) %>% filter(!is.na(number_na)) %>%
  summarise(
    mean=mean(number_na),
    sd=sd(number_na),
    median=median(number_na),
    Q1 = quantile(number_na, 0.25),
    Q3 = quantile(number_na, 0.75),
    min=min(number_na),
    max=max(number_na),
    n=n()
  )




# ALL SCOPA Total Year 3

AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
filter(number_na!=0) %>% filter(!is.na(number_na)) %>%
  summarise(
    mean=mean(number_na),
    sd=sd(number_na),
    median=median(number_na),
    Q1 = quantile(number_na, 0.25),
    Q3 = quantile(number_na, 0.75),
    min=min(number_na),
    max=max(number_na),
    n=n()
  )



# -----------------------------
# Overall Early CT V2  -------------------------------

# ALL SCOPA Total Year 0 


EarlyCT_Pop_Baseline_319 %>% inner_join(SCOPA) %>% filter(Year==0) %>%
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
filter(number_na!=0) %>% filter(!is.na(number_na)) %>%
  summarise(
    mean=mean(number_na),
    sd=sd(number_na),
    median=median(number_na),
    Q1 = quantile(number_na, 0.25),
    Q3 = quantile(number_na, 0.75),
    min=min(number_na),
    max=max(number_na),
    n=n()
  )




# ALL SCOPA Total Year 1

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCOPA) %>% filter(Year==1) %>%
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
filter(number_na!=0) %>% filter(!is.na(number_na)) %>%
  summarise(
    mean=mean(number_na),
    sd=sd(number_na),
    median=median(number_na),
    Q1 = quantile(number_na, 0.25),
    Q3 = quantile(number_na, 0.75),
    min=min(number_na),
    max=max(number_na),
    n=n()
  )


# ALL SCOPA Total Year 2

EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCOPA) %>% filter(Year==2) %>%
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
filter(number_na!=0) %>% filter(!is.na(number_na)) %>%
  summarise(
    mean=mean(number_na),
    sd=sd(number_na),
    median=median(number_na),
    Q1 = quantile(number_na, 0.25),
    Q3 = quantile(number_na, 0.75),
    min=min(number_na),
    max=max(number_na),
    n=n()
  )




# ALL SCOPA Total Year 3

EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCOPA) %>% filter(Year==3) %>%
  filter(!is.na(SCOPA_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% 
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% inner_join(SCOPA_Baseline_Pats) %>%
 filter(number_na!=0) %>% filter(!is.na(number_na)) %>%
  summarise(
    mean=mean(number_na),
    sd=sd(number_na),
    median=median(number_na),
    Q1 = quantile(number_na, 0.25),
    Q3 = quantile(number_na, 0.75),
    min=min(number_na),
    max=max(number_na),
    n=n()
  )




# --------------

