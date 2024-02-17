# Define Populations: ALL MSA / EarlyCT, MSA-C / MSA-P, Prob / Poss, 0, +1, +2, +3 ---------

sympathIds <- readRDS("Source/sympathIds.rds")

sympathIds <- sympathIds %>% filter(randomisation %in% c("PD03", "PD01")) %>% select(NUM) %>% distinct() %>% drop_na()

dataCohorteManaged <- readRDS("Source/dataCohorteManaged.rds")
dataCohorteManaged <- dataCohorteManaged %>% filter(NUM!="2MILEO1145"&NUM!="2DUJEA0242")

length(unique(dataCohorteManaged$NUM)) #730

ClinicalTrialPats <- fread("Source/ClinicalTrialPats.txt")
ClinicalTrialPats <- ClinicalTrialPats %>% filter(NUM!="2MILEO1145"&NUM!="2DUJEA0242")

length(unique(ClinicalTrialPats$NUM)) #337

dataCohorteManaged <- dataCohorteManaged %>% anti_join(sympathIds)
ClinicalTrialPats <- ClinicalTrialPats %>% anti_join(sympathIds)

length(unique(dataCohorteManaged$NUM)) #712
length(unique(ClinicalTrialPats$NUM)) #320



# Years 0  1  2  +3

# All MSA

AllMSA_Pop_Baseline_671 <- dataCohorteManaged %>% 
  filter(!is.na(UMSARS4) & !is.na(UMSARS1_TOT) & !is.na(UMSARS2_TOT)) %>%
  select(NUM, TIME_STUDY)  %>% 
  # inner_join(ClinicalTrialPats) %>%   # If early MSA Pop
  arrange(NUM, TIME_STUDY) %>% group_by(NUM) %>%
  mutate(TIME_STUDY = ifelse( is.na(TIME_STUDY), 0, TIME_STUDY)) %>%
  mutate(Year= ifelse(TIME_STUDY==0, 0,
                      ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                             ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                    ifelse(TIME_STUDY>=2.5 & TIME_STUDY<3.5,3, 4))))) %>%
  ungroup()  %>% select(NUM, Year) %>% distinct() %>%
  filter(Year==0 ) %>% group_by(NUM) %>% count() %>% filter(n==1) %>%
  select(NUM) %>% distinct() # 671  

fwrite(AllMSA_Pop_Baseline_671, "Source/AllMSA_Pop_Baseline_671.txt")

AllMSA_Pop_BaselineYear1_410 <- dataCohorteManaged %>% 
  filter(!is.na(UMSARS4) & !is.na(UMSARS1_TOT) & !is.na(UMSARS2_TOT)) %>%
  select(NUM, TIME_STUDY)  %>% 
  # inner_join(ClinicalTrialPats) %>%   # If early MSA Pop
  arrange(NUM, TIME_STUDY) %>% group_by(NUM) %>%
  mutate(TIME_STUDY = ifelse( is.na(TIME_STUDY), 0, TIME_STUDY)) %>%
  mutate(Year= ifelse(TIME_STUDY==0, 0,
                      ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                             ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                    ifelse(TIME_STUDY>=2.5 & TIME_STUDY<3.5,3, 4))))) %>%
  ungroup()  %>% select(NUM, Year) %>% distinct() %>%
  filter(Year==0  | Year==1) %>% group_by(NUM) %>% count() %>% filter(n==2) %>%
  select(NUM) %>% distinct() # 410  

fwrite(AllMSA_Pop_BaselineYear1_410, "Source/AllMSA_Pop_BaselineYear1_410.txt")


AllMSA_Pop_BaselineYear1Year2_245 <- dataCohorteManaged %>% 
  filter(!is.na(UMSARS4) & !is.na(UMSARS1_TOT) & !is.na(UMSARS2_TOT)) %>%
  select(NUM, TIME_STUDY)  %>% 
  # inner_join(ClinicalTrialPats) %>%   # If early MSA Pop
  arrange(NUM, TIME_STUDY) %>% group_by(NUM) %>%
  mutate(TIME_STUDY = ifelse( is.na(TIME_STUDY), 0, TIME_STUDY)) %>%
  mutate(Year= ifelse(TIME_STUDY==0, 0,
                      ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                             ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                    ifelse(TIME_STUDY>=2.5 ,3, NA))))) %>%
  ungroup()  %>% select(NUM, Year) %>% distinct() %>%
  filter(Year==0  | Year==1 | Year==2) %>% group_by(NUM) %>% count() %>% filter(n==3) %>%
  select(NUM) %>% distinct() # 245  

fwrite(AllMSA_Pop_BaselineYear1Year2_245, "Source/AllMSA_Pop_BaselineYear1Year2_245.txt")



AllMSA_Pop_BaselineYear1Year2Year3Plus_158 <- dataCohorteManaged %>% 
  filter(!is.na(UMSARS4) & !is.na(UMSARS1_TOT) & !is.na(UMSARS2_TOT)) %>%
  select(NUM, TIME_STUDY)  %>% 
  # inner_join(ClinicalTrialPats) %>%   # If early MSA Pop
  arrange(NUM, TIME_STUDY) %>% group_by(NUM) %>%
  mutate(TIME_STUDY = ifelse( is.na(TIME_STUDY), 0, TIME_STUDY)) %>%
  mutate(Year= ifelse(TIME_STUDY==0, 0,
                      ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                             ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                    ifelse(TIME_STUDY>=2.5 ,3, NA))))) %>%
  ungroup()  %>% select(NUM, Year) %>% distinct() %>%
  filter(Year==0  | Year==1 | Year==2  | Year==3) %>% group_by(NUM) %>% count() %>% filter(n==4) %>%
  select(NUM) %>% distinct() # 158  

fwrite(AllMSA_Pop_BaselineYear1Year2Year3Plus_158, "Source/AllMSA_Pop_BaselineYear1Year2Year3Plus_158.txt")





# Early CT Takeda Pop



EarlyCT_Pop_Baseline_319 <- dataCohorteManaged %>% 
  filter(!is.na(UMSARS4) & !is.na(UMSARS1_TOT) & !is.na(UMSARS2_TOT)) %>%
  select(NUM, TIME_STUDY)  %>% 
  inner_join(ClinicalTrialPats) %>%   # If early MSA Pop
  arrange(NUM, TIME_STUDY) %>% group_by(NUM) %>%
  mutate(TIME_STUDY = ifelse( is.na(TIME_STUDY), 0, TIME_STUDY)) %>%
  mutate(Year= ifelse(TIME_STUDY==0, 0,
                      ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                             ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                    ifelse(TIME_STUDY>=2.5 & TIME_STUDY<3.5,3, 4))))) %>%
  ungroup()  %>% select(NUM, Year) %>% distinct() %>%
  filter(Year==0 ) %>% group_by(NUM) %>% count() %>% filter(n==1) %>%
  select(NUM) %>% distinct() # 319  

fwrite(EarlyCT_Pop_Baseline_319, "Source/EarlyCT_Pop_Baseline_319.txt")



EarlyCT_Pop_BaselineYear1_208 <- dataCohorteManaged %>% 
  filter(!is.na(UMSARS4) & !is.na(UMSARS1_TOT) & !is.na(UMSARS2_TOT)) %>%
  select(NUM, TIME_STUDY)  %>% 
  inner_join(ClinicalTrialPats) %>%   # If early MSA Pop
  arrange(NUM, TIME_STUDY) %>% group_by(NUM) %>%
  mutate(TIME_STUDY = ifelse( is.na(TIME_STUDY), 0, TIME_STUDY)) %>%
  mutate(Year= ifelse(TIME_STUDY==0, 0,
                      ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                             ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                    ifelse(TIME_STUDY>=2.5 & TIME_STUDY<3.5,3, 4))))) %>%
  ungroup()  %>% select(NUM, Year) %>% distinct() %>%
  filter(Year==0  | Year==1) %>% group_by(NUM) %>% count() %>% filter(n==2) %>%
  select(NUM) %>% distinct() # 208  

fwrite(EarlyCT_Pop_BaselineYear1_208, "Source/EarlyCT_Pop_BaselineYear1_208.txt")




EarlyCT_Pop_BaselineYear1Year2_134 <- dataCohorteManaged %>% 
  filter(!is.na(UMSARS4) & !is.na(UMSARS1_TOT) & !is.na(UMSARS2_TOT)) %>%
  select(NUM, TIME_STUDY)  %>% 
  inner_join(ClinicalTrialPats) %>%   # If early MSA Pop
  arrange(NUM, TIME_STUDY) %>% group_by(NUM) %>%
  mutate(TIME_STUDY = ifelse( is.na(TIME_STUDY), 0, TIME_STUDY)) %>%
  mutate(Year= ifelse(TIME_STUDY==0, 0,
                      ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                             ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                    ifelse(TIME_STUDY>=2.5 ,3, NA))))) %>%
  ungroup()  %>% select(NUM, Year) %>% distinct() %>%
  filter(Year==0  | Year==1 | Year==2) %>% group_by(NUM) %>% count() %>% filter(n==3) %>%
  select(NUM) %>% distinct() # 134  

fwrite(EarlyCT_Pop_BaselineYear1Year2_134, "Source/EarlyCT_Pop_BaselineYear1Year2_134.txt")



EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 <- dataCohorteManaged %>% 
  filter(!is.na(UMSARS4) & !is.na(UMSARS1_TOT) & !is.na(UMSARS2_TOT)) %>%
  select(NUM, TIME_STUDY)  %>% 
  inner_join(ClinicalTrialPats) %>%   # If early MSA Pop
  arrange(NUM, TIME_STUDY) %>% group_by(NUM) %>%
  mutate(TIME_STUDY = ifelse( is.na(TIME_STUDY), 0, TIME_STUDY)) %>%
  mutate(Year= ifelse(TIME_STUDY==0, 0,
                      ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                             ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                    ifelse(TIME_STUDY>=2.5 ,3, NA))))) %>%
  ungroup()  %>% select(NUM, Year) %>% distinct() %>%
  filter(Year==0  | Year==1 | Year==2  | Year==3) %>% group_by(NUM) %>% count() %>% filter(n==4) %>%
  select(NUM) %>% distinct() # 99  

fwrite(EarlyCT_Pop_BaselineYear1Year2Year3Plus_99, "Source/EarlyCT_Pop_BaselineYear1Year2Year3Plus_99.txt")

# ---------------------------------
