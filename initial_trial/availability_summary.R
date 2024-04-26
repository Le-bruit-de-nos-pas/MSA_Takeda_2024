
library(tidyverse)
library(data.table)
options(scipen = 999)

# Inputs ----------------------------

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
#  ------------------------------------
# How many with UMSARS 1 2 and 4 ? ------------------------


UMSARS1_2_4 <- dataCohorteManaged %>% filter(!is.na(UMSARS4) & !is.na(UMSARS1_TOT) & !is.na(UMSARS2_TOT)) %>%
  select(NUM, DATECONSULT, TIME_STUDY, Year, UMSARS1_TOT, UMSARS2_TOT, UMSARS4)

# --------------------------------

# All MSA --------------------------


AllMSA_Pop_Baseline_671 %>% inner_join(UMSARS1_2_4) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup()  # 671 (671 * 1)
 

AllMSA_Pop_BaselineYear1_410 %>% inner_join(UMSARS1_2_4) %>% filter(Year==0|Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup()  # 812 (410 * 2)


AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(UMSARS1_2_4) %>% filter(Year==0|Year==1|Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup()  # 735 (245 * 3)


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(UMSARS1_2_4) %>% filter(Year==0|Year==1|Year==2|Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup()  # 632 (158* 4)


# --------------------------------

# EarlyCT  -----------------------------


EarlyCT_Pop_Baseline_319 %>% inner_join(UMSARS1_2_4) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM) %>% slice(1) %>% ungroup() %>% # 319
  group_by(DIAGNIV) %>% count()

# 1 POS       105
# 2 PROB      214

EarlyCT_Pop_BaselineYear1_208 %>% inner_join(UMSARS1_2_4) %>% filter(Year==0|Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% # 416
  group_by(DIAGNIV) %>% count() %>% mutate(n=n/2)

# 1 POS        69
# 2 PROB      139


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(UMSARS1_2_4) %>% filter(Year==0|Year==1|Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% # 402
  group_by(DIAGNIV) %>% count() %>% mutate(n=n/3)

# 1 POS        44
# 2 PROB       90


EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(UMSARS1_2_4) %>% filter(Year==0|Year==1|Year==2|Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>% # 396
  group_by(DIAGNIV) %>% count() %>% mutate(n=n/4)

# 1 POS        35
# 2 PROB       64

# -------------------------------

# How many with COMPASS  ? ---------------------------

# COMPASS <- dataCohorteManaged %>% filter(!is.na(COMPASS_TOT)) %>%
#   select(NUM, DATECONSULT, TIME_STUDY, Year, COMPASS_TOT)
# 
# COMPASS
# 
# 
# AllMSA_Pop_Baseline_671 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() # 106
# 
# 
# AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==0|Year==1) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
#   group_by(NUM) %>% count() %>% filter(n==2) #  43
# 
# 
# AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==0|Year==1|Year==2) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup()  %>%
#   group_by(NUM) %>% count() %>% filter(n==3) #19
# 
# 
# AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==0|Year==1|Year==2|Year==3) %>%
#   mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
#   group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
#   group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
#   group_by(NUM, Year) %>% slice(1) %>% ungroup()  %>%
#   group_by(NUM) %>% count() %>% filter(n==4) # 6



COMPASS <- dataCohorteManaged %>% 
  select(NUM, DATECONSULT, TIME_STUDY, Year, COMPASS1:COMPASS_TOT)

COMPASS$COMPASS_TOT_v2 <- rowSums(COMPASS[, 5:42], na.rm = TRUE)

COMPASS$all_na <- rowSums(is.na(COMPASS[, 5:42])) == (42 - 5 + 1)

COMPASS <- COMPASS %>% filter(!(all_na) ) %>% select(NUM, DATECONSULT, TIME_STUDY, Year, COMPASS_TOT_v2)


# ----------------------

# All MSA -----------------------


AllMSA_Pop_Baseline_671 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() # 183



AllMSA_Pop_BaselineYear1_410 %>% inner_join(COMPASS) %>% filter(Year==0|Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup()  %>%
  group_by(NUM) %>% count() %>% filter(n==2) # 98



AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(COMPASS) %>% filter(Year==0|Year==1|Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup()  %>%
  group_by(NUM) %>% count() %>% filter(n==3) # 49


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(COMPASS) %>% filter(Year==0|Year==1|Year==2|Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup()  %>%
  group_by(NUM) %>% count() %>% filter(n==4) # 24


# --------------------------------

# EarlyCT  -----------------------------



EarlyCT_Pop_Baseline_319 %>% inner_join(COMPASS) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup()  %>%
  group_by(NUM) %>% count() %>% filter(n==1) %>% # 105
  left_join(EarlyCT_Pop_Baseline_319) %>%
  group_by(DIAGNIV) %>% count() 

# 1 POS        34
# 2 PROB       71


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(COMPASS) %>% filter(Year==0|Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup()  %>%
  group_by(NUM) %>% count() %>% filter(n==2) %>% # 54
  left_join(EarlyCT_Pop_BaselineYear1_208) %>%
  group_by(DIAGNIV) %>% count() 

# 1 POS        21
# 2 PROB       33



EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(COMPASS) %>% filter(Year==0|Year==1|Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup()  %>%
  group_by(NUM) %>% count() %>% filter(n==3) %>% # 28
  left_join(EarlyCT_Pop_BaselineYear1Year2_134) %>%
  group_by(DIAGNIV) %>% count() 

# 1 POS         9
# 2 PROB       19

EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(COMPASS) %>% filter(Year==0|Year==1|Year==2|Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup()  %>%
  group_by(NUM) %>% count() %>% filter(n==4) %>% # 15
  left_join(EarlyCT_Pop_BaselineYear1Year2Year3Plus_99) %>%
  group_by(DIAGNIV) %>% count() 

# 1 POS         6
# 2 PROB        9



# ----------------------------------------




















# UMSARS 1 

UMSARS1_TOT <- dataCohorteManaged %>% select(NUM, DATECONSULT, TIME_STUDY, Year, UMSARS1_TOT)

AllMSA_Pop_BaselineYear1_410 %>% inner_join(UMSARS1_TOT) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% filter(!is.na(UMSARS1_TOT)) %>%
  group_by(NUM) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(UMSARS1_TOT),
    sd=sd(UMSARS1_TOT),
    n=n()
  )


UMSARS2_TOT <- dataCohorteManaged %>% select(NUM, DATECONSULT, TIME_STUDY, Year, UMSARS2_TOT)

AllMSA_Pop_Baseline_671 %>% inner_join(UMSARS2_TOT) %>% filter(Year==0 & TIME_STUDY==0) %>%
  group_by(NUM) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(UMSARS2_TOT),
    sd=sd(UMSARS2_TOT),
    n=n()
  )


UMSARS4 <- dataCohorteManaged %>% select(NUM, DATECONSULT, TIME_STUDY, Year, UMSARS4)

AllMSA_Pop_Baseline_671 %>% inner_join(UMSARS4) %>% filter(Year==0 & TIME_STUDY==0) %>%
  group_by(NUM) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(UMSARS4),
    sd=sd(UMSARS4),
    n=n()
  )
# How many with SHRAG MSA QoL  ? ---------------------------


SCHRAG <- dataCohorteManaged %>% 
  select(NUM, DATECONSULT, TIME_STUDY, Year, SCHRAG_1:SCHRAG_40, SCHRAG_TOT)

SCHRAG$missing_na <- rowSums(is.na(SCHRAG[, 5:44]))

SCHRAG <- SCHRAG %>% filter(missing_na<=10)

SCHRAG$SCHRAG_TOT_v2 <- rowSums(SCHRAG[, 5:44], na.rm = TRUE)

SCHRAG <- SCHRAG %>% select(NUM, DATECONSULT, TIME_STUDY, Year, SCHRAG_TOT_v2)

# ----------------------

# All MSA -----------------------


AllMSA_Pop_Baseline_671 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() # 421



AllMSA_Pop_BaselineYear1_410 %>% inner_join(SCHRAG) %>% filter(Year==0|Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup()  %>%
  group_by(NUM) %>% count() %>% filter(n==2) # 258



AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(SCHRAG) %>% filter(Year==0|Year==1|Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup()  %>%
  group_by(NUM) %>% count() %>% filter(n==3) # 152


AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(SCHRAG) %>% filter(Year==0|Year==1|Year==2|Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup()  %>%
  group_by(NUM) %>% count() %>% filter(n==4) # 91


# --------------------------------

# EarlyCT  -----------------------------



EarlyCT_Pop_Baseline_319 %>% inner_join(SCHRAG) %>% filter(Year==0) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup()  %>%
  group_by(NUM) %>% count() %>% filter(n==1) %>% # 211
  left_join(EarlyCT_Pop_Baseline_319) %>%
  group_by(DIAGNIV) %>% count() 

# 1 POS        61
# 2 PROB      150


EarlyCT_Pop_BaselineYear1_208 %>% inner_join(SCHRAG) %>% filter(Year==0|Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup()  %>%
  group_by(NUM) %>% count() %>% filter(n==2) %>% # 133
  left_join(EarlyCT_Pop_BaselineYear1_208) %>%
  group_by(DIAGNIV) %>% count() 

# 1 POS        43
# 2 PROB       90


EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(SCHRAG) %>% filter(Year==0|Year==1|Year==2) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup()  %>%
  group_by(NUM) %>% count() %>% filter(n==3) %>% # 87
  left_join(EarlyCT_Pop_BaselineYear1Year2_134) %>%
  group_by(DIAGNIV) %>% count() 

# 1 POS        26
# 2 PROB       61

EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(SCHRAG) %>% filter(Year==0|Year==1|Year==2|Year==3) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup()  %>%
  group_by(NUM) %>% count() %>% filter(n==4) %>% # 59
  left_join(EarlyCT_Pop_BaselineYear1Year2Year3Plus_99) %>%
  group_by(DIAGNIV) %>% count() 

# 1 POS        19
# 2 PROB       40



# ----------------------------------------









# OTHER -----------------------------










# UMSARS 1 

UMSARS1_TOT <- dataCohorteManaged %>% select(NUM, DATECONSULT, TIME_STUDY, Year, UMSARS1_TOT)

AllMSA_Pop_BaselineYear1_410 %>% inner_join(UMSARS1_TOT) %>% filter(Year==1) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% filter(!is.na(UMSARS1_TOT)) %>%
  group_by(NUM) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(UMSARS1_TOT),
    sd=sd(UMSARS1_TOT),
    n=n()
  )


UMSARS2_TOT <- dataCohorteManaged %>% select(NUM, DATECONSULT, TIME_STUDY, Year, UMSARS2_TOT)

AllMSA_Pop_Baseline_671 %>% inner_join(UMSARS2_TOT) %>% filter(Year==0 & TIME_STUDY==0) %>%
  group_by(NUM) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(UMSARS2_TOT),
    sd=sd(UMSARS2_TOT),
    n=n()
  )


UMSARS4 <- dataCohorteManaged %>% select(NUM, DATECONSULT, TIME_STUDY, Year, UMSARS4)

AllMSA_Pop_Baseline_671 %>% inner_join(UMSARS4) %>% filter(Year==0 & TIME_STUDY==0) %>%
  group_by(NUM) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM) %>% slice(1) %>% ungroup() %>%
  summarise(
    mean=mean(UMSARS4),
    sd=sd(UMSARS4),
    n=n()
  )
