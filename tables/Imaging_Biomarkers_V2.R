library(readxl)
library(tidyverse)
library(data.table)
library(moments)
library(lubridate)
options(scipen = 999)


# VOLUMETRIC MRI ---------------------------------------------------------------------------------------

irmForPaulo <- readRDS("Source/irmForPaulo.rds")
names(irmForPaulo)

irmForPaulo <- irmForPaulo %>% select(NUM, DateIRM,
                       Left_Putamen, 
                       Right_Putamen, 
                       Left_Cerebellum_White_Matter,Right_Cerebellum_White_Matter,
                       Left_Cerebellum_Cortex, Right_Cerebellum_Cortex,
                       Midbrain,
                       Pons,
                       Medulla,
                       CerebralWhiteMatterVol, TotalGrayVol)

irmForPaulo <- irmForPaulo %>% arrange(NUM, DateIRM) %>% group_by(NUM) %>%
  mutate(TIME_STUDY=time_length(difftime(DateIRM, lag(DateIRM) ), "years"))  %>%
  select(NUM, DateIRM, TIME_STUDY,
         Left_Putamen, 
         Right_Putamen, 
         Left_Cerebellum_White_Matter,Right_Cerebellum_White_Matter,
         Left_Cerebellum_Cortex, Right_Cerebellum_Cortex,
         Midbrain,
         Pons,
         Medulla,
         CerebralWhiteMatterVol, TotalGrayVol)


irmForPaulo <- irmForPaulo %>% mutate(TIME_STUDY=ifelse(is.na(TIME_STUDY), 0, TIME_STUDY))

irmForPaulo <- irmForPaulo %>% mutate(TIME_STUDY=cumsum(TIME_STUDY))

irmForPaulo <- irmForPaulo %>% group_by(NUM) %>% count() %>%
  left_join(irmForPaulo) %>%
  select(NUM, DateIRM, TIME_STUDY, n,
         Left_Putamen, 
         Right_Putamen, 
         Left_Cerebellum_White_Matter,Right_Cerebellum_White_Matter,
         Left_Cerebellum_Cortex, Right_Cerebellum_Cortex,
         Midbrain,
         Pons,
         Medulla,
         CerebralWhiteMatterVol, TotalGrayVol)

irmForPaulo$Medulla <- as.numeric(irmForPaulo$Medulla)
irmForPaulo$Midbrain <- as.numeric(irmForPaulo$Midbrain)
irmForPaulo$CerebralWhiteMatterVol <- as.numeric(irmForPaulo$CerebralWhiteMatterVol)


length(unique(irmForPaulo$NUM)) # 127

irmForPaulo %>% ungroup() %>% group_by(n) %>% count()

irmForPaulo %>% mutate(Year= ifelse(TIME_STUDY<=0.5, 0,
                    ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                           ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                  ifelse(TIME_STUDY>=2.5 ,3, NA))))) %>%
  select(NUM, Year) %>% group_by(NUM) %>%
  mutate(All=paste0(Year, collapse=","))



range(irmForPaulo$TIME_STUDY)



# PLOTS **************************************

irmForPaulo %>% select(NUM, n) %>% distinct() %>%
  group_by(n) %>% count() %>%
  ggplot(aes(n, nn)) +
  geom_col(fill="black", colour="black") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  xlab("\n Number of MRIs per patient") +
  ylab("\n Number of Patients \n") +
  geom_text(aes(label = nn), vjust = -0.5)
  
 
irmForPaulo %>% 
  select(NUM, TIME_STUDY, Left_Putamen) %>% ungroup() %>%
  ggplot(aes(TIME_STUDY, Left_Putamen)) +
  geom_line(aes(group=NUM), col="black" , linewidth=0.5, alpha=0.4) +
  geom_jitter(size=1, colour="firebrick", alpha=0.5, shape=1, stroke=1) +
  theme_minimal() +
  xlab("\n Elapsed number of years since study enrollment \n [All available patient records up to year 5]") +
  ylab("Left_Putamen  Volume \n At each MRI \n") +
  scale_color_manual(values=palette) +
  scale_fill_manual(values=palette) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_x_continuous(breaks = seq(0, 5, by = 0.5)) +
  ylim(0,6000)


irmForPaulo %>% 
  select(NUM, TIME_STUDY, Right_Putamen) %>% ungroup() %>%
  ggplot(aes(TIME_STUDY, Right_Putamen)) +
  geom_line(aes(group=NUM), col="black" , linewidth=0.5, alpha=0.4) +
  geom_jitter(size=1, colour="firebrick", alpha=0.5, shape=1, stroke=1) +
  theme_minimal() +
  xlab("\n Elapsed number of years since study enrollment \n [All available patient records up to year 5]") +
  ylab("Right_Putamen  Volume \n At each MRI \n") +
  scale_color_manual(values=palette) +
  scale_fill_manual(values=palette) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_x_continuous(breaks = seq(0, 5, by = 0.5)) +
  ylim(0,6000)


irmForPaulo %>% 
  mutate(Cerebellum=Left_Cerebellum_White_Matter+Right_Cerebellum_White_Matter+Left_Cerebellum_Cortex +Right_Cerebellum_Cortex ) %>%
  select(NUM, TIME_STUDY, Cerebellum) %>% ungroup() %>%
  ggplot(aes(TIME_STUDY, Cerebellum)) +
  geom_line(aes(group=NUM), col="black" , linewidth=0.5, alpha=0.4) +
  geom_jitter(size=1, colour="firebrick", alpha=0.5, shape=1, stroke=1) +
  theme_minimal() +
  xlab("\n Elapsed number of years since study enrollment \n [All available patient records up to year 5]") +
  ylab("Cerebellum Total Volume \n At each MRI \n") +
  scale_color_manual(values=palette) +
  scale_fill_manual(values=palette) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_x_continuous(breaks = seq(0, 5, by = 0.5)) +
  ylim(0,165000)


irmForPaulo %>% 
  mutate(Cerebellum=Left_Cerebellum_White_Matter+Right_Cerebellum_White_Matter) %>%
  select(NUM, TIME_STUDY, Cerebellum) %>% ungroup() %>%
  ggplot(aes(TIME_STUDY, Cerebellum)) +
  geom_line(aes(group=NUM), col="black" , linewidth=0.5, alpha=0.4) +
  geom_jitter(size=1, colour="firebrick", alpha=0.5, shape=1, stroke=1) +
  theme_minimal() +
  xlab("\n Elapsed number of years since study enrollment \n [All available patient records up to year 5]") +
  ylab("Cerebellum White Volume \n At each MRI \n") +
  scale_color_manual(values=palette) +
  scale_fill_manual(values=palette) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_x_continuous(breaks = seq(0, 5, by = 0.5)) +
  ylim(0,40000)


irmForPaulo %>% 
  select(NUM, TIME_STUDY, Midbrain) %>% ungroup() %>%
  ggplot(aes(TIME_STUDY, Midbrain)) +
  geom_line(aes(group=NUM), col="black" , linewidth=0.5, alpha=0.4) +
  geom_jitter(size=1, colour="firebrick", alpha=0.5, shape=1, stroke=1) +
  theme_minimal() +
  xlab("\n Elapsed number of years since study enrollment \n [All available patient records up to year 5]") +
  ylab("Midbrain Volume \n At each MRI \n") +
  scale_color_manual(values=palette) +
  scale_fill_manual(values=palette) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_x_continuous(breaks = seq(0, 5, by = 0.5)) +
  ylim(0,8000)


irmForPaulo %>% 
  select(NUM, TIME_STUDY, Pons) %>% ungroup() %>%
  ggplot(aes(TIME_STUDY, Pons)) +
  geom_line(aes(group=NUM), col="black" , linewidth=0.5, alpha=0.4) +
  geom_jitter(size=1, colour="firebrick", alpha=0.5, shape=1, stroke=1) +
  theme_minimal() +
  xlab("\n Elapsed number of years since study enrollment \n [All available patient records up to year 5]") +
  ylab("Pons Volume \n At each MRI \n") +
  scale_color_manual(values=palette) +
  scale_fill_manual(values=palette) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_x_continuous(breaks = seq(0, 5, by = 0.5)) +
  ylim(0,20000)


irmForPaulo %>% 
  select(NUM, TIME_STUDY, Medulla) %>% ungroup() %>%
  ggplot(aes(TIME_STUDY, Medulla)) +
  geom_line(aes(group=NUM), col="black" , linewidth=0.5, alpha=0.4) +
  geom_jitter(size=1, colour="firebrick", alpha=0.5, shape=1, stroke=1) +
  theme_minimal() +
  xlab("\n Elapsed number of years since study enrollment \n [All available patient records up to year 5]") +
  ylab("Medulla Volume \n At each MRI \n") +
  scale_color_manual(values=palette) +
  scale_fill_manual(values=palette) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_x_continuous(breaks = seq(0, 5, by = 0.5)) +
  ylim(0,7000)

irmForPaulo %>% 
  mutate(Total=as.numeric(CerebralWhiteMatterVol)+as.numeric(TotalGrayVol)) %>%
  select(NUM, TIME_STUDY, Total) %>% ungroup() %>%
  ggplot(aes(TIME_STUDY, Total)) +
  geom_line(aes(group=NUM), col="black" , linewidth=0.5, alpha=0.4) +
  geom_jitter(size=1, colour="firebrick", alpha=0.5, shape=1, stroke=1) +
  theme_minimal() +
  xlab("\n Elapsed number of years since study enrollment \n [All available patient records up to year 5]") +
  ylab("Whole Brain Volume \n At each MRI \n") +
  scale_color_manual(values=palette) +
  scale_fill_manual(values=palette) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_x_continuous(breaks = seq(0, 5, by = 0.5)) +
  ylim(0,1400000)

irmForPaulo <- irmForPaulo %>% mutate(TotalBrain=as.numeric(CerebralWhiteMatterVol)+as.numeric(TotalGrayVol))

irmForPaulo <- irmForPaulo %>% mutate(Cerebellum=as.numeric(Left_Cerebellum_White_Matter)+as.numeric(Right_Cerebellum_White_Matter)) 

# END of PLOTS ****************************


# ORIGINAL VOLUMETRIC ************************


# Left_Putamen *************************************

irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, TIME_STUDY, Left_Putamen) %>%
  summarise(
    mean=mean(Left_Putamen),
    sd=sd(Left_Putamen),
    median=median(Left_Putamen),
    Q1 = quantile(Left_Putamen, 0.25),
    Q3 = quantile(Left_Putamen, 0.75),
    min=min(Left_Putamen),
    max=max(Left_Putamen),
    n=n()
  )



irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>% 
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Left_Putamen, Elapsed) %>%
  summarise(
    mean=mean(TIME_STUDY*12),
    sd=sd(TIME_STUDY*12),
    median=median(TIME_STUDY*12),
    Q1 = quantile(TIME_STUDY*12, 0.25),
    Q3 = quantile(TIME_STUDY*12, 0.75),
    min=min(TIME_STUDY*12),
    max=max(TIME_STUDY*12),
    n=n()
  )


irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>% 
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Left_Putamen) %>%
  summarise(
    mean=mean(Left_Putamen),
    sd=sd(Left_Putamen),
    median=median(Left_Putamen),
    Q1 = quantile(Left_Putamen, 0.25),
    Q3 = quantile(Left_Putamen, 0.75),
    min=min(Left_Putamen),
    max=max(Left_Putamen),
    n=n()
  )




irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, Left_Putamen) %>% rename("Year0"="Left_Putamen") %>%
  inner_join(
    irmForPaulo %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, Left_Putamen)
  ) %>% mutate(Delta=Left_Putamen-Year0) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta),
    sd=sd(Delta),
    median=median(Delta),
    Q1 = quantile(Delta, 0.25),
    Q3 = quantile(Delta, 0.75),
    min=min(Delta),
    max=max(Delta),
    n=n()
  )



irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, Left_Putamen) %>% rename("Year0"="Left_Putamen") %>%
  inner_join(
    irmForPaulo %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, Left_Putamen)
  ) %>% mutate(Delta=100*(Left_Putamen-Year0)/Year0) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta),
    sd=sd(Delta),
    median=median(Delta),
    Q1 = quantile(Delta, 0.25),
    Q3 = quantile(Delta, 0.75),
    min=min(Delta),
    max=max(Delta),
    n=n()
  )




irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, Left_Putamen) %>% rename("Year0"="Left_Putamen") %>%
  inner_join(
    irmForPaulo %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, Left_Putamen)
  ) %>% mutate(Delta=Left_Putamen-Year0) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta/(TIME_STUDY)),
    sd=sd(Delta/(TIME_STUDY)),
    median=median(Delta/(TIME_STUDY)),
    Q1 = quantile(Delta/(TIME_STUDY), 0.25),
    Q3 = quantile(Delta/(TIME_STUDY), 0.75),
    min=min(Delta/(TIME_STUDY)),
    max=max(Delta/(TIME_STUDY)),
    n=n()
  )




irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, Left_Putamen) %>% rename("Year0"="Left_Putamen") %>%
  inner_join(
    irmForPaulo %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, Left_Putamen)
  ) %>% mutate(Delta=100*(Left_Putamen-Year0)/Year0) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta/(TIME_STUDY)),
    sd=sd(Delta/(TIME_STUDY)),
    median=median(Delta/(TIME_STUDY)),
    Q1 = quantile(Delta/(TIME_STUDY), 0.25),
    Q3 = quantile(Delta/(TIME_STUDY), 0.75),
    min=min(Delta/(TIME_STUDY)),
    max=max(Delta/(TIME_STUDY)),
    n=n()
  )







# Right_Putamen *************************************************

irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, TIME_STUDY, Right_Putamen) %>%
  summarise(
    mean=mean(Right_Putamen),
    sd=sd(Right_Putamen),
    median=median(Right_Putamen),
    Q1 = quantile(Right_Putamen, 0.25),
    Q3 = quantile(Right_Putamen, 0.75),
    min=min(Right_Putamen),
    max=max(Right_Putamen),
    n=n()
  )



irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>% 
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Right_Putamen, Elapsed) %>%
  summarise(
    mean=mean(TIME_STUDY*12),
    sd=sd(TIME_STUDY*12),
    median=median(TIME_STUDY*12),
    Q1 = quantile(TIME_STUDY*12, 0.25),
    Q3 = quantile(TIME_STUDY*12, 0.75),
    min=min(TIME_STUDY*12),
    max=max(TIME_STUDY*12),
    n=n()
  )


irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>% 
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Right_Putamen) %>%
  summarise(
    mean=mean(Right_Putamen),
    sd=sd(Right_Putamen),
    median=median(Right_Putamen),
    Q1 = quantile(Right_Putamen, 0.25),
    Q3 = quantile(Right_Putamen, 0.75),
    min=min(Right_Putamen),
    max=max(Right_Putamen),
    n=n()
  )




irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, Right_Putamen) %>% rename("Year0"="Right_Putamen") %>%
  inner_join(
    irmForPaulo %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, Right_Putamen)
  ) %>% mutate(Delta=Right_Putamen-Year0) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta),
    sd=sd(Delta),
    median=median(Delta),
    Q1 = quantile(Delta, 0.25),
    Q3 = quantile(Delta, 0.75),
    min=min(Delta),
    max=max(Delta),
    n=n()
  )



irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, Right_Putamen) %>% rename("Year0"="Right_Putamen") %>%
  inner_join(
    irmForPaulo %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, Right_Putamen)
  ) %>% mutate(Delta=100*(Right_Putamen-Year0)/Year0) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta),
    sd=sd(Delta),
    median=median(Delta),
    Q1 = quantile(Delta, 0.25),
    Q3 = quantile(Delta, 0.75),
    min=min(Delta),
    max=max(Delta),
    n=n()
  )



irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, Right_Putamen) %>% rename("Year0"="Right_Putamen") %>%
  inner_join(
    irmForPaulo %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, Right_Putamen)
  ) %>% mutate(Delta=Right_Putamen-Year0) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta/(TIME_STUDY)),
    sd=sd(Delta/(TIME_STUDY)),
    median=median(Delta/(TIME_STUDY)),
    Q1 = quantile(Delta/(TIME_STUDY), 0.25),
    Q3 = quantile(Delta/(TIME_STUDY), 0.75),
    min=min(Delta/(TIME_STUDY)),
    max=max(Delta/(TIME_STUDY)),
    n=n()
  )




irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, Right_Putamen) %>% rename("Year0"="Right_Putamen") %>%
  inner_join(
    irmForPaulo %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, Right_Putamen)
  ) %>% mutate(Delta=100*(Right_Putamen-Year0)/Year0) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta/(TIME_STUDY)),
    sd=sd(Delta/(TIME_STUDY)),
    median=median(Delta/(TIME_STUDY)),
    Q1 = quantile(Delta/(TIME_STUDY), 0.25),
    Q3 = quantile(Delta/(TIME_STUDY), 0.75),
    min=min(Delta/(TIME_STUDY)),
    max=max(Delta/(TIME_STUDY)),
    n=n()
  )




# Cerebellum White *************************************************

irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, TIME_STUDY, Cerebellum) %>%
  summarise(
    mean=mean(Cerebellum),
    sd=sd(Cerebellum),
    median=median(Cerebellum),
    Q1 = quantile(Cerebellum, 0.25),
    Q3 = quantile(Cerebellum, 0.75),
    min=min(Cerebellum),
    max=max(Cerebellum),
    n=n()
  )



irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>% 
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Cerebellum, Elapsed) %>%
  summarise(
    mean=mean(TIME_STUDY*12),
    sd=sd(TIME_STUDY*12),
    median=median(TIME_STUDY*12),
    Q1 = quantile(TIME_STUDY*12, 0.25),
    Q3 = quantile(TIME_STUDY*12, 0.75),
    min=min(TIME_STUDY*12),
    max=max(TIME_STUDY*12),
    n=n()
  )


irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>% 
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Cerebellum) %>%
  summarise(
    mean=mean(Cerebellum),
    sd=sd(Cerebellum),
    median=median(Cerebellum),
    Q1 = quantile(Cerebellum, 0.25),
    Q3 = quantile(Cerebellum, 0.75),
    min=min(Cerebellum),
    max=max(Cerebellum),
    n=n()
  )




irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, Cerebellum) %>% rename("Year0"="Cerebellum") %>%
  inner_join(
    irmForPaulo %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, Cerebellum)
  ) %>% mutate(Delta=Cerebellum-Year0) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta),
    sd=sd(Delta),
    median=median(Delta),
    Q1 = quantile(Delta, 0.25),
    Q3 = quantile(Delta, 0.75),
    min=min(Delta),
    max=max(Delta),
    n=n()
  )



irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, Cerebellum) %>% rename("Year0"="Cerebellum") %>%
  inner_join(
    irmForPaulo %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, Cerebellum)
  ) %>% mutate(Delta=100*(Cerebellum-Year0)/Year0) %>%  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta),
    sd=sd(Delta),
    median=median(Delta),
    Q1 = quantile(Delta, 0.25),
    Q3 = quantile(Delta, 0.75),
    min=min(Delta),
    max=max(Delta),
    n=n()
  )




irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, Cerebellum) %>% rename("Year0"="Cerebellum") %>%
  inner_join(
    irmForPaulo %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, Cerebellum)
  ) %>% mutate(Delta=Cerebellum-Year0) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta/(TIME_STUDY)),
    sd=sd(Delta/(TIME_STUDY)),
    median=median(Delta/(TIME_STUDY)),
    Q1 = quantile(Delta/(TIME_STUDY), 0.25),
    Q3 = quantile(Delta/(TIME_STUDY), 0.75),
    min=min(Delta/(TIME_STUDY)),
    max=max(Delta/(TIME_STUDY)),
    n=n()
  )




irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, Cerebellum) %>% rename("Year0"="Cerebellum") %>%
  inner_join(
    irmForPaulo %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, Cerebellum)
  ) %>% mutate(Delta=100*(Cerebellum-Year0)/Year0) %>%  ungroup() %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta/(TIME_STUDY)),
    sd=sd(Delta/(TIME_STUDY)),
    median=median(Delta/(TIME_STUDY)),
    Q1 = quantile(Delta/(TIME_STUDY), 0.25),
    Q3 = quantile(Delta/(TIME_STUDY), 0.75),
    min=min(Delta/(TIME_STUDY)),
    max=max(Delta/(TIME_STUDY)),
    n=n()
  )




# Cerebellum Total *************************************************

irmForPaulo <- irmForPaulo %>% mutate(Cerebellum=Left_Cerebellum_White_Matter+Right_Cerebellum_White_Matter+Left_Cerebellum_Cortex+Right_Cerebellum_Cortex   )

irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, TIME_STUDY, Cerebellum) %>%
  summarise(
    mean=mean(Cerebellum),
    sd=sd(Cerebellum),
    median=median(Cerebellum),
    Q1 = quantile(Cerebellum, 0.25),
    Q3 = quantile(Cerebellum, 0.75),
    min=min(Cerebellum),
    max=max(Cerebellum),
    n=n()
  )



irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>% 
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Cerebellum, Elapsed) %>%
  summarise(
    mean=mean(TIME_STUDY*12),
    sd=sd(TIME_STUDY*12),
    median=median(TIME_STUDY*12),
    Q1 = quantile(TIME_STUDY*12, 0.25),
    Q3 = quantile(TIME_STUDY*12, 0.75),
    min=min(TIME_STUDY*12),
    max=max(TIME_STUDY*12),
    n=n()
  )


irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>% 
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Cerebellum) %>%
  summarise(
    mean=mean(Cerebellum),
    sd=sd(Cerebellum),
    median=median(Cerebellum),
    Q1 = quantile(Cerebellum, 0.25),
    Q3 = quantile(Cerebellum, 0.75),
    min=min(Cerebellum),
    max=max(Cerebellum),
    n=n()
  )




irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, Cerebellum) %>% rename("Year0"="Cerebellum") %>%
  inner_join(
    irmForPaulo %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, Cerebellum)
  ) %>% mutate(Delta=Cerebellum-Year0) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta),
    sd=sd(Delta),
    median=median(Delta),
    Q1 = quantile(Delta, 0.25),
    Q3 = quantile(Delta, 0.75),
    min=min(Delta),
    max=max(Delta),
    n=n()
  )



irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, Cerebellum) %>% rename("Year0"="Cerebellum") %>%
  inner_join(
    irmForPaulo %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, Cerebellum)
  ) %>% mutate(Delta=100*(Cerebellum-Year0)/Year0) %>%  ungroup() %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta),
    sd=sd(Delta),
    median=median(Delta),
    Q1 = quantile(Delta, 0.25),
    Q3 = quantile(Delta, 0.75),
    min=min(Delta),
    max=max(Delta),
    n=n()
  )


irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, Cerebellum) %>% rename("Year0"="Cerebellum") %>%
  inner_join(
    irmForPaulo %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, Cerebellum)
  ) %>% mutate(Delta=Cerebellum-Year0) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta/(TIME_STUDY)),
    sd=sd(Delta/(TIME_STUDY)),
    median=median(Delta/(TIME_STUDY)),
    Q1 = quantile(Delta/(TIME_STUDY), 0.25),
    Q3 = quantile(Delta/(TIME_STUDY), 0.75),
    min=min(Delta/(TIME_STUDY)),
    max=max(Delta/(TIME_STUDY)),
    n=n()
  )



irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, Cerebellum) %>% rename("Year0"="Cerebellum") %>%
  inner_join(
    irmForPaulo %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, Cerebellum)
  ) %>% mutate(Delta=100*(Cerebellum-Year0)/Year0) %>%  ungroup() %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta/(TIME_STUDY)),
    sd=sd(Delta/(TIME_STUDY)),
    median=median(Delta/(TIME_STUDY)),
    Q1 = quantile(Delta/(TIME_STUDY), 0.25),
    Q3 = quantile(Delta/(TIME_STUDY), 0.75),
    min=min(Delta/(TIME_STUDY)),
    max=max(Delta/(TIME_STUDY)),
    n=n()
  )


# Midbrain  *************************************************

irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, TIME_STUDY, Midbrain) %>%
  summarise(
    mean=mean(Midbrain),
    sd=sd(Midbrain),
    median=median(Midbrain),
    Q1 = quantile(Midbrain, 0.25),
    Q3 = quantile(Midbrain, 0.75),
    min=min(Midbrain),
    max=max(Midbrain),
    n=n()
  )



irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>% 
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Midbrain, Elapsed) %>%
  summarise(
    mean=mean(TIME_STUDY*12),
    sd=sd(TIME_STUDY*12),
    median=median(TIME_STUDY*12),
    Q1 = quantile(TIME_STUDY*12, 0.25),
    Q3 = quantile(TIME_STUDY*12, 0.75),
    min=min(TIME_STUDY*12),
    max=max(TIME_STUDY*12),
    n=n()
  )


irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>% 
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Midbrain) %>%
  summarise(
    mean=mean(Midbrain),
    sd=sd(Midbrain),
    median=median(Midbrain),
    Q1 = quantile(Midbrain, 0.25),
    Q3 = quantile(Midbrain, 0.75),
    min=min(Midbrain),
    max=max(Midbrain),
    n=n()
  )




irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, Midbrain) %>% rename("Year0"="Midbrain") %>%
  inner_join(
    irmForPaulo %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, Midbrain)
  ) %>% mutate(Delta=Midbrain-Year0) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta),
    sd=sd(Delta),
    median=median(Delta),
    Q1 = quantile(Delta, 0.25),
    Q3 = quantile(Delta, 0.75),
    min=min(Delta),
    max=max(Delta),
    n=n()
  )


irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, Midbrain) %>% rename("Year0"="Midbrain") %>%
  inner_join(
    irmForPaulo %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, Midbrain)
  ) %>% mutate(Delta=100*(Midbrain-Year0)/Year0) %>%  ungroup() %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta),
    sd=sd(Delta),
    median=median(Delta),
    Q1 = quantile(Delta, 0.25),
    Q3 = quantile(Delta, 0.75),
    min=min(Delta),
    max=max(Delta),
    n=n()
  )



irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, Midbrain) %>% rename("Year0"="Midbrain") %>%
  inner_join(
    irmForPaulo %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, Midbrain)
  ) %>% mutate(Delta=Midbrain-Year0) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta/(TIME_STUDY)),
    sd=sd(Delta/(TIME_STUDY)),
    median=median(Delta/(TIME_STUDY)),
    Q1 = quantile(Delta/(TIME_STUDY), 0.25),
    Q3 = quantile(Delta/(TIME_STUDY), 0.75),
    min=min(Delta/(TIME_STUDY)),
    max=max(Delta/(TIME_STUDY)),
    n=n()
  )




irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, Midbrain) %>% rename("Year0"="Midbrain") %>%
  inner_join(
    irmForPaulo %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, Midbrain)
  ) %>% mutate(Delta=100*(Midbrain-Year0)/Year0) %>%  ungroup() %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta/(TIME_STUDY)),
    sd=sd(Delta/(TIME_STUDY)),
    median=median(Delta/(TIME_STUDY)),
    Q1 = quantile(Delta/(TIME_STUDY), 0.25),
    Q3 = quantile(Delta/(TIME_STUDY), 0.75),
    min=min(Delta/(TIME_STUDY)),
    max=max(Delta/(TIME_STUDY)),
    n=n()
  )




# Pons  *************************************************

irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, TIME_STUDY, Pons) %>%
  summarise(
    mean=mean(Pons),
    sd=sd(Pons),
    median=median(Pons),
    Q1 = quantile(Pons, 0.25),
    Q3 = quantile(Pons, 0.75),
    min=min(Pons),
    max=max(Pons),
    n=n()
  )



irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>% 
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Pons, Elapsed) %>%
  summarise(
    mean=mean(TIME_STUDY*12),
    sd=sd(TIME_STUDY*12),
    median=median(TIME_STUDY*12),
    Q1 = quantile(TIME_STUDY*12, 0.25),
    Q3 = quantile(TIME_STUDY*12, 0.75),
    min=min(TIME_STUDY*12),
    max=max(TIME_STUDY*12),
    n=n()
  )


irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>% 
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Pons) %>%
  summarise(
    mean=mean(Pons),
    sd=sd(Pons),
    median=median(Pons),
    Q1 = quantile(Pons, 0.25),
    Q3 = quantile(Pons, 0.75),
    min=min(Pons),
    max=max(Pons),
    n=n()
  )




irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, Pons) %>% rename("Year0"="Pons") %>%
  inner_join(
    irmForPaulo %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, Pons)
  ) %>% mutate(Delta=Pons-Year0) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta),
    sd=sd(Delta),
    median=median(Delta),
    Q1 = quantile(Delta, 0.25),
    Q3 = quantile(Delta, 0.75),
    min=min(Delta),
    max=max(Delta),
    n=n()
  )


irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, Pons) %>% rename("Year0"="Pons") %>%
  inner_join(
    irmForPaulo %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, Pons)
  ) %>% mutate(Delta=100*(Pons-Year0)/Year0) %>%  ungroup() %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta),
    sd=sd(Delta),
    median=median(Delta),
    Q1 = quantile(Delta, 0.25),
    Q3 = quantile(Delta, 0.75),
    min=min(Delta),
    max=max(Delta),
    n=n()
  )



irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, Pons) %>% rename("Year0"="Pons") %>%
  inner_join(
    irmForPaulo %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, Pons)
  ) %>% mutate(Delta=Pons-Year0) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta/(TIME_STUDY)),
    sd=sd(Delta/(TIME_STUDY)),
    median=median(Delta/(TIME_STUDY)),
    Q1 = quantile(Delta/(TIME_STUDY), 0.25),
    Q3 = quantile(Delta/(TIME_STUDY), 0.75),
    min=min(Delta/(TIME_STUDY)),
    max=max(Delta/(TIME_STUDY)),
    n=n()
  )



irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, Pons) %>% rename("Year0"="Pons") %>%
  inner_join(
    irmForPaulo %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, Pons)
  ) %>% mutate(Delta=100*(Pons-Year0)/Year0) %>%  ungroup() %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta/(TIME_STUDY)),
    sd=sd(Delta/(TIME_STUDY)),
    median=median(Delta/(TIME_STUDY)),
    Q1 = quantile(Delta/(TIME_STUDY), 0.25),
    Q3 = quantile(Delta/(TIME_STUDY), 0.75),
    min=min(Delta/(TIME_STUDY)),
    max=max(Delta/(TIME_STUDY)),
    n=n()
  )







# Medulla  *************************************************

irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, TIME_STUDY, Medulla) %>%
  summarise(
    mean=mean(Medulla),
    sd=sd(Medulla),
    median=median(Medulla),
    Q1 = quantile(Medulla, 0.25),
    Q3 = quantile(Medulla, 0.75),
    min=min(Medulla),
    max=max(Medulla),
    n=n()
  )



irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>% 
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Medulla, Elapsed) %>%
  summarise(
    mean=mean(TIME_STUDY*12),
    sd=sd(TIME_STUDY*12),
    median=median(TIME_STUDY*12),
    Q1 = quantile(TIME_STUDY*12, 0.25),
    Q3 = quantile(TIME_STUDY*12, 0.75),
    min=min(TIME_STUDY*12),
    max=max(TIME_STUDY*12),
    n=n()
  )


irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>% 
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Medulla) %>%
  summarise(
    mean=mean(Medulla),
    sd=sd(Medulla),
    median=median(Medulla),
    Q1 = quantile(Medulla, 0.25),
    Q3 = quantile(Medulla, 0.75),
    min=min(Medulla),
    max=max(Medulla),
    n=n()
  )




irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, Medulla) %>% rename("Year0"="Medulla") %>%
  inner_join(
    irmForPaulo %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, Medulla)
  ) %>% mutate(Delta=Medulla-Year0) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta),
    sd=sd(Delta),
    median=median(Delta),
    Q1 = quantile(Delta, 0.25),
    Q3 = quantile(Delta, 0.75),
    min=min(Delta),
    max=max(Delta),
    n=n()
  )



irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, Medulla) %>% rename("Year0"="Medulla") %>%
  inner_join(
    irmForPaulo %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, Medulla)
  ) %>% mutate(Delta=100*(Medulla-Year0)/Year0) %>%  ungroup() %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta),
    sd=sd(Delta),
    median=median(Delta),
    Q1 = quantile(Delta, 0.25),
    Q3 = quantile(Delta, 0.75),
    min=min(Delta),
    max=max(Delta),
    n=n()
  )


irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, Medulla) %>% rename("Year0"="Medulla") %>%
  inner_join(
    irmForPaulo %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, Medulla)
  ) %>% mutate(Delta=Medulla-Year0) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta/(TIME_STUDY)),
    sd=sd(Delta/(TIME_STUDY)),
    median=median(Delta/(TIME_STUDY)),
    Q1 = quantile(Delta/(TIME_STUDY), 0.25),
    Q3 = quantile(Delta/(TIME_STUDY), 0.75),
    min=min(Delta/(TIME_STUDY)),
    max=max(Delta/(TIME_STUDY)),
    n=n()
  )





irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, Medulla) %>% rename("Year0"="Medulla") %>%
  inner_join(
    irmForPaulo %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, Medulla)
  ) %>% mutate(Delta=100*(Medulla-Year0)/Year0) %>%  ungroup() %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta/(TIME_STUDY)),
    sd=sd(Delta/(TIME_STUDY)),
    median=median(Delta/(TIME_STUDY)),
    Q1 = quantile(Delta/(TIME_STUDY), 0.25),
    Q3 = quantile(Delta/(TIME_STUDY), 0.75),
    min=min(Delta/(TIME_STUDY)),
    max=max(Delta/(TIME_STUDY)),
    n=n()
  )






# TotalBrain   *************************************************

irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, TIME_STUDY, TotalBrain ) %>%
  summarise(
    mean=mean(TotalBrain ),
    sd=sd(TotalBrain ),
    median=median(TotalBrain ),
    Q1 = quantile(TotalBrain , 0.25),
    Q3 = quantile(TotalBrain , 0.75),
    min=min(TotalBrain ),
    max=max(TotalBrain ),
    n=n()
  )



irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>% 
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, TotalBrain , Elapsed) %>%
  summarise(
    mean=mean(TIME_STUDY*12),
    sd=sd(TIME_STUDY*12),
    median=median(TIME_STUDY*12),
    Q1 = quantile(TIME_STUDY*12, 0.25),
    Q3 = quantile(TIME_STUDY*12, 0.75),
    min=min(TIME_STUDY*12),
    max=max(TIME_STUDY*12),
    n=n()
  )


irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>% 
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, TotalBrain ) %>%
  summarise(
    mean=mean(TotalBrain ),
    sd=sd(TotalBrain ),
    median=median(TotalBrain ),
    Q1 = quantile(TotalBrain , 0.25),
    Q3 = quantile(TotalBrain , 0.75),
    min=min(TotalBrain ),
    max=max(TotalBrain ),
    n=n()
  )




irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, TotalBrain ) %>% rename("Year0"="TotalBrain") %>%
  inner_join(
    irmForPaulo %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, TotalBrain )
  ) %>% mutate(Delta=TotalBrain -Year0) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta),
    sd=sd(Delta),
    median=median(Delta),
    Q1 = quantile(Delta, 0.25),
    Q3 = quantile(Delta, 0.75),
    min=min(Delta),
    max=max(Delta),
    n=n()
  )


irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, TotalBrain ) %>% rename("Year0"="TotalBrain") %>%
  inner_join(
    irmForPaulo %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, TotalBrain )
  ) %>% mutate(Delta=100*(TotalBrain-Year0)/Year0) %>%  ungroup() %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta),
    sd=sd(Delta),
    median=median(Delta),
    Q1 = quantile(Delta, 0.25),
    Q3 = quantile(Delta, 0.75),
    min=min(Delta),
    max=max(Delta),
    n=n()
  )




irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, TotalBrain ) %>% rename("Year0"="TotalBrain") %>%
  inner_join(
    irmForPaulo %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, TotalBrain )
  ) %>% mutate(Delta=TotalBrain -Year0) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta/(TIME_STUDY)),
    sd=sd(Delta/(TIME_STUDY)),
    median=median(Delta/(TIME_STUDY)),
    Q1 = quantile(Delta/(TIME_STUDY), 0.25),
    Q3 = quantile(Delta/(TIME_STUDY), 0.75),
    min=min(Delta/(TIME_STUDY)),
    max=max(Delta/(TIME_STUDY)),
    n=n()
  )


irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, TotalBrain ) %>% rename("Year0"="TotalBrain") %>%
  inner_join(
    irmForPaulo %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, TotalBrain )
  ) %>% mutate(Delta=100*(TotalBrain-Year0)/Year0) %>%  ungroup() %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta/(TIME_STUDY)),
    sd=sd(Delta/(TIME_STUDY)),
    median=median(Delta/(TIME_STUDY)),
    Q1 = quantile(Delta/(TIME_STUDY), 0.25),
    Q3 = quantile(Delta/(TIME_STUDY), 0.75),
    min=min(Delta/(TIME_STUDY)),
    max=max(Delta/(TIME_STUDY)),
    n=n()
  )
# -----------------
# NORMALIZED VOLUMETRIC MRI --------------------------

TotalCranial <- readRDS("Source/irmForPaulo.rds")


irmForPaulo <- irmForPaulo %>%
  mutate(CerebellumWhite=Left_Cerebellum_White_Matter +Right_Cerebellum_White_Matter ) %>%
  mutate(CerebellumTotal=Left_Cerebellum_White_Matter +Right_Cerebellum_White_Matter+Left_Cerebellum_Cortex+Right_Cerebellum_Cortex   ) %>%
  left_join(TotalCranial %>% select(NUM, DateIRM, EstimatedTotalIntraCranialVol)) %>%
  mutate(Left_Putamen=Left_Putamen/EstimatedTotalIntraCranialVol) %>%
  mutate(Right_Putamen =Right_Putamen /EstimatedTotalIntraCranialVol) %>%
  mutate(CerebellumWhite =CerebellumWhite /EstimatedTotalIntraCranialVol) %>%
  mutate(CerebellumTotal =CerebellumTotal /EstimatedTotalIntraCranialVol) %>%
  mutate(Midbrain=Midbrain/EstimatedTotalIntraCranialVol) %>%
  mutate(Pons =Pons /EstimatedTotalIntraCranialVol) %>%
  mutate(Medulla =Medulla /EstimatedTotalIntraCranialVol) %>%
  mutate(TotalBrain =TotalBrain /EstimatedTotalIntraCranialVol) 






irmForPaulo %>% 
  select(NUM, TIME_STUDY, Left_Putamen) %>% ungroup() %>%
  ggplot(aes(TIME_STUDY, Left_Putamen)) +
  geom_line(aes(group=NUM), col="black" , linewidth=0.5, alpha=0.4) +
  geom_jitter(size=1, colour="firebrick", alpha=0.5, shape=1, stroke=1) +
  theme_minimal() +
  xlab("\n Elapsed number of years since study enrollment \n [All available patient records up to year 5]") +
  ylab("Left_Putamen  Volume \n At each MRI \n") +
  scale_color_manual(values=palette) +
  scale_fill_manual(values=palette) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_x_continuous(breaks = seq(0, 5, by = 0.5)) +
  ylim(0,0.005)


irmForPaulo %>% 
  select(NUM, TIME_STUDY, Right_Putamen) %>% ungroup() %>%
  ggplot(aes(TIME_STUDY, Right_Putamen)) +
  geom_line(aes(group=NUM), col="black" , linewidth=0.5, alpha=0.4) +
  geom_jitter(size=1, colour="firebrick", alpha=0.5, shape=1, stroke=1) +
  theme_minimal() +
  xlab("\n Elapsed number of years since study enrollment \n [All available patient records up to year 5]") +
  ylab("Right_Putamen  Volume \n At each MRI \n") +
  scale_color_manual(values=palette) +
  scale_fill_manual(values=palette) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_x_continuous(breaks = seq(0, 5, by = 0.5)) +
  ylim(0,0.005)


irmForPaulo %>% 
  select(NUM, TIME_STUDY, CerebellumTotal) %>% ungroup() %>%
  ggplot(aes(TIME_STUDY, CerebellumTotal)) +
  geom_line(aes(group=NUM), col="black" , linewidth=0.5, alpha=0.4) +
  geom_jitter(size=1, colour="firebrick", alpha=0.5, shape=1, stroke=1) +
  theme_minimal() +
  xlab("\n Elapsed number of years since study enrollment \n [All available patient records up to year 5]") +
  ylab("Cerebellum Total Volume \n At each MRI \n") +
  scale_color_manual(values=palette) +
  scale_fill_manual(values=palette) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_x_continuous(breaks = seq(0, 5, by = 0.5)) +
  ylim(0,0.13)


irmForPaulo %>% 
  select(NUM, TIME_STUDY, CerebellumWhite) %>% ungroup() %>%
  ggplot(aes(TIME_STUDY, CerebellumWhite)) +
  geom_line(aes(group=NUM), col="black" , linewidth=0.5, alpha=0.4) +
  geom_jitter(size=1, colour="firebrick", alpha=0.5, shape=1, stroke=1) +
  theme_minimal() +
  xlab("\n Elapsed number of years since study enrollment \n [All available patient records up to year 5]") +
  ylab("Cerebellum White Volume \n At each MRI \n") +
  scale_color_manual(values=palette) +
  scale_fill_manual(values=palette) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_x_continuous(breaks = seq(0, 5, by = 0.5)) +
  ylim(0,0.035)


irmForPaulo %>% 
  select(NUM, TIME_STUDY, Midbrain) %>% ungroup() %>%
  ggplot(aes(TIME_STUDY, Midbrain)) +
  geom_line(aes(group=NUM), col="black" , linewidth=0.5, alpha=0.4) +
  geom_jitter(size=1, colour="firebrick", alpha=0.5, shape=1, stroke=1) +
  theme_minimal() +
  xlab("\n Elapsed number of years since study enrollment \n [All available patient records up to year 5]") +
  ylab("Midbrain Volume \n At each MRI \n") +
  scale_color_manual(values=palette) +
  scale_fill_manual(values=palette) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_x_continuous(breaks = seq(0, 5, by = 0.5)) +
  ylim(0,0.0065)


irmForPaulo %>% 
  select(NUM, TIME_STUDY, Pons) %>% ungroup() %>%
  ggplot(aes(TIME_STUDY, Pons)) +
  geom_line(aes(group=NUM), col="black" , linewidth=0.5, alpha=0.4) +
  geom_jitter(size=1, colour="firebrick", alpha=0.5, shape=1, stroke=1) +
  theme_minimal() +
  xlab("\n Elapsed number of years since study enrollment \n [All available patient records up to year 5]") +
  ylab("Pons Volume \n At each MRI \n") +
  scale_color_manual(values=palette) +
  scale_fill_manual(values=palette) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_x_continuous(breaks = seq(0, 5, by = 0.5)) +
  ylim(0,0.018)


irmForPaulo %>% 
  select(NUM, TIME_STUDY, Medulla) %>% ungroup() %>%
  ggplot(aes(TIME_STUDY, Medulla)) +
  geom_line(aes(group=NUM), col="black" , linewidth=0.5, alpha=0.4) +
  geom_jitter(size=1, colour="firebrick", alpha=0.5, shape=1, stroke=1) +
  theme_minimal() +
  xlab("\n Elapsed number of years since study enrollment \n [All available patient records up to year 5]") +
  ylab("Medulla Volume \n At each MRI \n") +
  scale_color_manual(values=palette) +
  scale_fill_manual(values=palette) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_x_continuous(breaks = seq(0, 5, by = 0.5)) +
  ylim(0,0.005)

irmForPaulo %>% 
  select(NUM, TIME_STUDY, TotalBrain ) %>% ungroup() %>%
  ggplot(aes(TIME_STUDY, TotalBrain )) +
  geom_line(aes(group=NUM), col="black" , linewidth=0.5, alpha=0.4) +
  geom_jitter(size=1, colour="firebrick", alpha=0.5, shape=1, stroke=1) +
  theme_minimal() +
  xlab("\n Elapsed number of years since study enrollment \n [All available patient records up to year 5]") +
  ylab("Whole Brain Volume \n At each MRI \n") +
  scale_color_manual(values=palette) +
  scale_fill_manual(values=palette) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_x_continuous(breaks = seq(0, 5, by = 0.5)) +
  ylim(0,1)


# Left_Putamen *************************************

irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, TIME_STUDY, Left_Putamen) %>%
  summarise(
    mean=mean(Left_Putamen),
    sd=sd(Left_Putamen),
    median=median(Left_Putamen),
    Q1 = quantile(Left_Putamen, 0.25),
    Q3 = quantile(Left_Putamen, 0.75),
    min=min(Left_Putamen),
    max=max(Left_Putamen),
    n=n()
  )



irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>% 
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Left_Putamen, Elapsed) %>%
  summarise(
    mean=mean(TIME_STUDY*12),
    sd=sd(TIME_STUDY*12),
    median=median(TIME_STUDY*12),
    Q1 = quantile(TIME_STUDY*12, 0.25),
    Q3 = quantile(TIME_STUDY*12, 0.75),
    min=min(TIME_STUDY*12),
    max=max(TIME_STUDY*12),
    n=n()
  )


irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>% 
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Left_Putamen) %>%
  summarise(
    mean=mean(Left_Putamen),
    sd=sd(Left_Putamen),
    median=median(Left_Putamen),
    Q1 = quantile(Left_Putamen, 0.25),
    Q3 = quantile(Left_Putamen, 0.75),
    min=min(Left_Putamen),
    max=max(Left_Putamen),
    n=n()
  )




irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, Left_Putamen) %>% rename("Year0"="Left_Putamen") %>%
  inner_join(
    irmForPaulo %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, Left_Putamen)
  ) %>% mutate(Delta=Left_Putamen-Year0) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta),
    sd=sd(Delta),
    median=median(Delta),
    Q1 = quantile(Delta, 0.25),
    Q3 = quantile(Delta, 0.75),
    min=min(Delta),
    max=max(Delta),
    n=n()
  )



irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, Left_Putamen) %>% rename("Year0"="Left_Putamen") %>%
  inner_join(
    irmForPaulo %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, Left_Putamen)
  ) %>% mutate(Delta=100*(Left_Putamen-Year0)/Year0) %>%  ungroup() %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta),
    sd=sd(Delta),
    median=median(Delta),
    Q1 = quantile(Delta, 0.25),
    Q3 = quantile(Delta, 0.75),
    min=min(Delta),
    max=max(Delta),
    n=n()
  )



irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, Left_Putamen) %>% rename("Year0"="Left_Putamen") %>%
  inner_join(
    irmForPaulo %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, Left_Putamen)
  ) %>% mutate(Delta=Left_Putamen-Year0) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta/(TIME_STUDY)),
    sd=sd(Delta/(TIME_STUDY)),
    median=median(Delta/(TIME_STUDY)),
    Q1 = quantile(Delta/(TIME_STUDY), 0.25),
    Q3 = quantile(Delta/(TIME_STUDY), 0.75),
    min=min(Delta/(TIME_STUDY)),
    max=max(Delta/(TIME_STUDY)),
    n=n()
  )




irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, Left_Putamen) %>% rename("Year0"="Left_Putamen") %>%
  inner_join(
    irmForPaulo %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, Left_Putamen)
  ) %>% mutate(Delta=100*(Left_Putamen-Year0)/Year0) %>%  ungroup() %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta/(TIME_STUDY)),
    sd=sd(Delta/(TIME_STUDY)),
    median=median(Delta/(TIME_STUDY)),
    Q1 = quantile(Delta/(TIME_STUDY), 0.25),
    Q3 = quantile(Delta/(TIME_STUDY), 0.75),
    min=min(Delta/(TIME_STUDY)),
    max=max(Delta/(TIME_STUDY)),
    n=n()
  )







# Right_Putamen *************************************************

irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, TIME_STUDY, Right_Putamen) %>%
  summarise(
    mean=mean(Right_Putamen),
    sd=sd(Right_Putamen),
    median=median(Right_Putamen),
    Q1 = quantile(Right_Putamen, 0.25),
    Q3 = quantile(Right_Putamen, 0.75),
    min=min(Right_Putamen),
    max=max(Right_Putamen),
    n=n()
  )



irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>% 
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Right_Putamen, Elapsed) %>%
  summarise(
    mean=mean(TIME_STUDY*12),
    sd=sd(TIME_STUDY*12),
    median=median(TIME_STUDY*12),
    Q1 = quantile(TIME_STUDY*12, 0.25),
    Q3 = quantile(TIME_STUDY*12, 0.75),
    min=min(TIME_STUDY*12),
    max=max(TIME_STUDY*12),
    n=n()
  )


irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>% 
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Right_Putamen) %>%
  summarise(
    mean=mean(Right_Putamen),
    sd=sd(Right_Putamen),
    median=median(Right_Putamen),
    Q1 = quantile(Right_Putamen, 0.25),
    Q3 = quantile(Right_Putamen, 0.75),
    min=min(Right_Putamen),
    max=max(Right_Putamen),
    n=n()
  )




irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, Right_Putamen) %>% rename("Year0"="Right_Putamen") %>%
  inner_join(
    irmForPaulo %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, Right_Putamen)
  ) %>% mutate(Delta=Right_Putamen-Year0) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta),
    sd=sd(Delta),
    median=median(Delta),
    Q1 = quantile(Delta, 0.25),
    Q3 = quantile(Delta, 0.75),
    min=min(Delta),
    max=max(Delta),
    n=n()
  )



irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, Right_Putamen) %>% rename("Year0"="Right_Putamen") %>%
  inner_join(
    irmForPaulo %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, Right_Putamen)
  ) %>% mutate(Delta=100*(Right_Putamen-Year0)/Year0) %>%  ungroup() %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta),
    sd=sd(Delta),
    median=median(Delta),
    Q1 = quantile(Delta, 0.25),
    Q3 = quantile(Delta, 0.75),
    min=min(Delta),
    max=max(Delta),
    n=n()
  )



irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, Right_Putamen) %>% rename("Year0"="Right_Putamen") %>%
  inner_join(
    irmForPaulo %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, Right_Putamen)
  ) %>% mutate(Delta=Right_Putamen-Year0) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta/(TIME_STUDY)),
    sd=sd(Delta/(TIME_STUDY)),
    median=median(Delta/(TIME_STUDY)),
    Q1 = quantile(Delta/(TIME_STUDY), 0.25),
    Q3 = quantile(Delta/(TIME_STUDY), 0.75),
    min=min(Delta/(TIME_STUDY)),
    max=max(Delta/(TIME_STUDY)),
    n=n()
  )




irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, Right_Putamen) %>% rename("Year0"="Right_Putamen") %>%
  inner_join(
    irmForPaulo %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, Right_Putamen)
  ) %>% mutate(Delta=100*(Right_Putamen-Year0)/Year0) %>%  ungroup() %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta/(TIME_STUDY)),
    sd=sd(Delta/(TIME_STUDY)),
    median=median(Delta/(TIME_STUDY)),
    Q1 = quantile(Delta/(TIME_STUDY), 0.25),
    Q3 = quantile(Delta/(TIME_STUDY), 0.75),
    min=min(Delta/(TIME_STUDY)),
    max=max(Delta/(TIME_STUDY)),
    n=n()
  )





# Cerebellum White *************************************************

irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, TIME_STUDY, CerebellumWhite) %>%
  summarise(
    mean=mean(CerebellumWhite),
    sd=sd(CerebellumWhite),
    median=median(CerebellumWhite),
    Q1 = quantile(CerebellumWhite, 0.25),
    Q3 = quantile(CerebellumWhite, 0.75),
    min=min(CerebellumWhite),
    max=max(CerebellumWhite),
    n=n()
  )



irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>% 
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, CerebellumWhite, Elapsed) %>%
  summarise(
    mean=mean(TIME_STUDY*12),
    sd=sd(TIME_STUDY*12),
    median=median(TIME_STUDY*12),
    Q1 = quantile(TIME_STUDY*12, 0.25),
    Q3 = quantile(TIME_STUDY*12, 0.75),
    min=min(TIME_STUDY*12),
    max=max(TIME_STUDY*12),
    n=n()
  )


irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>% 
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, CerebellumWhite) %>%
  summarise(
    mean=mean(CerebellumWhite),
    sd=sd(CerebellumWhite),
    median=median(CerebellumWhite),
    Q1 = quantile(CerebellumWhite, 0.25),
    Q3 = quantile(CerebellumWhite, 0.75),
    min=min(CerebellumWhite),
    max=max(CerebellumWhite),
    n=n()
  )




irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, CerebellumWhite) %>% rename("Year0"="CerebellumWhite") %>%
  inner_join(
    irmForPaulo %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, CerebellumWhite)
  ) %>% mutate(Delta=CerebellumWhite-Year0) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta),
    sd=sd(Delta),
    median=median(Delta),
    Q1 = quantile(Delta, 0.25),
    Q3 = quantile(Delta, 0.75),
    min=min(Delta),
    max=max(Delta),
    n=n()
  )




irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, CerebellumWhite) %>% rename("Year0"="CerebellumWhite") %>%
  inner_join(
    irmForPaulo %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, CerebellumWhite)
  ) %>% mutate(Delta=100*(CerebellumWhite-Year0)/Year0) %>%  ungroup() %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta),
    sd=sd(Delta),
    median=median(Delta),
    Q1 = quantile(Delta, 0.25),
    Q3 = quantile(Delta, 0.75),
    min=min(Delta),
    max=max(Delta),
    n=n()
  )




irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, CerebellumWhite) %>% rename("Year0"="CerebellumWhite") %>%
  inner_join(
    irmForPaulo %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, CerebellumWhite)
  ) %>% mutate(Delta=CerebellumWhite-Year0) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta/(TIME_STUDY)),
    sd=sd(Delta/(TIME_STUDY)),
    median=median(Delta/(TIME_STUDY)),
    Q1 = quantile(Delta/(TIME_STUDY), 0.25),
    Q3 = quantile(Delta/(TIME_STUDY), 0.75),
    min=min(Delta/(TIME_STUDY)),
    max=max(Delta/(TIME_STUDY)),
    n=n()
  )




irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, CerebellumWhite) %>% rename("Year0"="CerebellumWhite") %>%
  inner_join(
    irmForPaulo %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, CerebellumWhite)
  ) %>% mutate(Delta=100*(CerebellumWhite-Year0)/Year0) %>%  ungroup() %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta/(TIME_STUDY)),
    sd=sd(Delta/(TIME_STUDY)),
    median=median(Delta/(TIME_STUDY)),
    Q1 = quantile(Delta/(TIME_STUDY), 0.25),
    Q3 = quantile(Delta/(TIME_STUDY), 0.75),
    min=min(Delta/(TIME_STUDY)),
    max=max(Delta/(TIME_STUDY)),
    n=n()
  )




# Cerebellum Total *************************************************

irmForPaulo <- irmForPaulo %>% mutate(Cerebellum=Left_Cerebellum_White_Matter+Right_Cerebellum_White_Matter+Left_Cerebellum_Cortex+Right_Cerebellum_Cortex   )

irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, TIME_STUDY, CerebellumTotal) %>%
  summarise(
    mean=mean(CerebellumTotal),
    sd=sd(CerebellumTotal),
    median=median(CerebellumTotal),
    Q1 = quantile(CerebellumTotal, 0.25),
    Q3 = quantile(CerebellumTotal, 0.75),
    min=min(CerebellumTotal),
    max=max(CerebellumTotal),
    n=n()
  )



irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>% 
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, CerebellumTotal, Elapsed) %>%
  summarise(
    mean=mean(TIME_STUDY*12),
    sd=sd(TIME_STUDY*12),
    median=median(TIME_STUDY*12),
    Q1 = quantile(TIME_STUDY*12, 0.25),
    Q3 = quantile(TIME_STUDY*12, 0.75),
    min=min(TIME_STUDY*12),
    max=max(TIME_STUDY*12),
    n=n()
  )


irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>% 
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, CerebellumTotal) %>%
  summarise(
    mean=mean(CerebellumTotal),
    sd=sd(CerebellumTotal),
    median=median(CerebellumTotal),
    Q1 = quantile(CerebellumTotal, 0.25),
    Q3 = quantile(CerebellumTotal, 0.75),
    min=min(CerebellumTotal),
    max=max(CerebellumTotal),
    n=n()
  )




irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, CerebellumTotal) %>% rename("Year0"="CerebellumTotal") %>%
  inner_join(
    irmForPaulo %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, CerebellumTotal)
  ) %>% mutate(Delta=CerebellumTotal-Year0) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta),
    sd=sd(Delta),
    median=median(Delta),
    Q1 = quantile(Delta, 0.25),
    Q3 = quantile(Delta, 0.75),
    min=min(Delta),
    max=max(Delta),
    n=n()
  )



irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, CerebellumTotal) %>% rename("Year0"="CerebellumTotal") %>%
  inner_join(
    irmForPaulo %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, CerebellumTotal)
  ) %>% mutate(Delta=100*(CerebellumTotal-Year0)/Year0) %>%  ungroup() %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta),
    sd=sd(Delta),
    median=median(Delta),
    Q1 = quantile(Delta, 0.25),
    Q3 = quantile(Delta, 0.75),
    min=min(Delta),
    max=max(Delta),
    n=n()
  )



irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, CerebellumTotal) %>% rename("Year0"="CerebellumTotal") %>%
  inner_join(
    irmForPaulo %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, CerebellumTotal)
  ) %>% mutate(Delta=CerebellumTotal-Year0) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta/(TIME_STUDY)),
    sd=sd(Delta/(TIME_STUDY)),
    median=median(Delta/(TIME_STUDY)),
    Q1 = quantile(Delta/(TIME_STUDY), 0.25),
    Q3 = quantile(Delta/(TIME_STUDY), 0.75),
    min=min(Delta/(TIME_STUDY)),
    max=max(Delta/(TIME_STUDY)),
    n=n()
  )



irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, CerebellumTotal) %>% rename("Year0"="CerebellumTotal") %>%
  inner_join(
    irmForPaulo %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, CerebellumTotal)
  ) %>% mutate(Delta=100*(CerebellumTotal-Year0)/Year0) %>%  ungroup() %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta/(TIME_STUDY)),
    sd=sd(Delta/(TIME_STUDY)),
    median=median(Delta/(TIME_STUDY)),
    Q1 = quantile(Delta/(TIME_STUDY), 0.25),
    Q3 = quantile(Delta/(TIME_STUDY), 0.75),
    min=min(Delta/(TIME_STUDY)),
    max=max(Delta/(TIME_STUDY)),
    n=n()
  )


# Midbrain  *************************************************

irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, TIME_STUDY, Midbrain) %>%
  summarise(
    mean=mean(Midbrain),
    sd=sd(Midbrain),
    median=median(Midbrain),
    Q1 = quantile(Midbrain, 0.25),
    Q3 = quantile(Midbrain, 0.75),
    min=min(Midbrain),
    max=max(Midbrain),
    n=n()
  )



irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>% 
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Midbrain, Elapsed) %>%
  summarise(
    mean=mean(TIME_STUDY*12),
    sd=sd(TIME_STUDY*12),
    median=median(TIME_STUDY*12),
    Q1 = quantile(TIME_STUDY*12, 0.25),
    Q3 = quantile(TIME_STUDY*12, 0.75),
    min=min(TIME_STUDY*12),
    max=max(TIME_STUDY*12),
    n=n()
  )


irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>% 
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Midbrain) %>%
  summarise(
    mean=mean(Midbrain),
    sd=sd(Midbrain),
    median=median(Midbrain),
    Q1 = quantile(Midbrain, 0.25),
    Q3 = quantile(Midbrain, 0.75),
    min=min(Midbrain),
    max=max(Midbrain),
    n=n()
  )




irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, Midbrain) %>% rename("Year0"="Midbrain") %>%
  inner_join(
    irmForPaulo %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, Midbrain)
  ) %>% mutate(Delta=Midbrain-Year0) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta),
    sd=sd(Delta),
    median=median(Delta),
    Q1 = quantile(Delta, 0.25),
    Q3 = quantile(Delta, 0.75),
    min=min(Delta),
    max=max(Delta),
    n=n()
  )



irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, Midbrain) %>% rename("Year0"="Midbrain") %>%
  inner_join(
    irmForPaulo %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, Midbrain)
  ) %>% mutate(Delta=100*(Midbrain-Year0)/Year0) %>%  ungroup() %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta),
    sd=sd(Delta),
    median=median(Delta),
    Q1 = quantile(Delta, 0.25),
    Q3 = quantile(Delta, 0.75),
    min=min(Delta),
    max=max(Delta),
    n=n()
  )

irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, Midbrain) %>% rename("Year0"="Midbrain") %>%
  inner_join(
    irmForPaulo %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, Midbrain)
  ) %>% mutate(Delta=Midbrain-Year0) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta/(TIME_STUDY)),
    sd=sd(Delta/(TIME_STUDY)),
    median=median(Delta/(TIME_STUDY)),
    Q1 = quantile(Delta/(TIME_STUDY), 0.25),
    Q3 = quantile(Delta/(TIME_STUDY), 0.75),
    min=min(Delta/(TIME_STUDY)),
    max=max(Delta/(TIME_STUDY)),
    n=n()
  )


irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, Midbrain) %>% rename("Year0"="Midbrain") %>%
  inner_join(
    irmForPaulo %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, Midbrain)
  ) %>% mutate(Delta=100*(Midbrain-Year0)/Year0) %>%  ungroup() %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta/(TIME_STUDY)),
    sd=sd(Delta/(TIME_STUDY)),
    median=median(Delta/(TIME_STUDY)),
    Q1 = quantile(Delta/(TIME_STUDY), 0.25),
    Q3 = quantile(Delta/(TIME_STUDY), 0.75),
    min=min(Delta/(TIME_STUDY)),
    max=max(Delta/(TIME_STUDY)),
    n=n()
  )



# Pons  *************************************************

irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, TIME_STUDY, Pons) %>%
  summarise(
    mean=mean(Pons),
    sd=sd(Pons),
    median=median(Pons),
    Q1 = quantile(Pons, 0.25),
    Q3 = quantile(Pons, 0.75),
    min=min(Pons),
    max=max(Pons),
    n=n()
  )



irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>% 
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Pons, Elapsed) %>%
  summarise(
    mean=mean(TIME_STUDY*12),
    sd=sd(TIME_STUDY*12),
    median=median(TIME_STUDY*12),
    Q1 = quantile(TIME_STUDY*12, 0.25),
    Q3 = quantile(TIME_STUDY*12, 0.75),
    min=min(TIME_STUDY*12),
    max=max(TIME_STUDY*12),
    n=n()
  )


irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>% 
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Pons) %>%
  summarise(
    mean=mean(Pons),
    sd=sd(Pons),
    median=median(Pons),
    Q1 = quantile(Pons, 0.25),
    Q3 = quantile(Pons, 0.75),
    min=min(Pons),
    max=max(Pons),
    n=n()
  )




irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, Pons) %>% rename("Year0"="Pons") %>%
  inner_join(
    irmForPaulo %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, Pons)
  ) %>% mutate(Delta=Pons-Year0) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta),
    sd=sd(Delta),
    median=median(Delta),
    Q1 = quantile(Delta, 0.25),
    Q3 = quantile(Delta, 0.75),
    min=min(Delta),
    max=max(Delta),
    n=n()
  )


irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, Pons) %>% rename("Year0"="Pons") %>%
  inner_join(
    irmForPaulo %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, Pons)
  ) %>% mutate(Delta=100*(Pons-Year0)/Year0) %>%  ungroup() %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta),
    sd=sd(Delta),
    median=median(Delta),
    Q1 = quantile(Delta, 0.25),
    Q3 = quantile(Delta, 0.75),
    min=min(Delta),
    max=max(Delta),
    n=n()
  )


irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, Pons) %>% rename("Year0"="Pons") %>%
  inner_join(
    irmForPaulo %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, Pons)
  ) %>% mutate(Delta=Pons-Year0) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta/(TIME_STUDY)),
    sd=sd(Delta/(TIME_STUDY)),
    median=median(Delta/(TIME_STUDY)),
    Q1 = quantile(Delta/(TIME_STUDY), 0.25),
    Q3 = quantile(Delta/(TIME_STUDY), 0.75),
    min=min(Delta/(TIME_STUDY)),
    max=max(Delta/(TIME_STUDY)),
    n=n()
  )




irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, Pons) %>% rename("Year0"="Pons") %>%
  inner_join(
    irmForPaulo %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, Pons)
  ) %>% mutate(Delta=100*(Pons-Year0)/Year0) %>%  ungroup() %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta/(TIME_STUDY)),
    sd=sd(Delta/(TIME_STUDY)),
    median=median(Delta/(TIME_STUDY)),
    Q1 = quantile(Delta/(TIME_STUDY), 0.25),
    Q3 = quantile(Delta/(TIME_STUDY), 0.75),
    min=min(Delta/(TIME_STUDY)),
    max=max(Delta/(TIME_STUDY)),
    n=n()
  )







# Medulla  *************************************************

irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, TIME_STUDY, Medulla) %>%
  summarise(
    mean=mean(Medulla),
    sd=sd(Medulla),
    median=median(Medulla),
    Q1 = quantile(Medulla, 0.25),
    Q3 = quantile(Medulla, 0.75),
    min=min(Medulla),
    max=max(Medulla),
    n=n()
  )



irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>% 
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Medulla, Elapsed) %>%
  summarise(
    mean=mean(TIME_STUDY*12),
    sd=sd(TIME_STUDY*12),
    median=median(TIME_STUDY*12),
    Q1 = quantile(TIME_STUDY*12, 0.25),
    Q3 = quantile(TIME_STUDY*12, 0.75),
    min=min(TIME_STUDY*12),
    max=max(TIME_STUDY*12),
    n=n()
  )


irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>% 
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Medulla) %>%
  summarise(
    mean=mean(Medulla),
    sd=sd(Medulla),
    median=median(Medulla),
    Q1 = quantile(Medulla, 0.25),
    Q3 = quantile(Medulla, 0.75),
    min=min(Medulla),
    max=max(Medulla),
    n=n()
  )




irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, Medulla) %>% rename("Year0"="Medulla") %>%
  inner_join(
    irmForPaulo %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, Medulla)
  ) %>% mutate(Delta=Medulla-Year0) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta),
    sd=sd(Delta),
    median=median(Delta),
    Q1 = quantile(Delta, 0.25),
    Q3 = quantile(Delta, 0.75),
    min=min(Delta),
    max=max(Delta),
    n=n()
  )



irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, Medulla) %>% rename("Year0"="Medulla") %>%
  inner_join(
    irmForPaulo %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, Medulla)
  ) %>% mutate(Delta=100*(Medulla-Year0)/Year0) %>%  ungroup() %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta),
    sd=sd(Delta),
    median=median(Delta),
    Q1 = quantile(Delta, 0.25),
    Q3 = quantile(Delta, 0.75),
    min=min(Delta),
    max=max(Delta),
    n=n()
  )



irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, Medulla) %>% rename("Year0"="Medulla") %>%
  inner_join(
    irmForPaulo %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, Medulla)
  ) %>% mutate(Delta=Medulla-Year0) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta/(TIME_STUDY)),
    sd=sd(Delta/(TIME_STUDY)),
    median=median(Delta/(TIME_STUDY)),
    Q1 = quantile(Delta/(TIME_STUDY), 0.25),
    Q3 = quantile(Delta/(TIME_STUDY), 0.75),
    min=min(Delta/(TIME_STUDY)),
    max=max(Delta/(TIME_STUDY)),
    n=n()
  )



irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, Medulla) %>% rename("Year0"="Medulla") %>%
  inner_join(
    irmForPaulo %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, Medulla)
  ) %>% mutate(Delta=100*(Medulla-Year0)/Year0) %>%  ungroup() %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta/(TIME_STUDY)),
    sd=sd(Delta/(TIME_STUDY)),
    median=median(Delta/(TIME_STUDY)),
    Q1 = quantile(Delta/(TIME_STUDY), 0.25),
    Q3 = quantile(Delta/(TIME_STUDY), 0.75),
    min=min(Delta/(TIME_STUDY)),
    max=max(Delta/(TIME_STUDY)),
    n=n()
  )





# TotalBrain   *************************************************

irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, TIME_STUDY, TotalBrain ) %>%
  summarise(
    mean=mean(TotalBrain ),
    sd=sd(TotalBrain ),
    median=median(TotalBrain ),
    Q1 = quantile(TotalBrain , 0.25),
    Q3 = quantile(TotalBrain , 0.75),
    min=min(TotalBrain ),
    max=max(TotalBrain ),
    n=n()
  )



irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>% 
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, TotalBrain , Elapsed) %>%
  summarise(
    mean=mean(TIME_STUDY*12),
    sd=sd(TIME_STUDY*12),
    median=median(TIME_STUDY*12),
    Q1 = quantile(TIME_STUDY*12, 0.25),
    Q3 = quantile(TIME_STUDY*12, 0.75),
    min=min(TIME_STUDY*12),
    max=max(TIME_STUDY*12),
    n=n()
  )


irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>% 
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, TotalBrain ) %>%
  summarise(
    mean=mean(TotalBrain ),
    sd=sd(TotalBrain ),
    median=median(TotalBrain ),
    Q1 = quantile(TotalBrain , 0.25),
    Q3 = quantile(TotalBrain , 0.75),
    min=min(TotalBrain ),
    max=max(TotalBrain ),
    n=n()
  )




irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, TotalBrain ) %>% rename("Year0"="TotalBrain") %>%
  inner_join(
    irmForPaulo %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, TotalBrain )
  ) %>% mutate(Delta=TotalBrain -Year0) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta),
    sd=sd(Delta),
    median=median(Delta),
    Q1 = quantile(Delta, 0.25),
    Q3 = quantile(Delta, 0.75),
    min=min(Delta),
    max=max(Delta),
    n=n()
  )



irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, TotalBrain ) %>% rename("Year0"="TotalBrain") %>%
  inner_join(
    irmForPaulo %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, TotalBrain )
  ) %>% mutate(Delta=100*(TotalBrain-Year0)/Year0) %>%  ungroup() %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta),
    sd=sd(Delta),
    median=median(Delta),
    Q1 = quantile(Delta, 0.25),
    Q3 = quantile(Delta, 0.75),
    min=min(Delta),
    max=max(Delta),
    n=n()
  )


irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, TotalBrain ) %>% rename("Year0"="TotalBrain") %>%
  inner_join(
    irmForPaulo %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, TotalBrain )
  ) %>% mutate(Delta=TotalBrain -Year0) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta/(TIME_STUDY)),
    sd=sd(Delta/(TIME_STUDY)),
    median=median(Delta/(TIME_STUDY)),
    Q1 = quantile(Delta/(TIME_STUDY), 0.25),
    Q3 = quantile(Delta/(TIME_STUDY), 0.75),
    min=min(Delta/(TIME_STUDY)),
    max=max(Delta/(TIME_STUDY)),
    n=n()
  )


irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, TotalBrain ) %>% rename("Year0"="TotalBrain") %>%
  inner_join(
    irmForPaulo %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, TotalBrain )
  ) %>% mutate(Delta=100*(TotalBrain-Year0)/Year0) %>%  ungroup() %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta/(TIME_STUDY)),
    sd=sd(Delta/(TIME_STUDY)),
    median=median(Delta/(TIME_STUDY)),
    Q1 = quantile(Delta/(TIME_STUDY), 0.25),
    Q3 = quantile(Delta/(TIME_STUDY), 0.75),
    min=min(Delta/(TIME_STUDY)),
    max=max(Delta/(TIME_STUDY)),
    n=n()
  )

# ------------------------------------------
# MEAN DIFFUSIVITY ------------------------


MD_Takeda <- readxl::read_xlsx("Source/MD_Takeda.xlsx")
MD_Takeda$DateIRM <-  as.Date(MD_Takeda$DateIRM)
names(MD_Takeda)

MD_Takeda <- MD_Takeda %>% select(NUM, DateIRM,
                                  MD_L_Puta, 
                                  MD_R_Puta, 
                                  MD_L_Cerebell_WM,MD_R_Cerebell_WM,
                                  MD_L_Cerebell_GM, MD_R_Cerebell_GM,
                                  MD_Midbrainn,
                                  MD_Pons,
                                  MD_Medulla)

MD_Takeda <- MD_Takeda %>% arrange(NUM, DateIRM) %>% group_by(NUM) %>%
  mutate(TIME_STUDY=time_length(difftime(DateIRM, lag(DateIRM) ), "years"))  %>%
  select(NUM, DateIRM, TIME_STUDY,
         MD_L_Puta, 
         MD_R_Puta, 
         MD_L_Cerebell_WM,MD_R_Cerebell_WM,
         MD_L_Cerebell_GM, MD_R_Cerebell_GM,
         MD_Midbrainn,
         MD_Pons,
         MD_Medulla)


MD_Takeda <- MD_Takeda %>% mutate(TIME_STUDY=ifelse(is.na(TIME_STUDY), 0, TIME_STUDY))

MD_Takeda <- MD_Takeda %>% mutate(TIME_STUDY=cumsum(TIME_STUDY))

MD_Takeda <- MD_Takeda %>% group_by(NUM) %>% count() %>%
  left_join(MD_Takeda) %>%
  select(NUM, DateIRM, TIME_STUDY, n,
         MD_L_Puta, 
         MD_R_Puta, 
         MD_L_Cerebell_WM,MD_R_Cerebell_WM,
         MD_L_Cerebell_GM, MD_R_Cerebell_GM,
         MD_Midbrainn,
         MD_Pons,
         MD_Medulla)


length(unique(MD_Takeda$NUM)) # 24

MD_Takeda %>% ungroup() %>% group_by(n) %>% count()

MD_Takeda %>% mutate(Year= ifelse(TIME_STUDY<=0.5, 0,
                                  ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                                         ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                                ifelse(TIME_STUDY>=2.5 ,3, NA))))) %>%
  select(NUM, Year) %>% group_by(NUM) %>%
  mutate(All=paste0(Year, collapse=","))

range(MD_Takeda$TIME_STUDY)


MD_Takeda <- MD_Takeda %>% mutate(CerebellumWhite=(MD_L_Cerebell_WM+MD_R_Cerebell_WM)/2) %>%
  mutate(CerebellumGray=(MD_L_Cerebell_GM+MD_R_Cerebell_GM)/2) %>%
  mutate(CerebellumTotal=0.82*CerebellumGray+0.18*CerebellumWhite)




# PLOTS **************************************

MD_Takeda %>% select(NUM, n) %>% distinct() %>%
  group_by(n) %>% count() %>%
  ggplot(aes(n, nn)) +
  geom_col(fill="black", colour="black") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  xlab("\n Number of MRIs per patient") +
  ylab("\n Number of Patients \n") +
  geom_text(aes(label = nn), vjust = -0.5)


MD_Takeda %>% 
  select(NUM, TIME_STUDY, MD_L_Puta) %>% ungroup() %>%
  ggplot(aes(TIME_STUDY, MD_L_Puta)) +
  geom_line(aes(group=NUM), col="black" , linewidth=0.5, alpha=0.4) +
  geom_jitter(size=1, colour="firebrick", alpha=0.5, shape=1, stroke=1) +
  theme_minimal() +
  xlab("\n Elapsed number of years since study enrollment \n [All available patient records up to year 5]") +
  ylab("Left Putamen Mean Diffusivity \n At each MRI \n") +
  scale_color_manual(values=palette) +
  scale_fill_manual(values=palette) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_x_continuous(breaks = seq(0, 5, by = 0.5)) +
  ylim(0,1.2)


MD_Takeda %>% 
  select(NUM, TIME_STUDY, MD_R_Puta) %>% ungroup() %>%
  ggplot(aes(TIME_STUDY, MD_R_Puta)) +
  geom_line(aes(group=NUM), col="black" , linewidth=0.5, alpha=0.4) +
  geom_jitter(size=1, colour="firebrick", alpha=0.5, shape=1, stroke=1) +
  theme_minimal() +
  xlab("\n Elapsed number of years since study enrollment \n [All available patient records up to year 5]") +
  ylab("Right Putamen Mean Diffusivity \n At each MRI \n") +
  scale_color_manual(values=palette) +
  scale_fill_manual(values=palette) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_x_continuous(breaks = seq(0, 5, by = 0.5)) +
  ylim(0,1.2)


MD_Takeda %>% 
  select(NUM, TIME_STUDY, CerebellumWhite) %>% ungroup() %>%
  ggplot(aes(TIME_STUDY, CerebellumWhite)) +
  geom_line(aes(group=NUM), col="black" , linewidth=0.5, alpha=0.4) +
  geom_jitter(size=1, colour="firebrick", alpha=0.5, shape=1, stroke=1) +
  theme_minimal() +
  xlab("\n Elapsed number of years since study enrollment \n [All available patient records up to year 5]") +
  ylab("Cerebellum White Mean Diffusivity \n At each MRI \n") +
  scale_color_manual(values=palette) +
  scale_fill_manual(values=palette) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_x_continuous(breaks = seq(0, 5, by = 0.5)) +
  ylim(0,1.4)


MD_Takeda %>% 
  select(NUM, TIME_STUDY, CerebellumTotal) %>% ungroup() %>%
  ggplot(aes(TIME_STUDY, CerebellumTotal)) +
  geom_line(aes(group=NUM), col="black" , linewidth=0.5, alpha=0.4) +
  geom_jitter(size=1, colour="firebrick", alpha=0.5, shape=1, stroke=1) +
  theme_minimal() +
  xlab("\n Elapsed number of years since study enrollment \n [All available patient records up to year 5]") +
  ylab("Cerebellum Total Mean Diffusivity  \n At each MRI \n") +
  scale_color_manual(values=palette) +
  scale_fill_manual(values=palette) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_x_continuous(breaks = seq(0, 5, by = 0.5)) +
  ylim(0,1.6)


MD_Takeda %>% 
  select(NUM, TIME_STUDY, MD_Midbrainn) %>% ungroup() %>%
  ggplot(aes(TIME_STUDY, MD_Midbrainn)) +
  geom_line(aes(group=NUM), col="black" , linewidth=0.5, alpha=0.4) +
  geom_jitter(size=1, colour="firebrick", alpha=0.5, shape=1, stroke=1) +
  theme_minimal() +
  xlab("\n Elapsed number of years since study enrollment \n [All available patient records up to year 5]") +
  ylab("Midbrain Mean Diffusivity \n At each MRI \n") +
  scale_color_manual(values=palette) +
  scale_fill_manual(values=palette) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_x_continuous(breaks = seq(0, 5, by = 0.5)) +
  ylim(0,1.4)


MD_Takeda %>% 
  select(NUM, TIME_STUDY, MD_Pons) %>% ungroup() %>%
  ggplot(aes(TIME_STUDY, MD_Pons)) +
  geom_line(aes(group=NUM), col="black" , linewidth=0.5, alpha=0.4) +
  geom_jitter(size=1, colour="firebrick", alpha=0.5, shape=1, stroke=1) +
  theme_minimal() +
  xlab("\n Elapsed number of years since study enrollment \n [All available patient records up to year 5]") +
  ylab("Pons Mean Diffusivity \n At each MRI \n") +
  scale_color_manual(values=palette) +
  scale_fill_manual(values=palette) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_x_continuous(breaks = seq(0, 5, by = 0.5)) +
  ylim(0,2.1)


MD_Takeda %>% 
  select(NUM, TIME_STUDY, MD_Medulla) %>% ungroup() %>%
  ggplot(aes(TIME_STUDY, MD_Medulla)) +
  geom_line(aes(group=NUM), col="black" , linewidth=0.5, alpha=0.4) +
  geom_jitter(size=1, colour="firebrick", alpha=0.5, shape=1, stroke=1) +
  theme_minimal() +
  xlab("\n Elapsed number of years since study enrollment \n [All available patient records up to year 5]") +
  ylab("Medulla Mean Diffusivity \n At each MRI \n") +
  scale_color_manual(values=palette) +
  scale_fill_manual(values=palette) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_x_continuous(breaks = seq(0, 5, by = 0.5)) +
  ylim(0,2.2)

# END of PLOTS ****************************


# ORIGINAL MEAN DIFFUSIVITY ************************


# MD_L_Puta *************************************

MD_Takeda %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, TIME_STUDY, MD_L_Puta) %>%
  summarise(
    mean=mean(MD_L_Puta),
    sd=sd(MD_L_Puta),
    median=median(MD_L_Puta),
    Q1 = quantile(MD_L_Puta, 0.25),
    Q3 = quantile(MD_L_Puta, 0.75),
    min=min(MD_L_Puta),
    max=max(MD_L_Puta),
    n=n()
  )



MD_Takeda %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>% 
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, MD_L_Puta, Elapsed) %>%
  summarise(
    mean=mean(TIME_STUDY*12),
    sd=sd(TIME_STUDY*12),
    median=median(TIME_STUDY*12),
    Q1 = quantile(TIME_STUDY*12, 0.25),
    Q3 = quantile(TIME_STUDY*12, 0.75),
    min=min(TIME_STUDY*12),
    max=max(TIME_STUDY*12),
    n=n()
  )


MD_Takeda %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>% 
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, MD_L_Puta) %>%
  summarise(
    mean=mean(MD_L_Puta),
    sd=sd(MD_L_Puta),
    median=median(MD_L_Puta),
    Q1 = quantile(MD_L_Puta, 0.25),
    Q3 = quantile(MD_L_Puta, 0.75),
    min=min(MD_L_Puta),
    max=max(MD_L_Puta),
    n=n()
  )




MD_Takeda %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, MD_L_Puta) %>% rename("Year0"="MD_L_Puta") %>%
  inner_join(
    MD_Takeda %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, MD_L_Puta)
  ) %>% mutate(Delta=MD_L_Puta-Year0) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta),
    sd=sd(Delta),
    median=median(Delta),
    Q1 = quantile(Delta, 0.25),
    Q3 = quantile(Delta, 0.75),
    min=min(Delta),
    max=max(Delta),
    n=n()
  )



MD_Takeda %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, MD_L_Puta) %>% rename("Year0"="MD_L_Puta") %>%
  inner_join(
    MD_Takeda %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, MD_L_Puta)
  ) %>% mutate(Delta=100*(MD_L_Puta-Year0)/Year0) %>%  ungroup() %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta),
    sd=sd(Delta),
    median=median(Delta),
    Q1 = quantile(Delta, 0.25),
    Q3 = quantile(Delta, 0.75),
    min=min(Delta),
    max=max(Delta),
    n=n()
  )


MD_Takeda %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, MD_L_Puta) %>% rename("Year0"="MD_L_Puta") %>%
  inner_join(
    MD_Takeda %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, MD_L_Puta)
  ) %>% mutate(Delta=MD_L_Puta-Year0) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta/(TIME_STUDY)),
    sd=sd(Delta/(TIME_STUDY)),
    median=median(Delta/(TIME_STUDY)),
    Q1 = quantile(Delta/(TIME_STUDY), 0.25),
    Q3 = quantile(Delta/(TIME_STUDY), 0.75),
    min=min(Delta/(TIME_STUDY)),
    max=max(Delta/(TIME_STUDY)),
    n=n()
  )



MD_Takeda %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, MD_L_Puta) %>% rename("Year0"="MD_L_Puta") %>%
  inner_join(
    MD_Takeda %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, MD_L_Puta)
  ) %>% mutate(Delta=100*(MD_L_Puta-Year0)/Year0) %>%  ungroup() %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta/(TIME_STUDY)),
    sd=sd(Delta/(TIME_STUDY)),
    median=median(Delta/(TIME_STUDY)),
    Q1 = quantile(Delta/(TIME_STUDY), 0.25),
    Q3 = quantile(Delta/(TIME_STUDY), 0.75),
    min=min(Delta/(TIME_STUDY)),
    max=max(Delta/(TIME_STUDY)),
    n=n()
  )








# MD_R_Puta *************************************************

MD_Takeda %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, TIME_STUDY, MD_R_Puta) %>%
  summarise(
    mean=mean(MD_R_Puta),
    sd=sd(MD_R_Puta),
    median=median(MD_R_Puta),
    Q1 = quantile(MD_R_Puta, 0.25),
    Q3 = quantile(MD_R_Puta, 0.75),
    min=min(MD_R_Puta),
    max=max(MD_R_Puta),
    n=n()
  )



MD_Takeda %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>% 
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, MD_R_Puta, Elapsed) %>%
  summarise(
    mean=mean(TIME_STUDY*12),
    sd=sd(TIME_STUDY*12),
    median=median(TIME_STUDY*12),
    Q1 = quantile(TIME_STUDY*12, 0.25),
    Q3 = quantile(TIME_STUDY*12, 0.75),
    min=min(TIME_STUDY*12),
    max=max(TIME_STUDY*12),
    n=n()
  )


MD_Takeda %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>% 
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, MD_R_Puta) %>%
  summarise(
    mean=mean(MD_R_Puta),
    sd=sd(MD_R_Puta),
    median=median(MD_R_Puta),
    Q1 = quantile(MD_R_Puta, 0.25),
    Q3 = quantile(MD_R_Puta, 0.75),
    min=min(MD_R_Puta),
    max=max(MD_R_Puta),
    n=n()
  )




MD_Takeda %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, MD_R_Puta) %>% rename("Year0"="MD_R_Puta") %>%
  inner_join(
    MD_Takeda %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, MD_R_Puta)
  ) %>% mutate(Delta=MD_R_Puta-Year0) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta),
    sd=sd(Delta),
    median=median(Delta),
    Q1 = quantile(Delta, 0.25),
    Q3 = quantile(Delta, 0.75),
    min=min(Delta),
    max=max(Delta),
    n=n()
  )



MD_Takeda %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, MD_R_Puta) %>% rename("Year0"="MD_R_Puta") %>%
  inner_join(
    MD_Takeda %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, MD_R_Puta)
  ) %>% mutate(Delta=100*(MD_R_Puta-Year0)/Year0) %>%  ungroup() %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta),
    sd=sd(Delta),
    median=median(Delta),
    Q1 = quantile(Delta, 0.25),
    Q3 = quantile(Delta, 0.75),
    min=min(Delta),
    max=max(Delta),
    n=n()
  )


MD_Takeda %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, MD_R_Puta) %>% rename("Year0"="MD_R_Puta") %>%
  inner_join(
    MD_Takeda %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, MD_R_Puta)
  ) %>% mutate(Delta=MD_R_Puta-Year0) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta/(TIME_STUDY)),
    sd=sd(Delta/(TIME_STUDY)),
    median=median(Delta/(TIME_STUDY)),
    Q1 = quantile(Delta/(TIME_STUDY), 0.25),
    Q3 = quantile(Delta/(TIME_STUDY), 0.75),
    min=min(Delta/(TIME_STUDY)),
    max=max(Delta/(TIME_STUDY)),
    n=n()
  )



MD_Takeda %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, MD_R_Puta) %>% rename("Year0"="MD_R_Puta") %>%
  inner_join(
    MD_Takeda %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, MD_R_Puta)
  ) %>% mutate(Delta=100*(MD_R_Puta-Year0)/Year0) %>%  ungroup() %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta/(TIME_STUDY)),
    sd=sd(Delta/(TIME_STUDY)),
    median=median(Delta/(TIME_STUDY)),
    Q1 = quantile(Delta/(TIME_STUDY), 0.25),
    Q3 = quantile(Delta/(TIME_STUDY), 0.75),
    min=min(Delta/(TIME_STUDY)),
    max=max(Delta/(TIME_STUDY)),
    n=n()
  )





# Cerebellum White *************************************************

MD_Takeda %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, TIME_STUDY, CerebellumWhite) %>%
  summarise(
    mean=mean(CerebellumWhite),
    sd=sd(CerebellumWhite),
    median=median(CerebellumWhite),
    Q1 = quantile(CerebellumWhite, 0.25),
    Q3 = quantile(CerebellumWhite, 0.75),
    min=min(CerebellumWhite),
    max=max(CerebellumWhite),
    n=n()
  )



MD_Takeda %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>% 
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, CerebellumWhite, Elapsed) %>%
  summarise(
    mean=mean(TIME_STUDY*12),
    sd=sd(TIME_STUDY*12),
    median=median(TIME_STUDY*12),
    Q1 = quantile(TIME_STUDY*12, 0.25),
    Q3 = quantile(TIME_STUDY*12, 0.75),
    min=min(TIME_STUDY*12),
    max=max(TIME_STUDY*12),
    n=n()
  )


MD_Takeda %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>% 
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, CerebellumWhite) %>%
  summarise(
    mean=mean(CerebellumWhite),
    sd=sd(CerebellumWhite),
    median=median(CerebellumWhite),
    Q1 = quantile(CerebellumWhite, 0.25),
    Q3 = quantile(CerebellumWhite, 0.75),
    min=min(CerebellumWhite),
    max=max(CerebellumWhite),
    n=n()
  )




MD_Takeda %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, CerebellumWhite) %>% rename("Year0"="CerebellumWhite") %>%
  inner_join(
    MD_Takeda %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, CerebellumWhite)
  ) %>% mutate(Delta=CerebellumWhite-Year0) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta),
    sd=sd(Delta),
    median=median(Delta),
    Q1 = quantile(Delta, 0.25),
    Q3 = quantile(Delta, 0.75),
    min=min(Delta),
    max=max(Delta),
    n=n()
  )


MD_Takeda %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, CerebellumWhite) %>% rename("Year0"="CerebellumWhite") %>%
  inner_join(
    MD_Takeda %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, CerebellumWhite)
  ) %>% mutate(Delta=100*(CerebellumWhite-Year0)/Year0) %>%  ungroup() %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta),
    sd=sd(Delta),
    median=median(Delta),
    Q1 = quantile(Delta, 0.25),
    Q3 = quantile(Delta, 0.75),
    min=min(Delta),
    max=max(Delta),
    n=n()
  )


MD_Takeda %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, CerebellumWhite) %>% rename("Year0"="CerebellumWhite") %>%
  inner_join(
    MD_Takeda %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, CerebellumWhite)
  ) %>% mutate(Delta=CerebellumWhite-Year0) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta/(TIME_STUDY)),
    sd=sd(Delta/(TIME_STUDY)),
    median=median(Delta/(TIME_STUDY)),
    Q1 = quantile(Delta/(TIME_STUDY), 0.25),
    Q3 = quantile(Delta/(TIME_STUDY), 0.75),
    min=min(Delta/(TIME_STUDY)),
    max=max(Delta/(TIME_STUDY)),
    n=n()
  )



MD_Takeda %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, CerebellumWhite) %>% rename("Year0"="CerebellumWhite") %>%
  inner_join(
    MD_Takeda %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, CerebellumWhite)
  ) %>% mutate(Delta=100*(CerebellumWhite-Year0)/Year0) %>%  ungroup() %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta/(TIME_STUDY)),
    sd=sd(Delta/(TIME_STUDY)),
    median=median(Delta/(TIME_STUDY)),
    Q1 = quantile(Delta/(TIME_STUDY), 0.25),
    Q3 = quantile(Delta/(TIME_STUDY), 0.75),
    min=min(Delta/(TIME_STUDY)),
    max=max(Delta/(TIME_STUDY)),
    n=n()
  )



# Cerebellum Total *************************************************


MD_Takeda %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, TIME_STUDY, CerebellumTotal) %>%
  summarise(
    mean=mean(CerebellumTotal),
    sd=sd(CerebellumTotal),
    median=median(CerebellumTotal),
    Q1 = quantile(CerebellumTotal, 0.25),
    Q3 = quantile(CerebellumTotal, 0.75),
    min=min(CerebellumTotal),
    max=max(CerebellumTotal),
    n=n()
  )



MD_Takeda %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>% 
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, CerebellumTotal, Elapsed) %>%
  summarise(
    mean=mean(TIME_STUDY*12),
    sd=sd(TIME_STUDY*12),
    median=median(TIME_STUDY*12),
    Q1 = quantile(TIME_STUDY*12, 0.25),
    Q3 = quantile(TIME_STUDY*12, 0.75),
    min=min(TIME_STUDY*12),
    max=max(TIME_STUDY*12),
    n=n()
  )


MD_Takeda %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>% 
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, CerebellumTotal) %>%
  summarise(
    mean=mean(CerebellumTotal),
    sd=sd(CerebellumTotal),
    median=median(CerebellumTotal),
    Q1 = quantile(CerebellumTotal, 0.25),
    Q3 = quantile(CerebellumTotal, 0.75),
    min=min(CerebellumTotal),
    max=max(CerebellumTotal),
    n=n()
  )




MD_Takeda %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, CerebellumTotal) %>% rename("Year0"="CerebellumTotal") %>%
  inner_join(
    MD_Takeda %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, CerebellumTotal)
  ) %>% mutate(Delta=CerebellumTotal-Year0) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta),
    sd=sd(Delta),
    median=median(Delta),
    Q1 = quantile(Delta, 0.25),
    Q3 = quantile(Delta, 0.75),
    min=min(Delta),
    max=max(Delta),
    n=n()
  )





MD_Takeda %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, CerebellumTotal) %>% rename("Year0"="CerebellumTotal") %>%
  inner_join(
    MD_Takeda %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, CerebellumTotal)
  ) %>% mutate(Delta=100*(CerebellumTotal-Year0)/Year0) %>%  ungroup() %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta),
    sd=sd(Delta),
    median=median(Delta),
    Q1 = quantile(Delta, 0.25),
    Q3 = quantile(Delta, 0.75),
    min=min(Delta),
    max=max(Delta),
    n=n()
  )



MD_Takeda %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, CerebellumTotal) %>% rename("Year0"="CerebellumTotal") %>%
  inner_join(
    MD_Takeda %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, CerebellumTotal)
  ) %>% mutate(Delta=CerebellumTotal-Year0) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta/(TIME_STUDY)),
    sd=sd(Delta/(TIME_STUDY)),
    median=median(Delta/(TIME_STUDY)),
    Q1 = quantile(Delta/(TIME_STUDY), 0.25),
    Q3 = quantile(Delta/(TIME_STUDY), 0.75),
    min=min(Delta/(TIME_STUDY)),
    max=max(Delta/(TIME_STUDY)),
    n=n()
  )


MD_Takeda %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, CerebellumTotal) %>% rename("Year0"="CerebellumTotal") %>%
  inner_join(
    MD_Takeda %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, CerebellumTotal)
  ) %>% mutate(Delta=100*(CerebellumTotal-Year0)/Year0) %>%  ungroup() %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta/(TIME_STUDY)),
    sd=sd(Delta/(TIME_STUDY)),
    median=median(Delta/(TIME_STUDY)),
    Q1 = quantile(Delta/(TIME_STUDY), 0.25),
    Q3 = quantile(Delta/(TIME_STUDY), 0.75),
    min=min(Delta/(TIME_STUDY)),
    max=max(Delta/(TIME_STUDY)),
    n=n()
  )



# MD_Midbrainn  *************************************************

MD_Takeda %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, TIME_STUDY, MD_Midbrainn) %>%
  summarise(
    mean=mean(MD_Midbrainn),
    sd=sd(MD_Midbrainn),
    median=median(MD_Midbrainn),
    Q1 = quantile(MD_Midbrainn, 0.25),
    Q3 = quantile(MD_Midbrainn, 0.75),
    min=min(MD_Midbrainn),
    max=max(MD_Midbrainn),
    n=n()
  )



MD_Takeda %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>% 
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, MD_Midbrainn, Elapsed) %>%
  summarise(
    mean=mean(TIME_STUDY*12),
    sd=sd(TIME_STUDY*12),
    median=median(TIME_STUDY*12),
    Q1 = quantile(TIME_STUDY*12, 0.25),
    Q3 = quantile(TIME_STUDY*12, 0.75),
    min=min(TIME_STUDY*12),
    max=max(TIME_STUDY*12),
    n=n()
  )


MD_Takeda %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>% 
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, MD_Midbrainn) %>%
  summarise(
    mean=mean(MD_Midbrainn),
    sd=sd(MD_Midbrainn),
    median=median(MD_Midbrainn),
    Q1 = quantile(MD_Midbrainn, 0.25),
    Q3 = quantile(MD_Midbrainn, 0.75),
    min=min(MD_Midbrainn),
    max=max(MD_Midbrainn),
    n=n()
  )




MD_Takeda %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, MD_Midbrainn) %>% rename("Year0"="MD_Midbrainn") %>%
  inner_join(
    MD_Takeda %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, MD_Midbrainn)
  ) %>% mutate(Delta=MD_Midbrainn-Year0) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta),
    sd=sd(Delta),
    median=median(Delta),
    Q1 = quantile(Delta, 0.25),
    Q3 = quantile(Delta, 0.75),
    min=min(Delta),
    max=max(Delta),
    n=n()
  )


MD_Takeda %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, MD_Midbrainn) %>% rename("Year0"="MD_Midbrainn") %>%
  inner_join(
    MD_Takeda %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, MD_Midbrainn)
  ) %>% mutate(Delta=100*(MD_Midbrainn-Year0)/Year0) %>%  ungroup() %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta),
    sd=sd(Delta),
    median=median(Delta),
    Q1 = quantile(Delta, 0.25),
    Q3 = quantile(Delta, 0.75),
    min=min(Delta),
    max=max(Delta),
    n=n()
  )



MD_Takeda %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, MD_Midbrainn) %>% rename("Year0"="MD_Midbrainn") %>%
  inner_join(
    MD_Takeda %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, MD_Midbrainn)
  ) %>% mutate(Delta=MD_Midbrainn-Year0) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta/(TIME_STUDY)),
    sd=sd(Delta/(TIME_STUDY)),
    median=median(Delta/(TIME_STUDY)),
    Q1 = quantile(Delta/(TIME_STUDY), 0.25),
    Q3 = quantile(Delta/(TIME_STUDY), 0.75),
    min=min(Delta/(TIME_STUDY)),
    max=max(Delta/(TIME_STUDY)),
    n=n()
  )


MD_Takeda %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, MD_Midbrainn) %>% rename("Year0"="MD_Midbrainn") %>%
  inner_join(
    MD_Takeda %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, MD_Midbrainn)
  ) %>% mutate(Delta=100*(MD_Midbrainn-Year0)/Year0) %>%  ungroup() %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta/(TIME_STUDY)),
    sd=sd(Delta/(TIME_STUDY)),
    median=median(Delta/(TIME_STUDY)),
    Q1 = quantile(Delta/(TIME_STUDY), 0.25),
    Q3 = quantile(Delta/(TIME_STUDY), 0.75),
    min=min(Delta/(TIME_STUDY)),
    max=max(Delta/(TIME_STUDY)),
    n=n()
  )




# MD_Pons  *************************************************

MD_Takeda %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, TIME_STUDY, MD_Pons) %>%
  summarise(
    mean=mean(MD_Pons),
    sd=sd(MD_Pons),
    median=median(MD_Pons),
    Q1 = quantile(MD_Pons, 0.25),
    Q3 = quantile(MD_Pons, 0.75),
    min=min(MD_Pons),
    max=max(MD_Pons),
    n=n()
  )



MD_Takeda %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>% 
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, MD_Pons, Elapsed) %>%
  summarise(
    mean=mean(TIME_STUDY*12),
    sd=sd(TIME_STUDY*12),
    median=median(TIME_STUDY*12),
    Q1 = quantile(TIME_STUDY*12, 0.25),
    Q3 = quantile(TIME_STUDY*12, 0.75),
    min=min(TIME_STUDY*12),
    max=max(TIME_STUDY*12),
    n=n()
  )


MD_Takeda %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>% 
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, MD_Pons) %>%
  summarise(
    mean=mean(MD_Pons),
    sd=sd(MD_Pons),
    median=median(MD_Pons),
    Q1 = quantile(MD_Pons, 0.25),
    Q3 = quantile(MD_Pons, 0.75),
    min=min(MD_Pons),
    max=max(MD_Pons),
    n=n()
  )




MD_Takeda %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, MD_Pons) %>% rename("Year0"="MD_Pons") %>%
  inner_join(
    MD_Takeda %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, MD_Pons)
  ) %>% mutate(Delta=MD_Pons-Year0) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta),
    sd=sd(Delta),
    median=median(Delta),
    Q1 = quantile(Delta, 0.25),
    Q3 = quantile(Delta, 0.75),
    min=min(Delta),
    max=max(Delta),
    n=n()
  )




MD_Takeda %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, MD_Pons) %>% rename("Year0"="MD_Pons") %>%
  inner_join(
    MD_Takeda %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, MD_Pons)
  ) %>% mutate(Delta=100*(MD_Pons-Year0)/Year0) %>%  ungroup() %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta),
    sd=sd(Delta),
    median=median(Delta),
    Q1 = quantile(Delta, 0.25),
    Q3 = quantile(Delta, 0.75),
    min=min(Delta),
    max=max(Delta),
    n=n()
  )



MD_Takeda %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, MD_Pons) %>% rename("Year0"="MD_Pons") %>%
  inner_join(
    MD_Takeda %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, MD_Pons)
  ) %>% mutate(Delta=MD_Pons-Year0) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta/(TIME_STUDY)),
    sd=sd(Delta/(TIME_STUDY)),
    median=median(Delta/(TIME_STUDY)),
    Q1 = quantile(Delta/(TIME_STUDY), 0.25),
    Q3 = quantile(Delta/(TIME_STUDY), 0.75),
    min=min(Delta/(TIME_STUDY)),
    max=max(Delta/(TIME_STUDY)),
    n=n()
  )



MD_Takeda %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, MD_Pons) %>% rename("Year0"="MD_Pons") %>%
  inner_join(
    MD_Takeda %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, MD_Pons)
  ) %>% mutate(Delta=100*(MD_Pons-Year0)/Year0) %>%  ungroup() %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta/(TIME_STUDY)),
    sd=sd(Delta/(TIME_STUDY)),
    median=median(Delta/(TIME_STUDY)),
    Q1 = quantile(Delta/(TIME_STUDY), 0.25),
    Q3 = quantile(Delta/(TIME_STUDY), 0.75),
    min=min(Delta/(TIME_STUDY)),
    max=max(Delta/(TIME_STUDY)),
    n=n()
  )








# MD_Medulla  *************************************************

MD_Takeda %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, TIME_STUDY, MD_Medulla) %>%
  summarise(
    mean=mean(MD_Medulla),
    sd=sd(MD_Medulla),
    median=median(MD_Medulla),
    Q1 = quantile(MD_Medulla, 0.25),
    Q3 = quantile(MD_Medulla, 0.75),
    min=min(MD_Medulla),
    max=max(MD_Medulla),
    n=n()
  )



MD_Takeda %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>% 
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, MD_Medulla, Elapsed) %>%
  summarise(
    mean=mean(TIME_STUDY*12),
    sd=sd(TIME_STUDY*12),
    median=median(TIME_STUDY*12),
    Q1 = quantile(TIME_STUDY*12, 0.25),
    Q3 = quantile(TIME_STUDY*12, 0.75),
    min=min(TIME_STUDY*12),
    max=max(TIME_STUDY*12),
    n=n()
  )


MD_Takeda %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>% 
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, MD_Medulla) %>%
  summarise(
    mean=mean(MD_Medulla),
    sd=sd(MD_Medulla),
    median=median(MD_Medulla),
    Q1 = quantile(MD_Medulla, 0.25),
    Q3 = quantile(MD_Medulla, 0.75),
    min=min(MD_Medulla),
    max=max(MD_Medulla),
    n=n()
  )




MD_Takeda %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, MD_Medulla) %>% rename("Year0"="MD_Medulla") %>%
  inner_join(
    MD_Takeda %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, MD_Medulla)
  ) %>% mutate(Delta=MD_Medulla-Year0) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta),
    sd=sd(Delta),
    median=median(Delta),
    Q1 = quantile(Delta, 0.25),
    Q3 = quantile(Delta, 0.75),
    min=min(Delta),
    max=max(Delta),
    n=n()
  )




MD_Takeda %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, MD_Medulla) %>% rename("Year0"="MD_Medulla") %>%
  inner_join(
    MD_Takeda %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, MD_Medulla)
  ) %>% mutate(Delta=100*(MD_Medulla-Year0)/Year0) %>%  ungroup() %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta),
    sd=sd(Delta),
    median=median(Delta),
    Q1 = quantile(Delta, 0.25),
    Q3 = quantile(Delta, 0.75),
    min=min(Delta),
    max=max(Delta),
    n=n()
  )




MD_Takeda %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, MD_Medulla) %>% rename("Year0"="MD_Medulla") %>%
  inner_join(
    MD_Takeda %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, MD_Medulla)
  ) %>% mutate(Delta=MD_Medulla-Year0) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta/(TIME_STUDY)),
    sd=sd(Delta/(TIME_STUDY)),
    median=median(Delta/(TIME_STUDY)),
    Q1 = quantile(Delta/(TIME_STUDY), 0.25),
    Q3 = quantile(Delta/(TIME_STUDY), 0.75),
    min=min(Delta/(TIME_STUDY)),
    max=max(Delta/(TIME_STUDY)),
    n=n()
  )




MD_Takeda %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, MD_Medulla) %>% rename("Year0"="MD_Medulla") %>%
  inner_join(
    MD_Takeda %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, MD_Medulla)
  ) %>% mutate(Delta=100*(MD_Medulla-Year0)/Year0) %>%  ungroup() %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta/(TIME_STUDY)),
    sd=sd(Delta/(TIME_STUDY)),
    median=median(Delta/(TIME_STUDY)),
    Q1 = quantile(Delta/(TIME_STUDY), 0.25),
    Q3 = quantile(Delta/(TIME_STUDY), 0.75),
    min=min(Delta/(TIME_STUDY)),
    max=max(Delta/(TIME_STUDY)),
    n=n()
  )

# -----------
# FRACTIONAL ANISOTROPY ------------------------


FA_Takeda <- readxl::read_xlsx("Source/FA_Takeda.xlsx")
FA_Takeda$DateIRM <-  as.Date(FA_Takeda$DateIRM)
names(FA_Takeda)

FA_Takeda <- FA_Takeda %>% select(NUM, DateIRM,
                                  FA_L_Puta, 
                                  FA_R_Puta, 
                                  FA_L_Cerebell_WM,FA_R_Cerebell_WM,
                                  FA_L_Cerebell_GM, FA_R_Cerebell_GM,
                                  FA_Midbrainn,
                                  FA_Pons,
                                  FA_Medulla)

FA_Takeda <- FA_Takeda %>% arrange(NUM, DateIRM) %>% group_by(NUM) %>%
  mutate(TIME_STUDY=time_length(difftime(DateIRM, lag(DateIRM) ), "years"))  %>%
  select(NUM, DateIRM, TIME_STUDY,
         FA_L_Puta, 
         FA_R_Puta, 
         FA_L_Cerebell_WM,FA_R_Cerebell_WM,
         FA_L_Cerebell_GM, FA_R_Cerebell_GM,
         FA_Midbrainn,
         FA_Pons,
         FA_Medulla)


FA_Takeda <- FA_Takeda %>% mutate(TIME_STUDY=ifelse(is.na(TIME_STUDY), 0, TIME_STUDY))

FA_Takeda <- FA_Takeda %>% mutate(TIME_STUDY=cumsum(TIME_STUDY))

FA_Takeda <- FA_Takeda %>% group_by(NUM) %>% count() %>%
  left_join(FA_Takeda) %>%
  select(NUM, DateIRM, TIME_STUDY, n,
         FA_L_Puta, 
         FA_R_Puta, 
         FA_L_Cerebell_WM,FA_R_Cerebell_WM,
         FA_L_Cerebell_GM, FA_R_Cerebell_GM,
         FA_Midbrainn,
         FA_Pons,
         FA_Medulla)


length(unique(FA_Takeda$NUM)) # 24

FA_Takeda %>% ungroup() %>% group_by(n) %>% count()

FA_Takeda %>% mutate(Year= ifelse(TIME_STUDY<=0.5, 0,
                                  ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                                         ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                                ifelse(TIME_STUDY>=2.5 ,3, NA))))) %>%
  select(NUM, Year) %>% group_by(NUM) %>%
  mutate(All=paste0(Year, collapse=","))

range(FA_Takeda$TIME_STUDY)


FA_Takeda <- FA_Takeda %>% mutate(CerebellumWhite=(FA_L_Cerebell_WM+FA_R_Cerebell_WM)/2) %>%
  mutate(CerebellumGray=(FA_L_Cerebell_GM+FA_R_Cerebell_GM)/2) %>%
  mutate(CerebellumTotal=0.82*CerebellumGray+0.18*CerebellumWhite)




# PLOTS **************************************

FA_Takeda %>% select(NUM, n) %>% distinct() %>%
  group_by(n) %>% count() %>%
  ggplot(aes(n, nn)) +
  geom_col(fill="black", colour="black") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  xlab("\n Number of MRIs per patient") +
  ylab("\n Number of Patients \n") +
  geom_text(aes(label = nn), vjust = -0.5)


FA_Takeda %>% 
  select(NUM, TIME_STUDY, FA_L_Puta) %>% ungroup() %>%
  ggplot(aes(TIME_STUDY, FA_L_Puta)) +
  geom_line(aes(group=NUM), col="black" , linewidth=0.5, alpha=0.4) +
  geom_jitter(size=1, colour="firebrick", alpha=0.5, shape=1, stroke=1) +
  theme_minimal() +
  xlab("\n Elapsed number of years since study enrollment \n [All available patient records up to year 5]") +
  ylab("Left Putamen Fractional Anisotropy \n At each MRI \n") +
  scale_color_manual(values=palette) +
  scale_fill_manual(values=palette) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_x_continuous(breaks = seq(0, 5, by = 0.5)) +
  ylim(0,0.4)


FA_Takeda %>% 
  select(NUM, TIME_STUDY, FA_R_Puta) %>% ungroup() %>%
  ggplot(aes(TIME_STUDY, FA_R_Puta)) +
  geom_line(aes(group=NUM), col="black" , linewidth=0.5, alpha=0.4) +
  geom_jitter(size=1, colour="firebrick", alpha=0.5, shape=1, stroke=1) +
  theme_minimal() +
  xlab("\n Elapsed number of years since study enrollment \n [All available patient records up to year 5]") +
  ylab("Right Putamen Fractional Anisotropy \n At each MRI \n") +
  scale_color_manual(values=palette) +
  scale_fill_manual(values=palette) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_x_continuous(breaks = seq(0, 5, by = 0.5)) +
  ylim(0,0.4)


FA_Takeda %>% 
  select(NUM, TIME_STUDY, CerebellumWhite) %>% ungroup() %>%
  ggplot(aes(TIME_STUDY, CerebellumWhite)) +
  geom_line(aes(group=NUM), col="black" , linewidth=0.5, alpha=0.4) +
  geom_jitter(size=1, colour="firebrick", alpha=0.5, shape=1, stroke=1) +
  theme_minimal() +
  xlab("\n Elapsed number of years since study enrollment \n [All available patient records up to year 5]") +
  ylab("Cerebellum White Fractional Anisotropy \n At each MRI \n") +
  scale_color_manual(values=palette) +
  scale_fill_manual(values=palette) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_x_continuous(breaks = seq(0, 5, by = 0.5)) +
  ylim(0,0.4)


FA_Takeda %>% 
  select(NUM, TIME_STUDY, CerebellumTotal) %>% ungroup() %>%
  ggplot(aes(TIME_STUDY, CerebellumTotal)) +
  geom_line(aes(group=NUM), col="black" , linewidth=0.5, alpha=0.4) +
  geom_jitter(size=1, colour="firebrick", alpha=0.5, shape=1, stroke=1) +
  theme_minimal() +
  xlab("\n Elapsed number of years since study enrollment \n [All available patient records up to year 5]") +
  ylab("Cerebellum Total Fractional Anisotropy  \n At each MRI \n") +
  scale_color_manual(values=palette) +
  scale_fill_manual(values=palette) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_x_continuous(breaks = seq(0, 5, by = 0.5)) +
  ylim(0,0.25)


FA_Takeda %>% 
  select(NUM, TIME_STUDY, FA_Midbrainn) %>% ungroup() %>%
  ggplot(aes(TIME_STUDY, FA_Midbrainn)) +
  geom_line(aes(group=NUM), col="black" , linewidth=0.5, alpha=0.4) +
  geom_jitter(size=1, colour="firebrick", alpha=0.5, shape=1, stroke=1) +
  theme_minimal() +
  xlab("\n Elapsed number of years since study enrollment \n [All available patient records up to year 5]") +
  ylab("Midbrain Fractional Anisotropy \n At each MRI \n") +
  scale_color_manual(values=palette) +
  scale_fill_manual(values=palette) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_x_continuous(breaks = seq(0, 5, by = 0.5)) +
  ylim(0,0.45)


FA_Takeda %>% 
  select(NUM, TIME_STUDY, FA_Pons) %>% ungroup() %>%
  ggplot(aes(TIME_STUDY, FA_Pons)) +
  geom_line(aes(group=NUM), col="black" , linewidth=0.5, alpha=0.4) +
  geom_jitter(size=1, colour="firebrick", alpha=0.5, shape=1, stroke=1) +
  theme_minimal() +
  xlab("\n Elapsed number of years since study enrollment \n [All available patient records up to year 5]") +
  ylab("Pons Fractional Anisotropy \n At each MRI \n") +
  scale_color_manual(values=palette) +
  scale_fill_manual(values=palette) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_x_continuous(breaks = seq(0, 5, by = 0.5)) +
  ylim(0,0.5)


FA_Takeda %>% 
  select(NUM, TIME_STUDY, FA_Medulla) %>% ungroup() %>%
  ggplot(aes(TIME_STUDY, FA_Medulla)) +
  geom_line(aes(group=NUM), col="black" , linewidth=0.5, alpha=0.4) +
  geom_jitter(size=1, colour="firebrick", alpha=0.5, shape=1, stroke=1) +
  theme_minimal() +
  xlab("\n Elapsed number of years since study enrollment \n [All available patient records up to year 5]") +
  ylab("Medulla Fractional Anisotropy \n At each MRI \n") +
  scale_color_manual(values=palette) +
  scale_fill_manual(values=palette) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_x_continuous(breaks = seq(0, 5, by = 0.5)) +
  ylim(0,0.45)

# END of PLOTS ****************************


# ORIGINAL Fractional Anisotropy ************************


# FA_L_Puta *************************************

FA_Takeda %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, TIME_STUDY, FA_L_Puta) %>%
  summarise(
    mean=mean(FA_L_Puta),
    sd=sd(FA_L_Puta),
    median=median(FA_L_Puta),
    Q1 = quantile(FA_L_Puta, 0.25),
    Q3 = quantile(FA_L_Puta, 0.75),
    min=min(FA_L_Puta),
    max=max(FA_L_Puta),
    n=n()
  )



FA_Takeda %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>% 
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, FA_L_Puta, Elapsed) %>%
  summarise(
    mean=mean(TIME_STUDY*12),
    sd=sd(TIME_STUDY*12),
    median=median(TIME_STUDY*12),
    Q1 = quantile(TIME_STUDY*12, 0.25),
    Q3 = quantile(TIME_STUDY*12, 0.75),
    min=min(TIME_STUDY*12),
    max=max(TIME_STUDY*12),
    n=n()
  )


FA_Takeda %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>% 
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, FA_L_Puta) %>%
  summarise(
    mean=mean(FA_L_Puta),
    sd=sd(FA_L_Puta),
    median=median(FA_L_Puta),
    Q1 = quantile(FA_L_Puta, 0.25),
    Q3 = quantile(FA_L_Puta, 0.75),
    min=min(FA_L_Puta),
    max=max(FA_L_Puta),
    n=n()
  )




FA_Takeda %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, FA_L_Puta) %>% rename("Year0"="FA_L_Puta") %>%
  inner_join(
    FA_Takeda %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, FA_L_Puta)
  ) %>% mutate(Delta=FA_L_Puta-Year0) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta),
    sd=sd(Delta),
    median=median(Delta),
    Q1 = quantile(Delta, 0.25),
    Q3 = quantile(Delta, 0.75),
    min=min(Delta),
    max=max(Delta),
    n=n()
  )



FA_Takeda %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, FA_L_Puta) %>% rename("Year0"="FA_L_Puta") %>%
  inner_join(
    FA_Takeda %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, FA_L_Puta)
  ) %>% mutate(Delta=100*(FA_L_Puta-Year0)/Year0) %>%  ungroup() %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta),
    sd=sd(Delta),
    median=median(Delta),
    Q1 = quantile(Delta, 0.25),
    Q3 = quantile(Delta, 0.75),
    min=min(Delta),
    max=max(Delta),
    n=n()
  )



FA_Takeda %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, FA_L_Puta) %>% rename("Year0"="FA_L_Puta") %>%
  inner_join(
    FA_Takeda %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, FA_L_Puta)
  ) %>% mutate(Delta=FA_L_Puta-Year0) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta/(TIME_STUDY)),
    sd=sd(Delta/(TIME_STUDY)),
    median=median(Delta/(TIME_STUDY)),
    Q1 = quantile(Delta/(TIME_STUDY), 0.25),
    Q3 = quantile(Delta/(TIME_STUDY), 0.75),
    min=min(Delta/(TIME_STUDY)),
    max=max(Delta/(TIME_STUDY)),
    n=n()
  )





FA_Takeda %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, FA_L_Puta) %>% rename("Year0"="FA_L_Puta") %>%
  inner_join(
    FA_Takeda %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, FA_L_Puta)
  ) %>% mutate(Delta=100*(FA_L_Puta-Year0)/Year0) %>%  ungroup() %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta/(TIME_STUDY)),
    sd=sd(Delta/(TIME_STUDY)),
    median=median(Delta/(TIME_STUDY)),
    Q1 = quantile(Delta/(TIME_STUDY), 0.25),
    Q3 = quantile(Delta/(TIME_STUDY), 0.75),
    min=min(Delta/(TIME_STUDY)),
    max=max(Delta/(TIME_STUDY)),
    n=n()
  )









# FA_R_Puta *************************************************

FA_Takeda %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, TIME_STUDY, FA_R_Puta) %>%
  summarise(
    mean=mean(FA_R_Puta),
    sd=sd(FA_R_Puta),
    median=median(FA_R_Puta),
    Q1 = quantile(FA_R_Puta, 0.25),
    Q3 = quantile(FA_R_Puta, 0.75),
    min=min(FA_R_Puta),
    max=max(FA_R_Puta),
    n=n()
  )



FA_Takeda %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>% 
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, FA_R_Puta, Elapsed) %>%
  summarise(
    mean=mean(TIME_STUDY*12),
    sd=sd(TIME_STUDY*12),
    median=median(TIME_STUDY*12),
    Q1 = quantile(TIME_STUDY*12, 0.25),
    Q3 = quantile(TIME_STUDY*12, 0.75),
    min=min(TIME_STUDY*12),
    max=max(TIME_STUDY*12),
    n=n()
  )


FA_Takeda %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>% 
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, FA_R_Puta) %>%
  summarise(
    mean=mean(FA_R_Puta),
    sd=sd(FA_R_Puta),
    median=median(FA_R_Puta),
    Q1 = quantile(FA_R_Puta, 0.25),
    Q3 = quantile(FA_R_Puta, 0.75),
    min=min(FA_R_Puta),
    max=max(FA_R_Puta),
    n=n()
  )




FA_Takeda %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, FA_R_Puta) %>% rename("Year0"="FA_R_Puta") %>%
  inner_join(
    FA_Takeda %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, FA_R_Puta)
  ) %>% mutate(Delta=FA_R_Puta-Year0) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta),
    sd=sd(Delta),
    median=median(Delta),
    Q1 = quantile(Delta, 0.25),
    Q3 = quantile(Delta, 0.75),
    min=min(Delta),
    max=max(Delta),
    n=n()
  )



FA_Takeda %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, FA_R_Puta) %>% rename("Year0"="FA_R_Puta") %>%
  inner_join(
    FA_Takeda %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, FA_R_Puta)
  ) %>% mutate(Delta=100*(FA_R_Puta-Year0)/Year0) %>%  ungroup() %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta),
    sd=sd(Delta),
    median=median(Delta),
    Q1 = quantile(Delta, 0.25),
    Q3 = quantile(Delta, 0.75),
    min=min(Delta),
    max=max(Delta),
    n=n()
  )



FA_Takeda %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, FA_R_Puta) %>% rename("Year0"="FA_R_Puta") %>%
  inner_join(
    FA_Takeda %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, FA_R_Puta)
  ) %>% mutate(Delta=FA_R_Puta-Year0) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta/(TIME_STUDY)),
    sd=sd(Delta/(TIME_STUDY)),
    median=median(Delta/(TIME_STUDY)),
    Q1 = quantile(Delta/(TIME_STUDY), 0.25),
    Q3 = quantile(Delta/(TIME_STUDY), 0.75),
    min=min(Delta/(TIME_STUDY)),
    max=max(Delta/(TIME_STUDY)),
    n=n()
  )



FA_Takeda %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, FA_R_Puta) %>% rename("Year0"="FA_R_Puta") %>%
  inner_join(
    FA_Takeda %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, FA_R_Puta)
  ) %>% mutate(Delta=100*(FA_R_Puta-Year0)/Year0) %>%  ungroup() %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta/(TIME_STUDY)),
    sd=sd(Delta/(TIME_STUDY)),
    median=median(Delta/(TIME_STUDY)),
    Q1 = quantile(Delta/(TIME_STUDY), 0.25),
    Q3 = quantile(Delta/(TIME_STUDY), 0.75),
    min=min(Delta/(TIME_STUDY)),
    max=max(Delta/(TIME_STUDY)),
    n=n()
  )





# Cerebellum White *************************************************

FA_Takeda %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, TIME_STUDY, CerebellumWhite) %>%
  summarise(
    mean=mean(CerebellumWhite),
    sd=sd(CerebellumWhite),
    median=median(CerebellumWhite),
    Q1 = quantile(CerebellumWhite, 0.25),
    Q3 = quantile(CerebellumWhite, 0.75),
    min=min(CerebellumWhite),
    max=max(CerebellumWhite),
    n=n()
  )



FA_Takeda %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>% 
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, CerebellumWhite, Elapsed) %>%
  summarise(
    mean=mean(TIME_STUDY*12),
    sd=sd(TIME_STUDY*12),
    median=median(TIME_STUDY*12),
    Q1 = quantile(TIME_STUDY*12, 0.25),
    Q3 = quantile(TIME_STUDY*12, 0.75),
    min=min(TIME_STUDY*12),
    max=max(TIME_STUDY*12),
    n=n()
  )


FA_Takeda %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>% 
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, CerebellumWhite) %>%
  summarise(
    mean=mean(CerebellumWhite),
    sd=sd(CerebellumWhite),
    median=median(CerebellumWhite),
    Q1 = quantile(CerebellumWhite, 0.25),
    Q3 = quantile(CerebellumWhite, 0.75),
    min=min(CerebellumWhite),
    max=max(CerebellumWhite),
    n=n()
  )




FA_Takeda %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, CerebellumWhite) %>% rename("Year0"="CerebellumWhite") %>%
  inner_join(
    FA_Takeda %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, CerebellumWhite)
  ) %>% mutate(Delta=CerebellumWhite-Year0) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta),
    sd=sd(Delta),
    median=median(Delta),
    Q1 = quantile(Delta, 0.25),
    Q3 = quantile(Delta, 0.75),
    min=min(Delta),
    max=max(Delta),
    n=n()
  )


FA_Takeda %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, CerebellumWhite) %>% rename("Year0"="CerebellumWhite") %>%
  inner_join(
    FA_Takeda %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, CerebellumWhite)
  ) %>% mutate(Delta=100*(CerebellumWhite-Year0)/Year0) %>%  ungroup() %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta),
    sd=sd(Delta),
    median=median(Delta),
    Q1 = quantile(Delta, 0.25),
    Q3 = quantile(Delta, 0.75),
    min=min(Delta),
    max=max(Delta),
    n=n()
  )



FA_Takeda %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, CerebellumWhite) %>% rename("Year0"="CerebellumWhite") %>%
  inner_join(
    FA_Takeda %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, CerebellumWhite)
  ) %>% mutate(Delta=CerebellumWhite-Year0) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta/(TIME_STUDY)),
    sd=sd(Delta/(TIME_STUDY)),
    median=median(Delta/(TIME_STUDY)),
    Q1 = quantile(Delta/(TIME_STUDY), 0.25),
    Q3 = quantile(Delta/(TIME_STUDY), 0.75),
    min=min(Delta/(TIME_STUDY)),
    max=max(Delta/(TIME_STUDY)),
    n=n()
  )



FA_Takeda %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, CerebellumWhite) %>% rename("Year0"="CerebellumWhite") %>%
  inner_join(
    FA_Takeda %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, CerebellumWhite)
  ) %>% mutate(Delta=100*(CerebellumWhite-Year0)/Year0) %>%  ungroup() %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta/(TIME_STUDY)),
    sd=sd(Delta/(TIME_STUDY)),
    median=median(Delta/(TIME_STUDY)),
    Q1 = quantile(Delta/(TIME_STUDY), 0.25),
    Q3 = quantile(Delta/(TIME_STUDY), 0.75),
    min=min(Delta/(TIME_STUDY)),
    max=max(Delta/(TIME_STUDY)),
    n=n()
  )



# Cerebellum Total *************************************************


FA_Takeda %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, TIME_STUDY, CerebellumTotal) %>%
  summarise(
    mean=mean(CerebellumTotal),
    sd=sd(CerebellumTotal),
    median=median(CerebellumTotal),
    Q1 = quantile(CerebellumTotal, 0.25),
    Q3 = quantile(CerebellumTotal, 0.75),
    min=min(CerebellumTotal),
    max=max(CerebellumTotal),
    n=n()
  )



FA_Takeda %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>% 
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, CerebellumTotal, Elapsed) %>%
  summarise(
    mean=mean(TIME_STUDY*12),
    sd=sd(TIME_STUDY*12),
    median=median(TIME_STUDY*12),
    Q1 = quantile(TIME_STUDY*12, 0.25),
    Q3 = quantile(TIME_STUDY*12, 0.75),
    min=min(TIME_STUDY*12),
    max=max(TIME_STUDY*12),
    n=n()
  )


FA_Takeda %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>% 
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, CerebellumTotal) %>%
  summarise(
    mean=mean(CerebellumTotal),
    sd=sd(CerebellumTotal),
    median=median(CerebellumTotal),
    Q1 = quantile(CerebellumTotal, 0.25),
    Q3 = quantile(CerebellumTotal, 0.75),
    min=min(CerebellumTotal),
    max=max(CerebellumTotal),
    n=n()
  )




FA_Takeda %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, CerebellumTotal) %>% rename("Year0"="CerebellumTotal") %>%
  inner_join(
    FA_Takeda %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, CerebellumTotal)
  ) %>% mutate(Delta=CerebellumTotal-Year0) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta),
    sd=sd(Delta),
    median=median(Delta),
    Q1 = quantile(Delta, 0.25),
    Q3 = quantile(Delta, 0.75),
    min=min(Delta),
    max=max(Delta),
    n=n()
  )


FA_Takeda %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, CerebellumTotal) %>% rename("Year0"="CerebellumTotal") %>%
  inner_join(
    FA_Takeda %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, CerebellumTotal)
  ) %>% mutate(Delta=100*(CerebellumTotal-Year0)/Year0) %>%  ungroup() %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta),
    sd=sd(Delta),
    median=median(Delta),
    Q1 = quantile(Delta, 0.25),
    Q3 = quantile(Delta, 0.75),
    min=min(Delta),
    max=max(Delta),
    n=n()
  )



FA_Takeda %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, CerebellumTotal) %>% rename("Year0"="CerebellumTotal") %>%
  inner_join(
    FA_Takeda %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, CerebellumTotal)
  ) %>% mutate(Delta=CerebellumTotal-Year0) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta/(TIME_STUDY)),
    sd=sd(Delta/(TIME_STUDY)),
    median=median(Delta/(TIME_STUDY)),
    Q1 = quantile(Delta/(TIME_STUDY), 0.25),
    Q3 = quantile(Delta/(TIME_STUDY), 0.75),
    min=min(Delta/(TIME_STUDY)),
    max=max(Delta/(TIME_STUDY)),
    n=n()
  )



FA_Takeda %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, CerebellumTotal) %>% rename("Year0"="CerebellumTotal") %>%
  inner_join(
    FA_Takeda %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, CerebellumTotal)
  ) %>% mutate(Delta=100*(CerebellumTotal-Year0)/Year0) %>%  ungroup() %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta/(TIME_STUDY)),
    sd=sd(Delta/(TIME_STUDY)),
    median=median(Delta/(TIME_STUDY)),
    Q1 = quantile(Delta/(TIME_STUDY), 0.25),
    Q3 = quantile(Delta/(TIME_STUDY), 0.75),
    min=min(Delta/(TIME_STUDY)),
    max=max(Delta/(TIME_STUDY)),
    n=n()
  )



# FA_Midbrainn  *************************************************

FA_Takeda %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, TIME_STUDY, FA_Midbrainn) %>%
  summarise(
    mean=mean(FA_Midbrainn),
    sd=sd(FA_Midbrainn),
    median=median(FA_Midbrainn),
    Q1 = quantile(FA_Midbrainn, 0.25),
    Q3 = quantile(FA_Midbrainn, 0.75),
    min=min(FA_Midbrainn),
    max=max(FA_Midbrainn),
    n=n()
  )



FA_Takeda %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>% 
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, FA_Midbrainn, Elapsed) %>%
  summarise(
    mean=mean(TIME_STUDY*12),
    sd=sd(TIME_STUDY*12),
    median=median(TIME_STUDY*12),
    Q1 = quantile(TIME_STUDY*12, 0.25),
    Q3 = quantile(TIME_STUDY*12, 0.75),
    min=min(TIME_STUDY*12),
    max=max(TIME_STUDY*12),
    n=n()
  )


FA_Takeda %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>% 
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, FA_Midbrainn) %>%
  summarise(
    mean=mean(FA_Midbrainn),
    sd=sd(FA_Midbrainn),
    median=median(FA_Midbrainn),
    Q1 = quantile(FA_Midbrainn, 0.25),
    Q3 = quantile(FA_Midbrainn, 0.75),
    min=min(FA_Midbrainn),
    max=max(FA_Midbrainn),
    n=n()
  )




FA_Takeda %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, FA_Midbrainn) %>% rename("Year0"="FA_Midbrainn") %>%
  inner_join(
    FA_Takeda %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, FA_Midbrainn)
  ) %>% mutate(Delta=FA_Midbrainn-Year0) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta),
    sd=sd(Delta),
    median=median(Delta),
    Q1 = quantile(Delta, 0.25),
    Q3 = quantile(Delta, 0.75),
    min=min(Delta),
    max=max(Delta),
    n=n()
  )


FA_Takeda %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, FA_Midbrainn) %>% rename("Year0"="FA_Midbrainn") %>%
  inner_join(
    FA_Takeda %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, FA_Midbrainn)
  ) %>% mutate(Delta=100*(FA_Midbrainn-Year0)/Year0) %>%  ungroup() %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta),
    sd=sd(Delta),
    median=median(Delta),
    Q1 = quantile(Delta, 0.25),
    Q3 = quantile(Delta, 0.75),
    min=min(Delta),
    max=max(Delta),
    n=n()
  )





FA_Takeda %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, FA_Midbrainn) %>% rename("Year0"="FA_Midbrainn") %>%
  inner_join(
    FA_Takeda %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, FA_Midbrainn)
  ) %>% mutate(Delta=FA_Midbrainn-Year0) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta/(TIME_STUDY)),
    sd=sd(Delta/(TIME_STUDY)),
    median=median(Delta/(TIME_STUDY)),
    Q1 = quantile(Delta/(TIME_STUDY), 0.25),
    Q3 = quantile(Delta/(TIME_STUDY), 0.75),
    min=min(Delta/(TIME_STUDY)),
    max=max(Delta/(TIME_STUDY)),
    n=n()
  )




FA_Takeda %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, FA_Midbrainn) %>% rename("Year0"="FA_Midbrainn") %>%
  inner_join(
    FA_Takeda %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, FA_Midbrainn)
  ) %>% mutate(Delta=100*(FA_Midbrainn-Year0)/Year0) %>%  ungroup() %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta/(TIME_STUDY)),
    sd=sd(Delta/(TIME_STUDY)),
    median=median(Delta/(TIME_STUDY)),
    Q1 = quantile(Delta/(TIME_STUDY), 0.25),
    Q3 = quantile(Delta/(TIME_STUDY), 0.75),
    min=min(Delta/(TIME_STUDY)),
    max=max(Delta/(TIME_STUDY)),
    n=n()
  )



# FA_Pons  *************************************************

FA_Takeda %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, TIME_STUDY, FA_Pons) %>%
  summarise(
    mean=mean(FA_Pons),
    sd=sd(FA_Pons),
    median=median(FA_Pons),
    Q1 = quantile(FA_Pons, 0.25),
    Q3 = quantile(FA_Pons, 0.75),
    min=min(FA_Pons),
    max=max(FA_Pons),
    n=n()
  )



FA_Takeda %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>% 
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, FA_Pons, Elapsed) %>%
  summarise(
    mean=mean(TIME_STUDY*12),
    sd=sd(TIME_STUDY*12),
    median=median(TIME_STUDY*12),
    Q1 = quantile(TIME_STUDY*12, 0.25),
    Q3 = quantile(TIME_STUDY*12, 0.75),
    min=min(TIME_STUDY*12),
    max=max(TIME_STUDY*12),
    n=n()
  )


FA_Takeda %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>% 
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, FA_Pons) %>%
  summarise(
    mean=mean(FA_Pons),
    sd=sd(FA_Pons),
    median=median(FA_Pons),
    Q1 = quantile(FA_Pons, 0.25),
    Q3 = quantile(FA_Pons, 0.75),
    min=min(FA_Pons),
    max=max(FA_Pons),
    n=n()
  )




FA_Takeda %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, FA_Pons) %>% rename("Year0"="FA_Pons") %>%
  inner_join(
    FA_Takeda %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, FA_Pons)
  ) %>% mutate(Delta=FA_Pons-Year0) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta),
    sd=sd(Delta),
    median=median(Delta),
    Q1 = quantile(Delta, 0.25),
    Q3 = quantile(Delta, 0.75),
    min=min(Delta),
    max=max(Delta),
    n=n()
  )




FA_Takeda %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, FA_Pons) %>% rename("Year0"="FA_Pons") %>%
  inner_join(
    FA_Takeda %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, FA_Pons)
  ) %>% mutate(Delta=100*(FA_Pons-Year0)/Year0) %>%  ungroup() %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta),
    sd=sd(Delta),
    median=median(Delta),
    Q1 = quantile(Delta, 0.25),
    Q3 = quantile(Delta, 0.75),
    min=min(Delta),
    max=max(Delta),
    n=n()
  )



FA_Takeda %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, FA_Pons) %>% rename("Year0"="FA_Pons") %>%
  inner_join(
    FA_Takeda %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, FA_Pons)
  ) %>% mutate(Delta=FA_Pons-Year0) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta/(TIME_STUDY)),
    sd=sd(Delta/(TIME_STUDY)),
    median=median(Delta/(TIME_STUDY)),
    Q1 = quantile(Delta/(TIME_STUDY), 0.25),
    Q3 = quantile(Delta/(TIME_STUDY), 0.75),
    min=min(Delta/(TIME_STUDY)),
    max=max(Delta/(TIME_STUDY)),
    n=n()
  )


FA_Takeda %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, FA_Pons) %>% rename("Year0"="FA_Pons") %>%
  inner_join(
    FA_Takeda %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, FA_Pons)
  ) %>% mutate(Delta=100*(FA_Pons-Year0)/Year0) %>%  ungroup() %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta/(TIME_STUDY)),
    sd=sd(Delta/(TIME_STUDY)),
    median=median(Delta/(TIME_STUDY)),
    Q1 = quantile(Delta/(TIME_STUDY), 0.25),
    Q3 = quantile(Delta/(TIME_STUDY), 0.75),
    min=min(Delta/(TIME_STUDY)),
    max=max(Delta/(TIME_STUDY)),
    n=n()
  )






# FA_Medulla  *************************************************

FA_Takeda %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, TIME_STUDY, FA_Medulla) %>%
  summarise(
    mean=mean(FA_Medulla),
    sd=sd(FA_Medulla),
    median=median(FA_Medulla),
    Q1 = quantile(FA_Medulla, 0.25),
    Q3 = quantile(FA_Medulla, 0.75),
    min=min(FA_Medulla),
    max=max(FA_Medulla),
    n=n()
  )



FA_Takeda %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>% 
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, FA_Medulla, Elapsed) %>%
  summarise(
    mean=mean(TIME_STUDY*12),
    sd=sd(TIME_STUDY*12),
    median=median(TIME_STUDY*12),
    Q1 = quantile(TIME_STUDY*12, 0.25),
    Q3 = quantile(TIME_STUDY*12, 0.75),
    min=min(TIME_STUDY*12),
    max=max(TIME_STUDY*12),
    n=n()
  )


FA_Takeda %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>% 
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, FA_Medulla) %>%
  summarise(
    mean=mean(FA_Medulla),
    sd=sd(FA_Medulla),
    median=median(FA_Medulla),
    Q1 = quantile(FA_Medulla, 0.25),
    Q3 = quantile(FA_Medulla, 0.75),
    min=min(FA_Medulla),
    max=max(FA_Medulla),
    n=n()
  )




FA_Takeda %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, FA_Medulla) %>% rename("Year0"="FA_Medulla") %>%
  inner_join(
    FA_Takeda %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, FA_Medulla)
  ) %>% mutate(Delta=FA_Medulla-Year0) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta),
    sd=sd(Delta),
    median=median(Delta),
    Q1 = quantile(Delta, 0.25),
    Q3 = quantile(Delta, 0.75),
    min=min(Delta),
    max=max(Delta),
    n=n()
  )



FA_Takeda %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, FA_Medulla) %>% rename("Year0"="FA_Medulla") %>%
  inner_join(
    FA_Takeda %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, FA_Medulla)
  ) %>% mutate(Delta=100*(FA_Medulla-Year0)/Year0) %>%  ungroup() %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta),
    sd=sd(Delta),
    median=median(Delta),
    Q1 = quantile(Delta, 0.25),
    Q3 = quantile(Delta, 0.75),
    min=min(Delta),
    max=max(Delta),
    n=n()
  )




FA_Takeda %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, FA_Medulla) %>% rename("Year0"="FA_Medulla") %>%
  inner_join(
    FA_Takeda %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, FA_Medulla)
  ) %>% mutate(Delta=FA_Medulla-Year0) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta/(TIME_STUDY)),
    sd=sd(Delta/(TIME_STUDY)),
    median=median(Delta/(TIME_STUDY)),
    Q1 = quantile(Delta/(TIME_STUDY), 0.25),
    Q3 = quantile(Delta/(TIME_STUDY), 0.75),
    min=min(Delta/(TIME_STUDY)),
    max=max(Delta/(TIME_STUDY)),
    n=n()
  )



FA_Takeda %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, FA_Medulla) %>% rename("Year0"="FA_Medulla") %>%
  inner_join(
    FA_Takeda %>%
      group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(NUM, TIME_STUDY, FA_Medulla)
  ) %>% mutate(Delta=100*(FA_Medulla-Year0)/Year0) %>%  ungroup() %>%
  ungroup() %>%
  select(NUM, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta/(TIME_STUDY)),
    sd=sd(Delta/(TIME_STUDY)),
    median=median(Delta/(TIME_STUDY)),
    Q1 = quantile(Delta/(TIME_STUDY), 0.25),
    Q3 = quantile(Delta/(TIME_STUDY), 0.75),
    min=min(Delta/(TIME_STUDY)),
    max=max(Delta/(TIME_STUDY)),
    n=n()
  )


# -----------
# CORRELATION INTERNAL BASELINE -----------------------------

# VOLUMETRIC ORIGINAL ***********************

irmForPaulo <- readRDS("Source/irmForPaulo.rds")
names(irmForPaulo)

irmForPaulo <- irmForPaulo %>% select(NUM, DateIRM,
                                      Left_Putamen, 
                                      Right_Putamen, 
                                      Left_Cerebellum_White_Matter,Right_Cerebellum_White_Matter,
                                      Left_Cerebellum_Cortex, Right_Cerebellum_Cortex,
                                      Midbrain,
                                      Pons,
                                      Medulla,
                                      CerebralWhiteMatterVol, TotalGrayVol)

irmForPaulo <- irmForPaulo %>% arrange(NUM, DateIRM) %>% group_by(NUM) %>%
  mutate(TIME_STUDY=time_length(difftime(DateIRM, lag(DateIRM) ), "years"))  %>%
  select(NUM, DateIRM, TIME_STUDY,
         Left_Putamen, 
         Right_Putamen, 
         Left_Cerebellum_White_Matter,Right_Cerebellum_White_Matter,
         Left_Cerebellum_Cortex, Right_Cerebellum_Cortex,
         Midbrain,
         Pons,
         Medulla,
         CerebralWhiteMatterVol, TotalGrayVol)


irmForPaulo <- irmForPaulo %>% mutate(TIME_STUDY=ifelse(is.na(TIME_STUDY), 0, TIME_STUDY))

irmForPaulo <- irmForPaulo %>% mutate(TIME_STUDY=cumsum(TIME_STUDY))

irmForPaulo <- irmForPaulo %>% group_by(NUM) %>% count() %>%
  left_join(irmForPaulo) %>%
  select(NUM, DateIRM, TIME_STUDY, n,
         Left_Putamen, 
         Right_Putamen, 
         Left_Cerebellum_White_Matter,Right_Cerebellum_White_Matter,
         Left_Cerebellum_Cortex, Right_Cerebellum_Cortex,
         Midbrain,
         Pons,
         Medulla,
         CerebralWhiteMatterVol, TotalGrayVol)

irmForPaulo$Medulla <- as.numeric(irmForPaulo$Medulla)
irmForPaulo$Midbrain <- as.numeric(irmForPaulo$Midbrain)
irmForPaulo$CerebralWhiteMatterVol <- as.numeric(irmForPaulo$CerebralWhiteMatterVol)

irmForPaulo <- irmForPaulo %>% mutate(CerebellumTotal=Left_Cerebellum_White_Matter+
                                        Right_Cerebellum_White_Matter+
                                        Left_Cerebellum_Cortex +
                                        Right_Cerebellum_Cortex ) %>%
  mutate(CerebellumWhite=Left_Cerebellum_White_Matter+Right_Cerebellum_White_Matter) %>%
  mutate(Total=as.numeric(CerebralWhiteMatterVol)+as.numeric(TotalGrayVol)) 


irmForPaulo <- irmForPaulo %>% ungroup() %>%
  filter(TIME_STUDY==0) %>% select(-c(NUM, DateIRM, TIME_STUDY, n, CerebralWhiteMatterVol, TotalGrayVol,
                                      Right_Cerebellum_Cortex, Right_Cerebellum_White_Matter, Left_Cerebellum_Cortex, Left_Cerebellum_White_Matter))


Correlations <- irmForPaulo %>% cor(method = "spearman")

round(Correlations, 3)

get_p_value <- function(x, y) {
  cor_test <- cor.test(x, y, method = "spearman")
  broom::tidy(cor_test)$p.value
}

p_value_matrix <- matrix(NA, ncol = 8, nrow = 8, 
                         dimnames = list(names(irmForPaulo), names(irmForPaulo)))


for (i in 1:ncol(irmForPaulo)) {
  for (j in 1:ncol(irmForPaulo)) {
    p_value_matrix[i, j] <- get_p_value(irmForPaulo[[i]], irmForPaulo[[j]])
  }
}

Correlations <- round(Correlations, 3)
p_value_matrix <- round(p_value_matrix, 3)


combined_matrix <- matrix(NA, ncol = 8, nrow = 8, 
                          dimnames = list(names(irmForPaulo), names(irmForPaulo)))


for (i in 1:ncol(irmForPaulo)) {
  for (j in 1:ncol(irmForPaulo)) {
    combined_matrix[i, j] <- paste0(Correlations[i, j], ", ", p_value_matrix[i, j])
  }
}


# VOLUMETRIC NORMALIZED ***********************


irmForPaulo <- readRDS("Source/irmForPaulo.rds")
names(irmForPaulo)

irmForPaulo <- irmForPaulo %>% select(NUM, DateIRM,
                                      Left_Putamen, 
                                      Right_Putamen, 
                                      Left_Cerebellum_White_Matter,Right_Cerebellum_White_Matter,
                                      Left_Cerebellum_Cortex, Right_Cerebellum_Cortex,
                                      Midbrain,
                                      Pons,
                                      Medulla,
                                      CerebralWhiteMatterVol, TotalGrayVol)

irmForPaulo <- irmForPaulo %>% arrange(NUM, DateIRM) %>% group_by(NUM) %>%
  mutate(TIME_STUDY=time_length(difftime(DateIRM, lag(DateIRM) ), "years"))  %>%
  select(NUM, DateIRM, TIME_STUDY,
         Left_Putamen, 
         Right_Putamen, 
         Left_Cerebellum_White_Matter,Right_Cerebellum_White_Matter,
         Left_Cerebellum_Cortex, Right_Cerebellum_Cortex,
         Midbrain,
         Pons,
         Medulla,
         CerebralWhiteMatterVol, TotalGrayVol)


irmForPaulo <- irmForPaulo %>% mutate(TIME_STUDY=ifelse(is.na(TIME_STUDY), 0, TIME_STUDY))

irmForPaulo <- irmForPaulo %>% mutate(TIME_STUDY=cumsum(TIME_STUDY))

irmForPaulo <- irmForPaulo %>% group_by(NUM) %>% count() %>%
  left_join(irmForPaulo) %>%
  select(NUM, DateIRM, TIME_STUDY, n,
         Left_Putamen, 
         Right_Putamen, 
         Left_Cerebellum_White_Matter,Right_Cerebellum_White_Matter,
         Left_Cerebellum_Cortex, Right_Cerebellum_Cortex,
         Midbrain,
         Pons,
         Medulla,
         CerebralWhiteMatterVol, TotalGrayVol)

irmForPaulo$Medulla <- as.numeric(irmForPaulo$Medulla)
irmForPaulo$Midbrain <- as.numeric(irmForPaulo$Midbrain)
irmForPaulo$CerebralWhiteMatterVol <- as.numeric(irmForPaulo$CerebralWhiteMatterVol)

irmForPaulo <- irmForPaulo %>% mutate(CerebellumTotal=Left_Cerebellum_White_Matter+
                                        Right_Cerebellum_White_Matter+
                                        Left_Cerebellum_Cortex +
                                        Right_Cerebellum_Cortex ) %>%
  mutate(CerebellumWhite=Left_Cerebellum_White_Matter+Right_Cerebellum_White_Matter) %>%
  mutate(Total=as.numeric(CerebralWhiteMatterVol)+as.numeric(TotalGrayVol)) 


TotalCranial <- readRDS("Source/irmForPaulo.rds")
TotalCranial <- TotalCranial %>% arrange(NUM, DateIRM)


irmForPaulo <- irmForPaulo %>% left_join(TotalCranial%>% select(NUM, DateIRM, EstimatedTotalIntraCranialVol))
names(irmForPaulo)

irmForPaulo <- irmForPaulo %>% mutate(Left_Putamen=Left_Putamen/EstimatedTotalIntraCranialVol) %>%
  mutate(Right_Putamen =Right_Putamen /EstimatedTotalIntraCranialVol) %>%
  mutate(CerebellumWhite =CerebellumWhite /EstimatedTotalIntraCranialVol) %>%
  mutate(CerebellumTotal =CerebellumTotal /EstimatedTotalIntraCranialVol) %>%
  mutate(Midbrain=Midbrain/EstimatedTotalIntraCranialVol) %>%
  mutate(Pons =Pons /EstimatedTotalIntraCranialVol) %>%
  mutate(Medulla =Medulla /EstimatedTotalIntraCranialVol) %>%
  mutate(Total =Total /EstimatedTotalIntraCranialVol) 


irmForPaulo <- irmForPaulo %>% ungroup() %>%
  filter(TIME_STUDY==0) %>% select(-c(NUM, DateIRM, TIME_STUDY, n, EstimatedTotalIntraCranialVol, CerebralWhiteMatterVol, TotalGrayVol,
                                      Right_Cerebellum_Cortex, Right_Cerebellum_White_Matter, Left_Cerebellum_Cortex, Left_Cerebellum_White_Matter))


Correlations <- irmForPaulo %>% cor(method = "spearman")

round(Correlations, 3)

get_p_value <- function(x, y) {
  cor_test <- cor.test(x, y, method = "spearman")
  broom::tidy(cor_test)$p.value
}

p_value_matrix <- matrix(NA, ncol = 8, nrow = 8, 
                         dimnames = list(names(irmForPaulo), names(irmForPaulo)))


for (i in 1:ncol(irmForPaulo)) {
  for (j in 1:ncol(irmForPaulo)) {
    p_value_matrix[i, j] <- get_p_value(irmForPaulo[[i]], irmForPaulo[[j]])
  }
}

Correlations <- round(Correlations, 3)
p_value_matrix <- round(p_value_matrix, 3)


combined_matrix <- matrix(NA, ncol = 8, nrow = 8, 
                          dimnames = list(names(irmForPaulo), names(irmForPaulo)))


for (i in 1:ncol(irmForPaulo)) {
  for (j in 1:ncol(irmForPaulo)) {
    combined_matrix[i, j] <- paste0(Correlations[i, j], ", ", p_value_matrix[i, j])
  }
}






# MEAN DIFFUSIVITY ***********************




MD_Takeda <- readxl::read_xlsx("Source/MD_Takeda.xlsx")
MD_Takeda$DateIRM <-  as.Date(MD_Takeda$DateIRM)
names(MD_Takeda)

MD_Takeda <- MD_Takeda %>% select(NUM, DateIRM,
                                  MD_L_Puta, 
                                  MD_R_Puta, 
                                  MD_L_Cerebell_WM,MD_R_Cerebell_WM,
                                  MD_L_Cerebell_GM, MD_R_Cerebell_GM,
                                  MD_Midbrainn,
                                  MD_Pons,
                                  MD_Medulla)

MD_Takeda <- MD_Takeda %>% arrange(NUM, DateIRM) %>% group_by(NUM) %>%
  mutate(TIME_STUDY=time_length(difftime(DateIRM, lag(DateIRM) ), "years"))  %>%
  select(NUM, DateIRM, TIME_STUDY,
         MD_L_Puta, 
         MD_R_Puta, 
         MD_L_Cerebell_WM,MD_R_Cerebell_WM,
         MD_L_Cerebell_GM, MD_R_Cerebell_GM,
         MD_Midbrainn,
         MD_Pons,
         MD_Medulla)


MD_Takeda <- MD_Takeda %>% mutate(TIME_STUDY=ifelse(is.na(TIME_STUDY), 0, TIME_STUDY))

MD_Takeda <- MD_Takeda %>% mutate(TIME_STUDY=cumsum(TIME_STUDY))

MD_Takeda <- MD_Takeda %>% group_by(NUM) %>% count() %>%
  left_join(MD_Takeda) %>%
  select(NUM, DateIRM, TIME_STUDY, n,
         MD_L_Puta, 
         MD_R_Puta, 
         MD_L_Cerebell_WM,MD_R_Cerebell_WM,
         MD_L_Cerebell_GM, MD_R_Cerebell_GM,
         MD_Midbrainn,
         MD_Pons,
         MD_Medulla)


length(unique(MD_Takeda$NUM)) # 24

MD_Takeda %>% ungroup() %>% group_by(n) %>% count()

MD_Takeda %>% mutate(Year= ifelse(TIME_STUDY<=0.5, 0,
                                  ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                                         ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                                ifelse(TIME_STUDY>=2.5 ,3, NA))))) %>%
  select(NUM, Year) %>% group_by(NUM) %>%
  mutate(All=paste0(Year, collapse=","))

range(MD_Takeda$TIME_STUDY)


MD_Takeda <- MD_Takeda %>% mutate(CerebellumWhite=(MD_L_Cerebell_WM+MD_R_Cerebell_WM)/2) %>%
  mutate(CerebellumGray=(MD_L_Cerebell_GM+MD_R_Cerebell_GM)/2) %>%
  mutate(CerebellumTotal=0.82*CerebellumGray+0.18*CerebellumWhite)

names(MD_Takeda)


MD_Takeda <- MD_Takeda %>% ungroup() %>%
  filter(TIME_STUDY==0) %>% select(-c(NUM, DateIRM, TIME_STUDY, n, CerebellumGray, MD_R_Cerebell_GM, MD_L_Cerebell_GM, MD_R_Cerebell_WM, MD_L_Cerebell_WM))


Correlations <- MD_Takeda %>% cor(method = "spearman")

round(Correlations, 3)

get_p_value <- function(x, y) {
  cor_test <- cor.test(x, y, method = "spearman")
  broom::tidy(cor_test)$p.value
}

p_value_matrix <- matrix(NA, ncol = 7, nrow = 7, 
                         dimnames = list(names(MD_Takeda), names(MD_Takeda)))


for (i in 1:ncol(MD_Takeda)) {
  for (j in 1:ncol(MD_Takeda)) {
    p_value_matrix[i, j] <- get_p_value(MD_Takeda[[i]], MD_Takeda[[j]])
  }
}

Correlations <- round(Correlations, 3)
p_value_matrix <- round(p_value_matrix, 3)


combined_matrix <- matrix(NA, ncol = 7, nrow = 7, 
                          dimnames = list(names(MD_Takeda), names(MD_Takeda)))


for (i in 1:ncol(MD_Takeda)) {
  for (j in 1:ncol(MD_Takeda)) {
    combined_matrix[i, j] <- paste0(Correlations[i, j], ", ", p_value_matrix[i, j])
  }
}




# FRACTIONAL ANISOTROPY ***********************




FA_Takeda <- readxl::read_xlsx("Source/FA_Takeda.xlsx")
FA_Takeda$DateIRM <-  as.Date(FA_Takeda$DateIRM)
names(FA_Takeda)

FA_Takeda <- FA_Takeda %>% select(NUM, DateIRM,
                                  FA_L_Puta, 
                                  FA_R_Puta, 
                                  FA_L_Cerebell_WM,FA_R_Cerebell_WM,
                                  FA_L_Cerebell_GM, FA_R_Cerebell_GM,
                                  FA_Midbrainn,
                                  FA_Pons,
                                  FA_Medulla)

FA_Takeda <- FA_Takeda %>% arrange(NUM, DateIRM) %>% group_by(NUM) %>%
  mutate(TIME_STUDY=time_length(difftime(DateIRM, lag(DateIRM) ), "years"))  %>%
  select(NUM, DateIRM, TIME_STUDY,
         FA_L_Puta, 
         FA_R_Puta, 
         FA_L_Cerebell_WM,FA_R_Cerebell_WM,
         FA_L_Cerebell_GM, FA_R_Cerebell_GM,
         FA_Midbrainn,
         FA_Pons,
         FA_Medulla)


FA_Takeda <- FA_Takeda %>% mutate(TIME_STUDY=ifelse(is.na(TIME_STUDY), 0, TIME_STUDY))

FA_Takeda <- FA_Takeda %>% mutate(TIME_STUDY=cumsum(TIME_STUDY))

FA_Takeda <- FA_Takeda %>% group_by(NUM) %>% count() %>%
  left_join(FA_Takeda) %>%
  select(NUM, DateIRM, TIME_STUDY, n,
         FA_L_Puta, 
         FA_R_Puta, 
         FA_L_Cerebell_WM,FA_R_Cerebell_WM,
         FA_L_Cerebell_GM, FA_R_Cerebell_GM,
         FA_Midbrainn,
         FA_Pons,
         FA_Medulla)


length(unique(FA_Takeda$NUM)) # 24

FA_Takeda %>% ungroup() %>% group_by(n) %>% count()

FA_Takeda %>% mutate(Year= ifelse(TIME_STUDY<=0.5, 0,
                                  ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                                         ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                                ifelse(TIME_STUDY>=2.5 ,3, NA))))) %>%
  select(NUM, Year) %>% group_by(NUM) %>%
  mutate(All=paste0(Year, collapse=","))

range(FA_Takeda$TIME_STUDY)


FA_Takeda <- FA_Takeda %>% mutate(CerebellumWhite=(FA_L_Cerebell_WM+FA_R_Cerebell_WM)/2) %>%
  mutate(CerebellumGray=(FA_L_Cerebell_GM+FA_R_Cerebell_GM)/2) %>%
  mutate(CerebellumTotal=0.82*CerebellumGray+0.18*CerebellumWhite)


names(FA_Takeda)


FA_Takeda <- FA_Takeda %>% ungroup() %>%
  filter(TIME_STUDY==0) %>% select(-c(NUM, DateIRM, TIME_STUDY, n, CerebellumGray, FA_L_Cerebell_WM, FA_R_Cerebell_WM, FA_L_Cerebell_GM, FA_R_Cerebell_GM))


Correlations <- FA_Takeda %>% cor(method = "spearman")

round(Correlations, 3)

get_p_value <- function(x, y) {
  cor_test <- cor.test(x, y, method = "spearman")
  broom::tidy(cor_test)$p.value
}

p_value_matrix <- matrix(NA, ncol = 7, nrow = 7, 
                         dimnames = list(names(FA_Takeda), names(FA_Takeda)))


for (i in 1:ncol(FA_Takeda)) {
  for (j in 1:ncol(FA_Takeda)) {
    p_value_matrix[i, j] <- get_p_value(FA_Takeda[[i]], FA_Takeda[[j]])
  }
}

Correlations <- round(Correlations, 3)
p_value_matrix <- round(p_value_matrix, 3)


combined_matrix <- matrix(NA, ncol = 7, nrow = 7, 
                          dimnames = list(names(FA_Takeda), names(FA_Takeda)))


for (i in 1:ncol(FA_Takeda)) {
  for (j in 1:ncol(FA_Takeda)) {
    combined_matrix[i, j] <- paste0(Correlations[i, j], ", ", p_value_matrix[i, j])
  }
}





# ------------------








# CORRELATION INTERNAL DELTA -----------------------------

# VOLUMETRIC ORIGINAL ***********************

irmForPaulo <- readRDS("Source/irmForPaulo.rds")
names(irmForPaulo)

irmForPaulo <- irmForPaulo %>% select(NUM, DateIRM,
                                      Left_Putamen, 
                                      Right_Putamen, 
                                      Left_Cerebellum_White_Matter,Right_Cerebellum_White_Matter,
                                      Left_Cerebellum_Cortex, Right_Cerebellum_Cortex,
                                      Midbrain,
                                      Pons,
                                      Medulla,
                                      CerebralWhiteMatterVol, TotalGrayVol)

irmForPaulo <- irmForPaulo %>% arrange(NUM, DateIRM) %>% group_by(NUM) %>%
  mutate(TIME_STUDY=time_length(difftime(DateIRM, lag(DateIRM) ), "years"))  %>%
  select(NUM, DateIRM, TIME_STUDY,
         Left_Putamen, 
         Right_Putamen, 
         Left_Cerebellum_White_Matter,Right_Cerebellum_White_Matter,
         Left_Cerebellum_Cortex, Right_Cerebellum_Cortex,
         Midbrain,
         Pons,
         Medulla,
         CerebralWhiteMatterVol, TotalGrayVol)


irmForPaulo <- irmForPaulo %>% mutate(TIME_STUDY=ifelse(is.na(TIME_STUDY), 0, TIME_STUDY))

irmForPaulo <- irmForPaulo %>% mutate(TIME_STUDY=cumsum(TIME_STUDY))

irmForPaulo <- irmForPaulo %>% group_by(NUM) %>% count() %>%
  left_join(irmForPaulo) %>%
  select(NUM, DateIRM, TIME_STUDY, n,
         Left_Putamen, 
         Right_Putamen, 
         Left_Cerebellum_White_Matter,Right_Cerebellum_White_Matter,
         Left_Cerebellum_Cortex, Right_Cerebellum_Cortex,
         Midbrain,
         Pons,
         Medulla,
         CerebralWhiteMatterVol, TotalGrayVol)

irmForPaulo$Medulla <- as.numeric(irmForPaulo$Medulla)
irmForPaulo$Midbrain <- as.numeric(irmForPaulo$Midbrain)
irmForPaulo$CerebralWhiteMatterVol <- as.numeric(irmForPaulo$CerebralWhiteMatterVol)

irmForPaulo <- irmForPaulo %>% mutate(CerebellumTotal=Left_Cerebellum_White_Matter+
                                        Right_Cerebellum_White_Matter+
                                        Left_Cerebellum_Cortex +
                                        Right_Cerebellum_Cortex ) %>%
  mutate(CerebellumWhite=Left_Cerebellum_White_Matter+Right_Cerebellum_White_Matter) %>%
  mutate(Total=as.numeric(CerebralWhiteMatterVol)+as.numeric(TotalGrayVol)) 

names(irmForPaulo)

Baseline <- irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(Left_Putamen, Right_Putamen, CerebellumWhite,CerebellumTotal, Midbrain,Pons,Medulla,Total) 

Year_1 <- irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>%
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(Left_Putamen, Right_Putamen, CerebellumWhite,CerebellumTotal, Midbrain,Pons,Medulla,Total) 


Delta <- Year_1 - Baseline


Correlations <- Delta %>% cor(method = "spearman")

round(Correlations, 3)

get_p_value <- function(x, y) {
  cor_test <- cor.test(x, y, method = "spearman")
  broom::tidy(cor_test)$p.value
}

p_value_matrix <- matrix(NA, ncol = 8, nrow = 8, 
                         dimnames = list(names(Delta), names(Delta)))


for (i in 1:ncol(Delta)) {
  for (j in 1:ncol(Delta)) {
    p_value_matrix[i, j] <- get_p_value(Delta[[i]], Delta[[j]])
  }
}

Correlations <- round(Correlations, 3)
p_value_matrix <- round(p_value_matrix, 3)


combined_matrix <- matrix(NA, ncol = 8, nrow = 8, 
                          dimnames = list(names(Delta), names(Delta)))


for (i in 1:ncol(Delta)) {
  for (j in 1:ncol(Delta)) {
    combined_matrix[i, j] <- paste0(Correlations[i, j], ", ", p_value_matrix[i, j])
  }
}


# VOLUMETRIC NORMALIZED ***********************


irmForPaulo <- readRDS("Source/irmForPaulo.rds")
names(irmForPaulo)

irmForPaulo <- irmForPaulo %>% select(NUM, DateIRM,
                                      Left_Putamen, 
                                      Right_Putamen, 
                                      Left_Cerebellum_White_Matter,Right_Cerebellum_White_Matter,
                                      Left_Cerebellum_Cortex, Right_Cerebellum_Cortex,
                                      Midbrain,
                                      Pons,
                                      Medulla,
                                      CerebralWhiteMatterVol, TotalGrayVol)

irmForPaulo <- irmForPaulo %>% arrange(NUM, DateIRM) %>% group_by(NUM) %>%
  mutate(TIME_STUDY=time_length(difftime(DateIRM, lag(DateIRM) ), "years"))  %>%
  select(NUM, DateIRM, TIME_STUDY,
         Left_Putamen, 
         Right_Putamen, 
         Left_Cerebellum_White_Matter,Right_Cerebellum_White_Matter,
         Left_Cerebellum_Cortex, Right_Cerebellum_Cortex,
         Midbrain,
         Pons,
         Medulla,
         CerebralWhiteMatterVol, TotalGrayVol)


irmForPaulo <- irmForPaulo %>% mutate(TIME_STUDY=ifelse(is.na(TIME_STUDY), 0, TIME_STUDY))

irmForPaulo <- irmForPaulo %>% mutate(TIME_STUDY=cumsum(TIME_STUDY))

irmForPaulo <- irmForPaulo %>% group_by(NUM) %>% count() %>%
  left_join(irmForPaulo) %>%
  select(NUM, DateIRM, TIME_STUDY, n,
         Left_Putamen, 
         Right_Putamen, 
         Left_Cerebellum_White_Matter,Right_Cerebellum_White_Matter,
         Left_Cerebellum_Cortex, Right_Cerebellum_Cortex,
         Midbrain,
         Pons,
         Medulla,
         CerebralWhiteMatterVol, TotalGrayVol)

irmForPaulo$Medulla <- as.numeric(irmForPaulo$Medulla)
irmForPaulo$Midbrain <- as.numeric(irmForPaulo$Midbrain)
irmForPaulo$CerebralWhiteMatterVol <- as.numeric(irmForPaulo$CerebralWhiteMatterVol)

irmForPaulo <- irmForPaulo %>% mutate(CerebellumTotal=Left_Cerebellum_White_Matter+
                                        Right_Cerebellum_White_Matter+
                                        Left_Cerebellum_Cortex +
                                        Right_Cerebellum_Cortex ) %>%
  mutate(CerebellumWhite=Left_Cerebellum_White_Matter+Right_Cerebellum_White_Matter) %>%
  mutate(Total=as.numeric(CerebralWhiteMatterVol)+as.numeric(TotalGrayVol)) 


TotalCranial <- readRDS("Source/irmForPaulo.rds")
TotalCranial <- TotalCranial %>% arrange(NUM, DateIRM)


irmForPaulo <- irmForPaulo %>% left_join(TotalCranial%>% select(NUM, DateIRM, EstimatedTotalIntraCranialVol))
names(irmForPaulo)

irmForPaulo <- irmForPaulo %>% mutate(Left_Putamen=Left_Putamen/EstimatedTotalIntraCranialVol) %>%
  mutate(Right_Putamen =Right_Putamen /EstimatedTotalIntraCranialVol) %>%
  mutate(CerebellumWhite =CerebellumWhite /EstimatedTotalIntraCranialVol) %>%
  mutate(CerebellumTotal =CerebellumTotal /EstimatedTotalIntraCranialVol) %>%
  mutate(Midbrain=Midbrain/EstimatedTotalIntraCranialVol) %>%
  mutate(Pons =Pons /EstimatedTotalIntraCranialVol) %>%
  mutate(Medulla =Medulla /EstimatedTotalIntraCranialVol) %>%
  mutate(Total =Total /EstimatedTotalIntraCranialVol) 



Baseline <- irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(Left_Putamen, Right_Putamen, CerebellumWhite,CerebellumTotal, Midbrain,Pons,Medulla,Total) 

Year_1 <- irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>%
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(Left_Putamen, Right_Putamen, CerebellumWhite,CerebellumTotal, Midbrain,Pons,Medulla,Total) 


Delta <- Year_1 - Baseline



Correlations <- Delta %>% cor(method = "spearman")

round(Correlations, 3)

get_p_value <- function(x, y) {
  cor_test <- cor.test(x, y, method = "spearman")
  broom::tidy(cor_test)$p.value
}

p_value_matrix <- matrix(NA, ncol = 8, nrow = 8, 
                         dimnames = list(names(Delta), names(Delta)))


for (i in 1:ncol(Delta)) {
  for (j in 1:ncol(Delta)) {
    p_value_matrix[i, j] <- get_p_value(Delta[[i]], Delta[[j]])
  }
}

Correlations <- round(Correlations, 3)
p_value_matrix <- round(p_value_matrix, 3)


combined_matrix <- matrix(NA, ncol = 8, nrow = 8, 
                          dimnames = list(names(Delta), names(Delta)))


for (i in 1:ncol(Delta)) {
  for (j in 1:ncol(Delta)) {
    combined_matrix[i, j] <- paste0(Correlations[i, j], ", ", p_value_matrix[i, j])
  }
}






# MEAN DIFFUSIVITY ***********************




MD_Takeda <- readxl::read_xlsx("Source/MD_Takeda.xlsx")
MD_Takeda$DateIRM <-  as.Date(MD_Takeda$DateIRM)
names(MD_Takeda)

MD_Takeda <- MD_Takeda %>% select(NUM, DateIRM,
                                  MD_L_Puta, 
                                  MD_R_Puta, 
                                  MD_L_Cerebell_WM,MD_R_Cerebell_WM,
                                  MD_L_Cerebell_GM, MD_R_Cerebell_GM,
                                  MD_Midbrainn,
                                  MD_Pons,
                                  MD_Medulla)

MD_Takeda <- MD_Takeda %>% arrange(NUM, DateIRM) %>% group_by(NUM) %>%
  mutate(TIME_STUDY=time_length(difftime(DateIRM, lag(DateIRM) ), "years"))  %>%
  select(NUM, DateIRM, TIME_STUDY,
         MD_L_Puta, 
         MD_R_Puta, 
         MD_L_Cerebell_WM,MD_R_Cerebell_WM,
         MD_L_Cerebell_GM, MD_R_Cerebell_GM,
         MD_Midbrainn,
         MD_Pons,
         MD_Medulla)


MD_Takeda <- MD_Takeda %>% mutate(TIME_STUDY=ifelse(is.na(TIME_STUDY), 0, TIME_STUDY))

MD_Takeda <- MD_Takeda %>% mutate(TIME_STUDY=cumsum(TIME_STUDY))

MD_Takeda <- MD_Takeda %>% group_by(NUM) %>% count() %>%
  left_join(MD_Takeda) %>%
  select(NUM, DateIRM, TIME_STUDY, n,
         MD_L_Puta, 
         MD_R_Puta, 
         MD_L_Cerebell_WM,MD_R_Cerebell_WM,
         MD_L_Cerebell_GM, MD_R_Cerebell_GM,
         MD_Midbrainn,
         MD_Pons,
         MD_Medulla)


length(unique(MD_Takeda$NUM)) # 24

MD_Takeda %>% ungroup() %>% group_by(n) %>% count()

MD_Takeda %>% mutate(Year= ifelse(TIME_STUDY<=0.5, 0,
                                  ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                                         ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                                ifelse(TIME_STUDY>=2.5 ,3, NA))))) %>%
  select(NUM, Year) %>% group_by(NUM) %>%
  mutate(All=paste0(Year, collapse=","))

range(MD_Takeda$TIME_STUDY)


MD_Takeda <- MD_Takeda %>% mutate(CerebellumWhite=(MD_L_Cerebell_WM+MD_R_Cerebell_WM)/2) %>%
  mutate(CerebellumGray=(MD_L_Cerebell_GM+MD_R_Cerebell_GM)/2) %>%
  mutate(CerebellumTotal=0.82*CerebellumGray+0.18*CerebellumWhite)

names(MD_Takeda)



Baseline <- MD_Takeda %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(MD_L_Puta, MD_R_Puta, CerebellumWhite,CerebellumTotal, MD_Midbrainn,MD_Pons,MD_Medulla) 

Year_1 <- MD_Takeda %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>%
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(MD_L_Puta, MD_R_Puta, CerebellumWhite,CerebellumTotal, MD_Midbrainn,MD_Pons,MD_Medulla) 


Delta <- Year_1 - Baseline



Correlations <- Delta %>% cor(method = "spearman")

round(Correlations, 3)

get_p_value <- function(x, y) {
  cor_test <- cor.test(x, y, method = "spearman")
  broom::tidy(cor_test)$p.value
}

p_value_matrix <- matrix(NA, ncol = 7, nrow = 7, 
                         dimnames = list(names(Delta), names(Delta)))


for (i in 1:ncol(Delta)) {
  for (j in 1:ncol(Delta)) {
    p_value_matrix[i, j] <- get_p_value(Delta[[i]], Delta[[j]])
  }
}

Correlations <- round(Correlations, 3)
p_value_matrix <- round(p_value_matrix, 3)


combined_matrix <- matrix(NA, ncol = 7, nrow = 7, 
                          dimnames = list(names(Delta), names(Delta)))


for (i in 1:ncol(Delta)) {
  for (j in 1:ncol(Delta)) {
    combined_matrix[i, j] <- paste0(Correlations[i, j], ", ", p_value_matrix[i, j])
  }
}




# FRACTIONAL ANISOTROPY ***********************




FA_Takeda <- readxl::read_xlsx("Source/FA_Takeda.xlsx")
FA_Takeda$DateIRM <-  as.Date(FA_Takeda$DateIRM)
names(FA_Takeda)

FA_Takeda <- FA_Takeda %>% select(NUM, DateIRM,
                                  FA_L_Puta, 
                                  FA_R_Puta, 
                                  FA_L_Cerebell_WM,FA_R_Cerebell_WM,
                                  FA_L_Cerebell_GM, FA_R_Cerebell_GM,
                                  FA_Midbrainn,
                                  FA_Pons,
                                  FA_Medulla)

FA_Takeda <- FA_Takeda %>% arrange(NUM, DateIRM) %>% group_by(NUM) %>%
  mutate(TIME_STUDY=time_length(difftime(DateIRM, lag(DateIRM) ), "years"))  %>%
  select(NUM, DateIRM, TIME_STUDY,
         FA_L_Puta, 
         FA_R_Puta, 
         FA_L_Cerebell_WM,FA_R_Cerebell_WM,
         FA_L_Cerebell_GM, FA_R_Cerebell_GM,
         FA_Midbrainn,
         FA_Pons,
         FA_Medulla)


FA_Takeda <- FA_Takeda %>% mutate(TIME_STUDY=ifelse(is.na(TIME_STUDY), 0, TIME_STUDY))

FA_Takeda <- FA_Takeda %>% mutate(TIME_STUDY=cumsum(TIME_STUDY))

FA_Takeda <- FA_Takeda %>% group_by(NUM) %>% count() %>%
  left_join(FA_Takeda) %>%
  select(NUM, DateIRM, TIME_STUDY, n,
         FA_L_Puta, 
         FA_R_Puta, 
         FA_L_Cerebell_WM,FA_R_Cerebell_WM,
         FA_L_Cerebell_GM, FA_R_Cerebell_GM,
         FA_Midbrainn,
         FA_Pons,
         FA_Medulla)


length(unique(FA_Takeda$NUM)) # 24

FA_Takeda %>% ungroup() %>% group_by(n) %>% count()

FA_Takeda %>% mutate(Year= ifelse(TIME_STUDY<=0.5, 0,
                                  ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                                         ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                                ifelse(TIME_STUDY>=2.5 ,3, NA))))) %>%
  select(NUM, Year) %>% group_by(NUM) %>%
  mutate(All=paste0(Year, collapse=","))

range(FA_Takeda$TIME_STUDY)


FA_Takeda <- FA_Takeda %>% mutate(CerebellumWhite=(FA_L_Cerebell_WM+FA_R_Cerebell_WM)/2) %>%
  mutate(CerebellumGray=(FA_L_Cerebell_GM+FA_R_Cerebell_GM)/2) %>%
  mutate(CerebellumTotal=0.82*CerebellumGray+0.18*CerebellumWhite)


names(FA_Takeda)




Baseline <- FA_Takeda %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(FA_L_Puta, FA_R_Puta, CerebellumWhite,CerebellumTotal, FA_Midbrainn,FA_Pons,FA_Medulla) 

Year_1 <- FA_Takeda %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>%
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(FA_L_Puta, FA_R_Puta, CerebellumWhite,CerebellumTotal, FA_Midbrainn,FA_Pons,FA_Medulla) 

Delta <- Year_1 - Baseline




Correlations <- Delta %>% cor(method = "spearman")

round(Correlations, 3)

get_p_value <- function(x, y) {
  cor_test <- cor.test(x, y, method = "spearman")
  broom::tidy(cor_test)$p.value
}

p_value_matrix <- matrix(NA, ncol = 7, nrow = 7, 
                         dimnames = list(names(Delta), names(Delta)))


for (i in 1:ncol(Delta)) {
  for (j in 1:ncol(Delta)) {
    p_value_matrix[i, j] <- get_p_value(Delta[[i]], Delta[[j]])
  }
}

Correlations <- round(Correlations, 3)
p_value_matrix <- round(p_value_matrix, 3)


combined_matrix <- matrix(NA, ncol = 7, nrow = 7, 
                          dimnames = list(names(Delta), names(Delta)))


for (i in 1:ncol(Delta)) {
  for (j in 1:ncol(Delta)) {
    combined_matrix[i, j] <- paste0(Correlations[i, j], ", ", p_value_matrix[i, j])
  }
}





# ------------------








# CLINICAL CORRELATION UMSARS MRIs -------------------

# UMSARS 1 DA-modified exc. #11 collapsed 
dataCohorteManaged <- readRDS("Source/dataCohorteManaged.rds")

UMSARS1 <- dataCohorteManaged %>% 
  select(NUM, DATECONSULT, TIME_STUDY, UMSARS1_1:UMSARS1_TOT)

UMSARS1$UMSARS1_TOT_v2 <- rowSums(UMSARS1[, 4:15], na.rm = TRUE)

UMSARS1$missing_na <- rowSums(is.na(UMSARS1[, 4:15]))
UMSARS1[, 4:15] <- UMSARS1[, 4:15] -1

for (i in 4:15) {
  UMSARS1[, i][UMSARS1[, i] < 0] <- 0
}

UMSARS1$UMSARS1_TOT_v2 <- rowSums(UMSARS1[, 4:15], na.rm = TRUE)
UMSARS1 <-  UMSARS1 %>%  select(NUM, DATECONSULT,  UMSARS1_TOT_v2, UMSARS1_11)
UMSARS1 <- UMSARS1 %>% mutate(UMSARS1_TOT_FDA=UMSARS1_TOT_v2-UMSARS1_11)
UMSARS1 <- UMSARS1 %>% select(NUM, DATECONSULT, UMSARS1_TOT_FDA)



Baseline_UMSARS1 <- UMSARS1 %>% inner_join(Date_Baseline) %>% 
  mutate(Elapsed=abs(as.numeric(DATECONSULT-DateIRM))) %>%
  group_by(NUM) %>% drop_na() %>% filter(Elapsed==min(Elapsed)) %>% distinct() %>% 
  select(NUM, DATECONSULT, UMSARS1_TOT_FDA, Elapsed) %>% 
  rename("Baseline_DATECONSULT"="DATECONSULT", 
         "Baseline_UMSARS1_TOT_FDA"="UMSARS1_TOT_FDA", 
         "Baseline_Elapsed"="Elapsed")

Year_1_UMSARS1 <- UMSARS1 %>% inner_join(Date_Year_1) %>% 
  mutate(Elapsed=abs(as.numeric(DATECONSULT-DateIRM))) %>%
  group_by(NUM) %>% drop_na() %>% filter(Elapsed==min(Elapsed)) %>% distinct() %>% 
  select(NUM, DATECONSULT, UMSARS1_TOT_FDA, Elapsed) %>% 
  rename("Year1_DATECONSULT"="DATECONSULT", 
         "Year1_UMSARS1_TOT_FDA"="UMSARS1_TOT_FDA", 
         "Year1_Elapsed"="Elapsed")



Year_1_UMSARS1 %>% inner_join(Baseline_UMSARS1) %>%
  filter(Year1_DATECONSULT!=Baseline_DATECONSULT) %>% ungroup() %>%
  mutate(Delta=Year1_UMSARS1_TOT_FDA-Baseline_UMSARS1_TOT_FDA) %>%
  mutate(TIME_STUDY=time_length(difftime(Year1_DATECONSULT , Baseline_DATECONSULT)  , "years")) %>%
  # summarise(mean=mean(TIME_STUDY*12), sd=sd(TIME_STUDY*12)) #11.3  3.18
  ggplot(aes(TIME_STUDY*12)) +
  geom_density(size=2, fill="midnightblue", alpha=0.5)+
  xlim(0,24) + xlab("\n Number of Months Elapsed \n Between The 2 UMSARS Records") +
  ylab("Patient density \n") +
  theme_minimal()

UMSARS_Delta <- Year_1_UMSARS1 %>% inner_join(Baseline_UMSARS1) %>%
  filter(Year1_DATECONSULT!=Baseline_DATECONSULT) %>% ungroup() %>%
  mutate(Delta=Year1_UMSARS1_TOT_FDA-Baseline_UMSARS1_TOT_FDA) %>%
  mutate(TIME_STUDY=time_length(difftime(Year1_DATECONSULT , Baseline_DATECONSULT)  , "years"))  %>%
  mutate(Delta=Delta/TIME_STUDY) %>%
  select(NUM, Delta) # 22


Original_UMSARS_Delta <- UMSARS_Delta







# VOLUMETRIC ORIGINAL ***********************

irmForPaulo <- readRDS("Source/irmForPaulo.rds")
names(irmForPaulo)

irmForPaulo <- irmForPaulo %>% select(NUM, DateIRM,
                                      Left_Putamen, 
                                      Right_Putamen, 
                                      Left_Cerebellum_White_Matter,Right_Cerebellum_White_Matter,
                                      Left_Cerebellum_Cortex, Right_Cerebellum_Cortex,
                                      Midbrain,
                                      Pons,
                                      Medulla,
                                      CerebralWhiteMatterVol, TotalGrayVol)

irmForPaulo <- irmForPaulo %>% arrange(NUM, DateIRM) %>% group_by(NUM) %>%
  mutate(TIME_STUDY=time_length(difftime(DateIRM, lag(DateIRM) ), "years"))  %>%
  select(NUM, DateIRM, TIME_STUDY,
         Left_Putamen, 
         Right_Putamen, 
         Left_Cerebellum_White_Matter,Right_Cerebellum_White_Matter,
         Left_Cerebellum_Cortex, Right_Cerebellum_Cortex,
         Midbrain,
         Pons,
         Medulla,
         CerebralWhiteMatterVol, TotalGrayVol)


irmForPaulo <- irmForPaulo %>% mutate(TIME_STUDY=ifelse(is.na(TIME_STUDY), 0, TIME_STUDY))

irmForPaulo <- irmForPaulo %>% mutate(TIME_STUDY=cumsum(TIME_STUDY))

irmForPaulo <- irmForPaulo %>% group_by(NUM) %>% count() %>%
  left_join(irmForPaulo) %>%
  select(NUM, DateIRM, TIME_STUDY, n,
         Left_Putamen, 
         Right_Putamen, 
         Left_Cerebellum_White_Matter,Right_Cerebellum_White_Matter,
         Left_Cerebellum_Cortex, Right_Cerebellum_Cortex,
         Midbrain,
         Pons,
         Medulla,
         CerebralWhiteMatterVol, TotalGrayVol)

irmForPaulo$Medulla <- as.numeric(irmForPaulo$Medulla)
irmForPaulo$Midbrain <- as.numeric(irmForPaulo$Midbrain)
irmForPaulo$CerebralWhiteMatterVol <- as.numeric(irmForPaulo$CerebralWhiteMatterVol)

irmForPaulo <- irmForPaulo %>% mutate(CerebellumTotal=Left_Cerebellum_White_Matter+
                                        Right_Cerebellum_White_Matter+
                                        Left_Cerebellum_Cortex +
                                        Right_Cerebellum_Cortex ) %>%
  mutate(CerebellumWhite=Left_Cerebellum_White_Matter+Right_Cerebellum_White_Matter) %>%
  mutate(Total=as.numeric(CerebralWhiteMatterVol)+as.numeric(TotalGrayVol)) 

names(irmForPaulo)

Baseline <- irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, Left_Putamen, Right_Putamen, CerebellumWhite,CerebellumTotal, Midbrain,Pons,Medulla,Total) 

Date_Baseline <- irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, DateIRM)

Year_1 <- irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>%
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(NUM, Left_Putamen, Right_Putamen, CerebellumWhite,CerebellumTotal, Midbrain,Pons,Medulla,Total) 


Date_Year_1 <- irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>%
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>% select(NUM, DateIRM)

TIME_STUDY <- irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>% 
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY)


Pats <- Year_1 %>% select(NUM) %>% 
  inner_join(Baseline %>% select(NUM)) %>% 
  inner_join(UMSARS_Delta %>% select(NUM)) %>%
  arrange(NUM)


Year_1 <- Pats %>% left_join(Year_1) %>% select(-NUM)
Baseline <- Pats %>% left_join(Baseline) %>% select(-NUM)
TIME_STUDY <- Pats %>% left_join(TIME_STUDY) %>% select(-NUM)
UMSARS_Delta <- Pats %>% left_join(UMSARS_Delta) %>% select(-NUM)


Delta <- (Year_1 - Baseline)

Delta <- Delta/rep(TIME_STUDY,8)

Delta <- Delta %>% bind_cols(UMSARS_Delta)


Correlations <- Delta %>% cor(method = "spearman")

round(Correlations, 3)

get_p_value <- function(x, y) {
  cor_test <- cor.test(x, y, method = "spearman")
  broom::tidy(cor_test)$p.value
}

p_value_matrix <- matrix(NA, ncol = 9, nrow = 9, 
                         dimnames = list(names(Delta), names(Delta)))


for (i in 1:ncol(Delta)) {
  for (j in 1:ncol(Delta)) {
    p_value_matrix[i, j] <- get_p_value(Delta[[i]], Delta[[j]])
  }
}

Correlations <- round(Correlations, 3)
p_value_matrix <- round(p_value_matrix, 3)


combined_matrix <- matrix(NA, ncol = 9, nrow = 9, 
                          dimnames = list(names(Delta), names(Delta)))


for (i in 1:ncol(Delta)) {
  for (j in 1:ncol(Delta)) {
    combined_matrix[i, j] <- paste0(Correlations[i, j], ", ", p_value_matrix[i, j])
  }
}





# VOLUMETRIC NORMALIZED ***********************


irmForPaulo <- readRDS("Source/irmForPaulo.rds")
names(irmForPaulo)

irmForPaulo <- irmForPaulo %>% select(NUM, DateIRM,
                                      Left_Putamen, 
                                      Right_Putamen, 
                                      Left_Cerebellum_White_Matter,Right_Cerebellum_White_Matter,
                                      Left_Cerebellum_Cortex, Right_Cerebellum_Cortex,
                                      Midbrain,
                                      Pons,
                                      Medulla,
                                      CerebralWhiteMatterVol, TotalGrayVol)

irmForPaulo <- irmForPaulo %>% arrange(NUM, DateIRM) %>% group_by(NUM) %>%
  mutate(TIME_STUDY=time_length(difftime(DateIRM, lag(DateIRM) ), "years"))  %>%
  select(NUM, DateIRM, TIME_STUDY,
         Left_Putamen, 
         Right_Putamen, 
         Left_Cerebellum_White_Matter,Right_Cerebellum_White_Matter,
         Left_Cerebellum_Cortex, Right_Cerebellum_Cortex,
         Midbrain,
         Pons,
         Medulla,
         CerebralWhiteMatterVol, TotalGrayVol)


irmForPaulo <- irmForPaulo %>% mutate(TIME_STUDY=ifelse(is.na(TIME_STUDY), 0, TIME_STUDY))

irmForPaulo <- irmForPaulo %>% mutate(TIME_STUDY=cumsum(TIME_STUDY))

irmForPaulo <- irmForPaulo %>% group_by(NUM) %>% count() %>%
  left_join(irmForPaulo) %>%
  select(NUM, DateIRM, TIME_STUDY, n,
         Left_Putamen, 
         Right_Putamen, 
         Left_Cerebellum_White_Matter,Right_Cerebellum_White_Matter,
         Left_Cerebellum_Cortex, Right_Cerebellum_Cortex,
         Midbrain,
         Pons,
         Medulla,
         CerebralWhiteMatterVol, TotalGrayVol)

irmForPaulo$Medulla <- as.numeric(irmForPaulo$Medulla)
irmForPaulo$Midbrain <- as.numeric(irmForPaulo$Midbrain)
irmForPaulo$CerebralWhiteMatterVol <- as.numeric(irmForPaulo$CerebralWhiteMatterVol)

irmForPaulo <- irmForPaulo %>% mutate(CerebellumTotal=Left_Cerebellum_White_Matter+
                                        Right_Cerebellum_White_Matter+
                                        Left_Cerebellum_Cortex +
                                        Right_Cerebellum_Cortex ) %>%
  mutate(CerebellumWhite=Left_Cerebellum_White_Matter+Right_Cerebellum_White_Matter) %>%
  mutate(Total=as.numeric(CerebralWhiteMatterVol)+as.numeric(TotalGrayVol)) 


TotalCranial <- readRDS("Source/irmForPaulo.rds")
TotalCranial <- TotalCranial %>% arrange(NUM, DateIRM)


irmForPaulo <- irmForPaulo %>% left_join(TotalCranial%>% select(NUM, DateIRM, EstimatedTotalIntraCranialVol))
names(irmForPaulo)

irmForPaulo <- irmForPaulo %>% mutate(Left_Putamen=Left_Putamen/EstimatedTotalIntraCranialVol) %>%
  mutate(Right_Putamen =Right_Putamen /EstimatedTotalIntraCranialVol) %>%
  mutate(CerebellumWhite =CerebellumWhite /EstimatedTotalIntraCranialVol) %>%
  mutate(CerebellumTotal =CerebellumTotal /EstimatedTotalIntraCranialVol) %>%
  mutate(Midbrain=Midbrain/EstimatedTotalIntraCranialVol) %>%
  mutate(Pons =Pons /EstimatedTotalIntraCranialVol) %>%
  mutate(Medulla =Medulla /EstimatedTotalIntraCranialVol) %>%
  mutate(Total =Total /EstimatedTotalIntraCranialVol) 


names(irmForPaulo)

Baseline <- irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, Left_Putamen, Right_Putamen, CerebellumWhite,CerebellumTotal, Midbrain,Pons,Medulla,Total) 

Date_Baseline <- irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, DateIRM)

Year_1 <- irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>%
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(NUM, Left_Putamen, Right_Putamen, CerebellumWhite,CerebellumTotal, Midbrain,Pons,Medulla,Total) 


Date_Year_1 <- irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>%
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>% select(NUM, DateIRM)

TIME_STUDY <- irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>% 
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY)




TIME_STUDY %>% 
  # summarise(mean=mean(TIME_STUDY*12), sd=sd(TIME_STUDY*12)) #10.7  2.49
  ggplot(aes(TIME_STUDY*12)) +
  geom_density(size=2, fill="firebrick", alpha=0.5)+
  xlim(0,24) + xlab("\n Number of Months Elapsed \n Between The 2 MRI Records") +
  ylab("Patient density \n") +
  theme_minimal()

Pats <- Year_1 %>% select(NUM) %>% 
  inner_join(Baseline %>% select(NUM)) %>% 
  inner_join(UMSARS_Delta %>% select(NUM)) %>%
  arrange(NUM)


Year_1 <- Pats %>% left_join(Year_1) %>% select(-NUM)
Baseline <- Pats %>% left_join(Baseline) %>% select(-NUM)
TIME_STUDY <- Pats %>% left_join(TIME_STUDY) %>% select(-NUM)
UMSARS_Delta <- Pats %>% left_join(UMSARS_Delta) %>% select(-NUM)


Delta <- (Year_1 - Baseline)

Delta <- Delta/rep(TIME_STUDY,8)

Delta <- Delta %>% bind_cols(UMSARS_Delta)


Correlations <- Delta %>% cor(method = "spearman")

round(Correlations, 3)

get_p_value <- function(x, y) {
  cor_test <- cor.test(x, y, method = "spearman")
  broom::tidy(cor_test)$p.value
}

p_value_matrix <- matrix(NA, ncol = 9, nrow = 9, 
                         dimnames = list(names(Delta), names(Delta)))


for (i in 1:ncol(Delta)) {
  for (j in 1:ncol(Delta)) {
    p_value_matrix[i, j] <- get_p_value(Delta[[i]], Delta[[j]])
  }
}

Correlations <- round(Correlations, 3)
p_value_matrix <- round(p_value_matrix, 3)


combined_matrix <- matrix(NA, ncol = 9, nrow = 9, 
                          dimnames = list(names(Delta), names(Delta)))


for (i in 1:ncol(Delta)) {
  for (j in 1:ncol(Delta)) {
    combined_matrix[i, j] <- paste0(Correlations[i, j], ", ", p_value_matrix[i, j])
  }
}










# MEAN DIFFUSIVITY  ***********************

MD_Takeda <- readxl::read_xlsx("Source/MD_Takeda.xlsx")
MD_Takeda$DateIRM <-  as.Date(MD_Takeda$DateIRM)
names(MD_Takeda)

MD_Takeda <- MD_Takeda %>% select(NUM, DateIRM,
                                  MD_L_Puta, 
                                  MD_R_Puta, 
                                  MD_L_Cerebell_WM,MD_R_Cerebell_WM,
                                  MD_L_Cerebell_GM, MD_R_Cerebell_GM,
                                  MD_Midbrainn,
                                  MD_Pons,
                                  MD_Medulla)

MD_Takeda <- MD_Takeda %>% arrange(NUM, DateIRM) %>% group_by(NUM) %>%
  mutate(TIME_STUDY=time_length(difftime(DateIRM, lag(DateIRM) ), "years"))  %>%
  select(NUM, DateIRM, TIME_STUDY,
         MD_L_Puta, 
         MD_R_Puta, 
         MD_L_Cerebell_WM,MD_R_Cerebell_WM,
         MD_L_Cerebell_GM, MD_R_Cerebell_GM,
         MD_Midbrainn,
         MD_Pons,
         MD_Medulla)


MD_Takeda <- MD_Takeda %>% mutate(TIME_STUDY=ifelse(is.na(TIME_STUDY), 0, TIME_STUDY))

MD_Takeda <- MD_Takeda %>% mutate(TIME_STUDY=cumsum(TIME_STUDY))

MD_Takeda <- MD_Takeda %>% group_by(NUM) %>% count() %>%
  left_join(MD_Takeda) %>%
  select(NUM, DateIRM, TIME_STUDY, n,
         MD_L_Puta, 
         MD_R_Puta, 
         MD_L_Cerebell_WM,MD_R_Cerebell_WM,
         MD_L_Cerebell_GM, MD_R_Cerebell_GM,
         MD_Midbrainn,
         MD_Pons,
         MD_Medulla)


length(unique(MD_Takeda$NUM)) # 24

MD_Takeda %>% ungroup() %>% group_by(n) %>% count()

MD_Takeda %>% mutate(Year= ifelse(TIME_STUDY<=0.5, 0,
                                  ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                                         ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                                ifelse(TIME_STUDY>=2.5 ,3, NA))))) %>%
  select(NUM, Year) %>% group_by(NUM) %>%
  mutate(All=paste0(Year, collapse=","))

range(MD_Takeda$TIME_STUDY)


MD_Takeda <- MD_Takeda %>% mutate(CerebellumWhite=(MD_L_Cerebell_WM+MD_R_Cerebell_WM)/2) %>%
  mutate(CerebellumGray=(MD_L_Cerebell_GM+MD_R_Cerebell_GM)/2) %>%
  mutate(CerebellumTotal=0.82*CerebellumGray+0.18*CerebellumWhite)

names(MD_Takeda)


Baseline <- MD_Takeda %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, MD_L_Puta, MD_R_Puta, CerebellumWhite,CerebellumTotal, MD_Midbrainn,MD_Pons,MD_Medulla) 

Date_Baseline <- MD_Takeda %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, DateIRM)

Year_1 <- MD_Takeda %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>%
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(NUM, MD_L_Puta, MD_R_Puta, CerebellumWhite,CerebellumTotal, MD_Midbrainn,MD_Pons,MD_Medulla) 


Date_Year_1 <- MD_Takeda %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>%
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>% select(NUM, DateIRM)

TIME_STUDY <- MD_Takeda %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>% 
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY)


Pats <- Year_1 %>% select(NUM) %>% 
  inner_join(Baseline %>% select(NUM)) %>% 
  inner_join(Original_UMSARS_Delta %>% select(NUM)) %>%
  arrange(NUM)


Year_1 <- Pats %>% left_join(Year_1) %>% select(-NUM)
Baseline <- Pats %>% left_join(Baseline) %>% select(-NUM)
TIME_STUDY <- Pats %>% left_join(TIME_STUDY) %>% select(-NUM)
UMSARS_Delta <- Pats %>% left_join(Original_UMSARS_Delta) %>% select(-NUM)



Delta <- (Year_1 - Baseline)

Delta <- Delta/rep(TIME_STUDY,7)

Delta <- Delta %>% bind_cols(UMSARS_Delta)


Correlations <- Delta %>% cor(method = "spearman")

round(Correlations, 3)

get_p_value <- function(x, y) {
  cor_test <- cor.test(x, y, method = "spearman")
  broom::tidy(cor_test)$p.value
}

p_value_matrix <- matrix(NA, ncol = 8, nrow = 8, 
                         dimnames = list(names(Delta), names(Delta)))


for (i in 1:ncol(Delta)) {
  for (j in 1:ncol(Delta)) {
    p_value_matrix[i, j] <- get_p_value(Delta[[i]], Delta[[j]])
  }
}

Correlations <- round(Correlations, 3)
p_value_matrix <- round(p_value_matrix, 3)


combined_matrix <- matrix(NA, ncol = 8, nrow = 8, 
                          dimnames = list(names(Delta), names(Delta)))


for (i in 1:ncol(Delta)) {
  for (j in 1:ncol(Delta)) {
    combined_matrix[i, j] <- paste0(Correlations[i, j], ", ", p_value_matrix[i, j])
  }
}












# FRACTIONAL ANISOTROPY ***********************




FA_Takeda <- readxl::read_xlsx("Source/FA_Takeda.xlsx")
FA_Takeda$DateIRM <-  as.Date(FA_Takeda$DateIRM)
names(FA_Takeda)

FA_Takeda <- FA_Takeda %>% select(NUM, DateIRM,
                                  FA_L_Puta, 
                                  FA_R_Puta, 
                                  FA_L_Cerebell_WM,FA_R_Cerebell_WM,
                                  FA_L_Cerebell_GM, FA_R_Cerebell_GM,
                                  FA_Midbrainn,
                                  FA_Pons,
                                  FA_Medulla)

FA_Takeda <- FA_Takeda %>% arrange(NUM, DateIRM) %>% group_by(NUM) %>%
  mutate(TIME_STUDY=time_length(difftime(DateIRM, lag(DateIRM) ), "years"))  %>%
  select(NUM, DateIRM, TIME_STUDY,
         FA_L_Puta, 
         FA_R_Puta, 
         FA_L_Cerebell_WM,FA_R_Cerebell_WM,
         FA_L_Cerebell_GM, FA_R_Cerebell_GM,
         FA_Midbrainn,
         FA_Pons,
         FA_Medulla)


FA_Takeda <- FA_Takeda %>% mutate(TIME_STUDY=ifelse(is.na(TIME_STUDY), 0, TIME_STUDY))

FA_Takeda <- FA_Takeda %>% mutate(TIME_STUDY=cumsum(TIME_STUDY))

FA_Takeda <- FA_Takeda %>% group_by(NUM) %>% count() %>%
  left_join(FA_Takeda) %>%
  select(NUM, DateIRM, TIME_STUDY, n,
         FA_L_Puta, 
         FA_R_Puta, 
         FA_L_Cerebell_WM,FA_R_Cerebell_WM,
         FA_L_Cerebell_GM, FA_R_Cerebell_GM,
         FA_Midbrainn,
         FA_Pons,
         FA_Medulla)


length(unique(FA_Takeda$NUM)) # 24

FA_Takeda %>% ungroup() %>% group_by(n) %>% count()

FA_Takeda %>% mutate(Year= ifelse(TIME_STUDY<=0.5, 0,
                                  ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                                         ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                                ifelse(TIME_STUDY>=2.5 ,3, NA))))) %>%
  select(NUM, Year) %>% group_by(NUM) %>%
  mutate(All=paste0(Year, collapse=","))

range(FA_Takeda$TIME_STUDY)


FA_Takeda <- FA_Takeda %>% mutate(CerebellumWhite=(FA_L_Cerebell_WM+FA_R_Cerebell_WM)/2) %>%
  mutate(CerebellumGray=(FA_L_Cerebell_GM+FA_R_Cerebell_GM)/2) %>%
  mutate(CerebellumTotal=0.82*CerebellumGray+0.18*CerebellumWhite)


names(FA_Takeda)



Baseline <- FA_Takeda %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, FA_L_Puta, FA_R_Puta, CerebellumWhite,CerebellumTotal, FA_Midbrainn,FA_Pons,FA_Medulla) 

Date_Baseline <- FA_Takeda %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, DateIRM)

Year_1 <- FA_Takeda %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>%
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(NUM, FA_L_Puta, FA_R_Puta, CerebellumWhite,CerebellumTotal, FA_Midbrainn,FA_Pons,FA_Medulla) 


Date_Year_1 <- FA_Takeda %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>%
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>% select(NUM, DateIRM)

TIME_STUDY <- FA_Takeda %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>% 
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY)


Pats <- Year_1 %>% select(NUM) %>% 
  inner_join(Baseline %>% select(NUM)) %>% 
  inner_join(Original_UMSARS_Delta %>% select(NUM)) %>%
  arrange(NUM)


Year_1 <- Pats %>% left_join(Year_1) %>% select(-NUM)
Baseline <- Pats %>% left_join(Baseline) %>% select(-NUM)
TIME_STUDY <- Pats %>% left_join(TIME_STUDY) %>% select(-NUM)
UMSARS_Delta <- Pats %>% left_join(Original_UMSARS_Delta) %>% select(-NUM)



Delta <- (Year_1 - Baseline)

Delta <- Delta/rep(TIME_STUDY,7)

Delta <- Delta %>% bind_cols(UMSARS_Delta)


Correlations <- Delta %>% cor(method = "spearman")

round(Correlations, 3)

get_p_value <- function(x, y) {
  cor_test <- cor.test(x, y, method = "spearman")
  broom::tidy(cor_test)$p.value
}

p_value_matrix <- matrix(NA, ncol = 8, nrow = 8, 
                         dimnames = list(names(Delta), names(Delta)))


for (i in 1:ncol(Delta)) {
  for (j in 1:ncol(Delta)) {
    p_value_matrix[i, j] <- get_p_value(Delta[[i]], Delta[[j]])
  }
}

Correlations <- round(Correlations, 3)
p_value_matrix <- round(p_value_matrix, 3)


combined_matrix <- matrix(NA, ncol = 8, nrow = 8, 
                          dimnames = list(names(Delta), names(Delta)))


for (i in 1:ncol(Delta)) {
  for (j in 1:ncol(Delta)) {
    combined_matrix[i, j] <- paste0(Correlations[i, j], ", ", p_value_matrix[i, j])
  }
}





# ------------------------
# CLINICAL CORRELATION COMPASS MRIs -------------------


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

COMPASS <- COMPASS %>% select(NUM, DATECONSULT, DomainTotals)




Baseline_COMPASS <- COMPASS %>% inner_join(Date_Baseline) %>% 
  mutate(Elapsed=abs(as.numeric(DATECONSULT-DateIRM))) %>%
  group_by(NUM) %>% drop_na() %>% filter(Elapsed==min(Elapsed)) %>% distinct() %>% 
  select(NUM, DATECONSULT, DomainTotals , Elapsed) %>% 
  rename("Baseline_DATECONSULT"="DATECONSULT", 
         "Baseline_DomainTotals"="DomainTotals", 
         "Baseline_Elapsed"="Elapsed")

Year_1_COMPASS <- COMPASS %>% inner_join(Date_Year_1) %>% 
  mutate(Elapsed=abs(as.numeric(DATECONSULT-DateIRM))) %>%
  group_by(NUM) %>% drop_na() %>% filter(Elapsed==min(Elapsed)) %>% distinct() %>% 
  select(NUM, DATECONSULT, DomainTotals, Elapsed) %>% 
  rename("Year1_DATECONSULT"="DATECONSULT", 
         "Year1_DomainTotals"="DomainTotals", 
         "Year1_Elapsed"="Elapsed")



Year_1_COMPASS %>% inner_join(Baseline_COMPASS) %>%
  filter(Year1_DATECONSULT!=Baseline_DATECONSULT) %>% ungroup() %>%
  mutate(Delta=Year1_DomainTotals-Baseline_DomainTotals) %>%
  mutate(TIME_STUDY=time_length(difftime(Year1_DATECONSULT , Baseline_DATECONSULT)  , "years")) %>%
  #summarise(mean=mean(TIME_STUDY*12), sd=sd(TIME_STUDY*12)) #9.14  3.22
  ggplot(aes(TIME_STUDY*12)) +
  geom_density(size=2, fill="midnightblue", alpha=0.5)+
  xlim(0,24) + xlab("\n Number of Months Elapsed \n Between The 2 COMPASS Records") +
  ylab("Patient density \n") +
  theme_minimal()

COMPASS_Delta <- Year_1_COMPASS %>% inner_join(Baseline_COMPASS) %>%
  filter(Year1_DATECONSULT!=Baseline_DATECONSULT) %>% ungroup() %>%
  mutate(Delta=Year1_DomainTotals-Baseline_DomainTotals) %>%
  mutate(TIME_STUDY=time_length(difftime(Year1_DATECONSULT , Baseline_DATECONSULT)  , "years"))  %>%
  mutate(Delta=Delta/TIME_STUDY) %>%
  select(NUM, Delta) # 22


Original_COMPASS_Delta <- COMPASS_Delta








# VOLUMETRIC ORIGINAL ***********************

irmForPaulo <- readRDS("Source/irmForPaulo.rds")
names(irmForPaulo)

irmForPaulo <- irmForPaulo %>% select(NUM, DateIRM,
                                      Left_Putamen, 
                                      Right_Putamen, 
                                      Left_Cerebellum_White_Matter,Right_Cerebellum_White_Matter,
                                      Left_Cerebellum_Cortex, Right_Cerebellum_Cortex,
                                      Midbrain,
                                      Pons,
                                      Medulla,
                                      CerebralWhiteMatterVol, TotalGrayVol)

irmForPaulo <- irmForPaulo %>% arrange(NUM, DateIRM) %>% group_by(NUM) %>%
  mutate(TIME_STUDY=time_length(difftime(DateIRM, lag(DateIRM) ), "years"))  %>%
  select(NUM, DateIRM, TIME_STUDY,
         Left_Putamen, 
         Right_Putamen, 
         Left_Cerebellum_White_Matter,Right_Cerebellum_White_Matter,
         Left_Cerebellum_Cortex, Right_Cerebellum_Cortex,
         Midbrain,
         Pons,
         Medulla,
         CerebralWhiteMatterVol, TotalGrayVol)


irmForPaulo <- irmForPaulo %>% mutate(TIME_STUDY=ifelse(is.na(TIME_STUDY), 0, TIME_STUDY))

irmForPaulo <- irmForPaulo %>% mutate(TIME_STUDY=cumsum(TIME_STUDY))

irmForPaulo <- irmForPaulo %>% group_by(NUM) %>% count() %>%
  left_join(irmForPaulo) %>%
  select(NUM, DateIRM, TIME_STUDY, n,
         Left_Putamen, 
         Right_Putamen, 
         Left_Cerebellum_White_Matter,Right_Cerebellum_White_Matter,
         Left_Cerebellum_Cortex, Right_Cerebellum_Cortex,
         Midbrain,
         Pons,
         Medulla,
         CerebralWhiteMatterVol, TotalGrayVol)

irmForPaulo$Medulla <- as.numeric(irmForPaulo$Medulla)
irmForPaulo$Midbrain <- as.numeric(irmForPaulo$Midbrain)
irmForPaulo$CerebralWhiteMatterVol <- as.numeric(irmForPaulo$CerebralWhiteMatterVol)

irmForPaulo <- irmForPaulo %>% mutate(CerebellumTotal=Left_Cerebellum_White_Matter+
                                        Right_Cerebellum_White_Matter+
                                        Left_Cerebellum_Cortex +
                                        Right_Cerebellum_Cortex ) %>%
  mutate(CerebellumWhite=Left_Cerebellum_White_Matter+Right_Cerebellum_White_Matter) %>%
  mutate(Total=as.numeric(CerebralWhiteMatterVol)+as.numeric(TotalGrayVol)) 

names(irmForPaulo)

Baseline <- irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, Left_Putamen, Right_Putamen, CerebellumWhite,CerebellumTotal, Midbrain,Pons,Medulla,Total) 

Date_Baseline <- irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, DateIRM)

Year_1 <- irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>%
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(NUM, Left_Putamen, Right_Putamen, CerebellumWhite,CerebellumTotal, Midbrain,Pons,Medulla,Total) 


Date_Year_1 <- irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>%
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>% select(NUM, DateIRM)

TIME_STUDY <- irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>% 
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY)


Pats <- Year_1 %>% select(NUM) %>% 
  inner_join(Baseline %>% select(NUM)) %>% 
  inner_join(COMPASS_Delta %>% select(NUM)) %>%
  arrange(NUM)


Year_1 <- Pats %>% left_join(Year_1) %>% select(-NUM)
Baseline <- Pats %>% left_join(Baseline) %>% select(-NUM)
TIME_STUDY <- Pats %>% left_join(TIME_STUDY) %>% select(-NUM)
COMPASS_Delta <- Pats %>% left_join(COMPASS_Delta) %>% select(-NUM)


Delta <- (Year_1 - Baseline)

Delta <- Delta/rep(TIME_STUDY,8)

Delta <- Delta %>% bind_cols(COMPASS_Delta)


Correlations <- Delta %>% cor(method = "spearman")

round(Correlations, 3)

get_p_value <- function(x, y) {
  cor_test <- cor.test(x, y, method = "spearman")
  broom::tidy(cor_test)$p.value
}

p_value_matrix <- matrix(NA, ncol = 9, nrow = 9, 
                         dimnames = list(names(Delta), names(Delta)))


for (i in 1:ncol(Delta)) {
  for (j in 1:ncol(Delta)) {
    p_value_matrix[i, j] <- get_p_value(Delta[[i]], Delta[[j]])
  }
}

Correlations <- round(Correlations, 3)
p_value_matrix <- round(p_value_matrix, 3)


combined_matrix <- matrix(NA, ncol = 9, nrow = 9, 
                          dimnames = list(names(Delta), names(Delta)))


for (i in 1:ncol(Delta)) {
  for (j in 1:ncol(Delta)) {
    combined_matrix[i, j] <- paste0(Correlations[i, j], ", ", p_value_matrix[i, j])
  }
}





# VOLUMETRIC NORMALIZED ***********************


irmForPaulo <- readRDS("Source/irmForPaulo.rds")
names(irmForPaulo)

irmForPaulo <- irmForPaulo %>% select(NUM, DateIRM,
                                      Left_Putamen, 
                                      Right_Putamen, 
                                      Left_Cerebellum_White_Matter,Right_Cerebellum_White_Matter,
                                      Left_Cerebellum_Cortex, Right_Cerebellum_Cortex,
                                      Midbrain,
                                      Pons,
                                      Medulla,
                                      CerebralWhiteMatterVol, TotalGrayVol)

irmForPaulo <- irmForPaulo %>% arrange(NUM, DateIRM) %>% group_by(NUM) %>%
  mutate(TIME_STUDY=time_length(difftime(DateIRM, lag(DateIRM) ), "years"))  %>%
  select(NUM, DateIRM, TIME_STUDY,
         Left_Putamen, 
         Right_Putamen, 
         Left_Cerebellum_White_Matter,Right_Cerebellum_White_Matter,
         Left_Cerebellum_Cortex, Right_Cerebellum_Cortex,
         Midbrain,
         Pons,
         Medulla,
         CerebralWhiteMatterVol, TotalGrayVol)


irmForPaulo <- irmForPaulo %>% mutate(TIME_STUDY=ifelse(is.na(TIME_STUDY), 0, TIME_STUDY))

irmForPaulo <- irmForPaulo %>% mutate(TIME_STUDY=cumsum(TIME_STUDY))

irmForPaulo <- irmForPaulo %>% group_by(NUM) %>% count() %>%
  left_join(irmForPaulo) %>%
  select(NUM, DateIRM, TIME_STUDY, n,
         Left_Putamen, 
         Right_Putamen, 
         Left_Cerebellum_White_Matter,Right_Cerebellum_White_Matter,
         Left_Cerebellum_Cortex, Right_Cerebellum_Cortex,
         Midbrain,
         Pons,
         Medulla,
         CerebralWhiteMatterVol, TotalGrayVol)

irmForPaulo$Medulla <- as.numeric(irmForPaulo$Medulla)
irmForPaulo$Midbrain <- as.numeric(irmForPaulo$Midbrain)
irmForPaulo$CerebralWhiteMatterVol <- as.numeric(irmForPaulo$CerebralWhiteMatterVol)

irmForPaulo <- irmForPaulo %>% mutate(CerebellumTotal=Left_Cerebellum_White_Matter+
                                        Right_Cerebellum_White_Matter+
                                        Left_Cerebellum_Cortex +
                                        Right_Cerebellum_Cortex ) %>%
  mutate(CerebellumWhite=Left_Cerebellum_White_Matter+Right_Cerebellum_White_Matter) %>%
  mutate(Total=as.numeric(CerebralWhiteMatterVol)+as.numeric(TotalGrayVol)) 


TotalCranial <- readRDS("Source/irmForPaulo.rds")
TotalCranial <- TotalCranial %>% arrange(NUM, DateIRM)


irmForPaulo <- irmForPaulo %>% left_join(TotalCranial%>% select(NUM, DateIRM, EstimatedTotalIntraCranialVol))
names(irmForPaulo)

irmForPaulo <- irmForPaulo %>% mutate(Left_Putamen=Left_Putamen/EstimatedTotalIntraCranialVol) %>%
  mutate(Right_Putamen =Right_Putamen /EstimatedTotalIntraCranialVol) %>%
  mutate(CerebellumWhite =CerebellumWhite /EstimatedTotalIntraCranialVol) %>%
  mutate(CerebellumTotal =CerebellumTotal /EstimatedTotalIntraCranialVol) %>%
  mutate(Midbrain=Midbrain/EstimatedTotalIntraCranialVol) %>%
  mutate(Pons =Pons /EstimatedTotalIntraCranialVol) %>%
  mutate(Medulla =Medulla /EstimatedTotalIntraCranialVol) %>%
  mutate(Total =Total /EstimatedTotalIntraCranialVol) 


names(irmForPaulo)

Baseline <- irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, Left_Putamen, Right_Putamen, CerebellumWhite,CerebellumTotal, Midbrain,Pons,Medulla,Total) 

Date_Baseline <- irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, DateIRM)

Year_1 <- irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>%
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(NUM, Left_Putamen, Right_Putamen, CerebellumWhite,CerebellumTotal, Midbrain,Pons,Medulla,Total) 


Date_Year_1 <- irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>%
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>% select(NUM, DateIRM)

TIME_STUDY <- irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>% 
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY)




TIME_STUDY %>% 
  # summarise(mean=mean(TIME_STUDY*12), sd=sd(TIME_STUDY*12)) #10.7  2.49
  ggplot(aes(TIME_STUDY*12)) +
  geom_density(size=2, fill="firebrick", alpha=0.5)+
  xlim(0,24) + xlab("\n Number of Months Elapsed \n Between The 2 MRI Records") +
  ylab("Patient density \n") +
  theme_minimal()

Pats <- Year_1 %>% select(NUM) %>% 
  inner_join(Baseline %>% select(NUM)) %>% 
  inner_join(Original_COMPASS_Delta %>% select(NUM)) %>%
  arrange(NUM)


Year_1 <- Pats %>% left_join(Year_1) %>% select(-NUM)
Baseline <- Pats %>% left_join(Baseline) %>% select(-NUM)
TIME_STUDY <- Pats %>% left_join(TIME_STUDY) %>% select(-NUM)
COMPASS_Delta <- Pats %>% left_join(COMPASS_Delta) %>% select(-NUM)


Delta <- (Year_1 - Baseline)

Delta <- Delta/rep(TIME_STUDY,8)

Delta <- Delta %>% bind_cols(COMPASS_Delta)


Correlations <- Delta %>% cor(method = "spearman")

round(Correlations, 3)

get_p_value <- function(x, y) {
  cor_test <- cor.test(x, y, method = "spearman")
  broom::tidy(cor_test)$p.value
}

p_value_matrix <- matrix(NA, ncol = 9, nrow = 9, 
                         dimnames = list(names(Delta), names(Delta)))


for (i in 1:ncol(Delta)) {
  for (j in 1:ncol(Delta)) {
    p_value_matrix[i, j] <- get_p_value(Delta[[i]], Delta[[j]])
  }
}

Correlations <- round(Correlations, 3)
p_value_matrix <- round(p_value_matrix, 3)


combined_matrix <- matrix(NA, ncol = 9, nrow = 9, 
                          dimnames = list(names(Delta), names(Delta)))


for (i in 1:ncol(Delta)) {
  for (j in 1:ncol(Delta)) {
    combined_matrix[i, j] <- paste0(Correlations[i, j], ", ", p_value_matrix[i, j])
  }
}










# MEAN DIFFUSIVITY  ***********************

MD_Takeda <- readxl::read_xlsx("Source/MD_Takeda.xlsx")
MD_Takeda$DateIRM <-  as.Date(MD_Takeda$DateIRM)
names(MD_Takeda)

MD_Takeda <- MD_Takeda %>% select(NUM, DateIRM,
                                  MD_L_Puta, 
                                  MD_R_Puta, 
                                  MD_L_Cerebell_WM,MD_R_Cerebell_WM,
                                  MD_L_Cerebell_GM, MD_R_Cerebell_GM,
                                  MD_Midbrainn,
                                  MD_Pons,
                                  MD_Medulla)

MD_Takeda <- MD_Takeda %>% arrange(NUM, DateIRM) %>% group_by(NUM) %>%
  mutate(TIME_STUDY=time_length(difftime(DateIRM, lag(DateIRM) ), "years"))  %>%
  select(NUM, DateIRM, TIME_STUDY,
         MD_L_Puta, 
         MD_R_Puta, 
         MD_L_Cerebell_WM,MD_R_Cerebell_WM,
         MD_L_Cerebell_GM, MD_R_Cerebell_GM,
         MD_Midbrainn,
         MD_Pons,
         MD_Medulla)


MD_Takeda <- MD_Takeda %>% mutate(TIME_STUDY=ifelse(is.na(TIME_STUDY), 0, TIME_STUDY))

MD_Takeda <- MD_Takeda %>% mutate(TIME_STUDY=cumsum(TIME_STUDY))

MD_Takeda <- MD_Takeda %>% group_by(NUM) %>% count() %>%
  left_join(MD_Takeda) %>%
  select(NUM, DateIRM, TIME_STUDY, n,
         MD_L_Puta, 
         MD_R_Puta, 
         MD_L_Cerebell_WM,MD_R_Cerebell_WM,
         MD_L_Cerebell_GM, MD_R_Cerebell_GM,
         MD_Midbrainn,
         MD_Pons,
         MD_Medulla)


length(unique(MD_Takeda$NUM)) # 24

MD_Takeda %>% ungroup() %>% group_by(n) %>% count()

MD_Takeda %>% mutate(Year= ifelse(TIME_STUDY<=0.5, 0,
                                  ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                                         ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                                ifelse(TIME_STUDY>=2.5 ,3, NA))))) %>%
  select(NUM, Year) %>% group_by(NUM) %>%
  mutate(All=paste0(Year, collapse=","))

range(MD_Takeda$TIME_STUDY)


MD_Takeda <- MD_Takeda %>% mutate(CerebellumWhite=(MD_L_Cerebell_WM+MD_R_Cerebell_WM)/2) %>%
  mutate(CerebellumGray=(MD_L_Cerebell_GM+MD_R_Cerebell_GM)/2) %>%
  mutate(CerebellumTotal=0.82*CerebellumGray+0.18*CerebellumWhite)

names(MD_Takeda)


Baseline <- MD_Takeda %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, MD_L_Puta, MD_R_Puta, CerebellumWhite,CerebellumTotal, MD_Midbrainn,MD_Pons,MD_Medulla) 

Date_Baseline <- MD_Takeda %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, DateIRM)

Year_1 <- MD_Takeda %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>%
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(NUM, MD_L_Puta, MD_R_Puta, CerebellumWhite,CerebellumTotal, MD_Midbrainn,MD_Pons,MD_Medulla) 


Date_Year_1 <- MD_Takeda %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>%
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>% select(NUM, DateIRM)

TIME_STUDY <- MD_Takeda %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>% 
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY)


Pats <- Year_1 %>% select(NUM) %>% 
  inner_join(Baseline %>% select(NUM)) %>% 
  inner_join(COMPASS_Delta %>% select(NUM)) %>%
  arrange(NUM)


Year_1 <- Pats %>% left_join(Year_1) %>% select(-NUM)
Baseline <- Pats %>% left_join(Baseline) %>% select(-NUM)
TIME_STUDY <- Pats %>% left_join(TIME_STUDY) %>% select(-NUM)
COMPASS_Delta <- Pats %>% left_join(COMPASS_Delta) %>% select(-NUM)



Delta <- (Year_1 - Baseline)

Delta <- Delta/rep(TIME_STUDY,7)

Delta <- Delta %>% bind_cols(COMPASS_Delta)


Correlations <- Delta %>% cor(method = "spearman")

round(Correlations, 3)

get_p_value <- function(x, y) {
  cor_test <- cor.test(x, y, method = "spearman")
  broom::tidy(cor_test)$p.value
}

p_value_matrix <- matrix(NA, ncol = 8, nrow = 8, 
                         dimnames = list(names(Delta), names(Delta)))


for (i in 1:ncol(Delta)) {
  for (j in 1:ncol(Delta)) {
    p_value_matrix[i, j] <- get_p_value(Delta[[i]], Delta[[j]])
  }
}

Correlations <- round(Correlations, 3)
p_value_matrix <- round(p_value_matrix, 3)


combined_matrix <- matrix(NA, ncol = 8, nrow = 8, 
                          dimnames = list(names(Delta), names(Delta)))


for (i in 1:ncol(Delta)) {
  for (j in 1:ncol(Delta)) {
    combined_matrix[i, j] <- paste0(Correlations[i, j], ", ", p_value_matrix[i, j])
  }
}












# FRACTIONAL ANISOTROPY ***********************




FA_Takeda <- readxl::read_xlsx("Source/FA_Takeda.xlsx")
FA_Takeda$DateIRM <-  as.Date(FA_Takeda$DateIRM)
names(FA_Takeda)

FA_Takeda <- FA_Takeda %>% select(NUM, DateIRM,
                                  FA_L_Puta, 
                                  FA_R_Puta, 
                                  FA_L_Cerebell_WM,FA_R_Cerebell_WM,
                                  FA_L_Cerebell_GM, FA_R_Cerebell_GM,
                                  FA_Midbrainn,
                                  FA_Pons,
                                  FA_Medulla)

FA_Takeda <- FA_Takeda %>% arrange(NUM, DateIRM) %>% group_by(NUM) %>%
  mutate(TIME_STUDY=time_length(difftime(DateIRM, lag(DateIRM) ), "years"))  %>%
  select(NUM, DateIRM, TIME_STUDY,
         FA_L_Puta, 
         FA_R_Puta, 
         FA_L_Cerebell_WM,FA_R_Cerebell_WM,
         FA_L_Cerebell_GM, FA_R_Cerebell_GM,
         FA_Midbrainn,
         FA_Pons,
         FA_Medulla)


FA_Takeda <- FA_Takeda %>% mutate(TIME_STUDY=ifelse(is.na(TIME_STUDY), 0, TIME_STUDY))

FA_Takeda <- FA_Takeda %>% mutate(TIME_STUDY=cumsum(TIME_STUDY))

FA_Takeda <- FA_Takeda %>% group_by(NUM) %>% count() %>%
  left_join(FA_Takeda) %>%
  select(NUM, DateIRM, TIME_STUDY, n,
         FA_L_Puta, 
         FA_R_Puta, 
         FA_L_Cerebell_WM,FA_R_Cerebell_WM,
         FA_L_Cerebell_GM, FA_R_Cerebell_GM,
         FA_Midbrainn,
         FA_Pons,
         FA_Medulla)


length(unique(FA_Takeda$NUM)) # 24

FA_Takeda %>% ungroup() %>% group_by(n) %>% count()

FA_Takeda %>% mutate(Year= ifelse(TIME_STUDY<=0.5, 0,
                                  ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                                         ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                                ifelse(TIME_STUDY>=2.5 ,3, NA))))) %>%
  select(NUM, Year) %>% group_by(NUM) %>%
  mutate(All=paste0(Year, collapse=","))

range(FA_Takeda$TIME_STUDY)


FA_Takeda <- FA_Takeda %>% mutate(CerebellumWhite=(FA_L_Cerebell_WM+FA_R_Cerebell_WM)/2) %>%
  mutate(CerebellumGray=(FA_L_Cerebell_GM+FA_R_Cerebell_GM)/2) %>%
  mutate(CerebellumTotal=0.82*CerebellumGray+0.18*CerebellumWhite)


names(FA_Takeda)



Baseline <- FA_Takeda %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, FA_L_Puta, FA_R_Puta, CerebellumWhite,CerebellumTotal, FA_Midbrainn,FA_Pons,FA_Medulla) 

Date_Baseline <- FA_Takeda %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, DateIRM)

Year_1 <- FA_Takeda %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>%
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(NUM, FA_L_Puta, FA_R_Puta, CerebellumWhite,CerebellumTotal, FA_Midbrainn,FA_Pons,FA_Medulla) 


Date_Year_1 <- FA_Takeda %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>%
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>% select(NUM, DateIRM)

TIME_STUDY <- FA_Takeda %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>% 
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY)


Pats <- Year_1 %>% select(NUM) %>% 
  inner_join(Baseline %>% select(NUM)) %>% 
  inner_join(COMPASS_Delta %>% select(NUM)) %>%
  arrange(NUM)


Year_1 <- Pats %>% left_join(Year_1) %>% select(-NUM)
Baseline <- Pats %>% left_join(Baseline) %>% select(-NUM)
TIME_STUDY <- Pats %>% left_join(TIME_STUDY) %>% select(-NUM)
COMPASS_Delta <- Pats %>% left_join(COMPASS_Delta) %>% select(-NUM)



Delta <- (Year_1 - Baseline)

Delta <- Delta/rep(TIME_STUDY,7)

Delta <- Delta %>% bind_cols(COMPASS_Delta)


Correlations <- Delta %>% cor(method = "spearman")

round(Correlations, 3)

get_p_value <- function(x, y) {
  cor_test <- cor.test(x, y, method = "spearman")
  broom::tidy(cor_test)$p.value
}

p_value_matrix <- matrix(NA, ncol = 8, nrow = 8, 
                         dimnames = list(names(Delta), names(Delta)))


for (i in 1:ncol(Delta)) {
  for (j in 1:ncol(Delta)) {
    p_value_matrix[i, j] <- get_p_value(Delta[[i]], Delta[[j]])
  }
}

Correlations <- round(Correlations, 3)
p_value_matrix <- round(p_value_matrix, 3)


combined_matrix <- matrix(NA, ncol = 8, nrow = 8, 
                          dimnames = list(names(Delta), names(Delta)))


for (i in 1:ncol(Delta)) {
  for (j in 1:ncol(Delta)) {
    combined_matrix[i, j] <- paste0(Correlations[i, j], ", ", p_value_matrix[i, j])
  }
}



# ------------
# CLINICAL CORRELATION MSA QOL MRIs -------------------

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

mean(SCHRAG$number_na)

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


SCHRAG <- SCHRAG %>% select(NUM, DATECONSULT, SCHRAG_TOT_v2)




Baseline_SCHRAG <- SCHRAG %>% inner_join(Date_Baseline) %>% 
  mutate(Elapsed=abs(as.numeric(DATECONSULT-DateIRM))) %>%
  group_by(NUM) %>% drop_na() %>% filter(Elapsed==min(Elapsed)) %>% distinct() %>% 
  select(NUM, DATECONSULT, SCHRAG_TOT_v2 , Elapsed) %>% 
  rename("Baseline_DATECONSULT"="DATECONSULT", 
         "Baseline_SCHRAG_TOT_v2"="SCHRAG_TOT_v2", 
         "Baseline_Elapsed"="Elapsed")

Year_1_SCHRAG <- SCHRAG %>% inner_join(Date_Year_1) %>% 
  mutate(Elapsed=abs(as.numeric(DATECONSULT-DateIRM))) %>%
  group_by(NUM) %>% drop_na() %>% filter(Elapsed==min(Elapsed)) %>% distinct() %>% 
  select(NUM, DATECONSULT, SCHRAG_TOT_v2, Elapsed) %>% 
  rename("Year1_DATECONSULT"="DATECONSULT", 
         "Year1_SCHRAG_TOT_v2"="SCHRAG_TOT_v2", 
         "Year1_Elapsed"="Elapsed")



Year_1_SCHRAG %>% inner_join(Baseline_SCHRAG) %>%
  filter(Year1_DATECONSULT!=Baseline_DATECONSULT) %>% ungroup() %>%
  mutate(Delta=Year1_SCHRAG_TOT_v2-Baseline_SCHRAG_TOT_v2) %>%
  mutate(TIME_STUDY=time_length(difftime(Year1_DATECONSULT , Baseline_DATECONSULT)  , "years")) %>%
  #summarise(mean=mean(TIME_STUDY*12), sd=sd(TIME_STUDY*12)) #10.9  3.03
  ggplot(aes(TIME_STUDY*12)) +
  geom_density(size=2, fill="midnightblue", alpha=0.5)+
  xlim(0,24) + xlab("\n Number of Months Elapsed \n Between The 2 SCHRAG Records") +
  ylab("Patient density \n") +
  theme_minimal()

SCHRAG_Delta <- Year_1_SCHRAG %>% inner_join(Baseline_SCHRAG) %>%
  filter(Year1_DATECONSULT!=Baseline_DATECONSULT) %>% ungroup() %>%
  mutate(Delta=Year1_SCHRAG_TOT_v2-Baseline_SCHRAG_TOT_v2) %>%
  mutate(TIME_STUDY=time_length(difftime(Year1_DATECONSULT , Baseline_DATECONSULT)  , "years"))  %>%
  mutate(Delta=Delta/TIME_STUDY) %>%
  select(NUM, Delta) # 22


Original_SCHRAG_Delta <- SCHRAG_Delta








# VOLUMETRIC ORIGINAL ***********************

irmForPaulo <- readRDS("Source/irmForPaulo.rds")
names(irmForPaulo)

irmForPaulo <- irmForPaulo %>% select(NUM, DateIRM,
                                      Left_Putamen, 
                                      Right_Putamen, 
                                      Left_Cerebellum_White_Matter,Right_Cerebellum_White_Matter,
                                      Left_Cerebellum_Cortex, Right_Cerebellum_Cortex,
                                      Midbrain,
                                      Pons,
                                      Medulla,
                                      CerebralWhiteMatterVol, TotalGrayVol)

irmForPaulo <- irmForPaulo %>% arrange(NUM, DateIRM) %>% group_by(NUM) %>%
  mutate(TIME_STUDY=time_length(difftime(DateIRM, lag(DateIRM) ), "years"))  %>%
  select(NUM, DateIRM, TIME_STUDY,
         Left_Putamen, 
         Right_Putamen, 
         Left_Cerebellum_White_Matter,Right_Cerebellum_White_Matter,
         Left_Cerebellum_Cortex, Right_Cerebellum_Cortex,
         Midbrain,
         Pons,
         Medulla,
         CerebralWhiteMatterVol, TotalGrayVol)


irmForPaulo <- irmForPaulo %>% mutate(TIME_STUDY=ifelse(is.na(TIME_STUDY), 0, TIME_STUDY))

irmForPaulo <- irmForPaulo %>% mutate(TIME_STUDY=cumsum(TIME_STUDY))

irmForPaulo <- irmForPaulo %>% group_by(NUM) %>% count() %>%
  left_join(irmForPaulo) %>%
  select(NUM, DateIRM, TIME_STUDY, n,
         Left_Putamen, 
         Right_Putamen, 
         Left_Cerebellum_White_Matter,Right_Cerebellum_White_Matter,
         Left_Cerebellum_Cortex, Right_Cerebellum_Cortex,
         Midbrain,
         Pons,
         Medulla,
         CerebralWhiteMatterVol, TotalGrayVol)

irmForPaulo$Medulla <- as.numeric(irmForPaulo$Medulla)
irmForPaulo$Midbrain <- as.numeric(irmForPaulo$Midbrain)
irmForPaulo$CerebralWhiteMatterVol <- as.numeric(irmForPaulo$CerebralWhiteMatterVol)

irmForPaulo <- irmForPaulo %>% mutate(CerebellumTotal=Left_Cerebellum_White_Matter+
                                        Right_Cerebellum_White_Matter+
                                        Left_Cerebellum_Cortex +
                                        Right_Cerebellum_Cortex ) %>%
  mutate(CerebellumWhite=Left_Cerebellum_White_Matter+Right_Cerebellum_White_Matter) %>%
  mutate(Total=as.numeric(CerebralWhiteMatterVol)+as.numeric(TotalGrayVol)) 

names(irmForPaulo)

Baseline <- irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, Left_Putamen, Right_Putamen, CerebellumWhite,CerebellumTotal, Midbrain,Pons,Medulla,Total) 

Date_Baseline <- irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, DateIRM)

Year_1 <- irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>%
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(NUM, Left_Putamen, Right_Putamen, CerebellumWhite,CerebellumTotal, Midbrain,Pons,Medulla,Total) 


Date_Year_1 <- irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>%
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>% select(NUM, DateIRM)

TIME_STUDY <- irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>% 
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY)


Pats <- Year_1 %>% select(NUM) %>% 
  inner_join(Baseline %>% select(NUM)) %>% 
  inner_join(SCHRAG_Delta %>% select(NUM)) %>%
  arrange(NUM)


Year_1 <- Pats %>% left_join(Year_1) %>% select(-NUM)
Baseline <- Pats %>% left_join(Baseline) %>% select(-NUM)
TIME_STUDY <- Pats %>% left_join(TIME_STUDY) %>% select(-NUM)
SCHRAG_Delta <- Pats %>% left_join(SCHRAG_Delta) %>% select(-NUM)


Delta <- (Year_1 - Baseline)

Delta <- Delta/rep(TIME_STUDY,8)

Delta <- Delta %>% bind_cols(SCHRAG_Delta)


Correlations <- Delta %>% cor(method = "spearman")

round(Correlations, 3)

get_p_value <- function(x, y) {
  cor_test <- cor.test(x, y, method = "spearman")
  broom::tidy(cor_test)$p.value
}

p_value_matrix <- matrix(NA, ncol = 9, nrow = 9, 
                         dimnames = list(names(Delta), names(Delta)))


for (i in 1:ncol(Delta)) {
  for (j in 1:ncol(Delta)) {
    p_value_matrix[i, j] <- get_p_value(Delta[[i]], Delta[[j]])
  }
}

Correlations <- round(Correlations, 3)
p_value_matrix <- round(p_value_matrix, 3)


combined_matrix <- matrix(NA, ncol = 9, nrow = 9, 
                          dimnames = list(names(Delta), names(Delta)))


for (i in 1:ncol(Delta)) {
  for (j in 1:ncol(Delta)) {
    combined_matrix[i, j] <- paste0(Correlations[i, j], ", ", p_value_matrix[i, j])
  }
}





# VOLUMETRIC NORMALIZED ***********************


irmForPaulo <- readRDS("Source/irmForPaulo.rds")
names(irmForPaulo)

irmForPaulo <- irmForPaulo %>% select(NUM, DateIRM,
                                      Left_Putamen, 
                                      Right_Putamen, 
                                      Left_Cerebellum_White_Matter,Right_Cerebellum_White_Matter,
                                      Left_Cerebellum_Cortex, Right_Cerebellum_Cortex,
                                      Midbrain,
                                      Pons,
                                      Medulla,
                                      CerebralWhiteMatterVol, TotalGrayVol)

irmForPaulo <- irmForPaulo %>% arrange(NUM, DateIRM) %>% group_by(NUM) %>%
  mutate(TIME_STUDY=time_length(difftime(DateIRM, lag(DateIRM) ), "years"))  %>%
  select(NUM, DateIRM, TIME_STUDY,
         Left_Putamen, 
         Right_Putamen, 
         Left_Cerebellum_White_Matter,Right_Cerebellum_White_Matter,
         Left_Cerebellum_Cortex, Right_Cerebellum_Cortex,
         Midbrain,
         Pons,
         Medulla,
         CerebralWhiteMatterVol, TotalGrayVol)


irmForPaulo <- irmForPaulo %>% mutate(TIME_STUDY=ifelse(is.na(TIME_STUDY), 0, TIME_STUDY))

irmForPaulo <- irmForPaulo %>% mutate(TIME_STUDY=cumsum(TIME_STUDY))

irmForPaulo <- irmForPaulo %>% group_by(NUM) %>% count() %>%
  left_join(irmForPaulo) %>%
  select(NUM, DateIRM, TIME_STUDY, n,
         Left_Putamen, 
         Right_Putamen, 
         Left_Cerebellum_White_Matter,Right_Cerebellum_White_Matter,
         Left_Cerebellum_Cortex, Right_Cerebellum_Cortex,
         Midbrain,
         Pons,
         Medulla,
         CerebralWhiteMatterVol, TotalGrayVol)

irmForPaulo$Medulla <- as.numeric(irmForPaulo$Medulla)
irmForPaulo$Midbrain <- as.numeric(irmForPaulo$Midbrain)
irmForPaulo$CerebralWhiteMatterVol <- as.numeric(irmForPaulo$CerebralWhiteMatterVol)

irmForPaulo <- irmForPaulo %>% mutate(CerebellumTotal=Left_Cerebellum_White_Matter+
                                        Right_Cerebellum_White_Matter+
                                        Left_Cerebellum_Cortex +
                                        Right_Cerebellum_Cortex ) %>%
  mutate(CerebellumWhite=Left_Cerebellum_White_Matter+Right_Cerebellum_White_Matter) %>%
  mutate(Total=as.numeric(CerebralWhiteMatterVol)+as.numeric(TotalGrayVol)) 


TotalCranial <- readRDS("Source/irmForPaulo.rds")
TotalCranial <- TotalCranial %>% arrange(NUM, DateIRM)


irmForPaulo <- irmForPaulo %>% left_join(TotalCranial%>% select(NUM, DateIRM, EstimatedTotalIntraCranialVol))
names(irmForPaulo)

irmForPaulo <- irmForPaulo %>% mutate(Left_Putamen=Left_Putamen/EstimatedTotalIntraCranialVol) %>%
  mutate(Right_Putamen =Right_Putamen /EstimatedTotalIntraCranialVol) %>%
  mutate(CerebellumWhite =CerebellumWhite /EstimatedTotalIntraCranialVol) %>%
  mutate(CerebellumTotal =CerebellumTotal /EstimatedTotalIntraCranialVol) %>%
  mutate(Midbrain=Midbrain/EstimatedTotalIntraCranialVol) %>%
  mutate(Pons =Pons /EstimatedTotalIntraCranialVol) %>%
  mutate(Medulla =Medulla /EstimatedTotalIntraCranialVol) %>%
  mutate(Total =Total /EstimatedTotalIntraCranialVol) 


names(irmForPaulo)

Baseline <- irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, Left_Putamen, Right_Putamen, CerebellumWhite,CerebellumTotal, Midbrain,Pons,Medulla,Total) 

Date_Baseline <- irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, DateIRM)

Year_1 <- irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>%
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(NUM, Left_Putamen, Right_Putamen, CerebellumWhite,CerebellumTotal, Midbrain,Pons,Medulla,Total) 


Date_Year_1 <- irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>%
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>% select(NUM, DateIRM)

TIME_STUDY <- irmForPaulo %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>% 
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY)




TIME_STUDY %>% 
  # summarise(mean=mean(TIME_STUDY*12), sd=sd(TIME_STUDY*12)) #10.7  2.49
  ggplot(aes(TIME_STUDY*12)) +
  geom_density(size=2, fill="firebrick", alpha=0.5)+
  xlim(0,24) + xlab("\n Number of Months Elapsed \n Between The 2 MRI Records") +
  ylab("Patient density \n") +
  theme_minimal()

Pats <- Year_1 %>% select(NUM) %>% 
  inner_join(Baseline %>% select(NUM)) %>% 
  inner_join(SCHRAG_Delta %>% select(NUM)) %>%
  arrange(NUM)


Year_1 <- Pats %>% left_join(Year_1) %>% select(-NUM)
Baseline <- Pats %>% left_join(Baseline) %>% select(-NUM)
TIME_STUDY <- Pats %>% left_join(TIME_STUDY) %>% select(-NUM)
SCHRAG_Delta <- Pats %>% left_join(SCHRAG_Delta) %>% select(-NUM)


Delta <- (Year_1 - Baseline)

Delta <- Delta/rep(TIME_STUDY,8)

Delta <- Delta %>% bind_cols(SCHRAG_Delta)


Correlations <- Delta %>% cor(method = "spearman")

round(Correlations, 3)

get_p_value <- function(x, y) {
  cor_test <- cor.test(x, y, method = "spearman")
  broom::tidy(cor_test)$p.value
}

p_value_matrix <- matrix(NA, ncol = 9, nrow = 9, 
                         dimnames = list(names(Delta), names(Delta)))


for (i in 1:ncol(Delta)) {
  for (j in 1:ncol(Delta)) {
    p_value_matrix[i, j] <- get_p_value(Delta[[i]], Delta[[j]])
  }
}

Correlations <- round(Correlations, 3)
p_value_matrix <- round(p_value_matrix, 3)


combined_matrix <- matrix(NA, ncol = 9, nrow = 9, 
                          dimnames = list(names(Delta), names(Delta)))


for (i in 1:ncol(Delta)) {
  for (j in 1:ncol(Delta)) {
    combined_matrix[i, j] <- paste0(Correlations[i, j], ", ", p_value_matrix[i, j])
  }
}










# MEAN DIFFUSIVITY  ***********************

MD_Takeda <- readxl::read_xlsx("Source/MD_Takeda.xlsx")
MD_Takeda$DateIRM <-  as.Date(MD_Takeda$DateIRM)
names(MD_Takeda)

MD_Takeda <- MD_Takeda %>% select(NUM, DateIRM,
                                  MD_L_Puta, 
                                  MD_R_Puta, 
                                  MD_L_Cerebell_WM,MD_R_Cerebell_WM,
                                  MD_L_Cerebell_GM, MD_R_Cerebell_GM,
                                  MD_Midbrainn,
                                  MD_Pons,
                                  MD_Medulla)

MD_Takeda <- MD_Takeda %>% arrange(NUM, DateIRM) %>% group_by(NUM) %>%
  mutate(TIME_STUDY=time_length(difftime(DateIRM, lag(DateIRM) ), "years"))  %>%
  select(NUM, DateIRM, TIME_STUDY,
         MD_L_Puta, 
         MD_R_Puta, 
         MD_L_Cerebell_WM,MD_R_Cerebell_WM,
         MD_L_Cerebell_GM, MD_R_Cerebell_GM,
         MD_Midbrainn,
         MD_Pons,
         MD_Medulla)


MD_Takeda <- MD_Takeda %>% mutate(TIME_STUDY=ifelse(is.na(TIME_STUDY), 0, TIME_STUDY))

MD_Takeda <- MD_Takeda %>% mutate(TIME_STUDY=cumsum(TIME_STUDY))

MD_Takeda <- MD_Takeda %>% group_by(NUM) %>% count() %>%
  left_join(MD_Takeda) %>%
  select(NUM, DateIRM, TIME_STUDY, n,
         MD_L_Puta, 
         MD_R_Puta, 
         MD_L_Cerebell_WM,MD_R_Cerebell_WM,
         MD_L_Cerebell_GM, MD_R_Cerebell_GM,
         MD_Midbrainn,
         MD_Pons,
         MD_Medulla)


length(unique(MD_Takeda$NUM)) # 24

MD_Takeda %>% ungroup() %>% group_by(n) %>% count()

MD_Takeda %>% mutate(Year= ifelse(TIME_STUDY<=0.5, 0,
                                  ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                                         ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                                ifelse(TIME_STUDY>=2.5 ,3, NA))))) %>%
  select(NUM, Year) %>% group_by(NUM) %>%
  mutate(All=paste0(Year, collapse=","))

range(MD_Takeda$TIME_STUDY)


MD_Takeda <- MD_Takeda %>% mutate(CerebellumWhite=(MD_L_Cerebell_WM+MD_R_Cerebell_WM)/2) %>%
  mutate(CerebellumGray=(MD_L_Cerebell_GM+MD_R_Cerebell_GM)/2) %>%
  mutate(CerebellumTotal=0.82*CerebellumGray+0.18*CerebellumWhite)

names(MD_Takeda)


Baseline <- MD_Takeda %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, MD_L_Puta, MD_R_Puta, CerebellumWhite,CerebellumTotal, MD_Midbrainn,MD_Pons,MD_Medulla) 

Date_Baseline <- MD_Takeda %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, DateIRM)

Year_1 <- MD_Takeda %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>%
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(NUM, MD_L_Puta, MD_R_Puta, CerebellumWhite,CerebellumTotal, MD_Midbrainn,MD_Pons,MD_Medulla) 


Date_Year_1 <- MD_Takeda %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>%
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>% select(NUM, DateIRM)

TIME_STUDY <- MD_Takeda %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>% 
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY)


Pats <- Year_1 %>% select(NUM) %>% 
  inner_join(Baseline %>% select(NUM)) %>% 
  inner_join(SCHRAG_Delta %>% select(NUM)) %>%
  arrange(NUM)


Year_1 <- Pats %>% left_join(Year_1) %>% select(-NUM)
Baseline <- Pats %>% left_join(Baseline) %>% select(-NUM)
TIME_STUDY <- Pats %>% left_join(TIME_STUDY) %>% select(-NUM)
SCHRAG_Delta <- Pats %>% left_join(SCHRAG_Delta) %>% select(-NUM)



Delta <- (Year_1 - Baseline)

Delta <- Delta/rep(TIME_STUDY,7)

Delta <- Delta %>% bind_cols(SCHRAG_Delta)


Correlations <- Delta %>% cor(method = "spearman")

round(Correlations, 3)

get_p_value <- function(x, y) {
  cor_test <- cor.test(x, y, method = "spearman")
  broom::tidy(cor_test)$p.value
}

p_value_matrix <- matrix(NA, ncol = 8, nrow = 8, 
                         dimnames = list(names(Delta), names(Delta)))


for (i in 1:ncol(Delta)) {
  for (j in 1:ncol(Delta)) {
    p_value_matrix[i, j] <- get_p_value(Delta[[i]], Delta[[j]])
  }
}

Correlations <- round(Correlations, 3)
p_value_matrix <- round(p_value_matrix, 3)


combined_matrix <- matrix(NA, ncol = 8, nrow = 8, 
                          dimnames = list(names(Delta), names(Delta)))


for (i in 1:ncol(Delta)) {
  for (j in 1:ncol(Delta)) {
    combined_matrix[i, j] <- paste0(Correlations[i, j], ", ", p_value_matrix[i, j])
  }
}












# FRACTIONAL ANISOTROPY ***********************




FA_Takeda <- readxl::read_xlsx("Source/FA_Takeda.xlsx")
FA_Takeda$DateIRM <-  as.Date(FA_Takeda$DateIRM)
names(FA_Takeda)

FA_Takeda <- FA_Takeda %>% select(NUM, DateIRM,
                                  FA_L_Puta, 
                                  FA_R_Puta, 
                                  FA_L_Cerebell_WM,FA_R_Cerebell_WM,
                                  FA_L_Cerebell_GM, FA_R_Cerebell_GM,
                                  FA_Midbrainn,
                                  FA_Pons,
                                  FA_Medulla)

FA_Takeda <- FA_Takeda %>% arrange(NUM, DateIRM) %>% group_by(NUM) %>%
  mutate(TIME_STUDY=time_length(difftime(DateIRM, lag(DateIRM) ), "years"))  %>%
  select(NUM, DateIRM, TIME_STUDY,
         FA_L_Puta, 
         FA_R_Puta, 
         FA_L_Cerebell_WM,FA_R_Cerebell_WM,
         FA_L_Cerebell_GM, FA_R_Cerebell_GM,
         FA_Midbrainn,
         FA_Pons,
         FA_Medulla)


FA_Takeda <- FA_Takeda %>% mutate(TIME_STUDY=ifelse(is.na(TIME_STUDY), 0, TIME_STUDY))

FA_Takeda <- FA_Takeda %>% mutate(TIME_STUDY=cumsum(TIME_STUDY))

FA_Takeda <- FA_Takeda %>% group_by(NUM) %>% count() %>%
  left_join(FA_Takeda) %>%
  select(NUM, DateIRM, TIME_STUDY, n,
         FA_L_Puta, 
         FA_R_Puta, 
         FA_L_Cerebell_WM,FA_R_Cerebell_WM,
         FA_L_Cerebell_GM, FA_R_Cerebell_GM,
         FA_Midbrainn,
         FA_Pons,
         FA_Medulla)


length(unique(FA_Takeda$NUM)) # 24

FA_Takeda %>% ungroup() %>% group_by(n) %>% count()

FA_Takeda %>% mutate(Year= ifelse(TIME_STUDY<=0.5, 0,
                                  ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                                         ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                                ifelse(TIME_STUDY>=2.5 ,3, NA))))) %>%
  select(NUM, Year) %>% group_by(NUM) %>%
  mutate(All=paste0(Year, collapse=","))

range(FA_Takeda$TIME_STUDY)


FA_Takeda <- FA_Takeda %>% mutate(CerebellumWhite=(FA_L_Cerebell_WM+FA_R_Cerebell_WM)/2) %>%
  mutate(CerebellumGray=(FA_L_Cerebell_GM+FA_R_Cerebell_GM)/2) %>%
  mutate(CerebellumTotal=0.82*CerebellumGray+0.18*CerebellumWhite)


names(FA_Takeda)



Baseline <- FA_Takeda %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, FA_L_Puta, FA_R_Puta, CerebellumWhite,CerebellumTotal, FA_Midbrainn,FA_Pons,FA_Medulla) 

Date_Baseline <- FA_Takeda %>%
  group_by(NUM) %>% filter(DateIRM==min(DateIRM)) %>% ungroup() %>%
  filter(n>1) %>%
  select(NUM, DateIRM)

Year_1 <- FA_Takeda %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>%
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(NUM, FA_L_Puta, FA_R_Puta, CerebellumWhite,CerebellumTotal, FA_Midbrainn,FA_Pons,FA_Medulla) 


Date_Year_1 <- FA_Takeda %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>%
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>% select(NUM, DateIRM)

TIME_STUDY <- FA_Takeda %>%
  group_by(NUM) %>% filter(DateIRM!=min(DateIRM)) %>% 
  filter(n>1) %>% 
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(NUM, TIME_STUDY)


Pats <- Year_1 %>% select(NUM) %>% 
  inner_join(Baseline %>% select(NUM)) %>% 
  inner_join(SCHRAG_Delta %>% select(NUM)) %>%
  arrange(NUM)


Year_1 <- Pats %>% left_join(Year_1) %>% select(-NUM)
Baseline <- Pats %>% left_join(Baseline) %>% select(-NUM)
TIME_STUDY <- Pats %>% left_join(TIME_STUDY) %>% select(-NUM)
SCHRAG_Delta <- Pats %>% left_join(SCHRAG_Delta) %>% select(-NUM)



Delta <- (Year_1 - Baseline)

Delta <- Delta/rep(TIME_STUDY,7)

Delta <- Delta %>% bind_cols(SCHRAG_Delta)


Correlations <- Delta %>% cor(method = "spearman")

round(Correlations, 3)

get_p_value <- function(x, y) {
  cor_test <- cor.test(x, y, method = "spearman")
  broom::tidy(cor_test)$p.value
}

p_value_matrix <- matrix(NA, ncol = 8, nrow = 8, 
                         dimnames = list(names(Delta), names(Delta)))


for (i in 1:ncol(Delta)) {
  for (j in 1:ncol(Delta)) {
    p_value_matrix[i, j] <- get_p_value(Delta[[i]], Delta[[j]])
  }
}

Correlations <- round(Correlations, 3)
p_value_matrix <- round(p_value_matrix, 3)


combined_matrix <- matrix(NA, ncol = 8, nrow = 8, 
                          dimnames = list(names(Delta), names(Delta)))


for (i in 1:ncol(Delta)) {
  for (j in 1:ncol(Delta)) {
    combined_matrix[i, j] <- paste0(Correlations[i, j], ", ", p_value_matrix[i, j])
  }
}



# ------------
# Biofluids -------------

PD01PD03ToExclude <- read_xlsx(path="Source/PD01PD03ToExclude.xlsx", skip=0, col_types = "text", trim_ws = TRUE)


biofluidsForPaulo <- readRDS("Source/biofluidsForPaulo.rds")
biofluidsForPaulo <- biofluidsForPaulo %>% anti_join(PD01PD03ToExclude)
names(biofluidsForPaulo)
biofluidsForPaulo <- biofluidsForPaulo %>% select(idGlobal,datePrelevement, 
                                                  total_alpha_syn_ipp_pgml_csf,
                                                  nfl_ipp_pgml_csf, 
                                                  nfl_pgml_csf, 
                                                  nfl_ipp_pgml_plas,
                                                  nfl_pgml_plas)
biofluidsForPaulo <- biofluidsForPaulo %>% arrange(idGlobal, datePrelevement) %>% group_by(idGlobal)

biofluidsForPaulo %>% group_by(idGlobal) %>% count() %>% filter(n>1)


biofluidsForPaulo <- data.frame(biofluidsForPaulo %>%
                                  mutate(ALpha_CSF=ifelse(!is.na(total_alpha_syn_ipp_pgml_csf), total_alpha_syn_ipp_pgml_csf, NA)) %>%
                                  mutate(NFL_CSF=ifelse(!is.na(nfl_ipp_pgml_csf), nfl_ipp_pgml_csf, nfl_pgml_csf )) %>%
                                  mutate(NFL_Plasma=ifelse(!is.na(nfl_ipp_pgml_plas ), nfl_ipp_pgml_plas , nfl_pgml_plas))) %>%
  select(idGlobal,datePrelevement,ALpha_CSF,NFL_CSF,NFL_Plasma )

biofluidsForPaulo %>% filter(!is.na(ALpha_CSF)) %>%
  group_by(idGlobal) %>% count() %>% ungroup() %>%
  group_by(n) %>% count()

biofluidsForPaulo %>% filter(!is.na(NFL_CSF)) %>%
  group_by(idGlobal) %>% count() %>% ungroup() %>%
  group_by(n) %>% count()

biofluidsForPaulo %>% filter(!is.na(NFL_Plasma)) %>%
  group_by(idGlobal) %>% count() %>% ungroup() %>%
  group_by(n) %>% count()


# ---------------------------
# ALpha_CSF ---------------------------------------------------------------------------------------

PD01PD03ToExclude <- read_xlsx(path="Source/PD01PD03ToExclude.xlsx", skip=0, col_types = "text", trim_ws = TRUE)


biofluidsForPaulo <- readRDS("Source/biofluidsForPaulo.rds")
biofluidsForPaulo <- biofluidsForPaulo %>% anti_join(PD01PD03ToExclude)
names(biofluidsForPaulo)
biofluidsForPaulo <- biofluidsForPaulo %>% select(idGlobal,datePrelevement, 
                                                  total_alpha_syn_ipp_pgml_csf,
                                                  nfl_ipp_pgml_csf, 
                                                  nfl_pgml_csf, 
                                                  nfl_ipp_pgml_plas,
                                                  nfl_pgml_plas)
biofluidsForPaulo <- biofluidsForPaulo %>% arrange(idGlobal, datePrelevement) %>% group_by(idGlobal)

biofluidsForPaulo %>% group_by(idGlobal) %>% count() %>% filter(n>1)


biofluidsForPaulo <- data.frame(biofluidsForPaulo %>%
                                  mutate(ALpha_CSF=ifelse(!is.na(total_alpha_syn_ipp_pgml_csf), total_alpha_syn_ipp_pgml_csf, NA)) %>%
                                  mutate(NFL_CSF=ifelse(!is.na(nfl_ipp_pgml_csf), nfl_ipp_pgml_csf, nfl_pgml_csf )) %>%
                                  mutate(NFL_Plasma=ifelse(!is.na(nfl_ipp_pgml_plas ), nfl_ipp_pgml_plas , nfl_pgml_plas))) %>%
  select(idGlobal,datePrelevement,ALpha_CSF ) %>% drop_na()

biofluidsForPaulo <- biofluidsForPaulo %>% arrange(idGlobal, datePrelevement) %>% group_by(idGlobal) %>%
  mutate(TIME_STUDY=time_length(difftime(datePrelevement, lag(datePrelevement) ), "years")) %>%
  select(idGlobal, datePrelevement, TIME_STUDY,ALpha_CSF)


biofluidsForPaulo <- biofluidsForPaulo %>% mutate(TIME_STUDY=ifelse(is.na(TIME_STUDY), 0, TIME_STUDY))

biofluidsForPaulo <- biofluidsForPaulo %>% mutate(TIME_STUDY=cumsum(TIME_STUDY))

biofluidsForPaulo <- biofluidsForPaulo %>% group_by(idGlobal) %>% count() %>%
  left_join(biofluidsForPaulo) %>%
  select(idGlobal, datePrelevement, TIME_STUDY, n,ALpha_CSF)


length(unique(biofluidsForPaulo$idGlobal)) # 37

biofluidsForPaulo %>% ungroup() %>% group_by(n) %>% count()

biofluidsForPaulo %>% mutate(Year= ifelse(TIME_STUDY<=0.5, 0,
                                          ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                                                 ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                                        ifelse(TIME_STUDY>=2.5 ,3, NA))))) %>%
  select(idGlobal, Year) %>% group_by(idGlobal) %>%
  mutate(All=paste0(Year, collapse=","))



range(biofluidsForPaulo$TIME_STUDY)



# PLOTS **************************************

biofluidsForPaulo %>% select(idGlobal, n) %>% distinct() %>%
  group_by(n) %>% count() %>%
  ggplot(aes(n, nn)) +
  geom_col(fill="black", colour="black") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  xlab("\n Number of Alpha CSF Measurements per patient") +
  ylab("\n Number of Patients \n") +
  geom_text(aes(label = nn), vjust = -0.5) + xlim(0,3)


biofluidsForPaulo %>% 
  select(idGlobal, TIME_STUDY, ALpha_CSF) %>% ungroup() %>%
  ggplot(aes(TIME_STUDY, ALpha_CSF)) +
  geom_line(aes(group=idGlobal), col="black" , linewidth=0.5, alpha=0.4) +
  geom_jitter(size=1, colour="firebrick", alpha=0.5, shape=1, stroke=1) +
  theme_minimal() +
  xlab("\n Elapsed number of years since study enrollment \n") +
  ylab("CSF Alpha SYN \n At each measurement \n") +
  scale_color_manual(values=palette) +
  scale_fill_manual(values=palette) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_x_continuous(breaks = seq(0, 5, by = 0.5)) +
  ylim(0,2100)


# END of PLOTS ****************************


# ALPHA SYN CSF ************************

biofluidsForPaulo %>%
  group_by(idGlobal) %>% filter(datePrelevement ==min(datePrelevement )) %>% ungroup() %>%
  filter(n>1) %>%
  select(idGlobal, TIME_STUDY, ALpha_CSF) %>%
  summarise(
    mean=mean(ALpha_CSF),
    sd=sd(ALpha_CSF),
    median=median(ALpha_CSF),
    Q1 = quantile(ALpha_CSF, 0.25),
    Q3 = quantile(ALpha_CSF, 0.75),
    min=min(ALpha_CSF),
    max=max(ALpha_CSF),
    n=n()
  )



biofluidsForPaulo %>%
  group_by(idGlobal) %>% filter(datePrelevement !=min(datePrelevement )) %>% 
  filter(n>1) %>% 
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(idGlobal, TIME_STUDY, ALpha_CSF, Elapsed) %>%
  summarise(
    mean=mean(TIME_STUDY*12),
    sd=sd(TIME_STUDY*12),
    median=median(TIME_STUDY*12),
    Q1 = quantile(TIME_STUDY*12, 0.25),
    Q3 = quantile(TIME_STUDY*12, 0.75),
    min=min(TIME_STUDY*12),
    max=max(TIME_STUDY*12),
    n=n()
  )


biofluidsForPaulo %>%
  group_by(idGlobal) %>% filter(datePrelevement !=min(datePrelevement )) %>% 
  filter(n>1) %>% 
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(idGlobal, TIME_STUDY, ALpha_CSF) %>%
  summarise(
    mean=mean(ALpha_CSF),
    sd=sd(ALpha_CSF),
    median=median(ALpha_CSF),
    Q1 = quantile(ALpha_CSF, 0.25),
    Q3 = quantile(ALpha_CSF, 0.75),
    min=min(ALpha_CSF),
    max=max(ALpha_CSF),
    n=n()
  )




biofluidsForPaulo %>%
  group_by(idGlobal) %>% filter(datePrelevement ==min(datePrelevement )) %>% ungroup() %>%
  filter(n>1) %>%
  select(idGlobal, ALpha_CSF) %>% rename("Year0"="ALpha_CSF") %>%
  inner_join(
    biofluidsForPaulo %>%
      group_by(idGlobal) %>% filter(datePrelevement !=min(datePrelevement )) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(idGlobal, TIME_STUDY, ALpha_CSF)
  ) %>% mutate(Delta=ALpha_CSF-Year0) %>%
  ungroup() %>%
  select(idGlobal, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta),
    sd=sd(Delta),
    median=median(Delta),
    Q1 = quantile(Delta, 0.25),
    Q3 = quantile(Delta, 0.75),
    min=min(Delta),
    max=max(Delta),
    n=n()
  )



biofluidsForPaulo %>%
  group_by(idGlobal) %>% filter(datePrelevement ==min(datePrelevement )) %>% ungroup() %>%
  filter(n>1) %>%
  select(idGlobal, ALpha_CSF) %>% rename("Year0"="ALpha_CSF") %>%
  inner_join(
    biofluidsForPaulo %>%
      group_by(idGlobal) %>% filter(datePrelevement !=min(datePrelevement )) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(idGlobal, TIME_STUDY, ALpha_CSF)
  ) %>% mutate(Delta=100*(ALpha_CSF-Year0)/Year0) %>%  ungroup() %>%
  ungroup() %>%
  select(idGlobal, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta),
    sd=sd(Delta),
    median=median(Delta),
    Q1 = quantile(Delta, 0.25),
    Q3 = quantile(Delta, 0.75),
    min=min(Delta),
    max=max(Delta),
    n=n()
  )



biofluidsForPaulo %>%
  group_by(idGlobal) %>% filter(datePrelevement ==min(datePrelevement )) %>% ungroup() %>%
  filter(n>1) %>%
  select(idGlobal, ALpha_CSF) %>% rename("Year0"="ALpha_CSF") %>%
  inner_join(
    biofluidsForPaulo %>%
      group_by(idGlobal) %>% filter(datePrelevement !=min(datePrelevement )) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(idGlobal, TIME_STUDY, ALpha_CSF)
  ) %>% mutate(Delta=ALpha_CSF-Year0) %>%
  ungroup() %>%
  select(idGlobal, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta/(TIME_STUDY)),
    sd=sd(Delta/(TIME_STUDY)),
    median=median(Delta/(TIME_STUDY)),
    Q1 = quantile(Delta/(TIME_STUDY), 0.25),
    Q3 = quantile(Delta/(TIME_STUDY), 0.75),
    min=min(Delta/(TIME_STUDY)),
    max=max(Delta/(TIME_STUDY)),
    n=n()
  )


biofluidsForPaulo %>%
  group_by(idGlobal) %>% filter(datePrelevement ==min(datePrelevement )) %>% ungroup() %>%
  filter(n>1) %>%
  select(idGlobal, ALpha_CSF) %>% rename("Year0"="ALpha_CSF") %>%
  inner_join(
    biofluidsForPaulo %>%
      group_by(idGlobal) %>% filter(datePrelevement !=min(datePrelevement )) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(idGlobal, TIME_STUDY, ALpha_CSF)
  ) %>% mutate(Delta=100*(ALpha_CSF-Year0)/Year0) %>%  ungroup() %>%
  ungroup() %>%
  select(idGlobal, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta/(TIME_STUDY)),
    sd=sd(Delta/(TIME_STUDY)),
    median=median(Delta/(TIME_STUDY)),
    Q1 = quantile(Delta/(TIME_STUDY), 0.25),
    Q3 = quantile(Delta/(TIME_STUDY), 0.75),
    min=min(Delta/(TIME_STUDY)),
    max=max(Delta/(TIME_STUDY)),
    n=n()
  )







# -----------------
# NFL_CSF ---------------------------------------------------------------------------------------

PD01PD03ToExclude <- read_xlsx(path="Source/PD01PD03ToExclude.xlsx", skip=0, col_types = "text", trim_ws = TRUE)


biofluidsForPaulo <- readRDS("Source/biofluidsForPaulo.rds")
biofluidsForPaulo <- biofluidsForPaulo %>% anti_join(PD01PD03ToExclude)
names(biofluidsForPaulo)
biofluidsForPaulo <- biofluidsForPaulo %>% select(idGlobal,datePrelevement, 
                                                  total_alpha_syn_ipp_pgml_csf,
                                                  nfl_ipp_pgml_csf, 
                                                  nfl_pgml_csf, 
                                                  nfl_ipp_pgml_plas,
                                                  nfl_pgml_plas)
biofluidsForPaulo <- biofluidsForPaulo %>% arrange(idGlobal, datePrelevement) %>% group_by(idGlobal)

biofluidsForPaulo %>% group_by(idGlobal) %>% count() %>% filter(n>1)


biofluidsForPaulo <- data.frame(biofluidsForPaulo %>%
                                  mutate(ALpha_CSF=ifelse(!is.na(total_alpha_syn_ipp_pgml_csf), total_alpha_syn_ipp_pgml_csf, NA)) %>%
                                  mutate(NFL_CSF=ifelse(!is.na(nfl_ipp_pgml_csf), nfl_ipp_pgml_csf, nfl_pgml_csf )) %>%
                                  mutate(NFL_Plasma=ifelse(!is.na(nfl_ipp_pgml_plas ), nfl_ipp_pgml_plas , nfl_pgml_plas))) %>%
  select(idGlobal,datePrelevement,NFL_CSF ) %>% drop_na()

biofluidsForPaulo <- biofluidsForPaulo %>% arrange(idGlobal, datePrelevement) %>% group_by(idGlobal) %>%
  mutate(TIME_STUDY=time_length(difftime(datePrelevement, lag(datePrelevement) ), "years")) %>%
  select(idGlobal, datePrelevement, TIME_STUDY,NFL_CSF)


biofluidsForPaulo <- biofluidsForPaulo %>% mutate(TIME_STUDY=ifelse(is.na(TIME_STUDY), 0, TIME_STUDY))

biofluidsForPaulo <- biofluidsForPaulo %>% mutate(TIME_STUDY=cumsum(TIME_STUDY))

biofluidsForPaulo <- biofluidsForPaulo %>% group_by(idGlobal) %>% count() %>%
  left_join(biofluidsForPaulo) %>%
  select(idGlobal, datePrelevement, TIME_STUDY, n,NFL_CSF)


length(unique(biofluidsForPaulo$idGlobal)) # 45

biofluidsForPaulo %>% ungroup() %>% group_by(n) %>% count()

biofluidsForPaulo %>% mutate(Year= ifelse(TIME_STUDY<=0.5, 0,
                                          ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                                                 ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                                        ifelse(TIME_STUDY>=2.5 ,3, NA))))) %>%
  select(idGlobal, Year) %>% group_by(idGlobal) %>%
  mutate(All=paste0(Year, collapse=","))



range(biofluidsForPaulo$TIME_STUDY)



# PLOTS **************************************

biofluidsForPaulo %>% select(idGlobal, n) %>% distinct() %>%
  group_by(n) %>% count() %>%
  ggplot(aes(n, nn)) +
  geom_col(fill="black", colour="black") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  xlab("\n Number of Nfl CSF Measurements per patient") +
  ylab("\n Number of Patients \n") +
  geom_text(aes(label = nn), vjust = -0.5) + xlim(0,3)


biofluidsForPaulo %>% 
  select(idGlobal, TIME_STUDY, NFL_CSF) %>% ungroup() %>%
  ggplot(aes(TIME_STUDY, NFL_CSF)) +
  geom_line(aes(group=idGlobal), col="black" , linewidth=0.5, alpha=0.4) +
  geom_jitter(size=1, colour="firebrick", alpha=0.5, shape=1, stroke=1) +
  theme_minimal() +
  xlab("\n Elapsed number of years since study enrollment \n") +
  ylab("CSF Nfl \n At each measurement \n") +
  scale_color_manual(values=palette) +
  scale_fill_manual(values=palette) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_x_continuous(breaks = seq(0, 5, by = 0.5)) +
  ylim(0,8500)


# END of PLOTS ****************************


# NFL CSF ************************

biofluidsForPaulo %>%
  group_by(idGlobal) %>% filter(datePrelevement ==min(datePrelevement )) %>% ungroup() %>%
  filter(n>1) %>%
  select(idGlobal, TIME_STUDY, NFL_CSF) %>%
  summarise(
    mean=mean(NFL_CSF),
    sd=sd(NFL_CSF),
    median=median(NFL_CSF),
    Q1 = quantile(NFL_CSF, 0.25),
    Q3 = quantile(NFL_CSF, 0.75),
    min=min(NFL_CSF),
    max=max(NFL_CSF),
    n=n()
  )



biofluidsForPaulo %>%
  group_by(idGlobal) %>% filter(datePrelevement !=min(datePrelevement )) %>% 
  filter(n>1) %>% 
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(idGlobal, TIME_STUDY, NFL_CSF, Elapsed) %>%
  summarise(
    mean=mean(TIME_STUDY*12),
    sd=sd(TIME_STUDY*12),
    median=median(TIME_STUDY*12),
    Q1 = quantile(TIME_STUDY*12, 0.25),
    Q3 = quantile(TIME_STUDY*12, 0.75),
    min=min(TIME_STUDY*12),
    max=max(TIME_STUDY*12),
    n=n()
  )


biofluidsForPaulo %>%
  group_by(idGlobal) %>% filter(datePrelevement !=min(datePrelevement )) %>% 
  filter(n>1) %>% 
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(idGlobal, TIME_STUDY, NFL_CSF) %>%
  summarise(
    mean=mean(NFL_CSF),
    sd=sd(NFL_CSF),
    median=median(NFL_CSF),
    Q1 = quantile(NFL_CSF, 0.25),
    Q3 = quantile(NFL_CSF, 0.75),
    min=min(NFL_CSF),
    max=max(NFL_CSF),
    n=n()
  )




biofluidsForPaulo %>%
  group_by(idGlobal) %>% filter(datePrelevement ==min(datePrelevement )) %>% ungroup() %>%
  filter(n>1) %>%
  select(idGlobal, NFL_CSF) %>% rename("Year0"="NFL_CSF") %>%
  inner_join(
    biofluidsForPaulo %>%
      group_by(idGlobal) %>% filter(datePrelevement !=min(datePrelevement )) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(idGlobal, TIME_STUDY, NFL_CSF)
  ) %>% mutate(Delta=NFL_CSF-Year0) %>%
  ungroup() %>%
  select(idGlobal, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta),
    sd=sd(Delta),
    median=median(Delta),
    Q1 = quantile(Delta, 0.25),
    Q3 = quantile(Delta, 0.75),
    min=min(Delta),
    max=max(Delta),
    n=n()
  )



biofluidsForPaulo %>%
  group_by(idGlobal) %>% filter(datePrelevement ==min(datePrelevement )) %>% ungroup() %>%
  filter(n>1) %>%
  select(idGlobal, NFL_CSF) %>% rename("Year0"="NFL_CSF") %>%
  inner_join(
    biofluidsForPaulo %>%
      group_by(idGlobal) %>% filter(datePrelevement !=min(datePrelevement )) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(idGlobal, TIME_STUDY, NFL_CSF)
  ) %>% mutate(Delta=100*(NFL_CSF-Year0)/Year0) %>%  ungroup() %>%
  ungroup() %>%
  select(idGlobal, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta),
    sd=sd(Delta),
    median=median(Delta),
    Q1 = quantile(Delta, 0.25),
    Q3 = quantile(Delta, 0.75),
    min=min(Delta),
    max=max(Delta),
    n=n()
  )


biofluidsForPaulo %>%
  group_by(idGlobal) %>% filter(datePrelevement ==min(datePrelevement )) %>% ungroup() %>%
  filter(n>1) %>%
  select(idGlobal, NFL_CSF) %>% rename("Year0"="NFL_CSF") %>%
  inner_join(
    biofluidsForPaulo %>%
      group_by(idGlobal) %>% filter(datePrelevement !=min(datePrelevement )) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(idGlobal, TIME_STUDY, NFL_CSF)
  ) %>% mutate(Delta=NFL_CSF-Year0) %>%
  ungroup() %>%
  select(idGlobal, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta/(TIME_STUDY)),
    sd=sd(Delta/(TIME_STUDY)),
    median=median(Delta/(TIME_STUDY)),
    Q1 = quantile(Delta/(TIME_STUDY), 0.25),
    Q3 = quantile(Delta/(TIME_STUDY), 0.75),
    min=min(Delta/(TIME_STUDY)),
    max=max(Delta/(TIME_STUDY)),
    n=n()
  )




biofluidsForPaulo %>%
  group_by(idGlobal) %>% filter(datePrelevement ==min(datePrelevement )) %>% ungroup() %>%
  filter(n>1) %>%
  select(idGlobal, NFL_CSF) %>% rename("Year0"="NFL_CSF") %>%
  inner_join(
    biofluidsForPaulo %>%
      group_by(idGlobal) %>% filter(datePrelevement !=min(datePrelevement )) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(idGlobal, TIME_STUDY, NFL_CSF)
  ) %>% mutate(Delta=100*(NFL_CSF-Year0)/Year0) %>%  ungroup() %>%
  ungroup() %>%
  select(idGlobal, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta/(TIME_STUDY)),
    sd=sd(Delta/(TIME_STUDY)),
    median=median(Delta/(TIME_STUDY)),
    Q1 = quantile(Delta/(TIME_STUDY), 0.25),
    Q3 = quantile(Delta/(TIME_STUDY), 0.75),
    min=min(Delta/(TIME_STUDY)),
    max=max(Delta/(TIME_STUDY)),
    n=n()
  )







# -----------------
# NFL_Plasma ---------------------------------------------------------------------------------------

PD01PD03ToExclude <- read_xlsx(path="Source/PD01PD03ToExclude.xlsx", skip=0, col_types = "text", trim_ws = TRUE)


biofluidsForPaulo <- readRDS("Source/biofluidsForPaulo.rds")
biofluidsForPaulo <- biofluidsForPaulo %>% anti_join(PD01PD03ToExclude)
names(biofluidsForPaulo)
biofluidsForPaulo <- biofluidsForPaulo %>% select(idGlobal,datePrelevement, 
                                                  total_alpha_syn_ipp_pgml_csf,
                                                  nfl_ipp_pgml_csf, 
                                                  nfl_pgml_csf, 
                                                  nfl_ipp_pgml_plas,
                                                  nfl_pgml_plas)
biofluidsForPaulo <- biofluidsForPaulo %>% arrange(idGlobal, datePrelevement) %>% group_by(idGlobal)

biofluidsForPaulo %>% group_by(idGlobal) %>% count() %>% filter(n>1)


biofluidsForPaulo <- data.frame(biofluidsForPaulo %>%
                                  mutate(ALpha_CSF=ifelse(!is.na(total_alpha_syn_ipp_pgml_csf), total_alpha_syn_ipp_pgml_csf, NA)) %>%
                                  mutate(NFL_CSF=ifelse(!is.na(nfl_ipp_pgml_csf), nfl_ipp_pgml_csf, nfl_pgml_csf )) %>%
                                  mutate(NFL_Plasma=ifelse(!is.na(nfl_ipp_pgml_plas ), nfl_ipp_pgml_plas , nfl_pgml_plas))) %>%
  select(idGlobal,datePrelevement,NFL_Plasma ) %>% drop_na()

biofluidsForPaulo <- biofluidsForPaulo %>% arrange(idGlobal, datePrelevement) %>% group_by(idGlobal) %>%
  mutate(TIME_STUDY=time_length(difftime(datePrelevement, lag(datePrelevement) ), "years")) %>%
  select(idGlobal, datePrelevement, TIME_STUDY,NFL_Plasma)


biofluidsForPaulo <- biofluidsForPaulo %>% mutate(TIME_STUDY=ifelse(is.na(TIME_STUDY), 0, TIME_STUDY))

biofluidsForPaulo <- biofluidsForPaulo %>% mutate(TIME_STUDY=cumsum(TIME_STUDY))

biofluidsForPaulo <- biofluidsForPaulo %>% group_by(idGlobal) %>% count() %>%
  left_join(biofluidsForPaulo) %>%
  select(idGlobal, datePrelevement, TIME_STUDY, n,NFL_Plasma)


length(unique(biofluidsForPaulo$idGlobal)) # 59

biofluidsForPaulo %>% ungroup() %>% group_by(n) %>% count()

biofluidsForPaulo %>% mutate(Year= ifelse(TIME_STUDY<=0.5, 0,
                                          ifelse(TIME_STUDY>=0.5 & TIME_STUDY<1.5 , 1,
                                                 ifelse(TIME_STUDY>=1.5 & TIME_STUDY<2.5, 2,
                                                        ifelse(TIME_STUDY>=2.5 ,3, NA))))) %>%
  select(idGlobal, Year) %>% group_by(idGlobal) %>%
  mutate(All=paste0(Year, collapse=","))



range(biofluidsForPaulo$TIME_STUDY)



# PLOTS **************************************

biofluidsForPaulo %>% select(idGlobal, n) %>% distinct() %>%
  group_by(n) %>% count() %>%
  ggplot(aes(n, nn)) +
  geom_col(fill="black", colour="black") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  xlab("\n Number of Nfl Plasma Measurements per patient") +
  ylab("\n Number of Patients \n") +
  geom_text(aes(label = nn), vjust = -0.5) + xlim(0,3)


biofluidsForPaulo %>% 
  select(idGlobal, TIME_STUDY, NFL_Plasma) %>% ungroup() %>%
  ggplot(aes(TIME_STUDY, NFL_Plasma)) +
  geom_line(aes(group=idGlobal), col="black" , linewidth=0.5, alpha=0.4) +
  geom_jitter(size=1, colour="firebrick", alpha=0.5, shape=1, stroke=1) +
  theme_minimal() +
  xlab("\n Elapsed number of years since study enrollment \n") +
  ylab("Plasma Nfl \n At each measurement \n") +
  scale_color_manual(values=palette) +
  scale_fill_manual(values=palette) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_x_continuous(breaks = seq(0, 5, by = 0.5)) +
  ylim(0,70)


# END of PLOTS ****************************


# NFL Plasma ************************

biofluidsForPaulo %>%
  group_by(idGlobal) %>% filter(datePrelevement ==min(datePrelevement )) %>% ungroup() %>%
  filter(n>1) %>%
  select(idGlobal, TIME_STUDY, NFL_Plasma) %>%
  summarise(
    mean=mean(NFL_Plasma),
    sd=sd(NFL_Plasma),
    median=median(NFL_Plasma),
    Q1 = quantile(NFL_Plasma, 0.25),
    Q3 = quantile(NFL_Plasma, 0.75),
    min=min(NFL_Plasma),
    max=max(NFL_Plasma),
    n=n()
  )



biofluidsForPaulo %>%
  group_by(idGlobal) %>% filter(datePrelevement !=min(datePrelevement )) %>% 
  filter(n>1) %>% 
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(idGlobal, TIME_STUDY, NFL_Plasma, Elapsed) %>%
  summarise(
    mean=mean(TIME_STUDY*12),
    sd=sd(TIME_STUDY*12),
    median=median(TIME_STUDY*12),
    Q1 = quantile(TIME_STUDY*12, 0.25),
    Q3 = quantile(TIME_STUDY*12, 0.75),
    min=min(TIME_STUDY*12),
    max=max(TIME_STUDY*12),
    n=n()
  )


biofluidsForPaulo %>%
  group_by(idGlobal) %>% filter(datePrelevement !=min(datePrelevement )) %>% 
  filter(n>1) %>% 
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  ungroup() %>%
  select(idGlobal, TIME_STUDY, NFL_Plasma) %>%
  summarise(
    mean=mean(NFL_Plasma),
    sd=sd(NFL_Plasma),
    median=median(NFL_Plasma),
    Q1 = quantile(NFL_Plasma, 0.25),
    Q3 = quantile(NFL_Plasma, 0.75),
    min=min(NFL_Plasma),
    max=max(NFL_Plasma),
    n=n()
  )




biofluidsForPaulo %>%
  group_by(idGlobal) %>% filter(datePrelevement ==min(datePrelevement )) %>% ungroup() %>%
  filter(n>1) %>%
  select(idGlobal, NFL_Plasma) %>% rename("Year0"="NFL_Plasma") %>%
  inner_join(
    biofluidsForPaulo %>%
      group_by(idGlobal) %>% filter(datePrelevement !=min(datePrelevement )) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(idGlobal, TIME_STUDY, NFL_Plasma)
  ) %>% mutate(Delta=NFL_Plasma-Year0) %>%
  ungroup() %>%
  select(idGlobal, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta),
    sd=sd(Delta),
    median=median(Delta),
    Q1 = quantile(Delta, 0.25),
    Q3 = quantile(Delta, 0.75),
    min=min(Delta),
    max=max(Delta),
    n=n()
  )




biofluidsForPaulo %>%
  group_by(idGlobal) %>% filter(datePrelevement ==min(datePrelevement )) %>% ungroup() %>%
  filter(n>1) %>%
  select(idGlobal, NFL_Plasma) %>% rename("Year0"="NFL_Plasma") %>%
  inner_join(
    biofluidsForPaulo %>%
      group_by(idGlobal) %>% filter(datePrelevement !=min(datePrelevement )) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(idGlobal, TIME_STUDY, NFL_Plasma)
  ) %>% mutate(Delta=100*(NFL_Plasma-Year0)/Year0) %>%  ungroup() %>%
  ungroup() %>%
  select(idGlobal, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta),
    sd=sd(Delta),
    median=median(Delta),
    Q1 = quantile(Delta, 0.25),
    Q3 = quantile(Delta, 0.75),
    min=min(Delta),
    max=max(Delta),
    n=n()
  )


biofluidsForPaulo %>%
  group_by(idGlobal) %>% filter(datePrelevement ==min(datePrelevement )) %>% ungroup() %>%
  filter(n>1) %>%
  select(idGlobal, NFL_Plasma) %>% rename("Year0"="NFL_Plasma") %>%
  inner_join(
    biofluidsForPaulo %>%
      group_by(idGlobal) %>% filter(datePrelevement !=min(datePrelevement )) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(idGlobal, TIME_STUDY, NFL_Plasma)
  ) %>% mutate(Delta=NFL_Plasma-Year0) %>%
  ungroup() %>%
  select(idGlobal, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta/(TIME_STUDY)),
    sd=sd(Delta/(TIME_STUDY)),
    median=median(Delta/(TIME_STUDY)),
    Q1 = quantile(Delta/(TIME_STUDY), 0.25),
    Q3 = quantile(Delta/(TIME_STUDY), 0.75),
    min=min(Delta/(TIME_STUDY)),
    max=max(Delta/(TIME_STUDY)),
    n=n()
  )




biofluidsForPaulo %>%
  group_by(idGlobal) %>% filter(datePrelevement ==min(datePrelevement )) %>% ungroup() %>%
  filter(n>1) %>%
  select(idGlobal, NFL_Plasma) %>% rename("Year0"="NFL_Plasma") %>%
  inner_join(
    biofluidsForPaulo %>%
      group_by(idGlobal) %>% filter(datePrelevement !=min(datePrelevement )) %>% 
      filter(n>1) %>% 
      mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
      ungroup() %>%
      select(idGlobal, TIME_STUDY, NFL_Plasma)
  ) %>% mutate(Delta=100*(NFL_Plasma-Year0)/Year0) %>%  ungroup() %>%
  ungroup() %>%
  select(idGlobal, TIME_STUDY, Delta) %>%
  summarise(
    mean=mean(Delta/(TIME_STUDY)),
    sd=sd(Delta/(TIME_STUDY)),
    median=median(Delta/(TIME_STUDY)),
    Q1 = quantile(Delta/(TIME_STUDY), 0.25),
    Q3 = quantile(Delta/(TIME_STUDY), 0.75),
    min=min(Delta/(TIME_STUDY)),
    max=max(Delta/(TIME_STUDY)),
    n=n()
  )







# -----------------
# CORRELATION INTERNAL BASELINE -----------------------
PD01PD03ToExclude <- read_xlsx(path="Source/PD01PD03ToExclude.xlsx", skip=0, col_types = "text", trim_ws = TRUE)


biofluidsForPaulo <- readRDS("Source/biofluidsForPaulo.rds")
biofluidsForPaulo <- biofluidsForPaulo %>% anti_join(PD01PD03ToExclude)
names(biofluidsForPaulo)
biofluidsForPaulo <- biofluidsForPaulo %>% select(idGlobal,datePrelevement, 
                                                  total_alpha_syn_ipp_pgml_csf,
                                                  nfl_ipp_pgml_csf, 
                                                  nfl_pgml_csf, 
                                                  nfl_ipp_pgml_plas,
                                                  nfl_pgml_plas)
biofluidsForPaulo <- biofluidsForPaulo %>% arrange(idGlobal, datePrelevement) %>% group_by(idGlobal)


biofluidsForPaulo <- data.frame(biofluidsForPaulo %>%
                                  mutate(ALpha_CSF=ifelse(!is.na(total_alpha_syn_ipp_pgml_csf), total_alpha_syn_ipp_pgml_csf, NA)) %>%
                                  mutate(NFL_CSF=ifelse(!is.na(nfl_ipp_pgml_csf), nfl_ipp_pgml_csf, nfl_pgml_csf )) %>%
                                  mutate(NFL_Plasma=ifelse(!is.na(nfl_ipp_pgml_plas ), nfl_ipp_pgml_plas , nfl_pgml_plas))) %>%
  select(idGlobal,datePrelevement,ALpha_CSF,NFL_CSF,NFL_Plasma )


First_ALpha_CSF <- biofluidsForPaulo %>% select(idGlobal, datePrelevement, ALpha_CSF) %>% drop_na() %>%
  group_by(idGlobal) %>% filter(datePrelevement==min(datePrelevement)) %>% select(-datePrelevement) %>% distinct() 

First_NFL_CSF <- biofluidsForPaulo %>% select(idGlobal, datePrelevement, NFL_CSF) %>% drop_na() %>%
  group_by(idGlobal) %>% filter(datePrelevement==min(datePrelevement)) %>% select(-datePrelevement) %>% distinct() 

First_NFL_Plasma <- biofluidsForPaulo %>% select(idGlobal, datePrelevement, NFL_Plasma) %>% drop_na() %>%
  group_by(idGlobal) %>% filter(datePrelevement==min(datePrelevement)) %>% select(-datePrelevement) %>% distinct() 

temp <- First_ALpha_CSF %>% inner_join(First_NFL_CSF) %>% inner_join(First_NFL_Plasma) %>% ungroup() %>% select(-idGlobal)


Correlations <- temp %>% cor(method = "spearman")

round(Correlations, 3)

get_p_value <- function(x, y) {
  cor_test <- cor.test(x, y, method = "spearman")
  broom::tidy(cor_test)$p.value
}

p_value_matrix <- matrix(NA, ncol = 3, nrow = 3, 
                         dimnames = list(names(temp), names(temp)))


for (i in 1:ncol(temp)) {
  for (j in 1:ncol(temp)) {
    p_value_matrix[i, j] <- get_p_value(temp[[i]], temp[[j]])
  }
}

Correlations <- round(Correlations, 3)
p_value_matrix <- round(p_value_matrix, 3)


combined_matrix <- matrix(NA, ncol = 3, nrow = 3, 
                          dimnames = list(names(temp), names(temp)))


for (i in 1:ncol(temp)) {
  for (j in 1:ncol(temp)) {
    combined_matrix[i, j] <- paste0(Correlations[i, j], ", ", p_value_matrix[i, j])
  }
}

# -------------
# CORRELATION INTERNAL DELTA -----------------------
PD01PD03ToExclude <- read_xlsx(path="Source/PD01PD03ToExclude.xlsx", skip=0, col_types = "text", trim_ws = TRUE)


biofluidsForPaulo <- readRDS("Source/biofluidsForPaulo.rds")
biofluidsForPaulo <- biofluidsForPaulo %>% anti_join(PD01PD03ToExclude)
names(biofluidsForPaulo)
biofluidsForPaulo <- biofluidsForPaulo %>% select(idGlobal,datePrelevement, 
                                                  total_alpha_syn_ipp_pgml_csf,
                                                  nfl_ipp_pgml_csf, 
                                                  nfl_pgml_csf, 
                                                  nfl_ipp_pgml_plas,
                                                  nfl_pgml_plas)
biofluidsForPaulo <- biofluidsForPaulo %>% arrange(idGlobal, datePrelevement) %>% group_by(idGlobal)


biofluidsForPaulo <- data.frame(biofluidsForPaulo %>%
                                  mutate(ALpha_CSF=ifelse(!is.na(total_alpha_syn_ipp_pgml_csf), total_alpha_syn_ipp_pgml_csf, NA)) %>%
                                  mutate(NFL_CSF=ifelse(!is.na(nfl_ipp_pgml_csf), nfl_ipp_pgml_csf, nfl_pgml_csf )) %>%
                                  mutate(NFL_Plasma=ifelse(!is.na(nfl_ipp_pgml_plas ), nfl_ipp_pgml_plas , nfl_pgml_plas))) %>%
  select(idGlobal,datePrelevement,ALpha_CSF,NFL_CSF,NFL_Plasma )


First_ALpha_CSF <- biofluidsForPaulo %>% select(idGlobal, datePrelevement, ALpha_CSF) %>% drop_na() %>%
  group_by(idGlobal) %>% filter(datePrelevement==min(datePrelevement)) %>% select(-datePrelevement) %>% distinct() 

First_NFL_CSF <- biofluidsForPaulo %>% select(idGlobal, datePrelevement, NFL_CSF) %>% drop_na() %>%
  group_by(idGlobal) %>% filter(datePrelevement==min(datePrelevement)) %>% select(-datePrelevement) %>% distinct() 

First_NFL_Plasma <- biofluidsForPaulo %>% select(idGlobal, datePrelevement, NFL_Plasma) %>% drop_na() %>%
  group_by(idGlobal) %>% filter(datePrelevement==min(datePrelevement)) %>% select(-datePrelevement) %>% distinct() 

Baseline <- First_ALpha_CSF %>% inner_join(First_NFL_CSF) %>% inner_join(First_NFL_Plasma) %>% ungroup() 



biofluidsForPaulo <- biofluidsForPaulo %>% arrange(idGlobal, datePrelevement) %>% group_by(idGlobal) %>%
  mutate(TIME_STUDY=time_length(difftime(datePrelevement, lag(datePrelevement) ), "years"))  %>%
  mutate(TIME_STUDY=ifelse(is.na(TIME_STUDY), 0, TIME_STUDY)) %>%
  mutate(TIME_STUDY=cumsum(TIME_STUDY))


Second_ALpha_CSF <- biofluidsForPaulo %>% select(idGlobal, TIME_STUDY, datePrelevement, ALpha_CSF) %>% drop_na() %>%
  group_by(idGlobal) %>% filter(datePrelevement!=min(datePrelevement)) %>%
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  select(-c(datePrelevement, TIME_STUDY,Elapsed)) %>% distinct() 

Second_NFL_CSF <- biofluidsForPaulo %>% select(idGlobal, TIME_STUDY, datePrelevement, NFL_CSF) %>% drop_na() %>%
  group_by(idGlobal) %>% filter(datePrelevement!=min(datePrelevement)) %>% 
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  select(-c(datePrelevement, TIME_STUDY,Elapsed)) %>% distinct() 

Second_NFL_Plasma <- biofluidsForPaulo %>% select(idGlobal, TIME_STUDY, datePrelevement, NFL_Plasma) %>% drop_na() %>%
  group_by(idGlobal) %>% filter(datePrelevement!=min(datePrelevement)) %>% 
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  select(-c(datePrelevement, TIME_STUDY,Elapsed)) %>% distinct() 


Year_1 <- Second_ALpha_CSF %>% inner_join(Second_NFL_CSF) %>% inner_join(Second_NFL_Plasma) %>% ungroup() 


Pats <- Year_1 %>% select(idGlobal) %>% inner_join(Baseline %>% select(idGlobal)) %>% arrange(idGlobal)

Baseline <- Pats %>% left_join(Baseline) %>% select(-idGlobal)
Year_1 <- Pats %>% left_join(Year_1) %>% select(-idGlobal)

Delta <- Year_1 - Baseline

Correlations <- Delta %>% cor(method = "spearman")

round(Correlations, 3)

get_p_value <- function(x, y) {
  cor_test <- cor.test(x, y, method = "spearman")
  broom::tidy(cor_test)$p.value
}

p_value_matrix <- matrix(NA, ncol = 3, nrow = 3, 
                         dimnames = list(names(Delta), names(Delta)))


for (i in 1:ncol(Delta)) {
  for (j in 1:ncol(Delta)) {
    p_value_matrix[i, j] <- get_p_value(Delta[[i]], Delta[[j]])
  }
}

Correlations <- round(Correlations, 3)
p_value_matrix <- round(p_value_matrix, 3)


combined_matrix <- matrix(NA, ncol = 3, nrow = 3, 
                          dimnames = list(names(Delta), names(Delta)))


for (i in 1:ncol(Delta)) {
  for (j in 1:ncol(Delta)) {
    combined_matrix[i, j] <- paste0(Correlations[i, j], ", ", p_value_matrix[i, j])
  }
}

# -------------
# Correlations with UMSARS ------------------------------

# UMSARS ALpha SYN CSF ******************************
PD01PD03ToExclude <- read_xlsx(path="Source/PD01PD03ToExclude.xlsx", skip=0, col_types = "text", trim_ws = TRUE)

biofluidsForPaulo <- readRDS("Source/biofluidsForPaulo.rds")
biofluidsForPaulo <- biofluidsForPaulo %>% anti_join(PD01PD03ToExclude)
names(biofluidsForPaulo)
biofluidsForPaulo <- biofluidsForPaulo %>% select(idGlobal,datePrelevement, 
                                                  total_alpha_syn_ipp_pgml_csf,
                                                  nfl_ipp_pgml_csf, 
                                                  nfl_pgml_csf, 
                                                  nfl_ipp_pgml_plas,
                                                  nfl_pgml_plas)
biofluidsForPaulo <- biofluidsForPaulo %>% arrange(idGlobal, datePrelevement) %>% group_by(idGlobal)


biofluidsForPaulo <- data.frame(biofluidsForPaulo %>%
                                  mutate(ALpha_CSF=ifelse(!is.na(total_alpha_syn_ipp_pgml_csf), total_alpha_syn_ipp_pgml_csf, NA)) %>%
                                  mutate(NFL_CSF=ifelse(!is.na(nfl_ipp_pgml_csf), nfl_ipp_pgml_csf, nfl_pgml_csf )) %>%
                                  mutate(NFL_Plasma=ifelse(!is.na(nfl_ipp_pgml_plas ), nfl_ipp_pgml_plas , nfl_pgml_plas))) %>%
  select(idGlobal,datePrelevement,ALpha_CSF,NFL_CSF,NFL_Plasma )



biofluidsForPaulo <- biofluidsForPaulo %>% arrange(idGlobal, datePrelevement) %>% group_by(idGlobal) %>%
  mutate(TIME_STUDY=time_length(difftime(datePrelevement, lag(datePrelevement) ), "years"))  %>%
  mutate(TIME_STUDY=ifelse(is.na(TIME_STUDY), 0, TIME_STUDY)) %>%
  mutate(TIME_STUDY=cumsum(TIME_STUDY))


First_ALpha_CSF <- biofluidsForPaulo %>% select(idGlobal, TIME_STUDY, datePrelevement, ALpha_CSF) %>% drop_na() %>%
  group_by(idGlobal) %>% filter(datePrelevement==min(datePrelevement)) %>%  distinct() 

Second_ALpha_CSF <- biofluidsForPaulo %>% select(idGlobal, TIME_STUDY, datePrelevement, ALpha_CSF) %>% drop_na() %>%
  group_by(idGlobal) %>% filter(datePrelevement!=min(datePrelevement)) %>%
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  select(-c(Elapsed)) %>% distinct() 



Delta <- Second_ALpha_CSF %>% select(idGlobal, TIME_STUDY, ALpha_CSF) %>%
  inner_join(First_ALpha_CSF %>% select(idGlobal, ALpha_CSF) %>% rename("Baseline"="ALpha_CSF")) %>%
  mutate(Delta=(ALpha_CSF-Baseline )/TIME_STUDY ) %>% select(idGlobal, Delta)


Baseline_UMSARS1 <- UMSARS1 %>% inner_join(First_ALpha_CSF, by=c("NUM"="idGlobal")) %>% 
  mutate(Elapsed=abs(as.numeric(DATECONSULT-datePrelevement))) %>%
  group_by(NUM) %>% drop_na() %>% filter(Elapsed==min(Elapsed)) %>% distinct() %>% 
  select(NUM, DATECONSULT, UMSARS1_TOT_FDA, Elapsed) %>% 
  rename("Baseline_DATECONSULT"="DATECONSULT", 
         "Baseline_UMSARS1_TOT_FDA"="UMSARS1_TOT_FDA", 
         "Baseline_Elapsed"="Elapsed")

Year_1_UMSARS1 <- UMSARS1 %>% inner_join(Second_ALpha_CSF, by=c("NUM"="idGlobal")) %>% 
  mutate(Elapsed=abs(as.numeric(DATECONSULT-datePrelevement))) %>%
  group_by(NUM) %>% drop_na() %>% filter(Elapsed==min(Elapsed)) %>% distinct() %>% 
  select(NUM, DATECONSULT, UMSARS1_TOT_FDA, Elapsed) %>% 
  rename("Year1_DATECONSULT"="DATECONSULT", 
         "Year1_UMSARS1_TOT_FDA"="UMSARS1_TOT_FDA", 
         "Year1_Elapsed"="Elapsed")



Year_1_UMSARS1 %>% inner_join(Baseline_UMSARS1) %>%
  filter(Year1_DATECONSULT!=Baseline_DATECONSULT) %>% ungroup() %>%
  mutate(Delta=Year1_UMSARS1_TOT_FDA-Baseline_UMSARS1_TOT_FDA) %>%
  mutate(TIME_STUDY=time_length(difftime(Year1_DATECONSULT , Baseline_DATECONSULT)  , "years")) %>%
  summarise(mean=mean(TIME_STUDY*12), sd=sd(TIME_STUDY*12)) #12.4  4.86
ggplot(aes(TIME_STUDY*12)) +
  geom_density(size=2, fill="midnightblue", alpha=0.5)+
  xlim(0,24) + xlab("\n Number of Months Elapsed \n Between The 2 UMSARS Records") +
  ylab("Patient density \n") +
  theme_minimal()

UMSARS_Delta <- Year_1_UMSARS1 %>% inner_join(Baseline_UMSARS1) %>%
  filter(Year1_DATECONSULT!=Baseline_DATECONSULT) %>% ungroup() %>%
  mutate(Delta=Year1_UMSARS1_TOT_FDA-Baseline_UMSARS1_TOT_FDA) %>%
  mutate(TIME_STUDY=time_length(difftime(Year1_DATECONSULT , Baseline_DATECONSULT)  , "years"))  %>%
  mutate(Delta=Delta/TIME_STUDY) %>%
  select(NUM, Delta) # 6


Delta <- UMSARS_Delta %>% rename("UMSARS"="Delta") %>% inner_join(Delta, by=c("NUM"="idGlobal"))



Delta <-  Delta %>% select(-NUM)

Correlations <- Delta %>% cor(method = "spearman")

round(Correlations, 3)

get_p_value <- function(x, y) {
  cor_test <- cor.test(x, y, method = "spearman")
  broom::tidy(cor_test)$p.value
}

p_value_matrix <- matrix(NA, ncol = 2, nrow = 2, 
                         dimnames = list(names(Delta), names(Delta)))


for (i in 1:ncol(Delta)) {
  for (j in 1:ncol(Delta)) {
    p_value_matrix[i, j] <- get_p_value(Delta[[i]], Delta[[j]])
  }
}

Correlations <- round(Correlations, 3)
p_value_matrix <- round(p_value_matrix, 3)


combined_matrix <- matrix(NA, ncol = 2, nrow = 2, 
                          dimnames = list(names(Delta), names(Delta)))


for (i in 1:ncol(Delta)) {
  for (j in 1:ncol(Delta)) {
    combined_matrix[i, j] <- paste0(Correlations[i, j], ", ", p_value_matrix[i, j])
  }
}







# UMSARS Nfl CSF ******************************

PD01PD03ToExclude <- read_xlsx(path="Source/PD01PD03ToExclude.xlsx", skip=0, col_types = "text", trim_ws = TRUE)

biofluidsForPaulo <- readRDS("Source/biofluidsForPaulo.rds")
biofluidsForPaulo <- biofluidsForPaulo %>% anti_join(PD01PD03ToExclude)
names(biofluidsForPaulo)
biofluidsForPaulo <- biofluidsForPaulo %>% select(idGlobal,datePrelevement, 
                                                  total_alpha_syn_ipp_pgml_csf,
                                                  nfl_ipp_pgml_csf, 
                                                  nfl_pgml_csf, 
                                                  nfl_ipp_pgml_plas,
                                                  nfl_pgml_plas)
biofluidsForPaulo <- biofluidsForPaulo %>% arrange(idGlobal, datePrelevement) %>% group_by(idGlobal)


biofluidsForPaulo <- data.frame(biofluidsForPaulo %>%
                                  mutate(ALpha_CSF=ifelse(!is.na(total_alpha_syn_ipp_pgml_csf), total_alpha_syn_ipp_pgml_csf, NA)) %>%
                                  mutate(NFL_CSF=ifelse(!is.na(nfl_ipp_pgml_csf), nfl_ipp_pgml_csf, nfl_pgml_csf )) %>%
                                  mutate(NFL_Plasma=ifelse(!is.na(nfl_ipp_pgml_plas ), nfl_ipp_pgml_plas , nfl_pgml_plas))) %>%
  select(idGlobal,datePrelevement,ALpha_CSF,NFL_CSF,NFL_Plasma )



biofluidsForPaulo <- biofluidsForPaulo %>% arrange(idGlobal, datePrelevement) %>% group_by(idGlobal) %>%
  mutate(TIME_STUDY=time_length(difftime(datePrelevement, lag(datePrelevement) ), "years"))  %>%
  mutate(TIME_STUDY=ifelse(is.na(TIME_STUDY), 0, TIME_STUDY)) %>%
  mutate(TIME_STUDY=cumsum(TIME_STUDY))


First_NFL_CSF <- biofluidsForPaulo %>% select(idGlobal, TIME_STUDY, datePrelevement, NFL_CSF) %>% drop_na() %>%
  group_by(idGlobal) %>% filter(datePrelevement==min(datePrelevement)) %>%  distinct() 

Second_NFL_CSF <- biofluidsForPaulo %>% select(idGlobal, TIME_STUDY, datePrelevement, NFL_CSF) %>% drop_na() %>%
  group_by(idGlobal) %>% filter(datePrelevement!=min(datePrelevement)) %>%
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  select(-c(Elapsed)) %>% distinct() 



Delta <- Second_NFL_CSF %>% select(idGlobal, TIME_STUDY, NFL_CSF) %>%
  inner_join(First_NFL_CSF %>% select(idGlobal, NFL_CSF) %>% rename("Baseline"="NFL_CSF")) %>%
  mutate(Delta=(NFL_CSF-Baseline )/TIME_STUDY ) %>% select(idGlobal, Delta)


Baseline_UMSARS1 <- UMSARS1 %>% inner_join(First_NFL_CSF, by=c("NUM"="idGlobal")) %>% 
  mutate(Elapsed=abs(as.numeric(DATECONSULT-datePrelevement))) %>%
  group_by(NUM) %>% drop_na() %>% filter(Elapsed==min(Elapsed)) %>% distinct() %>% 
  select(NUM, DATECONSULT, UMSARS1_TOT_FDA, Elapsed) %>% 
  rename("Baseline_DATECONSULT"="DATECONSULT", 
         "Baseline_UMSARS1_TOT_FDA"="UMSARS1_TOT_FDA", 
         "Baseline_Elapsed"="Elapsed")

Year_1_UMSARS1 <- UMSARS1 %>% inner_join(Second_NFL_CSF, by=c("NUM"="idGlobal")) %>% 
  mutate(Elapsed=abs(as.numeric(DATECONSULT-datePrelevement))) %>%
  group_by(NUM) %>% drop_na() %>% filter(Elapsed==min(Elapsed)) %>% distinct() %>% 
  select(NUM, DATECONSULT, UMSARS1_TOT_FDA, Elapsed) %>% 
  rename("Year1_DATECONSULT"="DATECONSULT", 
         "Year1_UMSARS1_TOT_FDA"="UMSARS1_TOT_FDA", 
         "Year1_Elapsed"="Elapsed")



Year_1_UMSARS1 %>% inner_join(Baseline_UMSARS1) %>%
  filter(Year1_DATECONSULT!=Baseline_DATECONSULT) %>% ungroup() %>%
  mutate(Delta=Year1_UMSARS1_TOT_FDA-Baseline_UMSARS1_TOT_FDA) %>%
  mutate(TIME_STUDY=time_length(difftime(Year1_DATECONSULT , Baseline_DATECONSULT)  , "years")) %>%
  summarise(mean=mean(TIME_STUDY*12), sd=sd(TIME_STUDY*12)) #12.2  4.06
ggplot(aes(TIME_STUDY*12)) +
  geom_density(size=2, fill="midnightblue", alpha=0.5)+
  xlim(0,24) + xlab("\n Number of Months Elapsed \n Between The 2 UMSARS Records") +
  ylab("Patient density \n") +
  theme_minimal()

UMSARS_Delta <- Year_1_UMSARS1 %>% inner_join(Baseline_UMSARS1) %>%
  filter(Year1_DATECONSULT!=Baseline_DATECONSULT) %>% ungroup() %>%
  mutate(Delta=Year1_UMSARS1_TOT_FDA-Baseline_UMSARS1_TOT_FDA) %>%
  mutate(TIME_STUDY=time_length(difftime(Year1_DATECONSULT , Baseline_DATECONSULT)  , "years"))  %>%
  mutate(Delta=Delta/TIME_STUDY) %>%
  select(NUM, Delta) # 12


Delta <- UMSARS_Delta %>% rename("UMSARS"="Delta") %>% inner_join(Delta, by=c("NUM"="idGlobal"))



Delta <-  Delta %>% select(-NUM)

Correlations <- Delta %>% cor(method = "spearman")

round(Correlations, 3)

get_p_value <- function(x, y) {
  cor_test <- cor.test(x, y, method = "spearman")
  broom::tidy(cor_test)$p.value
}

p_value_matrix <- matrix(NA, ncol = 2, nrow = 2, 
                         dimnames = list(names(Delta), names(Delta)))


for (i in 1:ncol(Delta)) {
  for (j in 1:ncol(Delta)) {
    p_value_matrix[i, j] <- get_p_value(Delta[[i]], Delta[[j]])
  }
}

Correlations <- round(Correlations, 3)
p_value_matrix <- round(p_value_matrix, 3)


combined_matrix <- matrix(NA, ncol = 2, nrow = 2, 
                          dimnames = list(names(Delta), names(Delta)))


for (i in 1:ncol(Delta)) {
  for (j in 1:ncol(Delta)) {
    combined_matrix[i, j] <- paste0(Correlations[i, j], ", ", p_value_matrix[i, j])
  }
}






#  Nfl Plasma ******************************

PD01PD03ToExclude <- read_xlsx(path="Source/PD01PD03ToExclude.xlsx", skip=0, col_types = "text", trim_ws = TRUE)

biofluidsForPaulo <- readRDS("Source/biofluidsForPaulo.rds")
biofluidsForPaulo <- biofluidsForPaulo %>% anti_join(PD01PD03ToExclude)
names(biofluidsForPaulo)
biofluidsForPaulo <- biofluidsForPaulo %>% select(idGlobal,datePrelevement, 
                                                  total_alpha_syn_ipp_pgml_csf,
                                                  nfl_ipp_pgml_csf, 
                                                  nfl_pgml_csf, 
                                                  nfl_ipp_pgml_plas,
                                                  nfl_pgml_plas)
biofluidsForPaulo <- biofluidsForPaulo %>% arrange(idGlobal, datePrelevement) %>% group_by(idGlobal)


biofluidsForPaulo <- data.frame(biofluidsForPaulo %>%
                                  mutate(ALpha_CSF=ifelse(!is.na(total_alpha_syn_ipp_pgml_csf), total_alpha_syn_ipp_pgml_csf, NA)) %>%
                                  mutate(NFL_CSF=ifelse(!is.na(nfl_ipp_pgml_csf), nfl_ipp_pgml_csf, nfl_pgml_csf )) %>%
                                  mutate(NFL_Plasma=ifelse(!is.na(nfl_ipp_pgml_plas ), nfl_ipp_pgml_plas , nfl_pgml_plas))) %>%
  select(idGlobal,datePrelevement,ALpha_CSF,NFL_CSF,NFL_Plasma )



biofluidsForPaulo <- biofluidsForPaulo %>% arrange(idGlobal, datePrelevement) %>% group_by(idGlobal) %>%
  mutate(TIME_STUDY=time_length(difftime(datePrelevement, lag(datePrelevement) ), "years"))  %>%
  mutate(TIME_STUDY=ifelse(is.na(TIME_STUDY), 0, TIME_STUDY)) %>%
  mutate(TIME_STUDY=cumsum(TIME_STUDY))


First_NFL_Plasma <- biofluidsForPaulo %>% select(idGlobal, TIME_STUDY, datePrelevement, NFL_Plasma) %>% drop_na() %>%
  group_by(idGlobal) %>% filter(datePrelevement==min(datePrelevement)) %>%  distinct() 

Second_NFL_Plasma <- biofluidsForPaulo %>% select(idGlobal, TIME_STUDY, datePrelevement, NFL_Plasma) %>% drop_na() %>%
  group_by(idGlobal) %>% filter(datePrelevement!=min(datePrelevement)) %>%
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  select(-c(Elapsed)) %>% distinct() 



Delta <- Second_NFL_Plasma %>% select(idGlobal, TIME_STUDY, NFL_Plasma) %>%
  inner_join(First_NFL_Plasma %>% select(idGlobal, NFL_Plasma) %>% rename("Baseline"="NFL_Plasma")) %>%
  mutate(Delta=(NFL_Plasma-Baseline )/TIME_STUDY ) %>% select(idGlobal, Delta)


Baseline_UMSARS1 <- UMSARS1 %>% inner_join(First_NFL_Plasma, by=c("NUM"="idGlobal")) %>% 
  mutate(Elapsed=abs(as.numeric(DATECONSULT-datePrelevement))) %>%
  group_by(NUM) %>% drop_na() %>% filter(Elapsed==min(Elapsed)) %>% distinct() %>% 
  select(NUM, DATECONSULT, UMSARS1_TOT_FDA, Elapsed) %>% 
  rename("Baseline_DATECONSULT"="DATECONSULT", 
         "Baseline_UMSARS1_TOT_FDA"="UMSARS1_TOT_FDA", 
         "Baseline_Elapsed"="Elapsed")

Year_1_UMSARS1 <- UMSARS1 %>% inner_join(Second_NFL_Plasma, by=c("NUM"="idGlobal")) %>% 
  mutate(Elapsed=abs(as.numeric(DATECONSULT-datePrelevement))) %>%
  group_by(NUM) %>% drop_na() %>% filter(Elapsed==min(Elapsed)) %>% distinct() %>% 
  select(NUM, DATECONSULT, UMSARS1_TOT_FDA, Elapsed) %>% 
  rename("Year1_DATECONSULT"="DATECONSULT", 
         "Year1_UMSARS1_TOT_FDA"="UMSARS1_TOT_FDA", 
         "Year1_Elapsed"="Elapsed")



Year_1_UMSARS1 %>% inner_join(Baseline_UMSARS1) %>%
  filter(Year1_DATECONSULT!=Baseline_DATECONSULT) %>% ungroup() %>%
  mutate(Delta=Year1_UMSARS1_TOT_FDA-Baseline_UMSARS1_TOT_FDA) %>%
  mutate(TIME_STUDY=time_length(difftime(Year1_DATECONSULT , Baseline_DATECONSULT)  , "years")) %>%
  summarise(mean=mean(TIME_STUDY*12), sd=sd(TIME_STUDY*12)) #12.2  6.31
ggplot(aes(TIME_STUDY*12)) +
  geom_density(size=2, fill="midnightblue", alpha=0.5)+
  xlim(0,24) + xlab("\n Number of Months Elapsed \n Between The 2 UMSARS Records") +
  ylab("Patient density \n") +
  theme_minimal()

UMSARS_Delta <- Year_1_UMSARS1 %>% inner_join(Baseline_UMSARS1) %>%
  filter(Year1_DATECONSULT!=Baseline_DATECONSULT) %>% ungroup() %>%
  mutate(Delta=Year1_UMSARS1_TOT_FDA-Baseline_UMSARS1_TOT_FDA) %>%
  mutate(TIME_STUDY=time_length(difftime(Year1_DATECONSULT , Baseline_DATECONSULT)  , "years"))  %>%
  mutate(Delta=Delta/TIME_STUDY) %>%
  select(NUM, Delta) # 15


Delta <- UMSARS_Delta %>% rename("UMSARS"="Delta") %>% inner_join(Delta, by=c("NUM"="idGlobal"))



Delta <-  Delta %>% select(-NUM)

Correlations <- Delta %>% cor(method = "spearman")

round(Correlations, 3)

get_p_value <- function(x, y) {
  cor_test <- cor.test(x, y, method = "spearman")
  broom::tidy(cor_test)$p.value
}

p_value_matrix <- matrix(NA, ncol = 2, nrow = 2, 
                         dimnames = list(names(Delta), names(Delta)))


for (i in 1:ncol(Delta)) {
  for (j in 1:ncol(Delta)) {
    p_value_matrix[i, j] <- get_p_value(Delta[[i]], Delta[[j]])
  }
}

Correlations <- round(Correlations, 3)
p_value_matrix <- round(p_value_matrix, 3)


combined_matrix <- matrix(NA, ncol = 2, nrow = 2, 
                          dimnames = list(names(Delta), names(Delta)))


for (i in 1:ncol(Delta)) {
  for (j in 1:ncol(Delta)) {
    combined_matrix[i, j] <- paste0(Correlations[i, j], ", ", p_value_matrix[i, j])
  }
}

# ---------------------
# Correlations with COMPASS ------------------------------



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

COMPASS <- COMPASS %>% select(NUM, DATECONSULT, DomainTotals)





# COMPASS ALpha SYN CSF ******************************
PD01PD03ToExclude <- read_xlsx(path="Source/PD01PD03ToExclude.xlsx", skip=0, col_types = "text", trim_ws = TRUE)

biofluidsForPaulo <- readRDS("Source/biofluidsForPaulo.rds")
biofluidsForPaulo <- biofluidsForPaulo %>% anti_join(PD01PD03ToExclude)
names(biofluidsForPaulo)
biofluidsForPaulo <- biofluidsForPaulo %>% select(idGlobal,datePrelevement, 
                                                  total_alpha_syn_ipp_pgml_csf,
                                                  nfl_ipp_pgml_csf, 
                                                  nfl_pgml_csf, 
                                                  nfl_ipp_pgml_plas,
                                                  nfl_pgml_plas)
biofluidsForPaulo <- biofluidsForPaulo %>% arrange(idGlobal, datePrelevement) %>% group_by(idGlobal)


biofluidsForPaulo <- data.frame(biofluidsForPaulo %>%
                                  mutate(ALpha_CSF=ifelse(!is.na(total_alpha_syn_ipp_pgml_csf), total_alpha_syn_ipp_pgml_csf, NA)) %>%
                                  mutate(NFL_CSF=ifelse(!is.na(nfl_ipp_pgml_csf), nfl_ipp_pgml_csf, nfl_pgml_csf )) %>%
                                  mutate(NFL_Plasma=ifelse(!is.na(nfl_ipp_pgml_plas ), nfl_ipp_pgml_plas , nfl_pgml_plas))) %>%
  select(idGlobal,datePrelevement,ALpha_CSF,NFL_CSF,NFL_Plasma )



biofluidsForPaulo <- biofluidsForPaulo %>% arrange(idGlobal, datePrelevement) %>% group_by(idGlobal) %>%
  mutate(TIME_STUDY=time_length(difftime(datePrelevement, lag(datePrelevement) ), "years"))  %>%
  mutate(TIME_STUDY=ifelse(is.na(TIME_STUDY), 0, TIME_STUDY)) %>%
  mutate(TIME_STUDY=cumsum(TIME_STUDY))


First_ALpha_CSF <- biofluidsForPaulo %>% select(idGlobal, TIME_STUDY, datePrelevement, ALpha_CSF) %>% drop_na() %>%
  group_by(idGlobal) %>% filter(datePrelevement==min(datePrelevement)) %>%  distinct() 

Second_ALpha_CSF <- biofluidsForPaulo %>% select(idGlobal, TIME_STUDY, datePrelevement, ALpha_CSF) %>% drop_na() %>%
  group_by(idGlobal) %>% filter(datePrelevement!=min(datePrelevement)) %>%
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  select(-c(Elapsed)) %>% distinct() 



Delta <- Second_ALpha_CSF %>% select(idGlobal, TIME_STUDY, ALpha_CSF) %>%
  inner_join(First_ALpha_CSF %>% select(idGlobal, ALpha_CSF) %>% rename("Baseline"="ALpha_CSF")) %>%
  mutate(Delta=(ALpha_CSF-Baseline )/TIME_STUDY ) %>% select(idGlobal, Delta)


Baseline_COMPASS <- COMPASS %>% inner_join(First_ALpha_CSF, by=c("NUM"="idGlobal")) %>% 
  mutate(Elapsed=abs(as.numeric(DATECONSULT-datePrelevement))) %>%
  group_by(NUM) %>% drop_na() %>% filter(Elapsed==min(Elapsed)) %>% distinct() %>% 
  select(NUM, DATECONSULT, DomainTotals, Elapsed) %>% 
  rename("Baseline_DATECONSULT"="DATECONSULT", 
         "Baseline_DomainTotals"="DomainTotals", 
         "Baseline_Elapsed"="Elapsed")

Year_1_COMPASS <- COMPASS %>% inner_join(Second_ALpha_CSF, by=c("NUM"="idGlobal")) %>% 
  mutate(Elapsed=abs(as.numeric(DATECONSULT-datePrelevement))) %>%
  group_by(NUM) %>% drop_na() %>% filter(Elapsed==min(Elapsed)) %>% distinct() %>% 
  select(NUM, DATECONSULT, DomainTotals, Elapsed) %>% 
  rename("Year1_DATECONSULT"="DATECONSULT", 
         "Year1_DomainTotals"="DomainTotals", 
         "Year1_Elapsed"="Elapsed")



Year_1_COMPASS %>% inner_join(Baseline_COMPASS) %>%
  filter(Year1_DATECONSULT!=Baseline_DATECONSULT) %>% ungroup() %>%
  mutate(Delta=Year1_DomainTotals-Baseline_DomainTotals) %>%
  mutate(TIME_STUDY=time_length(difftime(Year1_DATECONSULT , Baseline_DATECONSULT)  , "years")) %>%
  summarise(mean=mean(TIME_STUDY*12), sd=sd(TIME_STUDY*12)) #16.2  12.1
ggplot(aes(TIME_STUDY*12)) +
  geom_density(size=2, fill="midnightblue", alpha=0.5)+
  xlim(0,24) + xlab("\n Number of Months Elapsed \n Between The 2 UMSARS Records") +
  ylab("Patient density \n") +
  theme_minimal()

COMPASS_Delta <- Year_1_COMPASS %>% inner_join(Baseline_COMPASS) %>%
  filter(Year1_DATECONSULT!=Baseline_DATECONSULT) %>% ungroup() %>%
  mutate(Delta=Year1_DomainTotals-Baseline_DomainTotals) %>%
  mutate(TIME_STUDY=time_length(difftime(Year1_DATECONSULT , Baseline_DATECONSULT)  , "years"))  %>%
  mutate(Delta=Delta/TIME_STUDY) %>%
  select(NUM, Delta) # 4


Delta <- COMPASS_Delta %>% rename("COMPASS"="Delta") %>% inner_join(Delta, by=c("NUM"="idGlobal"))



Delta <-  Delta %>% select(-NUM)

Correlations <- Delta %>% cor(method = "spearman")

round(Correlations, 3)

get_p_value <- function(x, y) {
  cor_test <- cor.test(x, y, method = "spearman")
  broom::tidy(cor_test)$p.value
}

p_value_matrix <- matrix(NA, ncol = 2, nrow = 2, 
                         dimnames = list(names(Delta), names(Delta)))


for (i in 1:ncol(Delta)) {
  for (j in 1:ncol(Delta)) {
    p_value_matrix[i, j] <- get_p_value(Delta[[i]], Delta[[j]])
  }
}

Correlations <- round(Correlations, 3)
p_value_matrix <- round(p_value_matrix, 3)


combined_matrix <- matrix(NA, ncol = 2, nrow = 2, 
                          dimnames = list(names(Delta), names(Delta)))


for (i in 1:ncol(Delta)) {
  for (j in 1:ncol(Delta)) {
    combined_matrix[i, j] <- paste0(Correlations[i, j], ", ", p_value_matrix[i, j])
  }
}







# COMPASS Nfl CSF ******************************

PD01PD03ToExclude <- read_xlsx(path="Source/PD01PD03ToExclude.xlsx", skip=0, col_types = "text", trim_ws = TRUE)

biofluidsForPaulo <- readRDS("Source/biofluidsForPaulo.rds")
biofluidsForPaulo <- biofluidsForPaulo %>% anti_join(PD01PD03ToExclude)
names(biofluidsForPaulo)
biofluidsForPaulo <- biofluidsForPaulo %>% select(idGlobal,datePrelevement, 
                                                  total_alpha_syn_ipp_pgml_csf,
                                                  nfl_ipp_pgml_csf, 
                                                  nfl_pgml_csf, 
                                                  nfl_ipp_pgml_plas,
                                                  nfl_pgml_plas)
biofluidsForPaulo <- biofluidsForPaulo %>% arrange(idGlobal, datePrelevement) %>% group_by(idGlobal)


biofluidsForPaulo <- data.frame(biofluidsForPaulo %>%
                                  mutate(ALpha_CSF=ifelse(!is.na(total_alpha_syn_ipp_pgml_csf), total_alpha_syn_ipp_pgml_csf, NA)) %>%
                                  mutate(NFL_CSF=ifelse(!is.na(nfl_ipp_pgml_csf), nfl_ipp_pgml_csf, nfl_pgml_csf )) %>%
                                  mutate(NFL_Plasma=ifelse(!is.na(nfl_ipp_pgml_plas ), nfl_ipp_pgml_plas , nfl_pgml_plas))) %>%
  select(idGlobal,datePrelevement,ALpha_CSF,NFL_CSF,NFL_Plasma )



biofluidsForPaulo <- biofluidsForPaulo %>% arrange(idGlobal, datePrelevement) %>% group_by(idGlobal) %>%
  mutate(TIME_STUDY=time_length(difftime(datePrelevement, lag(datePrelevement) ), "years"))  %>%
  mutate(TIME_STUDY=ifelse(is.na(TIME_STUDY), 0, TIME_STUDY)) %>%
  mutate(TIME_STUDY=cumsum(TIME_STUDY))


First_NFL_CSF <- biofluidsForPaulo %>% select(idGlobal, TIME_STUDY, datePrelevement, NFL_CSF) %>% drop_na() %>%
  group_by(idGlobal) %>% filter(datePrelevement==min(datePrelevement)) %>%  distinct() 

Second_NFL_CSF <- biofluidsForPaulo %>% select(idGlobal, TIME_STUDY, datePrelevement, NFL_CSF) %>% drop_na() %>%
  group_by(idGlobal) %>% filter(datePrelevement!=min(datePrelevement)) %>%
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  select(-c(Elapsed)) %>% distinct() 



Delta <- Second_NFL_CSF %>% select(idGlobal, TIME_STUDY, NFL_CSF) %>%
  inner_join(First_NFL_CSF %>% select(idGlobal, NFL_CSF) %>% rename("Baseline"="NFL_CSF")) %>%
  mutate(Delta=(NFL_CSF-Baseline )/TIME_STUDY ) %>% select(idGlobal, Delta)


Baseline_COMPASS <- COMPASS %>% inner_join(First_NFL_CSF, by=c("NUM"="idGlobal")) %>% 
  mutate(Elapsed=abs(as.numeric(DATECONSULT-datePrelevement))) %>%
  group_by(NUM) %>% drop_na() %>% filter(Elapsed==min(Elapsed)) %>% distinct() %>% 
  select(NUM, DATECONSULT, DomainTotals, Elapsed) %>% 
  rename("Baseline_DATECONSULT"="DATECONSULT", 
         "Baseline_DomainTotals"="DomainTotals", 
         "Baseline_Elapsed"="Elapsed")

Year_1_COMPASS <- COMPASS %>% inner_join(Second_NFL_CSF, by=c("NUM"="idGlobal")) %>% 
  mutate(Elapsed=abs(as.numeric(DATECONSULT-datePrelevement))) %>%
  group_by(NUM) %>% drop_na() %>% filter(Elapsed==min(Elapsed)) %>% distinct() %>% 
  select(NUM, DATECONSULT, DomainTotals, Elapsed) %>% 
  rename("Year1_DATECONSULT"="DATECONSULT", 
         "Year1_DomainTotals"="DomainTotals", 
         "Year1_Elapsed"="Elapsed")



Year_1_COMPASS %>% inner_join(Baseline_COMPASS) %>%
  filter(Year1_DATECONSULT!=Baseline_DATECONSULT) %>% ungroup() %>%
  mutate(Delta=Year1_DomainTotals-Baseline_DomainTotals) %>%
  mutate(TIME_STUDY=time_length(difftime(Year1_DATECONSULT , Baseline_DATECONSULT)  , "years")) %>%
  summarise(mean=mean(TIME_STUDY*12), sd=sd(TIME_STUDY*12)) #14.6  8.7
ggplot(aes(TIME_STUDY*12)) +
  geom_density(size=2, fill="midnightblue", alpha=0.5)+
  xlim(0,24) + xlab("\n Number of Months Elapsed \n Between The 2 UMSARS Records") +
  ylab("Patient density \n") +
  theme_minimal()

COMPASS_Delta <- Year_1_COMPASS %>% inner_join(Baseline_COMPASS) %>%
  filter(Year1_DATECONSULT!=Baseline_DATECONSULT) %>% ungroup() %>%
  mutate(Delta=Year1_DomainTotals-Baseline_DomainTotals) %>%
  mutate(TIME_STUDY=time_length(difftime(Year1_DATECONSULT , Baseline_DATECONSULT)  , "years"))  %>%
  mutate(Delta=Delta/TIME_STUDY) %>%
  select(NUM, Delta) # 7


Delta <- COMPASS_Delta %>% rename("COMPASS"="Delta") %>% inner_join(Delta, by=c("NUM"="idGlobal"))



Delta <-  Delta %>% select(-NUM)

Correlations <- Delta %>% cor(method = "spearman")

round(Correlations, 3)

get_p_value <- function(x, y) {
  cor_test <- cor.test(x, y, method = "spearman")
  broom::tidy(cor_test)$p.value
}

p_value_matrix <- matrix(NA, ncol = 2, nrow = 2, 
                         dimnames = list(names(Delta), names(Delta)))


for (i in 1:ncol(Delta)) {
  for (j in 1:ncol(Delta)) {
    p_value_matrix[i, j] <- get_p_value(Delta[[i]], Delta[[j]])
  }
}

Correlations <- round(Correlations, 3)
p_value_matrix <- round(p_value_matrix, 3)


combined_matrix <- matrix(NA, ncol = 2, nrow = 2, 
                          dimnames = list(names(Delta), names(Delta)))


for (i in 1:ncol(Delta)) {
  for (j in 1:ncol(Delta)) {
    combined_matrix[i, j] <- paste0(Correlations[i, j], ", ", p_value_matrix[i, j])
  }
}






#  Nfl Plasma ******************************

PD01PD03ToExclude <- read_xlsx(path="Source/PD01PD03ToExclude.xlsx", skip=0, col_types = "text", trim_ws = TRUE)

biofluidsForPaulo <- readRDS("Source/biofluidsForPaulo.rds")
biofluidsForPaulo <- biofluidsForPaulo %>% anti_join(PD01PD03ToExclude)
names(biofluidsForPaulo)
biofluidsForPaulo <- biofluidsForPaulo %>% select(idGlobal,datePrelevement, 
                                                  total_alpha_syn_ipp_pgml_csf,
                                                  nfl_ipp_pgml_csf, 
                                                  nfl_pgml_csf, 
                                                  nfl_ipp_pgml_plas,
                                                  nfl_pgml_plas)
biofluidsForPaulo <- biofluidsForPaulo %>% arrange(idGlobal, datePrelevement) %>% group_by(idGlobal)


biofluidsForPaulo <- data.frame(biofluidsForPaulo %>%
                                  mutate(ALpha_CSF=ifelse(!is.na(total_alpha_syn_ipp_pgml_csf), total_alpha_syn_ipp_pgml_csf, NA)) %>%
                                  mutate(NFL_CSF=ifelse(!is.na(nfl_ipp_pgml_csf), nfl_ipp_pgml_csf, nfl_pgml_csf )) %>%
                                  mutate(NFL_Plasma=ifelse(!is.na(nfl_ipp_pgml_plas ), nfl_ipp_pgml_plas , nfl_pgml_plas))) %>%
  select(idGlobal,datePrelevement,ALpha_CSF,NFL_CSF,NFL_Plasma )



biofluidsForPaulo <- biofluidsForPaulo %>% arrange(idGlobal, datePrelevement) %>% group_by(idGlobal) %>%
  mutate(TIME_STUDY=time_length(difftime(datePrelevement, lag(datePrelevement) ), "years"))  %>%
  mutate(TIME_STUDY=ifelse(is.na(TIME_STUDY), 0, TIME_STUDY)) %>%
  mutate(TIME_STUDY=cumsum(TIME_STUDY))


First_NFL_Plasma <- biofluidsForPaulo %>% select(idGlobal, TIME_STUDY, datePrelevement, NFL_Plasma) %>% drop_na() %>%
  group_by(idGlobal) %>% filter(datePrelevement==min(datePrelevement)) %>%  distinct() 

Second_NFL_Plasma <- biofluidsForPaulo %>% select(idGlobal, TIME_STUDY, datePrelevement, NFL_Plasma) %>% drop_na() %>%
  group_by(idGlobal) %>% filter(datePrelevement!=min(datePrelevement)) %>%
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  select(-c(Elapsed)) %>% distinct() 



Delta <- Second_NFL_Plasma %>% select(idGlobal, TIME_STUDY, NFL_Plasma) %>%
  inner_join(First_NFL_Plasma %>% select(idGlobal, NFL_Plasma) %>% rename("Baseline"="NFL_Plasma")) %>%
  mutate(Delta=(NFL_Plasma-Baseline )/TIME_STUDY ) %>% select(idGlobal, Delta)


Baseline_COMPASS <- COMPASS %>% inner_join(First_NFL_Plasma, by=c("NUM"="idGlobal")) %>% 
  mutate(Elapsed=abs(as.numeric(DATECONSULT-datePrelevement))) %>%
  group_by(NUM) %>% drop_na() %>% filter(Elapsed==min(Elapsed)) %>% distinct() %>% 
  select(NUM, DATECONSULT, DomainTotals, Elapsed) %>% 
  rename("Baseline_DATECONSULT"="DATECONSULT", 
         "Baseline_DomainTotals"="DomainTotals", 
         "Baseline_Elapsed"="Elapsed")

Year_1_COMPASS <- COMPASS %>% inner_join(Second_NFL_Plasma, by=c("NUM"="idGlobal")) %>% 
  mutate(Elapsed=abs(as.numeric(DATECONSULT-datePrelevement))) %>%
  group_by(NUM) %>% drop_na() %>% filter(Elapsed==min(Elapsed)) %>% distinct() %>% 
  select(NUM, DATECONSULT, DomainTotals, Elapsed) %>% 
  rename("Year1_DATECONSULT"="DATECONSULT", 
         "Year1_DomainTotals"="DomainTotals", 
         "Year1_Elapsed"="Elapsed")



Year_1_COMPASS %>% inner_join(Baseline_COMPASS) %>%
  filter(Year1_DATECONSULT!=Baseline_DATECONSULT) %>% ungroup() %>%
  mutate(Delta=Year1_DomainTotals-Baseline_DomainTotals) %>%
  mutate(TIME_STUDY=time_length(difftime(Year1_DATECONSULT , Baseline_DATECONSULT)  , "years")) %>%
  summarise(mean=mean(TIME_STUDY*12), sd=sd(TIME_STUDY*12)) #14.9  9.34
ggplot(aes(TIME_STUDY*12)) +
  geom_density(size=2, fill="midnightblue", alpha=0.5)+
  xlim(0,24) + xlab("\n Number of Months Elapsed \n Between The 2 UMSARS Records") +
  ylab("Patient density \n") +
  theme_minimal()

COMPASS_Delta <- Year_1_COMPASS %>% inner_join(Baseline_COMPASS) %>%
  filter(Year1_DATECONSULT!=Baseline_DATECONSULT) %>% ungroup() %>%
  mutate(Delta=Year1_DomainTotals-Baseline_DomainTotals) %>%
  mutate(TIME_STUDY=time_length(difftime(Year1_DATECONSULT , Baseline_DATECONSULT)  , "years"))  %>%
  mutate(Delta=Delta/TIME_STUDY) %>%
  select(NUM, Delta) # 15


Delta <- COMPASS_Delta %>% rename("COMPASS"="Delta") %>% inner_join(Delta, by=c("NUM"="idGlobal"))



Delta <-  Delta %>% select(-NUM)

Correlations <- Delta %>% cor(method = "spearman")

round(Correlations, 3)

get_p_value <- function(x, y) {
  cor_test <- cor.test(x, y, method = "spearman")
  broom::tidy(cor_test)$p.value
}

p_value_matrix <- matrix(NA, ncol = 2, nrow = 2, 
                         dimnames = list(names(Delta), names(Delta)))


for (i in 1:ncol(Delta)) {
  for (j in 1:ncol(Delta)) {
    p_value_matrix[i, j] <- get_p_value(Delta[[i]], Delta[[j]])
  }
}

Correlations <- round(Correlations, 3)
p_value_matrix <- round(p_value_matrix, 3)


combined_matrix <- matrix(NA, ncol = 2, nrow = 2, 
                          dimnames = list(names(Delta), names(Delta)))


for (i in 1:ncol(Delta)) {
  for (j in 1:ncol(Delta)) {
    combined_matrix[i, j] <- paste0(Correlations[i, j], ", ", p_value_matrix[i, j])
  }
}

# ---------------------
# Correlations with MSA QoL ------------------------------



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

mean(SCHRAG$number_na)

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


SCHRAG <- SCHRAG %>% select(NUM, DATECONSULT, SCHRAG_TOT_v2)





# SCHRAG ALpha SYN CSF ******************************
PD01PD03ToExclude <- read_xlsx(path="Source/PD01PD03ToExclude.xlsx", skip=0, col_types = "text", trim_ws = TRUE)

biofluidsForPaulo <- readRDS("Source/biofluidsForPaulo.rds")
biofluidsForPaulo <- biofluidsForPaulo %>% anti_join(PD01PD03ToExclude)
names(biofluidsForPaulo)
biofluidsForPaulo <- biofluidsForPaulo %>% select(idGlobal,datePrelevement, 
                                                  total_alpha_syn_ipp_pgml_csf,
                                                  nfl_ipp_pgml_csf, 
                                                  nfl_pgml_csf, 
                                                  nfl_ipp_pgml_plas,
                                                  nfl_pgml_plas)
biofluidsForPaulo <- biofluidsForPaulo %>% arrange(idGlobal, datePrelevement) %>% group_by(idGlobal)


biofluidsForPaulo <- data.frame(biofluidsForPaulo %>%
                                  mutate(ALpha_CSF=ifelse(!is.na(total_alpha_syn_ipp_pgml_csf), total_alpha_syn_ipp_pgml_csf, NA)) %>%
                                  mutate(NFL_CSF=ifelse(!is.na(nfl_ipp_pgml_csf), nfl_ipp_pgml_csf, nfl_pgml_csf )) %>%
                                  mutate(NFL_Plasma=ifelse(!is.na(nfl_ipp_pgml_plas ), nfl_ipp_pgml_plas , nfl_pgml_plas))) %>%
  select(idGlobal,datePrelevement,ALpha_CSF,NFL_CSF,NFL_Plasma )



biofluidsForPaulo <- biofluidsForPaulo %>% arrange(idGlobal, datePrelevement) %>% group_by(idGlobal) %>%
  mutate(TIME_STUDY=time_length(difftime(datePrelevement, lag(datePrelevement) ), "years"))  %>%
  mutate(TIME_STUDY=ifelse(is.na(TIME_STUDY), 0, TIME_STUDY)) %>%
  mutate(TIME_STUDY=cumsum(TIME_STUDY))


First_ALpha_CSF <- biofluidsForPaulo %>% select(idGlobal, TIME_STUDY, datePrelevement, ALpha_CSF) %>% drop_na() %>%
  group_by(idGlobal) %>% filter(datePrelevement==min(datePrelevement)) %>%  distinct() 

Second_ALpha_CSF <- biofluidsForPaulo %>% select(idGlobal, TIME_STUDY, datePrelevement, ALpha_CSF) %>% drop_na() %>%
  group_by(idGlobal) %>% filter(datePrelevement!=min(datePrelevement)) %>%
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  select(-c(Elapsed)) %>% distinct() 



Delta <- Second_ALpha_CSF %>% select(idGlobal, TIME_STUDY, ALpha_CSF) %>%
  inner_join(First_ALpha_CSF %>% select(idGlobal, ALpha_CSF) %>% rename("Baseline"="ALpha_CSF")) %>%
  mutate(Delta=(ALpha_CSF-Baseline )/TIME_STUDY ) %>% select(idGlobal, Delta)


Baseline_SCHRAG <- SCHRAG %>% inner_join(First_ALpha_CSF, by=c("NUM"="idGlobal")) %>% 
  mutate(Elapsed=abs(as.numeric(DATECONSULT-datePrelevement))) %>%
  group_by(NUM) %>% drop_na() %>% filter(Elapsed==min(Elapsed)) %>% distinct() %>% 
  select(NUM, DATECONSULT, SCHRAG_TOT_v2, Elapsed) %>% 
  rename("Baseline_DATECONSULT"="DATECONSULT", 
         "Baseline_SCHRAG_TOT_v2"="SCHRAG_TOT_v2", 
         "Baseline_Elapsed"="Elapsed")

Year_1_SCHRAG <- SCHRAG %>% inner_join(Second_ALpha_CSF, by=c("NUM"="idGlobal")) %>% 
  mutate(Elapsed=abs(as.numeric(DATECONSULT-datePrelevement))) %>%
  group_by(NUM) %>% drop_na() %>% filter(Elapsed==min(Elapsed)) %>% distinct() %>% 
  select(NUM, DATECONSULT, SCHRAG_TOT_v2, Elapsed) %>% 
  rename("Year1_DATECONSULT"="DATECONSULT", 
         "Year1_SCHRAG_TOT_v2"="SCHRAG_TOT_v2", 
         "Year1_Elapsed"="Elapsed")



Year_1_SCHRAG %>% inner_join(Baseline_SCHRAG) %>%
  filter(Year1_DATECONSULT!=Baseline_DATECONSULT) %>% ungroup() %>%
  mutate(Delta=Year1_SCHRAG_TOT_v2-Baseline_SCHRAG_TOT_v2) %>%
  mutate(TIME_STUDY=time_length(difftime(Year1_DATECONSULT , Baseline_DATECONSULT)  , "years")) %>%
  summarise(mean=mean(TIME_STUDY*12), sd=sd(TIME_STUDY*12)) #16.6  11.5
ggplot(aes(TIME_STUDY*12)) +
  geom_density(size=2, fill="midnightblue", alpha=0.5)+
  xlim(0,24) + xlab("\n Number of Months Elapsed \n Between The 2 UMSARS Records") +
  ylab("Patient density \n") +
  theme_minimal()

SCHRAG_Delta <- Year_1_SCHRAG %>% inner_join(Baseline_SCHRAG) %>%
  filter(Year1_DATECONSULT!=Baseline_DATECONSULT) %>% ungroup() %>%
  mutate(Delta=Year1_SCHRAG_TOT_v2-Baseline_SCHRAG_TOT_v2) %>%
  mutate(TIME_STUDY=time_length(difftime(Year1_DATECONSULT , Baseline_DATECONSULT)  , "years"))  %>%
  mutate(Delta=Delta/TIME_STUDY) %>%
  select(NUM, Delta) # 4


Delta <- SCHRAG_Delta %>% rename("SCHRAG"="Delta") %>% inner_join(Delta, by=c("NUM"="idGlobal"))



Delta <-  Delta %>% select(-NUM)

Correlations <- Delta %>% cor(method = "spearman")

round(Correlations, 3)

get_p_value <- function(x, y) {
  cor_test <- cor.test(x, y, method = "spearman")
  broom::tidy(cor_test)$p.value
}

p_value_matrix <- matrix(NA, ncol = 2, nrow = 2, 
                         dimnames = list(names(Delta), names(Delta)))


for (i in 1:ncol(Delta)) {
  for (j in 1:ncol(Delta)) {
    p_value_matrix[i, j] <- get_p_value(Delta[[i]], Delta[[j]])
  }
}

Correlations <- round(Correlations, 3)
p_value_matrix <- round(p_value_matrix, 3)


combined_matrix <- matrix(NA, ncol = 2, nrow = 2, 
                          dimnames = list(names(Delta), names(Delta)))


for (i in 1:ncol(Delta)) {
  for (j in 1:ncol(Delta)) {
    combined_matrix[i, j] <- paste0(Correlations[i, j], ", ", p_value_matrix[i, j])
  }
}







# SCHRAG Nfl CSF ******************************

PD01PD03ToExclude <- read_xlsx(path="Source/PD01PD03ToExclude.xlsx", skip=0, col_types = "text", trim_ws = TRUE)

biofluidsForPaulo <- readRDS("Source/biofluidsForPaulo.rds")
biofluidsForPaulo <- biofluidsForPaulo %>% anti_join(PD01PD03ToExclude)
names(biofluidsForPaulo)
biofluidsForPaulo <- biofluidsForPaulo %>% select(idGlobal,datePrelevement, 
                                                  total_alpha_syn_ipp_pgml_csf,
                                                  nfl_ipp_pgml_csf, 
                                                  nfl_pgml_csf, 
                                                  nfl_ipp_pgml_plas,
                                                  nfl_pgml_plas)
biofluidsForPaulo <- biofluidsForPaulo %>% arrange(idGlobal, datePrelevement) %>% group_by(idGlobal)


biofluidsForPaulo <- data.frame(biofluidsForPaulo %>%
                                  mutate(ALpha_CSF=ifelse(!is.na(total_alpha_syn_ipp_pgml_csf), total_alpha_syn_ipp_pgml_csf, NA)) %>%
                                  mutate(NFL_CSF=ifelse(!is.na(nfl_ipp_pgml_csf), nfl_ipp_pgml_csf, nfl_pgml_csf )) %>%
                                  mutate(NFL_Plasma=ifelse(!is.na(nfl_ipp_pgml_plas ), nfl_ipp_pgml_plas , nfl_pgml_plas))) %>%
  select(idGlobal,datePrelevement,ALpha_CSF,NFL_CSF,NFL_Plasma )



biofluidsForPaulo <- biofluidsForPaulo %>% arrange(idGlobal, datePrelevement) %>% group_by(idGlobal) %>%
  mutate(TIME_STUDY=time_length(difftime(datePrelevement, lag(datePrelevement) ), "years"))  %>%
  mutate(TIME_STUDY=ifelse(is.na(TIME_STUDY), 0, TIME_STUDY)) %>%
  mutate(TIME_STUDY=cumsum(TIME_STUDY))


First_NFL_CSF <- biofluidsForPaulo %>% select(idGlobal, TIME_STUDY, datePrelevement, NFL_CSF) %>% drop_na() %>%
  group_by(idGlobal) %>% filter(datePrelevement==min(datePrelevement)) %>%  distinct() 

Second_NFL_CSF <- biofluidsForPaulo %>% select(idGlobal, TIME_STUDY, datePrelevement, NFL_CSF) %>% drop_na() %>%
  group_by(idGlobal) %>% filter(datePrelevement!=min(datePrelevement)) %>%
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  select(-c(Elapsed)) %>% distinct() 



Delta <- Second_NFL_CSF %>% select(idGlobal, TIME_STUDY, NFL_CSF) %>%
  inner_join(First_NFL_CSF %>% select(idGlobal, NFL_CSF) %>% rename("Baseline"="NFL_CSF")) %>%
  mutate(Delta=(NFL_CSF-Baseline )/TIME_STUDY ) %>% select(idGlobal, Delta)


Baseline_SCHRAG <- SCHRAG %>% inner_join(First_NFL_CSF, by=c("NUM"="idGlobal")) %>% 
  mutate(Elapsed=abs(as.numeric(DATECONSULT-datePrelevement))) %>%
  group_by(NUM) %>% drop_na() %>% filter(Elapsed==min(Elapsed)) %>% distinct() %>% 
  select(NUM, DATECONSULT, SCHRAG_TOT_v2, Elapsed) %>% 
  rename("Baseline_DATECONSULT"="DATECONSULT", 
         "Baseline_SCHRAG_TOT_v2"="SCHRAG_TOT_v2", 
         "Baseline_Elapsed"="Elapsed")

Year_1_SCHRAG <- SCHRAG %>% inner_join(Second_NFL_CSF, by=c("NUM"="idGlobal")) %>% 
  mutate(Elapsed=abs(as.numeric(DATECONSULT-datePrelevement))) %>%
  group_by(NUM) %>% drop_na() %>% filter(Elapsed==min(Elapsed)) %>% distinct() %>% 
  select(NUM, DATECONSULT, SCHRAG_TOT_v2, Elapsed) %>% 
  rename("Year1_DATECONSULT"="DATECONSULT", 
         "Year1_SCHRAG_TOT_v2"="SCHRAG_TOT_v2", 
         "Year1_Elapsed"="Elapsed")



Year_1_SCHRAG %>% inner_join(Baseline_SCHRAG) %>%
  filter(Year1_DATECONSULT!=Baseline_DATECONSULT) %>% ungroup() %>%
  mutate(Delta=Year1_SCHRAG_TOT_v2-Baseline_SCHRAG_TOT_v2) %>%
  mutate(TIME_STUDY=time_length(difftime(Year1_DATECONSULT , Baseline_DATECONSULT)  , "years")) %>%
  summarise(mean=mean(TIME_STUDY*12), sd=sd(TIME_STUDY*12)) #14.8  7.2
ggplot(aes(TIME_STUDY*12)) +
  geom_density(size=2, fill="midnightblue", alpha=0.5)+
  xlim(0,24) + xlab("\n Number of Months Elapsed \n Between The 2 UMSARS Records") +
  ylab("Patient density \n") +
  theme_minimal()

SCHRAG_Delta <- Year_1_SCHRAG %>% inner_join(Baseline_SCHRAG) %>%
  filter(Year1_DATECONSULT!=Baseline_DATECONSULT) %>% ungroup() %>%
  mutate(Delta=Year1_SCHRAG_TOT_v2-Baseline_SCHRAG_TOT_v2) %>%
  mutate(TIME_STUDY=time_length(difftime(Year1_DATECONSULT , Baseline_DATECONSULT)  , "years"))  %>%
  mutate(Delta=Delta/TIME_STUDY) %>%
  select(NUM, Delta) # 7


Delta <- SCHRAG_Delta %>% rename("SCHRAG"="Delta") %>% inner_join(Delta, by=c("NUM"="idGlobal"))



Delta <-  Delta %>% select(-NUM)

Correlations <- Delta %>% cor(method = "spearman")

round(Correlations, 3)

get_p_value <- function(x, y) {
  cor_test <- cor.test(x, y, method = "spearman")
  broom::tidy(cor_test)$p.value
}

p_value_matrix <- matrix(NA, ncol = 2, nrow = 2, 
                         dimnames = list(names(Delta), names(Delta)))


for (i in 1:ncol(Delta)) {
  for (j in 1:ncol(Delta)) {
    p_value_matrix[i, j] <- get_p_value(Delta[[i]], Delta[[j]])
  }
}

Correlations <- round(Correlations, 3)
p_value_matrix <- round(p_value_matrix, 3)


combined_matrix <- matrix(NA, ncol = 2, nrow = 2, 
                          dimnames = list(names(Delta), names(Delta)))


for (i in 1:ncol(Delta)) {
  for (j in 1:ncol(Delta)) {
    combined_matrix[i, j] <- paste0(Correlations[i, j], ", ", p_value_matrix[i, j])
  }
}






#  SCHRAG Nfl Plasma ******************************

PD01PD03ToExclude <- read_xlsx(path="Source/PD01PD03ToExclude.xlsx", skip=0, col_types = "text", trim_ws = TRUE)

biofluidsForPaulo <- readRDS("Source/biofluidsForPaulo.rds")
biofluidsForPaulo <- biofluidsForPaulo %>% anti_join(PD01PD03ToExclude)
names(biofluidsForPaulo)
biofluidsForPaulo <- biofluidsForPaulo %>% select(idGlobal,datePrelevement, 
                                                  total_alpha_syn_ipp_pgml_csf,
                                                  nfl_ipp_pgml_csf, 
                                                  nfl_pgml_csf, 
                                                  nfl_ipp_pgml_plas,
                                                  nfl_pgml_plas)
biofluidsForPaulo <- biofluidsForPaulo %>% arrange(idGlobal, datePrelevement) %>% group_by(idGlobal)


biofluidsForPaulo <- data.frame(biofluidsForPaulo %>%
                                  mutate(ALpha_CSF=ifelse(!is.na(total_alpha_syn_ipp_pgml_csf), total_alpha_syn_ipp_pgml_csf, NA)) %>%
                                  mutate(NFL_CSF=ifelse(!is.na(nfl_ipp_pgml_csf), nfl_ipp_pgml_csf, nfl_pgml_csf )) %>%
                                  mutate(NFL_Plasma=ifelse(!is.na(nfl_ipp_pgml_plas ), nfl_ipp_pgml_plas , nfl_pgml_plas))) %>%
  select(idGlobal,datePrelevement,ALpha_CSF,NFL_CSF,NFL_Plasma )



biofluidsForPaulo <- biofluidsForPaulo %>% arrange(idGlobal, datePrelevement) %>% group_by(idGlobal) %>%
  mutate(TIME_STUDY=time_length(difftime(datePrelevement, lag(datePrelevement) ), "years"))  %>%
  mutate(TIME_STUDY=ifelse(is.na(TIME_STUDY), 0, TIME_STUDY)) %>%
  mutate(TIME_STUDY=cumsum(TIME_STUDY))


First_NFL_Plasma <- biofluidsForPaulo %>% select(idGlobal, TIME_STUDY, datePrelevement, NFL_Plasma) %>% drop_na() %>%
  group_by(idGlobal) %>% filter(datePrelevement==min(datePrelevement)) %>%  distinct() 

Second_NFL_Plasma <- biofluidsForPaulo %>% select(idGlobal, TIME_STUDY, datePrelevement, NFL_Plasma) %>% drop_na() %>%
  group_by(idGlobal) %>% filter(datePrelevement!=min(datePrelevement)) %>%
  mutate(Elapsed=abs(1-TIME_STUDY)) %>% filter(Elapsed==min(Elapsed)) %>%
  select(-c(Elapsed)) %>% distinct() 



Delta <- Second_NFL_Plasma %>% select(idGlobal, TIME_STUDY, NFL_Plasma) %>%
  inner_join(First_NFL_Plasma %>% select(idGlobal, NFL_Plasma) %>% rename("Baseline"="NFL_Plasma")) %>%
  mutate(Delta=(NFL_Plasma-Baseline )/TIME_STUDY ) %>% select(idGlobal, Delta)


Baseline_SCHRAG <- SCHRAG %>% inner_join(First_NFL_Plasma, by=c("NUM"="idGlobal")) %>% 
  mutate(Elapsed=abs(as.numeric(DATECONSULT-datePrelevement))) %>%
  group_by(NUM) %>% drop_na() %>% filter(Elapsed==min(Elapsed)) %>% distinct() %>% 
  select(NUM, DATECONSULT, SCHRAG_TOT_v2, Elapsed) %>% 
  rename("Baseline_DATECONSULT"="DATECONSULT", 
         "Baseline_SCHRAG_TOT_v2"="SCHRAG_TOT_v2", 
         "Baseline_Elapsed"="Elapsed")

Year_1_SCHRAG <- SCHRAG %>% inner_join(Second_NFL_Plasma, by=c("NUM"="idGlobal")) %>% 
  mutate(Elapsed=abs(as.numeric(DATECONSULT-datePrelevement))) %>%
  group_by(NUM) %>% drop_na() %>% filter(Elapsed==min(Elapsed)) %>% distinct() %>% 
  select(NUM, DATECONSULT, SCHRAG_TOT_v2, Elapsed) %>% 
  rename("Year1_DATECONSULT"="DATECONSULT", 
         "Year1_SCHRAG_TOT_v2"="SCHRAG_TOT_v2", 
         "Year1_Elapsed"="Elapsed")



Year_1_SCHRAG %>% inner_join(Baseline_SCHRAG) %>%
  filter(Year1_DATECONSULT!=Baseline_DATECONSULT) %>% ungroup() %>%
  mutate(Delta=Year1_SCHRAG_TOT_v2-Baseline_SCHRAG_TOT_v2) %>%
  mutate(TIME_STUDY=time_length(difftime(Year1_DATECONSULT , Baseline_DATECONSULT)  , "years")) %>%
  summarise(mean=mean(TIME_STUDY*12), sd=sd(TIME_STUDY*12)) #14.8  8.7
ggplot(aes(TIME_STUDY*12)) +
  geom_density(size=2, fill="midnightblue", alpha=0.5)+
  xlim(0,24) + xlab("\n Number of Months Elapsed \n Between The 2 UMSARS Records") +
  ylab("Patient density \n") +
  theme_minimal()

SCHRAG_Delta <- Year_1_SCHRAG %>% inner_join(Baseline_SCHRAG) %>%
  filter(Year1_DATECONSULT!=Baseline_DATECONSULT) %>% ungroup() %>%
  mutate(Delta=Year1_SCHRAG_TOT_v2-Baseline_SCHRAG_TOT_v2) %>%
  mutate(TIME_STUDY=time_length(difftime(Year1_DATECONSULT , Baseline_DATECONSULT)  , "years"))  %>%
  mutate(Delta=Delta/TIME_STUDY) %>%
  select(NUM, Delta) # 15


Delta <- SCHRAG_Delta %>% rename("SCHRAG"="Delta") %>% inner_join(Delta, by=c("NUM"="idGlobal"))



Delta <-  Delta %>% select(-NUM)

Correlations <- Delta %>% cor(method = "spearman")

round(Correlations, 3)

get_p_value <- function(x, y) {
  cor_test <- cor.test(x, y, method = "spearman")
  broom::tidy(cor_test)$p.value
}

p_value_matrix <- matrix(NA, ncol = 2, nrow = 2, 
                         dimnames = list(names(Delta), names(Delta)))


for (i in 1:ncol(Delta)) {
  for (j in 1:ncol(Delta)) {
    p_value_matrix[i, j] <- get_p_value(Delta[[i]], Delta[[j]])
  }
}

Correlations <- round(Correlations, 3)
p_value_matrix <- round(p_value_matrix, 3)


combined_matrix <- matrix(NA, ncol = 2, nrow = 2, 
                          dimnames = list(names(Delta), names(Delta)))


for (i in 1:ncol(Delta)) {
  for (j in 1:ncol(Delta)) {
    combined_matrix[i, j] <- paste0(Correlations[i, j], ", ", p_value_matrix[i, j])
  }
}

# ---------------------