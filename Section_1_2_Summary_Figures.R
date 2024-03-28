# Summary Plots --------------------

library(tidyverse)
library(data.table)
options(scipen = 999)
# ---------------
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



plot <- AllMSA_Pop_Baseline_671 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
  filter(!is.na(UMSARS1_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  bind_rows(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(UMSARS1) %>% filter(Year==1) %>%
      filter(!is.na(UMSARS1_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() 
  ) %>%
  bind_rows(
    
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(UMSARS1) %>% filter(Year==2) %>%
      filter(!is.na(UMSARS1_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup()
  ) %>%
  bind_rows(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(UMSARS1) %>% filter(Year==3) %>%
      filter(!is.na(UMSARS1_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup()
  )  

palette <- c( "#D45769", "#0099E0","#0072BB", "#00468B")


plot  %>% ggplot(aes(x = TIME_STUDY*12, y = as.factor(Year), fill=as.factor(Year), colour=as.factor(Year) )) +
  theme_minimal() +
  geom_vline(xintercept = c(0, 12, 24, 36), linetype = "dashed", color = "black", alpha = 0.5, size = 0.5) +
  geom_violin(scale = "width", trim = FALSE,  width = 2.0, alpha=0.85, adjust = 0.4) +
  geom_point(position = position_jitter(height = 0.22), size=0.4 , colour="black", alpha = 0.6) +
  geom_boxplot(width = 0.5,  outlier.shape = NA, fill="transparent", notch = TRUE, colour="white", alpha=0.6) +
    labs(x = "\n Months Since Baseline Visit", y = "Visit Year \n") +
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
  scale_x_continuous(breaks = seq(0, 54, by = 6)) 



plot  %>% ggplot(aes(y = UMSARS1_TOT , x = as.factor(Year), fill=as.factor(Year), colour=as.factor(Year) )) +
  theme_minimal() +
  geom_hline(yintercept = c(22, 26, 29, 31), linetype = "dashed", color = "black", alpha = 0.5, size = 0.5) +
  geom_violin(scale = "width", trim = FALSE,  width = 1.0, alpha=0.80, adjust = 0.4) +
  geom_point( position = position_jitter(height = 0.22, width = 0.22), size=1.5 , colour="black", alpha = 0.4) +
  geom_boxplot(width = 0.5,  outlier.shape = NA, fill="transparent", notch = TRUE, colour="white", alpha=0.6) +
  labs(y = " UMSARS 1 Total Score \n", x = "\n Visit Year") +
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
  scale_y_continuous(breaks = seq(0, 55, by = 5)) 


palette <- c( "#D45769", "#0072BB")

plot  %>% 
  mutate(DIAG=ifelse(DIAG=="CB", "MSA-C", "MSA-P")) %>%
  mutate(Year=ifelse(Year==0, "Year 0",
                     ifelse(Year==1, "Year 1 ",
                            ifelse(Year==2, "Year 2",
                                   ifelse(Year==3, "Year 3+", NA))))) %>%
  ggplot(aes(y = UMSARS1_TOT , x = as.factor(DIAG), fill=as.factor(DIAG), colour=as.factor(DIAG) )) +
  facet_wrap(~as.factor(Year), ncol=4) +
  theme_minimal() +
  geom_hline(yintercept = c(22, 26, 29, 31), linetype = "dashed", color = "black", alpha = 0.5, size = 0.5) +
  geom_violin(scale = "width", trim = FALSE,  width = 1.0, alpha=0.70, adjust = 0.4) +
  geom_point( position = position_jitter(height = 0.22, width = 0.22), size=1.5 , colour="black", alpha = 0.4) +
  geom_boxplot(width = 0.5,  outlier.shape = NA, fill="transparent", notch = TRUE, colour="white", alpha=0.6) +
  labs(y = " UMSARS 1 Total Score \n", x = "\n MSA-C vs MSA-P \n Year-over-year") +
  scale_color_manual(values=palette) +
  scale_fill_manual(values=palette) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
       # strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_y_continuous(breaks = seq(0, 55, by = 5))




palette <- c( "#D45769", "#0072BB")

plot  %>% 
  mutate(DIAG=ifelse(DIAGNIV=="PROB", "Probable MSA", "Possible MSA")) %>%
  mutate(Year=ifelse(Year==0, "Year 0",
                     ifelse(Year==1, "Year 1 ",
                            ifelse(Year==2, "Year 2",
                                   ifelse(Year==3, "Year 3+", NA))))) %>%
  ggplot(aes(y = UMSARS1_TOT , x = as.factor(DIAG), fill=as.factor(DIAG), colour=as.factor(DIAG) )) +
  facet_wrap(~as.factor(Year), ncol=4) +
  theme_minimal() +
  geom_hline(yintercept = c(22, 26, 29, 31), linetype = "dashed", color = "black", alpha = 0.5, size = 0.5) +
  geom_violin(scale = "width", trim = FALSE,  width = 1.0, alpha=0.70, adjust = 0.4) +
  geom_point( position = position_jitter(height = 0.22, width = 0.22), size=1.5 , colour="black", alpha = 0.4) +
  geom_boxplot(width = 0.5,  outlier.shape = NA, fill="transparent", notch = TRUE, colour="white", alpha=0.6) +
  labs(y = " UMSARS 1 Total Score \n", x = "\n Probable MSA vs Possible MSA \n Year-over-year") +
  scale_color_manual(values=palette) +
  scale_fill_manual(values=palette) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        # strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_y_continuous(breaks = seq(0, 55, by = 5))





palette <- c( "#D45769", "#0072BB")


UMSARS1 %>% inner_join(AllMSA_Pop_Baseline_671) %>%
  filter(TIME_STUDY<=5) %>%
  ggplot(aes(TIME_STUDY, UMSARS1_TOT, )) +
  geom_line(aes(group=NUM), col="black" , alpha=0.1) +
  geom_jitter(size=0.1, colour="black", alpha=0.4) +
  stat_smooth(method="gam", colour="#00468B", fill="#00468B" , alpha=0.5, lwd=1.5, se=TRUE, formula = y ~ s(x, bs = "cs", k =10))+
  theme_minimal() +
  xlab("\n Elapsed number of years since study enrollment \n [All available patient records up to year 5]") +
  ylab("UMSARS 1 Total Score \n At each evaluation \n") +
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
  scale_x_continuous(breaks = seq(0, 5, by = 0.5)) 


UMSARS1 %>% inner_join(AllMSA_Pop_Baseline_671) %>%
  mutate(DIAG=ifelse(DIAG=="CB", "MSA-C", "MSA-P")) %>%
  filter(TIME_STUDY<=5) %>%
  ggplot(aes(TIME_STUDY, UMSARS1_TOT, group=as.factor(DIAG), col=as.factor(DIAG), fill=as.factor(DIAG))) +
  geom_line(aes(group=NUM), col="black" , alpha=0.1) +
  geom_jitter(size=0.1, colour="black", alpha=0.4) +
  stat_smooth(method="gam", , alpha=0.5, lwd=1.5, se=TRUE, formula = y ~ s(x, bs = "cs", k =10))+
  theme_minimal() +
  xlab("\n MSA-C vs MSA-P \n Elapsed number of years since study enrollment \n [All available patient records up to year 5]") +
  ylab("UMSARS 1 Total Score \n At each evaluation \n") +
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
  scale_x_continuous(breaks = seq(0, 5, by = 0.5)) 



UMSARS1 %>% inner_join(AllMSA_Pop_Baseline_671) %>%
  mutate(DIAG=ifelse(DIAGNIV=="PROB", "Probable MSA", "Possible MSA")) %>%
  filter(TIME_STUDY<=5) %>%
  ggplot(aes(TIME_STUDY, UMSARS1_TOT, group=as.factor(DIAG), col=as.factor(DIAG), fill=as.factor(DIAG))) +
  geom_line(aes(group=NUM), col="black" , alpha=0.1) +
  geom_jitter(size=0.1, colour="black", alpha=0.4) +
  stat_smooth(method="gam", , alpha=0.5, lwd=1.5, se=TRUE, formula = y ~ s(x, bs = "cs", k =10))+
  theme_minimal() +
  xlab("\n Probable vs Possible MSA \n Elapsed number of years since study enrollment \n [All available patient records up to year 5]") +
  ylab("UMSARS 1 Total Score \n At each evaluation \n") +
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
  scale_x_continuous(breaks = seq(0, 5, by = 0.5)) 


# -------------------
# Overall Early CT  -------------------------------

plot <- EarlyCT_Pop_Baseline_319 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
  filter(!is.na(UMSARS1_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  bind_rows(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(UMSARS1) %>% filter(Year==1) %>%
      filter(!is.na(UMSARS1_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() 
  ) %>%
  bind_rows(
    
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(UMSARS1) %>% filter(Year==2) %>%
      filter(!is.na(UMSARS1_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup()
  ) %>%
  bind_rows(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(UMSARS1) %>% filter(Year==3) %>%
      filter(!is.na(UMSARS1_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup()
  )  


palette <- c( "#D45769", "#0099E0","#0072BB", "#00468B")


plot  %>% ggplot(aes(x = TIME_STUDY*12, y = as.factor(Year), fill=as.factor(Year), colour=as.factor(Year) )) +
  theme_minimal() +
  geom_vline(xintercept = c(0, 12, 24, 36), linetype = "dashed", color = "black", alpha = 0.5, size = 0.5) +
  geom_violin(scale = "width", trim = FALSE,  width = 2.0, alpha=0.85, adjust = 0.4) +
  geom_point(position = position_jitter(height = 0.22), size=0.4 , colour="black", alpha = 0.6) +
  geom_boxplot(width = 0.5,  outlier.shape = NA, fill="transparent", notch = TRUE, colour="white", alpha=0.6) +
  labs(x = "\n Months Since Baseline Visit", y = "Visit Year \n") +
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
  scale_x_continuous(breaks = seq(0, 54, by = 6)) 



plot  %>% ggplot(aes(y = UMSARS1_TOT , x = as.factor(Year), fill=as.factor(Year), colour=as.factor(Year) )) +
  theme_minimal() +
  geom_hline(yintercept = c(17, 22, 26, 28), linetype = "dashed", color = "black", alpha = 0.5, size = 0.5) +
  geom_violin(scale = "width", trim = FALSE,  width = 1.0, alpha=0.80, adjust = 0.4) +
  geom_point( position = position_jitter(height = 0.22, width = 0.22), size=1.5 , colour="black", alpha = 0.4) +
  geom_boxplot(width = 0.5,  outlier.shape = NA, fill="transparent", notch = TRUE, colour="white", alpha=0.6) +
  labs(y = " UMSARS 1 Total Score \n", x = "\n Visit Year") +
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
  scale_y_continuous(breaks = seq(0, 55, by = 5)) 


palette <- c( "#D45769", "#0072BB")

plot  %>% 
  mutate(DIAG=ifelse(DIAG=="CB", "MSA-C", "MSA-P")) %>%
  mutate(Year=ifelse(Year==0, "Year 0",
                     ifelse(Year==1, "Year 1 ",
                            ifelse(Year==2, "Year 2",
                                   ifelse(Year==3, "Year 3+", NA))))) %>%
  ggplot(aes(y = UMSARS1_TOT , x = as.factor(DIAG), fill=as.factor(DIAG), colour=as.factor(DIAG) )) +
  facet_wrap(~as.factor(Year), ncol=4) +
  theme_minimal() +
  geom_hline(yintercept = c(17, 22, 26, 28), linetype = "dashed", color = "black", alpha = 0.5, size = 0.5) +
  geom_violin(scale = "width", trim = FALSE,  width = 1.0, alpha=0.70, adjust = 0.4) +
  geom_point( position = position_jitter(height = 0.22, width = 0.22), size=1.5 , colour="black", alpha = 0.4) +
  geom_boxplot(width = 0.5,  outlier.shape = NA, fill="transparent", notch = TRUE, colour="white", alpha=0.6) +
  labs(y = " UMSARS 1 Total Score \n", x = "\n MSA-C vs MSA-P \n Year-over-year") +
  scale_color_manual(values=palette) +
  scale_fill_manual(values=palette) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        # strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_y_continuous(breaks = seq(0, 55, by = 5))




palette <- c( "#D45769", "#0072BB")

plot  %>% 
  mutate(DIAG=ifelse(DIAGNIV=="PROB", "Probable MSA", "Possible MSA")) %>%
  mutate(Year=ifelse(Year==0, "Year 0",
                     ifelse(Year==1, "Year 1 ",
                            ifelse(Year==2, "Year 2",
                                   ifelse(Year==3, "Year 3+", NA))))) %>%
  ggplot(aes(y = UMSARS1_TOT , x = as.factor(DIAG), fill=as.factor(DIAG), colour=as.factor(DIAG) )) +
  facet_wrap(~as.factor(Year), ncol=4) +
  theme_minimal() +
  geom_hline(yintercept = c(17, 22, 26, 28), linetype = "dashed", color = "black", alpha = 0.5, size = 0.5) +
  geom_violin(scale = "width", trim = FALSE,  width = 1.0, alpha=0.70, adjust = 0.4) +
  geom_point( position = position_jitter(height = 0.22, width = 0.22), size=1.5 , colour="black", alpha = 0.4) +
  geom_boxplot(width = 0.5,  outlier.shape = NA, fill="transparent", notch = TRUE, colour="white", alpha=0.6) +
  labs(y = " UMSARS 1 Total Score \n", x = "\n Probable MSA vs Possible MSA \n Year-over-year") +
  scale_color_manual(values=palette) +
  scale_fill_manual(values=palette) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        # strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_y_continuous(breaks = seq(0, 55, by = 5))








palette <- c( "#D45769", "#0072BB")


UMSARS1 %>% inner_join(EarlyCT_Pop_Baseline_319) %>%
  filter(TIME_STUDY<=5) %>%
  ggplot(aes(TIME_STUDY, UMSARS1_TOT, )) +
  geom_line(aes(group=NUM), col="black" , alpha=0.1) +
  geom_jitter(size=0.1, colour="black", alpha=0.4) +
  stat_smooth(method="gam", colour="#00468B", fill="#00468B" , alpha=0.5, lwd=1.5, se=TRUE, formula = y ~ s(x, bs = "cs", k =10))+
  theme_minimal() +
  xlab("\n Elapsed number of years since study enrollment \n [All available patient records up to year 5]") +
  ylab("UMSARS 1 Total Score \n At each evaluation \n") +
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
  scale_x_continuous(breaks = seq(0, 5, by = 0.5)) 


UMSARS1 %>% inner_join(EarlyCT_Pop_Baseline_319) %>%
  mutate(DIAG=ifelse(DIAG=="CB", "MSA-C", "MSA-P")) %>%
  filter(TIME_STUDY<=5) %>%
  ggplot(aes(TIME_STUDY, UMSARS1_TOT, group=as.factor(DIAG), col=as.factor(DIAG), fill=as.factor(DIAG))) +
  geom_line(aes(group=NUM), col="black" , alpha=0.1) +
  geom_jitter(size=0.1, colour="black", alpha=0.4) +
  stat_smooth(method="gam", , alpha=0.5, lwd=1.5, se=TRUE, formula = y ~ s(x, bs = "cs", k =10))+
  theme_minimal() +
  xlab("\n MSA-C vs MSA-P \n Elapsed number of years since study enrollment \n [All available patient records up to year 5]") +
  ylab("UMSARS 1 Total Score \n At each evaluation \n") +
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
  scale_x_continuous(breaks = seq(0, 5, by = 0.5)) 



UMSARS1 %>% inner_join(EarlyCT_Pop_Baseline_319) %>%
  mutate(DIAG=ifelse(DIAGNIV=="PROB", "Probable MSA", "Possible MSA")) %>%
  filter(TIME_STUDY<=5) %>%
  ggplot(aes(TIME_STUDY, UMSARS1_TOT, group=as.factor(DIAG), col=as.factor(DIAG), fill=as.factor(DIAG))) +
  geom_line(aes(group=NUM), col="black" , alpha=0.1) +
  geom_jitter(size=0.1, colour="black", alpha=0.4) +
  stat_smooth(method="gam", , alpha=0.5, lwd=1.5, se=TRUE, formula = y ~ s(x, bs = "cs", k =10))+
  theme_minimal() +
  xlab("\n Probable vs Possible MSA \n Elapsed number of years since study enrollment \n [All available patient records up to year 5]") +
  ylab("UMSARS 1 Total Score \n At each evaluation \n") +
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
  scale_x_continuous(breaks = seq(0, 5, by = 0.5)) 



# -------------------
# Inputs UMSARS 1 FDA-modified exc. #11 Collpased 0-33 ----------------------------------

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

range(UMSARS1$UMSARS1_TOT_v2)

UMSARS1$missing_na <- rowSums(is.na(UMSARS1[, 5:16]))

UMSARS1[, 5:16] <- UMSARS1[, 5:16] -1


for (i in 5:16) {
  UMSARS1[, i][UMSARS1[, i] < 0] <- 0
}


UMSARS1$UMSARS1_TOT_v2 <- rowSums(UMSARS1[, 5:16], na.rm = TRUE)

range(UMSARS1$UMSARS1_TOT_v2)

UMSARS1 <-  UMSARS1 %>%  select(NUM, TIME_STUDY, Year, UMSARS1_TOT_v2, UMSARS1_11)

UMSARS1 <- UMSARS1 %>% mutate(UMSARS1_TOT_FDA=UMSARS1_TOT_v2-UMSARS1_11)

UMSARS1 %>% filter(is.na(UMSARS1_TOT_FDA))

range(UMSARS1$UMSARS1_TOT_FDA, na.rm=T)


# ----------------
# Overall MSA Entire -------------------------------



plot <- AllMSA_Pop_Baseline_671 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
  filter(!is.na(UMSARS1_TOT_FDA)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  bind_rows(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(UMSARS1) %>% filter(Year==1) %>%
      filter(!is.na(UMSARS1_TOT_FDA)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() 
  ) %>%
  bind_rows(
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(UMSARS1) %>% filter(Year==2) %>%
      filter(!is.na(UMSARS1_TOT_FDA)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup()
  ) %>%
  bind_rows(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(UMSARS1) %>% filter(Year==3) %>%
      filter(!is.na(UMSARS1_TOT_FDA)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup()
  )  

palette <- c( "#D45769", "#0099E0","#0072BB", "#00468B")


plot  %>% ggplot(aes(x = TIME_STUDY*12, y = as.factor(Year), fill=as.factor(Year), colour=as.factor(Year) )) +
  theme_minimal() +
  geom_vline(xintercept = c(0, 12, 24, 36), linetype = "dashed", color = "black", alpha = 0.5, size = 0.5) +
  geom_violin(scale = "width", trim = FALSE,  width = 2.0, alpha=0.85, adjust = 0.4) +
  geom_point(position = position_jitter(height = 0.22), size=0.4 , colour="black", alpha = 0.6) +
  geom_boxplot(width = 0.5,  outlier.shape = NA, fill="transparent", notch = TRUE, colour="white", alpha=0.6) +
  labs(x = "\n Months Since Baseline Visit", y = "Visit Year \n") +
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
  scale_x_continuous(breaks = seq(0, 54, by = 6)) 



plot  %>% ggplot(aes(y = UMSARS1_TOT_FDA , x = as.factor(Year), fill=as.factor(Year), colour=as.factor(Year) )) +
  theme_minimal() +
  geom_hline(yintercept = c(9, 12, 15, 17), linetype = "dashed", color = "black", alpha = 0.5, size = 0.5) +
  geom_violin(scale = "width", trim = FALSE,  width = 1.0, alpha=0.80, adjust = 0.4) +
  geom_point( position = position_jitter(height = 0.22, width = 0.22), size=1.5 , colour="black", alpha = 0.4) +
  geom_boxplot(width = 0.5,  outlier.shape = NA, fill="transparent", notch = TRUE, colour="white", alpha=0.6) +
  labs(y = " UMSARS 1 FDA-modified Collapsed Score \n", x = "\n Visit Year") +
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
  scale_y_continuous(breaks = seq(0, 55, by = 5)) 


palette <- c( "#D45769", "#0072BB")

plot  %>% 
  mutate(DIAG=ifelse(DIAG=="CB", "MSA-C", "MSA-P")) %>%
  mutate(Year=ifelse(Year==0, "Year 0",
                     ifelse(Year==1, "Year 1 ",
                            ifelse(Year==2, "Year 2",
                                   ifelse(Year==3, "Year 3+", NA))))) %>%
  ggplot(aes(y = UMSARS1_TOT_FDA , x = as.factor(DIAG), fill=as.factor(DIAG), colour=as.factor(DIAG) )) +
  facet_wrap(~as.factor(Year), ncol=4) +
  theme_minimal() +
  geom_hline(yintercept = c(9, 12, 15, 17), linetype = "dashed", color = "black", alpha = 0.5, size = 0.5) +
  geom_violin(scale = "width", trim = FALSE,  width = 1.0, alpha=0.70, adjust = 0.4) +
  geom_point( position = position_jitter(height = 0.22, width = 0.22), size=1.5 , colour="black", alpha = 0.4) +
  geom_boxplot(width = 0.5,  outlier.shape = NA, fill="transparent", notch = TRUE, colour="white", alpha=0.6) +
  labs(y = " UMSARS 1 FDA-modified Collapsed Score \n", x = "\n MSA-C vs MSA-P \n Year-over-year") +
  scale_color_manual(values=palette) +
  scale_fill_manual(values=palette) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        # strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_y_continuous(breaks = seq(0, 55, by = 5))




palette <- c( "#D45769", "#0072BB")

plot  %>% 
  mutate(DIAG=ifelse(DIAGNIV=="PROB", "Probable MSA", "Possible MSA")) %>%
  mutate(Year=ifelse(Year==0, "Year 0",
                     ifelse(Year==1, "Year 1 ",
                            ifelse(Year==2, "Year 2",
                                   ifelse(Year==3, "Year 3+", NA))))) %>%
  ggplot(aes(y = UMSARS1_TOT_FDA , x = as.factor(DIAG), fill=as.factor(DIAG), colour=as.factor(DIAG) )) +
  facet_wrap(~as.factor(Year), ncol=4) +
  theme_minimal() +
  geom_hline(yintercept = c(9, 12, 15, 17), linetype = "dashed", color = "black", alpha = 0.5, size = 0.5) +
  geom_violin(scale = "width", trim = FALSE,  width = 1.0, alpha=0.70, adjust = 0.4) +
  geom_point( position = position_jitter(height = 0.22, width = 0.22), size=1.5 , colour="black", alpha = 0.4) +
  geom_boxplot(width = 0.5,  outlier.shape = NA, fill="transparent", notch = TRUE, colour="white", alpha=0.6) +
  labs(y = " UMSARS 1 FDA-modified Collapsed Score \n", x = "\n Probable MSA vs Possible MSA \n Year-over-year") +
  scale_color_manual(values=palette) +
  scale_fill_manual(values=palette) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        # strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_y_continuous(breaks = seq(0, 55, by = 5))






palette <- c( "#D45769", "#0072BB")


UMSARS1 %>% inner_join(AllMSA_Pop_Baseline_671) %>%
  filter(TIME_STUDY<=5) %>%
  ggplot(aes(TIME_STUDY, UMSARS1_TOT_FDA, )) +
  geom_line(aes(group=NUM), col="black" , alpha=0.1) +
  geom_jitter(size=0.1, colour="black", alpha=0.4) +
  stat_smooth(method="gam", colour="#00468B", fill="#00468B" , alpha=0.5, lwd=1.5, se=TRUE, formula = y ~ s(x, bs = "cs", k =10))+
  theme_minimal() +
  xlab("\n Elapsed number of years since study enrollment \n [All available patient records up to year 5]") +
  ylab("UMSARS 1 FAD-modified Collapsed \n At each evaluation \n") +
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
  scale_x_continuous(breaks = seq(0, 5, by = 0.5)) 


UMSARS1 %>% inner_join(AllMSA_Pop_Baseline_671) %>%
  mutate(DIAG=ifelse(DIAG=="CB", "MSA-C", "MSA-P")) %>%
  filter(TIME_STUDY<=5) %>%
  ggplot(aes(TIME_STUDY, UMSARS1_TOT_FDA, group=as.factor(DIAG), col=as.factor(DIAG), fill=as.factor(DIAG))) +
  geom_line(aes(group=NUM), col="black" , alpha=0.1) +
  geom_jitter(size=0.1, colour="black", alpha=0.4) +
  stat_smooth(method="gam", , alpha=0.5, lwd=1.5, se=TRUE, formula = y ~ s(x, bs = "cs", k =10))+
  theme_minimal() +
  xlab("\n MSA-C vs MSA-P \n Elapsed number of years since study enrollment \n [All available patient records up to year 5]") +
  ylab("UMSARS 1 FAD-modified Collapsed \n At each evaluation \n") +
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
  scale_x_continuous(breaks = seq(0, 5, by = 0.5)) 



UMSARS1 %>% inner_join(AllMSA_Pop_Baseline_671) %>%
  mutate(DIAG=ifelse(DIAGNIV=="PROB", "Probable MSA", "Possible MSA")) %>%
  filter(TIME_STUDY<=5) %>%
  ggplot(aes(TIME_STUDY, UMSARS1_TOT_FDA, group=as.factor(DIAG), col=as.factor(DIAG), fill=as.factor(DIAG))) +
  geom_line(aes(group=NUM), col="black" , alpha=0.1) +
  geom_jitter(size=0.1, colour="black", alpha=0.4) +
  stat_smooth(method="gam", , alpha=0.5, lwd=1.5, se=TRUE, formula = y ~ s(x, bs = "cs", k =10))+
  theme_minimal() +
  xlab("\n Probable vs Possible MSA \n Elapsed number of years since study enrollment \n [All available patient records up to year 5]") +
  ylab("UMSARS 1 FAD-modified Collapsed \n At each evaluation \n") +
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
  scale_x_continuous(breaks = seq(0, 5, by = 0.5)) 




# -------------------
# Overall Early CT  -------------------------------

plot <- EarlyCT_Pop_Baseline_319 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
  filter(!is.na(UMSARS1_TOT_FDA)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  bind_rows(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(UMSARS1) %>% filter(Year==1) %>%
      filter(!is.na(UMSARS1_TOT_FDA)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() 
  ) %>%
  bind_rows(
    
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(UMSARS1) %>% filter(Year==2) %>%
      filter(!is.na(UMSARS1_TOT_FDA)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup()
  ) %>%
  bind_rows(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(UMSARS1) %>% filter(Year==3) %>%
      filter(!is.na(UMSARS1_TOT_FDA)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup()
  )  


palette <- c( "#D45769", "#0099E0","#0072BB", "#00468B")


plot  %>% ggplot(aes(x = TIME_STUDY*12, y = as.factor(Year), fill=as.factor(Year), colour=as.factor(Year) )) +
  theme_minimal() +
  geom_vline(xintercept = c(0, 12, 24, 36), linetype = "dashed", color = "black", alpha = 0.5, size = 0.5) +
  geom_violin(scale = "width", trim = FALSE,  width = 2.0, alpha=0.85, adjust = 0.4) +
  geom_point(position = position_jitter(height = 0.22), size=0.4 , colour="black", alpha = 0.6) +
  geom_boxplot(width = 0.5,  outlier.shape = NA, fill="transparent", notch = TRUE, colour="white", alpha=0.6) +
  labs(x = "\n Months Since Baseline Visit", y = "Visit Year \n") +
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
  scale_x_continuous(breaks = seq(0, 54, by = 6)) 



plot  %>% ggplot(aes(y = UMSARS1_TOT_FDA , x = as.factor(Year), fill=as.factor(Year), colour=as.factor(Year) )) +
  theme_minimal() +
  geom_hline(yintercept = c(4, 9, 12, 15), linetype = "dashed", color = "black", alpha = 0.5, size = 0.5) +
  geom_violin(scale = "width", trim = FALSE,  width = 1.0, alpha=0.80, adjust = 0.4) +
  geom_point( position = position_jitter(height = 0.22, width = 0.22), size=1.5 , colour="black", alpha = 0.4) +
  geom_boxplot(width = 0.5,  outlier.shape = NA, fill="transparent", notch = TRUE, colour="white", alpha=0.6) +
  labs(y = " UMSARS 1 FDA-modified Collapsed Score \n", x = "\n Visit Year") +
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
  scale_y_continuous(breaks = seq(0, 55, by = 5)) 


palette <- c( "#D45769", "#0072BB")

plot  %>% 
  mutate(DIAG=ifelse(DIAG=="CB", "MSA-C", "MSA-P")) %>%
  mutate(Year=ifelse(Year==0, "Year 0",
                     ifelse(Year==1, "Year 1 ",
                            ifelse(Year==2, "Year 2",
                                   ifelse(Year==3, "Year 3+", NA))))) %>%
  ggplot(aes(y = UMSARS1_TOT_FDA , x = as.factor(DIAG), fill=as.factor(DIAG), colour=as.factor(DIAG) )) +
  facet_wrap(~as.factor(Year), ncol=4) +
  theme_minimal() +
  geom_hline(yintercept = c(4, 9, 12, 15), linetype = "dashed", color = "black", alpha = 0.5, size = 0.5) +
  geom_violin(scale = "width", trim = FALSE,  width = 1.0, alpha=0.70, adjust = 0.4) +
  geom_point( position = position_jitter(height = 0.22, width = 0.22), size=1.5 , colour="black", alpha = 0.4) +
  geom_boxplot(width = 0.5,  outlier.shape = NA, fill="transparent", notch = TRUE, colour="white", alpha=0.6) +
  labs(y = " UMSARS 1 FDA-modified Collapsed Score \n", x = "\n MSA-C vs MSA-P \n Year-over-year") +
  scale_color_manual(values=palette) +
  scale_fill_manual(values=palette) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        # strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_y_continuous(breaks = seq(0, 55, by = 5))




palette <- c( "#D45769", "#0072BB")

plot  %>% 
  mutate(DIAG=ifelse(DIAGNIV=="PROB", "Probable MSA", "Possible MSA")) %>%
  mutate(Year=ifelse(Year==0, "Year 0",
                     ifelse(Year==1, "Year 1 ",
                            ifelse(Year==2, "Year 2",
                                   ifelse(Year==3, "Year 3+", NA))))) %>%
  ggplot(aes(y = UMSARS1_TOT_FDA , x = as.factor(DIAG), fill=as.factor(DIAG), colour=as.factor(DIAG) )) +
  facet_wrap(~as.factor(Year), ncol=4) +
  theme_minimal() +
  geom_hline(yintercept = c(4, 9, 12, 15), linetype = "dashed", color = "black", alpha = 0.5, size = 0.5) +
  geom_violin(scale = "width", trim = FALSE,  width = 1.0, alpha=0.70, adjust = 0.4) +
  geom_point( position = position_jitter(height = 0.22, width = 0.22), size=1.5 , colour="black", alpha = 0.4) +
  geom_boxplot(width = 0.5,  outlier.shape = NA, fill="transparent", notch = TRUE, colour="white", alpha=0.6) +
  labs(y = " UMSARS 1 FDA-modified Collapsed Score \n", x = "\n Probable MSA vs Possible MSA \n Year-over-year") +
  scale_color_manual(values=palette) +
  scale_fill_manual(values=palette) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        # strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_y_continuous(breaks = seq(0, 55, by = 5))











palette <- c( "#D45769", "#0072BB")


UMSARS1 %>% inner_join(EarlyCT_Pop_Baseline_319) %>%
  filter(TIME_STUDY<=5) %>%
  ggplot(aes(TIME_STUDY, UMSARS1_TOT_FDA, )) +
  geom_line(aes(group=NUM), col="black" , alpha=0.1) +
  geom_jitter(size=0.1, colour="black", alpha=0.4) +
  stat_smooth(method="gam", colour="#00468B", fill="#00468B" , alpha=0.5, lwd=1.5, se=TRUE, formula = y ~ s(x, bs = "cs", k =10))+
  theme_minimal() +
  xlab("\n Elapsed number of years since study enrollment \n [All available patient records up to year 5]") +
  ylab("UMSARS 1 FAD-modified Collapsed \n At each evaluation \n") +
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
  scale_x_continuous(breaks = seq(0, 5, by = 0.5)) 


UMSARS1 %>% inner_join(EarlyCT_Pop_Baseline_319) %>%
  mutate(DIAG=ifelse(DIAG=="CB", "MSA-C", "MSA-P")) %>%
  filter(TIME_STUDY<=5) %>%
  ggplot(aes(TIME_STUDY, UMSARS1_TOT_FDA, group=as.factor(DIAG), col=as.factor(DIAG), fill=as.factor(DIAG))) +
  geom_line(aes(group=NUM), col="black" , alpha=0.1) +
  geom_jitter(size=0.1, colour="black", alpha=0.4) +
  stat_smooth(method="gam", , alpha=0.5, lwd=1.5, se=TRUE, formula = y ~ s(x, bs = "cs", k =10))+
  theme_minimal() +
  xlab("\n MSA-C vs MSA-P \n Elapsed number of years since study enrollment \n [All available patient records up to year 5]") +
  ylab("UMSARS 1 FAD-modified Collapsed \n At each evaluation \n") +
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
  scale_x_continuous(breaks = seq(0, 5, by = 0.5)) 



UMSARS1 %>% inner_join(EarlyCT_Pop_Baseline_319) %>%
  mutate(DIAG=ifelse(DIAGNIV=="PROB", "Probable MSA", "Possible MSA")) %>%
  filter(TIME_STUDY<=5) %>%
  ggplot(aes(TIME_STUDY, UMSARS1_TOT_FDA, group=as.factor(DIAG), col=as.factor(DIAG), fill=as.factor(DIAG))) +
  geom_line(aes(group=NUM), col="black" , alpha=0.1) +
  geom_jitter(size=0.1, colour="black", alpha=0.4) +
  stat_smooth(method="gam", , alpha=0.5, lwd=1.5, se=TRUE, formula = y ~ s(x, bs = "cs", k =10))+
  theme_minimal() +
  xlab("\n Probable vs Possible MSA \n Elapsed number of years since study enrollment \n [All available patient records up to year 5]") +
  ylab("UMSARS 1 FAD-modified Collapsed \n At each evaluation \n") +
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
  scale_x_continuous(breaks = seq(0, 5, by = 0.5)) 





# -------------------
# Inputs UMSARS 2 Total ----------------------------------

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


UMSARS2 <- dataCohorteManaged %>% 
  select(NUM, DATECONSULT, TIME_STUDY, Year, UMSARS2_1:UMSARS2_TOT)


UMSARS2$UMSARS2_TOT_v2 <- rowSums(UMSARS2[, 5:19], na.rm = TRUE)

UMSARS2$missing_na <- rowSums(is.na(UMSARS2[, 5:19]))

UMSARS2 <-  UMSARS2 %>%  select(NUM, TIME_STUDY, Year, UMSARS2_TOT)

# ----------------
# Overall MSA Entire -------------------------------



plot <- AllMSA_Pop_Baseline_671 %>% inner_join(UMSARS2) %>% filter(Year==0) %>%
  filter(!is.na(UMSARS2_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  bind_rows(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(UMSARS2) %>% filter(Year==1) %>%
      filter(!is.na(UMSARS2_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() 
  ) %>%
  bind_rows(
    
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(UMSARS2) %>% filter(Year==2) %>%
      filter(!is.na(UMSARS2_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup()
  ) %>%
  bind_rows(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(UMSARS2) %>% filter(Year==3) %>%
      filter(!is.na(UMSARS2_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup()
  )  

palette <- c( "#D45769", "#0099E0","#0072BB", "#00468B")


plot  %>% ggplot(aes(x = TIME_STUDY*12, y = as.factor(Year), fill=as.factor(Year), colour=as.factor(Year) )) +
  theme_minimal() +
  geom_vline(xintercept = c(0, 12, 24, 36), linetype = "dashed", color = "black", alpha = 0.5, size = 0.5) +
  geom_violin(scale = "width", trim = FALSE,  width = 2.0, alpha=0.85, adjust = 0.4) +
  geom_point(position = position_jitter(height = 0.22), size=0.4 , colour="black", alpha = 0.6) +
  geom_boxplot(width = 0.5,  outlier.shape = NA, fill="transparent", notch = TRUE, colour="white", alpha=0.6) +
  labs(x = "\n Months Since Baseline Visit", y = "Visit Year \n") +
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
  scale_x_continuous(breaks = seq(0, 54, by = 6)) 



plot  %>% ggplot(aes(y = UMSARS2_TOT , x = as.factor(Year), fill=as.factor(Year), colour=as.factor(Year) )) +
  theme_minimal() +
  geom_hline(yintercept = c(24, 28, 31, 33), linetype = "dashed", color = "black", alpha = 0.5, size = 0.5) +
  geom_violin(scale = "width", trim = FALSE,  width = 1.0, alpha=0.80, adjust = 0.4) +
  geom_point( position = position_jitter(height = 0.22, width = 0.22), size=1.5 , colour="black", alpha = 0.4) +
  geom_boxplot(width = 0.5,  outlier.shape = NA, fill="transparent", notch = TRUE, colour="white", alpha=0.6) +
  labs(y = " UMSARS 2 Total Score \n", x = "\n Visit Year") +
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
  scale_y_continuous(breaks = seq(0, 55, by = 5)) 


palette <- c( "#D45769", "#0072BB")

plot  %>% 
  mutate(DIAG=ifelse(DIAG=="CB", "MSA-C", "MSA-P")) %>%
  mutate(Year=ifelse(Year==0, "Year 0",
                     ifelse(Year==1, "Year 1 ",
                            ifelse(Year==2, "Year 2",
                                   ifelse(Year==3, "Year 3+", NA))))) %>%
  ggplot(aes(y = UMSARS2_TOT , x = as.factor(DIAG), fill=as.factor(DIAG), colour=as.factor(DIAG) )) +
  facet_wrap(~as.factor(Year), ncol=4) +
  theme_minimal() +
  geom_hline(yintercept = c(24, 28, 31, 33), linetype = "dashed", color = "black", alpha = 0.5, size = 0.5) +
  geom_violin(scale = "width", trim = FALSE,  width = 1.0, alpha=0.70, adjust = 0.4) +
  geom_point( position = position_jitter(height = 0.22, width = 0.22), size=1.5 , colour="black", alpha = 0.4) +
  geom_boxplot(width = 0.5,  outlier.shape = NA, fill="transparent", notch = TRUE, colour="white", alpha=0.6) +
  labs(y = " UMSARS 2 Total Score \n", x = "\n MSA-C vs MSA-P \n Year-over-year") +
  scale_color_manual(values=palette) +
  scale_fill_manual(values=palette) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        # strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_y_continuous(breaks = seq(0, 55, by = 5))




palette <- c( "#D45769", "#0072BB")

plot  %>% 
  mutate(DIAG=ifelse(DIAGNIV=="PROB", "Probable MSA", "Possible MSA")) %>%
  mutate(Year=ifelse(Year==0, "Year 0",
                     ifelse(Year==1, "Year 1 ",
                            ifelse(Year==2, "Year 2",
                                   ifelse(Year==3, "Year 3+", NA))))) %>%
  ggplot(aes(y = UMSARS2_TOT , x = as.factor(DIAG), fill=as.factor(DIAG), colour=as.factor(DIAG) )) +
  facet_wrap(~as.factor(Year), ncol=4) +
  theme_minimal() +
  geom_hline(yintercept = c(24, 28, 31, 33), linetype = "dashed", color = "black", alpha = 0.5, size = 0.5) +
  geom_violin(scale = "width", trim = FALSE,  width = 1.0, alpha=0.70, adjust = 0.4) +
  geom_point( position = position_jitter(height = 0.22, width = 0.22), size=1.5 , colour="black", alpha = 0.4) +
  geom_boxplot(width = 0.5,  outlier.shape = NA, fill="transparent", notch = TRUE, colour="white", alpha=0.6) +
  labs(y = " UMSARS 2 Total Score \n", x = "\n Probable MSA vs Possible MSA \n Year-over-year") +
  scale_color_manual(values=palette) +
  scale_fill_manual(values=palette) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        # strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_y_continuous(breaks = seq(0, 55, by = 5))













palette <- c( "#D45769", "#0072BB")


UMSARS2 %>% inner_join(AllMSA_Pop_Baseline_671) %>%
  filter(TIME_STUDY<=5) %>%
  ggplot(aes(TIME_STUDY, UMSARS2_TOT, )) +
  geom_line(aes(group=NUM), col="black" , alpha=0.1) +
  geom_jitter(size=0.1, colour="black", alpha=0.4) +
  stat_smooth(method="gam", colour="#00468B", fill="#00468B" , alpha=0.5, lwd=1.5, se=TRUE, formula = y ~ s(x, bs = "cs", k =10))+
  theme_minimal() +
  xlab("\n Elapsed number of years since study enrollment \n [All available patient records up to year 5]") +
  ylab("UMSARS 2 Total Score \n At each evaluation \n") +
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
  scale_x_continuous(breaks = seq(0, 5, by = 0.5)) 


UMSARS2 %>% inner_join(AllMSA_Pop_Baseline_671) %>%
  mutate(DIAG=ifelse(DIAG=="CB", "MSA-C", "MSA-P")) %>%
  filter(TIME_STUDY<=5) %>%
  ggplot(aes(TIME_STUDY, UMSARS2_TOT, group=as.factor(DIAG), col=as.factor(DIAG), fill=as.factor(DIAG))) +
  geom_line(aes(group=NUM), col="black" , alpha=0.1) +
  geom_jitter(size=0.1, colour="black", alpha=0.4) +
  stat_smooth(method="gam", , alpha=0.5, lwd=1.5, se=TRUE, formula = y ~ s(x, bs = "cs", k =10))+
  theme_minimal() +
  xlab("\n MSA-C vs MSA-P \n Elapsed number of years since study enrollment \n [All available patient records up to year 5]") +
  ylab("UMSARS 2 Total Score \n At each evaluation \n") +
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
  scale_x_continuous(breaks = seq(0, 5, by = 0.5)) 



UMSARS2 %>% inner_join(AllMSA_Pop_Baseline_671) %>%
  mutate(DIAG=ifelse(DIAGNIV=="PROB", "Probable MSA", "Possible MSA")) %>%
  filter(TIME_STUDY<=5) %>%
  ggplot(aes(TIME_STUDY, UMSARS2_TOT, group=as.factor(DIAG), col=as.factor(DIAG), fill=as.factor(DIAG))) +
  geom_line(aes(group=NUM), col="black" , alpha=0.1) +
  geom_jitter(size=0.1, colour="black", alpha=0.4) +
  stat_smooth(method="gam", , alpha=0.5, lwd=1.5, se=TRUE, formula = y ~ s(x, bs = "cs", k =10))+
  theme_minimal() +
  xlab("\n Probable vs Possible MSA \n Elapsed number of years since study enrollment \n [All available patient records up to year 5]") +
  ylab("UMSARS 2 Total Score \n At each evaluation \n") +
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
  scale_x_continuous(breaks = seq(0, 5, by = 0.5)) 


# -------------------
# Overall Early CT  -------------------------------

plot <- EarlyCT_Pop_Baseline_319 %>% inner_join(UMSARS2) %>% filter(Year==0) %>%
  filter(!is.na(UMSARS2_TOT)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  bind_rows(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(UMSARS2) %>% filter(Year==1) %>%
      filter(!is.na(UMSARS2_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() 
  ) %>%
  bind_rows(
    
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(UMSARS2) %>% filter(Year==2) %>%
      filter(!is.na(UMSARS2_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup()
  ) %>%
  bind_rows(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(UMSARS2) %>% filter(Year==3) %>%
      filter(!is.na(UMSARS2_TOT)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup()
  )  


palette <- c( "#D45769", "#0099E0","#0072BB", "#00468B")


plot  %>% ggplot(aes(x = TIME_STUDY*12, y = as.factor(Year), fill=as.factor(Year), colour=as.factor(Year) )) +
  theme_minimal() +
  geom_vline(xintercept = c(0, 12, 24, 36), linetype = "dashed", color = "black", alpha = 0.5, size = 0.5) +
  geom_violin(scale = "width", trim = FALSE,  width = 2.0, alpha=0.85, adjust = 0.4) +
  geom_point(position = position_jitter(height = 0.22), size=0.4 , colour="black", alpha = 0.6) +
  geom_boxplot(width = 0.5,  outlier.shape = NA, fill="transparent", notch = TRUE, colour="white", alpha=0.6) +
  labs(x = "\n Months Since Baseline Visit", y = "Visit Year \n") +
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
  scale_x_continuous(breaks = seq(0, 54, by = 6)) 



plot  %>% ggplot(aes(y = UMSARS2_TOT , x = as.factor(Year), fill=as.factor(Year), colour=as.factor(Year) )) +
  theme_minimal() +
  geom_hline(yintercept = c(18, 24, 27, 30), linetype = "dashed", color = "black", alpha = 0.5, size = 0.5) +
  geom_violin(scale = "width", trim = FALSE,  width = 1.0, alpha=0.80, adjust = 0.4) +
  geom_point( position = position_jitter(height = 0.22, width = 0.22), size=1.5 , colour="black", alpha = 0.4) +
  geom_boxplot(width = 0.5,  outlier.shape = NA, fill="transparent", notch = TRUE, colour="white", alpha=0.6) +
  labs(y = " UMSARS 2 Total Score \n", x = "\n Visit Year") +
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
  scale_y_continuous(breaks = seq(0, 55, by = 5)) 


palette <- c( "#D45769", "#0072BB")

plot  %>% 
  mutate(DIAG=ifelse(DIAG=="CB", "MSA-C", "MSA-P")) %>%
  mutate(Year=ifelse(Year==0, "Year 0",
                     ifelse(Year==1, "Year 1 ",
                            ifelse(Year==2, "Year 2",
                                   ifelse(Year==3, "Year 3+", NA))))) %>%
  ggplot(aes(y = UMSARS2_TOT , x = as.factor(DIAG), fill=as.factor(DIAG), colour=as.factor(DIAG) )) +
  facet_wrap(~as.factor(Year), ncol=4) +
  theme_minimal() +
  geom_hline(yintercept = c(18, 24, 27, 30), linetype = "dashed", color = "black", alpha = 0.5, size = 0.5) +
  geom_violin(scale = "width", trim = FALSE,  width = 1.0, alpha=0.70, adjust = 0.4) +
  geom_point( position = position_jitter(height = 0.22, width = 0.22), size=1.5 , colour="black", alpha = 0.4) +
  geom_boxplot(width = 0.5,  outlier.shape = NA, fill="transparent", notch = TRUE, colour="white", alpha=0.6) +
  labs(y = " UMSARS 2 Total Score \n", x = "\n MSA-C vs MSA-P \n Year-over-year") +
  scale_color_manual(values=palette) +
  scale_fill_manual(values=palette) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        # strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_y_continuous(breaks = seq(0, 55, by = 5))




palette <- c( "#D45769", "#0072BB")

plot  %>% 
  mutate(DIAG=ifelse(DIAGNIV=="PROB", "Probable MSA", "Possible MSA")) %>%
  mutate(Year=ifelse(Year==0, "Year 0",
                     ifelse(Year==1, "Year 1 ",
                            ifelse(Year==2, "Year 2",
                                   ifelse(Year==3, "Year 3+", NA))))) %>%
  ggplot(aes(y = UMSARS2_TOT , x = as.factor(DIAG), fill=as.factor(DIAG), colour=as.factor(DIAG) )) +
  facet_wrap(~as.factor(Year), ncol=4) +
  theme_minimal() +
  geom_hline(yintercept = c(18, 24, 27, 30), linetype = "dashed", color = "black", alpha = 0.5, size = 0.5) +
  geom_violin(scale = "width", trim = FALSE,  width = 1.0, alpha=0.70, adjust = 0.4) +
  geom_point( position = position_jitter(height = 0.22, width = 0.22), size=1.5 , colour="black", alpha = 0.4) +
  geom_boxplot(width = 0.5,  outlier.shape = NA, fill="transparent", notch = TRUE, colour="white", alpha=0.6) +
  labs(y = " UMSARS 2 Total Score \n", x = "\n Probable MSA vs Possible MSA \n Year-over-year") +
  scale_color_manual(values=palette) +
  scale_fill_manual(values=palette) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        # strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_y_continuous(breaks = seq(0, 55, by = 5))








palette <- c( "#D45769", "#0072BB")


UMSARS2 %>% inner_join(EarlyCT_Pop_Baseline_319) %>%
  filter(TIME_STUDY<=5) %>%
  ggplot(aes(TIME_STUDY, UMSARS2_TOT, )) +
  geom_line(aes(group=NUM), col="black" , alpha=0.1) +
  geom_jitter(size=0.1, colour="black", alpha=0.4) +
  stat_smooth(method="gam", colour="#00468B", fill="#00468B" , alpha=0.5, lwd=1.5, se=TRUE, formula = y ~ s(x, bs = "cs", k =10))+
  theme_minimal() +
  xlab("\n Elapsed number of years since study enrollment \n [All available patient records up to year 5]") +
  ylab("UMSARS 2 Total Score \n At each evaluation \n") +
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
  scale_x_continuous(breaks = seq(0, 5, by = 0.5)) 


UMSARS2 %>% inner_join(EarlyCT_Pop_Baseline_319) %>%
  mutate(DIAG=ifelse(DIAG=="CB", "MSA-C", "MSA-P")) %>%
  filter(TIME_STUDY<=5) %>%
  ggplot(aes(TIME_STUDY, UMSARS2_TOT, group=as.factor(DIAG), col=as.factor(DIAG), fill=as.factor(DIAG))) +
  geom_line(aes(group=NUM), col="black" , alpha=0.1) +
  geom_jitter(size=0.1, colour="black", alpha=0.4) +
  stat_smooth(method="gam", , alpha=0.5, lwd=1.5, se=TRUE, formula = y ~ s(x, bs = "cs", k =10))+
  theme_minimal() +
  xlab("\n MSA-C vs MSA-P \n Elapsed number of years since study enrollment \n [All available patient records up to year 5]") +
  ylab("UMSARS 2 Total Score \n At each evaluation \n") +
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
  scale_x_continuous(breaks = seq(0, 5, by = 0.5)) 



UMSARS2 %>% inner_join(EarlyCT_Pop_Baseline_319) %>%
  mutate(DIAG=ifelse(DIAGNIV=="PROB", "Probable MSA", "Possible MSA")) %>%
  filter(TIME_STUDY<=5) %>%
  ggplot(aes(TIME_STUDY, UMSARS2_TOT, group=as.factor(DIAG), col=as.factor(DIAG), fill=as.factor(DIAG))) +
  geom_line(aes(group=NUM), col="black" , alpha=0.1) +
  geom_jitter(size=0.1, colour="black", alpha=0.4) +
  stat_smooth(method="gam", , alpha=0.5, lwd=1.5, se=TRUE, formula = y ~ s(x, bs = "cs", k =10))+
  theme_minimal() +
  xlab("\n Probable vs Possible MSA \n Elapsed number of years since study enrollment \n [All available patient records up to year 5]") +
  ylab("UMSARS 2 Total Score \n At each evaluation \n") +
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
  scale_x_continuous(breaks = seq(0, 5, by = 0.5)) 



# ---------
# Inputs UMSARS 1+2 9-item Total ----------------------------------

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
  mutate(UMSARS_9item = UMSARS1_1 + UMSARS1_4 + UMSARS1_5 + UMSARS1_6 + UMSARS1_7 + UMSARS1_10 + UMSARS2_11 + UMSARS2_12 + UMSARS2_14) %>%
  mutate(UMSARS_11item = UMSARS1_2 + UMSARS1_3 + UMSARS1_6 + UMSARS1_7 + UMSARS1_11 + UMSARS2_1 + UMSARS2_2 + UMSARS2_9 + UMSARS2_11 + UMSARS2_12 + UMSARS2_14 ) %>% 
  select(NUM, DATECONSULT, TIME_STUDY, Year, UMSARS_9item)


# ----------------
# Overall MSA Entire -------------------------------



plot <- AllMSA_Pop_Baseline_671 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
  filter(!is.na(UMSARS_9item)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  bind_rows(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(UMSARS1) %>% filter(Year==1) %>%
      filter(!is.na(UMSARS_9item)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() 
  ) %>%
  bind_rows(
    
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(UMSARS1) %>% filter(Year==2) %>%
      filter(!is.na(UMSARS_9item)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup()
  ) %>%
  bind_rows(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(UMSARS1) %>% filter(Year==3) %>%
      filter(!is.na(UMSARS_9item)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup()
  )  

palette <- c( "#D45769", "#0099E0","#0072BB", "#00468B")


plot  %>% ggplot(aes(x = TIME_STUDY*12, y = as.factor(Year), fill=as.factor(Year), colour=as.factor(Year) )) +
  theme_minimal() +
  geom_vline(xintercept = c(0, 12, 24, 36), linetype = "dashed", color = "black", alpha = 0.5, size = 0.5) +
  geom_violin(scale = "width", trim = FALSE,  width = 2.0, alpha=0.85, adjust = 0.4) +
  geom_point(position = position_jitter(height = 0.22), size=0.4 , colour="black", alpha = 0.6) +
  geom_boxplot(width = 0.5,  outlier.shape = NA, fill="transparent", notch = TRUE, colour="white", alpha=0.6) +
  labs(x = "\n Months Since Baseline Visit", y = "Visit Year \n") +
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
  scale_x_continuous(breaks = seq(0, 54, by = 6)) 



plot  %>% ggplot(aes(y = UMSARS_9item , x = as.factor(Year), fill=as.factor(Year), colour=as.factor(Year) )) +
  theme_minimal() +
  geom_hline(yintercept = c(16, 20, 23, 25), linetype = "dashed", color = "black", alpha = 0.5, size = 0.5) +
  geom_violin(scale = "width", trim = FALSE,  width = 1.0, alpha=0.80, adjust = 0.4) +
  geom_point( position = position_jitter(height = 0.22, width = 0.22), size=1.5 , colour="black", alpha = 0.4) +
  geom_boxplot(width = 0.5,  outlier.shape = NA, fill="transparent", notch = TRUE, colour="white", alpha=0.6) +
  labs(y = " UMSARS 1+2 \n 9-item Score \n", x = "\n Visit Year") +
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
  scale_y_continuous(breaks = seq(0, 55, by = 5)) 


palette <- c( "#D45769", "#0072BB")

plot  %>% 
  mutate(DIAG=ifelse(DIAG=="CB", "MSA-C", "MSA-P")) %>%
  mutate(Year=ifelse(Year==0, "Year 0",
                     ifelse(Year==1, "Year 1 ",
                            ifelse(Year==2, "Year 2",
                                   ifelse(Year==3, "Year 3+", NA))))) %>%
  ggplot(aes(y = UMSARS_9item , x = as.factor(DIAG), fill=as.factor(DIAG), colour=as.factor(DIAG) )) +
  facet_wrap(~as.factor(Year), ncol=4) +
  theme_minimal() +
  geom_hline(yintercept = c(24, 28, 31, 33), linetype = "dashed", color = "black", alpha = 0.5, size = 0.5) +
  geom_violin(scale = "width", trim = FALSE,  width = 1.0, alpha=0.70, adjust = 0.4) +
  geom_point( position = position_jitter(height = 0.22, width = 0.22), size=1.5 , colour="black", alpha = 0.4) +
  geom_boxplot(width = 0.5,  outlier.shape = NA, fill="transparent", notch = TRUE, colour="white", alpha=0.6) +
  labs(y = " UMSARS 1+2 \n 9-item Score \n", x = "\n MSA-C vs MSA-P \n Year-over-year") +
  scale_color_manual(values=palette) +
  scale_fill_manual(values=palette) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        # strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_y_continuous(breaks = seq(0, 55, by = 5))




palette <- c( "#D45769", "#0072BB")

plot  %>% 
  mutate(DIAG=ifelse(DIAGNIV=="PROB", "Probable MSA", "Possible MSA")) %>%
  mutate(Year=ifelse(Year==0, "Year 0",
                     ifelse(Year==1, "Year 1 ",
                            ifelse(Year==2, "Year 2",
                                   ifelse(Year==3, "Year 3+", NA))))) %>%
  ggplot(aes(y = UMSARS_9item , x = as.factor(DIAG), fill=as.factor(DIAG), colour=as.factor(DIAG) )) +
  facet_wrap(~as.factor(Year), ncol=4) +
  theme_minimal() +
  geom_hline(yintercept = c(24, 28, 31, 33), linetype = "dashed", color = "black", alpha = 0.5, size = 0.5) +
  geom_violin(scale = "width", trim = FALSE,  width = 1.0, alpha=0.70, adjust = 0.4) +
  geom_point( position = position_jitter(height = 0.22, width = 0.22), size=1.5 , colour="black", alpha = 0.4) +
  geom_boxplot(width = 0.5,  outlier.shape = NA, fill="transparent", notch = TRUE, colour="white", alpha=0.6) +
  labs(y = " UMSARS 1+2 \n 9-item Score \n", x = "\n Probable MSA vs Possible MSA \n Year-over-year") +
  scale_color_manual(values=palette) +
  scale_fill_manual(values=palette) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        # strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_y_continuous(breaks = seq(0, 55, by = 5))








palette <- c( "#D45769", "#0072BB")


UMSARS1 %>% inner_join(AllMSA_Pop_Baseline_671) %>%
  filter(TIME_STUDY<=5) %>%
  ggplot(aes(TIME_STUDY, UMSARS_9item, )) +
  geom_line(aes(group=NUM), col="black" , alpha=0.1) +
  geom_jitter(size=0.1, colour="black", alpha=0.4) +
  stat_smooth(method="gam", colour="#00468B", fill="#00468B" , alpha=0.5, lwd=1.5, se=TRUE, formula = y ~ s(x, bs = "cs", k =10))+
  theme_minimal() +
  xlab("\n Elapsed number of years since study enrollment \n [All available patient records up to year 5]") +
  ylab("9-item UMSARS 1+2 \n At each evaluation \n") +
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
  scale_x_continuous(breaks = seq(0, 5, by = 0.5)) 


UMSARS1 %>% inner_join(AllMSA_Pop_Baseline_671) %>%
  mutate(DIAG=ifelse(DIAG=="CB", "MSA-C", "MSA-P")) %>%
  filter(TIME_STUDY<=5) %>%
  ggplot(aes(TIME_STUDY, UMSARS_9item, group=as.factor(DIAG), col=as.factor(DIAG), fill=as.factor(DIAG))) +
  geom_line(aes(group=NUM), col="black" , alpha=0.1) +
  geom_jitter(size=0.1, colour="black", alpha=0.4) +
  stat_smooth(method="gam", , alpha=0.5, lwd=1.5, se=TRUE, formula = y ~ s(x, bs = "cs", k =10))+
  theme_minimal() +
  xlab("\n MSA-C vs MSA-P \n Elapsed number of years since study enrollment \n [All available patient records up to year 5]") +
  ylab("9-item UMSARS 1+2 \n At each evaluation \n") +
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
  scale_x_continuous(breaks = seq(0, 5, by = 0.5)) 



UMSARS1 %>% inner_join(AllMSA_Pop_Baseline_671) %>%
  mutate(DIAG=ifelse(DIAGNIV=="PROB", "Probable MSA", "Possible MSA")) %>%
  filter(TIME_STUDY<=5) %>%
  ggplot(aes(TIME_STUDY, UMSARS_9item, group=as.factor(DIAG), col=as.factor(DIAG), fill=as.factor(DIAG))) +
  geom_line(aes(group=NUM), col="black" , alpha=0.1) +
  geom_jitter(size=0.1, colour="black", alpha=0.4) +
  stat_smooth(method="gam", , alpha=0.5, lwd=1.5, se=TRUE, formula = y ~ s(x, bs = "cs", k =10))+
  theme_minimal() +
  xlab("\n Probable vs Possible MSA \n Elapsed number of years since study enrollment \n [All available patient records up to year 5]") +
  ylab("9-item UMSARS 1+2 \n At each evaluation \n") +
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
  scale_x_continuous(breaks = seq(0, 5, by = 0.5)) 


# -------------------
# Overall Early CT  -------------------------------

plot <- EarlyCT_Pop_Baseline_319 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
  filter(!is.na(UMSARS_9item)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  bind_rows(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(UMSARS1) %>% filter(Year==1) %>%
      filter(!is.na(UMSARS_9item)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() 
  ) %>%
  bind_rows(
    
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(UMSARS1) %>% filter(Year==2) %>%
      filter(!is.na(UMSARS_9item)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup()
  ) %>%
  bind_rows(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(UMSARS1) %>% filter(Year==3) %>%
      filter(!is.na(UMSARS_9item)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup()
  )  


palette <- c( "#D45769", "#0099E0","#0072BB", "#00468B")


plot  %>% ggplot(aes(x = TIME_STUDY*12, y = as.factor(Year), fill=as.factor(Year), colour=as.factor(Year) )) +
  theme_minimal() +
  geom_vline(xintercept = c(0, 12, 24, 36), linetype = "dashed", color = "black", alpha = 0.5, size = 0.5) +
  geom_violin(scale = "width", trim = FALSE,  width = 2.0, alpha=0.85, adjust = 0.4) +
  geom_point(position = position_jitter(height = 0.22), size=0.4 , colour="black", alpha = 0.6) +
  geom_boxplot(width = 0.5,  outlier.shape = NA, fill="transparent", notch = TRUE, colour="white", alpha=0.6) +
  labs(x = "\n Months Since Baseline Visit", y = "Visit Year \n") +
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
  scale_x_continuous(breaks = seq(0, 54, by = 6)) 



plot  %>% ggplot(aes(y = UMSARS_9item , x = as.factor(Year), fill=as.factor(Year), colour=as.factor(Year) )) +
  theme_minimal() +
  geom_hline(yintercept = c(11, 16, 20, 22), linetype = "dashed", color = "black", alpha = 0.5, size = 0.5) +
  geom_violin(scale = "width", trim = FALSE,  width = 1.0, alpha=0.80, adjust = 0.4) +
  geom_point( position = position_jitter(height = 0.22, width = 0.22), size=1.5 , colour="black", alpha = 0.4) +
  geom_boxplot(width = 0.5,  outlier.shape = NA, fill="transparent", notch = TRUE, colour="white", alpha=0.6) +
  labs(y = " UMSARS 1+2 \n 9-item Score \n", x = "\n Visit Year") +
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
  scale_y_continuous(breaks = seq(0, 55, by = 5)) 


palette <- c( "#D45769", "#0072BB")

plot  %>% 
  mutate(DIAG=ifelse(DIAG=="CB", "MSA-C", "MSA-P")) %>%
  mutate(Year=ifelse(Year==0, "Year 0",
                     ifelse(Year==1, "Year 1 ",
                            ifelse(Year==2, "Year 2",
                                   ifelse(Year==3, "Year 3+", NA))))) %>%
  ggplot(aes(y = UMSARS_9item , x = as.factor(DIAG), fill=as.factor(DIAG), colour=as.factor(DIAG) )) +
  facet_wrap(~as.factor(Year), ncol=4) +
  theme_minimal() +
  geom_hline(yintercept = c(18, 24, 27, 30), linetype = "dashed", color = "black", alpha = 0.5, size = 0.5) +
  geom_violin(scale = "width", trim = FALSE,  width = 1.0, alpha=0.70, adjust = 0.4) +
  geom_point( position = position_jitter(height = 0.22, width = 0.22), size=1.5 , colour="black", alpha = 0.4) +
  geom_boxplot(width = 0.5,  outlier.shape = NA, fill="transparent", notch = TRUE, colour="white", alpha=0.6) +
  labs(y = " UMSARS 1+2 \n 9-item Score \n", x = "\n MSA-C vs MSA-P \n Year-over-year") +
  scale_color_manual(values=palette) +
  scale_fill_manual(values=palette) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        # strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_y_continuous(breaks = seq(0, 55, by = 5))




palette <- c( "#D45769", "#0072BB")

plot  %>% 
  mutate(DIAG=ifelse(DIAGNIV=="PROB", "Probable MSA", "Possible MSA")) %>%
  mutate(Year=ifelse(Year==0, "Year 0",
                     ifelse(Year==1, "Year 1 ",
                            ifelse(Year==2, "Year 2",
                                   ifelse(Year==3, "Year 3+", NA))))) %>%
  ggplot(aes(y = UMSARS_9item , x = as.factor(DIAG), fill=as.factor(DIAG), colour=as.factor(DIAG) )) +
  facet_wrap(~as.factor(Year), ncol=4) +
  theme_minimal() +
  geom_hline(yintercept = c(18, 24, 27, 30), linetype = "dashed", color = "black", alpha = 0.5, size = 0.5) +
  geom_violin(scale = "width", trim = FALSE,  width = 1.0, alpha=0.70, adjust = 0.4) +
  geom_point( position = position_jitter(height = 0.22, width = 0.22), size=1.5 , colour="black", alpha = 0.4) +
  geom_boxplot(width = 0.5,  outlier.shape = NA, fill="transparent", notch = TRUE, colour="white", alpha=0.6) +
  labs(y = " UMSARS 1+2 \n 9-item Score \n", x = "\n Probable MSA vs Possible MSA \n Year-over-year") +
  scale_color_manual(values=palette) +
  scale_fill_manual(values=palette) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        # strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_y_continuous(breaks = seq(0, 55, by = 5))







palette <- c( "#D45769", "#0072BB")


UMSARS1 %>% inner_join(EarlyCT_Pop_Baseline_319) %>%
  filter(TIME_STUDY<=5) %>%
  ggplot(aes(TIME_STUDY, UMSARS_9item, )) +
  geom_line(aes(group=NUM), col="black" , alpha=0.1) +
  geom_jitter(size=0.1, colour="black", alpha=0.4) +
  stat_smooth(method="gam", colour="#00468B", fill="#00468B" , alpha=0.5, lwd=1.5, se=TRUE, formula = y ~ s(x, bs = "cs", k =10))+
  theme_minimal() +
  xlab("\n Elapsed number of years since study enrollment \n [All available patient records up to year 5]") +
  ylab("9-item UMSARS 1+2 \n At each evaluation \n") +
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
  scale_x_continuous(breaks = seq(0, 5, by = 0.5)) 


UMSARS1 %>% inner_join(EarlyCT_Pop_Baseline_319) %>%
  mutate(DIAG=ifelse(DIAG=="CB", "MSA-C", "MSA-P")) %>%
  filter(TIME_STUDY<=5) %>%
  ggplot(aes(TIME_STUDY, UMSARS_9item, group=as.factor(DIAG), col=as.factor(DIAG), fill=as.factor(DIAG))) +
  geom_line(aes(group=NUM), col="black" , alpha=0.1) +
  geom_jitter(size=0.1, colour="black", alpha=0.4) +
  stat_smooth(method="gam", , alpha=0.5, lwd=1.5, se=TRUE, formula = y ~ s(x, bs = "cs", k =10))+
  theme_minimal() +
  xlab("\n MSA-C vs MSA-P \n Elapsed number of years since study enrollment \n [All available patient records up to year 5]") +
  ylab("9-item UMSARS 1+2 \n At each evaluation \n") +
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
  scale_x_continuous(breaks = seq(0, 5, by = 0.5)) 



UMSARS1 %>% inner_join(EarlyCT_Pop_Baseline_319) %>%
  mutate(DIAG=ifelse(DIAGNIV=="PROB", "Probable MSA", "Possible MSA")) %>%
  filter(TIME_STUDY<=5) %>%
  ggplot(aes(TIME_STUDY, UMSARS_9item, group=as.factor(DIAG), col=as.factor(DIAG), fill=as.factor(DIAG))) +
  geom_line(aes(group=NUM), col="black" , alpha=0.1) +
  geom_jitter(size=0.1, colour="black", alpha=0.4) +
  stat_smooth(method="gam", , alpha=0.5, lwd=1.5, se=TRUE, formula = y ~ s(x, bs = "cs", k =10))+
  theme_minimal() +
  xlab("\n Probable vs Possible MSA \n Elapsed number of years since study enrollment \n [All available patient records up to year 5]") +
  ylab("9-item UMSARS 1+2 \n At each evaluation \n") +
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
  scale_x_continuous(breaks = seq(0, 5, by = 0.5)) 




# ---------
# Inputs UMSARS 1+2 11-item Total ----------------------------------

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
  mutate(UMSARS_9item = UMSARS1_1 + UMSARS1_4 + UMSARS1_5 + UMSARS1_6 + UMSARS1_7 + UMSARS1_10 + UMSARS2_11 + UMSARS2_12 + UMSARS2_14) %>%
  mutate(UMSARS_11item = UMSARS1_2 + UMSARS1_3 + UMSARS1_6 + UMSARS1_7 + UMSARS1_11 + UMSARS2_1 + UMSARS2_2 + UMSARS2_9 + UMSARS2_11 + UMSARS2_12 + UMSARS2_14 ) %>% 
  select(NUM, DATECONSULT, TIME_STUDY, Year, UMSARS_11item)


# ----------------
# Overall MSA Entire -------------------------------



plot <- AllMSA_Pop_Baseline_671 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
  filter(!is.na(UMSARS_11item)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  bind_rows(
    AllMSA_Pop_BaselineYear1_410 %>% inner_join(UMSARS1) %>% filter(Year==1) %>%
      filter(!is.na(UMSARS_11item)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() 
  ) %>%
  bind_rows(
    
    AllMSA_Pop_BaselineYear1Year2_245 %>% inner_join(UMSARS1) %>% filter(Year==2) %>%
      filter(!is.na(UMSARS_11item)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup()
  ) %>%
  bind_rows(
    AllMSA_Pop_BaselineYear1Year2Year3Plus_158 %>% inner_join(UMSARS1) %>% filter(Year==3) %>%
      filter(!is.na(UMSARS_11item)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup()
  )  

palette <- c( "#D45769", "#0099E0","#0072BB", "#00468B")


plot  %>% ggplot(aes(x = TIME_STUDY*12, y = as.factor(Year), fill=as.factor(Year), colour=as.factor(Year) )) +
  theme_minimal() +
  geom_vline(xintercept = c(0, 12, 24, 36), linetype = "dashed", color = "black", alpha = 0.5, size = 0.5) +
  geom_violin(scale = "width", trim = FALSE,  width = 2.0, alpha=0.85, adjust = 0.4) +
  geom_point(position = position_jitter(height = 0.22), size=0.4 , colour="black", alpha = 0.6) +
  geom_boxplot(width = 0.5,  outlier.shape = NA, fill="transparent", notch = TRUE, colour="white", alpha=0.6) +
  labs(x = "\n Months Since Baseline Visit", y = "Visit Year \n") +
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
  scale_x_continuous(breaks = seq(0, 54, by = 6)) 



plot  %>% ggplot(aes(y = UMSARS_11item , x = as.factor(Year), fill=as.factor(Year), colour=as.factor(Year) )) +
  theme_minimal() +
  geom_hline(yintercept = c(21, 25, 28, 30), linetype = "dashed", color = "black", alpha = 0.5, size = 0.5) +
  geom_violin(scale = "width", trim = FALSE,  width = 1.0, alpha=0.80, adjust = 0.4) +
  geom_point( position = position_jitter(height = 0.22, width = 0.22), size=1.5 , colour="black", alpha = 0.4) +
  geom_boxplot(width = 0.5,  outlier.shape = NA, fill="transparent", notch = TRUE, colour="white", alpha=0.6) +
  labs(y = " UMSARS 1+2 \n 11-item Score \n", x = "\n Visit Year") +
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
  scale_y_continuous(breaks = seq(0, 55, by = 5)) 


palette <- c( "#D45769", "#0072BB")

plot  %>% 
  mutate(DIAG=ifelse(DIAG=="CB", "MSA-C", "MSA-P")) %>%
  mutate(Year=ifelse(Year==0, "Year 0",
                     ifelse(Year==1, "Year 1 ",
                            ifelse(Year==2, "Year 2",
                                   ifelse(Year==3, "Year 3+", NA))))) %>%
  ggplot(aes(y = UMSARS_11item , x = as.factor(DIAG), fill=as.factor(DIAG), colour=as.factor(DIAG) )) +
  facet_wrap(~as.factor(Year), ncol=4) +
  theme_minimal() +
  geom_hline(yintercept = c(21, 25, 28, 30), linetype = "dashed", color = "black", alpha = 0.5, size = 0.5) +
  geom_violin(scale = "width", trim = FALSE,  width = 1.0, alpha=0.70, adjust = 0.4) +
  geom_point( position = position_jitter(height = 0.22, width = 0.22), size=1.5 , colour="black", alpha = 0.4) +
  geom_boxplot(width = 0.5,  outlier.shape = NA, fill="transparent", notch = TRUE, colour="white", alpha=0.6) +
  labs(y = " UMSARS 1+2 \n 11-item Score \n", x = "\n MSA-C vs MSA-P \n Year-over-year") +
  scale_color_manual(values=palette) +
  scale_fill_manual(values=palette) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        # strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_y_continuous(breaks = seq(0, 55, by = 5))




palette <- c( "#D45769", "#0072BB")

plot  %>% 
  mutate(DIAG=ifelse(DIAGNIV=="PROB", "Probable MSA", "Possible MSA")) %>%
  mutate(Year=ifelse(Year==0, "Year 0",
                     ifelse(Year==1, "Year 1 ",
                            ifelse(Year==2, "Year 2",
                                   ifelse(Year==3, "Year 3+", NA))))) %>%
  ggplot(aes(y = UMSARS_11item , x = as.factor(DIAG), fill=as.factor(DIAG), colour=as.factor(DIAG) )) +
  facet_wrap(~as.factor(Year), ncol=4) +
  theme_minimal() +
  geom_hline(yintercept = c(21, 25, 28, 30), linetype = "dashed", color = "black", alpha = 0.5, size = 0.5) +
  geom_violin(scale = "width", trim = FALSE,  width = 1.0, alpha=0.70, adjust = 0.4) +
  geom_point( position = position_jitter(height = 0.22, width = 0.22), size=1.5 , colour="black", alpha = 0.4) +
  geom_boxplot(width = 0.5,  outlier.shape = NA, fill="transparent", notch = TRUE, colour="white", alpha=0.6) +
  labs(y = " UMSARS 1+2 \n 11-item Score \n", x = "\n Probable MSA vs Possible MSA \n Year-over-year") +
  scale_color_manual(values=palette) +
  scale_fill_manual(values=palette) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        # strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_y_continuous(breaks = seq(0, 55, by = 5))




palette <- c( "#D45769", "#0072BB")


UMSARS1 %>% inner_join(AllMSA_Pop_Baseline_671) %>%
  filter(TIME_STUDY<=5) %>%
  ggplot(aes(TIME_STUDY, UMSARS_11item, )) +
  geom_line(aes(group=NUM), col="black" , alpha=0.1) +
  geom_jitter(size=0.1, colour="black", alpha=0.4) +
  stat_smooth(method="gam", colour="#00468B", fill="#00468B" , alpha=0.5, lwd=1.5, se=TRUE, formula = y ~ s(x, bs = "cs", k =10))+
  theme_minimal() +
  xlab("\n Elapsed number of years since study enrollment \n [All available patient records up to year 5]") +
  ylab("9-item UMSARS 1+2 \n At each evaluation \n") +
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
  scale_x_continuous(breaks = seq(0, 5, by = 0.5)) 


UMSARS1 %>% inner_join(AllMSA_Pop_Baseline_671) %>%
  mutate(DIAG=ifelse(DIAG=="CB", "MSA-C", "MSA-P")) %>%
  filter(TIME_STUDY<=5) %>%
  ggplot(aes(TIME_STUDY, UMSARS_11item, group=as.factor(DIAG), col=as.factor(DIAG), fill=as.factor(DIAG))) +
  geom_line(aes(group=NUM), col="black" , alpha=0.1) +
  geom_jitter(size=0.1, colour="black", alpha=0.4) +
  stat_smooth(method="gam", , alpha=0.5, lwd=1.5, se=TRUE, formula = y ~ s(x, bs = "cs", k =10))+
  theme_minimal() +
  xlab("\n MSA-C vs MSA-P \n Elapsed number of years since study enrollment \n [All available patient records up to year 5]") +
  ylab("9-item UMSARS 1+2 \n At each evaluation \n") +
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
  scale_x_continuous(breaks = seq(0, 5, by = 0.5)) 



UMSARS1 %>% inner_join(AllMSA_Pop_Baseline_671) %>%
  mutate(DIAG=ifelse(DIAGNIV=="PROB", "Probable MSA", "Possible MSA")) %>%
  filter(TIME_STUDY<=5) %>%
  ggplot(aes(TIME_STUDY, UMSARS_11item, group=as.factor(DIAG), col=as.factor(DIAG), fill=as.factor(DIAG))) +
  geom_line(aes(group=NUM), col="black" , alpha=0.1) +
  geom_jitter(size=0.1, colour="black", alpha=0.4) +
  stat_smooth(method="gam", , alpha=0.5, lwd=1.5, se=TRUE, formula = y ~ s(x, bs = "cs", k =10))+
  theme_minimal() +
  xlab("\n Probable vs Possible MSA \n Elapsed number of years since study enrollment \n [All available patient records up to year 5]") +
  ylab("9-item UMSARS 1+2 \n At each evaluation \n") +
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
  scale_x_continuous(breaks = seq(0, 5, by = 0.5)) 



# -------------------
# Overall Early CT  -------------------------------

plot <- EarlyCT_Pop_Baseline_319 %>% inner_join(UMSARS1) %>% filter(Year==0) %>%
  filter(!is.na(UMSARS_11item)) %>%
  mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
  group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
  group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
  group_by(NUM, Year) %>% slice(1) %>% ungroup() %>%
  bind_rows(
    EarlyCT_Pop_BaselineYear1_208 %>% inner_join(UMSARS1) %>% filter(Year==1) %>%
      filter(!is.na(UMSARS_11item)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup() 
  ) %>%
  bind_rows(
    
    EarlyCT_Pop_BaselineYear1Year2_134 %>% inner_join(UMSARS1) %>% filter(Year==2) %>%
      filter(!is.na(UMSARS_11item)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup()
  ) %>%
  bind_rows(
    EarlyCT_Pop_BaselineYear1Year2Year3Plus_99 %>% inner_join(UMSARS1) %>% filter(Year==3) %>%
      filter(!is.na(UMSARS_11item)) %>%
      mutate(Elapsed=abs(TIME_STUDY-Year)) %>% 
      group_by(NUM, Year) %>% filter(Elapsed==min(Elapsed)) %>%
      group_by(NUM, Year) %>% filter(TIME_STUDY==min(TIME_STUDY)) %>% drop_na() %>%
      group_by(NUM, Year) %>% slice(1) %>% ungroup()
  )  


palette <- c( "#D45769", "#0099E0","#0072BB", "#00468B")


plot  %>% ggplot(aes(x = TIME_STUDY*12, y = as.factor(Year), fill=as.factor(Year), colour=as.factor(Year) )) +
  theme_minimal() +
  geom_vline(xintercept = c(0, 12, 24, 36), linetype = "dashed", color = "black", alpha = 0.5, size = 0.5) +
  geom_violin(scale = "width", trim = FALSE,  width = 2.0, alpha=0.85, adjust = 0.4) +
  geom_point(position = position_jitter(height = 0.22), size=0.4 , colour="black", alpha = 0.6) +
  geom_boxplot(width = 0.5,  outlier.shape = NA, fill="transparent", notch = TRUE, colour="white", alpha=0.6) +
  labs(x = "\n Months Since Baseline Visit", y = "Visit Year \n") +
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
  scale_x_continuous(breaks = seq(0, 54, by = 6)) 



plot  %>% ggplot(aes(y = UMSARS_11item , x = as.factor(Year), fill=as.factor(Year), colour=as.factor(Year) )) +
  theme_minimal() +
  geom_hline(yintercept = c(16, 21, 25, 28), linetype = "dashed", color = "black", alpha = 0.5, size = 0.5) +
  geom_violin(scale = "width", trim = FALSE,  width = 1.0, alpha=0.80, adjust = 0.4) +
  geom_point( position = position_jitter(height = 0.22, width = 0.22), size=1.5 , colour="black", alpha = 0.4) +
  geom_boxplot(width = 0.5,  outlier.shape = NA, fill="transparent", notch = TRUE, colour="white", alpha=0.6) +
  labs(y = " UMSARS 1+2 \n 11-item Score \n", x = "\n Visit Year") +
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
  scale_y_continuous(breaks = seq(0, 55, by = 5)) 


palette <- c( "#D45769", "#0072BB")

plot  %>% 
  mutate(DIAG=ifelse(DIAG=="CB", "MSA-C", "MSA-P")) %>%
  mutate(Year=ifelse(Year==0, "Year 0",
                     ifelse(Year==1, "Year 1 ",
                            ifelse(Year==2, "Year 2",
                                   ifelse(Year==3, "Year 3+", NA))))) %>%
  ggplot(aes(y = UMSARS_11item , x = as.factor(DIAG), fill=as.factor(DIAG), colour=as.factor(DIAG) )) +
  facet_wrap(~as.factor(Year), ncol=4) +
  theme_minimal() +
  geom_hline(yintercept = c(16, 21, 25, 28), linetype = "dashed", color = "black", alpha = 0.5, size = 0.5) +
  geom_violin(scale = "width", trim = FALSE,  width = 1.0, alpha=0.70, adjust = 0.4) +
  geom_point( position = position_jitter(height = 0.22, width = 0.22), size=1.5 , colour="black", alpha = 0.4) +
  geom_boxplot(width = 0.5,  outlier.shape = NA, fill="transparent", notch = TRUE, colour="white", alpha=0.6) +
  labs(y = " UMSARS 1+2 \n 11-item Score \n", x = "\n MSA-C vs MSA-P \n Year-over-year") +
  scale_color_manual(values=palette) +
  scale_fill_manual(values=palette) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        # strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_y_continuous(breaks = seq(0, 55, by = 5))




palette <- c( "#D45769", "#0072BB")

plot  %>% 
  mutate(DIAG=ifelse(DIAGNIV=="PROB", "Probable MSA", "Possible MSA")) %>%
  mutate(Year=ifelse(Year==0, "Year 0",
                     ifelse(Year==1, "Year 1 ",
                            ifelse(Year==2, "Year 2",
                                   ifelse(Year==3, "Year 3+", NA))))) %>%
  ggplot(aes(y = UMSARS_11item , x = as.factor(DIAG), fill=as.factor(DIAG), colour=as.factor(DIAG) )) +
  facet_wrap(~as.factor(Year), ncol=4) +
  theme_minimal() +
  geom_hline(yintercept = c(16, 21, 25, 28), linetype = "dashed", color = "black", alpha = 0.5, size = 0.5) +
  geom_violin(scale = "width", trim = FALSE,  width = 1.0, alpha=0.70, adjust = 0.4) +
  geom_point( position = position_jitter(height = 0.22, width = 0.22), size=1.5 , colour="black", alpha = 0.4) +
  geom_boxplot(width = 0.5,  outlier.shape = NA, fill="transparent", notch = TRUE, colour="white", alpha=0.6) +
  labs(y = " UMSARS 1+2 \n 11-item Score \n", x = "\n Probable MSA vs Possible MSA \n Year-over-year") +
  scale_color_manual(values=palette) +
  scale_fill_manual(values=palette) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        # strip.text = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12, vjust = -0.5),
        axis.title.y = element_text(size = 12, vjust = -0.5),
        plot.margin = margin(5, 5, 5, 5, "pt")) +
  scale_y_continuous(breaks = seq(0, 55, by = 5))





palette <- c( "#D45769", "#0072BB")


UMSARS1 %>% inner_join(EarlyCT_Pop_Baseline_319) %>%
  filter(TIME_STUDY<=5) %>%
  ggplot(aes(TIME_STUDY, UMSARS_11item, )) +
  geom_line(aes(group=NUM), col="black" , alpha=0.1) +
  geom_jitter(size=0.1, colour="black", alpha=0.4) +
  stat_smooth(method="gam", colour="#00468B", fill="#00468B" , alpha=0.5, lwd=1.5, se=TRUE, formula = y ~ s(x, bs = "cs", k =10))+
  theme_minimal() +
  xlab("\n Elapsed number of years since study enrollment \n [All available patient records up to year 5]") +
  ylab("9-item UMSARS 1+2 \n At each evaluation \n") +
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
  scale_x_continuous(breaks = seq(0, 5, by = 0.5)) 


UMSARS1 %>% inner_join(EarlyCT_Pop_Baseline_319) %>%
  mutate(DIAG=ifelse(DIAG=="CB", "MSA-C", "MSA-P")) %>%
  filter(TIME_STUDY<=5) %>%
  ggplot(aes(TIME_STUDY, UMSARS_11item, group=as.factor(DIAG), col=as.factor(DIAG), fill=as.factor(DIAG))) +
  geom_line(aes(group=NUM), col="black" , alpha=0.1) +
  geom_jitter(size=0.1, colour="black", alpha=0.4) +
  stat_smooth(method="gam", , alpha=0.5, lwd=1.5, se=TRUE, formula = y ~ s(x, bs = "cs", k =10))+
  theme_minimal() +
  xlab("\n MSA-C vs MSA-P \n Elapsed number of years since study enrollment \n [All available patient records up to year 5]") +
  ylab("9-item UMSARS 1+2 \n At each evaluation \n") +
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
  scale_x_continuous(breaks = seq(0, 5, by = 0.5)) 



UMSARS1 %>% inner_join(EarlyCT_Pop_Baseline_319) %>%
  mutate(DIAG=ifelse(DIAGNIV=="PROB", "Probable MSA", "Possible MSA")) %>%
  filter(TIME_STUDY<=5) %>%
  ggplot(aes(TIME_STUDY, UMSARS_11item, group=as.factor(DIAG), col=as.factor(DIAG), fill=as.factor(DIAG))) +
  geom_line(aes(group=NUM), col="black" , alpha=0.1) +
  geom_jitter(size=0.1, colour="black", alpha=0.4) +
  stat_smooth(method="gam", , alpha=0.5, lwd=1.5, se=TRUE, formula = y ~ s(x, bs = "cs", k =10))+
  theme_minimal() +
  xlab("\n Probable vs Possible MSA \n Elapsed number of years since study enrollment \n [All available patient records up to year 5]") +
  ylab("9-item UMSARS 1+2 \n At each evaluation \n") +
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
  scale_x_continuous(breaks = seq(0, 5, by = 0.5)) 


# ---------