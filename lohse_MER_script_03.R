
library(tidyverse); library(RCurl); library(ez); library(lme4); library(lmerTest)

DATA <- read.csv("https://raw.githubusercontent.com/keithlohse/mixed_effects_models/master/data_example.csv")

data_TIME <- aggregate(speed ~ subID + time + age_group + group, data=DATA, FUN=mean)
data_TIME <- data_TIME %>% arrange(subID, age_group, group, time)
head(data_TIME)
data_TIME$time.sq <- data_TIME$time^2
data_TIME$time.c <- data_TIME$time-mean(data_TIME$time)
data_TIME$time.c.sq <- data_TIME$time.c^2


ggplot(data_TIME, aes(x = time, y = speed)) +
  geom_line(aes(group=subID), col="black", alpha=0.8)+
  geom_point(aes(fill=subID), pch=21, size=2)+
  facet_wrap(~age_group) +
  scale_x_continuous(name = "Trial") +
  scale_y_continuous(name = "Speed (m/s)") +
  theme(axis.text=element_text(size=16, color="black"), 
        axis.title=element_text(size=16, face="bold"),
        plot.title=element_text(size=16, face="bold", hjust=0.5),
        strip.text = element_text(size=16, face="bold"),
        legend.position = "none")
  

DAT2 <- read.csv("https://raw.githubusercontent.com/keithlohse/LMER_Clinical_Science/master/ACRM_2018/data/data_session1.csv")


ggplot(DAT2, aes(x = time, y = rasch_FIM)) +
  geom_line(aes(group=subID), col="black", alpha=0.8)+
  geom_point(aes(fill=subID), pch=21, size=2)+
  facet_wrap(~AIS_grade) +
  scale_x_continuous(name = "Time (Months)") +
  scale_y_continuous(name = "Rasch-Scaled FIM") +
  theme(axis.text=element_text(size=16, color="black"), 
        axis.title=element_text(size=16, face="bold"),
        plot.title=element_text(size=16, face="bold", hjust=0.5),
        strip.text = element_text(size=16, face="bold"),
        legend.position = "none")
  

# Fixed slope random intercepts model ---- 
raneff_00<-lmer(speed~ # Fixed-effects 
                  1+time+ 
                  # Random-effects 
                  (1|subID), data=data_TIME, REML=FALSE)

summary(raneff_00)

# Random slope random intercepts model ---- 
raneff_01<-lmer(speed~ # Fixed-effects 
                  1+time.c+ 
                  # Random-effects 
                  (1+time.c|subID), data=data_TIME, REML=FALSE)

summary(raneff_01)

# Quadratic Random slopes model ---- 
raneff_02<-lmer(speed~ # Fixed-effects 
                  1+time.c+time.c.sq+ 
                  # Random-effects 
                  (1+time.c+time.c.sq|subID), data=data_TIME, REML=FALSE)

summary(raneff_02)
                                                                                                                                                                                                               
                                                                                                                                                                                                                             
                                                                                                                                                                                                                             # Modeling Changes in Functional Independence over Months
                                                                                                                                                                                                                             