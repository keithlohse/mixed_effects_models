library(tidyverse); library(RCurl); library(ez); library(lme4); library(car); library(lmerTest)

DATA <- read.csv("https://raw.githubusercontent.com/keithlohse/mixed_effects_models/master/data_example.csv",
                 stringsAsFactors = TRUE)

data_TIME <- aggregate(speed ~ subID + time + age_group + group, data=DATA, FUN=mean)
data_TIME <- data_TIME %>% arrange(subID, age_group, group, time)

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

DAT2 <- read.csv("https://raw.githubusercontent.com/keithlohse/LMER_Clinical_Science/master/ACRM_2018/data/data_session1.csv",
                 stringsAsFactors = TRUE)

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

# Centering time on the first point and converting to years:
DAT2$year.0 <- (DAT2$time-1)/12
DAT2$year.0_sq<-DAT2$year.0^2
DAT2$year.0_cu<-DAT2$year.0^3

# Centering time on the mean time:
DAT2$year.c <- scale(DAT2$time)
DAT2$year.c_sq<-DAT2$year.c^2
DAT2$year.c_cu<-DAT2$year.c^3


raneff_00<-lmer(rasch_FIM~
                  # Fixed-effects
                  1+
                  # Random-effects
                  (1|subID), data=DAT2, REML=FALSE,
                control=lmerControl(optimizer="bobyqa",
                                    optCtrl=list(maxfun=5e5)))
summary(raneff_00)

# Random slope random intercepts model ---- 
raneff_01<-lmer(rasch_FIM~
                  # Fixed-effects
                  1+year.0+
                  # Random-effects
                  (1+year.0|subID), data=DAT2, REML=FALSE,
                control=lmerControl(optimizer="bobyqa",
                                    optCtrl=list(maxfun=5e5)))
summary(raneff_01)

# Random quadratic slopes and intercepts model ---- 
raneff_02<-lmer(rasch_FIM~
                  # Fixed-effects
                  1+year.0+year.0_sq+
                  # Random-effects
                  (1+year.0+year.0_sq|subID), data=DAT2, REML=FALSE,
                control=lmerControl(optimizer="bobyqa",
                                    optCtrl=list(maxfun=5e5)))
summary(raneff_02)

# Random cubic slopes and intercepts model ---- 
raneff_03<-lmer(rasch_FIM~
                  # Fixed-effects
                  1+year.0+year.0_sq+year.0_cu+
                  # Random-effects
                  (1+year.0+year.0_sq+year.0_cu|subID), data=DAT2, REML=FALSE,
                control=lmerControl(optimizer="bobyqa",
                                    optCtrl=list(maxfun=2e5)))
summary(raneff_03)

anova(raneff_00,raneff_01,raneff_02, raneff_03)


# Effect of AIS Grade on Time
cond_01<-lmer(rasch_FIM~
                # Fixed-effects
                1+year.0*AIS_grade+year.0_sq*AIS_grade+year.0_cu*AIS_grade+
                # Random-effects
                (1+year.0+year.0_sq+year.0_cu|subID), data=DAT2, REML=FALSE,
              control=lmerControl(optimizer="bobyqa",
                                  optCtrl=list(maxfun=5e5)))
anova(raneff_03, cond_01)

anova(cond_01)
#Note that I actually prefer to use the Anova() function from the car package, where you can easily specify the type of SS calculation.
Anova(cond_01, type="III")

summary(cond_01)



