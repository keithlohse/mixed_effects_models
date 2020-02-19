library(tidyverse); library(RCurl); library(ez); library(lme4); library(lmerTest)

DATA <- read.csv("https://raw.githubusercontent.com/keithlohse/mixed_effects_models/master/data_example.csv")

data_TIME <- aggregate(speed ~ subID + time + age_group + group, data=DATA, FUN=mean)
data_TIME <- data_TIME %>% arrange(subID, age_group, group, time)
data_TIME$time.sq <- data_TIME$time^2
data_TIME$time.c <- data_TIME$time-mean(data_TIME$time, na.rm=TRUE)
data_TIME$time.c.sq <- data_TIME$time.c^2
data_TIME$old.c<-(as.numeric(data_TIME$age_group)-1.5)*(-1)

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
  

## 1.1. Unconditional Models of Time
### 1.1.1. Fixed-Slope Random-Intercepts Model
# Fixed slope random intercepts model ---- 
raneff_00<-lmer(speed~ 
                  # Fixed-effects 
                  1+time.c+ 
                  # Random-effects 
                  (1|subID), data=data_TIME, REML=FALSE,
                control = lmerControl(calc.derivs = FALSE))

summary(raneff_00)



### 1.1.2. Random-Slopes Random-Intercepts Model
# Random slope random intercepts model ---- 
raneff_01<-lmer(speed~ 
                  # Fixed-effects 
                  1+time.c+ 
                  # Random-effects 
                  (1+time.c|subID), data=data_TIME, REML=FALSE,
                control = lmerControl(calc.derivs = FALSE))

summary(raneff_01)



### 1.1.3. Quadratic Random-Slopes Random-Intercepts Model
# Quadratic slope random intercepts model ---- 
raneff_02<-lmer(speed~ 
                  # Fixed-effects 
                  1+time.c+time.c.sq+ 
                  # Random-effects 
                  (1+time.c+time.c.sq|subID), data=data_TIME, REML=FALSE,
                control = lmerControl(calc.derivs = FALSE))

summary(raneff_02)

# Fixed Quadratic slope random intercepts model ---- 
raneff_02<-lmer(speed~ 
                  # Fixed-effects 
                  1+time.c+time.c.sq+ 
                  # Random-effects 
                  (1+time.c|subID), data=data_TIME, REML=FALSE,
                control = lmerControl(calc.derivs = FALSE))

summary(raneff_02)


## 1.5. Creating a Conditional Model of Change over Time
# Conditional Model of Time ---- 
oa_mod_01<-lmer(speed~ 
                  # Fixed-effects 
                  1+time.c*old.c+time.c.sq*old.c+
                  # Random-effects 
                  (1+time.c|subID), data=data_TIME, REML=FALSE,
                control = lmerControl(calc.derivs = FALSE))

anova(raneff_00, raneff_01, raneff_02, oa_mod_01)



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


DAT2$year.0 <- (DAT2$time-1)/12
DAT2$year.0_sq<-DAT2$year.0^2
DAT2$year.0_cu<-DAT2$year.0^3


# Linear Effect of Time
time_linear<-lmer(rasch_FIM~
                    # Fixed-effects
                    1+year.0+
                    # Random-effects
                    (1+year.0|subID), data=DAT2, REML=FALSE,
                  control = lmerControl(calc.derivs = FALSE))
summary(time_linear)


# Quadratic Effect of Time
time_square<-lmer(rasch_FIM~
                    # Fixed-effects
                    1+year.0+year.0_sq+
                    # Random-effects
                    (1+year.0+year.0_sq|subID), data=DAT2, REML=FALSE,
                  control = lmerControl(calc.derivs = FALSE))
summary(time_square)


# Cubic Effect of Time
time_cube<-lmer(rasch_FIM~
                  # Fixed-effects
                  1+year.0+year.0_sq+year.0_cu+
                  # Random-effects
                  (1+year.0+year.0_sq+year.0_cu|subID), data=DAT2, REML=FALSE,
                control = lmerControl(calc.derivs = FALSE))
summary(time_cube)

anova(time_linear, time_square, time_cube)

# Effect of AIS Grade on Time
cond_01<-lmer(rasch_FIM~
                # Fixed-effects
                1+year.0*AIS_grade+year.0_sq+year.0_cu+
                # Random-effects
                (1+year.0+year.0_sq+year.0_cu|subID), data=DAT2, REML=FALSE,
              control = lmerControl(calc.derivs = FALSE))
anova(cond_01)


# Effect of AIS Grade on Quadratic Time
cond_02<-lmer(rasch_FIM~
                # Fixed-effects
                1+year.0*AIS_grade+year.0_sq*AIS_grade+year.0_cu+
                # Random-effects
                (1+year.0+year.0_sq+year.0_cu|subID), data=DAT2, REML=FALSE,
              control = lmerControl(calc.derivs = FALSE))
anova(cond_02)

# Effect of AIS Grade on Quadratic Time
cond_03<-lmer(rasch_FIM~
                # Fixed-effects
                1+year.0*AIS_grade+year.0_sq*AIS_grade+year.0_cu*AIS_grade+
                # Random-effects
                (1+year.0+year.0_sq+year.0_cu|subID), data=DAT2, REML=FALSE,
              control = lmerControl(calc.derivs = FALSE))
anova(cond_03)

# Comparing between Models
anova(cond_01, cond_02, cond_03)



