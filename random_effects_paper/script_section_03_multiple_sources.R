# title: "Model Specification in Mixed-Effects Models: A Focus on Random Effects"
# authors: Keith Lohse, PhD, PStat; Mike Strube, PhD; Allan Kozlowksi, PhD, PT
# date: "2022-07-26"

library(lme4); library(lmerTest); library(ez); library(tidyverse)


# 1.0. Variation in Multiple Nested Factors ----
DAT1 <- read.csv("https://raw.githubusercontent.com/keithlohse/mixed_effects_models/master/random_effects_paper/data_longitudinal.csv",
                 stringsAsFactors = TRUE, na.strings=c("NA","NaN"," ",""))
# Use the head() function to check the structure of the data file. 
head(DAT1)

DAT1$year.0 <- (DAT1$time -1)/12

ggplot(DAT1, aes(x = year.0, y = rasch_FIM)) + 
  geom_line(aes(group=subID, col=site)) + 
  geom_point(aes(fill=site), shape=21, size=1.5) + 
  facet_wrap(~AIS_grade) +
  scale_fill_grey()+
  scale_color_grey()+
  labs(col="Site", fill="Site")+
  scale_x_continuous(name = "Time from Admission (Years)", breaks=c(0,0.5,1,1.5), limits=c(0,1.6)) + 
  scale_y_continuous(name = "Independence (0-100)",limits=c(0,100)) + 
  theme_bw() + theme(axis.text.x=element_text(size=10, colour="black"),
                     axis.text.y=element_text(size=10, colour="black"), 
                     axis.title=element_text(size=12,face="bold")) +
  theme(strip.text.x = element_text(size = 12))+ theme(legend.position="bottom") 



raneff_quad_site<-lmer(rasch_FIM~ 
                           # Fixed-effects 
                           1+year.0+I(year.0^2)+ 
                           # Random-effects 
                           (1+year.0|subID)+(1|site), data=DAT1, REML=FALSE) 
summary(raneff_quad_site)









# 2.0. Variation in Multiple Crossed Factors ----
DAT2 <- read.csv("https://raw.githubusercontent.com/keithlohse/mixed_effects_models/master/random_effects_paper/rt_dummy_data.csv",
                 stringsAsFactors = TRUE, na.strings=c("NA","NaN"," ",""))

head(DAT2)
DAT2$PID <-factor(DAT2$PID)

xtabs(is.na(RT)==FALSE~stim, data=DAT2)
xtabs(is.na(RT)==FALSE~PID, data=DAT2)

summary(DAT2$stim)
length(unique(DAT2$stim))
length(unique(DAT2$PID))

ggplot(data=DAT2 %>% filter(stim %in% c(unique(DAT2$stim)[1:12]) == TRUE),
       aes(x = stim, y = RT)) +
  geom_point(aes(col=stim), shape=16, size=1,
             position = position_jitter(width=0.2))+ 
  geom_boxplot(aes(fill=stim), col="black", alpha=0.2,
               outlier.shape = NA) +
  scale_x_discrete(name = "Stimulus") +
  scale_y_continuous(name = "Response Time") +
  scale_fill_grey()+
  scale_color_grey()+
  theme_bw()+
  theme(axis.text.x=element_text(size=12, color="black", angle=90), 
        axis.text.y=element_text(size=12, color="black", angle=0), 
        legend.text=element_text(size=12, color="black"),
        axis.title=element_text(size=12, face="bold"),
        plot.title=element_text(size=12, face="bold", hjust=0.5),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=12, face="bold"),
        legend.title=element_blank(),
        legend.position = "none")

ggplot(data=DAT2 %>% filter(PID %in% c(unique(DAT2$PID)[1:10]) == TRUE), 
       aes(x = PID, y = RT)) +
  geom_point(aes(col=PID), shape=16, size=1,
             position = position_jitter(width=0.2))+ 
  geom_boxplot(aes(fill=PID), col="black", alpha=0.2,
               outlier.shape = NA) +
  scale_fill_grey()+
  scale_color_grey()+
  scale_x_discrete(name = "Subject ID") +
  scale_y_continuous(name = "Response Time") +
  theme_bw()+
  theme(axis.text.x=element_text(size=12, color="black", angle=90), 
        axis.text.y=element_text(size=12, color="black", angle=0), 
        legend.text=element_text(size=12, color="black"),
        axis.title=element_text(size=12, face="bold"),
        plot.title=element_text(size=12, face="bold", hjust=0.5),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=12, face="bold"),
        legend.title=element_blank(),
        legend.position = "none")



# Basic Model ----
head(DAT2)
rand01 <- lmer(log(RT)~
                 # Fixed Effects 
                 1+modality+ 
                 # Random Effects
                 (1|PID)+(1|stim),
               data=DAT2, REML=TRUE)
anova(rand01)
summary(rand01)


fixef(rand01)
ranef(rand01)
resid(rand01)
var(resid(rand01))
qqnorm(y=rstudent(rand01))
abline(0,1)

getME(rand01, "mmList")
getME(rand01, "b")
getME(rand01, "Ztlist")
vcov.merMod(rand01)


# Random Slopes Model ----
head(DAT2)
rand02 <- lmer(log(RT)~
                 # Fixed Effects 
                 1+modality+ 
                 # Random Effects
                 (1+modality|PID)+(1|stim),
               data=DAT2, REML=TRUE)
anova(rand02)
summary(rand02)


fixef(rand02)
ranef(rand02)
resid(rand02)
var(resid(rand02))
qqnorm(y=rstudent(rand02))
abline(0,1)

getME(rand02, "mmList")
getME(rand02, "b")
getME(rand02, "Ztlist")
vcov.merMod(rand02)


