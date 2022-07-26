# title: "Model Specification in Mixed-Effects Models: A Focus on Random Effects"
# authors: Keith Lohse, PhD, PStat; Mike Strube, PhD; Allan Kozlowksi, PhD, PT
# date: "2022-07-26"

library(lme4); library(lmerTest); library(ez); library(tidyverse)


list.files()
DATA<-read.csv("rt_dummy_data.csv", header = TRUE,
               stringsAsFactors = TRUE)
head(DATA)
DATA$PID <-factor(DATA$PID)

?xtabs
xtabs(is.na(RT)==FALSE~stim, data=DATA)
xtabs(is.na(RT)==FALSE~PID, data=DATA)

summary(DATA$stim)
unique(DATA$stim)
length(unique(DATA$PID))

STIM_LIST <- c(unique(DATA$stim)[1:10])
STIM_LIST

DATA$stim %in% STIM_LIST


ggplot(data=DATA %>% filter(stim %in% c(unique(DATA$stim)[1:12]) == TRUE),
       aes(x = stim, y = RT)) +
  geom_point(aes(col=stim), shape=16, size=1,
             position = position_jitter(width=0.2))+ 
  geom_boxplot(aes(fill=stim), col="black", alpha=0.2,
               outlier.shape = NA) +
  scale_x_discrete(name = "Stimulus") +
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

ggplot(data=DATA %>% filter(PID %in% c(unique(DATA$PID)[1:10]) == TRUE), 
       aes(x = PID, y = RT)) +
  geom_point(aes(col=PID), shape=16, size=1,
             position = position_jitter(width=0.2))+ 
  geom_boxplot(aes(fill=PID), col="black", alpha=0.2,
               outlier.shape = NA) +
  scale_x_discrete(name = "Stimulus") +
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
head(DATA)
rand01 <- lmer(log(RT)~
                 # Fixed Effects 
                 1+modality+ 
                 # Random Effects
                 (1|PID)+(1|stim),
               data=DATA, REML=TRUE)
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



