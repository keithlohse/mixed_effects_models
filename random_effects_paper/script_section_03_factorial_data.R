# title: "Model Specification in Mixed-Effects Models: A Focus on Random Effects"
# authors: Keith Lohse, PhD, PStat; Mike Strube, PhD; Allan Kozlowksi, PhD, PT
# date: "2022-07-07"

library(lme4); library(lmerTest); library(ez); library(tidyverse)

HR <- c(60, 72, 42, 50)
subID <- factor(c("s1", "s1", "s2", "s2"))
condition <- factor(c("ctl", "ex", "ctl", "ex"))

DATA <- data.frame(subID, condition, HR)

mod01 <- lmer(HR~condition + (1|subID), REML=FALSE)
summary(mod01)
anova(mod01)
var(c(-9.9, 9.9, -9.9, 9.9))
var(c(-0.9, 1.1, 0.9, -1.1))


fixef(mod01)
ranef(mod01)
resid(mod01)
var(resid(mod01))
plot(resid(mod01))

getME(mod01, "mmList")
getME(mod01, "b")
getME(mod01, "Ztlist")
vcov.merMod(mod01)

(60+42)/2
(72+50)/2
mean(DATA$HR)


###################################

HR <- c(60, 72, 42, 50, 65, 80)
subID <- factor(c("s1", "s1", "s2", "s2", "s3", "s3"))
condition <- factor(c("ctl", "ex", "ctl", "ex", "ctl", "ex"))

DATA <- data.frame(subID, condition, HR)
DATA

mod01 <- lmer(HR~1+condition + (1|subID), REML=FALSE)
summary(mod01)
anova(mod01)

fixef(mod01)
ranef(mod01)
resid(mod01)
var(resid(mod01))
plot(resid(mod01))

getME(mod01, "mmList")
getME(mod01, "b")
getME(mod01, "Ztlist")
vcov.merMod(mod01)

(60+42)/2
(72+50)/2
mean(DATA$HR)


# Repeated Measures ANOVA example ----
list.files()

DATA<-read.csv("data_heart_rate.csv", header = TRUE,
               stringsAsFactors = TRUE)
head(DATA)

DATA$Cond <- fct_relevel(DATA$Cond, "rest", "imm", "delay")
DATA$Alt <- fct_relevel(DATA$Alt, "low", "high")

ggplot(data=DATA, aes(x = Cond, y = Heart_Rate)) +
  geom_line(aes(group=SUBID), col="grey")+
  geom_point(aes(group=Cond, fill=Cond), shape=21, col="black", size=2,
             position = position_dodge(width=0.75))+ 
  geom_boxplot(aes(fill=Cond), alpha=0.2) +
  scale_fill_grey()+
  facet_wrap(~Alt)+
  scale_x_discrete(name = "Condition") +
  scale_y_continuous(name = "Heart Rate (bpm)") +
  theme_bw()+
  theme(axis.text=element_text(size=12, color="black"), 
        legend.text=element_text(size=12, color="black"),
        axis.title=element_text(size=12, face="bold"),
        plot.title=element_text(size=12, face="bold", hjust=0.5),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=12, face="bold"),
        legend.title=element_blank(),
        legend.position = "none")


summary(aov(Heart_Rate ~ Alt*Cond + Error(SUBID/(Alt*Cond)), data=DATA))

ezANOVA(
  data=DATA
  , dv = .(Heart_Rate)
  , wid =.(SUBID)
  , within = .(Alt, Cond)
  , type = 3
  , white.adjust = FALSE
  , detailed = FALSE
  , return_aov = FALSE
)


# Maximum Likelihood Estimation ----
rand01 <- lmer(Heart_Rate~
                       # Fixed Effects 
                       1+Alt*Cond+
                       # Random Effects
                       (1|SUBID),
                     data=DATA, REML=FALSE)
anova(rand01)
summary(rand01)

rand02 <- lmer(Heart_Rate~
                 # Fixed Effects 
                 1+Alt*Cond+
                 # Random Effects
                 (1|SUBID)+(1|Alt)+(1|Cond),
               data=DATA, REML=FALSE)
anova(rand02)
summary(rand02)


xtabs(~SUBID+Alt+Cond, data=DATA)
rand03 <- lmer(Heart_Rate~
                 # Fixed Effects 
                 1+Alt*Cond+
                 # Random Effects
                 (1|SUBID)+(1|Alt:SUBID)+(1|Cond:SUBID),
               data=DATA, REML=FALSE)
anova(rand03)
summary(rand03)


fixef(rand03)
ranef(rand03)
resid(rand03)
var(resid(rand03))
plot(resid(rand03))

getME(rand03, "mmList")
getME(rand03, "b")
getME(rand03, "Ztlist")
vcov.merMod(rand03)





# Restricted Maximum Likelihood Estimation ----
rand01 <- lmer(Heart_Rate~
                 # Fixed Effects 
                 1+Alt*Cond+
                 # Random Effects
                 (1|SUBID),
               data=DATA, REML=TRUE)
anova(rand01)
summary(rand01)

rand02 <- lmer(Heart_Rate~
                 # Fixed Effects 
                 1+Alt*Cond+
                 # Random Effects
                 (1|SUBID)+(1|Alt)+(1|Cond),
               data=DATA, REML=TRUE)
anova(rand02)
summary(rand02)


xtabs(~SUBID+Alt+Cond, data=DATA)
rand03 <- lmer(Heart_Rate~
                 # Fixed Effects 
                 1+Alt*Cond+
                 # Random Effects
                 (1|SUBID)+(1|Alt:SUBID)+(1|Cond:SUBID),
               data=DATA, REML=TRUE)
anova(rand03)
summary(rand03)

fixef(rand03)
ranef(rand03)
resid(rand03)
var(resid(rand03))
plot(resid(rand03))

getME(rand03, "mmList")
getME(rand03, "b")
as.matrix(getME(rand03, "Ztlist")$`Alt:SUBID.(Intercept)`)
as.matrix(getME(rand03, "Ztlist")$`Cond:SUBID.(Intercept)`)
as.matrix(getME(rand03, "Ztlist")$`SUBID.(Intercept)`)
vcov.merMod(rand03)

write.csv(as.matrix(getME(rand03, "Ztlist")$`Alt:SUBID.(Intercept)`), "./RE_sub_alt.csv")
write.csv(as.matrix(getME(rand03, "Ztlist")$`Cond:SUBID.(Intercept)`), "./RE_sub_cond.csv")
write.csv(as.matrix(getME(rand03, "Ztlist")$`SUBID.(Intercept)`), "./RE_sub.csv")



