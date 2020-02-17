library(tidyverse); library(RCurl); library(ez); library(lme4); library(lmerTest)

DATA <- read.csv("https://raw.githubusercontent.com/keithlohse/mixed_effects_models/master/data_example.csv")

data_TIME <- aggregate(speed ~ subID + time + age_group + group, data=DATA, FUN=mean)
data_TIME <- data_TIME %>% arrange(subID, age_group, group, time)
head(data_TIME)


ggplot(data_TIME, aes(x = time, y = speed)) +
  geom_point(aes(fill=subID), pch=21, size=2)+
  geom_line(aes(col=subID), alpha=0.4)+
  facet_wrap(~age_group) +
  scale_x_continuous(name = "Trial") +
  scale_y_continuous(name = "Speed (m/s)") +
  theme(axis.text=element_text(size=16, color="black"), 
        axis.title=element_text(size=16, face="bold"),
        plot.title=element_text(size=16, face="bold", hjust=0.5),
        strip.text = element_text(size=16, face="bold"),
        legend.position = "none")
  

ggplot(data_TIME, aes(x = time, y = speed)) +
  stat_smooth(aes(col=subID, lty=age_group), method="lm", se=FALSE, alpha=0.4)+
  geom_point(aes(fill=subID), pch=21, size=2)+
  stat_smooth(aes(lty=age_group), method="lm", col="black", lwd=1.5,
              se=FALSE, alpha=0.4)+
  facet_wrap(~age_group) +
  scale_x_continuous(name = "Trial") +
  scale_y_continuous(name = "Speed (m/s)") +
  theme(axis.text=element_text(size=16, color="black"), 
        axis.title=element_text(size=16, face="bold"),
        plot.title=element_text(size=16, face="bold", hjust=0.5),
        strip.text = element_text(size=16, face="bold"),
        legend.position = "none")

