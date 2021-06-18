library(tidyverse); library(RCurl); library(ez); library(lme4); library(car); library(lmerTest)

DATA <- read.csv("https://raw.githubusercontent.com/keithlohse/mixed_effects_models/master/data_example.csv",
                 stringsAsFactors = TRUE)

head(DATA, 10)

data_COND <- DATA %>% group_by(subID, condition, age_group, group) %>% # Note we ignore trial to average across it
  summarize(speed = mean(speed, na.rm=TRUE)) %>% # I have included na.rm=TRUE even though there are no missing data
  arrange(age_group, subID, condition) # finally, we can resort our data by subIDs within groups
head(data_COND, 10)

data_TIME <- DATA %>% group_by(subID, time, age_group, group) %>% # Note we ignore condition to average across it
  summarize(speed = mean(speed, na.rm=TRUE)) %>% # I have included na.rm=TRUE even though there are no missing data
  arrange(age_group, subID, time) # finally, we can resort our data by subIDs within groups
head(data_TIME, 10)

ggplot(data_COND, aes(x = condition, y = speed)) +
  geom_point(aes(fill=condition), pch=21, size=2,
             position=position_jitter(w=0.2, h=0))+
  geom_boxplot(aes(fill=condition), col="black", 
               alpha=0.4, width=0.5, outlier.shape = NA)+
  scale_x_discrete(name = "Condition") +
  scale_y_continuous(name = "Speed (m/s)") +
  theme(axis.text=element_text(size=16, color="black"), 
        axis.title=element_text(size=16, face="bold"),
        plot.title=element_text(size=16, face="bold", hjust=0.5),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=16, face="bold"),
        legend.position = "none")


summary(aov(speed ~ condition + Error(subID/condition), data=data_COND))

ezANOVA(data = data_COND, 
        dv = .(speed),
        wid = .(subID),
        within = .(condition)
)

# First we will define our model
mod1 <- lmer(speed ~ 
               # Fixed Effects:
               condition + 
               # Random Effects: 
               (1|subID), 
             # Define the data: 
             data=data_COND, REML = TRUE)

# We can then get the ANOVA results for our model:
anova(mod1)

summary(mod1)


DATA$time <- factor(DATA$time)

ggplot(DATA, aes(x = time, y = speed)) +
  geom_point(aes(fill=condition), pch=21, size=2,
             position=position_jitterdodge(dodge.width = 0.5, jitter.width = 0.1))+
  geom_boxplot(aes(fill=condition), col="black", 
               alpha=0.4, width=0.5, outlier.shape = NA)+
  labs(fill="Condition")+
  scale_x_discrete(name = "Trial") +
  scale_y_continuous(name = "Speed (m/s)", limits = c(0,3)) +
  theme(axis.text=element_text(size=16, color="black"), 
        axis.title=element_text(size=16, face="bold"),
        legend.text=element_text(size=16, color="black"), 
        legend.title=element_text(size=16, face="bold"),
        plot.title=element_text(size=16, face="bold", hjust=0.5),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=16, face="bold"),
        legend.position = "bottom")

DATA$time <- factor(DATA$time)
summary(aov(speed ~ condition*time + Error(subID/(condition*time)), data=DATA))

# First we will define our model
mod2 <- lmer(speed ~ 
               # Fixed Effects:
               time*condition + 
               # Random Effects
               (1|subID)+ (1|time:subID) + (1|condition:subID), 
             # Define your data, 
             data=DATA, REML=TRUE)

# We can then get the ANOVA results for our model:
anova(mod2)

summary(mod2)


ggplot(data_COND, aes(x = condition, y = speed)) +
  geom_point(aes(fill=age_group), pch=21, size=2,
             position=position_jitterdodge(dodge.width = 0.5, jitter.width = 0.1))+
  geom_boxplot(aes(fill=age_group), col="black",
               alpha=0.4, width=0.5, outlier.shape=NA)+
  labs(fill="Age Group")+
  scale_x_discrete(name = "Condition") +
  scale_y_continuous(name = "Speed (m/s)") +
  theme(axis.text=element_text(size=16, color="black"), 
        axis.title=element_text(size=16, face="bold"),
        legend.text=element_text(size=16, color="black"), 
        legend.title=element_text(size=16, face="bold"),
        plot.title=element_text(size=16, face="bold", hjust=0.5),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=16, face="bold"),
        legend.position = "bottom")

summary(aov(speed ~ age_group*condition + Error(subID/condition), data=data_COND))

ezANOVA(data = data_COND, 
        dv = .(speed),
        wid = .(subID),
        within = .(condition),
        between = .(age_group)
)

mod3 <- lmer(speed ~ 
               # Fixed Effects:
               age_group*condition + 
               # Random Effects
               (1|subID), 
             # Define your data, 
             data=data_COND, REML=TRUE)

# We can then get the ANOVA results for our model:
anova(mod3)


summary(mod3)

DATA$time <- factor(DATA$time)

ggplot(DATA, aes(x = time, y = speed)) +
  geom_point(aes(fill=age_group), size=2, shape=21,
             position=position_jitterdodge(dodge.width = 0.5, jitter.width = 0.1))+
  geom_boxplot(aes(fill=age_group), col="black", 
               alpha=0.4, width=0.5, outlier.shape = NA)+
  labs(fill="Age Group")+
  facet_wrap(~condition)+
  scale_x_discrete(name = "Trial") +
  scale_y_continuous(name = "Speed (m/s)", limits = c(0,3)) +
  theme(axis.text=element_text(size=16, color="black"), 
        axis.title=element_text(size=16, face="bold"),
        legend.text=element_text(size=16, color="black"), 
        legend.title=element_text(size=16, face="bold"),
        plot.title=element_text(size=16, face="bold", hjust=0.5),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=16, face="bold"),
        legend.position = "bottom")

summary(aov(speed ~ age_group*condition*time + Error(subID/(condition*time)), data=DATA))


ezANOVA(data = DATA, 
       dv = .(speed),
       wid = .(subID),
       within = .(condition, time),
       between = .(age_group)
)

# First we will define our model
mod4 <- lmer(speed ~ 
               # Fixed Effects:
               age_group*condition*time + 
               # Random Effects
               (1|subID)+(1|condition:subID)+(1|time:subID), 
             # Define your data, 
             data=DATA, REML=TRUE)

# We can then get the ANOVA results for our model:
anova(mod4)

summary(mod4)

