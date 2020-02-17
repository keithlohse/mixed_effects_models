
library(tidyverse); library(RCurl); library(ez); library(lme4); library(lmerTest)

DATA <- read.csv("https://raw.githubusercontent.com/keithlohse/mixed_effects_models/master/data_example.csv")


head(DATA, 10)


data_COND <- aggregate(speed ~ subID + condition + age_group + group, data=DATA, FUN=mean)
data_COND <- data_COND %>% arrange(subID, age_group, group, condition)
head(data_COND, 10)

data_TIME <- aggregate(speed ~ subID + time + age_group + group, data=DATA, FUN=mean)
data_TIME <- data_TIME %>% arrange(subID, age_group, group, time)
head(data_TIME)

ggplot(data_COND, aes(x = condition, y = speed)) +
       geom_point(aes(fill=condition), pch=21, size=2,
      position=position_jitter(w=0.2, h=0))+
     geom_boxplot(aes(fill=condition), col="black", 
      alpha=0.4, width=0.5)+
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
             position=position_jitterdodge(dodge.width = 0.5))+
  geom_boxplot(aes(fill=condition), col="black", 
               alpha=0.4, width=0.5)+
  scale_x_discrete(name = "Time") +
  scale_y_continuous(name = "Speed (m/s)", limits = c(0,3)) +
  theme(axis.text=element_text(size=16, color="black"), 
        axis.title=element_text(size=16, face="bold"),
        plot.title=element_text(size=16, face="bold", hjust=0.5),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=16, face="bold"),
        legend.position = "right")


DATA$time <- factor(DATA$time)

summary(aov(speed ~ condition*time + Error(subID/(condition*time)), data=DATA))

ezANOVA(data = DATA, 
    dv = .(speed),
    wid = .(subID),
    within = .(time, condition)
)
mod1 <- lmer(speed ~ 
               # Fixed Effects:
               time*condition + 
               # Random Effects
               (1|subID)+ (1|time:subID) + (1|condition:subID), 
             # Define your data, 
             data=DATA, REML=TRUE)

# We can then get the ANOVA results for our model:
anova(mod1)

summary(mod1)






ggplot(data_COND, aes(x = condition, y = speed)) +
  geom_point(aes(fill=group), pch=21, size=2,
             position=position_jitterdodge(dodge.width = 0.5))+
  geom_boxplot(aes(fill=group), col="black",
               alpha=0.4, width=0.5)+
  facet_wrap(~age_group)+
  scale_x_discrete(name = "Condition") +
  scale_y_continuous(name = "Speed (m/s)") +
  theme(axis.text=element_text(size=16, color="black"), 
        axis.title=element_text(size=16, face="bold"),
        plot.title=element_text(size=16, face="bold", hjust=0.5),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=16, face="bold"),
        legend.position = "right")

summary(aov(speed ~ age_group*group*condition + Error(subID/condition), data=data_COND))

ezANOVA(data = data_COND, 
        dv = .(speed),
        wid = .(subID),
        within = .(condition),
        between = .(group, age_group)
)



# First we will define our model
mod1 <- lmer(speed ~ 
               # Fixed Effects:
               group*age_group*condition + 
               # Random Effects
               (1|subID), 
               # Define your data, 
             data=data_COND, REML=TRUE)

# We can then get the ANOVA results for our model:
anova(mod1)

summary(mod1)

DATA$time <- factor(DATA$time)

ggplot(DATA, aes(x = time, y = speed)) +
  geom_point(aes(fill=group), size=2, shape=21,
             position=position_jitterdodge(dodge.width = 0.5))+
  geom_boxplot(aes(fill=group), col="black", 
               alpha=0.4, width=0.5)+
  facet_wrap(~condition+age_group)+
  scale_x_discrete(name = "Time") +
  scale_y_continuous(name = "Speed (m/s)", limits = c(0,3)) +
  theme(axis.text=element_text(size=16, color="black"), 
        axis.title=element_text(size=16, face="bold"),
        plot.title=element_text(size=16, face="bold", hjust=0.5),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=16, face="bold"),
        legend.position = "right")

summary(aov(speed ~ age_group*group*condition*time + Error(subID/(condition*time)), data=DATA))

ezANOVA(data = DATA, 
    dv = .(speed),
    wid = .(subID),
    within = .(condition, time),
    between = .(group, age_group)
)

# First we will define our model
mod1 <- lmer(speed ~ 
               # Fixed Effects:
               group*age_group*condition*time + 
               # Random Effects
               (1|subID)+(1|condition:subID)+(1|time:subID), 
               # Define your data, 
             data=DATA, REML=TRUE)

# We can then get the ANOVA results for our model:
anova(mod1)


summary(mod1)


