# title: "Model Specification in Mixed-Effects Models: A Focus on Random Effects"
# authors: Keith Lohse, PhD, PStat; Mike Strube, PhD; Allan Kozlowksi, PhD, PT
# date: "2022-07-07"

library("ggplot2"); library("lme4"); library("car"); library("dplyr"); library("lmerTest")

DATA <- read.csv("https://raw.githubusercontent.com/keithlohse/mixed_effects_models/master/random_effects_paper/data_longitudinal.csv",
                 stringsAsFactors = TRUE, na.strings=c("NA","NaN"," ",""))
# Use the head() function to check the structure of the data file. 
head(DATA)



## ----------------------- Basic Data Visualization ------------------------- 
DATA$year.0 <- (DATA$time -1)/12

## FIM scores by group and time -------------------------------------------- 
head(DATA)
ggplot(DATA, aes(x = year.0, y = rasch_FIM)) + 
  geom_line(aes(group=subID)) + 
  geom_point(aes(fill=as.factor(subID)), pch=21, size=1.5, stroke=1) + 
  scale_fill_grey()+
  facet_wrap(~AIS_grade) +
  scale_x_continuous(name = "Time from Admission (Years)", breaks=c(0,0.5,1,1.5), limits=c(0,1.6)) + 
  scale_y_continuous(name = "Independence (0-100)",limits=c(0,100)) + 
  theme_bw() + theme(axis.text.x=element_text(size=10, colour="black"),
                     axis.text.y=element_text(size=10, colour="black"), 
                     axis.title=element_text(size=12,face="bold")) +
  theme(strip.text.x = element_text(size = 12))+ theme(legend.position="none") 
## --------------------------------------------------------------------------



raneff_int<-lmer(rasch_FIM~ 
                   # Fixed-effects 
                   1+ 
                   # Random-effects 
                   (1|subID), data=DATA, REML=FALSE) 


summary(raneff_int)

# Recall that the fixed-effect for the intercept is the overall, "group-level" 
# intercept in our model. However, we also have a random-effect of subject for 
# the intercept. This means that our model estimates a deviate for each subject 
# from the group-level intercept. To see these random-effects using the raneff() 
# function.
ranef(raneff_int) 

# Remember that these are deviates from the fixed-effect, so if we want to see 
# what the model is actually estimating for each person, we need to add the 
# fixed-effect back in: 
fixef(raneff_int) 

# We could do this manually, by adding the fixef() output to the ranef() output, 
# but we can also get the individual values using the coef() function: 
coef(raneff_int)$subID 

# If you want the actual predictions of the model, rather than the estimated 
# effects, you can use the fitted() function. Note the difference in the size 
# of these arrays. The fitted() function gives us a prediction for each person 
# at each point. 
fitted(raneff_int)


first5<-DATA[c(1:90),] # Second, we'll make a smaller dataset with the predictions for these 10: 
PRED<-data.frame(subID=c("s01","s02","s03","s04","s05"), Intercepts=c(coef(raneff_int)$subID[c(1:5),])) 
PRED

ggplot(first5, aes(x = year.0, y = rasch_FIM)) + 
  geom_line(aes(group=subID)) + facet_wrap(~subID, ncol=5)+
  geom_point(fill="grey", pch=21, size=2, stroke=1.25) + 
  scale_x_continuous(name = "Time from Admission (Years)") + 
  scale_y_continuous(name = "Independence (0-100)",limits=c(0,100))+
  geom_abline(aes(intercept=Intercepts, slope=0), col="black", lwd=1, PRED) +
  theme_bw() + theme(axis.text=element_text(size=10, colour="black"), 
        axis.title=element_text(size=10,face="bold")) +
  theme(strip.text.x = element_text(size = 10))+ theme(legend.position="none") 


raneff_lin_fixed<-lmer(rasch_FIM~ 
                   # Fixed-effects 
                   1+year.0+
                   # Random-effects 
                   (1|subID), data=DATA, REML=FALSE) 


summary(raneff_lin_fixed)

first5<-DATA[c(1:90),] # Second, we'll make a smaller dataset with the predictions for these 10: 
PRED<-data.frame(subID=c("s01","s02","s03","s04","s05"), 
                 Intercepts=c(coef(raneff_lin_fixed)$subID[c(1:5),1]), 
                 Slopes=c(coef(raneff_lin_fixed)$subID[c(1:5),2]))  
PRED

ggplot(first5, aes(x = year.0, y = rasch_FIM)) + 
  geom_line(aes(group=subID)) + facet_wrap(~subID, ncol=5)+
  geom_point(fill="grey", pch=21, size=2, stroke=1.25) + 
  scale_x_continuous(name = "Time from Admission (Years)") + 
  scale_y_continuous(name = "Independence (0-100)",limits=c(0,100))+
  geom_abline(aes(intercept=Intercepts, slope=Slopes), col="black", lwd=1, PRED) +
  theme_bw() + theme(axis.text=element_text(size=10, colour="black"), 
                     axis.title=element_text(size=10,face="bold")) +
  theme(strip.text.x = element_text(size = 10))+ theme(legend.position="none") 


raneff_lin_rand<-lmer(rasch_FIM~ 
                        # Fixed-effects 
                        1+year.0+ 
                        # Random-effects 
                        (1+year.0|subID), data=DATA, REML=FALSE) 
summary(raneff_lin_rand)


PRED<-data.frame(subID=c("s01","s02","s03","s04","s05"), 
                 Intercepts=c(coef(raneff_lin_rand)$subID[c(1:5),1]), 
                 Slopes=c(coef(raneff_lin_rand)$subID[c(1:5),2])) 
PRED

ggplot(first5, aes(x = year.0, y = rasch_FIM)) + 
  geom_point(fill="grey", pch=21, size=2, stroke=1.25) + 
  geom_line(aes(group=subID)) + facet_wrap(~subID, ncol=5)+
  scale_x_continuous(name = "Time from Admission (Years)") + 
  scale_y_continuous(name = "Independence (0-100)",limits=c(0,100))+
  geom_abline(aes(intercept=Intercepts, slope=Slopes), col="red", lwd=1, PRED)+
  theme_bw() + theme(axis.text=element_text(size=10, colour="black"), 
        axis.title=element_text(size=10,face="bold")) +
  theme(strip.text.x = element_text(size = 10))+ theme(legend.position="none") 


raneff_quad_rand<-lmer(rasch_FIM~ 
                         # Fixed-effects 
                         1+year.0+I(year.0^2)+ 
                         # Random-effects 
                         (1+year.0+I(year.0^2)|subID), data=DATA, REML=FALSE) 
summary(raneff_quad_rand)


raneff_quad_rand_b<-lmer(rasch_FIM~ 
                         # Fixed-effects 
                         1+year.0+I(year.0^2)+ 
                         # Random-effects 
                         (1+year.0|subID), data=DATA, REML=FALSE) 
summary(raneff_quad_rand_b)

anova(raneff_quad_rand, raneff_quad_rand_b)

?getME
getME(raneff_quad_rand, "mmList")
getME(raneff_quad_rand, "b")
getME(raneff_quad_rand, "Ztlist")
vcov.merMod(raneff_quad_rand)
write.csv(as.matrix(getME(raneff_quad_rand, "X")), "./X_matrix.csv")
write.csv(as.matrix(getME(raneff_quad_rand, "Z")), "./Z_matrix.csv")

DATA$quad_pred <- fitted(raneff_quad_rand) # Save model predictions to data frame
first5<-DATA[c(1:90),] # Second, we'll make a smaller dataset with the predictions for these 10: 


ggplot(first5, aes(x = year.0, y = rasch_FIM)) + 
  geom_line(aes(group=subID)) + facet_wrap(~subID, ncol=5)+
  geom_point(fill="grey", pch=21, size=2, stroke=1.25) + 
  scale_x_continuous(name = "Time from Admission (Years)") + 
  scale_y_continuous(name = "Independence (0-100)",limits=c(0,100))+
  geom_line(aes(y=quad_pred), lwd=1, col="dodgerblue") +
  theme_bw() + theme(axis.text=element_text(size=10, colour="black"), 
                     axis.title=element_text(size=10,face="bold")) +
  theme(strip.text.x = element_text(size = 10))+ theme(legend.position="none") 


# Negative Exponential Model ----
ggplot(DATA, aes(x = year.0, y = rasch_FIM)) + 
  geom_line(aes(group=subID), col="grey") + 
  #stat_smooth(method="lm", formula=y~x+I(x^2), col="black", lwd=1.5, se=FALSE)+
  scale_x_continuous(name = "Time from Admission (Years)") + 
  scale_y_continuous(name = "Rasch-Scaled FIM Score (0-100)",limits=c(0,100)) +
  theme_bw() + 
  theme(axis.text=element_text(size=14, colour="black"), 
        axis.title=element_text(size=14,face="bold")) +
  theme(strip.text.x = element_text(size = 14))+ theme(legend.position="none") 

library(nlme)

set.seed(100)
neg_exp_rand_mod <- nlme(rasch_FIM ~ b_1i + 
                           (b_2i)*(exp(b_3i * year.0)),
                         data = DATA,
                         fixed = b_1i + b_2i + b_3i ~ 1,
                         random = list(b_1i ~ 1, b_2i ~ 1, b_3i ~1),
                         groups = ~ subID,
                         start = c(80, -70, -1),
                         na.action = na.omit)
summary(neg_exp_rand_mod)

coef(neg_exp_rand_mod)
fitted(neg_exp_rand_mod)

year <- seq(from=0, to=1.5, by=0.01)
rasch_FIM <- 58.53985-48.13958*exp(-2.53036*year)
PRED <- data.frame(year, rasch_FIM)

head(DATA)
ggplot(DATA, aes(x = year.0, y = rasch_FIM)) + 
  geom_line(aes(group=subID), col="grey") + 
  scale_x_continuous(name = "Time from Admission (Years)") + 
  scale_y_continuous(name = "Independence (0-100)",limits=c(0,100)) +
  #facet_wrap(~AIS_grade) +
  theme_bw() + 
  theme(axis.text=element_text(size=14, colour="black"), 
        axis.title=element_text(size=14,face="bold")) +
  theme(strip.text.x = element_text(size = 14))+ theme(legend.position="none")+
  geom_line(data=PRED, aes(x=year, y=rasch_FIM), col="black", lwd=1.5)


DATA$exp_pred <- fitted(neg_exp_rand_mod) # Save model predictions to data frame
first4<-DATA[c(1:72),] # Second, we'll make a smaller dataset with the predictions for these 10: 


ggplot(first4, aes(x = year.0, y = rasch_FIM)) + 
  geom_line(aes(group=subID)) + facet_wrap(~subID, ncol=2)+
  geom_point(fill="grey", pch=21, size=2, stroke=1.25) + 
  scale_x_continuous(name = "Time from Admission (Years)") + 
  scale_y_continuous(name = "Independence (0-100)",limits=c(0,100))+
  geom_line(aes(y=exp_pred), lwd=1, col="purple") +
  theme_bw() + theme(axis.text=element_text(size=12, colour="black"), 
                     axis.title=element_text(size=14,face="bold")) +
  theme(strip.text.x = element_text(size = 12))+ theme(legend.position="none") 
