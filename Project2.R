## Project 2 #### 
library(ggplot2)

## Load the data #### 
plasma <- read.delim("Data/plasma.txt")
head(plasma)
summary(plasma)


## Turn the categorical variables into factor variables and pick reference categories ####
#a)Labeling
plasma$sex <- factor(plasma$sex,
                        levels = c(1, 2),
                        labels = c("Male", "Female"))

plasma$smokstat <- factor(plasma$smokstat,
                             levels = c(1, 2,3),
                             labels = c("Never", "Former", "Current Smoker"))

plasma$bmicat <- factor(plasma$bmicat,
                           levels = c(1, 2,3,4),
                           labels = c("Underweight", "Normal","Overweight", "Obese"))

plasma$vituse <- factor(plasma$vituse,
                           levels = c(1, 2,3),
                           labels = c("Yes, fairly often", "Yes, not often","No"))


plasma$vituse<- relevel(plasma$vituse,"Yes, fairly often")
plasma$bmicat<- relevel(plasma$bmicat,"Normal")
plasma$sex<- relevel(plasma$sex,"Female")
plasma$smokstat<- relevel(plasma$smokstat,"Never")


#1(a) Is the concentration low or not? Model the probability of having a low concentration ####
one_mole <- 6.02214076*10^23
bc_molar_mass <- 536.9
gram_per_liter <- bc_molar_mass*0.42*10^-6
ng_per_liter <- gram_per_liter*10^9
ng_per_ml <- ng_per_liter/(1000)
a <- ng_per_ml

#1(b) Create the variable lowplasma ####
plasma$lowplasma <- as.numeric(plasma$betaplasma < a)

plasma$plasmacat <- factor(plasma$lowplasma,
                           levels = c(0,1),
                           labels = c("high", "low"))

ggplot(data = plasma, aes(x = age,y=lowplasma, color = plasmacat)) + geom_point()

# Part 1 - Introduction to logistic regression

# Examine the relationship between low plasma and smoking status
table <-table(plasma$smokstat,plasma$lowplasma)

# Estimate the probabilities and odds for having low plasma beta-carotene and add
# Columns to the table

table <- cbind(table,c(109,86,40)/c(157,115,43),1-c(109,86,40)/c(157,115,43))
table <-cbind(table,c(0.6942675/0.30573248,0.7478261/0.25217391, 0.9302326/0.06976744   ) )

# Fit a logistic regression model for lowplasma with smokstat as explanatory variable. 
log.model <- glm(lowplasma ~ smokstat, family = "binomial", data = plasma)


summary(log.model)
log.model$coefficients
confint(log.model)
exp(log.model$coefficients)
expconf <- exp(confint(log.model))

probs1 <-exp(log.model$coefficients[1])/(1+exp(log.model$coefficients[1]))
probs2 <-exp(log.model$coefficients[1])*exp(log.model$coefficients[2])/(1+exp(log.model$coefficients[1])*exp(log.model$coefficients[2]))
probs3 <-exp(log.model$coefficients[1])*exp(log.model$coefficients[3])/(1+exp(log.model$coefficients[1])*exp(log.model$coefficients[3]))

probs1.lwr <- expconf[1,1]/(1+expconf[1,1])
probs1.upr <- expconf[1,2]/(1+expconf[1,2])

probs2.lwr <- expconf[1,1]*expconf[2,1]/(1+expconf[1,1]*expconf[2,1])
probs2.upr <- expconf[1,2]*expconf[2,2]/(1+expconf[1,2]*expconf[2,2])

probs3.lwr <- expconf[1,1]*expconf[3,1]/(1+expconf[1,1]*expconf[3,1])
probs3.upr <- expconf[1,2]*expconf[3,2]/(1+expconf[1,2]*expconf[3,2])



## test whether there are differences between the categories
# Wald test:
summary(log.model)$coefficients
# Since |4.92| > lambda_0.025 = 1.96 we can reject
# H0: beta_1 = 0
# Alt. Since
# P(|N(0,1)| > 4.92) = 2*P(N(0,1) > 4.92) = 8.7*10^(-7) < 0.05
# we can reject H0.
# The number of cars (or, rather, the number of thousands of cars)
# has a significant impact on the probability of a high
# concentration of PM10-particles.


## Plot of lowplasma against age ####
ggplot(data = plasma, aes(x = age,y= lowplasma)) + geom_point() + geom_smooth()


## Age model ####
age.model <- glm(lowplasma ~ age, family = "binomial", data = plasma)
age.model$coefficients
summary(age.model)$coefficients
exp(age.model$coefficients)

plasma.pred <- cbind(
  plasma,
  phat = predict(age.model, type = "response")
)
head(plasma.pred)


# logit = logodds with s.e. for constructing C.I.
plasma.pred <- cbind(
  plasma.pred,
  logit = predict(age.model, se.fit = TRUE))
head(plasma.pred)
# An unnecessary variable:
plasma.pred$logit.residual.scale <- NULL


# Calculate confidence intervals for the log odds
# standard normal quantile:
(lambda <- qnorm(1 - 0.05/2))
plasma.pred$logit.lwr <- plasma.pred$logit.fit - lambda*plasma.pred$logit.se.fit
plasma.pred$logit.upr <- plasma.pred$logit.fit + lambda*plasma.pred$logit.se.fit
head(plasma.pred)

# transform the log-odds intervals into C.I. for odds
plasma.pred$odds.lwr <- exp(plasma.pred$logit.lwr)
plasma.pred$odds.upr <- exp(plasma.pred$logit.upr)
head(plasma.pred)

# transform the odds intervals into C.I. for p
plasma.pred$p.lwr <- plasma.pred$odds.lwr/(1 + plasma.pred$odds.lwr)
plasma.pred$p.upr <- plasma.pred$odds.upr/(1 + plasma.pred$odds.upr)
head(plasma.pred)

# Plot the intervals and probability predictions  against smokstat
ggplot(plasma.pred, aes(age, lowplasma)) +
  geom_point() +
  geom_line(aes(y = phat), color = "red", size = 1) +
  geom_ribbon(aes(ymin = p.lwr, ymax = p.upr), alpha = 0.2) +
  xlab("age") +
  ylab("Low betaplasma") +
  labs(title = "Low plasma (=1) or Not low plasma (=0) vs age",
       caption = "red = fitted line, with 95% confidence interval") +
  theme(text = element_text(size = 14))


# Get specific age estimates
plasma.pred[, ]

# Examine differences in probability depending on age (HOW DO I DO THIS?)
age30 <- data.frame(age = c(30))
age31 <- data.frame(age = c(31))
age70 <- data.frame(age = c(70))
age71 <- data.frame(age = c(71))
(logodds30 <- predict(age.model, age30))
(prob30 <- exp(logodds30)/(1+exp(logodds30)))
(logodds31 <- predict(age.model, age31))
(prob31 <- exp(logodds31)/(1+exp(logodds31)))
diff30 <- prob30-prob31

(logodds70 <- predict(age.model, age70))
(prob70 <- exp(logodds70)/(1+exp(logodds70)))
(logodds71 <- predict(age.model, age71))
(prob71 <- exp(logodds71)/(1+exp(logodds71)))
diff70 <- prob70-prob71


# 1(c) ####
# Compute leverages and plot against age
plasma.pred$v <- influence(age.model)$hat

highlev <- 2*(length(age.model$coefficients)+1)/nrow(plasma.pred)
(
  agelev.plot <- ggplot(plasma.pred, aes(x = age, y = v)) + 
    geom_point() +
    geom_hline(yintercept = 1/nrow(plasma.pred)) +
    geom_hline(yintercept = highlev, color = "red") +
    labs(title = "lowplasma: leverage vs age") +
    labs(caption = "y = 1/n (black) and 2(p+1)/n (red)") +
    theme(text = element_text(size = 18)) +
    ylim(0, max(plasma.pred$v)*1.1) +
    facet_wrap(~lowplasma)
)

#  Fit linear model for comparison
linage.model <- lm(lowplasma ~ age, data = plasma)
linage.pred <- cbind(
  plasma,
  fit = predict(linage.model),
  conf = predict(linage.model, interval = "confidence"),
  pred = predict(linage.model, interval = "prediction")
)
linage.pred$conf.fit <- linage.model$pred.fit <- NULL 
linage.pred$v <- influence(linage.model)$hat

# Plot in original leverage plot but in different color
(agelev.plot <- agelev.plot + geom_point(data = linage.pred, color = "blue"))


# Observations with highest leverage 
levindices <- which(plasma.pred$v > highlev)
highestlevindices <- levindices[1:3]  # found manually.. can this be done automatically?
# agehighestlev <- which.max(plasma.pred$v)

# Plot these in the probability prediction plot in 1(b)
(ageplot.data <-  ageplot.data + geom_point(data = plasma.pred[highestlevindices, ], 
                                            color = "red", size = 3))


# 1(d) ####
# Examining the standardized deviance residuals for the age model (L8, sl.15)
plasma.pred$devres <- influence(age.model)$dev.res
plasma.pred$devstd <- plasma.pred$devres/sqrt(1 - plasma.pred$v)
head(plasma.pred)

# Plot std. dev. res. against age
ggplot(plasma.pred, aes(x = age, y = devstd, color = plasmacat)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-2, 2), linetype = "dashed", size = 1) +
  geom_hline(yintercept = c(-3, 3), linetype = "dotted", size = 1) +
  labs(title = "Standardized deviance residuals vs linear predictor",
       color = "lowplasma") +
  theme(text = element_text(size = 14)) +
  geom_point(data = plasma.pred[highestlevindices, ], 
             color = "red", size = 3)

# Find the observation with largest (absolute value) residual
highestdevstdindex <- which(abs(plasma.pred$devstd) > 2)  # make sure that it is only one observation
highestdevstd <- plasma.pred$devstd[highestdevstdindex]

# Plot this in the probability prediction plot in 1(b)
(ageplot.data <-  ageplot.data + geom_point(data = plasma.pred[highestdevstdindex, ], 
                                            color = "blue", size = 3))


# 1(e) ####
# Calculate Cook's distance for the age model 
plasma.pred$Dcook <- cooks.distance(age.model)
head(plasma.pred)

ggplot(plasma.pred, aes(x = age, y = Dcook, color = plasmacat)) +
  geom_point() +
  geom_hline(yintercept = 4/nrow(plasma), linetype = "dotted",
             size = 1) +
  labs(title = "Cook's distance vs linear predictor, by age",
       color = "lowplasma", 
       caption = "4/n in black, high leverage highlighted") +
  theme(text = element_text(size = 14)) + 
  geom_point(data = plasma.pred[highestlevindices, ], 
             color = "red", size = 3) + 
  geom_point(data = plasma.pred[highestdevstdindex, ], 
             color = "blue", size = 3)


# Part 2 ####
# 2(a) ####
# Forward selection using AIC from null model (see lecture06_ex1_cabbage_crit.R)

null.model <- glm(lowplasma ~ 1, family = "binomial", data = plasma)
upper.model <- glm(lowplasma ~ age + sex + smokstat + quetelet, data = plasma)

background.model <- step(null.model,
                         scope = list(upper = upper.model),
                         direction = "forward")

# Summarize parameter estimates and such
background.model$coefficients
confint(background.model)
exp(background.model$coefficients)
exp(confint(background.model))

# Divide ages into three categories 
plasma$agecat <- cut(plasma$age, breaks = c(0, 40, 55, 100))

# Compute predicted probabilities
background.pred <- cbind(
  plasma,  # should have used an 'original' plasma.data (now this contains v, devres etc..)
  phat = predict(background.model, type = "response")
)
head(background.pred)

# Plot lowplasma and predictions against age for each smokstat category
(
  background_age.plot <-
    ggplot(data = background.pred, aes(x = age, y = lowplasma)) + 
    geom_point(size = 2) +
    xlab("age") +
    ylab("lowplasma (1 or 0) + probabilities") +
    labs(title = "Plot of lowplasma vs age with predicted probabilities") +
    theme(text = element_text(size = 18)) +
    geom_point(aes(y = phat, color = bmicat), size = 1) +
    facet_wrap(~smokstat)
)

# Plot lowplasma and predictions against quetelet for each agecat
(
  background_quetelet.plot <-
    ggplot(data = background.pred, aes(x = quetelet, y = lowplasma)) + 
    geom_point(size = 2) +
    xlab("quetelet") +
    ylab("lowplasma (1 or 0) + probabilities") +
    labs(title = "Plot of lowplasma vs quetelet with predicted probabilities") +
    theme(text = element_text(size = 18)) +
    geom_point(aes(y = phat, color = agecat), size = 1) +
    facet_wrap(~smokstat)
)


# 2(b) ####
# Calculate leverage, std.dev.res and Cook's distances for Background model
background.pred$v <- influence(background.model)$hat

background.pred$devres <- influence(background.model)$dev.res
background.pred$devstd <- background.pred$devres/sqrt(1 - background.pred$v)


background.pred$Dcook <- cooks.distance(background.model)

head(background.pred)

# Plot leverage against age
highlev <- 2*(length(background.model$coefficients)+1)/nrow(background.pred)
(
  backgroundlevage.plot <- ggplot(background.pred, aes(x = age, y = v, color = bmicat)) + 
    geom_point() +
    geom_hline(yintercept = 1/nrow(background.pred)) +
    geom_hline(yintercept = highlev, color = "red") +
    labs(title = "background model: leverage vs age") +
    labs(caption = "y = 1/n (black) and 2(p+1)/n (red)") +
    theme(text = element_text(size = 18)) +
    ylim(0, max(background.pred$v)*1.1) +
    facet_wrap(~smokstat)
)

# Plot leverage against quetelet
(
  backgroundlevquetelet.plot <- ggplot(background.pred, aes(x = quetelet, y = v, color = agecat)) + 
    geom_point() +
    geom_hline(yintercept = 1/nrow(background.pred)) +
    geom_hline(yintercept = highlev, color = "red") +
    labs(title = "background model: leverage vs quetelet") +
    labs(caption = "y = 1/n (black) and 2(p+1)/n (red)") +
    theme(text = element_text(size = 18)) +
    ylim(0, max(background.pred$v)*1.1) +
    facet_wrap(~smokstat)
)

# Plot standardized deviance residuals in the same way as for the leverages
# first vs age
ggplot(background.pred, aes(x = age, y = devstd, color = bmicat)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-2, 2), linetype = "dashed", size = 1) +
  geom_hline(yintercept = c(-3, 3), linetype = "dotted", size = 1) +
  labs(title = "Standardized deviance residuals vs age") +
  theme(text = element_text(size = 14)) +
  facet_wrap(~smokstat)

# then vs quetelet
ggplot(background.pred, aes(x = quetelet, y = devstd, color = agecat)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-2, 2), linetype = "dashed", size = 1) +
  geom_hline(yintercept = c(-3, 3), linetype = "dotted", size = 1) +
  labs(title = "Standardized deviance residuals vs age") +
  theme(text = element_text(size = 14)) +
  facet_wrap(~smokstat)

# Find the observation with larger than +-2 residual value
highdevstdindices <- which(abs(background.pred$devstd) > 2)  # make sure that it is only one observation
highdevstd <- plasma.pred$devstd[highdevstdindices]

# Plot Cook's distance in the same way as before
# first for age
ggplot(background.pred, aes(x = age, y = Dcook, color = bmicat)) +
  geom_point() +
  # geom_hline(yintercept = 1, color = "red", linetype = "dashed",
  #                        size = 1) +
  geom_hline(yintercept = 4/nrow(plasma), linetype = "dotted",
             size = 1) +
  labs(title = "Cook's distance vs linear predictor, by age",
       caption = "4/n in black, high leverage highlighted") +
  theme(text = element_text(size = 14)) + 
  geom_point(data = background.pred[highdevstdindices, ], 
             color = "blue", size = 3) + 
  facet_wrap(~smokstat)

# then for quetelet
ggplot(background.pred, aes(x = quetelet, y = Dcook, color = agecat)) +
  geom_point() +
  geom_hline(yintercept = 4/nrow(plasma), linetype = "dotted",
             size = 1) +
  labs(title = "Cook's distance vs linear predictor, by quetelet",
       caption = "4/n in black, high leverage highlighted") +
  theme(text = element_text(size = 14)) + 
  geom_point(data = background.pred[highdevstdindices, ], 
             color = "blue", size = 3) + 
  facet_wrap(~smokstat)

# Plot large residual points in the probability prediction plot in 2(a)
(background_age.plot <-  background_age.plot + geom_point(data = background.pred[highdevstdindices, ], 
                                                          color = "blue", size = 3))
(background_quetelet.plot <-  background_quetelet.plot + geom_point(data = background.pred[highdevstdindices, ], 
                                                                    color = "blue", size = 3))
## 2.3 (c) The diet model ####
null.model <- glm(lowplasma ~ 1, family = "binomial", data = plasma)
upper.model <- glm(lowplasma ~ vituse + calories + fat + fiber + alcohol + cholesterol + betadiet, data = plasma)

diet.model <- step(null.model,
                         scope = list(upper = upper.model),
                         direction = "forward")
#Present the beta-estimates
diet.model$coefficients
exp(diet.model$coefficients)
exp(confint(diet.model))

#For background.model, diet.model and age.model present McFadden pseudo R2 ####
# pseudo R2####

model.0 <- glm(lowplasma ~ 1, family = "binomial", data = plasma)



# Null model: ln L(b0)
logLik(model.0)
(lnL0 <- logLik(model.0)[1])

(R2CS.max <- 1 - (exp(lnL0))^(2/nrow(plasma)))

bic <- BIC(model.0, age.model, background.model, diet.model)
aic <- AIC(model.0, age.model, background.model, diet.model)
(collect.AIC <- data.frame(aic, bic))





# Collect the log likelihoods L(betahat)
collect.AIC$loglik <- 
  c(logLik(model.0)[1],
    logLik(age.model)[1],
    logLik(background.model)[1],
    logLik(diet.model)[1]
    )
# calculate R2_McF;
collect.AIC$R2McF <- 1 - collect.AIC$loglik/lnL0
# calculate R2_McF,adj. Note that p+1 = df (and df.1):
collect.AIC$R2McF.adj <- 1 - (collect.AIC$loglik - (collect.AIC$df - 1)/2)/lnL0
# calculate R2_CS:
collect.AIC$R2CS <- 1 - (exp(lnL0 - collect.AIC$loglik))^(2/nrow(plasma))
# Calculate R2_N:
collect.AIC$R2N <- collect.AIC$R2CS/R2CS.max

# Show them as % with one decimal value:
round(100*collect.AIC[, c("R2McF", "R2McF.adj", "R2CS", "R2N")], digits = 3)
collect.AIC$df

#2.3 (d) - Suggest a bunch of models ####

# Construct the Final model allowing all background and dietary variables
finalmax.model <- glm(lowplasma ~ vituse + calories + fat + fiber + alcohol + cholesterol + betadiet
                      + age + sex + smokstat + quetelet, family = "binomial", data = plasma)

# Test forward selection
finalforward.model <- step(null.model,
                           scope = list(upper = finalmax.model),
                           direction = "forward")

# Backward selection
finalbackward.model <- step(finalmax.model,
                            scope = list(lower = null.model),
                            direction = "backward")

# Stepwise selection
# from background model -> produces same as finalforward.model
#finalstepwisebackground.model <- step(background.model,
#                                      scope = list(lower = null.model, upper = finalmax.model),
#                                      direction = "both")

# from diet model -> produces same as finalbackward.model
#finalstepwisediet.model <- step(diet.model,
#                                      scope = list(lower = null.model, upper = finalmax.model),
#                                      direction = "both")

AICs <- AIC(finalforward.model, finalbackward.model
            #, finalstepwisebackground.model, finalstepwisediet.model
)
finalmodelsummary <- data.frame(AICs)
finalmodelsummary$loglik <- c(logLik(finalforward.model)[1],
                              logLik(finalbackward.model)[1]
                              #,logLik(finalstepwisebackground.model)[1],
                              #logLik(finalstepwisediet.model)[1]
)
finalmodelsummary$R2McF <- 1 - finalmodelsummary$loglik/lnL0
finalmodelsummary$R2McF.adj <- 1 - (finalmodelsummary$loglik - (finalmodelsummary$df - 1)/2)/lnL0
finalmodelsummary

# Pick out best model with respect to R2McFaddenAdj
finaltotune.model <- finalbackward.model

# Tune by hand by removing insignificant parameters
finaltuned.model <- finaltotune.model
#<- glm(lowplasma ~ vituse  + betadiet + age + smokstat + quetelet, data = plasma)
AIC(finaltuned.model)
finaltuned_df <- finaltuned.model$rank-1
finaltuned_logLik <- logLik(finaltuned.model)[1]
finaltuned_R2McF <- 1 - finaltuned_logLik/lnL0
finaltuned_R2McF.adj <- 1 - (finaltuned_logLik - (finaltuned_df-1)/2)/lnL0
su# this did not improve the value of R2McFAdj....


# estimate p_i using all the different models:

pred.phat <- cbind(
  plasma,
  p.0 = predict(model.0, type = "response"),
  p.1 = predict(finaltuned.model, type = "response"),
  p.2 = predict(age.model, type = "response"),
  p.3 = predict(diet.model, type = "response"),
  p.4 = predict(background.model, type = "response"))
head(pred.phat)

# Confusion matrix for model 3 and model oslo####
# Calculate Y-hat using model 3 and model oslo.

pred.phat$yhat.final <- as.numeric(pred.phat$p.1 > 0.5)
pred.phat$yhat.age <- as.numeric(pred.phat$p.2 > 0.5)
pred.phat$yhat.diet <- as.numeric(pred.phat$p.3 > 0.5)
pred.phat$yhat.background <- as.numeric(pred.phat$p.4 > 0.5)



(row.01 <- table(plasma$lowplasma))

(col.01.final <- table(pred.phat$yhat.final))
(confusion.final <- table(pred.phat$lowplasma, pred.phat$yhat.final))
(spec.final <- confusion.final[1, 1] / row.01[1])
(sens.final <- confusion.final[2, 2] / row.01[2])
(accu.final <- sum(diag(confusion.final)) / sum(confusion.final))
(prec.final <- confusion.final[2, 2] / col.01.final[2])

(col.01.age <- table(pred.phat$yhat.age))
(confusion.age <- table(pred.phat$lowplasma, pred.phat$yhat.age))
(spec.age <- 0)
(sens.age <- confusion.age[2] / row.01[2])
(accu.age <- 235 / sum(confusion.age))
(prec.age <- confusion.age[2] / col.01.age[1])

(col.01.diet <- table(pred.phat$yhat.diet))
(confusion.diet <- table(pred.phat$lowplasma, pred.phat$yhat.diet))
(spec.diet <- confusion.diet[1, 1] / row.01[1])
(sens.diet <- confusion.diet[2, 2] / row.01[2])
(accu.diet <- sum(diag(confusion.diet)) / sum(confusion.diet))
(prec.diet <- confusion.diet[2, 2] / col.01.diet[2])


(col.01.background <- table(pred.phat$yhat.background))
(confusion.background <- table(pred.phat$lowplasma, pred.phat$yhat.background))
(spec.background <- confusion.background[1, 1] / row.01[1])
(sens.background <- confusion.background[2, 2] / row.01[2])
(accu.background <- sum(diag(confusion.background)) / sum(confusion.background))
(prec.background <- confusion.background[2, 2] / col.01.background[2])



# confidence interval for AUc:
install.packages("pROC")

library(pROC)
# Hosmer-Lemeshow goodness of fit test:
install.packages("ResourceSelection")

library(ResourceSelection)

##roc
# ROC-curves####
# Calculate for model 0 and 3.
(roc.0 <- roc(lowplasma ~ p.1, data = pred.phat))
# save the coordinates in a data frame for plotting.
roc.df.0 <- coords(roc.0, transpose = FALSE)
roc.df.0$model <- "Final"
roc.df.0

(roc.1 <- roc(lowplasma ~ p.2, data = pred.phat))
# save the coordinates in a data frame for plotting.
roc.df.1 <- coords(roc.1, transpose = FALSE)
roc.df.1$model <- "Age"
roc.df.1

(roc.2 <- roc(lowplasma ~ p.3, data = pred.phat))
# save the coordinates in a data frame for plotting.
roc.df.2 <- coords(roc.2, transpose = FALSE)
roc.df.2$model <- "Diet"
roc.df.2

(roc.3 <- roc(lowplasma ~ p.4, data = pred.phat))
# save the coordinates in a data frame for plotting.
roc.df.3 <- coords(roc.3, transpose = FALSE)
roc.df.3$model <- "Background"
roc.df.3

# Create the data for the Ideal model by hand:
roc.df.ideal <- data.frame(sensitivity = c(0, 1, 1),
                           specificity = c(1, 1, 0),
                           threshold = c(NA, NA, NA))
roc.df.ideal$model <- "ideal"

# Built-in function for plotting one Roc-curve
# Note that the x-axis is reversed!
# If we want the diagonal with geom_abline, it has to be reversed!
# Since both axes are 0-1, we want a square plot area:

roc.df <- rbind(roc.df.0, roc.df.1, roc.df.2, roc.df.3)

# Plot all the curves, in different colors:
ggplot(roc.df, aes(specificity, sensitivity,
                   color = model)) +
  geom_path(size = 1) +
  coord_fixed() +       # square plotting area
  scale_x_reverse() +   # Reverse scale on the x-axis!
  labs(title = "ROC-curves for all the models") +
  theme(text = element_text(size = 14))




# Plot the three ROC-curves:
# Use geom_path() instead of geom_line()
#
# For model 3 the curve is color coded according to
# the threshold. The color scheme is set by
# + scale_color_gradientn(colours = rainbow(5)) +
#
# Note that the x-axis is reversed!
# + scale_x_reverse()
# You could use 1 - spec instead.
# If we want the diagonal with geom_abline, it has to be reversed!
#
# Since both axes are 0-1, we want a square plot area:
# + coord_fixed()
#
ggplot(roc.df.0, aes(specificity, sensitivity)) +
  geom_path(aes(color = threshold), size = 2) +
  geom_path(data = roc.df.ideal, color = "black", size = 1) +
  geom_path(data = roc.df.0, color = "red", size = 1,
            linetype = "dashed") +
  geom_point(data = roc.df.0[I_max.3, ], color = "black", size = 3) +
  #  geom_abline(intercept = 1, slope = 1, linetype = "dashed") +
  scale_color_gradientn(colours = rainbow(5)) +
  coord_fixed() +       # square plotting area
  scale_x_reverse() +   # Reverse scale on the x-axis!
  labs(title = "ROC-curve for the final model",
       caption = "Black dot = optimal threshold") +
  theme(text = element_text(size = 14))


# AUC####
auc(roc.0)
# Confidence interval for AUC
(ci.0 <- ci(roc.0))

auc(roc.1)
# Confidence interval for AUC
(ci.1 <- ci(roc.1))

auc(roc.2)
# Confidence interval for AUC
(ci.2 <- ci(roc.2))

auc(roc.3)
# Confidence interval for AUC
(ci.3 <- ci(roc.3))

#Collect for all models
(aucs <- 
    data.frame(
      model = c("0", "1", "2", "3"),
      auc = c(auc(roc.0), auc(roc.1), auc(roc.2), auc(roc.3)),
      lwr = c(ci(roc.0)[1], ci(roc.1)[1],
              ci(roc.2)[1], ci(roc.3)[1]),
      upr = c(ci(auc(roc.0))[3], ci(auc(roc.1))[3],
              ci(auc(roc.2))[3], ci(auc(roc.3))[3])))

# Compare the AUC for the models:

#Age vs background
roc.test(roc.1, roc.3)

#Age vs diet
roc.test(roc.1, roc.2)

#Age vs final
roc.test(roc.1, roc.0)

#Background vs diet
roc.test(roc.3, roc.2)

#Background vs final
roc.test(roc.3, roc.0)


#Diet vs Final
roc.test(roc.2, roc.0)





#roc.0 is final
#roc.1 is age
#roc.2 is diet
#roc.3 is background


## 3c Find the optimal p value ####
# Create the data for the Ideal model by hand:
roc.df.ideal <- data.frame(sensitivity = c(0, 1, 1),
                           specificity = c(1, 1, 0),
                           threshold = c(NA, NA, NA))
roc.df.ideal$model <- "ideal"


# Built-in function for plotting one Roc-curve
# Note that the x-axis is reversed!
# If we want the diagonal with geom_abline, it has to be reversed!
# Since both axes are 0-1, we want a square plot area:
# + coord_fixed()
ggroc(roc.0) +
  geom_abline(intercept = 1, slope = 1, linetype = "dashed") +
  coord_fixed() +
  labs(title = "ROC-curve for final model")

# experiment with different values of "cutoff.p" to find the
# one that gives the optimal combination of sens and spec.


#Final model
cutoff.p <- 0.69
roc.df.0[roc.df.0$sensitivity > cutoff.p & 
           roc.df.0$specificity > cutoff.p, ]
##
(I_max.0 <- which(roc.df.0$sensitivity > cutoff.p & 
                    roc.df.0$specificity > cutoff.p))
#Age model
cutoff.p <- 0.55
roc.df.1[roc.df.1$sensitivity > cutoff.p & 
           roc.df.1$specificity > cutoff.p, ]


#Diet model
cutoff.p <- 0.6749
roc.df.2[roc.df.2$sensitivity > cutoff.p & 
           roc.df.2$specificity > cutoff.p, ]

#Background model
cutoff.p <- 0.655
roc.df.3[roc.df.3$sensitivity > cutoff.p & 
           roc.df.3$specificity > cutoff.p, ]



#Compute new confusion matrices
pred.phat$yhat.final <- as.numeric(pred.phat$p.1 > 0.7496528)
pred.phat$yhat.age <- as.numeric(pred.phat$p.2 > 0.7624026)
pred.phat$yhat.diet <- as.numeric(pred.phat$p.3 > 0.7507085)
pred.phat$yhat.background <- as.numeric(pred.phat$p.4 > 0.7184900)



(row.01 <- table(plasma$lowplasma))

(col.01.final <- table(pred.phat$yhat.final))
(confusion.final <- table(pred.phat$lowplasma, pred.phat$yhat.final))
(spec.final <- confusion.final[1, 1] / row.01[1])
(sens.final <- confusion.final[2, 2] / row.01[2])
(accu.final <- sum(diag(confusion.final)) / sum(confusion.final))
(prec.final <- confusion.final[2, 2] / col.01.final[2])

(col.01.age <- table(pred.phat$yhat.age))
(confusion.age <- table(pred.phat$lowplasma, pred.phat$yhat.age))
(spec.age <- confusion.age[1, 1] / row.01[1])
(sens.age <- confusion.age[2, 2] / row.01[2])
(accu.age <- sum(diag(confusion.age)) / sum(confusion.age))
(prec.age <- confusion.age[2, 2] / col.01.age[2])

(col.01.diet <- table(pred.phat$yhat.diet))
(confusion.diet <- table(pred.phat$lowplasma, pred.phat$yhat.diet))
(spec.diet <- confusion.diet[1, 1] / row.01[1])
(sens.diet <- confusion.diet[2, 2] / row.01[2])
(accu.diet <- sum(diag(confusion.diet)) / sum(confusion.diet))
(prec.diet <- confusion.diet[2, 2] / col.01.diet[2])


(col.01.background <- table(pred.phat$yhat.background))
(confusion.background <- table(pred.phat$lowplasma, pred.phat$yhat.background))
(spec.background <- confusion.background[1, 1] / row.01[1])
(sens.background <- confusion.background[2, 2] / row.01[2])
(accu.background <- sum(diag(confusion.background)) / sum(confusion.background))
(prec.background <- confusion.background[2, 2] / col.01.background[2])


## 3d Goodness of fit tests ####


# Hosmer-Lemeshow-test####
# Illustrating example: plot in sorted p-order
# order(variable) gives the ranks for the values in variable.
# It can then be used to sort the data frame:
pred.sort <- pred.phat[order(pred.phat$p.0), ]
pred.sort$rank <- seq(1, nrow(pred.sort))
head(pred.sort)

# HL using hoslem.test####
# p+1:



## The final model
length(finaltuned.model$coefficients)
# so we need g > 10
# while the smallest expected value is at least approx 5:
# Allowing 4 here and have experimented with g:
g <- 30
(HL.1 <- hoslem.test(pred.sort$lowplasma, pred.sort$p.1, g ))
HL.1$expected


# Collect the data in a useful form for plotting:
(HL.df.1 <- data.frame(group = seq(1, g),
                       Obs0 = HL.1$observed[, 1],
                       Obs1 = HL.1$observed[, 2],
                       Exp0 = HL.1$expected[, 1],
                       Exp1 = HL.1$expected[, 2]))

ggplot(HL.df.1, aes(x = group)) +
  geom_line(aes(y = Obs0, linetype = "observed", color = "Y = 0"), size = 1) +
  geom_line(aes(y = Obs1, linetype = "observed", color = "Y = 1"), size = 1) +
  geom_line(aes(y = Exp0, linetype = "expected", color = "Y = 0"), size = 1) +
  geom_line(aes(y = Exp1, linetype = "expected", color = "Y = 1"), size = 1) +
  labs(title = "Final model: Observed and expected in each group",
       y = "number of observations") +
  scale_x_continuous(breaks = seq(1, g)) +
  theme(text = element_text(size = 14))

## The age model
length(age.model$coefficients)
# so we need g > 10
# while the smallest expected value is at least approx 5:
# Allowing 4 here and have experimented with g:
g <- 7
(HL.2 <- hoslem.test(pred.sort$lowplasma, pred.sort$p.2, g ))
HL.2$expected


# Collect the data in a useful form for plotting:
(HL.df.2 <- data.frame(group = seq(1, g),
                       Obs0 = HL.2$observed[, 1],
                       Obs1 = HL.2$observed[, 2],
                       Exp0 = HL.2$expected[, 1],
                       Exp1 = HL.2$expected[, 2]))

ggplot(HL.df.2, aes(x = group)) +
  geom_line(aes(y = Obs0, linetype = "observed", color = "Y = 0"), size = 1) +
  geom_line(aes(y = Obs1, linetype = "observed", color = "Y = 1"), size = 1) +
  geom_line(aes(y = Exp0, linetype = "expected", color = "Y = 0"), size = 1) +
  geom_line(aes(y = Exp1, linetype = "expected", color = "Y = 1"), size = 1) +
  labs(title = "Age model: Observed and expected in each group",
       y = "number of observations") +
  scale_x_continuous(breaks = seq(1, g)) +
  theme(text = element_text(size = 14))


## The Diet model
length(diet.model$coefficients)
# so we need g > 10
# while the smallest expected value is at least approx 5:
# Allowing 4 here and have experimented with g:
g <- 8
(HL.3 <- hoslem.test(pred.sort$lowplasma, pred.sort$p.3, g ))
HL.3$expected


# Collect the data in a useful form for plotting:
(HL.df.3 <- data.frame(group = seq(1, g),
                       Obs0 = HL.3$observed[, 1],
                       Obs1 = HL.3$observed[, 2],
                       Exp0 = HL.3$expected[, 1],
                       Exp1 = HL.3$expected[, 2]))

ggplot(HL.df.3, aes(x = group)) +
  geom_line(aes(y = Obs0, linetype = "observed", color = "Y = 0"), size = 1) +
  geom_line(aes(y = Obs1, linetype = "observed", color = "Y = 1"), size = 1) +
  geom_line(aes(y = Exp0, linetype = "expected", color = "Y = 0"), size = 1) +
  geom_line(aes(y = Exp1, linetype = "expected", color = "Y = 1"), size = 1) +
  labs(title = "Diet model: Observed and expected in each group",
       y = "number of observations") +
  scale_x_continuous(breaks = seq(1, g)) +
  theme(text = element_text(size = 14))

## The Background model
length(background.model$coefficients)
# so we need g > 10
# while the smallest expected value is at least approx 5:
# Allowing 4 here and have experimented with g:
g <- 21
(HL.4 <- hoslem.test(pred.sort$lowplasma, pred.sort$p.4, g ))
HL.4$expected


# Collect the data in a useful form for plotting:
(HL.df.4 <- data.frame(group = seq(1, g),
                       Obs0 = HL.4$observed[, 1],
                       Obs1 = HL.4$observed[, 2],
                       Exp0 = HL.4$expected[, 1],
                       Exp1 = HL.4$expected[, 2]))

ggplot(HL.df.4, aes(x = group)) +
  geom_line(aes(y = Obs0, linetype = "observed", color = "Y = 0"), size = 1) +
  geom_line(aes(y = Obs1, linetype = "observed", color = "Y = 1"), size = 1) +
  geom_line(aes(y = Exp0, linetype = "expected", color = "Y = 0"), size = 1) +
  geom_line(aes(y = Exp1, linetype = "expected", color = "Y = 1"), size = 1) +
  labs(title = "Background model: Observed and expected in each group",
       y = "number of observations") +
  scale_x_continuous(breaks = seq(1, g)) +
  theme(text = element_text(size = 14))






