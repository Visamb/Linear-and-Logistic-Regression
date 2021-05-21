## Project 2 #### 
library(ggplot2)
library(MASS)

## Load the data #### 
plasma <- read.delim("Data/plasma.txt")
summary(plasma)


## Turn the categorical variables into factor variables and pick reference categories ####
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


quantile(plasma$betaplasma)

a <- 105
b <- 170

summary(plasma$plasmacat)


# Create the variables  ####
plasma$plasmacat <- cut(plasma$betaplasma, breaks = c(-1, a, b, 2000))


#Comments: Make an age model, a diet model, a background model and a final model.

#Null model
(null.model <- polr(plasmacat ~ 1, data = plasma))

##Age model ####
(age.model <- polr(plasmacat ~ age, data = plasma))
cooks.distance(age.model)

##Diet model ####
upper.model <- polr(plasmacat ~ vituse + calories + fat + fiber + alcohol + cholesterol + betadiet, data = plasma)

diet.model <- step(null.model,
                   scope = list(upper = upper.model),
                   direction = "forward")

#Present the beta-estimates
diet.model$coefficients
exp(diet.model$coefficients)
exp(confint(diet.model))

## Background model ####
upper.model <- polr(plasmacat ~ age + sex + smokstat + quetelet, data = plasma)

background.model <- step(null.model,
                         scope = list(upper = upper.model),
                         direction = "forward")

# Summarize parameter estimates and such
summary(background.model)
background.model$coefficients
confint(background.model)
exp(background.model$coefficients)
exp(confint(background.model))


## The final model ####


finalmax.model <- polr(plasmacat ~ vituse + calories + fat + fiber + alcohol + cholesterol + betadiet
                      + age + sex + smokstat + quetelet, data = plasma)

# Test forward selection
finalforward.model <- step(null.model,
                           scope = list(upper = finalmax.model),
                           direction = "forward")

# Backward selection
finalbackward.model <- step(finalmax.model,
                            scope = list(lower = null.model),
                            direction = "backward")

AICs <- AIC(finalforward.model, finalbackward.model)
AICs

final.model <- finalforward.model
summary(final.model)

#beta-estimates
cbind(beta = final.model$coefficients, 
      expbeta = exp(final.model$coefficients),
      exp(confint(final.model)))

#zeta-extimates
cbind(zeta = final.model$zeta, 
      expzeta = exp(final.model$zeta))

#Estimated probabilities
predict(final.model,type = "prob")

predict(final.model,type="class")

model.final <- final.model
model.null <- null.model
model.full <- finalmax.model


## Information and tests ####
# aic/bic, R2####
# deviance
model.final$deviance
# total number of parameters (beta and zeta)
model.final$edf
model.diet<-diet.model
model.background<-background.model
model.age<-age.model

info <- cbind(aic = AIC(model.null, model.final, model.full, model.diet, model.background,model.age),
              bic = BIC(model.null, model.final, model.full, model.diet, model.background,model.age),
              R2D = 100*c(1 - model.null$deviance/model.null$deviance, 
                          1 - model.final$deviance/model.null$deviance, 
                          1 - model.full$deviance/model.null$deviance,
                          1 - model.diet$deviance/model.null$deviance,
                          1 - model.background$deviance/model.null$deviance,
                          1 - model.age$deviance/model.null$deviance),
              R2D.adj = 100*c(1 - (model.null$deviance + model.null$edf - model.null$edf)/
                                model.null$deviance, 
                              1 - (model.final$deviance + model.final$edf - model.null$edf)/
                                model.null$deviance, 
                              1 - (model.full$deviance + model.full$edf - model.null$edf)/
                                model.null$deviance,
                              1 - (model.diet$deviance + model.diet$edf - model.null$edf)/
                                model.null$deviance, 
                              1 - (model.background$deviance + model.background$edf - model.null$edf)/
                                model.null$deviance,
                              1 - (model.age$deviance + model.age$edf - model.null$edf)/
                                model.null$deviance))
round(info, digits = 1)

#R2ADJ higher for final


#LR-test comparing nested models
anova(null.model,final.model)
anova(final.model,model.full)


#Confusion matrix####

pred.final <- cbind(plasma,
                    yhat = predict(model.final))
yhat = predict(model.final)

length(yhat)
(conf.matrix <- table(pred.final$plasmacat, pred.final$yhat))
table(pred.final$plasmacat)
table(pred.final$yhat)
sum(conf.matrix)

(sens <- 100*(diag(conf.matrix)/table(pred.final$plasmacat)))
(prec <- 100*(diag(conf.matrix)/table(pred.final$yhat)))
(acc <- 100*sum(diag(conf.matrix)/sum(conf.matrix)))


model.age<-age.model
model.diet<-diet.model
model.background<-background.model

pred.phat <- cbind(
  plasma,
  p.0 = predict(model.null, type = "prob"),
  p.1 = predict(model.age, type = "prob"),
  p.2 = predict(model.diet, type = "prob"),
  p.3 = predict(model.background, type = "prob"),
  p.4 = predict(model.final, type = "prob"))
head(pred.phat)



library(pROC)
# Hosmer-Lemeshow goodness of fit test:
install.packages("ResourceSelection")

library(ResourceSelection)

##roc
# ROC-curves####
# Calculate for model 0 and 3.
(roc.0 <- roc(plasmacat ~ p.1.(-1,90], data = pred.phat))
# save the coordinates in a data frame for plotting.
roc.df.0 <- coords(roc.0, transpose = FALSE)
roc.df.0$model <- "age"
roc.df.0

(roc.1 <- roc(plasmacat ~ p.2, data = pred.phat))
# save the coordinates in a data frame for plotting.
roc.df.1 <- coords(roc.1, transpose = FALSE)
roc.df.1$model <- "diet"
roc.df.1

(roc.2 <- roc(plasmacat ~ p.3, data = pred.phat))
# save the coordinates in a data frame for plotting.
roc.df.2 <- coords(roc.2, transpose = FALSE)
roc.df.2$model <- "background"
roc.df.2

(roc.3 <- roc(plasmacat ~ p.4, data = pred.phat))
# save the coordinates in a data frame for plotting.
roc.df.3 <- coords(roc.3, transpose = FALSE)
roc.df.3$model <- "final"
roc.df.3


roc.df <- rbind(roc.df.0, roc.df.1, roc.df.2, roc.df.3)

# Plot all the curves, in different colors:
ggplot(roc.df, aes(specificity, sensitivity,
                   color = model)) +
  geom_path(size = 1) +
  coord_fixed() +       # square plotting area
  scale_x_reverse() +   # Reverse scale on the x-axis!
  labs(title = "ROC-curves for all the models") +
  theme(text = element_text(size = 14))




