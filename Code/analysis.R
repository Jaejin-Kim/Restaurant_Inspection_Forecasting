##MDML Final Project
##Shannon Kay, Jaejin Kim, & Jessica Spencer
##December 13, 2019
##Analysis

##Load required packages
require(tidyverse)
require(ranger)
require(class)
require(ROCR)
require(e1071)

##Read in data
restaurant_data <- read_csv("data/final_restaurant_data.csv")

#1. Create the Train/Test/Validation sets =====================
shuffle = restaurant_data
sample_size = nrow(shuffle)
set_prop = c(Train = 0.6, Validate = 0.2, Test = 0.2)
set_freq = diff(floor(sample_size*cumsum(c(0,set_prop))))
shuffle$set = sample(rep(names(set_prop),times=set_freq))

train = restaurant_data[shuffle$set == "Train",]
validation = restaurant_data[shuffle$set == "Validate",]
test = restaurant_data[shuffle$set=="Test",]

#2. Run the baseline model
baseline <- glm(outcome~ City, family ="binomial", data = train)
exp(summary(baselin)$coef)

#3. Run the Logistic Regression Model
model_glm = glm(outcome~ Neighborhood + Weekday + Month + critical_flag + Number_Violations + avg_AGI, family ="binomial", data = train)

#4. Run the Logistic Regression Model with L1 Regularization
train_x = model.matrix(outcome ~ Neighborhood + Weekday + Month +
                         critical_flag + Number_Violations + avg_AGI, train)[,-1]
validation_x = model.matrix(outcome ~ Neighborhood + Weekday + Month +
                              critical_flag + Number_Violations + avg_AGI, validation)[,-1]

model_l1 = glmnet(train_x, train$outcome, family = "binomial", alpha = 1, lambda = 0.01) #lasso
coef(model_l1)

#5. Run the Random Forest model
model_rf = ranger(outcome~ Neighborhood + Weekday + Month + critical_flag + Number_Violations + avg_AGI, data = train, num.trees = 1000, respect.unordered.factors = T)

#6. Calculate AUC
validation$pred.glm <- predict(model_glm, validation, type = 'response')
validation$pred.l1 <- predict(model_l1, validation, type = 'response')
validation$pred.rf <- predict(model_rf, validation, type = 'response')$predictions

rocr.pred.glm <- prediction(validation$pred.glm, validation$outcome)
perf.glm <- performance(rocr.pred.glm, "auc")
auc.glm <- perf.glm@y.values[[1]]

rocr.pred.l1 <- prediction(validation$pred.l1, validation$outcome)
perf.l1 <- performance(rocr.pred.l1, "auc")
auc.l1 <- perf.l1@y.values[[1]]

rocr.pred.rf <- prediction(validation$pred.rf, validation$outcome)
perf.rf <- performance(rocr.pred.rf, "auc")
auc.rf <- perf.rf@y.values[[1]]

auc.glm
auc.l1
auc.rf

perf1 <- performance(rocr.pred.glm, "tpr", "fpr")
perf2 <- performance(rocr.pred.l1, "tpr", "fpr")
perf3 <- performance(rocr.pred.rf, "tpr", "fpr")

#7. figure 1
png("figures/figure1.png")
plot(perf1, main = "Figure 1: Validation Set ROC Curves")
plot(perf2, add = T, col = 2)
plot(perf3, add = T, col = 3)
legend("bottomright", col = c(1,2,3), lty = 1,
       legend = c("Logistic Regression",
                  "Lasso Regularization",
                  "Random Forest"),
       cex= 0.75)
dev.off()

#8. Re-train the Random Forest model on train + validation set and calculate AUC
train_valid = rbind(train, validation[,-c(16:18)])

model_rf2 = ranger(outcome~ Neighborhood + Weekday + Month + critical_flag + Number_Violations + avg_AGI, data = train_valid, num.trees = 1000, respect.unordered.factors = T)

test$pred <- predict(model_rf2, test, type = 'response')$predictions

rocr.pred <- prediction(test$pred, test$outcome)
perf <- performance(rocr.pred, "auc")
auc <- perf@y.values[[1]]
auc

#9. figure 2
png("figures/figure2.png")
plot(performance(rocr.pred, "tpr", "fpr"), main = "Figure2: Test Set ROC Curve: Random Forest")
dev.off()