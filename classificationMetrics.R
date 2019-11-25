# 1. Import dataset
trainData <- read.csv('https://raw.githubusercontent.com/selva86/datasets/master/breastcancer_training.csv')
testData <- read.csv('https://raw.githubusercontent.com/selva86/datasets/master/breastcancer_test.csv')

# 2. Build Logistic Model
logitmod <- glm(Class ~ Cl.thickness + Cell.size + Cell.shape, family = "binomial", data=trainData)
logitmod

# 3. Predict on testData
pred <- predict(logitmod, newdata = testData, type = "response")
pred
summary(pred)

# 4. If p > .5, then Class is 1 else 0
y_pred <- ifelse(pred > 0.5, 1, 0)
y_act <- testData$Class

# 5. Accuracy
mean(y_pred == y_act)  # 94%


####### Confusion Matrix ########
library(caret)
caret::confusionMatrix(as.factor(y_pred), as.factor(y_act), positive="1", mode="everything")
