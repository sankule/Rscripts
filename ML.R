# Sectionwise merged code in R from course named Machine Learning A-Z by SuperData Science Team 
# superdatascience.com/machine-learning/


                                                          ###### Preprocessing #####
library(dplyr)
library(ggplot2)                                                          
                                                          
                                                          
                                                          ###  Missing variable ###
dataset = read.csv('Data.csv')
# Taking care of missing data
dataset$Age = ifelse(is.na(dataset$Age),
                     ave(dataset$Age, FUN = function(x) mean(x, na.rm = TRUE)),
                     dataset$Age)
dataset$Salary = ifelse(is.na(dataset$Salary),
                        ave(dataset$Salary, FUN = function(x) mean(x, na.rm = TRUE)),
                        dataset$Salary)


                                              ### Categorical Variable ###
dataset$Country # changing factors from char to int
dataset$Country = factor(dataset$Country,
                         levels = c('France', 'Spain', 'Germany'),
                         labels = c(1, 2, 3))
dataset$Purchased = factor(dataset$Purchased,
                           levels = c('No', 'Yes'),
                           labels = c(0, 1))


          # reconverting numeric to factor - char
          dataset$State
          # reference for backtrack to original 
          levels(dataset$State)
          stateRef <- as.data.frame(levels(dataset$State)) %>% mutate(id = row_number()) 
          stateRef
          dataset$StateNew <- as.numeric(dataset$State)
          dataset$StateNew
          # reverting back to original
          dataset$StateNew <- mapvalues(dataset$StateNew, from = stateRef$id, to = as.character(stateRef$`levels(dataset$State)`))
          # check 
          summary(dataset$StateNew == dataset$State)


                                                    ####  Test and Train split ###

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$DependentVariable, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)



                                                        ###### ~~~ Regression ~~~ #####
# Numeric Predictions - duh!
library(ggplot2)
library(data.table)
library(dplyr)
setwd("~/SWARIT/Udemy/Machine Learning A-Z Template Folder/")


                                            ##### Simple Linear Regression #####
# Assumptions of Linear Regression 
# Linearity / Homoscedasticity / Multivariate Normality / Idependence of errors / Lack of multicollinearity 



# Importing the dataset
dataset = read.csv('Part 2 - Regression/Section 4 - Simple Linear Regression/Simple_Linear_Regression/Simple_Linear_Regression/Salary_Data.csv')

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Salary, SplitRatio = 2/3)
summary(split)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
training_set = as.data.frame(scale(training_set))
test_set = as.data.frame(scale(test_set))

# Fitting Simple Linear Regression to the Training set
regressor = lm(formula = Salary ~ YearsExperience,
               data = training_set)
summary(regressor)

# Predicting the Test set results
y_pred = predict(regressor, newdata = test_set)

# Visualising the Training set results
library(ggplot2)
ggplot() +
  geom_point(aes(x = training_set$YearsExperience, y = training_set$Salary),
             colour = 'red') +
  geom_line(aes(x = training_set$YearsExperience, y = predict(regressor, newdata = training_set)),
            colour = 'blue') +
  ggtitle('Salary vs Experience (Training set)') +
  xlab('Years of experience') +
  ylab('Salary')

# Visualising the Test set results
library(ggplot2)
ggplot() +
  geom_point(aes(x = test_set$YearsExperience, y = test_set$Salary),
             colour = 'red') +
  geom_line(aes(x = training_set$YearsExperience, y = predict(regressor, newdata = training_set)),
            colour = 'blue') +
  ggtitle('Salary vs Experience (Test set)') +
  xlab('Years of experience') +
  ylab('Salary')

                                        #### Multiple Linear Regression #####

#####  ** Model Building #####

# QnA
# The lower the p-value between two variables, the more likely they are related and less likely their relation is by chance

# 1. all-in approach for all predictors

# 2. Backward Elimination Model: starting with all-in approach and check predictors p-value, we will eliminate predictors with  
#    p-value higher (high p-value => bad predictor) than the significance level and repeat the process with remaning predictors 

# 3. Forward Selection Model: fit all possible simple linar regression model with all predictors 
#    then we select the predictor with the lowest p-value (selecting just 1 predictor). then we add this variable to all
#    other predictors before and now do linear regression with 2 variables with 1 being the selected one.
#    now select the predictos with lowest p-value again and repeat for 3,4,5--n variable set untill any predictor has
#    p-value greater than the significance level and we will keep the previous model (where all predictors were significant)

# 4. Bi-Directional Elimination: combination of the above two methods 
#     


# Importing the dataset
dataset = read.csv('Part 2 - Regression/Section 5 - Multiple Linear Regression/Multiple_Linear_Regression/Multiple_Linear_Regression/50_Startups.csv')
head(dataset)
str(dataset)

# Encoding categorical data
dataset$State = factor(dataset$State, levels = c('New York', 'California', 'Florida'), labels = c(1, 2, 3))

              # reconverting numeric to factor - char
              dataset$State
              # reference for backtrack to original 
              levels(dataset$State)
              stateRef <- as.data.frame(levels(dataset$State)) %>% mutate(id = row_number()) 
              stateRef
              dataset$StateNew <- as.numeric(dataset$State)
              dataset$StateNew
              # reverting back to original
              dataset$StateNew <- mapvalues(dataset$StateNew, from = stateRef$id, to = as.character(stateRef$`levels(dataset$State)`))
              # check 
              summary(dataset$StateNew == dataset$State)


# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Profit, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
# training_set = scale(training_set)
# test_set = scale(test_set)

# Fitting Multiple Linear Regression to the Training set
regressor = lm(formula = Profit ~ .,
               data = training_set)

# Predicting the Test set results
y_pred = predict(regressor, newdata = test_set)


# automatic implementation of Backward Elimination
backwardElimination <- function(x, sl) {
  numVars = length(x)
  for (i in c(1:numVars)){
    regressor = lm(formula = Profit ~ ., data = x)
    maxVar = max(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"])
    if (maxVar > sl){
      j = which(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"] == maxVar)
      x = x[, -j]
    }
    numVars = numVars - 1
  }
  return(summary(regressor))
}

SL = 0.05
dataset = dataset[, c(1,2,3,4,5)]
backwardElimination(training_set, SL)



                                        #### Polynomial Regression #####

# Importing the dataset
dataset = read.csv('Position_Salaries.csv')
dataset = dataset[2:3]
dataset
# Splitting the dataset into the Training set and Test set
# # install.packages('caTools')
# library(caTools)
# set.seed(123)
# split = sample.split(dataset$Salary, SplitRatio = 2/3)
# training_set = subset(dataset, split == TRUE)
# test_set = subset(dataset, split == FALSE)

# Feature Scaling
# training_set = scale(training_set)
# test_set = scale(test_set)

# Fitting Linear Regression to the dataset
lin_reg = lm(formula = Salary ~ .,
             data = dataset)

# Fitting Polynomial Regression to the dataset
dataset$Level2 = dataset$Level^2
dataset$Level3 = dataset$Level^3
dataset$Level4 = dataset$Level^4
poly_reg = lm(formula = Salary ~ .,
              data = dataset)

# Visualising the Linear Regression results
# install.packages('ggplot2')
library(ggplot2)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = dataset$Level, y = predict(lin_reg, newdata = dataset)),
            colour = 'blue') +
  ggtitle('Truth or Bluff (Linear Regression)') +
  xlab('Level') +
  ylab('Salary')

# Visualising the Polynomial Regression results
# install.packages('ggplot2')
library(ggplot2)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = dataset$Level, y = predict(poly_reg, newdata = dataset)),
            colour = 'blue') +
  ggtitle('Truth or Bluff (Polynomial Regression)') +
  xlab('Level') +
  ylab('Salary')

# Visualising the Regression Model results (for higher resolution and smoother curve)
# install.packages('ggplot2')
library(ggplot2)
x_grid = seq(min(dataset$Level), max(dataset$Level), 0.1)
x_grid
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = x_grid, y = predict(poly_reg,
                                        newdata = data.frame(Level = x_grid,
                                                             Level2 = x_grid^2,
                                                             Level3 = x_grid^3,
                                                             Level4 = x_grid^4))),
            colour = 'blue') +
  ggtitle('Truth or Bluff (Polynomial Regression)') +
  xlab('Level') +
  ylab('Salary')

# Predicting a new result with Linear Regression
predict(lin_reg, data.frame(Level = 6.5))

# Predicting a new result with Polynomial Regression
predict(poly_reg, data.frame(Level = 6.5,
                             Level2 = 6.5^2,
                             Level3 = 6.5^3,
                             Level4 = 6.5^4))



                                              ##### SVR #####

# Importing the dataset
dataset = read.csv('Part 2 - Regression/Section 7 - Support Vector Regression (SVR)/SVR/SVR/Position_Salaries.csv')
dataset
dataset = dataset[2:3]

# Fitting SVR to the dataset
# install.packages('e1071')
library(e1071)
regressor = svm(formula = Salary ~ .,
                data = dataset,
                type = 'eps-regression',
                kernel = 'radial')

                                      # eps-regression  => for numeric output
                                      # radial kernel => for non-linearity

# Predicting a new result
y_pred = predict(regressor, data.frame(Level = 6.5))

# Visualising the SVR results
# install.packages('ggplot2')
library(ggplot2)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = dataset$Level, y = predict(regressor, newdata = dataset)),
            colour = 'blue') +
  ggtitle('Truth or Bluff (SVR)') +
  xlab('Level') +
  ylab('Salary')

# CEO salary being considered outlier by the model is not considered in the prediction

# Visualising the SVR results (for higher resolution and smoother curve)
# install.packages('ggplot2')
library(ggplot2)
# sequence generated for plot 
x_grid = seq(min(dataset$Level), max(dataset$Level), 0.1)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = x_grid, y = predict(regressor, newdata = data.frame(Level = x_grid))),
            colour = 'blue') +
  ggtitle('Truth or Bluff (SVR)') +
  xlab('Level') +
  ylab('Salary')



                                    #### Decision Trees ####


# CART => Classification And Regression Trees

# lets say we have a scatterplot of X1 and X2. Decision trees algorithm will split the scatter into different 
# sections(branches) one at a time based on some condition (X2 < 20 for example). 
# splitting criteria is decided by MATHEMATICAL INFORMATION ENTROPY

# In a nutshell: the split is decided by whether the split is increasing the amount of information of information 
# we have about the points is it adding some values to groups. algorithm knows when to stop when there is 
# no further information need to be added by splitting the leaf any further. 
# for example: if the leaf has less that 5% of the information 
# Final leaves are called TERMINAL LEAVES

# >>>>  SPLITTING CRITERIA <<<<<< 

# The core algorithm for building decision trees called ID3 by J. R. Quinlan which employs a "top-down", "greedy search" 
# through the space of possible branches with no "backtracking". The ID3 algorithm can be used to construct a 
# decision tree for regression by replacing Information Gain with Standard Deviation Reduction.

# A decision tree is built top-down from a root node and involves partitioning the data into subsets that contain 
# instances with similar values (homogenous). We use standard deviation to calculate the homogeneity of a numerical sample. 
# If the numerical sample is completely homogeneous its standard deviation is zero.

# A decision tree is built top-down from a root node and involves partitioning the data into subsets that contain
# instances with similar values (homogenous). We use standard deviation to calculate the homogeneity of a numerical sample. 
# If the numerical sample is completely homogeneous its standard deviation is zero.

#     Standard Deviation (S) is for tree building (branching).
#     Coefficient of Deviation (CV) is used to decide when to stop branching. We can use Count (n) as well.
#     Average (Avg) is the value in the leaf nodes.

# Standard deviation for two attributes (target and predictor):
#   S(T,X) = SIGMA { p(c)*S(c)} for c in X
#   where,
#   S is standard deviation, T is Target, X is predictor


# Standard Deviation Reduction (SDR)		
#   The standard deviation reduction is based on the decrease in standard deviation after a dataset is split on an attribute. 
#   Constructing a decision tree is all about finding attribute that returns the highest standard deviation
#   reduction (i.e., the most homogeneous branches).
#   SDR(T,X) = S(T) - S(T,X)

# Feature Scaling is not required in decision trees as it is not based on euclidean distances like other models

## >>>> STEPS 
# Step 1: The standard deviation of the target is calculated.
# Step 2: The dataset is then split on the different attributes. The standard deviation for each branch is calculated. 
#         The resulting standard deviation is subtracted from the standard deviation before the split. The result 
#         is the standard deviation reduction.

# Step 3: The attribute with the largest standard deviation reduction is chosen for the decision node. 

# Step 4a: The dataset is divided based on the values of the selected attribute. This process is run recursively on the 
# non-leaf branches, until all data is processed.

# In practice, we need some termination criteria. For example, when coefficient of deviation (CV) for a branch becomes
# smaller than a certain threshold (e.g., 10%) and/or when too few instances (n) remain in the branch (e.g., 3). 		

# Step 4b: The related leaf node gets the average of the subset.

# https://en.wikipedia.org/wiki/ID3_algorithm

# Importing the dataset
dataset = read.csv('Part 2 - Regression/Section 8 - Decision Tree Regression/Decision_Tree_Regression/Decision_Tree_Regression/Position_Salaries.csv')
dataset = dataset[2:3]

# Fitting Decision Tree Regression to the dataset
# install.packages('rpart')
library(rpart)
regressor = rpart(formula = Salary ~ .,
                  data = dataset,
                  control = rpart.control(minsplit = 1))

# Predicting a new result with Decision Tree Regression
y_pred = predict(regressor, data.frame(Level = 6.5))

# Visualising the Decision Tree Regression results (higher resolution)
# install.packages('ggplot2')
library(ggplot2)
x_grid = seq(min(dataset$Level), max(dataset$Level), 0.01)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = x_grid, y = predict(regressor, newdata = data.frame(Level = x_grid))),
            colour = 'blue') +
  ggtitle('Truth or Bluff (Decision Tree Regression)') +
  xlab('Level') +
  ylab('Salary')

# Plotting the tree
plot(regressor)
text(regressor)


                                                  ###### Random Forest Regression #######


# Difference between Decision Trees and Random Forests:

# The superficial answer is that Random Forest (RF) is a collection of Decision Trees (DT).
# However, there is more to this than meets the eye. One problem that might occur with one big (deep) single DT is that it can 
# overfit. That is the DT can “memorize” the training set the way a person might memorize an Eye Chart.
# The point of RF is to prevent overfitting. It does this by creating random subsets of the features and building smaller (shallow) 
# trees using the subsets and then it combines the subtrees.

# The Decision tree classifiers uses greedy approach hence an attribute chooses at first step can’t be used anymore which can give 
# better classification if used in later steps. Also it overfit the training data which can give poor results for unseen data. 
# So, to overcome this limitation ensemble model is used. In ensemble model results from different models are combined. The result 
# obtained from an ensemble model is usually better than the result from any one of individual models.
# Random Forests is an ensemble classifier which uses many decision tree models to predict the result. A different subset of
# training data is selected, with replacement to train each tree. 

# A decision tree is built using the whole dataset considering all features,but in random forests a fraction of the number of rows is 
# selected at random and a particular number of features are selected at random to train on and a decision tree is built on this subset.


# Importing the dataset
dataset = read.csv('Part 2 - Regression/Section 9 - Random Forest Regression/Random_Forest_Regression/Random_Forest_Regression/Position_Salaries.csv')
dataset = dataset[2:3]

# same as decision trees, as the models is not dependent on the euclidean distances we will not be doing FEATURE SCALING 

# Fitting Random Forest Regression to the dataset
# install.packages('randomForest')
library(randomForest)
set.seed(1234)
regressor = randomForest(x = dataset[-2],
                         y = dataset$Salary,
                         ntree = 100)
                        # 100 trees     
summary(regressor)
regressor

# Predicting a new result with Random Forest Regression
y_pred = predict(regressor, data.frame(Level = 6.5))

# Visualising the Random Forest Regression results (higher resolution)
# install.packages('ggplot2')
library(ggplot2)
# for granular plot
x_grid = seq(min(dataset$Level), max(dataset$Level), 0.01)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = x_grid, y = predict(regressor, newdata = data.frame(Level = x_grid))),
            colour = 'blue') +
  ggtitle('Truth or Bluff (Random Forest Regression)') +
  xlab('Level') +
  ylab('Salary')

# If you encounter => Error in .Call.graphics(C_palette2, .Call(C_palette2, NULL)) : invalid graphics state
# try running dev.off() and the plot again


                                          #### Evaluating Regression Model Performance #####

                                            ### 1. R-squared ####

# residual sum of squares - RSS ->  SUM{yi - predicted(yi)^2}
# total sum of sqaures - TSS ->  SUM{yi - mean(y)^2}

# Rsquare =  1 - (RSS/TSS)
# Rsqaure close to 1 indicates good fit

                                            ### 2. Adjusted R-squared ####
# WHY?
# R-squared cannot determine whether the coefficient estimates and predictions are biased, which is why you must assess the residual plots
# Adjusted R-square penalizes the model for adding variables which do not improve your existing model.
# adjusted R squared  = 1 - {(1-R2)*(N-1)/(N-p-1)}
# where, 
# R2 -> sample Rsquared
# p -> Number of Predictors
# N -> Sample size/number of training samples 

# RESIDUAL PLOTS
# residuals should not contain any predictive information.


                                                    ### Pros & Cons ####

        # Regression Model Pros Cons
# PRO: Linear Regression Works on any size of dataset, gives informations about relevance of features 
# CON: The Linear Regression Assumptions

        # Polynomial Regression 
# PRO: Works on any size of dataset, works very well on non linear problems
# CON: Need to choose the right polynomial degree for a good bias/variance tradeoff

        # SVR 
# PRO: Easily adaptable, works very well on non linear problems, not biased by outliers
# CON: Compulsory to apply feature scaling, not well known, more difficult to understand
 
        # Decision Tree Regression 
# PRO: Interpretability, no need for feature scaling, works on both linear / nonlinear problems
# CON: Poor results on too small datasets, overfitting can easily occur

        # Random Forest Regression 
# PRO: Powerful and accurate, good performance on many problems, including non linear
# CON: No interpretability, overfitting can easily occur, need to choose the number of trees



# each model is composed of two types of parameters:
# 1. the parameters that are learnt, for example the coefficients in Linear Regression,
# 2. the hyperparameters

# The hyperparameters are the parameters that are not learnt and that are fixed values inside the model equations. 
# For example, the regularization parameter lambda or the penalty parameter C are hyperparameters. So far we used the 
# default value of these hyperparameters, and we haven't searched for their optimal value so that your model reaches 
# even higher performance. Finding their optimal value is exactly what Parameter Tuning is about.


                                            #### Regularization ####
# LASSO / RIDGE / ELASTICNET # 

# WHY ?
# Regularization, significantly reduces the variance of the model, without substantial increase in its bias.
# The GRADIENT DECSENT ALGORITHM that is applied on minimizing the COST FUNCTION

# This is a form of regression, that constrains/ regularizes or shrinks the coefficient estimates towards zero. 
# In other words, this technique discourages learning a more complex or flexible model, so as to avoid the risk ofoverfitting.

# the tuning parameter λ, used in the regularization techniques, controls the impact on bias and variance.
# As the value of λ rises, it reduces the value of coefficients and thus reducing the variance. Till a point, this increase in λ is 
# beneficial as it is only reducing the variance(hence avoiding overfitting), without loosing any important properties in the data.
# But after certain value, the model starts loosing important properties, giving rise to bias in the model and thus underfitting. 
# Therefore, the value of λ should be carefully selected.



                                                  ######  ~~~ Classification ~~~ #####

library(ggplot2)
library(data.table)
library(dplyr)
setwd("~/SWARIT/Udemy/Machine Learning A-Z Template Folder/Part 3 - Classification/")

                                                  #### Logistic Regression #####

# Importing the dataset
dataset = read.csv('Section 14 - Logistic Regression/Logistic_Regression/Logistic_Regression/Social_Network_Ads.csv')
head(dataset)
dataset = dataset[3:5]
# Encoding the target feature as factor
dataset$Purchased = factor(dataset$Purchased, levels = c(0, 1))

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
training_set[-3] = scale(training_set[-3])
test_set[-3] = scale(test_set[-3])

# Fitting Logistic Regression to the Training set
classifier = glm(formula = Purchased ~ .,
                 family = binomial,
                 data = training_set)

classifier
# Predicting the Test set results
prob_pred = predict(classifier, type = 'response', newdata = test_set[-3])
y_pred = ifelse(prob_pred > 0.5, 1, 0)

# Making the Confusion Matrix
cm = table(test_set[, 3], y_pred > 0.5)

# Visualising the Training set results
# install.packages("ElemStatLearn")
library(ElemStatLearn)
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
prob_set = predict(classifier, type = 'response', newdata = grid_set)
y_grid = ifelse(prob_set > 0.5, 1, 0)
plot(set[, -3],
     main = 'Logistic Regression (Training set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

# Visualising the Test set results
library(ElemStatLearn)
set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
prob_set = predict(classifier, type = 'response', newdata = grid_set)
y_grid = ifelse(prob_set > 0.5, 1, 0)
plot(set[, -3],
     main = 'Logistic Regression (Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))



### TEST SVM  ####
data(iris)
attach(iris)

## classification mode
# default with factor response:
model <- svm(Species ~ ., data = iris)
model
summary(model)

# alternatively the traditional interface:
x <- subset(iris, select = -Species)
y <- Species
model <- svm(x, y) 

print(model)
summary(model)

# test with train data
pred <- predict(model, x)
# (same as:)
pred <- fitted(model)

# Check accuracy:
table(pred, y)

# compute decision values and probabilities:
pred <- predict(model, x, decision.values = TRUE)
attr(pred, "decision.values")[1:4,]

# visualize (classes by color, SV by crosses):
plot(cmdscale(dist(iris[,-5])),
     col = as.integer(iris[,5]),
     pch = c("o","+")[1:150 %in% model$index + 1])

## try regression mode on two dimensions

# create data
x <- seq(0.1, 5, by = 0.05)
y <- log(x) + rnorm(x, sd = 0.2)

# estimate model and predict input values
m   <- svm(x, y)
new <- predict(m, x)

# visualize
plot(x, y)
points(x, log(x), col = 2)
points(x, new, col = 4)

## density-estimation

# create 2-dim. normal with rho=0:
X <- data.frame(a = rnorm(1000), b = rnorm(1000))
attach(X)

# traditional way:
m <- svm(X, gamma = 0.1)

# formula interface:
m <- svm(~., data = X, gamma = 0.1)
# or:
m <- svm(~ a + b, gamma = 0.1)

# test:
newdata <- data.frame(a = c(0, 4), b = c(0, 4))
predict (m, newdata)

# visualize:
plot(X, col = 1:1000 %in% m$index + 1, xlim = c(-5,5), ylim=c(-5,5))
points(newdata, pch = "+", col = 2, cex = 5)

# weights: (example not particularly sensible)
i2 <- iris
levels(i2$Species)[3] <- "versicolor"
summary(i2$Species)
wts <- 100 / table(i2$Species)
wts
m <- svm(Species ~ ., data = i2, class.weights = wts)




######  ~~~ TIME SERIES ~~~ #####

# ARIMA Model >>> 
# ARIMA(p,d,q). Here p,d, and q are the levels for each of the AR, I, and MA parts

# Integrated (I) – subtract time series with its lagged series to extract trends from the data

  # Differencing is one of the most commonly used mechanisms for extraction of trends. Here, 
  # the original series is subtracted from it’s lagged series. The residual data of most time series 
  # usually become trend-less after the first order differencing which is represented as ARIMA(0,1,0)
  # 1st Differencing (d=1)	 Y_{t}^{'}=Y_t -Y_{t-1} 
  # If the residual series still has a trend it is further differenced and is called 2nd order differencing. 
  # This trend-less series is called stationary on mean series i.e. mean or average value for series does not change over time
  # 2nd Differencing (d=2)	Y_{t}^{'}=Y_t-Y_{t-1}-(Y_{t-1}-Y_{t-2})=Y_{t}-2\times Y_{t-1}+Y_{t-2} 


# AutoRegressive (AR) – extract the influence of the previous periods’ values on the current period

  # As the name auto-regression suggests, here we try to extract the influence of the values of previous periods on 
  # the current period e.g. the influence of the September and October’s sales value on the November’s sales. 
  # This is done through developing a regression model with the time-lagged period values as independent or predictor variables.
  # AR model of order 1 i.e. p=1 or ARIMA(1,0,0) is represented by the following regression equation
  # Y_{t} = c + \phi_{1}Y_{t-1} + e_{t} 

# Moving Average (MA) – extract the influence of the previous period’s error terms on the current period’s error
  # MA involves finding relationships between the previous periods’ error terms on the current period’s error term. 
  # Y_{t} = c + e_t + \theta_{1}e_{t-1} + \theta_{2}e_{t-2} + .... + \theta_{q}e_{t-q}
  # MA model of order 1 i.e. q=1 or ARIMA(0,0,1) is represented by the following regression equation
  # Y_{t} = c + e_t + \theta_{1}e_{t-1} 


# ACF

  # A good way to distinguish between signal and noise is ACF (AutoCorrelation Function)
  # This is developed by finding the correlation between a series of its lagged values
  # for a significant correlation the vertical bars should fall outside the horizontal dotted lines


# R Script
data = read.csv('http://ucanalytics.com/blogs/wp-content/uploads/2015/06/Tractor-Sales.csv')
head(data)
summary(data)
data$Month.Year
# converting to TimeSeries Matrix - starting from JAN '03
data = ts(data[,2],start = c(2003,1),frequency = 12)
plot(data, xlab='Years', ylab = 'Tractor Sales')

# next thing to do is to make the series stationary - remove the upward trend through 1st order differencing the series
# Lagged Difference function
plot(diff(data),ylab='Differenced Tractor Sales')
# Series is not stationary on variance i.e. variation in the plot is increasing as we move towards the right of the chart.

# Log Transform
# One of the best ways to make a series stationary on variance is through transforming the original series through log transform

# starting again from step 1
plot(log10(data),ylab='Log (Tractor Sales)')
# step-2
plot(diff(log10(data)),ylab='Differenced Log (Tractor Sales)')
#  now this series looks stationary on both mean and variance. This also gives us the clue that I or integrated part 
# of our ARIMA model will be equal to 1 as 1st difference is making the series stationary.



# The best fit model is selected based on Akaike Information Criterion (AIC)
# and Bayesian Information Criterion (BIC) values
# The idea is to choose a model with minimum AIC and BIC values

# create autocorrelation factor (ACF) and partial autocorrelation factor (PACF) plots to identify patterns in the 
# above data which is stationary on both mean and variance. The idea is to identify presence of AR and MA components in the residuals.
# par(mfrow = c(1,2))
acf(ts(diff(log10(data))),main='ACF Tractor Sales')
pacf(ts(diff(log10(data))),main='PACF Tractor Sales')

# Since, there are enough spikes in the plots outside the insignificant zone (dotted horizontal lines) we can conclude 
# that the residuals are not random that means, there is still information that can be extracted from the data
# This implies that there is juice or information available in residuals to be extracted by AR and MA models. 
# Also, there is a seasonal component available in the residuals at the lag 12 (represented by spikes at lag 12). 
# This makes sense since we are analyzing monthly data that tends to have seasonality of 12 months because of 
# patterns in tractor sales.

# ACF plot: it is merely a bar chart of the coefficients of correlation between a time series and lags of itself. 
# The PACF plot is a plot of the partial correlation coefficients between the series and lags of itself.
# A partial autocorrelation is the amount of correlation between a variable and a lag of itself that is not
# explained by correlations at all lower-order-lags

# ARIMA 
# Auto arima function in forecast package in R helps us identify the best fit ARIMA model on the fly
library(forecast)
ARIMAfit = auto.arima(log10(data), approximation=FALSE,trace=FALSE)
summary(ARIMAfit)


# our model has I (or integrated) component equal to 1. This represents differencing of order 1. 
# There is additional differencing of lag 12 in the above best fit model. Moreover, the best fit model has MA value of order 1.
# Also, there is seasonal MA with lag 12 of order 1.


# predict tractor sales for next 3 years i.e. for 2015, 2016, and 2017 through the above model. 
pred = predict(ARIMAfit, n.ahead = 36)
# n.ahead = 36, meaning 36 unit (months is this case) ahead
pred
plot(data,type='l',xlim=c(2004,2018),ylim=c(1,1600),xlab = 'Year',ylab = 'Tractor Sales')
lines(10^(pred$pred),col='blue')
# 10^pred as they are log values 
# errors
lines(10^(pred$pred+2*pred$se),col='orange')
lines(10^(pred$pred-2*pred$se),col='orange')
# forecasted values of tractor sales in blue. Also, the range of expected error 
# (i.e. 2 times standard deviation) is displayed with orange lines on either side of predicted blue line.

# ACF - PCF
# let’s create an ACF and PACF plot of the residuals of our best fit ARIMA model i.e. ARIMA(0,1,1)(0,1,1)
acf(ts(ARIMAfit$residuals),main='ACF Residual')
pacf(ts(ARIMAfit$residuals),main='PACF Residual')
# all residuals inside the insignificant zone


###


######  ~~~ RETICULATE ~~~ #####
library(reticulate)

reticulate::conda_version()
reticulate::conda_list()
# reticulate
