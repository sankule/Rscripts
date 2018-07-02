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


                                                  ###### Random Forest  #######


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

# cost function is similar as we used in linear regression but its kept inside the SIGMOID FUNCTION
# y = b0 + b1X1 + b2X2 + .....
# p = 1 / (1 + e^-y)  -> sigmoid function

# LOGIT FUNCTION
#   p = e^y/ 1 + e^y 
#   where p is the probability of success. This is the Logit Function
#   log(p/1-p) is the link function. Logarithmic transformation on the outcome variable allows us to model a non-linear association in a linear way.
#   (p/1-p) is the odds ratio. Whenever the log of odd ratio is found to be positive, the probability of success is always more than 50%. 

# GENERALIZED LINEAR MODEL (GLM)
#   The fundamental equation of generalized linear model is:
#   g(E(y)) = α + βx1 + γx2
#   Here, g() is the link function, E(y) is the expectation of target variable and α + βx1 + γx2 is the linear predictor 
#   (α,β,γ to be predicted). The role of link function is to ‘link’ the expectation of y to linear predictor.

# IMP POINTS TO BE NOTED:
# 1. GLM does not assume a linear relationship between dependent and independent variables. However, it assumes a linear relationship 
#    between link function and independent variables in logit model.
# 2. The dependent variable need not to be normally distributed.
# 3. It does not uses OLS (Ordinary Least Square) for parameter estimation. Instead, it uses maximum likelihood estimation (MLE).
# 4. Errors need to be independent but not normally distributed.


# projecting probability on y axis (Dependent Variable) gives the chances of YES/NO for that particular X value
# y_hat (predicted value) is taken by dividing the y into two planes, usually a horizontal line: y = 0.5
# any value below 0.5 line is taken as 0/NO and above 0.5 line is taken as 1/YES
# probability scores - will take a look later

# Importing the dataset
dataset = read.csv('Section 14 - Logistic Regression/Logistic_Regression/Logistic_Regression/Social_Network_Ads.csv')
head(dataset)
# we will train only with age and salary - subsetting
dataset = dataset[3:5]
# Encoding the target feature as factor - 1 being Bought the car, 0 being Not Bought
dataset$Purchased = factor(dataset$Purchased, levels = c(0, 1))

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling - scaling both age and salary
training_set[-3] = scale(training_set[-3])
test_set[-3] = scale(test_set[-3])

# Fitting Logistic Regression to the Training set

# GLM - Generalized Linear Model
#   Logistic regression is a linear classifier - our logistic regression classifier will linearly separate our two classes of users.
# Binomial family is used for binary DV

classifier = glm(formula = Purchased ~ .,
                 family = binomial,
                 data = training_set)
classifier
# Predicting the Test set results
prob_pred = predict(classifier, type = 'response', newdata = test_set[-3])
# using if-else to mark 0 and 1 probability with 0.5 being the cutoff
y_pred = ifelse(prob_pred > 0.5, 1, 0)
summary.factor(y_pred)

# Making the Confusion Matrix
cm = table(test_set[, 3], y_pred > 0.5)
cm
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

# As you can see that the prediction boundary is Linear, which is the essence of Logistic Regression.
# becauase the logistic regression classifier is a linear classifier


# LOGISTIIC MODEL PERFORMANCE # 
# 1. AIC (Akaike Information Criteria) – The analogous metric of adjusted R² in logistic regression is AIC. AIC is the measure of 
#    fit which penalizes model for the number of model coefficients. Therefore, we always prefer model with minimum AIC value.

# 2. Null Deviance and Residual Deviance – Null Deviance indicates the response predicted by a model with nothing but an intercept.
#    Lower the value, better the model. Residual deviance indicates the response predicted by a model on adding independent variables. 
#    Lower the value, better the model.

# 3. Confusion Matrix: It is nothing but a tabular representation of Actual vs Predicted values. This helps us to find the accuracy of the model 
#    and avoid overfitting. This is how it looks like:
#    calculate the ACCURACY/PRECISION/RECALL of your model using:
#    ACCURACY = (TP + TN)/(TP + TN + FP + FN)
#    PRECISION = TP / (TP + FP)
#    RECALL = TP / (TP + FN)
#    F1 SCORE = 2 * Precision * Recall / (Precision + Recall)

# 4. ROC Curve: Receiver Operating Characteristic(ROC) summarizes the model’s performance by evaluating the trade offs between true positive rate (sensitivity)
#    and false positive rate(1- specificity). For plotting ROC, it is advisable to assume p > 0.5 since we are more concerned about success rate. 
#    ROC summarizes the predictive power for all possible values of p > 0.5.  The area under curve (AUC), referred to as index of accuracy(A) or 
#    concordance index, is a perfect performance metric for ROC curve. Higher the area under curve, better the prediction power of the model.

# Take a look at this: https://www.r-bloggers.com/roc-curves-in-two-lines-of-r-code/


                                                            #### K-NN  #####

# Example: Given that the data has few categories. If a new data point is introduced, find the category it belongs to
# Steps
# 1. Choose the number K of neighbours (usually k=5 is considered a decent number)
# 2. Take the K nearest neighbours of the new data point, according to euclidian distance
# 3. Among these K neighbours, count the number of data points in each category (eg: category 1: 3 neighbours, category 2: 2 neighbours)
# 4. Assign the new data point the category where you counted the most neighbours (eg: will assign to category 1, as it has more neighbours)
#                            Model Ready!


# K-NN is based on the euclidean distance between the points 
# standardizing the numerical features helps in increasing the accuracy of model.

# Importing the dataset
dataset = read.csv('Section 15 - K-Nearest Neighbors (K-NN)/K_Nearest_Neighbors/K_Nearest_Neighbors/Social_Network_Ads.csv')
# same dataset as used in logistic regression - same preprocessing 
dataset = dataset[3:5]

# Encoding the target feature as factor
dataset$Purchased = factor(dataset$Purchased, levels = c(0, 1))

# Splitting the dataset into the Training set and Test set
library(caTools)
set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
training_set[-3] = scale(training_set[-3])
test_set[-3] = scale(test_set[-3])

# Fitting K-NN to the Training set and Predicting the Test set results
library(class)
y_pred = knn(train = training_set[, -3],
             test = test_set[, -3],
             cl = training_set[, 3],
             k = 5,
             prob = TRUE)
y_pred
# Making the Confusion Matrix
cm = table(test_set[, 3], y_pred)
cm
# as you can see - the KNN gives better results than the logistic regression
# Visualising the Training set results
library(ElemStatLearn)
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = knn(train = training_set[, -3], test = grid_set, cl = training_set[, 3], k = 5)
plot(set[, -3],
     main = 'K-NN (Training set)',
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
y_grid = knn(train = training_set[, -3], test = grid_set, cl = training_set[, 3], k = 5)
plot(set[, -3],
     main = 'K-NN (Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))


                                                                  #### SVM #####

# Support Vector Machine (SVM)


# A Support Vector Machine (SVM) performs classification by finding the hyperplane that maximizes the margin between the two classes. 
# The vectors (cases - points(few in each class)) that define the hyperplane are the support vectors.
# means that the set of points from each class are supporting the algorithm, if we remove all other points, it wont make a difference.
# Why vector? the location of points in N dimensional space is defined by a vector i^ j^ k^ ....

# MAXIMUM MARGIN HYPERPLANE a.k.a MAXIMUM MARGIN CLASSIFIER is between the POSITIVE AND NEGATIVE HYPERPLANES
# Positive/Negative just a convention - the classes to the right is considered the positive.

# The beauty of SVM is that if the data is linearly separable, there is a unique global minimum value. An ideal SVM analysis 
# should produce a hyperplane that completely separates the vectors (cases) into two non-overlapping classes. However, perfect
# separation may not be possible, or it may result in a model with so many cases that the model does not classify correctly. 
# In this situation SVM finds the hyperplane that maximizes the margin and minimizes the misclassifications.

# The algorithm tries to maintain the slack variable to zero while maximizing margin. However, it does not minimize the 
# number of misclassifications (NP-complete problem) but the sum of distances from the margin hyperplanes.

# The simplest way to separate two groups of data is with a straight line (1 dimension), flat plane (2 dimensions) or an N-dimensional 
# hyperplane. However, there are situations where a nonlinear region can separate the groups more efficiently. SVM handles this by 
# using a kernel function (nonlinear) to map the data into a different space where a hyperplane (linear) cannot be used to do the 
# separation. It means a non-linear function is learned by a linear learning machine in a high-dimensional feature space while the 
# capacity of the system is controlled by a parameter that does not depend on the dimensionality of the space. This is called kernel
# trick which means the kernel function transform the data into a higher dimensional feature space to make it possible to perform the 
# linear separation.  

# Importing the dataset
dataset = read.csv('Section 16 - Support Vector Machine (SVM)/SVM/SVM/Social_Network_Ads.csv')
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

# Fitting SVM to the Training set
# install.packages('e1071')
library(e1071)
classifier = svm(formula = Purchased ~ .,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'linear')

classifier
# Number of Support Vectors:  116 with LINEAR KERNEL

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-3])

# Making the Confusion Matrix
cm = table(test_set[, 3], y_pred)
cm 

# Visualising the Training set results
library(ElemStatLearn)
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(classifier, newdata = grid_set)
plot(set[, -3],
     main = 'SVM (Training set)',
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
y_grid = predict(classifier, newdata = grid_set)
plot(set[, -3], main = 'SVM (Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))



                                                        #### KERNEL SVM #####

# What if we cant find the LINEAR HYPERPLANE ? Data is not LINEARLY SEPARABLE
# There are a few ways to encounter this problem
# 1. Higher Dimensional Space - adding an extra dimension and make data linearly separable
# 2. Kernel Trick - dealing with higher dimension

# As we have seen in SVM:
#   The simplest way to separate two groups of data is with a straight line (1 dimension), flat plane (2 dimensions) or an N-dimensional 
#   hyperplane. However, there are situations where a nonlinear region can separate the groups more efficiently. SVM handles this by 
#   using a kernel function (nonlinear) to map the data into a different space where a hyperplane (linear) cannot be used to do the 
#   separation. It means a non-linear function is learned by a linear learning machine in a high-dimensional feature space while the 
#   capacity of the system is controlled by a parameter that does not depend on the dimensionality of the space. This is called "kernel
#   trick" which means the kernel function transform the data into a higher dimensional feature space to make it possible to perform the 
#   linear separation

# If LINEAR SEPARATION is Impossible
# How can we apply the method of increasing the dimensionality of space to make it a LINEARLY SEPARABLE DATASET in HIGHER DIMENSION ?

# MAPPING FUNCTION  -  example of projecting point on a linear scale onto a parabola will make the dataset linearly separable
# By theory we know that dataset would be LINEARLY SEPARABLE in the HIGHER DIMENSION - we just have to figure out the MAPPING FUNCTION
# which can separate the points (or vectors) into LINEARLY SEPARABLE
# after figuring out the mapping function and the Maximum Margin Hyperplane we project the dataset back to lower dimensional space
# and we will get our NON-LINEAR SEPARATOR

# PROBLEM?
# Mapping data to a higher dimension then back again to lowe dimension can be highly Computing Intensive
# For this problem we can use the KERNEL TR

# Map data into new space(Higher Dimension), then take the inner product of the new vectors. The image of the inner product of the data is the 
# inner product of the images of the data

# KERNEL TRICK

#  GAUSSIAN RADIAL BASIS FUNCTION:

# K(x,l) = exp(-||x - l||^2 / 2*sig^2)
# where, 
# l  => landmark - middle of the gaussian curve
# x => random point in the plane

# The idea is to place the landmark optimally - which would separate the points based on Gaussian RBF.
# after placing the landmark, we find the Gaussian RBF FUNCTION VALUE for all points. 
# the points on the bell curve is projected down - creating a non-linear separator
# with points far from the landmark being nearly zero/insignificant and points close to the landmark being significant 
# sigma (variance = sigma^2) decide the wideness of circumference - higher the sigma - larger the circumference

# For complex distribution of points we can do a combination of Gaussian RBF function, like:
# K(x,l1) + K(x,l2)


# Types of Kernel Functions: 
# 1. Polynomial
# 2. Gaussian Radial Basis Function
# 3. Sigmoid Kernel

# various other types of Kernels, find more at: http://mlkernels.readthedocs.io/en/latest/kernels.html


# Kernel SVM Code:

# Importing the dataset
dataset = read.csv('Section 17 - Kernel SVM/Kernel-SVM/Kernel_SVM/Social_Network_Ads.csv')
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

# Fitting Kernel SVM to the Training set
# install.packages('e1071')
library(e1071)
classifier = svm(formula = Purchased ~ .,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'radial')

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-3])

# Making the Confusion Matrix
cm = table(test_set[, 3], y_pred)
cm # better than LINEAR SVM
# Visualising the Training set results
library(ElemStatLearn)
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(classifier, newdata = grid_set)
plot(set[, -3],
     main = 'Kernel SVM (Training set)',
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
y_grid = predict(classifier, newdata = grid_set)
plot(set[, -3], main = 'Kernel SVM (Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))



                                                          #### Naive Bayes #####

# BAYES THEOREM:

#   P(A|B)  = P(B|A)*P(A)/P(B)
#  Also its good to know, 
#   P(A) = P(B|A)*P(B) + P(A|B')*P(B')
#  Why naive? as it assumes the INDEPENDENCE (No Correlation between the variables) 
#  which is not the case in practical situations

# Note: when comparing the conditional probability of 2 classes for a given data point, the probability
#       of a class remains the same, ie p(B) will remain the same and thus can be ignored when comparing two cases
# eg:  p(walks | X) = P(X | walks)*P(Walks) / P(X)
#      p(drive | X) = P(X | drive)*P(drive) / P(X)
# here we have two classes: drive and walk, with X being the data point for which we have to find the probability
# when comparing these two probabilities, we actually dont need P(X), as its a common term and irrespective of how
# many classes there is going to be in the dataset, P(X) will remain the same


# Predictors Contribution:
#   Kononenko's information gain as a sum of information contributed by each attribute can offer an explanation on 
#   how values of the predictors influence the class probability.

#   The contribution of predictors can also be visualized by plotting "NOMOGRAMS". Nomogram plots log odds ratios for
#   each value of each predictor. Lengths of the lines correspond to spans of odds ratios, suggesting importance 
#   of the related predictor. It also shows impacts of individual values of the predictor.

# Naive Bayes

# Importing the dataset
dataset = read.csv('Section 18 - Naive Bayes/Naive_Bayes/Naive_Bayes/Social_Network_Ads.csv')
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

# Fitting SVM to the Training set
# install.packages('e1071')
library(e1071)
classifier = naiveBayes(x = training_set[-3],
                        y = training_set$Purchased)

classifier
# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-3])
y_pred
# Making the Confusion Matrix
cm = table(test_set[, 3], y_pred)
cm 

# Visualising the Training set results
library(ElemStatLearn)
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(classifier, newdata = grid_set)
plot(set[, -3],
     main = 'naiveBayes (Training set)',
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
y_grid = predict(classifier, newdata = grid_set)
plot(set[, -3], main = 'naiveBayes (Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))



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

# Split - to maximize the number of a category in the split region (increasing the entropy?)


# The core algorithm for building decision trees called ID3 by J. R. Quinlan which employs a "top-down", "greedy search" 
# through the space of possible branches with no "backtracking". ID3 uses "Entropy" and "Information Gain" to construct a 
# decision tree. In ZeroR model there is no predictor, in OneR model we try to find the single best predictor,
# naive Bayesian includes all predictors using Bayes' rule and the independence assumptions between predictors but decision
# tree includes all predictors with the dependence assumptions between predictors.		

#                                                     Entropy:		
# A decision tree is built top-down from a root node and involves partitioning the data into subsets that contain 
# instances with similar values (homogenous). ID3 algorithm uses entropy to calculate the homogeneity of a sample. 
# If the sample is completely homogeneous the entropy is zero and if the sample is an equally divided it has entropy of one.

# To build a decision tree, we need to calculate two types of entropy using frequency tables as follows:
# 1. Entropy (E) using the frequency table of one attribute:
#     E(S) = SUM{-p*log(p)} for i in 1:c, 
#     where c being the number of classes

# 2. Entropy (E) using the frequency table of two attributes:
#     E(T,X) = SUM{P(c)*E(c)} for c in X, and E(c) is calculated using single attribute Entropy

#                                                 Information Gain:		
# The information gain is based on the decrease in entropy after a dataset is split on an attribute. Constructing a 
# decision tree is all about finding attribute that returns the highest information gain (i.e., the most homogeneous branches).

# Step 1: Calculate entropy of the target: E(S), entropy of the single attribute

# Step 2: The dataset is then split on the different attributes. The entropy for each branch is calculated. Then it is added
# proportionally, to get total entropy for the split. The resulting entropy is subtracted from the entropy before the split.
# The result is the Information Gain, or decrease in entropy. 

# Gain:
#   Gain(T,X) = Entropy(T) - Entropy(T,X)

# Step 3: Choose attribute with the largest information gain (most decreased entropy) as the decision node, divide the dataset by 
# its branches and repeat the same process on every branch

# Step 4a: A branch with entropy of 0 is a leaf node (homogenous - subest has the same category).
# Step 4b: A branch with entropy more than 0 needs further splitting.

# Step 5: The ID3 algorithm is run recursively on the non-leaf branches, until all data is classified.

# https://en.wikipedia.org/wiki/ID3_algorithm


#  GINI IMPURITY
#  http://dni-institute.in/blogs/cart-decision-tree-gini-index-explained/
#  https://en.wikipedia.org/wiki/Decision_tree_learning#Gini_impurity

# Used by the CART (classification and regression tree) algorithm for classification trees, Gini impurity is a measure
# of how often a randomly chosen element from the set would be incorrectly labeled if it was randomly labeled according 
# to the distribution of labels in the subset

# Decision Tree Classification

# Importing the dataset
dataset = read.csv('Section 19 - Decision Tree Classification/Decision_Tree_Classification/Decision_Tree_Classification/Social_Network_Ads.csv')
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

# Fitting Decision Tree Classification to the Training set
# install.packages('rpart')
library(rpart)
classifier = rpart(formula = Purchased ~ .,
                   data = training_set)

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-3], type = 'class')

# Making the Confusion Matrix
cm = table(test_set[, 3], y_pred)
cm 
# Visualising the Training set results
library(ElemStatLearn)
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(classifier, newdata = grid_set, type = 'class')
plot(set[, -3],
     main = 'Decision Tree Classification (Training set)',
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
y_grid = predict(classifier, newdata = grid_set, type = 'class')
plot(set[, -3], main = 'Decision Tree Classification (Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

# Plotting the tree
plot(classifier)
text(classifier)




                                                            ###### Random Forest  #######


# Ensemle average of singular decision trees (same principle as ensemble average in molecular statistical thermodynamics)

# Random Forest Classification Code:

# Importing the dataset
dataset = read.csv('Section 20 - Random Forest Classification/Random_Forest_Classification/Random_Forest_Classification/Social_Network_Ads.csv')
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

# Fitting Random Forest Classification to the Training set
# install.packages('randomForest')
library(randomForest)
set.seed(123)
classifier = randomForest(x = training_set[-3],
                          y = training_set$Purchased,
                          ntree = 500)

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-3])

# Making the Confusion Matrix
cm = table(test_set[, 3], y_pred)
cm
# Visualising the Training set results
library(ElemStatLearn)
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(classifier, grid_set)
plot(set[, -3],
     main = 'Random Forest Classification (Training set)',
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
y_grid = predict(classifier, grid_set)
plot(set[, -3], main = 'Random Forest Classification (Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

# Choosing the number of trees
plot(classifier)



                                              #### Evaluating Classification Model Performance #####

### 1. False Positve and Negatives ####

# FP - False Positive - Type I error
# FN - False Negative - Type II error

#### 2. Confusion Matrix ####
#    Confusion Matrix: It is nothing but a tabular representation of Actual vs Predicted values. This helps us to find the accuracy of the model 
#    and avoid overfitting. This is how it looks like:
#    calculate the ACCURACY/PRECISION/RECALL of your model using:
#    ACCURACY = (TP + TN)/(TP + TN + FP + FN)
#    PRECISION = TP / (TP + FP)
#    RECALL = TP / (TP + FN)   a.k.a sensitivity or true positive rate (TPR)
#    F1 SCORE = 2 * Precision * Recall / (Precision + Recall)

# Accuracy Paradox:
# Accuracy can be higher for a model but not precise

#### 3. Cumulative Accuracy Profile (CAP) - Gain Chart #### 
# https://en.wikipedia.org/wiki/Cumulative_accuracy_profile 

# The CAP of a model represents the cumulative number of positive outcomes along the y-axis versus the 
# corresponding cumulative number of a classifying parameter along the x-axis. 
# Larger the area under the curve of the model - better the model
# a_p = area under perfect model and random model 
# a_r = area under the model and random model 
# AR = a_p/a_r  => AR closer to 1 for good models

# Other way 
# at X = 50%, if:
# Y < 60% Rubbish 
# 60% < Y < 80% Good  
# 80% < Y < 90% Very Good  
# 90% < Y < 100% Too good to be true - overfitting 



#### ROC (Receiver Operating Characteristic) Curve ####
#    ROC Curve: Receiver Operating Characteristic(ROC) summarizes the model’s performance by evaluating the trade offs between true positive rate (sensitivity)
#    and false positive rate(1- specificity). For plotting ROC, it is advisable to assume p > 0.5 since we are more concerned about success rate. 
#    ROC summarizes the predictive power for all possible values of p > 0.5.  The area under curve (AUC), referred to as index of accuracy(A) or 
#    concordance index, is a perfect performance metric for ROC curve. Higher the area under curve, better the prediction power of the model.


# The ROC curve is created by plotting the true positive rate (TPR) against the false positive rate (FPR) at various threshold settings. 
# The true-positive rate is also known as sensitivity, recall or probability of detection[1] in machine learning. The false-positive rate 
# is also known as the fall-out or probability of false alarm and can be calculated as (1 − specificity)

      ## ROC Curve Sample:
      library(ggplot2)
      diamonds$is_expensive <- diamonds$price > 2400
      is_test <- runif(nrow(diamonds)) > 0.75
      train <- diamonds[is_test==FALSE,]
      test <- diamonds[is_test==TRUE,]
      
      summary(fit <- glm(is_expensive ~ carat + cut + clarity, data=train))

      library(ROCR)
      
      prob <- predict(fit, newdata=test, type="response")
      pred <- prediction(prob, test$is_expensive)
      perf <- performance(pred, measure = "tpr", x.measure = "fpr")
      # I know, the following code is bizarre. Just go with it.
      auc <- performance(pred, measure = "auc")
      auc <- auc@y.values[[1]]
      
      roc.data <- data.frame(fpr=unlist(perf@x.values),
                             tpr=unlist(perf@y.values),
                             model="GLM")
      ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
        geom_ribbon(alpha=0.2) +
        geom_line(aes(y=tpr)) +
        ggtitle(paste0("ROC Curve w/ AUC=", auc))

# If your problem is linear, you should go for Logistic Regression or SVM.
# If your problem is non linear, you should go for K-NN, Naive Bayes, Decision Tree or Random Forest.

                                      ### Pros & Cons of Classification Models ####

  # Logistic Regression
# PRO: Probabilistic approach, gives informations about statistical significance of features 
# CON: The Logistic Regression Assumptions

  # K-NN 
# PRO: Simple to understand, fast and efficient 
# CON: Need to choose the number of neighbours k

  # SVM
# PRO: Performant, not biased by outliers, not sensitive to overfitting 
# CON: Not appropriate for non linear problems, not the best choice for large number of features

  # Kernel SVM
# PRO: High performance on nonlinear problems, not # biased by outliers, not sensitive to overfitting 
# CON: Not the best choice for large number of features, more complex

  # Naive Bayes
# PRO: Efficient, not biased by outliers, works on nonlinear problems, probabilistic approach 
# CON: Based on the assumption that features have same statistical relevance

  # Decision Tree Classification
# PRO: Interpretability, no need for feature scaling, works on both linear / nonlinear problems 
# CON: Poor results on too small datasets, overfitting can easily occur

  # Random Forest Classification
# PRO: Powerful and accurate, good performance on many problems, including non linear
# CON: No interpretability, overfitting can easily occur, need to choose the number of trees



                                              ######  ~~~ Clustering ~~~ #####

library(ggplot2)
library(data.table)
library(dplyr)
setwd("~/SWARIT/Udemy/Machine Learning A-Z Template Folder/Part 4 - Clustering/")


                                            #### K-Means Clustering #####

# STEPS:
# 1 : Choose the number of K clusters
# 2 : Select at random K points, the centroids (not necessarily from our dataset)
# 3 : Assign each data point to the closest centroid => That forms K clusters ; according to the Euclidean distance function.
# 4 : Compute and place the new centroid of each cluster
# 5 : Reassign each data point to the new closest centroid. and repeat from step 4 if there are reassignments 

# K-means is all about the analysis-of-variance paradigm. ANOVA - both uni- and multivariate - is based on the fact that 
# the sum of squared deviations about the grand centroid is comprised of such scatter about the group centroids and the 
# scatter of those centroids about the grand one: 
# SStotal=SSwithin+SSbetween. So, if SSwithin is minimized then SSbetween is maximized.

# SS of deviations of some points about their centroid (arithmetic mean) is known to be directly related to the overall squared 
# euclidean distance between the points: the sum of squared deviations from centroid is equal to the sum of pairwise squared 
# Euclidean distances divided by the number of points.

# Choosing the right number of clusters:
# WCSS (Within Cluster Sum of Squares) - computes the sum distances of points in a particular cluster
# WCSS for multiple clusters are sum of WCSS for each cluster (for more that 2, its the euclidean distance)
# use the elbow method (WCSS vs K) to determine what could be the best number of clusters


# for 2 dimension data, it is simple sum of square method and for more than 2 it is euclidean distance method to 
# calculate the WCSS

# K-Means Clustering Code

# Importing the dataset
dataset = read.csv('Section 24 - K-Means Clustering/K_Means/K_Means/Mall_Customers.csv')
head(dataset)
dataset = dataset[4:5]

# Using the elbow method to find the optimal number of clusters
set.seed(6)
wcss = vector()
for (i in 1:20) wcss[i] = sum(kmeans(dataset, i)$withinss)
wcss
plot(1:20,
     wcss,
     type = 'b',
     main = paste('The Elbow Method'),
     xlab = 'Number of clusters',
     ylab = 'WCSS')
# 5 Clusters seems to be an optimal value

# Fitting K-Means to the dataset
set.seed(29)
kmeans = kmeans(x = dataset, centers = 5)
kmeans
kmeans$
y_kmeans = kmeans$cluster

# Visualising the clusters 
library(cluster)
clusplot(dataset,
         y_kmeans,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste('Clusters of customers'),
         xlab = 'Annual Income',
         ylab = 'Spending Score')



                                                      #### Hierarchical Clustering #####

# Types:
##### Agglomerative #####

# Steps:
# 1: Make each data point a cluster => forming N clusters, N is the number of data points 
# 2: Take the two closest points and make them one cluster => forming N-1 cluster
# 3: Take the two closest clusters and make them one cluster => forming N-2 cluster
# 4: Repeat step-3 till there is just one huge cluster left

# What is closest cluster ?
# is it Euclidean distance of centroid or of some other point?
# there are many ways to do this, like closest points, farthest points etc - approach depends on business problem 

#### * Dendograms ####
# it keeps the record of all the steps that we have taken in the agglomerative hierarchical clustering
# the y-coord is based on the euclidean distance and we keep a thereshold of this distance to find the number clusters 
# the number of vertical line the thereshold crosses is the number of optimum clusters


# Hierarchical Clustering Code

# Importing the dataset
dataset = read.csv('Section 25 - Hierarchical Clustering/Hierarchical-Clustering/Hierarchical_Clustering/Mall_Customers.csv')
dataset = dataset[4:5]

# Using the dendrogram to find the optimal number of clusters
dendrogram = hclust(d = dist(dataset, method = 'euclidean'), method = 'ward.D')
dendrogram
plot(dendrogram,
     main = paste('Dendrogram'),
     xlab = 'Customers',
     ylab = 'Euclidean distances')

# 5 seems to be the optimum number of clusters based on this dendogram 

# Fitting Hierarchical Clustering to the dataset
hc = hclust(d = dist(dataset, method = 'euclidean'), method = 'ward.D')
y_hc = cutree(hc, 5)
y_hc

# Visualising the clusters
library(cluster)
clusplot(dataset,
         y_hc,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels= 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste('Clusters of customers'),
         xlab = 'Annual Income',
         ylab = 'Spending Score')


##### Divisive ####
# Reasearch 

                                              ### Pros & Cons of Clustering Models ####

    # K-Means
# PRO: Simple to understand, easily adaptable, works well on small or large datasets,fast, efficient and performant
# CON: Need to choose the number of clusters

    # Hierarchical Clustering
# PRO: The optimal number of clusters can be obtained by the model itself, practical visualisation with the dendrogram
# CON: Not appropriate for large datasets


                                                    ### ~~~  Association Rule Learning ~~~  ####
library(ggplot2)
library(data.table)
library(dplyr)
setwd("~/SWARIT/Udemy/Machine Learning A-Z Template Folder/Part 5 - Association Rule Learning/")

# it answers the question: People who bought ___  also bought ____
 
                                                        #### Apriori Intuition #####

# Support(L) = no. of transactions containing L / total transactions  

# Confidence(L1 -> L2) = no. of transactions containing L1 and L2 / transactions containing L1

# Lift(L1 -> L2) = confidence(L1 -> L2) / support(L2)
# Lift is the improvement in the prediction - based on the original prediction

# Steps:
# Step 1: Set a minimum support and confidence
# Step 2: Take all the subsets in transactions having higher support than the minimum support
# Step 3: Take all the rules of these subsets having higher confidence than the minimum confidence
# Step 4: Sort the rules by decreasing lift


# Apriori intuition Code:

# Data Preprocessing
# install.packages('arules')
library(arules)
dataset = read.csv('Section 28 - Apriori/Apriori-R/Apriori/Market_Basket_Optimisation.csv', header = FALSE)
# each line correspond to one customer transaction
# but the arules model does not take the data is this format - it needs the data to be in "SPARSE MATRIX" FORMAT 
# each column is going to be the unique item sold 
# and each customer will have a 0 or 1 denoting whether the item was bought or not

dataset = read.transactions('Section 28 - Apriori/Apriori-R/Apriori/Market_Basket_Optimisation.csv', sep = ',', rm.duplicates = TRUE)
# CLASS = "transactions"
# reads the transcations and removes the duplicates
# transactions class represents transaction data used for mining itemsets or rules.
summary(dataset)
# transactions as itemMatrix in sparse format with
# density means the fraction of zero's in the matrix 
# size means number of transactions with no. of items in each  

itemFrequencyPlot(dataset, topN = 20)

# Training Apriori on the dataset

# 3*7/7500 - considering this to be the mimum support of any item 
# considering items that are bought 3 times a day for a week / total number of transactions
# confidence value is 0.8 => rule must be true atleast 80% time
# with this confidence its very rare to get any rule - we will use 20% minimum confidence
# we can change the parameters based on data, business problem and level of rules we are looking for 

rules = apriori(data = dataset, parameter = list(support = 0.004, confidence = 0.2))
rules
# a reduced confidence will output a lot of rules but when sort by lift we can figure out 
# rules which are significant
# Visualising the results
inspect(rules)
inspect(sort(rules, by = 'lift')[1:15])
# sometimes items with high support can be misleading. for eg: chocolate => beef
# as chocolate has very high support 


                                                            #### Eclat Intuition #####

# Support(L) = no. of transactions containing L / total transactions  
# we just consider support in Eclat model
# here L can be a set of items (consider atleast 2 items in the support)

# Steps:
# Step 1: Set a minimum support
# Step 2: Take all the subsets in transactions having higher support than the minimum support
# Step 3: sort there subsets by decreasing support

# Eclat Code:
# install.packages('arules')
library(arules)
dataset = read.csv('Section 29 - Eclat/Eclat/Eclat/Market_Basket_Optimisation.csv')
dataset = read.transactions('Section 29 - Eclat/Eclat/Eclat/Market_Basket_Optimisation.csv', sep = ',', rm.duplicates = TRUE)
summary(dataset)
itemFrequencyPlot(dataset, topN = 30)

# Training Eclat on the dataset
# minlen = 2 as we want to consider atleast 2 items in the support calculation
rules = eclat(data = dataset, parameter = list(support = 0.003, minlen = 2))
rules # gives the number of sets not rules
# Visualising the results
inspect(sort(rules, by = 'support')[1:10])

  
                                                    ### ~~~  Reinforcement Learning ~~~  ####
library(ggplot2)
library(data.table)
library(dplyr)
setwd("~/SWARIT/Udemy/Machine Learning A-Z Template Folder/Part 6 - Reinforcement Learning/")

                                  # Reinforcement Learning
# Reinforcement Learning is a branch of Machine Learning, also called Online Learning. It is used to solve
# interacting problems where the data observed up to time t is considered to decide which action to take at 
# time t + 1. Desired outcomes provide the AI with reward, undesired with punishment. Machines learn through trial and error.


                                  # Multi Armed Bandit Problem
# is a problem in which a fixed limited set of resources must be allocated between competing (alternative) choices 
# in a way that maximizes their expected gain, when each choice's properties are only partially known at the time of 
# allocation, and may become better understood as time passes or by allocating resources to the choice

# The name comes from imagining a gambler at a row of slot machines (sometimes known as "one-armed bandits"), who has to 
# decide which machines to play, how many times to play each machine and in which order to play them, and whether to continue
# with the current machine or try a different machine.

# Each machine has its probability distribution - but we dont know these distributions
# we have to figure out, which of these distribution is the best - or best machine

# Exploration and Exploitation:
# In the problem, each machine provides a random reward from a probability distribution specific to that machine. 
# The objective of the gambler is to maximize the sum of rewards earned through a sequence of lever pulls. The crucial 
# tradeoff the gambler faces at each trial is between "exploitation" of the machine that has the highest expected payoff 
# and "exploration" to get more information about the expected payoffs of the other machines.

# => need to explore the machines and its distributions and while doing it also exploit the best ones to make max return

# we will be working on 5 advertisements - and need to figure out which advertisement gives best conversion
# one way to do that is to run an A/B test
# A/B test can be used to find the best option - but A/B is pure exploration uniformally in random way
# - and we dont exploit the best option
# Reinforcement Learning -  Finding the best/optimal (exploiting) while in the process of exploration 

# problem statement:

# 1. We have d arms, for example arms are ads that we display to users each time they connect to a web page
# 2. Each time a user connects to this web page, that makes a round
# 3. At each round n, we choose one ad to display to the user.
# 4. At each round n, ad i gives reward ri(n) in {0,1}:
#    ri(n) = 1 if the user clicked on the ad i, 0 if the user didnt
# 5. Our goal is to maximize the total reward we get over many rounds


                                              #### Upper Bound Confidence (UCB) #####

# 1. assume same begining to all machines - assuming same distribution for all 
# 2. for every distribution we assume a certain starting value 
# 3. we make some confidence bounds - and its designed in a way that the confidence bound will include 
#    the acutal expected return 
# 4. first few rounds would be a trials and come up with a confidence bound(which is very high)
# 5. we pick the machine with the highest confidence bounds 
# 6. we start running the ads - and the observed averages will move towards the real mean and confidence bounds go down
# 7. pick the machine with highest confidence bound again
# 8. after a few rounds it becomes confident if few machines and vice versa

# Math:
# Step 1: At each round n, we consider two numbers for each ad i;
#   Ni(n) - the number of times the ad i was selected up to round n,
#   Ri(n) - the sum of rewards of the ad i upto round n.

# Step 2: From these two number we compute:
#  - the average reward of ad i upto round n
#   r(n) = Ri(n)/Ni(n)

# - the confidence interval of r(n) +- delta_i(n) at round n with
#   delta_i(n) = {3/2 * log(n)/Ni(n)}^1/2

# Step 3: we select the ad i that has maximum UCB r(n) + delta_i(n)


# Upper Confidence Bound Code:

# Importing the dataset
# we want to find the ad which will get the most clicked 
# 1 - means the user clicked on the ad and 0 means ad not clicked by the user
dataset = read.csv('Section 32 - Upper Confidence Bound (UCB)/UCB/UCB/Ads_CTR_Optimisation.csv')
head(dataset)
str(dataset)


    # Implementing Random Selection
    N = 10000
    d = 10
    ads_selected = integer(0)
    total_reward = 0
    # random reward
    for (n in 1:N) {
      ad = sample(1:10, 1)
      ads_selected = append(ads_selected, ad)
      reward = dataset[n, ad]
      total_reward = total_reward + reward
    }
    
    # Visualising the results
    hist(ads_selected,
         col = 'blue',
         main = 'Histogram of ads selections',
         xlab = 'Ads',
         ylab = 'Number of times each ad was selected')

    # uniform histogram for random selection as expected  


# Implementing UCB from scratch    
dataset = read.csv('Section 32 - Upper Confidence Bound (UCB)/UCB/UCB/Ads_CTR_Optimisation.csv')
head(dataset)
str(dataset)    

N = 10000
d = 10
ads_selected = integer(0)
numbers_of_selections = integer(d)
sums_of_rewards = integer(d)
total_reward = 0
# for each round
for (n in 1:N) {
  ad = 0
  max_upper_bound = 0
  # for each ad
  for (i in 1:d) {
    # check for 1 - when ad gets clicked
    if (numbers_of_selections[i] > 0) {
      average_reward = sums_of_rewards[i] / numbers_of_selections[i]
      delta_i = sqrt(3/2 * log(n) / numbers_of_selections[i])
      upper_bound = average_reward + delta_i
    } else {
      # when ad not clicked
      upper_bound = 1e400
    }
    if (upper_bound > max_upper_bound) {
      max_upper_bound = upper_bound
      ad = i
    }
  }
  # vector of ads that are selected in each round
  ads_selected = append(ads_selected, ad)
  
  # number of selection of each ad
  numbers_of_selections[ad] = numbers_of_selections[ad] + 1
  
  # reward = {0,1} 
  reward = dataset[n, ad]
  
  # sum of rewards of each ad
  sums_of_rewards[ad] = sums_of_rewards[ad] + reward
  
  # total reward
  total_reward = total_reward + reward
}



# quick look at ads selected
head(ads_selected, 500)
tail(ads_selected, 500)

# Visualising the results
hist(ads_selected,
     col = 'blue',
     main = 'Histogram of ads selections',
     xlab = 'Ads',
     ylab = 'Number of times each ad was selected')


                                                #### Thompson Sampling #####

# UCB vs Thompson Sampling
# thompson sampling is a probabilistic algorithm unlike the UCB which is deterministic algorithm
# UCB require update at every round 
# TS can accomodate delayed feedback
# TS has better empirical evidence


# same assumptions - we dont know the distribution of machines 
# Thompson Sampling algorithm creates a distribution for each machine 
# but we dont want to know/guess the distributions behind the machines 
# we are constructing the distributions where we "think" the actually value might lie
#  > auxiliary mechanism < 

# the algorithm refines the distribution with each round

dataset = read.csv('Section 32 - Upper Confidence Bound (UCB)/UCB/UCB/Ads_CTR_Optimisation.csv')
head(dataset)
str(dataset)


    # Implementing Random Selection
    N = 10000
    d = 10
    ads_selected = integer(0)
    total_reward = 0
    # random reward
    for (n in 1:N) {
      ad = sample(1:10, 1)
      ads_selected = append(ads_selected, ad)
      reward = dataset[n, ad]
      total_reward = total_reward + reward
    }
    
    # Visualising the results
    hist(ads_selected,
         col = 'blue',
         main = 'Histogram of ads selections',
         xlab = 'Ads',
         ylab = 'Number of times each ad was selected')
    
    # uniform histogram for random selection as expected  


# Thompson Sampling from scratch Code:
dataset = read.csv('Section 32 - Upper Confidence Bound (UCB)/UCB/UCB/Ads_CTR_Optimisation.csv')
head(dataset)
str(dataset)

N = 10000
d = 10
ads_selected = integer(0)
numbers_of_rewards_1 = integer(d)
numbers_of_rewards_0 = integer(d)
total_reward = 0
for (n in 1:N) {
  ad = 0
  max_random = 0
  for (i in 1:d) {
    random_beta = rbeta(n = 1,
                        shape1 = numbers_of_rewards_1[i] + 1,
                        shape2 = numbers_of_rewards_0[i] + 1)
    if (random_beta > max_random) {
      max_random = random_beta
      ad = i
    }
  }
  ads_selected = append(ads_selected, ad)
  reward = dataset[n, ad]
  if (reward == 1) {
    numbers_of_rewards_1[ad] = numbers_of_rewards_1[ad] + 1
  } else {
    numbers_of_rewards_0[ad] = numbers_of_rewards_0[ad] + 1
  }
  total_reward = total_reward + reward
}

# Visualising the results
hist(ads_selected,
     col = 'blue',
     main = 'Histogram of ads selections',
     xlab = 'Ads',
     ylab = 'Number of times each ad was selected')

# better than UCB algorithm





                                        ### ~~~  Natural Language Processing (NLP) ~~~  ####

library(ggplot2)
library(data.table)
library(dplyr)
setwd("~/SWARIT/Udemy/Machine Learning A-Z Template Folder/Part 7 - Natural Language Processing/")

# Natural Language Processing (or NLP) is applying Machine Learning models to text and language. Teaching 
# machines to understand what is said in spoken and written word is the focus of Natural Language Processing.

# A very well-known model in NLP is the Bag of Words model. It is a model used to preprocess the texts to
# classify before fitting the classification algorithms on the observations containing the texts.

# Uses:
# sentiment analysis
# document summarization
# predicting genre of book
# question analysi
# building speech recognition 

# Mail NLP libraries
# NLTK / Spacy / Stanford NLP / openNLP
# we will use NLTK:  http://www.nltk.org/

# Importing the dataset
dataset_original <-
  read.delim('Section 36 - Natural Language Processing/Natural-Language-Processing/Natural_Language_Processing/Restaurant_Reviews.tsv', 
             quote = '', stringsAsFactors = FALSE)

# Bag of Words Model

# Cleaning the texts
library(tm)
library(SnowballC)
corpus = VCorpus(VectorSource(dataset_original$Review))
corpus = tm_map(corpus, content_transformer(tolower)) # to lower case 
  as.character(corpus[[1]])
corpus = tm_map(corpus, removeNumbers) # removing numbers
  as.character(corpus[[841]])
corpus = tm_map(corpus, removePunctuation) # removing punctuations
  as.character(corpus[[1]])
corpus = tm_map(corpus, removeWords, stopwords())  # removing non-relevant words (and, or, the, this .. etc)
  as.character(corpus[[1]])
corpus = tm_map(corpus, stemDocument) # getting the root word
  as.character(corpus[[1]])
corpus = tm_map(corpus, stripWhitespace) # removing whitespaces
  as.character(corpus[[1]])

# Creating the Bag of Words model

# https://en.wikipedia.org/wiki/Document-term_matrix  
# A document-term matrix or term-document matrix is a mathematical matrix that describes the frequency
# of terms that occur in a collection of documents. In a document-term matrix, rows correspond to documents
# in the collection and columns correspond to terms.
# converting corpus to document-term matrix
dtm = DocumentTermMatrix(corpus)
dtm
dtm = removeSparseTerms(dtm, 0.999)  # removing sparse terms from the document-term matrix
dataset = as.data.frame(as.matrix(dtm)) # converting to a dataframe
str(dataset)
colnames(dataset)
# attaching liked column 
dataset$Liked = dataset_original$Liked
head(dataset)

# Importing the dataset
# dataset = read.csv('Social_Network_Ads.csv')
# dataset = dataset[3:5]

# Encoding the target feature as factor
dataset$Liked = factor(dataset$Liked, levels = c(0, 1))

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Liked, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Fitting Random Forest Classification to the Training set
# install.packages('randomForest')
library(randomForest)
classifier = randomForest(x = training_set[-692],
                          y = training_set$Liked,
                          ntree = 50)

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-692])

# Making the Confusion Matrix
cm = table(test_set[, 692], y_pred)
cm



                                     ### ~~~  Deep Learning ~~~  ####

library(ggplot2)
library(data.table)
library(dplyr)
setwd("~/SWARIT/Udemy/Machine Learning A-Z Template Folder/Part 8 - Deep Learning/")


# Deep Learning is the most exciting and powerful branch of Machine Learning. 
# Deep Learning models can be used for a variety of complex tasks:
#   
# Artificial Neural Networks for Regression and Classification
# Convolutional Neural Networks for Computer Vision
# Recurrent Neural Networks for Time Series Analysis
# Self Organizing Maps for Feature Extraction
# Deep Boltzmann Machines for Recommendation Systems
# Auto Encoders for Recommendation Systems


                                          ### Artifical Neural Networks  ####


# node (neuron) get the synapse (input) from independent variables 
# input(synapses) should be standardized (0 mean & 1 SD)
# output can be continuous / binary / categorical (multiple outputs)
# synapses are assigned weights - weights are how neural network learn - which signals to pass along

# what happens in the neuron ?
# step1 : weighted sum of all synapses to the neuron is measured (SUM{Xi*Wi})
# step2 : on this sum, the activation function is applied, and based on the result output is desided

                              ###### ** Activation Functions ** #####

### 1. Threshold Function ####
# very straightfoward function - yes/no - x is the weighted sum of the synapses
  # phi(x) = 1 if x>=0 
  # phi(x) = 0 if x <0

### 2. Sigmoid Function  ####
# same as used in logistic regression
  # phi(x) = 1 / {1 + e^-x}

### 3. Rectifier Function ####
# - one of the most popular function used in artifical Nueral Network
  # phi(x) = max(x,0)


### 4. Hyperbold Tangent (tanh) Function ####
# similar to sigmoi function but here the function value goes below zero as well
  # phi(x) = {1 - e^-2x}/{1 + e^-2x}

# when to use ?
# if DV is {0,1} we can use Thereshold or Sigmoid
# Mostly Rectifier activation function is used in the hidden layer and sigmoid function in output layer


# How does Neural Network Work?
# not all synapses have a non zero weight in the hidden layer neurons - many have zero 
# that depends on the training set and how the neuron understands the synapses
# each neuron work independently - and the ensemble of all helps in the right prediction 

# How does Neural Network Learn?
# there are two approaches - lets say we want the computer to figure out whether the image is of cat/dog 
# 1. in which we explicitely show that if the ears are pointy etc then its cat or if ears are down its a dog --
# 2. in which we feed the images classified as dogs and cats and tell the computer to learn on its own 
# we will see how the second approach works - we will look at the perceptron

# we use cost function minimization using the gradient descent algorithm to find the optimal weights (coefficients)
# using the backpropogation, adjusting weights each time and trying to minimize the cost function 

# A list of cost functions used in neural networks, alongside applications:
# https://stats.stackexchange.com/questions/154879/a-list-of-cost-functions-used-in-neural-networks-alongside-applications

# Curse of Dimensionality:
# Lets say we have 1000 data points in our training set and 5 independent variables
# we have 5 neurons in the hidden layer = total of 25 weights to be calculated 
# total combinations (brut force) of weights for all data points is 10^75
# which is physically impossible to solve for

# we use gradient descent to find the optimal weights (its the same as finding coefficients in regression)

# Stochastic Gradient Descent:
# gradient descent requires the cost function to be convex to find the minimum value
# what if its not covex and we get to local minima?
# to avoid this we use stochastic gradient descent
# In stochastic gradient descent we dont take the all rows and plug them into the neural network like we 
# do in the gradient descent. In this we take one row at a time - also called BATCH GRADIENT DESCENT

# more Resources:

# http://iamtrask.github.io/2015/07/27/python-network-part2/
# https://www.cse.iitk.ac.in/users/sigml/lec/DeepLearningLib.pdf
# http://neuralnetworksanddeeplearning.com/
# http://deeplearning.net/tutorial/deeplearning.pdf
# http://www.deeplearningbook.org/


### ** Backpropagation** ####
# it adjust all the weights simultaneously

# Training the ANN with Stochastic Gradient Descent
# Step 1: Randomly initialise the weights to small numbers close to 0 (not 0).
# Step 2: Input the first observation of your dataset in the input layer, each feature in one input node.
# Step 3: Forward-Propagation: from left to right, the neurons are activated in a way that the impact of each 
#         neurons activation is limited by the weights. Propagate untill getting the predicted result y.
# Step 4: Compare the predicted result to the actual result. Measure the generated error.
# Step 5: Back-Propagation: from right to left, the error is back-propagated. Update the weights according 
#         to how much they are responsible for the error. The learning rate decides by how much we update the weights.
# Step 6: Repeat steps 1 to 5 and update the weights after each observation(Reinforcement Learning) Or:
#         Repeat steps 1 to 5 but update the weights only after a batch of observations (Batch Learning)
# Step 7: When the whole training set passed through the ANN, that makes an epoch. Redo more epochs.


# Problem Description: - a classification problem 
# data about back details and whether the customer leaves the back within 6 months
# make a customer segmentation and figure out which customers are at the highest risk of exiting the bank 


# Artificial Neural Network Code:
# Importing the dataset
dataset = read.csv('Section 39 - Artificial Neural Networks (ANN)/Artificial-Neural-Networks/Artificial_Neural_Networks/Churn_Modelling.csv')
head(dataset)
# subsetting only the requied features
dataset = dataset[4:14]

# Encoding the categorical variables as factors
dataset$Geography = as.numeric(factor(dataset$Geography,
                                      levels = c('France', 'Spain', 'Germany'),
                                      labels = c(1, 2, 3)))
dataset$Gender = as.numeric(factor(dataset$Gender,
                                   levels = c('Female', 'Male'),
                                   labels = c(1, 2)))

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Exited, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
training_set[-11] = scale(training_set[-11])
test_set[-11] = scale(test_set[-11])

# Fitting ANN to the Training set
# install.packages('h2o')
library(h2o)
# connecting to h2o instance
h2o.init(nthreads = -1)

# using Rectifier activation function in the hidden layer
# 6 nodes x 2 layers => usually the number of neuron in a hidden layer is taken 
#                       as average of input and output features
# training sample per iteration => batch size = -2 for auto-tuning
model = h2o.deeplearning(y = 'Exited',
                         training_frame = as.h2o(training_set),
                         activation = 'Rectifier',
                         hidden = c(6,6),
                         epochs = 100,
                         train_samples_per_iteration = -2)
model

# Predicting the Test set results
y_pred = h2o.predict(model, newdata = as.h2o(test_set[-11]))
y_pred
y_pred = ifelse(y_pred > 0.5, 1, 0)
y_pred = as.vector(y_pred)

# Making the Confusion Matrix
cm = table(test_set[, 11], y_pred)
cm

# accuracy in %
100*(cm[1,1]+cm[2,2])/sum(cm)

h2o.shutdown()

                                        ### Convolutional Neural Networks  ####

# still a work in progress for R:
# https://community.h2o.ai/questions/325/when-will-deep-water-be-released-for-its-first-ver.html
# https://github.com/h2oai/deepwater
# https://community.h2o.ai/questions/452/rnncnn-with-h2o.html
                                                                

                                                ### ~~~  Dimensionality Reduction ~~~  ####

library(ggplot2)
library(data.table)
library(dplyr)
setwd("~/SWARIT/Udemy/Machine Learning A-Z Template Folder/Part 9 - Dimensionality Reduction/")

# Remember in Part 3 - Classification, we worked with datasets composed of only two independent variables. 
# We did for two reasons:
# 1.  Because we needed two dimensions to visualize better how Machine Learning models worked 
#     (by plotting the prediction regions and the prediction boundary for each model).
# 2.  Because whatever is the original number of our independent variables, we can often end up with two 
#     independent variables by applying an appropriate Dimensionality Reduction technique.

# There are two types of Dimensionality Reduction techniques:
# 1.  Feature Selection
# 2.  Feature Extraction

# Feature Selection techniques are Backward Elimination, Forward Selection, Bidirectional Elimination,
# Score Comparison and more. We covered these techniques in Part 2 - Regression.

# In this part we will cover the following Feature Extraction techniques:
#   Principal Component Analysis (PCA)
#   Linear Discriminant Analysis (LDA)
#   Kernel PCA
#   Quadratic Discriminant Analysis (QDA)


                                    ### Principal Component Analysis (PCA) ####

# In a nutshell:
# From m independent variables of your dataset. PCA extracts p <= m new independent variables that 
# explain the most variance of the dataset, regardless of dependent variable

# The Fact that dependent variables are not considered, PCA is an "unsupervised" model 


# PCA Code:
# Importing the dataset
dataset = read.csv('Section 43 - Principal Component Analysis (PCA)/PCA/PCA/Wine.csv')
head(dataset)
str(dataset)

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Customer_Segment, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling - 
training_set[-14] = scale(training_set[-14])
test_set[-14] = scale(test_set[-14])

# Applying PCA

# install.packages('caret')
library(lattice)
library(caret)
# install.packages('e1071')
library(e1071)
pca = preProcess(x = training_set[-14], method = 'pca', pcaComp = 2) #pcaComp = 2, as we want to create 2
                                                                     # extracted features 
pca
training_set = predict(pca, training_set)
  head(training_set)
training_set = training_set[c(2, 3, 1)]
  head(training_set)
test_set = predict(pca, test_set)
test_set = test_set[c(2, 3, 1)]

# Fitting SVM to the Training set
library(e1071)
classifier = svm(formula = Customer_Segment ~ .,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'linear')

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-3])

# Making the Confusion Matrix
cm = table(test_set[, 3], y_pred)
cm

# Visualising the Training set results
library(ElemStatLearn)
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('PC1', 'PC2')
y_grid = predict(classifier, newdata = grid_set)
plot(set[, -3],
     main = 'SVM (Training set)',
     xlab = 'PC1', ylab = 'PC2',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 2, 'deepskyblue', ifelse(y_grid == 1, 'springgreen3', 'tomato')))
points(set, pch = 21, bg = ifelse(set[, 3] == 2, 'blue3', ifelse(set[, 3] == 1, 'green4', 'red3')))

# Visualising the Test set results
library(ElemStatLearn)
set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('PC1', 'PC2')
y_grid = predict(classifier, newdata = grid_set)
plot(set[, -3], main = 'SVM (Test set)',
     xlab = 'PC1', ylab = 'PC2',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 2, 'deepskyblue', ifelse(y_grid == 1, 'springgreen3', 'tomato')))
points(set, pch = 21, bg = ifelse(set[, 3] == 2, 'blue3', ifelse(set[, 3] == 1, 'green4', 'red3')))



                                ### Linear Discriminant Analysis (LDA)  ####

# In a nutshell:
# From n independent variables of your dataset, LDA extracts p <= n new independent variables that
# separate the most classes of the dependent variable

# The Fact that dependent variables are considered, LDA is a "supervised" model 

# assumption: Linear separability and normality 
# assumes a multivariate normal distribution for the predictors and so works well in case of continuous quantitative variables,
# but fails when categorical variables are to be used as predictors. Besides, it requires linear
# relationships between dependent and independent variables


# http://www.saedsayad.com/lda.htm

# LDA Code:

# Importing the dataset
dataset = read.csv('Section 44 - Linear Discriminant Analysis (LDA)/LDA/LDA/Wine.csv')
head(dataset)
str(dataset)

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Customer_Segment, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
training_set[-14] = scale(training_set[-14])
test_set[-14] = scale(test_set[-14])

# Applying LDA
library(MASS)
lda = lda(formula = Customer_Segment ~ ., data = training_set)
  lda
training_set = as.data.frame(predict(lda, training_set))
  head(training_set)
training_set = training_set[c(5, 6, 1)] # taking LD1 and LD2
test_set = as.data.frame(predict(lda, test_set))
test_set = test_set[c(5, 6, 1)]

# Fitting SVM to the Training set
# install.packages('e1071')
library(e1071)
classifier = svm(formula = class ~ .,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'linear')

classifier

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-3])

# Making the Confusion Matrix
cm = table(test_set[, 3], y_pred)
cm

# Visualising the Training set results
library(ElemStatLearn)
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('x.LD1', 'x.LD2')
y_grid = predict(classifier, newdata = grid_set)
plot(set[, -3],
     main = 'SVM (Training set)',
     xlab = 'LD1', ylab = 'LD2',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 2, 'deepskyblue', ifelse(y_grid == 1, 'springgreen3', 'tomato')))
points(set, pch = 21, bg = ifelse(set[, 3] == 2, 'blue3', ifelse(set[, 3] == 1, 'green4', 'red3')))

# Visualising the Test set results
library(ElemStatLearn)
set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('x.LD1', 'x.LD2')
y_grid = predict(classifier, newdata = grid_set)
plot(set[, -3], main = 'SVM (Test set)',
     xlab = 'LD1', ylab = 'LD2',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 2, 'deepskyblue', ifelse(y_grid == 1, 'springgreen3', 'tomato')))
points(set, pch = 21, bg = ifelse(set[, 3] == 2, 'blue3', ifelse(set[, 3] == 1, 'green4', 'red3')))




                                              ### Kernel PCA  ####

# Both LDA and PCA deals where data is linearly separable. What if its not?
# we use Kernel PCA

# same fundamental as seen in kernel SVM, where we map the data to higher dimension 
# and there we extract new principal components


# Kernel PCA Code:

# Importing the dataset
dataset = read.csv('Section 45 - Kernel PCA/Kernel-PCA/Kernel_PCA/Social_Network_Ads.csv')
head(dataset)
dataset = dataset[, 3:5]

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
training_set[, 1:2] = scale(training_set[, 1:2])
test_set[, 1:2] = scale(test_set[, 1:2])

# Applying Kernel PCA
# install.packages('kernlab')
library(kernlab)
kpca = kpca(~., data = training_set[-3], kernel = 'rbfdot', features = 2)

  # Differene Kernels available for KPCA:
# rbfdot - Radial Basis kernel function "Gaussian"
# polydot - Polynomial kernel function
# vanilladot -  Linear kernel function
# tanhdot - Hyperbolic tangent kernel function
# laplacedot - Laplacian kernel function
# besseldot - Bessel kernel function
# anovadot - ANOVA RBF kernel function
# splinedot - Spline kernel

training_set_pca = as.data.frame(predict(kpca, training_set))
training_set_pca$Purchased = training_set$Purchased
test_set_pca = as.data.frame(predict(kpca, test_set))
test_set_pca$Purchased = test_set$Purchased

# Fitting Logistic Regression to the Training set
classifier = glm(formula = Purchased ~ .,
                 family = binomial,
                 data = training_set_pca)

# Predicting the Test set results
prob_pred = predict(classifier, type = 'response', newdata = test_set_pca[-3])
y_pred = ifelse(prob_pred > 0.5, 1, 0)

# Making the Confusion Matrix
cm = table(test_set_pca[, 3], y_pred)
cm

# accuracy in %
100*(cm[1,1]+cm[2,2])/sum(cm)

# Visualising the Training set results
# install.packages('ElemStatLearn')
library(ElemStatLearn)
set = training_set_pca
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('V1', 'V2')
prob_set = predict(classifier, type = 'response', newdata = grid_set)
y_grid = ifelse(prob_set > 0.5, 1, 0)
plot(set[, -3],
     main = 'Logistic Regression (Training set)',
     xlab = 'PC1', ylab = 'PC2',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

# Visualising the Test set results
# install.packages('ElemStatLearn')
library(ElemStatLearn)
set = test_set_pca
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('V1', 'V2')
prob_set = predict(classifier, type = 'response', newdata = grid_set)
y_grid = ifelse(prob_set > 0.5, 1, 0)
plot(set[, -3],
     main = 'Logistic Regression (Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))



                                          ### ~~~  Model Selection and Boosting ~~~  ####

library(data.table)
library(dplyr)
library(ggplot2)
setwd("~/SWARIT/Udemy/Machine Learning A-Z Template Folder/Part 10 - Model Selection & Boosting/")

# After we built our Machine Learning models, some questions remained unanswered:
#   
# 1. How to deal with the bias variance tradeoff when building a model and evaluating its performance ?
# 2. How to choose the optimal values for the hyperparameters (the parameters that are not learned) ?
# 3. How to find the most appropriate Machine Learning model for my business problem ?

# In this part we will answer these questions thanks to Model Selection techniques including:
# 1. k-Fold Cross Validation
# 2. Grid Search

                                          ### K-Fold Cross Validation ####



# Importing the dataset
dataset = read.csv('Section 48 - Model Selection/Model-Selection/Model_Selection/Social_Network_Ads.csv')
head(dataset)
dataset = dataset[3:5]

# Encoding the target feature as factor
dataset$Purchased = factor(dataset$Purchased, levels = c(0, 1))

# Splitting the dataset into the Training set and Test set
library(caTools)
set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
training_set[-3] = scale(training_set[-3])
test_set[-3] = scale(test_set[-3])

# Fitting Kernel SVM to the Training set
library(e1071)
classifier = svm(formula = Purchased ~ .,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'radial')

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-3])

# Making the Confusion Matrix
cm = table(test_set[, 3], y_pred)
cm

# accuracy in %
100*(cm[1,1]+cm[2,2])/sum(cm)

# Applying k-Fold Cross Validation
library(caret)
folds = createFolds(training_set$Purchased, k = 10) # 10 folds
folds

# list of 10 different test folds comprising of our dataset2 
cv = lapply(folds, function(x) {
  training_fold = training_set[-x, ] # 270 rows
  test_fold = training_set[x, ] # 30 rows
  classifier = svm(formula = Purchased ~ .,
                   data = training_fold,
                   type = 'C-classification',
                   kernel = 'radial')
  y_pred = predict(classifier, newdata = test_fold[-3])
  cm = table(test_fold[, 3], y_pred)
  accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
  return(accuracy)
})

cv # accuracies of all 10 different folds

accuracy = mean(as.numeric(cv))
accuracy*100 # better than single test-train kernel svm


                                      #### Grid Search #####

# improving the models by tweaking the hyperparameters
# Grid search will help us determine the optimal values for these hyperparameters

# we will use the "caret" package, which is the most used machine learning package used in R
# it also gives the optimal values of the parameters for the model

# available models:
# http://topepo.github.io/caret/available-models.html

# Importing the dataset
dataset = read.csv('Section 48 - Model Selection/Model-Selection/Model_Selection/Social_Network_Ads.csv')
dataset = dataset[3:5]

# Encoding the target feature as factor
dataset$Purchased = factor(dataset$Purchased, levels = c(0, 1))

# Splitting the dataset into the Training set and Test set
library(caTools)
set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
training_set[-3] = scale(training_set[-3])
test_set[-3] = scale(test_set[-3])

# Fitting Kernel SVM to the Training set
library(e1071)
classifier = svm(formula = Purchased ~ .,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'radial')

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-3])

# Making the Confusion Matrix
cm = table(test_set[, 3], y_pred)
cm

# accuracy in %
100*(cm[1,1]+cm[2,2])/sum(cm)

# Applying k-Fold Cross Validation
library(caret)
folds = createFolds(training_set$Purchased, k = 10)
cv = lapply(folds, function(x) {
  training_fold = training_set[-x, ]
  test_fold = training_set[x, ]
  classifier = svm(formula = Purchased ~ .,
                   data = training_fold,
                   type = 'C-classification',
                   kernel = 'radial')
  y_pred = predict(classifier, newdata = test_fold[-3])
  cm = table(test_fold[, 3], y_pred)
  accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
  return(accuracy)
})
accuracy = mean(as.numeric(cv))



# Applying Grid Search to find the best parameters (also to compare model performance with k fold cv)
# install.packages('caret')
library(caret)
classifier = caret::train(form = Purchased ~ ., data = training_set, method = 'svmRadial')
classifier
classifier$bestTune # optimal values




                                                    #####  XGBoost ######

# high performance 
# fast execution speed
# can keep the interpretation of the problem and the model 

# we will use the same problem as we took in ANN where we saw 86% accuracy

# For XGBoost, feature scaling is not required unlike the deep learning models
# since XGBoost is gradient boosting algorithm for decision trees, scaling is not required

# XGBoost Code:
# Importing the dataset
dataset = read.csv('Section 49 - XGBoost/XGBoost/XGBoost/Churn_Modelling.csv')
dataset = dataset[4:14]

# Encoding the categorical variables as factors
dataset$Geography = as.numeric(factor(dataset$Geography,
                                      levels = c('France', 'Spain', 'Germany'),
                                      labels = c(1, 2, 3)))
dataset$Gender = as.numeric(factor(dataset$Gender,
                                   levels = c('Female', 'Male'),
                                   labels = c(1, 2)))

# Splitting the dataset into the Training set and Test set
library(caTools)
set.seed(123)
split = sample.split(dataset$Exited, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Fitting XGBoost to the Training set
library(xgboost)
classifier = xgboost(data = as.matrix(training_set[-11]), label = training_set$Exited, nrounds = 10)
classifier

# Predicting the Test set results
y_pred = predict(classifier, newdata = as.matrix(test_set[-11]))
y_pred = ifelse(y_pred >= 0.5, 1, 0)

# Making the Confusion Matrix
cm = table(test_set[, 11], y_pred)
cm

# Applying k-Fold Cross Validation
library(caret)
folds = createFolds(training_set$Exited, k = 10)
cv = lapply(folds, function(x) {
  training_fold = training_set[-x, ]
  test_fold = training_set[x, ]
  classifier = xgboost(data = as.matrix(training_set[-11]), label = training_set$Exited, nrounds = 10)
  y_pred = predict(classifier, newdata = as.matrix(test_fold[-11]))
  y_pred = (y_pred >= 0.5)
  cm = table(test_fold[, 11], y_pred)
  accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
  return(accuracy)
})

accuracy = mean(as.numeric(cv))
accuracy
# better than ANN


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
