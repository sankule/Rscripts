###### *************** STATS WITH R ************** #########

### QuaLitative Data #####

library(MASS)
head(painters)
str(painters)
  # frequency distribution
school.freq <- table(painters$School)
school.freq
    # in column format
cbind(school.freq)

  # relative frequency = frequency / sample size
school.relfreq <- school.freq/nrow(painters)
school.relfreq
options(digits = 4)
school.relfreq

  # Bar plot
barplot(school.freq)
barplot(school.relfreq)
pie(school.freq)

### QuanTitative Data #####
head(faithful)
str(faithful)

duration.cut <- cut(faithful$eruptions, breaks = seq(1.5, 5.5, by=0.5), right=FALSE)
table(duration.cut)

  # cumulative frequency
duration.cumfreq <- cumsum(table(duration.cut))
duration.cumfreq
  
  # Stem and Leaf
stem(faithful$eruptions)

  # scatter plot
plot(x = faithful$eruptions, y = faithful$waiting, xlab="Eruption duration", ylab="Time waited") 
  # linear regression line
abline(lm(faithful$waiting ~ faithful$eruptions))

  # quartile
quantile(faithful$eruptions)
summary(faithful$eruptions)

  # nth percentile
quantile(faithful$eruptions, probs = c(0.1,0.45,0.65,0.90,0.95,0.99))

### Numeric Measures #####
  # variance
var(faithful$eruptions)
sum(((faithful$eruptions-mean(faithful$eruptions))^2))/(nrow(faithful)-1)

  # correlation coefficient 
    # covariance divided by the product of their individual standard deviations.
cor(faithful$eruptions, faithful$waiting) # high correlation 

  # skewness
# -ve => left skewed ; +ve => right skewed
library(e1071)
e1071::skewness(faithful$eruptions)

 # kurtosis
# -ve => thin tails ; +ve => fat tailed
e1071::kurtosis(faithful$eruptions)

### Probability Distributions #####
  # Binomial
dbinom(x = 4, size = 12, prob = 0.2) # answering exactly 4 correct
pbinom(q = 4, size = 12, prob = 0.2) # answering 4 or less correct

  # Poisson
ppois(q = 16, lambda = 12, lower.tail = T)
  # probability of passing 16 cars or less for lamb = 12 car per min

  # Continuius Uniform Distribution
runif(10, min = 0, max = 20) # 10 random number between 0 and 20

  # Exponential Distribution
    # describes the arrival time of a randomly recurring independent event sequence
pexp(2, rate = 1/3)

  # Normal Distribution
pnorm(84, mean=72, sd=15.2, lower.tail=FALSE) 
pnorm(84, mean=72, sd=15.2)

  # Chi Squared Distribution
    # is X1,X2,…,Xm are m independent random variables having the standard normal distribution
    # then V = X1^2 + X2^2 + X3^2 + ... + Xm^2 is a chi-squared distribution with 
    # m degrees of freedom

#  95th percentile of the Chi-Squared distribution with 7 degrees of freedom.
qchisq(p = 0.95, df = 7)

  # Student T distribution
      # if Z is standard normal distribution and V is chi-squared with m d.o.f
      # then t = Z/(V/m)^0.5 is t-distributed with m d.o.f

# 2.5th and 97.5th percentiles of the Student t distribution with 5 degrees of freedom.
qt(c(0.025,0.975), df = 5)

  
  # F Distribution
      # if V1 and V2 are chi-squared with m1 and m2 d.o.f respectively
      # then F = (V1/m1)/(V2/m2) is a F distribution with (m1,m2) d.o.f
# 95th percentile of the F distribution with (5, 2) degrees of freedom.
qf(0.95, df1 = 5, df2 = 2)


### Interval Estimation #####
library(MASS)  
  
      # Interval Estimate of Population Mean with Known Variance

# Assume the population standard deviation σ of the student height in survey is 9.48. 
# Find the margin of error and interval estimate at 95% confidence level.  
  # M1
n <- length(na.omit(survey$Height))
sigma <- 9.48
sem <- sigma/sqrt(n) # standard error of mean
sem
E <- qnorm(0.975)*sem # Margin of Error
E
mean(na.omit(survey$Height)) + c(-E, +E) # 95% (two tailed) Confidence Interval

  # M2
TeachingDemos::z.test(x = na.omit(survey$Height), sd = sigma)


        # Interval Estimate of Population Mean with Unknown Variance

# M2
t.test(na.omit(survey$Height))

        # Interval Estimate of Population Proportion
# Compute the margin of error and estimate interval for the female students proportion in survey at 95% confidence
gender.response = na.omit(survey$Sex)
n = length(gender.response)
k = sum(gender.response == "Female")
prop.test(k, n) 


### Hypothesis Testing #####

# A type I error is the mishap of falsely rejecting a null hypothesis when the null hypothesis is true.
# The probability of committing a type I error is called the significance level of the hypothesis testing,
# and is denoted by the Greek letter α .




### Type II Error #####

### Inference About Population #####

### Goodness of Fit #####

### Analysis of variance #####

### Simple Linear Regression #####
  eruption.lm = lm(eruptions ~ waiting, data=faithful)
  eruption.lm
  summary(eruption.lm)
  help("summary.lm")

  # confidence interval
    # nterval estimate for the mean of the dependent variable, ¯y , 
    # is called the confidence interval.
  newdata = data.frame(waiting=80)  
  predict(eruption.lm, newdata, interval="confidence") 

  # prediction interval
  predict(eruption.lm, newdata, interval="predict") 

  
### Multiple Linear Regression #####
  head(stackloss)
  stackloss.lm = lm(stack.loss ~ Air.Flow + Water.Temp + Acid.Conc., data=stackloss)
  summary(stackloss.lm)
  
  newdata = data.frame(Air.Flow=72, Water.Temp=20, Acid.Conc.=85)
  predict(object = stackloss.lm, newdata = newdata)
  
  # Significance test
    # from summary, P-values for air flow and water temp are <<0.05, hence significant 
    # on the other hand acid.con p-value is 0.34, thus it is not significant
    # model would work without acid conc
  stackloss_wihoutAcid.lm <- lm(stack.loss ~ Air.Flow + Water.Temp, data=stackloss)
  summary(stackloss_wihoutAcid.lm)
  predict(stackloss_wihoutAcid.lm, newdata)
  
  # Confidence interval
    # nterval estimate for the mean of the dependent variable, ¯y , 
    # is called the confidence interval.
  predict(stackloss.lm, newdata, interval="confidence") 
  
  # Prediction Interval
    #  interval estimate of the dependent variable y is called the prediction interval.
  predict(stackloss.lm, newdata, interval="predict") 
  
  
### Logistic Regression #####
  am.glm = glm(formula=am ~ hp + wt, data=mtcars,family=binomial)
  summary(am.glm)
  newdata = data.frame(hp=120, wt=2.8)
  predict(am.glm, newdata, type="response") 
  

###### ******* GPU COMPUTING WITH R ******** #########
