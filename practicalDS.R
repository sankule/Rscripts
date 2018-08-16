
#### Practical DS with R ####



################# ****************   PART - 1  *********************  ################# 


# example 1.1 of section 1.2.3 
# (example 1.1 of section 1.2.3)  : The data science process : Stages of a data science project : Modeling 
# Title: Building a decision tree 

library('rpart')
load('/home/ideaplunge/Downloads/GCDData.RData')
head(d)
str(d)
model <- rpart(Good.Loan ~
                 Duration.in.month +
                 Installment.rate.in.percentage.of.disposable.income +
                 Credit.amount  +
                 Other.installment.plans,
               data=d,
               control=rpart.control(maxdepth=4),
               method="class")
model



# example 1.2 of section 1.2.4 
# (example 1.2 of section 1.2.4)  : The data science process : Stages of a data science project : Model evaluation and critique 

################# Plotting the confusion matrix #################
creditdata <- d
head(creditdata)
str(creditdata)
resultframe <- data.frame(Good.Loan=creditdata$Good.Loan,
                          pred=predict(model, type="class"))
rtab <- table(resultframe) 	# Note: 1 
rtab
##           pred
## Good.Loan  BadLoan GoodLoan
##   BadLoan       41      259
##   GoodLoan      13      687

summary.factor(creditdata$Good.Loan)

sum(diag(rtab))/sum(rtab)  	# Note: 2 
## [1] 0.728
sum(rtab[1,1])/sum(rtab[,1]) 	# Note: 3 
## [1] 0.7592593
sum(rtab[1,1])/sum(rtab[1,]) 	# Note: 4 
## [1] 0.1366667
sum(rtab[2,1])/sum(rtab[2,]) 	# Note: 5 
## [1] 0.01857143

# Note 1: 
#   Create the confusion matrix. Rows represent 
#   actual loan status; columns represent predicted 
#   loan status. The diagonal entries represent 
#   correct predictions. 

# Note 2: 
#   accuracyconfusion matrixOverall model accuracy: 73% of the predictions 
#   were correct. 

# Note 3: 
#   precisionconfusion matrixModel precision: 76% of the applicants 
#   predicted as bad really did default. 

# Note 4: 
#   recallconfusion matrixModel recall: the model found 14% of the 
#   defaulting loans. 

# Note 5: 
#   false positive rateconfusion matrixFalse positive rate: 2% of the good applicants 
#   were mistakenly identified as bad. 


# example 1.3 of section 1.3.1 
# (example 1.3 of section 1.3.1)  : The data science process : Setting expectations : Determining lower and upper bounds on model performance 
# Title: Plotting the relation between disposable income and loan outcome 


# One thing to look at is what statisticians call the unexplainable variance: how much
# of the variation in your output can’t be explained by your input variables.
# Let’s take a very simple example: suppose you want to use the rule of thumb that loans that equal
# more than 15% of the borrower’s disposable income will default; otherwise, loans are
# good. You want to know if this rule alone will meet your goal of predicting bad loans
# with at least 85% accuracy. Let’s consider the two populations next.

tab1 <- as.table(matrix(data=c(50,6,0,44),nrow=2,ncol=2))
dimnames(tab1) <- list('loan.as.pct.disposable.income'=
                         c('LT.15pct','GT.15pct'),
                       'loan.quality.pop1'=
                         c('goodloan','badloan'))
tab2 <- as.table(matrix(data=c(34,18,16,32),nrow=2,ncol=2))
dimnames(tab2) <- list('loan.as.pct.disposable.income'=
                         c('LT.15pct','GT.15pct'),
                       'loan.quality.pop2'=
                         c('goodloan','badloan'))
tab1
##                              loan.quality.pop1 	# Note: 1 
## loan.as.pct.disposable.income goodloan badloan
##                      LT.15pct       50       0
##                      GT.15pct        6      44
sum(diag(tab1))/sum(tab1)                  	# Note: 2 
## [1] 0.94

tab2
##                              loan.quality.pop2  	# Note: 3 
## loan.as.pct.disposable.income goodloan badloan
##                      LT.15pct       34      16
##                      GT.15pct       18      32
sum(diag(tab2))/sum(tab2)
## [1] 0.66                                                        	# Note: 4

# Note 1: 
#   The count of correct predictions is on the 
#   diagonal of tab1. In this first population, all 
#   the loans that were less than 15% of disposable 
#   income were good loans, and all but six of the 
#   loans that were greater than 15% of disposable 
#   income defaulted. So you know that 
#   loan.as.pct.disposable.income models loan quality 
#   well in this population. Or as statisticians might 
#   say, loan.as.pct.disposable.income “explains” the 
#   output (loan quality). 

# Note 2: 
#   In fact, it’s 94% accurate. 

# Note 3: 
#   In the second population, about a third of 
#   the loans that were less than 15% of disposable 
#   income defaulted, and over half of the loans that 
#   were greater than 15% of disposable income were 
#   good. So you know that 
#   loan.as.pct.disposable.income doesn’t model loan 
#   quality well in this population. 

# Note 4: 
#   The rule of thumb that we chose is only 66% accurate. 


################# ifelse - vectorized #################
custdata$is.employed.fix <- ifelse(is.na(custdata$is.employed),
                                   "not in active workforce",
                                   ifelse(custdata$is.employed==T,
                                          "employed",
                                          "not employed"))


# (informalexample 4.3 of section 4.1.1)  : Managing data : Cleaning data : Treating missing values (NAs) 
meanIncome <- mean(custdata$Income, na.rm=T) 	# Note: 1 
Income.fix <- ifelse(is.na(custdata$Income),
                     meanIncome,
                     custdata$Income)
summary(Income.fix)

##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
##       0   35000   66200   66200   66200  615000

# Note 1: 
#   Don’t forget the argument "na.rm=T"! 
#   Otherwise, the mean() function will include the 
#   NAs by default, and meanIncome will be NA. 

################## Converting missing numeric data to a level #################

breaks <-c(0, 10000, 50000, 100000, 250000, 1000000)           	# Note: 1 

Income.groups <- cut(custdata$income,
                     breaks=breaks, include.lowest=T)  	# Note: 2 

summary(Income.groups)                                        	# Note: 3 

##  [0,1e+04] (1e+04,5e+04] (5e+04,1e+05] (1e+05,2.5e+05] (2.5e+05,1e+06]
##         63           312           178              98              21
##       NA's
##        328

Income.groups <- as.character(Income.groups)                   	# Note: 4 

Income.groups <- ifelse(is.na(Income.groups),                  	# Note: 5 
                        "no income", Income.groups)

summary(as.factor(Income.groups))

##  (1e+04,5e+04] (1e+05,2.5e+05] (2.5e+05,1e+06]  (5e+04,1e+05]  [0,1e+04]
##            312              98              21            178         63
##      no income
##            328

# Note 1: 
#   Select some income ranges of interest. To 
#   use the cut() function, the upper and lower bounds 
#   should encompass the full income range of the 
#   data. 

# Note 2: 
#   Cut the data into income ranges. The 
#   include.lowest=T argument makes sure that zero 
#   income data is included in the lowest income range 
#   category. By default it would be excluded. 

# Note 3: 
#   The cut() function produces factor 
#   variables. Note the NAs are preserved. 

# Note 4: 
#   To preserve the category names before adding 
#   a new category, convert the variables to strings. 

# Note 5: 
#   Add the "no income" category to replace the 
#   NAs. 



################# Normalizing income by state #################

medianincome <- aggregate(income~state.of.res,custdata,FUN=median)
colnames(medianincome) <- c('State','Median.Income')
summary(medianincome)  	# Note: 1 

##         State    Median.Income
##            : 1   Min.   :37427
##  Alabama   : 1   1st Qu.:47483
##  Alaska    : 1   Median :52274
##  Arizona   : 1   Mean   :52655
##  Arkansas  : 1   3rd Qu.:57195
##  California: 1   Max.   :68187
##  (Other)   :46


custdata <- merge(custdata, medianincome,
                  by.x="state.of.res", by.y="State")  	# Note: 2 

summary(custdata[,c("state.of.res", "income", "Median.Income")]) 	# Note: 3 

##        state.of.res     income       Median.Income
##  California  :100   Min.   : -8700   Min.   :37427
##  New York    : 71   1st Qu.: 14600   1st Qu.:44819
##  Pennsylvania: 70   Median : 35000   Median :50977
##  Texas       : 56   Mean   : 53505   Mean   :51161
##  Michigan    : 52   3rd Qu.: 67000   3rd Qu.:55559
##  Ohio        : 51   Max.   :615000   Max.   :68187
##  (Other)     :600

custdata$income.norm <- with(custdata, income/Median.Income) 	# Note: 4 
summary(custdata$income.norm)

##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
## -0.1791  0.2729  0.6992  1.0820  1.3120 11.6600

# Note 1: 
#   medianincome is a data frame of median 
#   income by state. 

# Note 2: 
#   Merge median income information into the 
#   custdata data frame by matching the column 
#   custdata$state.of.res to the column 
#   medianincome$State. 

# Note 3: 
#   Median.Income is now part of custdata. 

# Note 4: 
#   Normalize income by Median.Income. 


################# Converting age into ranges ################# 

brks <- c(0, 25, 65, Inf)  	# Note: 1 
custdata$age.range <- cut(custdata$age,
                          breaks=brks, include.lowest=T) 	# Note: 2 
summary(custdata$age.range) 	# Note: 3 

##   [0,25]  (25,65] (65,Inf]
##       56      732      212

# Note 1: 
#   Select the age ranges of interest. The upper 
#   and lower bounds should encompass the full range 
#   of the data. 

# Note 2: 
#   Cut the data into age ranges. The 
#   include.lowest=T argument makes sure that zero age 
#   data is included in the lowest age range category. 
#   By default it would be excluded. 

# Note 3: 
#   The output of cut() is a factor variable.



################# Summarizing age ################# 

summary(custdata$age)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
##     0.0    38.0    50.0    51.7    64.0   146.7
meanage <- mean(custdata$age)  	# Note: 1 
stdage <- sd(custdata$age)     	# Note: 2 
meanage
## [1] 51.69981
stdage
## [1] 18.86343
custdata$age.normalized <- (custdata$age-meanage)/stdage 	# Note: 3 
summary(custdata$age.normalized)
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max.
## -2.74100 -0.72630 -0.09011  0.00000  0.65210  5.03500

# Note 1: 
#   Take the mean. 

# Note 2: 
#   Take the standard deviation. 

# Note 3: 
#   Use the mean value as the origin (or 
#   reference point) and rescale the distance from the 
#   mean by the standard deviation. 



signedlog10 <- function(x) {
  ifelse(abs(x) <= 1, 0, sign(x)*log10(abs(x)))
}


################# Splitting into test and training using a random group mark ################# 

custdata$gp <- runif(dim(custdata)[1])  	# Note: 1 
testSet <- subset(custdata, custdata$gp <= 0.1) 	# Note: 2 
trainingSet <- subset(custdata, custdata$gp > 0.1) 	# Note: 3 
dim(testSet)[1]
## [1] 93
dim(trainingSet)[1]
## [1] 907

# Note 1: 
#   dim(custdata) returns the number of rows and 
#   columns of the data frame as a vector, so 
#   dim(custdata)[1] returns the number of rows. 

# Note 2: 
#   Here we generate a test set of about 10% of 
#   the data (93 customers—a little over 9%, actually) 
#   and train on the remaining 90%. 

# Note 3: 
#   Here we generate a training using the 
#   remaining data. 



#################  Ensuring test/train split doesn’t split inside a household ################# 

hh <- unique(hhdata$household_id) 	# Note: 1 
households <- data.frame(household_id = hh, gp = runif(length(hh))) 	# Note: 2 
hhdata <- merge(hhdata, households, by="household_id") 	# Note: 3

# Note 1: 
#   Get all unique household IDs from your data 
#   frame. 

# Note 2: 
#   Create a temporary data frame of household IDs 
#   and a uniformly random number from 0 to 1. 

# Note 3: 
#   Merge new random sample group column back into 
#   original data frame. 

