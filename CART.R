
## About the script ##

#'  This project is used for POCLS data analysis.
#'  The project aimed to identify risk profiles (child’s demographics, pre-care, placement, and caregiver-related factors)
#'  for socio-emotional problems of children at their first survey in out-of-home care (OOHC) 
#'  using a decision-tree analytic approach

# Install required packages ----
install.packages('rpart')
install.packages('caret')
install.packages('rpart.plot')
install.packages('rattle')

# loading libraries
library(rpart,quietly = TRUE)
library(caret,quietly = TRUE)
library(rpart.plot,quietly = TRUE)
library(rattle,quietly = TRUE)

## INSPECT THE DATA ## ----
# disply the structure of the data

str(newdata)

# data cleaning 

newdata$Sub_group <- NULL #remove the correlated variable

# nrows with missing values

nrow(newdata) -sum(complete.cases(newdata))

## DATA EXPLORATION
# analyzing the variables one by one
table(newdata$bit_cbcl, newdata$Age_at_assessment) #age at assessment
table(newdata$bit_cbcl, newdata$KD_ADMIN_STUDYCHILD_ATSI) #Indiginous status
table(newdata$bit_cbcl, newdata$CAT_AGE_AT_ENTRY_YR) #age at first entry into care
table(newdata$bit_cbcl, newdata$Placement2)#Placement type during the interview period
table(newdata$bit_cbcl, newdata$dom_abuse)#pre-care maltreatment
table(newdata$bit_cbcl, newdata$Sub_group)#substantiated 
table(newdata$bit_cbcl, newdata$rosh_group)#ROSH
table(newdata$bit_cbcl, newdata$caregivers_stress)#carer stress level
table(newdata$bit_cbcl, newdata$placementnumber) #placement number
table(newdata$bit_cbcl, newdata$caseworker_assist) #careriver support from caseworker

# plot a graph that represents the split for each the 11 variables
numbre.perfect.splits <- apply(X=newdata[-1], MARGIN = 2, FUN = function(col){
  t <- table(newdata$bit_cbcl, col)
  sum(t == 0)
})

## STATISTICAL ANALYSIS ## -----

## Data preparation ## 
# data slicing
# The data randomley sliced into train and test data in 80% to 20% percentage
set.seed(12345)
train.a <- sample(1:nrow(newdata), size = ceiling(0.8*nrow(newdata)),replace = FALSE)
# train set
newdata_tarin <- newdata[train.a,]
# test set
newdata_test <- newdata[-train.a,]

# Minimize the missclassfication we will assign a penality 10x bigger that the penality for classifying 
# penality matrix

penality.matrix <- matrix(c(0,1,10,0), byrow = T, nrow = 2)

# Building a model

ytree<- rpart(bit_cbcl ~.,
              data = newdata_tarin,
              parms = list(loss = penality.matrix),
              method = "class")

# visualize the decision tree

rpart.plot(ytree,nn = TRUE)

# testing the model

pred <- predict(object = ytree, newdata_test[-1], type = "class")

# calculating accuracy

t <- table(newdata_test$bit_cbcl, pred) > confusionMatrix(t)

## Modelling ## ----

## logistic regression

library(mice)
library(cowplot)
library(caTools)
library(modeest)
library(Amelia)

str(newdata)

# age at interview

addmargins(table(newdata$Age_at_assessment, newdata$bit_cbcl)) #2x2 table 

#age at entry 

addmargins(table(newdata$age_entry, newdata$bit_cbcl)) #2x2 table 

#cargiver_stress

addmargins(table(newdata$caregivers_stress, newdata$bit_cbcl)) #52 rows missed 

#ATSI

addmargins(table(newdata$KD_ADMIN_STUDYCHILD_ATSI, newdata$bit_cbcl)) #2x2 table 

#pre_care maltreatment - types of maltreatment 

addmargins(table(logidata$dom_abuse, logidata$bit_cbcl)) #5 rows missed


#pre_care maltreatment - number of ROSH 

addmargins(table(newdata$rosh_group, newdata$bit_cbcl)) #5 rows missed

#number of placement changes 

addmargins(table(logidata$placementnumber, logidata$bit_cbcl)) # 4 rows missed

#case workers support 

addmargins(table(newdata$caseworker_assist, newdata$bit_cbcl)) #120 rows missed

#case workers acess 

addmargins(table(logidata$caseworker_access, logidata$bit_cbcl)) #119 rows missed

#placement type during the interview

addmargins(table(logidata$Placement, logidata$bit_cbcl))

#subset cyp who have measured for socio-emotional difficuluties 

logidata <- newdata %>% filter(!is.na(bit_cbcl),)

#delete duplicate column

logidata <- logidata %>% 
  select(-c(placementnumber, Placement2, Sub_group, rosh_group, CAT_AGE_AT_ENTRY_YR, KD_ADMIN_CHILD_AGE, age_entry))
str(logidata)
logidata <- as_tibble(logidata)

#map missing 
missmap(obj = logidata)
str(logidata)

#used mode(the most frequent value) imputation for total_plc, rosh_group and maltreatment type 
#because they conatins only 5 missing values

logidata$total_plc[is.na(logidata$total_plc)] <- mlv(logidata$total_plc, method = "mfv")

#imputing with MICE
factor_vars <- c("caseworker_access", "dom_abuse","Placement","caseworker_assist", "caregivers_stress")

logidata[factor_vars] <- lapply(logidata[factor_vars], function(x) as.factor(x))

impute_mice <- mice(logidata[, !names(logidata) %in% c("bit_cbcl","Age_at_assessment","age_entry","KD_ADMIN_STUDYCHILD_ATSI")], method = "rf")
result_mice <- complete(impute_mice)

#Check continous var before and after imputation
denisty_before <- ggplot(
  logidata, aes(x = rosh_sum.A)) +
  geom_density(fill = "#e74c3c", alpha = 0.6) +
  labs(title = "ROSH:Before") + theme_classic()

denisty_after<- ggplot(
  result_mice, aes(x = rosh_sum.A)) +
  geom_density(fill = "#2ecc71", alpha = 0.6) +
  labs(title = "ROSH:After") + theme_classic()                    

plot_grid(denisty_before,denisty_after) #the distribution stayed the same (small missing values) 

#assign the imputed results to the original datasets
logidata$age_entry <- as.factor(logidata$age_entry)
logidata$rosh_sum.A <- result_mice$rosh_sum.A
logidata$total_plc <- result_mice$total_plc
logidata$dom_abuse <- as.factor(result_mice$dom_abuse)
logidata$caseworker_assist <- as.factor(result_mice$caseworker_assist)
logidata$caseworker_access <- as.factor(result_mice$caseworker_access)
logidata$caregivers_stress <- as.factor(result_mice$caregivers_stress)

missmap(logidata) #after imputation - no missing

## CART ##
#split the data set into training and testing subsets

set.seed(42)

#split the data randomly in 2 70:30 ratio

sample_split <- sample.split(Y = logidata$bit_cbcl, SplitRatio = 0.7) 
train_set <- subset(x = logidata, sample_split == TRUE)
test_set <- subset(x = logidata, sample_split == FALSE)

#Train the model using glm() function 

logistic <- glm(bit_cbcl~., data = train_set, family = "binomial")
summary(logistic)

#explore variable importance

importances <- varImp(logistic)
importances %>% 
  arrange(desc(Overall)) %>% 
  top_n(10)
#generating prediction 

probs <- predict(logistic, newdata = test_set, type = "response")
pred <- ifelse(probs >0.5, 1, 0)
confusionMatrix(factor(pred), factor(test_set$bit_cbcl), positive = as.character(1))