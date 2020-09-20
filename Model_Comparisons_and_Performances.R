#read the data file present in the your working directory
#---------------------------------------------------------#

raw_data <- read.csv('COMPAS recidivate.csv')


#to check number of observations and variables
#----------------------------------------------#

dim(raw_data)

#to compress warnings
#---------------------#

options(warn=-1)

#To check all features
#---------------------#
str(raw_data)
str(df)

#No all the rows will be selected for the first round of analysis and there are a number of reasons remove rows because of missing data:

#If the charge date of a defendants Compas scored crime was not within 30 days from when the person was arrested, 
#we assume that because of data quality reasons, that we do not have the right offense.
#We coded the recidivist flag -- is_recid -- to be -1 if we could not find a compas case at all.
#In a similar vein, ordinary traffic offenses -- those with a c_charge_degree of 'O' -- will not result in Jail time are removed (only two of them).


library(dplyr)

#Created a dataframe and filtered the raw_data on the basis of above discussed criterias
#----------------------------------------------------------------------------------------#

df <- dplyr::select(raw_data, id, sex, age, age_cat, race, juv_fel_count, decile_score, juv_misd_count, juv_other_count, priors_count, days_b_screening_arrest,
                    c_jail_in, c_jail_out, c_offense_date, c_arrest_date, c_days_from_compas, c_charge_degree, c_charge_desc, is_recid, r_charge_degree,
                    r_days_from_arrest) %>% 
  filter(days_b_screening_arrest <= 30) %>%
  filter(days_b_screening_arrest >= -30) %>%
  filter(is_recid != -1) %>%
  filter(c_charge_degree != "O") 
nrow(df)

#Feature Engineering
#--------------------#

#To enhance our analysis of the data set, we believe additional features will aid prediction. 
#Hidden within the data set are additional features/variables that could improve the predictive performance of our models. 
#Below is the list of new features and a brief explanation:

# 1> Days spent in Jail

#While we lose the exact dates of when an individual entered and exited jail, we gain the ability to see if the duration or the term of the charge impacts the recidivism rate. 
#To calcuate this value, we subtract the date the person entered jail from the date the person exited jail. 
#This subtraction provides us with the number of days spent in jail.

# conversion
df$date1 <-  as.POSIXct(x=df$c_jail_out,format="%d/%m/%Y")
df$date2 <-  as.POSIXct(x=df$c_jail_in,format="%d/%m/%Y")

# take the difference
df$days_in_jail <- with(df,difftime(time1=date1,time2=date2,units="days"))
df$days_in_jail <- as.numeric(round(df$days_in_jail))


# 2> Number of Junevile Charges (FELONY, MISDEMEANOR, OTHER)
#The original data set provides the number of juvenile charges, separated by type: felony, misdemeanor or other. 
#To analyze the impact of criminal activity from an individual's youth, we summed the counts together. 
#While we may lose the severity of the crime(s), we gain insight into how much juvenile criminal activity, as a whole, feeds into criminal recidivism.

df$juv_total <- df$juv_fel_count + df$juv_misd_count + df$juv_other_count

# 3> Categorise Data

df$risk[df$decile_score == 1 | df$decile_score == 2 | df$decile_score == 3 | df$decile_score == 4 ] = "Low"
df$risk[df$decile_score == 5 | df$decile_score == 6 | df$decile_score == 7                        ] = "Medium"
df$risk[df$decile_score == 8 | df$decile_score == 9 | df$decile_score == 10                       ] = "High"

df$conviction[df$c_charge_degree == "M" ] = "Light Conviction"
df$conviction[df$c_charge_degree == "F" ] = "Heavy Conviction"

# 3> Categroise all records with heavy conviction with 1 and light conviction with 0

df$possible_recid[df$c_charge_degree == "M" ] = 0
df$possible_recid[df$c_charge_degree == "F" ] = 1

#Create and new dataframe and store only limited variables
#----------------------------------------------------------#

dfnew <- subset(df, select = c(id, sex, age, age_cat, race, juv_fel_count, decile_score, juv_misd_count, juv_other_count, priors_count, days_b_screening_arrest,
                               c_jail_in, c_jail_out, c_offense_date, c_arrest_date, c_days_from_compas, c_charge_degree, c_charge_desc, is_recid, r_charge_degree,
                               r_days_from_arrest, 
                               days_in_jail, juv_total, possible_recid))

#Converting race into numeric based on instances
#------------------------------------------------#

if (as.factor(dfnew$race == 'African-American')) {
  dfnew$raceAfricanAmerican = 1
} else {
  dfnew$raceAfricanAmerican = 0
}

dfnew$raceAfricanAmerican[dfnew$race != 'African-American' ] = 0

if (as.factor(dfnew$race == 'Asian')) {
  dfnew$raceAsian = 1
} else {
  dfnew$raceAsian = 0
}

dfnew$raceAsian[dfnew$race != 'Caucasian' ] = 0

if (as.factor(dfnew$race == 'Caucasian')) {
  dfnew$raceCaucasian = 1
} else {
  dfnew$raceCaucasian = 0
}

dfnew$raceCaucasian[dfnew$race != 'Hispanic' ] = 0

if (as.factor(dfnew$race == 'Hispanic')) {
  dfnew$raceHispanic = 1
} else {
  dfnew$raceHispanic = 0
}

dfnew$raceHispanic[dfnew$race != 'Hispanic' ] = 0

if (as.factor(dfnew$race == 'Native American')) {
  dfnew$raceNativeAmerican = 1
} else {
  dfnew$raceNativeAmerican = 0
}

dfnew$raceNativeAmerican[dfnew$race != 'Native American' ] = 0

if (as.factor(dfnew$race == 'Other')) {
  dfnew$raceOther = 1
} else {
  dfnew$raceOther = 0
}

dfnew$raceOther[dfnew$race != 'Other' ] = 0

#Converting sex into numeric based on instances
#------------------------------------------------#

if (as.factor(dfnew$sex == 'Male')) {
  dfnew$sexMale = 1
} else {
  dfnew$sexMale = 0
}

dfnew$sexMale[dfnew$sex != 'Male' ] = 0

if (as.factor(dfnew$sex == 'Female')) {
  dfnew$sexFemale = 1
} else {
  dfnew$sexFemale = 0
}

dfnew$sexFemale[dfnew$sex != 'Female' ] = 0


df_corr <- subset(dfnew, select = c(priors_count, is_recid, juv_total, days_b_screening_arrest, r_days_from_arrest, c_days_from_compas, age, decile_score, days_in_jail, possible_recid, raceAfricanAmerican, raceAsian, raceCaucasian, raceHispanic, raceNativeAmerican, raceOther, sexMale, sexFemale))


#Correlation of decile score with other variables
#------------------------------------------------#

library(corrr)
library(ggplot2)
df_corr %>% correlate() %>% focus(is_recid) %>%
  mutate(rowname = factor(rowname, levels = rowname[order(is_recid)])) %>%
  ggplot(aes(x = rowname, y = is_recid)) +
  geom_bar(stat = "identity", color = "lightgray", horizontal = "TRUE") + theme_bw() + coord_flip() +
  ylab("Correlation with is_recid") +
  xlab("Variable")

df_corr %>% correlate() %>% focus(is_recid) %>%
  mutate(rowname = factor(rowname, levels = rowname[order(is_recid)]))

dfnew <- subset(df_corr, select = c(priors_count, is_recid, juv_total, days_b_screening_arrest, age, decile_score, days_in_jail, possible_recid, raceAfricanAmerican, raceAsian, raceCaucasian, raceHispanic, raceOther, sexMale, sexFemale))



#Data Partition
#---------------#

#Create random training, validation and test sets & Input. 
#Set the fractions of the dataframe you want to split into training, validation and test.

library(caret)
set.seed(1234) #To get reproducible result
ind <- sample(3,nrow(dfnew), replace=TRUE, prob=c(0.6,0.2,0.2))
dfTraining <- dfnew[ind==1,]
dfTest <- dfnew[ind==2,]
dfValidation <- dfnew[ind==3,]


#check dimensions of training & testing set
dim(dfTraining) 
dim(dfTest)
dim(dfValidation)

#Preprocessing and Training
#---------------------------#
anyNA(dfTraining)

################################################################################################################################################

#Create a decision Tree on the basis of abover created datasets
#---------------------------------------------------------------#

#Training the Decision Tree classifier with criterion as information gain

library(rpart)
library(rpart.plot)
library(party)
rPartModel = rpart(is_recid ~ age + priors_count + days_in_jail + days_b_screening_arrest + decile_score + juv_total + possible_recid  + raceAfricanAmerican + raceAsian + raceCaucasian + raceHispanic + raceOther + sexMale + sexFemale, 
                   data=dfTraining, method="class", parms=list(split="information"))

summary(rPartModel)

rpart.plot(rPartModel, extra = 104, nn = TRUE)
        

#default value of complexity parameter is 0.01 and after that rpart package will not split unless 
#we override the cp parameter

printcp(rPartModel)
plotcp(rPartModel)
rpart.control()


#Predicting the pruned tree
#---------------------------#

test_pred1 <- predict(rPartModel,dfTest)
#View(predict)

dfTest$recid_predicted <- predict(rPartModel,dfTest, type = 'class')

#generting the table format
xlab <- table(actualclass=dfTest$is_recid,predictedclass=dfTest$recid_predicted)  

#generating the confusion matrix
library(caret)
confusionMatrix(xlab)

evaluation <- function(model, data, atype) {
  cat("\nConfusion matrix:\n")
  prediction = predict(model, data, type=atype)
  xtab = table(prediction, dfTest$is_recid)
  print(xtab)
  cat("\nEvaluation:\n\n")
  accuracy = sum(prediction == dfTest$is_recid)/length(dfTest$is_recid)
  precision = xtab[1,1]/sum(xtab[,1])
  recall = xtab[1,1]/sum(xtab[1,])
  f = 2 * (precision * recall) / (precision + recall)
  cat(paste("Accuracy:\t", format(accuracy, digits=2), "\n",sep=" "))
  cat(paste("Precision:\t", format(precision, digits=2), "\n",sep=" "))
  cat(paste("Recall:\t\t", format(recall, digits=2), "\n",sep=" "))
  cat(paste("F-measure:\t", format(f, digits=2), "\n",sep=" "))
}
evaluation(rPartModel, dfTest, "class")

library(precrec)
library(ggplot2)
precrec_obj1 <- evalmod(scores = as.numeric(dfTest$recid_predicted), labels = as.numeric(dfTest$is_recid))
autoplot(precrec_obj1)

library(pROC)
pROC_obj1 <- roc(as.numeric(dfTest$is_recid),as.numeric(dfTest$recid_predicted),
                smoothed = TRUE,
                # arguments for ci
                ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                # arguments for plot
                plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                print.auc=TRUE, show.thres=TRUE)


sens.ci <- ci.se(pROC_obj1)
plot(sens.ci, type="shape", col="lightblue")
plot(sens.ci, type="bars")

dfTest$recid_predicted <- as.integer(dfTest$recid_predicted)
dfTest$is_recid <- as.integer(dfTest$is_recid)

library(lift)
plotLift(test_pred1, dfTest$is_recid, cumulative = TRUE, n.buckets = 10)

######################################################################################################################################

#SVM Model
#---------#

library(caret)
set.seed(1234) #To get reproducible result
ind2 <- sample(3,nrow(dfnew), replace=TRUE, prob=c(0.6,0.2,0.2))
dfTraining2 <- dfnew[ind==1,]
dfTest2 <- dfnew[ind==2,]
dfValidation2 <- dfnew[ind==3,]


#check dimensions of training & testing set
dim(dfTraining2) 
dim(dfTest2)
dim(dfValidation2)

dfTraining2$is_recid <- as.factor(dfTraining2$is_recid)
dfTest2$is_recid <- as.factor(dfTest2$is_recid)

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

library(e1071)
set.seed(1800)
svm_Linear <- svm(is_recid ~ age + priors_count + days_in_jail + days_b_screening_arrest + decile_score + juv_total + possible_recid  + raceAfricanAmerican + raceAsian + raceCaucasian + raceHispanic + raceOther + sexMale + sexFemale, data = dfTraining2, method = "svmLinear",
                    trControl=trctrl, cost = 1.5,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)

svm_Linear

grid <- expand.grid(C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))
svm_Linear_Grid <- train(is_recid ~ age + priors_count + days_in_jail + days_b_screening_arrest + decile_score + juv_total + possible_recid  + raceAfricanAmerican + raceAsian + raceCaucasian + raceHispanic + raceOther + sexMale + sexFemale, data = dfTraining2, method = "svmLinear",
                         trControl=trctrl,
                         preProcess = c("center", "scale"),
                         tuneGrid = grid,
                         tuneLength = 10)
svm_Linear_Grid
plot(svm_Linear_Grid)

w = t(svm_Linear$coefs) %*% svm_Linear$SV
w <- apply(w, 2, function(v){sqrt(sum(v^2))})  # weight
w <- sort(w, decreasing = T)
print(w)

test_pred2 <- predict(svm_Linear, newdata = dfTest2, type = "raw")
test_pred2
  
confusionMatrix(table(actualclass=test_pred2,predictedclass=dfTest2$is_recid))

evaluation <- function(model, data, atype) {
  cat("\nConfusion matrix:\n")
  prediction = predict(model, data, type=atype)
  xtab = table(prediction, data$is_recid)
  print(xtab)
  cat("\nEvaluation:\n\n")
  accuracy = sum(prediction == data$is_recid)/length(data$is_recid)
  precision = xtab[1,1]/sum(xtab[,1])
  recall = xtab[1,1]/sum(xtab[1,])
  f = 2 * (precision * recall) / (precision + recall)
  cat(paste("Accuracy:\t", format(accuracy, digits=2), "\n",sep=" "))
  cat(paste("Precision:\t", format(precision, digits=2), "\n",sep=" "))
  cat(paste("Recall:\t\t", format(recall, digits=2), "\n",sep=" "))
  cat(paste("F-measure:\t", format(f, digits=2), "\n",sep=" "))
}
evaluation(svm_Linear, dfTest2, "raw")

library(precrec)
library(ggplot2)
precrec_obj2 <- evalmod(scores = as.numeric(test_pred2), labels = as.numeric(dfTest2$is_recid))
autoplot(precrec_obj2)

library(pROC)
pROC_obj2 <- roc(as.numeric(dfTest2$is_recid),as.numeric(test_pred2),
                smoothed = TRUE,
                # arguments for ci
                ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                # arguments for plot
                plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                print.auc=TRUE, show.thres=TRUE)


sens.ci2 <- ci.se(pROC_obj2)
plot(sens.ci2, type="shape", col="lightblue")
plot(sens.ci2, type="bars")

# And then a lift chart

library(lift)
plotLift(test_pred2, dfTest2$is_recid, cumulative = TRUE, n.buckets = 10)

#########################################################################################################################################

#Random Forest 
#--------------#

library(caret)
set.seed(1234) #To get reproducible result
ind3 <- sample(3,nrow(dfnew), replace=TRUE, prob=c(0.6,0.2,0.2))
dfTraining3 <- dfnew[ind==1,]
dfTest3 <- dfnew[ind==2,]
dfValidation3 <- dfnew[ind==3,]


#check dimensions of training & testing set
dim(dfTraining3) 
dim(dfTest3)
dim(dfValidation3)

dfTraining3$is_recid <- as.factor(dfTraining3$is_recid)
dfTest3$is_recid <- as.factor(dfTest3$is_recid)


library(randomForest)
set.seed(1800)

fit <- randomForest(is_recid ~ age + priors_count + days_in_jail + days_b_screening_arrest + decile_score + juv_total + possible_recid  + raceAfricanAmerican + raceAsian + raceCaucasian + raceHispanic + raceOther + sexMale + sexFemale,
                    data=dfTraining3, 
                    importance=TRUE, 
                    ntree=690)
plot(fit)
varImpPlot(fit)

#Evaluate variable importance
importance(fit)
varImpPlot(fit, scale = TRUE, font.size = 12)

test_pred3 <- predict(fit, newdata = dfTest3, type = "response")
test_pred3

confusionMatrix(table(actualclass=test_pred3,predictedclass=dfTest3$is_recid))


evaluation <- function(model, data, atype) {
  cat("\nConfusion matrix:\n")
  prediction = predict(model, data, type=atype)
  xtab = table(prediction, data$is_recid)
  print(xtab)
  cat("\nEvaluation:\n\n")
  accuracy = sum(prediction == data$is_recid)/length(data$is_recid)
  precision = xtab[1,1]/sum(xtab[,1])
  recall = xtab[1,1]/sum(xtab[1,])
  f = 2 * (precision * recall) / (precision + recall)
  cat(paste("Accuracy:\t", format(accuracy, digits=2), "\n",sep=" "))
  cat(paste("Precision:\t", format(precision, digits=2), "\n",sep=" "))
  cat(paste("Recall:\t\t", format(recall, digits=2), "\n",sep=" "))
  cat(paste("F-measure:\t", format(f, digits=2), "\n",sep=" "))
}
evaluation(fit, dfTest3, "class")

to.dendrogram <- function(dfrep,rownum=1,height.increment=0.1){
  
  if(dfrep[rownum,'status'] == -1){
    rval <- list()
    
    attr(rval,"members") <- 1
    attr(rval,"height") <- 0.0
    attr(rval,"label") <- dfrep[rownum,'prediction']
    attr(rval,"leaf") <- TRUE
    
  }else{##note the change "to.dendrogram" and not "to.dendogram"
    left <- to.dendrogram(dfrep,dfrep[rownum,'left daughter'],height.increment)
    right <- to.dendrogram(dfrep,dfrep[rownum,'right daughter'],height.increment)
    rval <- list(left,right)
    
    attr(rval,"members") <- attr(left,"members") + attr(right,"members")
    attr(rval,"height") <- max(attr(left,"height"),attr(right,"height")) + height.increment
    attr(rval,"leaf") <- FALSE
    attr(rval,"edgetext") <- dfrep[rownum,'split var']
    #To add Split Point in Dendrogram
    #attr(rval,"edgetext") <- paste(dfrep[rownum,'split var'],"\n<",round(dfrep[rownum,'split point'], digits = 2),"=>", sep = " ")
  }
  
  class(rval) <- "dendrogram"
  
  return(rval)
}

tree <- getTree(fit,1,labelVar=TRUE)

d <- to.dendrogram(tree)
str(d)
plot(d,center=TRUE,leaflab='none',edgePar=list(t.cex=0.7,p.col=NA,p.lty=0), )


# Plot the performance of the model applied to the evaluation set as an ROC curve.

# library(ROCR)
# pred3 <- prediction(as.numeric(test_pred2), as.numeric(dfTest3$is_recid), label.ordering = NULL)
# perf3 <- performance(pred3,"tpr","fpr")
# plot(perf3)

library(precrec)
library(ggplot2)
precrec_obj3 <- evalmod(scores = as.numeric(test_pred2), labels = as.numeric(dfTest3$is_recid))
autoplot(precrec_obj3)

library(pROC)
pROC_obj3 <- roc(as.numeric(dfTest3$is_recid),as.numeric(test_pred3),
                smoothed = TRUE,
                # arguments for ci
                ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                # arguments for plot
                plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                print.auc=TRUE, show.thres=TRUE) 


sens.ci3 <- ci.se(pROC_obj3)
plot(sens.ci3, type="shape", col="lightblue")
plot(sens.ci3, type="bars")

# And then a lift chart

library(lift)
plotLift(test_pred3, dfTest3$is_recid, cumulative = TRUE, n.buckets = 10)

#########################################################################################################

write.csv(dfnew,"G:\\Predective Analysis\\Assignments\\datanew.csv", row.names = TRUE)

#Neural Network
#---------------#

library(caret)
set.seed(1234) #To get reproducible result
ind4 <- sample(3,nrow(dfnew), replace=TRUE, prob=c(0.6,0.2,0.2))
dfTraining4 <- dfnew[ind==1,]
dfTest4 <- dfnew[ind==2,]
dfValidation4 <- dfnew[ind==3,]


#check dimensions of training & testing set
dim(dfTraining4) 
dim(dfTest4)
dim(dfValidation4)

dfTraining4$is_recid <- as.factor(dfTraining4$is_recid)
dfTest4$is_recid <- as.factor(dfTest4$is_recid)

library(nnet)
library(NeuralNetTools)
library(caret)

train_set_ex <- dfTraining4

# actualy create data frame with test set (predictors and outcome together)
test_set_ex <- dfTest4

# note: these setting chunks are separated for reuse later

# set seed, so that statistics don't keep changing for every analysis
set.seed(2019)

# -----------------------------------------------------------------------------
# STEP 1: SELECT TUNING PARAMETERS

# part a: set range of tuning parameters (layer size and weight decay)
tune_grid_neural <- expand.grid(size = c(1:10, 10),
                                decay = c(0, 0.05, 0.1, 1, 2))


# part b: set some other consrains to be imposed on network (to keep computation manageable)
max_size_neaural <- max(tune_grid_neural$size)
max_weights_neural <- max_size_neaural*(nrow(train_set_ex) + 1) + max_size_neaural + 1

# -----------------------------------------------------------------------------
# STEP 2: SELECT TUNING METHOD
# set up train control object, which specifies training/testing technique
train_control_neural <- trainControl(method = "LGOCV",
                                     number = 3,
                                     p = 0.50)

# set seed, so that statistics don't keep changing for every analysis
# (applies for models which might have random parameters)
set.seed(2019)

# start timer
start_time <- Sys.time()

# -----------------------------------------------------------------------------
# STEP 3: TRAIN MODEL

# use caret "train" function to train svm
model_ex <- 
  train(form = is_recid ~ age + priors_count + days_in_jail + days_b_screening_arrest + decile_score + juv_total + possible_recid  + raceAfricanAmerican + raceAsian + raceCaucasian + raceHispanic + raceOther + sexMale + sexFemale,
        data = train_set_ex,
        method = "nnet",
        tuneGrid = tune_grid_neural,
        trControl = train_control_neural,
        metric = "Accuracy", # how to select among models
        trace = FALSE,
        maxit = 100,
        MaxNWts = max_weights_neural) # don't print output along the way

# end timer
total_time <- Sys.time() - start_time

model_ex

test_pred4 <- predict(model_ex, newdata = dfTest4, type = "raw")
test_pred4

confusionMatrix(table(actualclass=test_pred4,predictedclass=dfTest4$is_recid))

NeuralNetTools::neuralweights(model_ex)

evaluation <- function(model, data, atype) {
  cat("\nConfusion matrix:\n")
  prediction = predict(model, data, type=atype)
  xtab = table(prediction, data$is_recid)
  print(xtab)
  cat("\nEvaluation:\n\n")
  accuracy = sum(prediction == data$is_recid)/length(data$is_recid)
  precision = xtab[1,1]/sum(xtab[,1])
  recall = xtab[1,1]/sum(xtab[1,])
  f = 2 * (precision * recall) / (precision + recall)
  cat(paste("Accuracy:\t", format(accuracy, digits=2), "\n",sep=" "))
  cat(paste("Precision:\t", format(precision, digits=2), "\n",sep=" "))
  cat(paste("Recall:\t\t", format(recall, digits=2), "\n",sep=" "))
  cat(paste("F-measure:\t", format(f, digits=2), "\n",sep=" "))
}
evaluation(model_ex, dfTest4, "raw")


plotnet(model_ex$finalModel, y_names = "is_recid")
title("Graphical Representation of Neural Network")


library(precrec)
library(ggplot2)
precrec_obj4 <- evalmod(scores = as.numeric(test_pred3), labels = as.numeric(dfTest4$is_recid))
autoplot(precrec_obj4)

library(pROC)
pROC_obj4 <- roc(as.numeric(dfTest4$is_recid),as.numeric(test_pred3),
                 smoothed = TRUE,
                 # arguments for ci
                 ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                 # arguments for plot
                 plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                 print.auc=TRUE, show.thres=TRUE) 


sens.ci3 <- ci.se(pROC_obj4)
plot(sens.ci3, type="shape", col="lightblue")
plot(sens.ci3, type="bars")

library(lift)
plotLift(test_pred4, dfTest4$is_recid, cumulative = TRUE, n.buckets = 10)

##########################################################################################################################

#Ensemble and new prediction
#---------------------------#

df_ensemble <- subset(dfnew, select = c(priors_count, is_recid, juv_total, days_b_screening_arrest, age, decile_score, days_in_jail, possible_recid, raceAfricanAmerican, raceAsian, raceCaucasian, raceHispanic, raceOther, sexMale, sexFemale))


#Main ensemble wowrk starts here
#-------------------------------#

set.seed(1234)
df_ensemble <- df_ensemble[sample(nrow(df_ensemble)),]
split <- floor(nrow(df_ensemble)/3)
ensembleData <- df_ensemble[0:split,]
blenderData <- df_ensemble[(split+1):(split*2),]
testingData <- df_ensemble[(split*2+1):nrow(df_ensemble),]

labelName = 'is_recid'
predictors <- names(ensembleData)[names(ensembleData) != labelName]

myControl <- trainControl(method = 'cv', number = 3, repeats = 1, returnResamp = 'none')

model_rf <- train(ensembleData[,predictors], ensembleData[,labelName], method = 'rf', trControl = myControl)
model_knn <- train(ensembleData[,predictors], ensembleData[,labelName], method = 'knn', trControl = myControl)
model_lr <- train(ensembleData[,predictors], ensembleData[,labelName], method = 'glm', trControl = myControl)

blenderData$rf_PROB <- predict(object=model_rf, blenderData[,predictors])
blenderData$knn_PROB <- predict(object=model_knn, blenderData[,predictors])
blenderData$lr_PROB <- predict(object=model_lr, blenderData[,predictors])

testingData$rf_PROB <- round(predict(object=model_rf, testingData[,predictors]),digits = 0)
testingData$knn_PROB <-round(predict(object=model_knn, testingData[,predictors]), digits = 0)
testingData$lr_PROB <- round(predict(object=model_lr, testingData[,predictors]),digits = 0)


#Splitting into binary classes at 0.5
testingData$pred_avg<-as.factor(ifelse(testingData$pred_avg>0.5,'Y','N'))

#The majority vote
testingData$pred_majority<-as.factor(ifelse(testingData$rf_PROB== 0 & testingData$knn_PROB== 0, 0,ifelse(testingData$rf_PROB==0 & testingData$lr_PROB==0, 0, ifelse(testingData$knn_PROB==0 & testingData$lr_PROB==0,0,1))))

model_gbm <- train(ensembleData[,predictors], ensembleData[,labelName], method = 'gbm', trControl = myControl)

testingData$pred_majority <- as.factor(testingData$pred_majority)
test_pred5 <- predict(object=model_glm, testingData[,predictors], type = 'raw')
test_pred5 <- round(test_pred5,digits = 0)
test_pred5

confusionMatrix(table(actualclass=test_pred5,predictedclass=testingData$pred_majority))


evaluation <- function(model, data, atype) {
  cat("\nConfusion matrix:\n")
  prediction = predict(model, data, type=atype)
  xtab = table(prediction, data$is_recid)
  print(xtab)
  cat("\nEvaluation:\n\n")
  accuracy = sum(prediction == data$is_recid)/length(data$is_recid)
  precision = xtab[1,1]/sum(xtab[,1])
  recall = xtab[1,1]/sum(xtab[1,])
  f = 2 * (precision * recall) / (precision + recall)
  cat(paste("Accuracy:\t", format(accuracy, digits=2), "\n",sep=" "))
  cat(paste("Precision:\t", format(precision, digits=2), "\n",sep=" "))
  cat(paste("Recall:\t\t", format(recall, digits=2), "\n",sep=" "))
  cat(paste("F-measure:\t", format(f, digits=2), "\n",sep=" "))
}
evaluation(GBMModel, testingData$is_recid, "raw")

library(pROC)
pROC_obj5 <- roc(as.numeric(testingData$pred_majority),as.numeric(test_pred5),
                 smoothed = TRUE,
                 # arguments for ci
                 ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                 # arguments for plot
                 plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                 print.auc=TRUE, show.thres=TRUE) 


sens.ci3 <- ci.se(pROC_obj5)
plot(sens.ci3, type="shape", col="lightblue")
plot(sens.ci3, type="bars")

library(lift)
plotLift(test_pred5, testingData$pred_majority, cumulative = TRUE, n.buckets = 10)


