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

# Descriptive Statistics
#-----------------------#

#To familiarize ourselves with the data set, we evaluate the key variables including the outcome variable. 
#In the following section we describe the data and the distributions for each variable.

#Age

library(psych)
describe(df$age)

library(plotly)
plot_ly(x = df$age,
        type = "histogram",
        marker = list(color = "lightgray",
                      line = list(color = "darkgray",
                                  width = 2))) %>% 
  layout(title = "Distribution of Individuals by Age",
         yaxis = list(title = "Number of Individuals",
                      zeroline = FALSE),
         xaxis = list(title = "Age",
                      zeroline = FALSE))

#Gender 

library(summarytools)
summary(df$sex)

library(plotly)
plot_ly(df, labels = ~sex, type = 'pie') %>% layout(title = 'Deistribution of Individuals by Gender', xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                    yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

#Race

library(summarytools)
summary(df$race)

library(plotly)
plot_ly(df, x = ~race,
        type = "histogram",
        marker = list(color = "lightgray",
                      line = list(color = "darkgray",
                                  width = 2))) %>% 
  layout(title = "Distribution of Individuals by Race",
         yaxis = list(title = "Number of individuals",
                      zeroline = FALSE),
         xaxis = list(title = "Race",
                      zeroline = FALSE),
         sort(df$race,decreasing = TRUE))

#Prior Count

library(psych)
describe(df$priors_count)

library(plotly)
plot_ly(x = df$priors_count,
        type = "histogram",
        marker = list(color = "lightgray",
                      line = list(color = "darkgray",
                                  width = 2))) %>% 
  layout(title = "Distribution of Indivisuals by Prior Count",
         yaxis = list(title = "Number of Individuals",
                      zeroline = FALSE),
         xaxis = list(title = "Prior Count",
                      zeroline = FALSE))

#Total Juvenile

library(psych)
describe(df$juv_total)

library(plotly)
plot_ly(x = df$juv_total,
        type = "histogram",
        marker = list(color = "lightgray",
                      line = list(color = "darkgray",
                                  width = 2))) %>% 
  layout(title = "Distribution of Individuals by Total Juvenile Cases",
         yaxis = list(title = "Number of Individuals",
                      zeroline = FALSE),
         xaxis = list(title = "Total Juvenile Cases",
                      zeroline = FALSE))

#Days in Jail

library(psych)
describe(df$days_in_jail)

library(plotly)
plot_ly(x = df$days_in_jail,
        type = "histogram",
        marker = list(color = "lightgray",
                      line = list(color = "darkgray",
                                  width = 2))) %>% 
  layout(title = "Distribution of Individuals by Days in Jail",
         yaxis = list(title = "Number of Individuals",
                      zeroline = FALSE),
         xaxis = list(title = "Days in Jail",
                      zeroline = FALSE))

#Decile Score

library(summarytools)
summary(df$decile_score)

library(plotly)
plot_ly(x = df$decile_score,
        type = "histogram",
        marker = list(color = "lightgray",
                      line = list(color = "darkgray",
                                  width = 2))) %>% 
  layout(title = "Distribution of Individuals by Decile Score",
         yaxis = list(title = "Number of Individuals",
                      zeroline = FALSE),
         xaxis = list(title = "Decile Score",
                      zeroline = FALSE))

table(df$decile_score)

# Risk

table(df$risk)
library(plotly)
plot_ly(x = df$risk,
        type = "histogram",
        marker = list(color = "lightgray",
                      line = list(color = "darkgray",
                                  width = 2))) %>% 
  layout(title = "Distribution of Individuals by Risk (Decile Score Category)",
         yaxis = list(title = "Number of Individuals",
                      zeroline = FALSE),
         xaxis = list(title = "Risk",
                      zeroline = FALSE))

#Charge Description

library(summarytools)
summary(df$c_charge_desc)

library(plotly)
plot_ly(df, x = ~c_charge_desc,
        type = "histogram",
        marker = list(color = "lightgray",
                      line = list(color = "darkgray",
                                  width = 2))) %>% 
  layout(title = "Distribution of Indivisuals by Charge by Description",
         yaxis = list(title = "Number of indivisuals",
                      zeroline = FALSE),
         xaxis = list(title = "Charge by Description",
                      zeroline = FALSE))

#Charge Degree

library(summarytools)
summary(df$c_charge_degree)

library(plotly)
plot_ly(df, labels = ~c_charge_degree, type = 'pie') %>% layout(title = 'Dliistribution of Indivisuals by Charge Degree', xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

#Judges are often presented with two sets of scores from the Compas system -- one that classifies people into High, Medium and Low risk, 
#and a corresponding decile score. There is a clear downward trend in the decile scores as those scores increase for white defendants.

library(grid)
library(gridExtra)
pblack <- ggplot(data=filter(df, race =="African-American"), aes(ordered(decile_score))) + 
  geom_bar() + xlab("Decile Score") +
  ylim(0, 650) + ggtitle("Black Defendant's Decile Scores")
pwhite <- ggplot(data=filter(df, race =="Caucasian"), aes(ordered(decile_score))) + 
  geom_bar() + xlab("Decile Score") +
  ylim(0, 650) + ggtitle("White Defendant's Decile Scores")
grid.arrange(pblack, pwhite,  ncol = 2)

xtabs(~ decile_score + race, data=df)




#Recidvism Analysis
#-------------------#
# library(plotly)
# plot_ly(df, x = ~age, color = ~is_recid) %>% add_histogram() %>% layout(title = 'Visualisation of Category by Country (United States of America)', xaxis = list(title = "", tickfont = list(size = 12, color = 'rgb(107, 107, 107)')), yaxis = list( title = 'Counts',titlefont = list(size = 16, color = 'rgb(107, 107, 107)'),tickfont = list(size = 12,color = 'rgb(107, 107, 107)')))
# 
# 
# 
# df$recid_class <- factor(df$is_recid, levels = 0:1, labels = c("Didn't commit", "Again commited a crime"))
# plot(recid_class ~ age, data = df)
# 
# 
# library(ggplot2)
# ggplotly(ggplot(df, aes_string(age, fill = recid_class)) + 
#            geom_density(position='fill', alpha = 0.5) + 
#            xlab("Age") + labs(fill='Recidivism') +
#            theme(legend.text=element_text(size=12), 
#                  axis.title=element_text(size=14)))



#Create and new dataframe and store only limited variables
#----------------------------------------------------------#

dfnew <- subset(df, select = c(id, sex, age, age_cat, race, juv_fel_count, decile_score, juv_misd_count, juv_other_count, priors_count, days_b_screening_arrest,
                               c_jail_in, c_jail_out, c_offense_date, c_arrest_date, c_days_from_compas, c_charge_degree, c_charge_desc, is_recid, r_charge_degree,
                               #r_days_from_arrest, 
                               days_in_jail, juv_total, possible_recid))

# Looping over all the columns to replace the NA's with the mean of the relevant column
#for(i in 1:ncol(dfnew)){
#  dfnew[is.na(dfnew[,i]), i] <- mean(dfnew[,i], na.rm = TRUE)
#}


df_corr <- subset(df, select = c(priors_count, is_recid, juv_total, days_b_screening_arrest, c_days_from_compas, r_days_from_arrest, age, decile_score, days_in_jail, possible_recid))

#Correlation of decile score with other variables
#------------------------------------------------#

library(corrr)
df_corr %>% correlate() %>% focus(is_recid) %>%
  mutate(rowname = factor(rowname, levels = rowname[order(is_recid)])) %>%
  ggplot(aes(x = rowname, y = is_recid)) +
  geom_bar(stat = "identity", color = "lightgray") +
  ylab("Correlation with Decile Score") +
  xlab("Variable")

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

#Create a decision Tree on the basis of abover created datasets
#---------------------------------------------------------------#

#Training the Decision Tree classifier with criterion as information gain

library(rpart)
library(rpart.plot)
library(party)
rPartModel = rpart(is_recid ~ age + priors_count + days_in_jail + days_b_screening_arrest + decile_score + juv_total + possible_recid, 
                   data=dfTraining, method="class", parms=list(split="information"))

summary(rPartModel)

rpart.plot(rPartModel, extra = 104, nn = TRUE)

#default value of complexity parameter is 0.01 and after that rpart package will not split unless 
#we override the cp parameter

printcp(rPartModel)
plotcp(rPartModel)
rpart.control()


# ames_dt3 <- train(
#   is_recid ~ age + priors_count + days_in_jail + days_b_screening_arrest + decile_score + juv_total,
#   data = dfTraining,
#   method = "rpart",
#   trControl = trainControl(method = "cv", number = 10),
#   tuneLength = 20
# )
# 
# ggplot(ames_dt3)
# 
# library(vip)
# vip(ames_dt3, num_features = 40, bar = FALSE)


#Predicting the pruned tree
#---------------------------#

predict <- predict(rPartModel,dfTest)
#View(predict)

dfTest$recid_predicted <- predict(rPartModel,dfTest, type = 'class')

#generting the table format
xlab <- table(actualclass=dfTest$is_recid,predictedclass=dfTest$recid_predicted)  

#generating the confusion matrix
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

