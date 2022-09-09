setwd("C:/Users/Meabh/Documents")

#Load libraries
library(randomForest)
library(caTools)
library(caret)
library(stringr)
library(lubridate)
library(magrittr)
library(ggplot2)
library(dplyr)
library(mltools)
library(pROC)
library(data.table)


#Read in the csvW
noShows <- read.csv("noShowCleaned.csv",encoding = "latin1")

#Feature engineering and data preprocessing

#Convert DV to a binary
noShows$No.show[which(noShows$No.show == "Yes")] = 1
noShows$No.show[which(noShows$No.show == "No")] = 0
noShows$No.show %<>% as.integer()

#Convert dates to correct datatype
noShows$AppointmentDay <- ymd_hms(noShows$AppointmentDay)
noShows$ScheduledDay <- ymd_hms(noShows$ScheduledDay)

#Add appt day of weekand scheduled day of week columns
noShows$AppointmentDayOfWeek <- weekdays(noShows$AppointmentDay)
noShows$ScheduledDayOfWeek <- weekdays(noShows$ScheduledDay)

#PatientID is not unique, so a primary key must be constructed
noShows$PK <- paste0(noShows$PatientId,noShows$Gender,noShows$Age)




#Add child and pensioner columns
child <- vector("numeric", length = nrow(noShows))
pensioner <- vector("integer", length = nrow(noShows))
noShows <- cbind(noShows, child, pensioner)
for (i in 1:nrow(noShows)) {
  if (noShows$Age[i] < 18) {
    noShows$child[i] = 1
  }
  else if (noShows$Age[i] >= 18) {
    noShows$child[i] = 0
  }
  else {
    noShows$child[i] = NA
  }}

for (i in 1:nrow(noShows)) {
  if (noShows$Age[i] <= 60) {
    noShows$pensioner[i] = 0
  }
  else if (noShows$Age[i] > 65) {
    noShows$pensioner[i] = 1
  }
  else if (noShows$Age[i] > 60 & noShows$Gender[i] == "F") {
    noShows$pensioner[i] = 1
  }
  else if (noShows$Age[i] > 60 & noShows$Gender[i] == "M") {
    noShows$pensioner[i] = 0
  }
  else {
    noShows$pensioner[i] = NA
  }
}

#Time from Scheduling to Appointment in days
noShows$DaysWait <- round(difftime(noShows$AppointmentDay,noShows$ScheduledDay,units="days"),0)
noShows$DaysWait %<>% as.numeric()



#Create breaks
breaks <- c(0,6,12,18,24)
#Labels for the breaks
labels <- c("Night", "Morning", "Afternoon", "Evening")

#Add a scheduled time of day column
noShows$ScheduledTimeOfDay <- cut(x=hour(noShows$ScheduledDay), breaks = breaks, labels = labels, include.lowest=TRUE)

#Drop unused columns
noShows <- subset(noShows, select = -c(AppointmentID,
                                               PatientId,
                                               ScheduledDay,
                                               name_neighborhood))




#Make a sub-table for each patient, their most recent appt and whether
#they have a history of no-showing prior to the most recent appt
noShowPatient <- noShows %>% group_by(PK) %>%
  add_count() %>%
  mutate(mean_prior_No.show = (sum(No.show)-No.show)/(n-1)) %>%
  mutate(total_prior_No.show = sum(No.show)-No.show) %>%
  mutate(recent_appt = max(AppointmentDay)) %>%
  ungroup()

#Replace NaNs in mean_prior_No.show with zero
noShowPatient[which(is.na(noShowPatient$mean_prior_No.show)),]$mean_prior_No.show = 0

#Take only appts matching the most recent appt date
noShowPatient <- noShowPatient[which(noShowPatient$AppointmentDay == noShowPatient$recent_appt),]


#Add a randomised tiebreaker for appts that all fall on the final day
randoms <- vector("numeric", length = nrow(noShowPatient))
for (row in 1:nrow(noShowPatient)){
  randoms[row] = runif(1,0,1)
}

noShowPatient2 <- cbind(noShowPatient,Random = randoms)

noShowPatient3 <- noShowPatient2 %>% group_by(PK) %>%
  add_count() %>%
  mutate(maxRandom = max(Random)) %>%
  ungroup()

#Take only appts matching the randomised max per group to ensure uniqueness
noShowPatient4 <- noShowPatient3[which(noShowPatient3$maxRandom == noShowPatient3$Random),]

#Rename column "n" to "Appt_Count" as this is clearer
colnames(noShowPatient4)[which(colnames(noShowPatient4) == "n")] = "Appt_Count"

#Rename column "nn" to "Same_Day_Appts" as this is clearer
colnames(noShowPatient4)[which(colnames(noShowPatient4) == "nn")] = "Same_Day_Appts"


#Drop unused columns
noShowPatient5 <- subset(noShowPatient4, select = -c(recent_appt,
                                       Random,
                                       maxRandom,
                                       PK,
                                       AppointmentDay))



#Convert all the chr columns to Factor
charstofactors <- c("AppointmentDayOfWeek","ScheduledDayOfWeek"
                    ,"District"
                    ,"Region"
                    ,"Neighbourhood"
                    ,"Gender")
noShowPatient5[charstofactors] <- lapply(noShowPatient5[charstofactors], factor)


#Reality check: are the District and Region features that have been
#joined in useful additions?
regionSelection <- one_hot(as.data.table(noShowPatient5[,c("Neighbourhood","Region","District","No.show")]))

#Model weight. Upweight no-shows by 500%, keep all shows.
wt <- vector("numeric", length = nrow(regionSelection))
for (row in 1:nrow(regionSelection)){
  if (regionSelection$No.show[row] == 1){
    wt[row] = 5
  }
  else {
    wt[row] = 1
  }
}

#Check the coefficients
regionglm <- glm(No.show ~ .,
    family = binomial,
    data = regionSelection,
    weights = wt)

#In conclusion: region and district are not useful features to add.

#Coding the dummy variables using one_hot and dropping Region and District
noShowPatient5 <- subset(noShowPatient5, select = -c(Region,District))
noShowPatient6 <- one_hot(as.data.table(noShowPatient5))



#Feature Selection
correlationMatrix <- cor(noShowPatient6[,-c("No.show")])
correlationMatrixDV <- cor(noShowPatient6)
#Summarize the correlation matrix

#Find attributes that are corrected
mediumCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)

#Check for attributes that are correlated with the DV at at least .01
onepercentfeatures <- sort(abs(correlationMatrixDV[91,]), decreasing = TRUE)[1:39]

#Subset with only correlated features
noShowPatient7 <- subset(noShowPatient6, select = names(onepercentfeatures))

#Model weight. Upweight no-shows by 500%, keep all shows.
wt <- vector("numeric", length = nrow(noShowPatient7))
for (row in 1:nrow(noShowPatient7)){
  if (regionSelection$No.show[row] == 1){
    wt[row] = 50
  }
  else {
    wt[row] = 10
  }
}
#
#glm <- glm(No.show ~ .,
#                 family = binomial(link = "logit"),
#                 data = noShowPatient7,
#                 weights = wt)


#Set the seed
set.seed(121)

#Set the train-test split
split = sample.split(noShowPatient7$No.show, SplitRatio = 0.75)
training_set = subset(noShowPatient7, split == TRUE)
test_set = subset(noShowPatient7, split == FALSE)


#Create the weights
wt <- vector("numeric", length = nrow(training_set))
for (row in 1:nrow(training_set)){
  if (regionSelection$No.show[row] == 1){
    wt[row] = 1
  }
  else {
    wt[row] = 5
  }
}

#Build the model
classifier <- glm(No.show ~ .,
                         family = binomial(link = "logit"),
                         data = training_set,
                  weights = wt
                  )

#Predictions
y_pred <- predict(classifier, newdata = test_set[,-1], type = "response")
X_pred <- predict(classifier, newdata = training_set[,-1], type = "response")

#Confusion matrices
cmX <- table(actual = training_set$No.show, predicted = ifelse(X_pred > 0.5,1,0))
cmX <- confusionMatrix(cmX)
cmy <- table(actual = test_set$No.show, predicted = round(y_pred,0))
cmy <- confusionMatrix(cmy)



#Visualise an ROC curve
roc(training_set$No.show,ifelse(X_pred > 0.2,1,0),plot = TRUE)

#Test on the test dataset
roc(test_set$No.show,ifelse(y_pred > 0.2,1,0),plot = TRUE)