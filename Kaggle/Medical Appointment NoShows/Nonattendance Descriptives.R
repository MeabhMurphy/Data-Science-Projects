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

#Convert output to a binary
noShows$No.show[which(noShows$No.show == "Yes")] = 1
noShows$No.show[which(noShows$No.show == "No")] = 0
noShows$No.show %<>% as.integer()

#Convert dates to correct datatype
noShows$AppointmentDay <- ymd_hms(noShows$AppointmentDay)
noShows$ScheduledDay <- ymd_hms(noShows$ScheduledDay)

#Add appt day of weekand scheduled day of week columns
noShows$AppointmentDayOfWeek <- weekdays(noShows$AppointmentDay)
noShows$ScheduledDayOfWeek <- weekdays(noShows$ScheduledDay)

#Time from Scheduling to Appointment in days
noShows$DaysWait <- round(difftime(noShows$AppointmentDay,noShows$ScheduledDay,units="days"),0)
noShows$DaysWait %<>% as.numeric()

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



#Create breaks for the time of day
breaks <- c(0,6,12,18,24)
#Labels for the time of day breaks
labels <- c("Night", "Morning", "Afternoon", "Evening")

#Add the scheduled time of day column
noShows$ScheduledTimeOfDay <- cut(x=hour(noShows$ScheduledDay), breaks = breaks, labels = labels, include.lowest=TRUE)


#Drop unused columns
noShows <- subset(noShows, select = -c(AppointmentID,
                                               PatientId,
                                               ScheduledDay,
                                               name_neighborhood))

#Convert character column types into factors
charstofactors <- c("AppointmentDayOfWeek","ScheduledDayOfWeek"
                    ,"District"
                    ,"Region"
                    ,"Neighbourhood"
                    ,"Gender")
noShows[charstofactors] <- lapply(noShows[charstofactors], factor)

#When were appointments scheduled?
summary(noShows$ScheduledTimeOfDay)

#What day of the week were appointments scheduled for?
summary(noShows$ScheduledDayOfWeek)

#What neighbourhoods do people live in?
summary(noShows$Neighbourhood)

#What regions do people live in?
summary(noShows$Region)

#What regions do people live in?
summary(noShows$District)

#What is the gender breakdown?
summary(noShows$Gender)


#What does the population pyramid look like?
df <- as_tibble(noShows) %>% 
  mutate(AgeDecade=as.factor(floor(Age/10)*10)) %>% 
  group_by(Gender, AgeDecade) %>% 
  dplyr::summarise(N=n(), .groups="drop") %>% 
  # A more transparent way of managing the transformation to get "Females to the left".
  mutate(PlotN=ifelse(Gender=="F", -N, N)) 
# Create the plot
df %>% ggplot() +
  geom_col(aes(fill=Gender, x=AgeDecade, y=PlotN)) +
  scale_y_continuous(breaks=c(-10*10**3,
                              -8*10**3,
                              -6*10**3,
                              -4*10**3,
                              -2*10**3,
                              0,
                              2*10**3,
                              4*10**3,
                              6*10**3,
                              8*10**3),
                     labels=c("10000",
                              "8000",
                              "6000",
                              "4000",
                              "2000",
                              "0",
                              "2000",
                              "4000",
                              "6000",
                              "8000")) +
  labs(y="Population", x="Age group") +
  scale_fill_discrete(name="Gender") +
  coord_flip()



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
noShowPatient2 <- noShowPatient[which(noShowPatient$AppointmentDay == noShowPatient$recent_appt),]


#Add a randomised tiebreaker for appts that all fall on the final day
randoms <- vector("numeric", length = nrow(noShowPatient2))
for (row in 1:nrow(noShowPatient2)){
  randoms[row] = runif(1,0,1)
}

noShowPatient2 <- cbind(noShowPatient2,Random = randoms)

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



#What is the gender breakdown for the most recent appointment?
summary(noShowPatient5$Gender) #Female majority narrower.

#What is the gender breakdown for appt count?
aggregate(n ~ Gender, noShowPatient, mean)
#F = 3.33, M = 4.36. How can this be reconciled with the above?

#Is there a gender difference in wait times?
aggregate(DaysWait ~ Gender, noShowPatient, mean)
#Yes, F = 10.11, M = 10.4.

#What is the wait time gender gap when accounting for child status?
aggregate(DaysWait ~ child+Gender_M, noShowPatient6, mean)
#(I am aware of the risk of falling prey to Simpson's Paradox)

#What is the wait time gap for pensioners and is this accounted for by
#some non-pensioners being children?
aggregate(DaysWait ~ pensioner+child, noShowPatient6, mean)
#pensioners = 12.1, non-pensioners = 10.8.