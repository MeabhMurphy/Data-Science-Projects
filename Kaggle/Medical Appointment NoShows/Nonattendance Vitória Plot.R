setwd("C:/Users/Meabh/Documents")

#Load Libraries
library(sf)
library(geobr)
library(magrittr)
library(ggplot2)
library(dplyr)
library(viridis)
library(plotly)

#Read in the csv
noShows <- read.csv("noShowCleaned.csv",encoding = "latin1")

#Convert outcome variable to a binary
noShows$No.show[which(noShows$No.show == "Yes")] = 1
noShows$No.show[which(noShows$No.show == "No")] = 0
noShows$No.show <- as.integer(noShows$No.show)


#Convert dates to date
noShows$AppointmentDay <- ymd_hms(noShows$AppointmentDay)
noShows$ScheduledDay <- ymd_hms(noShows$ScheduledDay)

#Add appt day of weekand scheduled day of week columns
noShows$AppointmentDayOfWeek <- weekdays(noShows$AppointmentDay)
noShows$ScheduledDayOfWeek <- weekdays(noShows$ScheduledDay)

#Time from Scheduling to Appointment in days
noShows$DaysWait <- round(difftime(noShows$AppointmentDay,noShows$ScheduledDay,units="days"),0)
noShows$DaysWait %<>% as.numeric()

#Get a shapefile for the city and join it to the main dataframe
neighbourhood <- read_neighborhood(year = 2010)
vit <- neighbourhood[which(neighbourhood$name_muni == "Vitória"),]
noShowLocations <- merge(noShows,vit)

#Include only essential columns
noShowLocations <- noShowLocations[c("name_neighborhood","geom","No.show","AppointmentID"
                                     #,"DaysWait"
                                     )]

#Group by neighbourhood
noShowLocations %<>% group_by(name_neighborhood) %>%
  add_count() %>%
  mutate(No.show = mean(No.show)) %>%
  ungroup()

#Drop appointmentID column
noShowLocations <- subset(noShowLocations, select = -AppointmentID)

#Drop duplicate rows
distinct_neighbourhood <- distinct(noShowLocations)

#Convert to sf object for ease of mapping
neighbourhoodPlot <- st_as_sf(distinct_neighbourhood)

#Create the plot
neighbourhoods <- ggplot(data = neighbourhoodPlot) +
  geom_sf(color = "black",aes(text = paste0(name_neighborhood,". n= ",n), fill = No.show)) +
  
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="bottom",
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank()) +
  
  scale_fill_viridis(option="magma", direction = -1)

#Call the plot
ggplotly(neighbourhoods)


