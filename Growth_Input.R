# Load libraries

library(tidyverse)
library(plyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(glmmTMB)
library(zoo)

####### Integrate growth ########

# Load temperature Data!  Convert to Celsius if in Fahrenheight.
# Format should be daily temperature (C) = Temp and Date = DT in the format 'YYYY-MM-DD'


Temp <- read.csv('./Temp.csv')


# Convert to Celsius if required
#Temp$Temp <-  (Temp$Temp - 32) * (5/9)

# Ensure DT is in Date format
Temp$DT <- as.Date(Temp$DT)


Temp$day <- (Temp$DT-min(Temp$DT)) + 1
Temp$day <- as.numeric(Temp$day)

# Fit temperature to a generalized annual temperature curve
fit <- lm(Temp ~ sin(2*pi/365*day)+cos(2*pi/365*day),data=Temp)

# Predict growth over an iterative growth function from August Y2 - August 31 Y4

# Iterate days
d <- seq(1,760, by=1)

pr.temp <- predict(fit, newdata = data.frame(day = d),
                   type = "response")

# Function to convert Temperature to optimal degree growth days

Tmin <- -2
Tmax <- 22
Topt <- 12.5

t.weighted <- function(x) {
  ((x-Tmin)*(x-Tmax))/((x-Tmin)*(x-Tmax) - (x-Topt)^2)
}

# Apply WGDD to temperatures
pr.temp <- sapply(pr.temp,t.weighted)

# Load growth Model
Growth <- readRDS('Growth.rda')
# Ear Hanging iterative growth function
populate_value.eh <- function(x){
  for(i in 2:length(x)){
    x[i] <- ((predict (Growth, newdata = data.frame(mean_sh = x[i-1], 
                                                    wgdd.mean = pr.temp[i], 
                                                    Trial = 'Ear Hanging', 
                                                    Site = 'NA',
                                                    Cohort = 'NA'),
                       type = "response", allow.new.levels = TRUE)))+x[i-1]
  }
  x
}

# Lantern Net iterative growth function
populate_value.ln <- function(x){
  for(i in 2:length(x)){
    x[i] <- ((predict (Growth, newdata = data.frame(mean_sh = x[i-1], 
                                                    wgdd.mean = pr.temp[i], 
                                                    Trial = 'Lantern Net', 
                                                    Site = 'NA',
                                                    Cohort = 'NA'),
                       type = "response", allow.new.levels = TRUE)))+x[i-1]
  }
  x
}


ret = data.frame(matrix(nrow = 760, ncol = 1))
colnames(ret) = 'x'

## Iterate starting at a projected size of pri (recommended is 55mm)
pri = ret
pri[1,] = 55

#d <- seq(0,759, by=1)                                  
predicted.eh <- data.frame(day = d)

predicted.eh$ShellHeight <- sapply(pri,populate_value.eh)

colnames(predicted.eh) <- c("day","Sh_Height")
predicted.eh$Trial <- 'Ear Hanging'

predicted.ln <- data.frame(day = d)

predicted.ln$ShellHeight <- sapply(pri,populate_value.ln)

colnames(predicted.ln) <- c("day","Sh_Height")
predicted.ln$Trial <- 'Lantern Net'

D <- seq(min(Temp$DT), length = 760, by = "days")

predicted.ln$Date <- D
predicted.eh$Date <- D

Predicted <- rbind(predicted.ln,predicted.eh)
Predicted$Year <- as.numeric(format(Predicted$Date, '%y'))
Predicted$Month <- as.numeric(format(Predicted$Date, '%m'))
Predicted$yrDate <- as.yearmon(Predicted$Date, "%y %m")

Predicted <- subset(Predicted, Month == '5' | Month == '8' | Month == '11' | Month == '2')

Predicted <- aggregate(data = Predicted, Sh_Height~Trial+yrDate+Month, FUN = 'mean')
Predicted$Site <- 'NA'
Predicted<- Predicted %>% 
  dplyr::rename('Sh_Height' = 4)


weight <- readRDS('./Adductor.rda')

Predicted$Adductor <- predict(weight,Predicted,allow.new.levels=TRUE,
                              type = 'response')
Predicted.full <- Predicted

Predicted.full$Month <- month(Predicted.full$yrDate)
Predicted.full$Year <- year(Predicted.full$yrDate)
Predicted.full$Harvest.Y <- ifelse(Predicted.full$Year == max(Predicted.full$Year),ifelse(Predicted.full$Month == 5 | Predicted.full$Month == 2, 'Y3','Y4'),
                                      ifelse(Predicted.full$Year == min(Predicted.full$Year),'Y2',
                                             ifelse(Predicted.full$Year == median(Predicted.full$Year)&Predicted.full$Month == 11|Predicted.full$Month == 8, 'Y3','Y2')))

Predicted.full$Harvest.Time <- ifelse(Predicted.full$Month == 2,'Spring',
                                      ifelse(Predicted.full$Month == 5,'Summer',
                                      ifelse(Predicted.full$Month == 8, 'Fall','Winter')))

Predicted.full <- Predicted.full %>% 
  mutate (Sh_Height.inches = Sh_Height*0.0393701) %>% 
  mutate (Adductor.lbs = 0.00220462*Adductor) %>% 
  mutate (count.lbs = 1/Adductor.lbs)
Predicted.full<- Predicted.full[,-7]
Predicted.full<- Predicted.full %>% 
  dplyr::rename('Year' = 7) %>%
  dplyr::rename('Season' = 8) 
