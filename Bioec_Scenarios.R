# Load libraries for analysis

library(readr)
library(tidyr)
library(lubridate)
library(DT)
library(shinyBS)
library(zoo)
library(shinyjs)
library(readxl) # For reading excel tables
library(dplyr) # For data manipulation
library(plyr)  # For data manipulation and to seemingly interfere with dplyr
library(hablar) # For the function 'retype' that assigns values to data frame columns (ie character or numeric)
library(ggplot2) # For graphics 
library(treemapify) # Create a treemap even though donut charts are prettier
library(openxlsx)

# Set file path for spreadsheet
file_path <- "./Components_V5.xlsx"

#Just read in the Growth Input instead of running the whole program
Predicted.full <- read_excel(file_path, sheet = 'Growth_Input')


# These are 'general' equipment inputs, labor tasks, fuel costs, and maintenance.  
# I envision these will be designed to essentially be the 'entry' sections while the following sheet 
# Will be the hard coding not meant to be altered.


# Load all equipment
Equipment.Data <- read_excel(file_path, sheet = 'Equipment')
# Quantity is switched to character initially to merge and allow for entry of specialized equipment
Equipment.Data$Quantity <- as.character(Equipment.Data$Quantity)


# Enter Task, fuel, and maintenance data
Task.Data <- read_excel(file_path, sheet = 'Labor')
Fuel.Data <- read_excel(file_path, sheet = 'Fuel')
Maint.Data <- read_excel(file_path, sheet = 'Maintenance')

# Entry and vectorization of primary parameters
Primary.Parameter.Data<- read_excel(file_path, sheet = 'Primary')

# loops by row to take column 1 as the variable name, and column 2 as either a character or 
# numeric and vectorize via assign function
for (i in 1:nrow(Primary.Parameter.Data)) {
  VariableName <- as.character(Primary.Parameter.Data$VariableName[i])
  Value <- retype(Primary.Parameter.Data$Value[i,drop=TRUE]) 
  assign(VariableName, Value, envir = .GlobalEnv)
}

# Remove extra stuff for clarity and good data practice
rm(VariableName, Value)

# Subset by Harvest Year, Season and Grow Out Method
# This section creates vectors for subsetting different criteria

# Creates vectors for Year steps
Y0 <- c('Y0','all')
Y1 <- c('Y0','Y1','all')
Y2 <- c('Y0','Y1','Y2','all')
Y3 <- c('Y0','Y1','Y2','Y3','all')
Y4 <- c('Y0','Y1','Y2','Y3','Y4','all')

Fall <- 'Fall'
Winter <- c('Fall','Winter')
Spring <- c('Fall','Winter','Spring')
Summer <- c('Fall','Winter','Spring','Summer')

# Remove vessel and truck purchase

Equipment.Data$Quantity[Equipment.Data$Equipment == 'Vessel'] <- '0'
Equipment.Data$Quantity[Equipment.Data$Equipment == 'Truck'] <- '0'

Output.Analysis3 <- data.frame()

Product.s <- seq(50000,600000, by = 10000)
HYear <- '4 Years (August-October only)'
HSeason <- 'August-October'
GOMethod <- 'Ear Hanging'
spec.equip <- c('Drill (Dremel)','Drill (Automated)','Automated Drill and Pin')
owner <- c('Sole Ownership','Cooperative Ownership')
Scen1 <- expand.grid(Product.s,HYear,HSeason,GOMethod,spec.equip,owner)


for(i in 1:nrow(Scen1))
  {

  # Load all equipment
  Equipment.Data <- read_excel(file_path, sheet = 'Equipment')
  # Quantity is switched to character initially to merge and allow for entry of specialized equipment
  Equipment.Data$Quantity <- as.character(Equipment.Data$Quantity)
  
  
  # Enter Task, fuel, and maintenance data
  Task.Data <- read_excel(file_path, sheet = 'Labor')
  Fuel.Data <- read_excel(file_path, sheet = 'Fuel')
  Maint.Data <- read_excel(file_path, sheet = 'Maintenance')
  
  Equipment.Data$Quantity[Equipment.Data$Equipment == 'Drill (Dremel)']  <- '0'
  Equipment.Data$Quantity[Equipment.Data$Equipment == 'Drill (Automated)']  <- '0'
  Equipment.Data$Quantity[Equipment.Data$Equipment == 'Automated Drill and Pin']  <- '0'
  
  Product <- Scen1[i,1]
  `Harvest Year` <- as.character(Scen1[i,2])
  `Harvest Season` <- as.character(Scen1[i,3])
  `Grow Out Method` <- as.character(Scen1[i,4])
  Equipment.Data$Quantity[Equipment.Data$Equipment == as.character(Scen1[i,5])]  <- '1'

  Equipment.Data$Item[is.na(Equipment.Data$Item)] <- 0      
  if(as.character(Scen1[i,6])=='Cooperative Ownership'){
    
    
    Equipment.Data <- Equipment.Data %>%
      mutate(Unit.Cost = case_when(Item == 'Specialized Equipment' ~ Unit.Cost/10,
                                   Item != 'Specialized Equipment' ~ Unit.Cost)) 
    
    Maint.Data$Cost[Maint.Data$Item == 'Specialized Equipment'] <- Maint.Data$Cost[Maint.Data$Item == 'Specialized Equipment']/10
    
  }
         
  
  Spec.equip = as.character(Scen1[i,5])
  ownership = as.character(Scen1[i,6])
  
  # Matches Harvest Year vector with appropriate year vector
  Harvest.Year <- switch(`Harvest Year`, 'Y0'= Y0, 'Y1' = Y1, '2-3 Years'= Y2, '3-4 Years'= Y3, '4 Years (August-October only)' = Y4)
  
  # creates a season vector for final labor allotment
  Harvest.Season <- switch(`Harvest Season`, 'August-October'= Fall, 'November-January' = Winter, 'February-April'= Spring, 'May-July'= Summer)
  
  # Switchs data entry into standard season and year formats
  `Harvest Year` <- switch(`Harvest Year`, 'Y0'= 'Y0', 'Y1' = 'Y1', '2-3 Years'= 'Y2', '3-4 Years'= 'Y3', '4 Years (August-October only)' = 'Y4')
  `Harvest Season` <- switch(`Harvest Season`, 'August-October'= 'Fall', 'November-January' = 'Winter', 'February-April'= 'Spring', 'May-July'= 'Summer')
  
  
  
  # Let's add the secondary parameters
  Secondary.Data<- read_excel(file_path, sheet = 'Secondary')
  
  # Similar to primary, secondary parameters either based on farm layout or husbandry
  for (i in 1:nrow(Secondary.Data)) {
    VariableName <- as.character(Secondary.Data$VariableName[i])
    Value <- retype(Secondary.Data$Value[i, drop=TRUE])
    Value <- eval(parse(text=Value))
    assign(VariableName, Value, envir = .GlobalEnv)
  }
  
  # Remove extra stuff for clarity and good data practice
  rm(VariableName, Value)  
  
# Market Data calculates seasonal mortality for available harvest times 
Market.Data <- read_excel(file_path, sheet = 'Market')
for (i in 1:nrow(Market.Data)) {
  # Evaluate the Quarterly Mortality
  Market.Data$Market.Product[i]<- as.numeric(eval(parse(text = Market.Data$Market.Product[i])))
}
# Change market data to numeric and subset from predicted growth based on harvest year, season, and grow out
Market.Data$Market.Product <- as.numeric(Market.Data$Market.Product)
Growth.Data <- subset(Predicted.full, Year == `Harvest Year`& Season == `Harvest Season`& Trial == `Grow Out Method`)
Growth.Data <- Growth.Data[,-c(1,2,3,5)]
Growth.Data <- left_join(Growth.Data, Market.Data, by = c('Year','Season'))

# Creates a farm strategy vector for subseting based on grow out type
Farm.strat <- c(`Grow Out Method`,`Spat Procurement`,`Intermediate Culture`,'Global')

# Equipment

# Read Equipment Outputs, subset by Harvest year and farm strategy then merge with Unit Cost, 
#Lifespan and quantity.  Also merge so that specialized and large equipment purchases can be entered
Equipment <- read_excel(file_path, sheet = 'Equipment_Output')
Equipment.Subset <- Equipment[which(Equipment$Year %in% Harvest.Year& Equipment$Type %in% Farm.strat),]
Equipment.Subset <- within(merge(Equipment.Subset,Equipment.Data, by = 'Equipment'), 
                           {Quantity <- ifelse(is.na(Quantity.y),Quantity.x,Quantity.y); Quantity.x <- NULL; Quantity.y <- NULL})

# Separate into year class equipment and global equipment.  Global equipment is the lease stuff that 
# is calculated dependent on year class stuff

Equipment.Subset.Year <- subset(Equipment.Subset, Type != 'Global')
Equipment.Subset.Global <- subset(Equipment.Subset, Type == 'Global')

#Equipment for used in farm strategy and harvest year
for (i in 1:nrow(Equipment.Subset.Year)) {
  # Evaluate the Quantity expression for this row, and do cost.basis and depreciation too
  Equipment.Subset.Year$Quantity[i] <- as.numeric(eval(parse(text = Equipment.Subset.Year$Quantity[i])))
  
  Equipment.Subset.Year$Cost.Basis[i] <- Equipment.Subset.Year$Unit.Cost[i] * as.numeric(eval(parse(text = Equipment.Subset.Year$Quantity[i])))
  
  Equipment.Subset.Year$Depreciation[i] <- Equipment.Subset.Year$Cost.Basis[i] / Equipment.Subset.Year$Lifespan[i]
}
# Change Quantity to numeric because I am not a good coder
Equipment.Subset.Year$Quantity <- as.numeric(Equipment.Subset.Year$Quantity)
# Overwrite main data frame with year data only
Equipment.Subset <- Equipment.Subset.Year

#Equipment for used in farm strategy and harvest year
for (i in 1:nrow(Equipment.Subset.Global)) {
  # Evaluate the Quantity expression for this row, and do cost.basis and depreciation too
  Equipment.Subset.Global$Quantity[i] <- as.numeric(eval(parse(text = Equipment.Subset.Global$Quantity[i])))
  
  Equipment.Subset.Global$Cost.Basis[i] <- Equipment.Subset.Global$Unit.Cost[i] * as.numeric(eval(parse(text = Equipment.Subset.Global$Quantity[i])))
  
  Equipment.Subset.Global$Depreciation[i] <- Equipment.Subset.Global$Cost.Basis[i] / Equipment.Subset.Global$Lifespan[i]
}
# Change Quantity to numeric because I am not a good coder
Equipment.Subset.Global$Quantity <- as.numeric(Equipment.Subset.Global$Quantity)

# bind the two frames and delete leftovers
Equipment.Subset <- rbind(Equipment.Subset,Equipment.Subset.Global)
rm(Equipment.Subset.Global, Equipment.Subset.Year)

# Labor tasks similar to equipment but introduce seasonality

# Read Labor Outputs, subset by Harvest year harvest season farm strategy then merge with tasks, rate, and part time
Labor <- read_excel(file_path, sheet = 'Labor_Output')
Labor.Subset <- left_join(Labor,Task.Data, by = 'Task')

# Assign Harvest task to final year and season
Labor.Subset <- subset(Labor.Subset, Year != `Harvest Year` | Season != `Harvest Season`)
Labor.Subset$Year[Labor.Subset$Task == 'Harvest'|Labor.Subset$Task == 'Harvest Net Cleaning'|Labor.Subset$Task == 'Harvest Depinning + Cleaning'] <- `Harvest Year`
Labor.Subset$Season[Labor.Subset$Task == 'Harvest'|Labor.Subset$Task == 'Harvest Net Cleaning'|Labor.Subset$Task == 'Harvest Depinning + Cleaning'] <- `Harvest Season`

# Subset by Harvest Year, Farm type, and whether a task is completed (used for the cleaning)
Labor.Subset <- Labor.Subset[which(Labor.Subset$Year %in% Harvest.Year & Labor.Subset$Type %in% Farm.strat & Labor.Subset$Completed %in% 'Y'),]
Labor.Subset <- Labor.Subset[!(Labor.Subset$Year == `Harvest Year` & !(Labor.Subset$Season %in% Harvest.Season)), ]
# Create initialized columns because it annoys me to see a warnng message
Labor.Subset$Hours.Paid <- NA
Labor.Subset$Labor.Costs <- NA

for (i in 1:nrow(Labor.Subset)){
  Labor.Subset$Time[i] <- as.numeric(eval(parse(text = Labor.Subset$Time[i])))
  Labor.Subset$Trips[i] <- eval(parse(text = Labor.Subset$Trips[i]))
  Labor.Subset$Hours.Paid[i] <- ifelse(Labor.Subset$Trips[i] > 0, round_any(as.numeric(Labor.Subset$Time[i]), 8, f=ceiling), Labor.Subset$Time[i])
  Labor.Subset$Labor.Costs[i] <- as.numeric(Labor.Subset$Hours.Paid[i]) * `Part Time Wage` * Labor.Subset$Part.Time[i]
}
Labor.Subset$Time <- as.numeric(Labor.Subset$Time)
Labor.Subset$Trips <- as.numeric(Labor.Subset$Trips)

# Subset out Specialized equipment with quantity = 0 or time = Inf  
Labor.Subset <- subset(Labor.Subset, Time != Inf)
Equipment.Subset <- subset(Equipment.Subset, Quantity!=0)

# Read Fuel Outputs, subset to relevant, and merge with price.gallon and fuel.trip
Fuel <- read_excel(file_path, sheet = 'Fuel_Output')
Fuel.Subset <- left_join(Fuel,Fuel.Data, by = 'Vehicle') 

Fuel.Subset <- Fuel.Subset[which(Fuel.Subset$Year %in% Harvest.Year & Fuel.Subset$Type %in% Farm.strat),]
# Same as labor, this kicks back a warning message and it's just annoying  
Fuel.Subset$Fuel.Cost <- NA
#Fuel for a given period by year
for (i in 1:nrow(Fuel.Subset)){
  Fuel.Subset$Fuel.Cost[i] <- Fuel.Subset$Price.Gallon[i] * Fuel.Subset$Usage.Trip[i] * as.numeric((sum(Labor.Subset[which(Labor.Subset$Year == Fuel.Subset$Year[i]),6]) + Fuel.Subset$Additional.Trips[i]))
}

# Read Fuel Outputs, subset to relevant and ,merge with price.gallon and fuel.trip
Maintenance <- read_excel(file_path, sheet = 'Maintenance_Output')
Maintenance.Subset <- left_join(Maintenance,Maint.Data, by = 'Item') 

Maintenance.Subset <- Maintenance.Subset[which(Maintenance.Subset$Year %in% Harvest.Year & Maintenance.Subset$Type %in% Farm.strat),]

#Maintenance for a given period
for (i in 1:nrow(Maintenance.Subset)){
  Maintenance.Subset$Maintenance.Cost[i] <- as.numeric(eval(parse(text = Maintenance.Subset$Maintenance.Cost[i])))
}
Maintenance.Subset$Maintenance.Cost <- as.numeric(Maintenance.Subset$Maintenance.Cost)
# Alright, this should be the basic global data, if we add up all columns we should get the annual 
# values once all year classes have been introduced.  The below section is the fun stuff, 
# deliverable metrics!

# Metric ideas

# Total lease size and Cost
# Leases have three designations: Standard, LPA, and Experimental with different fees by acreage
# Take longline length, mooring length (distance along bottom), and Longline Spacing to calculate acreage

# Create data frame with Longline Quantity in it for 'reasons'
Lease.Footprint <- data.frame(`Longline Quantity` = Equipment.Subset[which(Equipment.Subset$Equipment == 'Longline + Installation'),8])
# total longline length is the total cost of global rope + bottom length of mooring rope (pythag)*2*number of longlines 
Lease.Footprint$Feet.Longline.Total <- (Lease.Footprint$`Longline.Quantity`*`Individual Longline Length`) + 
  ((Lease.Footprint$`Longline.Quantity`*2) * sqrt(((`Longline Depth`-`Longline Suspended Depth`)*`Mooring Length`)^2 - (`Longline Depth`-`Longline Suspended Depth`)^2))
# Size of each longline in the event of multiple longlines
Lease.Footprint$l.Feet <- Lease.Footprint$Feet.Longline.Total/Lease.Footprint$`Longline.Quantity`
# Total meters for longline length (no m/longline as most growers won't find it relevant)
Lease.Footprint$Meters.Longline.Total <- Lease.Footprint$Feet.Longline.Total * .3048
# Total lease area in ft^2
Lease.Footprint$A.Feet <- Lease.Footprint$Feet.Longline.Total * `Longline Spacing`
# Total lease area in m^2
Lease.Footprint$A.Meters <- Lease.Footprint$A.Feet * .3048

# This value is the Acreage which will be the most valuable statistic for growers
Lease.Footprint$Acres <- (Lease.Footprint$Feet.Longline.Total*`Longline Spacing`)*.0000229568

# Leasing fees, from DMR and updated annually with lease type, Application fee, and annual fixed fee
Lease.Type.M <- data.frame(     # DMR lease type
  Type = c('Experimental Lease','LPA','Standard Lease'),     # DMR lease types
  App.Fee = c(0,100,1500),     # Application fee (1 time)
  Annual.Fee = c(50,100,100)     # Annual lease fee charged by the acre
)

# Set lease type from Preset
Lease.Type.M <- subset(Lease.Type.M, Type == `Lease Type`)

# Create year month

# Set year start to August 1, Year and create an annual date matrix
Year_0 <- ymd(`Starting Year`,truncated=2L) + months(7)
Date.Frame <- data.frame(Year = seq(0,10,by=1), 
                         Date = seq(ymd(Year_0),ymd(Year_0 %m+% years(10)),by = 'year'))
Date.Frame$Date<- as.yearmon(Date.Frame$Date)

# Labor metrics 
# Calculate total labor time by season, hours worked, hours paid, etc and rounded up work days 
# for pane 1 graph
Labor.Subset$Hours.Paid<- as.numeric(Labor.Subset$Hours.Paid)
Labor.metrics <- aggregate(cbind(Time,Trips,Labor.Costs,Hours.Paid)~Season,data = Labor.Subset, sum)
Labor.metrics$Work.Days <- round(Labor.metrics$Hours.Paid/8)  

# Economic Metrics
# Create a matrix to assign columns by their year class, growers might have to wait up to 4 years
# Prior to first sale and that leads to significant deferment of costs.
# Then add up total costs for all categories plus consumables == cost of goods sold.

# Create a cost of good sold data set starting with years  
COG <- Date.Frame  

# Sum equipment, Labor, Fuel, and Maintenance by year cumulatively until the 
# final year when it is a fully operational farm

COG$Equipment <- ifelse(COG$Year == 0,sum(Equipment.Subset$Cost.Basis[which(Equipment.Subset$Year=='Y0' | Equipment.Subset$Year=='all')]),
                        ifelse(COG$Year == 1,sum(Equipment.Subset$Cost.Basis[which(Equipment.Subset$Year== 'Y1')]),
                               ifelse(COG$Year == 2, sum(Equipment.Subset$Cost.Basis[which(Equipment.Subset$Year== 'Y2')]),
                                      ifelse(COG$Year == 3, sum(Equipment.Subset$Cost.Basis[which(Equipment.Subset$Year== 'Y3')]),0))))

COG$Labor <- ifelse(COG$Year == 0, sum(Labor.Subset$Labor.Costs[which(Labor.Subset$Year %in% Y0)]),
                    ifelse(COG$Year == 1, sum(Labor.Subset$Labor.Costs[which(Labor.Subset$Year %in% Y1)]),
                           ifelse(COG$Year == 2, sum(Labor.Subset$Labor.Costs[which(Labor.Subset$Year %in% Y2)]),
                                  ifelse(COG$Year == 3, sum(Labor.Subset$Labor.Costs[which(Labor.Subset$Year %in% Y3)]),
                                         sum(Labor.Subset$Labor.Costs)))))    

COG$Fuel <- ifelse(COG$Year == 0, sum(Fuel.Subset$Fuel.Cost[which(Fuel.Subset$Year %in% Y0)]),
                   ifelse(COG$Year == 1, sum(Fuel.Subset$Fuel.Cost[which(Fuel.Subset$Year %in% Y1)]),
                          ifelse(COG$Year == 2, sum(Fuel.Subset$Fuel.Cost[which(Fuel.Subset$Year %in% Y2)]),
                                 ifelse(COG$Year == 3, sum(Fuel.Subset$Fuel.Cost[which(Fuel.Subset$Year %in% Y3)]),
                                        sum(Fuel.Subset$Fuel.Cost))))) 

COG$Maintenance <- ifelse(COG$Year == 0, sum(Maintenance.Subset$Maintenance.Cost[which(Maintenance.Subset$Year %in% Y0)]),
                          ifelse(COG$Year == 1, sum(Maintenance.Subset$Maintenance.Cost[which(Maintenance.Subset$Year %in% Y1)]),
                                 ifelse(COG$Year == 2, sum(Maintenance.Subset$Maintenance.Cost[which(Maintenance.Subset$Year %in% Y2)]),
                                        ifelse(COG$Year == 3, sum(Maintenance.Subset$Maintenance.Cost[which(Maintenance.Subset$Year %in% Y3)]),
                                               sum(Maintenance.Subset$Maintenance.Cost)))))

# Add consumables which is just an annual odds and ends expense
COG$Consumables <- Consumables

# Sum for the Cost of Goods Sold
COG$`Cost of Goods Sold` <- rowSums(COG[,(3:6)])

# Note above, these are all variable costs

# Fixed overhead costs (FOC)
# Create a data frame for FOC costs annually.  These are mostly a flat annual fee except lease rent
# and depreciation which vary based on the initial application fee in year 0 and the time in which
# Equipment was first purchased/put to use
FOC <- Date.Frame

# Lease fees, consists of an initial application fee and then an annual fee based on acreage
FOC$Lease <- ifelse(FOC$Year == 0, 
                    Lease.Type.M$App.Fee + (Lease.Type.M$Annual.Fee*Lease.Footprint$Acres), 
                    Lease.Type.M$Annual.Fee*Lease.Footprint$Acres)
# Insurance is just the summed annual insurance payments
FOC$Insurance <- Insurance
# Annual shellfish aquaculture license fee
FOC$`Aquaculture License` <- `Shellfish License`
# Owner Salary is an annual payment amount to the owner
FOC$`Owner Salary` <- `Owner Salary`
# Full time employee salary is an annual salary multiplied by the number of full time employees
FOC$`Full Time Employee` <- `Full Time Employee` * `Employee Salary`
# Depreciation is based on the lifespan of a piece of equipment divided by the cost of the item.
# It is an unrealized expense in that the cash is not spent, but should be considered reinvested to
# replace gear in the future
FOC$Depreciation <- ifelse(COG$Year == 0,sum(Equipment.Subset$Depreciation[which(Equipment.Subset$Year %in% Y0)]),
                           ifelse(COG$Year == 1,sum(Equipment.Subset$Depreciation[which(Equipment.Subset$Year %in% Y1)]),
                                  ifelse(COG$Year == 2, sum(Equipment.Subset$Depreciation[which(Equipment.Subset$Year %in% Y2)]),
                                         ifelse(COG$Year == 3, sum(Equipment.Subset$Depreciation[which(Equipment.Subset$Year %in% Y3)]),
                                                sum(Equipment.Subset$Depreciation)))))
# Sum all rows for total annual fixed operating costs
FOC$`Fixed Overhead Costs` <- rowSums(FOC[,(3:8)]) 

# Annual costs irregardless of year and business plan
# Sum Insurance, Shelffish/Aq License, Lease Rent, Owner Salary, Depreciation (By year)
# These are fixed overhead costs, ie costs that cannot be circumvented 

# Cost of production is COG+FOC and is all realized and unrealized expenses...basically the total cost
COP <- data.frame(Date.Frame,COG$`Cost of Goods Sold`,FOC$`Fixed Overhead Costs`)
colnames(COP)[3] <- "Cost of Goods Sold"
colnames(COP)[4] <- "Fixed Overhead Costs"
COP$`Cost of Production` <- rowSums(COP[,3:4])
# Cumulative COP is what I am calling debt
COP$Debt <- cumsum(COP$`Cost of Production`)
# Scallops sold at market, this is a fixed amount
COP$`Individual Scallops` <-  ifelse(COP$Year == 0 & `Harvest Year` == 'Y0', Growth.Data$Market.Product,
                                     ifelse(COP$Year == 1 & `Harvest Year` == 'Y1', Growth.Data$Market.Product,
                                            ifelse(COP$Year == 2 & `Harvest Year` == 'Y2', Growth.Data$Market.Product,
                                                   ifelse(COP$Year == 3 & `Harvest Year` == 'Y3', Growth.Data$Market.Product,
                                                          ifelse(COP$Year >3 & `Harvest Year` %in% Y4, Growth.Data$Market.Product,0)))))
# Shell height in millimeters of market scallops
COP$`ShellHeight (mm)` <- ifelse(COP$`Individual Scallops` == 0, 0,Growth.Data$Sh_Height)
# Shell height in inches for market scallops, we will use imperial units for the 
# app since it is more valuable to fishermen
COP$`ShellHeight (Inches)` <- ifelse(COP$`Individual Scallops` == 0, 0,Growth.Data$Sh_Height.inches)
# Adductor weight in grams
COP$`Adductor (g)` <- ifelse(COP$`Individual Scallops` == 0, 0,Growth.Data$Adductor)
# Adductor weight in pounds
COP$`Adductor (lb)` <- ifelse(COP$`Individual Scallops` == 0, 0,Growth.Data$Adductor.lbs)
# The industry standard for sale is as the amount of adductor meats in a pound.
# Also called count per pound.  It's a psuedo weight binning strategy.
# In general 30 count is small, 20 count is pretty normal and U10 is a large premium scallop
# This isn't used in the calculations but is valuable to growers at a glance
COP$`Adductor Count per lb` <- ifelse(COP$`Individual Scallops` == 0, 0,Growth.Data$count.lbs)
# calculate run rate and break even price for scallops, run rate is essentially constant
# and breka even is averaged out over time.  run rate is the asymptote for break even curve
COP$`Run Rate (Whole Scallop)` <- COP$`Cost of Production`/COP$`Individual Scallops`
COP$`Break Even Price (Whole Scallop)` <- COP$Debt/cumsum(COP$`Individual Scallops`)
COP$`Run Rate (Adductor)` <- COP$`Cost of Production`/(COP$`Individual Scallops`*COP$`Adductor (lb)`)
COP$`Break Even Price (Adductor)` <- COP$Debt/cumsum((COP$`Individual Scallops`*COP$`Adductor (lb)`))
COP <- COP %>% 
  mutate_if(is.numeric, round,digits=2)
COP$Date <- year(COP$Date)
COP$`Individual Scallops` <- round(COP$`Individual Scallops`, digits = 0)


# COG + FOC = Cost of Production
# We calculate from here two values, 10 year break even and run rate.
# 10 year break even takes all 10 years of COP, sums them, and then averages by total scallop
# sales (Individuals & Price/lb).  Run rate assumes no initial debt for equipment purchases.

# COP, break even, and run rate used in analysis  

# Next allow growers to set a price (maybe in primary inputs?) or in this section if possible
# Allow entry of scallop price/individual and adductor/lbs
Whole.Scallop.Price <- 2.50
ScallopAdductor.lbs <- 35

# gross profit, 10 year forecast
# subtract COGs from the total scallops sold each year (or total lbs sold each year) * Price (total revenue)
# so we have gross sales revenue = total income from product, gross profit is that minus COGS
# Margin is the percent difference between gross profit and sales revenue
# Net profit adds in FOC and profit margin is sales revenue and COP differences
# Finally depreciation is taken out as an unrealized expense for physical cash flow.

PL.add <- Date.Frame
PL.add$`Gross Sales Revenue` <- ScallopAdductor.lbs*(COP$`Individual Scallops`*Growth.Data$Adductor.lbs)
PL.add$`Gross Profit` <- PL.add$`Gross Sales Revenue` - COP$`Cost of Goods Sold` 
PL.add$`Gross Profit Margin` <- (PL.add$`Gross Profit`/PL.add$`Gross Sales Revenue`)*100
PL.add$`Net Profit` <- PL.add$`Gross Profit` - COP$`Fixed Overhead Costs`
PL.add$`Net Profit Margin` <- (PL.add$`Net Profit`/PL.add$`Gross Sales Revenue`)*100
PL.add$Depreciation <- FOC$Depreciation
PL.add$`Year End Cash Flow` <- cumsum(PL.add$`Net Profit`-FOC$Depreciation)

PL.Whole <- Date.Frame
PL.Whole$`Gross Sales Revenue` <- Whole.Scallop.Price*COP$`Individual Scallops`
PL.Whole$`Gross Profit` <-  PL.Whole$`Gross Sales Revenue` - COP$`Cost of Goods Sold` 
PL.Whole$`Gross Profit Margin` <-  (PL.Whole$`Gross Profit`/PL.Whole$`Gross Sales Revenue`)*100
PL.Whole$`Net Profit` <-  PL.Whole$`Gross Profit` - COP$`Fixed Overhead Costs`
PL.Whole$`Net Profit Margin` <-  (PL.Whole$`Net Profit`/ PL.Whole$`Gross Sales Revenue`)*100
PL.Whole$Depreciation <- FOC$Depreciation
PL.Whole$`Year End Cash Flow` <- cumsum(PL.Whole$`Net Profit`-FOC$Depreciation)
# Net Profit, 10 years
# Subtract FOC from GP to get net profit


# Finally, a Downloadable series of tables in an excel or .csv format including:
# Equipment, Labor, Fuel, and Maintenance tables + Primary and secondary inputs and Pane2 contents


  # Create desired outputs in a table over all iterations.
  # For this option I want
  
  Output <- data.frame('Product' = Product, 'Harvest Year' = `Harvest Year`, 
                       'Harvest Season' = `Harvest Season`, 'Grow Out Method' = `Grow Out Method`
                       , 'Lease Footprint (Acres)' = Lease.Footprint$Acres)
  
  Output <- cbind(Output, COG[11,], COP[11,],FOC[11,])
  #Output <- Output[,-c(9,15,16,17,31,32,39)]
  Output <- cbind(Output, 'Sea.Days' = sum(Labor.metrics$Trips),
                  'Totalequipment' = sum(COG[,3]), 'Longlines' = Lease.Footprint$Longline.Quantity,
                  'Spec.equip' = Spec.equip, 'Ownership' = ownership)
  
  Output.Analysis3 <- rbind(Output.Analysis3,Output)
  
}

Output.Analysis.sub <- subset(Output.Analysis3, Sea.Days <= 234)
Output.Analysis.sub$BEAkg <- Output.Analysis.sub$`Break Even Price (Adductor)`/2.205

write.csv(Output.Analysis3, 'Output_Analysis_3.csv')

Output.Analysis.sub$Grow.Out.Method[Output.Analysis.sub$Grow.Out.Method == 'Ear Hanging'] <- 'Ear-Hanging'


options(scipen = 999)


    
P5 <- ggplot(Output.Analysis.sub, aes(x=Product, y = BEAkg, color = Spec.equip,shape = Ownership))  + 
  geom_point()+
  theme_minimal() +
  scale_color_brewer(palette = 'Set1') +
  scale_x_continuous(labels = scales::comma)+
  geom_rect(aes(ymin = 22,ymax = 52,xmin = 45000, xmax = 100000), color = 'red', alpha = 0) +
  geom_rect(aes(ymin = 9,ymax = 15,xmin = 300000, xmax = 405000), color = 'blue', alpha =0) +
  annotate("text", x=75000, y = 23, label = "Small-Scale") +
  annotate("text", x=350000, y = 10, label = "Large-Scale") +
  theme(panel.grid.major.x=element_blank(), panel.grid.major.y=element_blank()) + #remove gridlines
  theme(panel.grid.minor.x=element_blank(), panel.grid.minor.y=element_blank()) + #remove gridlines
  ggtitle("") +
  ylab(bquote('Adductor-Only BE10 '(USD~kg^-1)))+
  xlab("Target Scallop Spat")+
  labs(color = 'Specialized Equipment Type', shape = 'Ownership')+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        axis.text.x=element_text(face='bold'),
        axis.text.y=element_text(face='bold'),
        legend.position = c(0.995, 0.99),
        legend.justification = c("right", "top"),
        legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid'))

Output.Analysis.subL <- subset(Output.Analysis.sub, Product <= 100000)
P6 <- ggplot(Output.Analysis.subL, aes(x=Product, y = BEAkg, color = Spec.equip,shape = Ownership))  + 
  geom_point()+
  theme_minimal() +
  scale_color_brewer(palette = 'Set1') +
  scale_x_continuous(labels = scales::comma)+
  theme(panel.grid.major.x=element_blank(), panel.grid.major.y=element_blank()) + #remove gridlines
  theme(panel.grid.minor.x=element_blank(), panel.grid.minor.y=element_blank()) + #remove gridlines
  ggtitle("") +
  ylab(bquote('Adductor-Only BE10 '(USD~kg^-1)))+
  xlab("Target Scallop Spat")+
  theme(panel.border = element_rect(colour = "red", fill=NA, size=1), legend.position = "none")

Output.Analysis.subH <- subset(Output.Analysis.sub, Product >= 300000)
P7 <- ggplot(Output.Analysis.subH, aes(x=Product, y = BEAkg, color = Spec.equip, shape = Ownership))  + 
  geom_point()+
  theme_minimal() +
  scale_color_brewer(palette = 'Set1') +
  scale_x_continuous(labels = scales::comma)+
  theme(panel.grid.major.x=element_blank(), panel.grid.major.y=element_blank()) + #remove gridlines
  theme(panel.grid.minor.x=element_blank(), panel.grid.minor.y=element_blank()) + #remove gridlines
  ggtitle("") +
  ylab("")+
  xlab("Target Scallop Spat")+
  theme(panel.border = element_rect(colour = "blue", fill=NA, size=1), legend.position = "none")

P8 <- plot_grid(P6,P7, nrow=1)

plot_grid(P5,P8, nrow = 2, labels = 'AUTO')

ggplot(Output.Analysis.sub, aes(x=Product, y = `Break Even Price (Whole Scallop)`, color = Spec.equip,shape = Ownership))  + 
  geom_point()+
  theme_minimal() +
  scale_color_brewer(palette = 'Set1') +
  scale_x_continuous(labels = scales::comma)+
  theme(panel.grid.major.x=element_blank(), panel.grid.major.y=element_blank()) + #remove gridlines
  theme(panel.grid.minor.x=element_blank(), panel.grid.minor.y=element_blank()) + #remove gridlines
  ggtitle("") +
  ylab(bquote('Adductor-Only BE10  '(USD~kg^-1)))+
  xlab("Target Scallop Spat")+
  labs(color = 'Specialized Equipment Type', shape = 'Ownership')+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        axis.text.x=element_text(face='bold'),
        axis.text.y=element_text(face='bold'),
        legend.position = c(0.995, 0.99),
        legend.justification = c("right", "top"),
        legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid'))



P5 <- ggplot(Output.Analysis.sub, aes(x=Product, y = BEAkg, color = Spec.equip))  + 
  geom_line(aes(linetype = Ownership), size = 1.1)+
  theme_minimal() +
  scale_color_brewer(palette = 'Set1') +
  scale_x_continuous(labels = scales::comma)+
  geom_rect(aes(ymin = 22,ymax = 52,xmin = 45000, xmax = 100000), color = 'red', alpha = 0) +
  geom_rect(aes(ymin = 9,ymax = 15,xmin = 300000, xmax = 405000), color = 'blue', alpha =0) +
  annotate("text", x=75000, y = 23, label = "Small-Scale") +
  annotate("text", x=350000, y = 10, label = "Large-Scale") +
  theme(panel.grid.major.x=element_blank(), panel.grid.major.y=element_blank()) + #remove gridlines
  theme(panel.grid.minor.x=element_blank(), panel.grid.minor.y=element_blank()) + #remove gridlines
  ggtitle("") +
  ylab(bquote('Adductor-Only BE10  '(USD~kg^-1)))+
  xlab("Target Scallop Spat")+
  labs(color = 'Specialized Equipment Type', shape = 'Ownership')+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        axis.text.x=element_text(face='bold'),
        axis.text.y=element_text(face='bold'),
        legend.position = c(0.995, 0.99),
        legend.justification = c("right", "top"),
        legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid'))

Output.Analysis.subL <- subset(Output.Analysis.sub, Product <= 100000)
P6 <- ggplot(Output.Analysis.subL, aes(x=Product, y = BEAkg, color = Spec.equip))  + 
  geom_line(aes(linetype = Ownership), size = 1.3)+
  theme_minimal() +
  scale_color_brewer(palette = 'Set1') +
  scale_x_continuous(labels = scales::comma)+
  theme(panel.grid.major.x=element_blank(), panel.grid.major.y=element_blank()) + #remove gridlines
  theme(panel.grid.minor.x=element_blank(), panel.grid.minor.y=element_blank()) + #remove gridlines
  ggtitle("") +
  ylab(bquote('Adductor-Only BE10  '(USD~kg^-1)))+
  xlab("Target Scallop Spat")+
  theme(panel.border = element_rect(colour = "red", fill=NA, size=1), legend.position = "none")

Output.Analysis.subH <- subset(Output.Analysis.sub, Product >= 300000)
P7 <- ggplot(Output.Analysis.subH, aes(x=Product, y = BEAkg, color = Spec.equip))  + 
  geom_line(aes(linetype = Ownership), size = 1.3)+
  theme_minimal() +
  scale_color_brewer(palette = 'Set1') +
  scale_x_continuous(labels = scales::comma)+
  theme(panel.grid.major.x=element_blank(), panel.grid.major.y=element_blank()) + #remove gridlines
  theme(panel.grid.minor.x=element_blank(), panel.grid.minor.y=element_blank()) + #remove gridlines
  ggtitle("") +
  ylab("")+
  xlab("Target Scallop Spat")+
  theme(panel.border = element_rect(colour = "blue", fill=NA, size=1), legend.position = "none")

P8 <- plot_grid(P6,P7, nrow=1)

plot_grid(P5,P8, nrow = 2, labels = 'AUTO')
