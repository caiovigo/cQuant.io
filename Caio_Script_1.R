
library(tidyverse)
library(magrittr)
library(lubridate)
library(data.table)

# library(PerformanceAnalytics)   # To use chart.Correlation()
library(zoo)                    # For time series [using only as.Date() & as.yearmon()]
library(corrplot)               # To use corrplot()

my_path <- "G:/My Drive/Documents/PhD/19. Job Market/60.Coding Exercises/02.cQuant.io/"

#--------------------------------------------------------------------------
# Task 1: Import data

contracts <- read_csv(paste0(my_path, "model-dev-coding-exercise-public-master/contracts/", "Contracts.csv"))

#-- fuelPrices --
fuelPrices_GDA_TETSTX <- read_csv(paste0(my_path, "model-dev-coding-exercise-public-master/fuelPrices/", "GDA_TETSTX.csv"))
fuelPrices_Henry_Hub <- read_csv(paste0(my_path, "model-dev-coding-exercise-public-master/fuelPrices/", "Henry Hub.csv"))

fuelPrices <- bind_rows(fuelPrices_GDA_TETSTX, fuelPrices_Henry_Hub)
rm(fuelPrices_GDA_TETSTX, fuelPrices_Henry_Hub)

Plant_Parameters <- read_csv(paste0(my_path, "model-dev-coding-exercise-public-master/plantParameters/", "Plant_Parameters.csv"))

#-- powerPrices --
years_list <- c(2016, 2017, 2018, 2019)
powerPrices <- NULL

for (year_j in years_list){
  powerPrices <- bind_rows(powerPrices,
                           read_csv(paste0(my_path, "model-dev-coding-exercise-public-master/powerPrices/", "ERCOT_DA_Prices_", year_j, ".csv")))
}

#--------------------------------------------------------------------------
# Task 2: Calculate basic descriptive statistics

powerPrices_desc_stats <- powerPrices %>% 
                            mutate(date_ym=format(as.Date(Date), "%Y-%m")) %>% 
                            group_by(date_ym, SettlementPoint) %>% 
                            summarize(Mean = mean(Price), 
                                      Min = min(Price), 
                                      Max = max(Price), 
                                      SD = sd(Price))

#--------------------------------------------------------------------------
# Task 3: Calculate volatility
powerPrices_volatility <- powerPrices %>% 
                            mutate(date_ym=format(as.Date(Date), "%Y-%m")) %>% 
                            group_by(SettlementPoint) %>% 
                            arrange(SettlementPoint) %>% 
                            mutate(log_ret=log(Price/lag(Price,1))) %>% 
                            na.omit() %>% 
                            ungroup() %>%
                            group_by(date_ym, SettlementPoint) %>%
                            summarize(Volatility=sd(log_ret))

Power_Price_Statistics <- left_join(powerPrices_desc_stats, powerPrices_volatility, by = c("date_ym", "SettlementPoint")) %>% 
                            mutate(Year=year(parse_date_time(date_ym, "ym")), 
                                   Month=month(parse_date_time(date_ym, "ym"))) %>%
                            ungroup() %>% 
                            select(SettlementPoint, Year, Month, Mean, Min, Max, SD, Volatility)
                          
#--------------------------------------------------------------------------
# Task 4: Write the results to file

write.csv(Power_Price_Statistics, file=paste0(my_path, "Output/", "MonthlyPowerPriceStatistics.csv"))

#--------------------------------------------------------------------------
# Task 5: Expand the contracts across relevant time periods

# contracts %>% 
#   filter(Granularity=="Daily") %>% slice(1) %>% 
#   # group_by(ContractName) %>%
#   complete(my_date = seq(from=as.Date("2017-01-01"), to=as.Date("2017-03-31"))) %>%
#   fill(ContractName, DealType, Volume, Granularity, StrikePrice, Premium, PriceName )

Daily_S1 <- tibble(date_granularity = seq(from=as.Date(filter(contracts,Granularity=="Daily", ContractName=="S1")$StartDate), 
                                             to  =as.Date(filter(contracts,Granularity=="Daily", ContractName=="S1")$EndDate), by = 'days'),
                   ContractName = "S1") %>% 
                   left_join(filter(contracts,Granularity=="Daily", ContractName=="S1"), by = "ContractName")  
                  
Daily_O1 <- tibble(date_granularity = seq(from=as.Date(filter(contracts,Granularity=="Daily", ContractName=="O1")$StartDate), 
                                             to  =as.Date(filter(contracts,Granularity=="Daily", ContractName=="O1")$EndDate), by = 'days'),
                    ContractName = "O1") %>% 
                    left_join(filter(contracts,Granularity=="Daily", ContractName=="O1"), by = "ContractName")   


Hourly_S2 <- tibble(date_granularity = seq(from=ymd_hms(paste0((filter(contracts,Granularity=="Hourly", ContractName=="S2")$StartDate), " 00:00:00")), 
                                              to  =ymd_hms(paste0((filter(contracts,Granularity=="Hourly", ContractName=="S2")$EndDate), " 23:00:00")), by = 'hours'),
                    ContractName = "S2") %>% 
                    left_join(filter(contracts,Granularity=="Hourly", ContractName=="S2"), by = "ContractName")  

Hourly_O2 <- tibble(date_granularity = seq(from=ymd_hms(paste0((filter(contracts,Granularity=="Hourly", ContractName=="O2")$StartDate), " 00:00:00")), 
                                              to  =ymd_hms(paste0((filter(contracts,Granularity=="Hourly", ContractName=="O2")$EndDate), " 23:00:00")), by = 'hours'),
                    ContractName = "O2") %>% 
                    left_join(filter(contracts,Granularity=="Hourly", ContractName=="O2"), by = "ContractName") 

Daily_contracts <- bind_rows(Daily_S1, Daily_O1)
Hourly_contracts <- bind_rows(Hourly_S2, Hourly_O2)

#--------------------------------------------------------------------------
# Task 6: Join relevant prices

Daily_contracts %<>% 
  left_join(fuelPrices, by=c("PriceName"="Variable", "date_granularity"="Date"))

Hourly_contracts %<>% 
  left_join(powerPrices, by=c("PriceName"="SettlementPoint", "date_granularity"="Date"))

#--------------------------------------------------------------------------
# Task 7: Calculate payoffs

Daily_contracts %<>% 
  mutate(Swaps = (Price-StrikePrice)*Volume,                  # Swaps: (AssetPrice – StrikePrice) x Volume
         Options = (max(Price-StrikePrice,0)-Premium)*Volume) # Options: [MAX(AssetPrice – StrikePrice, 0) - Premium] x Volume

Hourly_contracts %<>% 
  mutate(Swaps = (Price-StrikePrice)*Volume,                  # Swaps: (AssetPrice – StrikePrice) x Volume
         Options = (max(Price-StrikePrice,0)-Premium)*Volume) # Options: [MAX(AssetPrice – StrikePrice, 0) - Premium] x Volume

#--------------------------------------------------------------------------
# Task 8: Calculate aggregated payoffs

MonthlyContractPayoffs <- Daily_contracts %>% 
                              mutate(date_ym=format(as.Date(date_granularity), "%Y-%m")) %>% 
                              group_by(date_ym, ContractName) %>% 
                              summarize(Sum_Swaps = sum(Swaps, na.omit=TRUE), 
                                        Sum_Options = sum(Options, na.omit=TRUE)) %>% 
                              bind_rows(
                                    Hourly_contracts %>% 
                                      mutate(date_ym=format(as.Date(date_granularity), "%Y-%m")) %>% 
                                      group_by(date_ym, ContractName) %>% 
                                      summarize(Sum_Swaps = sum(Swaps, na.omit=TRUE), 
                                                Sum_Options = sum(Options, na.omit=TRUE))
                                        )

#--------------------------------------------------------------------------
# Task 9: Write the results to file
write.csv(MonthlyContractPayoffs, file=paste0(my_path, "Output/", "MonthlyContractPayoffs.csv"))

#--------------------------------------------------------------------------
# Task 10: Calculate the hourly running cost of each power plant included in the Plant_Parameters.csv input file

Plant_Dispatch <- Plant_Parameters %>% 
                    left_join(
                      fuelPrices %>% 
                        mutate(Year=year(Date),
                               Month=month(Date)) %>% 
                        rename("Fuel_Price"="Price"),
                      by=c("Year"="Year", "Month"="Month", "FuelPriceName"="Variable")) %>% 
                      mutate(RunningCost=((Fuel_Price+FuelTransportationCost)*HeatRate)+VOM) #RunningCost = ((FuelPrice + FuelTransportationCost) x HeatRate) + VOM

#--------------------------------------------------------------------------
# Task 11: Join the hourly power prices

Plant_Dispatch %<>% 
  left_join(
    powerPrices %>% 
      mutate(Date_daily=as.Date(format(as.Date(Date), "%Y-%m-%d"))) %>% 
      rename("Power_Price"="Price",
             "Date_hour"="Date"),
    by=c("Date"="Date_daily", "PowerPriceName"="SettlementPoint"))

# Note (start here ) -------------
# The pdf says that it should be 30,040 observations, however, checking the original 
# powerPrices datasets, we see that actually there are only
# 35,036 rows. See code below:

  powerPrices %>% 
    filter(SettlementPoint=="HB_HOUSTON" | SettlementPoint=="HB_SOUTH") %>% 
    filter(year(Date)==2018 | year(Date)==2017) %>% 
    dim()

# Note (End here ) -------------

#--------------------------------------------------------------------------
# Task 12: Identify hours in which the power plant should be on

Plant_Dispatch %<>% 
    mutate(Generation=ifelse(Power_Price > RunningCost, Capacity, 0),
           RunningMargin=(Power_Price-RunningCost)*Generation) # RunningMargin = (PowerPrice – RunningCost) * Generation

#--------------------------------------------------------------------------
# Task 13: Account for start costs
Plant_Dispatch %<>% 
  group_by(group = data.table::rleid(RunningMargin > FixedStartCost)) %>% 
  left_join(Plant_Dispatch %>% 
              group_by(group = data.table::rleid(RunningMargin > FixedStartCost)) %>%
              summarize(Generation2 = sum(RunningMargin)), by = "group")

#--------------------------------------------------------------------------
# Task 14: Write the results to file
write.csv(Plant_Dispatch, file=paste0(my_path, "Output/", "Plant_Dispatch.csv"))
