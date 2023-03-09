library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(rsconnect)

prepare_data <- function() {
  #TODO Save file
  
  if (file.exists("asylum_data.RData")) {
    load("asylum_data.RData")
    
  } else {
    dt.asylum.data <- read.csv("data\\asylum-decisions.csv", header=TRUE, sep=";")
    dt.country.income <- read.csv("data\\income_data.csv", header=TRUE)
    dt.country.capitals <- read.csv("data\\concap.csv", header=TRUE)
    dt.country.info.merge <- merge(dt.country.income, dt.country.capitals, by.x= "Country", 
                                   by.y = "CountryName")
    
    # Merge for Asylum Information
    dt.asylum.data <- merge(dt.asylum.data, dt.country.info.merge, by.x="Country.of.asylum", by.y="Country", all.x=TRUE)
    # renaming column
    names(dt.asylum.data)[names(dt.asylum.data)=="Income.group"] <- "Asylum_Income"
    names(dt.asylum.data)[names(dt.asylum.data)=="CapitalName"] <- "Asylum_Capital"
    names(dt.asylum.data)[names(dt.asylum.data)=="CapitalLatitude"] <- "Asylum_Capital_Lat"
    names(dt.asylum.data)[names(dt.asylum.data)=="CapitalLongitude"] <- "Asylum_Capital_Long"
    
    # Merge for Origin Information
    dt.asylum.data <- merge(dt.asylum.data, dt.country.info.merge, by.x="Country.of.origin", by.y="Country", all.x=TRUE)
    # renaming columns
    names(dt.asylum.data)[names(dt.asylum.data)=="Income.group"] <- "Origin_Income"
    names(dt.asylum.data)[names(dt.asylum.data)=="CapitalName"] <- "Origin_Capital"
    names(dt.asylum.data)[names(dt.asylum.data)=="CapitalLatitude"] <- "Origin_Capital_Lat"
    names(dt.asylum.data)[names(dt.asylum.data)=="CapitalLongitude"] <- "Origin_Capital_Long"
    
    save(dt.asylum.data, file="asylum_data.RData")
    
  }
  # Return prepared data
  return(dt.asylum.data)

}

aggregate_data <- function() {
  #This function sums up all information for each year
  dt.aggregated.asylum <- prepare_data() %>%
    group_by(Year) %>%
    summarise(Total_decisions = sum(Total.decisions),
              Recognized_decisions = sum(Recognized.decisions),
              Rejected_decisions = sum(Rejected.decisions),
              Otherwise_closed = sum(Otherwise.closed),
              # Calculate all closed cases
              Total_closed = sum(Rejected_decisions) + sum(Otherwise.closed),
              Complementary_protection = sum(Complementary.protection))
  
  return(dt.aggregated.asylum)
}
# Group by nach Land und Jahr, da manche Rows doppelt

# Header bauen


