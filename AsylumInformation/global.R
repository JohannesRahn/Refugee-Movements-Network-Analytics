library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(rsconnect)
library(shinyWidgets)
library(igraph)
library(data.table)


prepare_data <- function(country.of.origin = NULL, country.of.destination = NULL, 
                         income.group.origin = NULL, income.group.asylum = NULL) {
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
  # Filtering in case arguments to filters were passed in the method
  if (!is.null(country.of.origin)) {
    dt.asylum.data <- dt.asylum.data[dt.asylum.data$Country.of.origin == country.of.origin,]
  }
  if (!is.null(country.of.destination)) {
    dt.asylum.data <- dt.asylum.data[dt.asylum.data$Country.of.asylum == country.of.destination,]
  }
  if (!is.null(income.group.origin)) {
    dt.asylum.data <- dt.asylum.data[dt.asylum.data$Origin_Income == income.group.origin,]
  }
  if (!is.null(income.group.asylum)) {
    dt.asylum.data <- dt.asylum.data[dt.asylum.data$Asylum_Income == income.group.asylum,]
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


# vertices.network <- function() {
#   dt.asylum <- prepare_data()
#   all.origins <- dt.asylum[, list(name = unique(Country.of.origin), type = TRUE)]
#   all.origins
#   all.asylum <- dt.asylum[, list(name = unique(Country.of.asylum), type = FALSE)]
#   all.asylum
#   all.vertices <- rbind(all.origins, all.asylum)
#   return(all.vertices)
# }
# Group by nach Land und Jahr, da manche Rows doppelt

# Header bauen
