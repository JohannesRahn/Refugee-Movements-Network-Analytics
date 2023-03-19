# List of required packages
required.packages <- c(
  "shiny",
  "shinydashboard",
  "shinyalert",
  "ggplot2",
  "dplyr",
  "rsconnect",
  "shinyWidgets",
  "igraph",
  "data.table",
  "leaflet",
  "sp",
  "circlize",
  "networkD3",
  "visNetwork",
  "RColorBrewer",
  "ggimage",
  "png",
  "grid"
)

# Function to check and install missing packages
check.and.install.packages <- function() {
  installed_packages <- installed.packages()
  for (package in required.packages) {
    if (!(package %in% rownames(installed_packages))) {
      message("Installing package: ", package)
      install.packages(package)
    }
  }
}

initialize.data <- function() {
  dt.asylum.data <- read.csv("data/asylum-decisions.csv", header=TRUE, sep=";")
  dt.country.income <- read.csv("data/income_data.csv", header=TRUE)
  dt.country.capitals <- read.csv("data/concap.csv", header=TRUE)
  dt.country.meta.info <- read.csv("data/country_region.csv", header=TRUE)
  
  dt.country.info.merge <- merge(dt.country.income, dt.country.meta.info,
                                 by.x= "Code", by.y = "alpha.3")
  dt.country.info.merge <- merge(dt.country.info.merge, dt.country.capitals, 
                                 by.x= "alpha.2", by.y="CountryCode")
  
  # Merge for Asylum Information
  dt.asylum.data <- merge(dt.asylum.data, dt.country.info.merge, 
                          by.x="Country.of.asylum..ISO.", by.y="Code", 
                          all.x = TRUE)
  # renaming column
  names(dt.asylum.data)[names(dt.asylum.data)=="Income.group"] <- "Asylum_Income"
  names(dt.asylum.data)[names(dt.asylum.data)=="CapitalName"] <- "Asylum_Capital"
  names(dt.asylum.data)[names(dt.asylum.data)=="CapitalLatitude"] <- "Asylum_Capital_Lat"
  names(dt.asylum.data)[names(dt.asylum.data)=="CapitalLongitude"] <- "Asylum_Capital_Long"
  names(dt.asylum.data)[names(dt.asylum.data)=="region"] <- "Asylum_Region"
  names(dt.asylum.data)[names(dt.asylum.data)=="sub.region"] <- "Asylum_Sub_Region"
  names(dt.asylum.data)[names(dt.asylum.data)=="ContinentName"] <- "Asylum_Continent"
  
  
  # Merge for Origin Information
  dt.asylum.data <- merge(dt.asylum.data, dt.country.info.merge, 
                          by.x="Country.of.origin..ISO.", by.y="Code", all.x = TRUE)
  # renaming columns
  names(dt.asylum.data)[names(dt.asylum.data) == "Income.group"] <- "Origin_Income"
  names(dt.asylum.data)[names(dt.asylum.data) == "CapitalName"] <- "Origin_Capital"
  names(dt.asylum.data)[names(dt.asylum.data) == "CapitalLatitude"] <- "Origin_Capital_Lat"
  names(dt.asylum.data)[names(dt.asylum.data) == "CapitalLongitude"] <- "Origin_Capital_Long"
  names(dt.asylum.data)[names(dt.asylum.data) == "region"] <- "Origin_Region"
  names(dt.asylum.data)[names(dt.asylum.data) == "sub.region"] <- "Origin_Sub_Region"
  names(dt.asylum.data)[names(dt.asylum.data) == "ContinentName"] <- "Origin_Continent"
  
  col.order <- c("Country.of.origin", "Country.of.asylum", "Year", 
                 "Country.of.origin..ISO.", "Country.of.asylum..ISO.", 
                 "Authority", "Stage.of.procedure", "Cases...Persons", 
                 "Recognized.decisions", "Complementary.protection", 
                 "Rejected.decisions", "Otherwise.closed", "Total.decisions", 
                 "Asylum_Income", "Asylum_Capital", "Asylum_Capital_Lat", 
                 "Asylum_Capital_Long", "Origin_Income", "Origin_Capital", 
                 "Origin_Capital_Lat", "Origin_Capital_Long", "Asylum_Region", 
                 "Asylum_Sub_Region", "Asylum_Continent", "Origin_Region", 
                 "Origin_Sub_Region", "Origin_Continent", "alpha.2.y", 
                 "alpha.2.x")
  
  dt.asylum.data <- dt.asylum.data[, col.order]
  
  save(dt.asylum.data, file="data/asylum_data.RData")
}
