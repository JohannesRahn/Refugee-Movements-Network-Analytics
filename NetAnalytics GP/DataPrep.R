prepare_data <- function() {

  dt.asylum.data <- read.csv("data\\asylum-decisions.csv", header=TRUE, sep=";")
  
  dt.country.income <- read.csv("data\\income_data.csv", header=TRUE)
  
  #country.demographics <- read.csv("data\\demographics.csv", header=TRUE, sep=";")
  
  dt.country.capitals <- read.csv("data\\concap.csv", header=TRUE)
  
  dt.country.info.merge <- merge(dt.country.income, dt.country.capitals, by.x= "Country", 
                             by.y = "CountryName")
  
  dt.full.data <- dt.asylum.data
  
  # Merge for Asylum Information
  dt.full.data <- merge(dt.full.data, dt.country.info.merge, by.x="Country.of.asylum", by.y="Country")
  # renaming column
  names(dt.full.data)[names(dt.full.data)=="Income.group"] <- "Asylum_Income"
  names(dt.full.data)[names(dt.full.data)=="CapitalName"] <- "Asylum_Capital"
  names(dt.full.data)[names(dt.full.data)=="CapitalLatitude"] <- "Asylum_Capital_Lat"
  names(dt.full.data)[names(dt.full.data)=="CapitalLongitude"] <- "Asylum_Capital_Long"
  
  # Merge for Origin Information
  dt.full.data <- merge(dt.full.data, dt.country.info.merge, by.x="Country.of.origin", by.y="Country")
  # renaming columns
  names(dt.full.data)[names(dt.full.data)=="Income.group"] <- "Origin_Income"
  names(dt.full.data)[names(dt.full.data)=="CapitalName"] <- "Origin_Capital"
  names(dt.full.data)[names(dt.full.data)=="CapitalLatitude"] <- "Origin_Capital_Lat"
  names(dt.full.data)[names(dt.full.data)=="CapitalLongitude"] <- "Origin_Capital_Long"
  
  dt.full.data
  
  # Return prepared data
  return(dt.full.data)

}
# Group by nach Land und Jahr, da manche Rows doppelt

# Header bauen




