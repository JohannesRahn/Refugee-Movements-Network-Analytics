asylum_data = read.csv("data\\asylum-decisions.csv", header=TRUE, sep=";")

country_income = read.csv("data\\income_data.csv", header=TRUE)

#country_demographics = read.csv("data\\demographics.csv", header=TRUE, sep=";")

country_capitals = read.csv("data\\concap.csv", header=TRUE)

country_info_merge = merge(country_income, country_capitals, by.x= "Country", 
                           by.y = "CountryName")

full_data = asylum_data

full_data = merge(full_data, country_info_merge, by.x="Country.of.origin",
                                   by.y="Country")
#TODO: Rename to origin
colnames(full_data,)

full_data = merge(full_data, country_income, by.x="Country.of.destination",
                  by.y="Country")
full_data
# TODO: Rename to destination



# normalize names to comply with NDA style guide
setnames(dt.iris, names(dt.iris), 
         tolower(gsub(".", "_", names(dt.iris), fixed=TRUE))) 