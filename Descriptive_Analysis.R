library(dplyr)



# Add a new column with the sum of total decisions by country of origin

#Country of asylum sums
dt.full.data2 <- dt.asylum.data %>%
  group_by(Country.of.asylum) %>%
  mutate(asylum.total.decisions.by.country = sum(Total.decisions),
         asylum.total.complementary.protection = sum(Complementary.protection),
         asylum.total.rejected.decisions = sum(Rejected.decisions), 
         asylum.total.otherwise.closed = sum(Otherwise.closed))

#Country of origin sums
dt.full.data3 <- dt.full.data2 %>%
  group_by(Country.of.origin) %>%
  mutate(origin.total.decisions.by.country = sum(Total.decisions),
         origin.total.complementary.protection = sum(Complementary.protection),
         origin.total.rejected.decisions = sum(Rejected.decisions), 
         origin.total.otherwise.closed = sum(Otherwise.closed))

#Total number of Country of asylum income
dt.full.data4 <- dt.full.data3 %>%
  group_by(Asylum_Income) %>%
  mutate(origin.total.decisions.by.asylum.income = sum(Total.decisions))


#Create Unique Data Frames by asylum, origin and income asylum country
dt.unique.asylum.country<- dt.full.data4[!duplicated(dt.full.data4$Country.of.asylum), ]
dt.unique.origin.country<- dt.full.data4[!duplicated(dt.full.data4$Country.of.origin), ]
dt.unique.income.asylum <- dt.full.data4[!duplicated(dt.full.data4$Asylum_Income), ]

#Piechart
# Calculate percentages
percentages.pie.asylum.income <- round(dt.unique.income.asylum$origin.total.decisions.by.asylum.income / sum(dt.unique.income.asylum$origin.total.decisions.by.asylum.income) * 100, 1)

# Create a pie chart with labels and percentages
pie(dt.unique.income.asylum$origin.total.decisions.by.asylum.income,
    labels = sprintf("%s (%.1f%%)", dt.unique.income.asylum$Asylum_Income, percentages.pie.asylum.income),
    main = "Hosting Countries by Asylum_Income")


#Top5 Hosting
sorted_totals.hosting <- dt.unique.asylum.country[order(-dt.unique.asylum.country$asylum.total.decisions.by.country), ]
top5<- head(sorted_totals.hosting, 5)

barplot(top5$asylum.total.decisions.by.country,
        names.arg = top5$Country.of.asylum..ISO.,
        main = "Top 5 Hosting Countries",
        horiz = TRUE,
        xlim = c(0, 5000000))


#Top5 Origin
sorted_totals.origin <- dt.unique.origin.country[order(-dt.unique.origin.country$origin.total.decisions.by.country), ]
sorted_totals.origin <- sorted_totals.origin[-1, ] #delete first row, since first country is unknown NA

top5.origin<- head(sorted_totals.origin, 5)
barplot(top5.origin$origin.total.decisions.by.country, names.arg = top5.origin$Country.of.origin..ISO., 
        main = "Top 5 Origin Countries", 
        horiz = T,
        xlim = c(0, 2000000))

Total.Sum<- sum(dt.asylum.data$Total.decisions)

#Aufteilung nach top 5 Länder
top5.origin.sub <- top5.origin[, c(1, 26)]
newrow <- c("Others", (Total.Sum - sum(top5.origin.sub$origin.total.decisions.by.country)))
top5.origin.sub <- rbind(top5.origin.sub, newrow)
top5.origin.sub$origin.total.decisions.by.country <- as.numeric(top5.origin.sub$origin.total.decisions.by.country)

str(top5.origin.sub)
sum(top5.origin.sub$origin.total.decisions.by.country) == Total.Sum #Check

percent <- round(100 * top5.origin.sub$origin.total.decisions.by.country / sum(top5.origin.sub$origin.total.decisions.by.country), 1)
labels <- paste(top5.origin.sub$Country.of.origin, "(", percent, "%)", sep = "")
pie(top5.origin.sub$origin.total.decisions.by.country,
    labels = labels,
    main = "Countries of Origin")



#Top 5 Rejecting Rate of Asylum Countries
#Rejecting Rate
dt.unique.asylum.country <- dt.unique.asylum.country %>%
  mutate(rejecting.rate.asylum.country = asylum.total.rejected.decisions/asylum.total.decisions.by.country)

sorted_rej.asylum <- dt.unique.asylum.country[order(-dt.unique.asylum.country$rejecting.rate.asylum.country), ]

top10.rej<- head(sorted_rej.asylum, 5)
barplot(top10.rej$rejecting.rate.asylum.country, names.arg = top10.rej$Country.of.asylum..ISO., main = "Top 10 Hosting Countries by Rejection Rate", horiz = F)


top10.rej <- top10.rej[, c(2, 31)]

library(leaflet)
library(ggmap)


your_data_frame <- sorted_rej.asylum
pal <- colorNumeric(palette = "Blues", domain = your_data_frame$rejecting.rate.asylum.country)

# Create map
map <- leaflet(data = your_data_frame) %>%
  addTiles() %>%
  addCircleMarkers(lng = ~Asylum_Capital_Long, lat = ~Asylum_Capital_Lat,
                   color = ~pal(rejecting.rate.asylum.country), fillOpacity = 10,
                   radius = 10,
                   popup = ~paste(Country.of.asylum..ISO., "<br>",
                                  "Rejection Rate: ", rejecting.rate.asylum.country)) %>%
  addLegend(pal = pal, values = your_data_frame$rejecting.rate.asylum.country,
            title = "Rejection Rate", position = "bottomright")

# Display map
map

#Summary Statistics

n_hosting <- nrow(dt.unique.asylum.country)
n_hosting

n_origin <- nrow(dt.unique.origin.country)
n_origin

#Total sums
col_sum <- colSums(dt.asylum.data[, 9:13], na.rm = TRUE)
dt.total <- data.frame(Total_Asylum_Seekers = col_sum)
dt.total

#Max Asylum Country

max_hosting <- dt.unique.asylum.country[which.max(dt.unique.asylum.country$asylum.total.decisions.by.country), "Country.of.asylum" & "asylum.total.decisions.by.country"]
max_hosting


max_origin <- data.frame(Country = dt.unique.origin.country[which.max(dt.unique.origin.country$origin.total.decisions.by.country), "Country.of.origin"],
                          Total_Asylum_Seekers = max(dt.unique.origin.country$origin.total.decisions.by.country))
max_origin

min_hosting <- data.frame(Country = dt.unique.asylum.country[which.min(dt.unique.asylum.country$asylum.total.decisions.by.country), "Country.of.asylum"],
                          Total_Asylum_Seekers = min(dt.unique.asylum.country$asylum.total.decisions.by.country))
min_hosting

min_origin <- data.frame(Country = dt.unique.origin.country[which.min(dt.unique.origin.country$origin.total.decisions.by.country), "Country.of.origin"],
                         Total_Asylum_Seekers = min(dt.unique.origin.country$origin.total.decisions.by.country))
min_origin


install.packages("knitr")
install.packages("kableExtra")

library(knitr)
library(kableExtra)




