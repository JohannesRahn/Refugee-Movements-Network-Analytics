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
pie(dt.unique.income.asylum$origin.total.decisions.by.asylum.income, labels =dt.unique.income.asylum$Asylum_Income , main = "Total Decisions by Asylum_Income")

#Top5 Hosting
sorted_totals.hosting <- dt.unique.asylum.country[order(-dt.unique.asylum.country$asylum.total.decisions.by.country), ]
top5<- head(sorted_totals.hosting, 5)
barplot(top5$asylum.total.decisions.by.country, names.arg = top5$Country.of.asylum..ISO., main = "Total Decisions by Asylum Country", horiz = T)


#Top5 Origin
sorted_totals.origin <- dt.unique.origin.country[order(-dt.unique.origin.country$origin.total.decisions.by.country), ]
top5.origin<- head(sorted_totals.origin, 5)
barplot(top5.origin$origin.total.decisions.by.country, names.arg = top5.origin$Country.of.origin..ISO., main = "Total Decisions by Asylum Country", horiz = T)

Total.Sum<- sum(dt.asylum.data$Total.decisions)
Total.Sum

#Aufteilung nach top 5 Länder
top5.origin.sub <- top5.origin[, c(1, 26)]
newrow <- c("others", (Total.Sum - sum(top5.origin.sub$origin.total.decisions.by.country)))
top5.origin.sub <- rbind(top5.origin.sub, newrow)
top5.origin.sub$origin.total.decisions.by.country <- as.numeric(top5.origin.sub$origin.total.decisions.by.country)

str(top5.origin.sub)
sum(top5.origin.sub$origin.total.decisions.by.country) == Total.Sum #Check

top5.origin.sub

pie(top5.origin.sub$origin.total.decisions.by.country, labels = top5.origin.sub$Country.of.origin , main = "Total Decisions by Asylum_Income")


#Top 5 Rejecting Rate of Asylum Countries
#Rejecting Rate
dt.unique.asylum.country <- dt.unique.asylum.country %>%
  mutate(rejecting.rate.asylum.country = asylum.total.rejected.decisions/asylum.total.decisions.by.country)

sorted_rej.asylum <- dt.unique.asylum.country[order(-dt.unique.asylum.country$rejecting.rate.asylum.country), ]

top10.rej<- head(sorted_rej.asylum, 10)
top10.rej
barplot(top10.rej$rejecting.rate.asylum.country, names.arg = top10.rej$Country.of.asylum..ISO., main = "Total Decisions by Asylum Country", horiz = F)


top10.rej <- top10.rej[, c(2, 31)]
str(top10.rej)

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
                                  "Rejecting Rate: ", rejecting.rate.asylum.country)) %>%
  addLegend(pal = pal, values = your_data_frame$rejecting.rate.asylum.country,
            title = "Rejecting Rate", position = "bottomright")

# Display map
map

