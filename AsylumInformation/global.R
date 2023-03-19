library(shiny)
library(shinydashboard)
library(shinyalert)
library(ggplot2)
library(dplyr)
library(rsconnect)
library(shinyWidgets)
library(igraph)
library(data.table)
library(leaflet)
library(sp)
library(circlize)
library(networkD3)
library(visNetwork)
library(RColorBrewer)
library(ggimage)
library(png)
library(grid)

prepare_data <- function() {
  
  if (file.exists("data/asylum_data.RData")) {
    load("data/asylum_data.RData")
    
  } else {
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
    dt.asylum.data <- merge(dt.asylum.data, dt.country.info.merge, by.x="Country.of.origin..ISO.", by.y="Code", all.x = TRUE)
    # renaming columns
    names(dt.asylum.data)[names(dt.asylum.data)=="Income.group"] <- "Origin_Income"
    names(dt.asylum.data)[names(dt.asylum.data)=="CapitalName"] <- "Origin_Capital"
    names(dt.asylum.data)[names(dt.asylum.data)=="CapitalLatitude"] <- "Origin_Capital_Lat"
    names(dt.asylum.data)[names(dt.asylum.data)=="CapitalLongitude"] <- "Origin_Capital_Long"
    names(dt.asylum.data)[names(dt.asylum.data)=="region"] <- "Origin_Region"
    names(dt.asylum.data)[names(dt.asylum.data)=="sub.region"] <- "Origin_Sub_Region"
    names(dt.asylum.data)[names(dt.asylum.data)=="ContinentName"] <- "Origin_Continent"
    
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
  # Return prepared data
  return(dt.asylum.data)
}

aggregate_data <- function(dt.asylum) {
  #This function sums up all information for each year
  dt.aggregated.asylum <- dt.asylum %>%
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

create.origin.graph <- function(dt.asylum, country, Year_input, income_level) {
  dt.asylum <- prepare_data()
  dt.asylum.filtered <- data.table(dt.asylum[dt.asylum$Country.of.origin == country & dt.asylum$Year == Year_input, ])
  dt.asylum.filtered <- dt.asylum.filtered[!(dt.asylum.filtered$Country.of.origin == "Unknown" | dt.asylum.filtered$Country.of.asylum == "Unknown"), ]
  
  if (nrow(dt.asylum.filtered) == 0) {
    shinyalert("No Data Available!", "There is no data matching your criteria.", type = "error")}
  
  lon.lat <- function() {
    dt.location.vertices <- data.table(dt.asylum.filtered) 
    dt.location.origin <- dt.location.vertices[, c("Country.of.origin", "Origin_Capital_Lat", "Origin_Capital_Long", "Year", "Origin_Income")]
    dt.location.origin <- rename(dt.location.origin,c("name" = "Country.of.origin", "lat" = "Origin_Capital_Lat", "lon" = "Origin_Capital_Long", "income" = "Origin_Income"))
    dt.location.origin <- dt.location.origin[, list(unique(dt.location.origin), type = TRUE)]
    
    dt.location.asylum <- dt.location.vertices[, c("Country.of.asylum", "Asylum_Capital_Lat", "Asylum_Capital_Long", "Year", "Asylum_Income")]
    dt.location.asylum <- rename(dt.location.asylum,c("name" = "Country.of.asylum", "lat" = "Asylum_Capital_Lat", "lon" = "Asylum_Capital_Long", "income" = "Asylum_Income"))
    dt.location.asylum <- dt.location.asylum[, list(unique(dt.location.asylum), type = FALSE)]
    
    dt.all.locations <- rbind(dt.location.origin, dt.location.asylum)
    dt.all.locations <- dt.all.locations[, list(unique(dt.all.locations))]
    
    
    return(dt.all.locations)
  }
  
  dt.location.vertices <- lon.lat()
  edges <- dt.asylum.filtered[, c("Country.of.origin", "Country.of.asylum")]
  edges <- rename(edges, c("from" = "Country.of.origin", "to" = "Country.of.asylum"))
  
  # Match vertex names to indices in the vertex data frame
  from.idx <- match(edges$from, dt.location.vertices$name)
  to.idx <- match(edges$to, dt.location.vertices$name)
  
  # If to check if the filtering is for all income levels or specific one
  if (income_level == "All levels"){
    # Create the graph
    g <- graph.data.frame(edges, directed = TRUE, vertices = dt.location.vertices)
    g <- set_edge_attr(g, "weight", value= dt.asylum.filtered$Total.decisions + 0.001)
    weights <- E(g)$weight
    plot(g)
    
    # There is a chosen income level
  } else {
    # Create a new vertex attribute indicating whether the vertex should be included in the income filter
    filtered.vertices <- subset(dt.location.vertices, income %in% c(income_level) | type)
    
    if (nrow(filtered_vertices) == 0) {
      shinyalert("No Data Available!", "There is no data matching your criteria.", type = "error")}
    
    # Create a vector with names
    filtered.vertices.vec <- filtered.vertices$name
    
    # Include in the filtered.edges just the filtered vertices
    filtered.edges <- subset(edges, from %in% filtered.vertices.vec & to %in% filtered.vertices.vec)
    # Some asylum countries are doubled so take unique values only
    filtered.edges.idx <- match(paste(filtered.edges$from, filtered.edges$to), paste(edges$from, edges$to))
    filtered.decisions <- dt.asylum.filtered$Total.decisions[filtered.edges.idx]
    
    unique.filtered.edges <- unique(filtered.edges)
    edges <- unique.filtered.edges
    
    g <- graph.data.frame(filtered.edges, directed = TRUE, vertices = filtered.vertices)
    g <- set_edge_attr(g, "weight", value = filtered.decisions + 0.001)
    weights <- E(g)$weight
    
    # Plot the filtered graph
    plot(g)
  }
  
  gg <- get.data.frame(g, "both")
  gg <- lapply(gg, function(df) df[complete.cases(df), ])
  
  vert <- gg$vertices
  vert <- vert[complete.cases(vert), ]
  coordinates(vert) <- ~lon+lat
  
  edges <- gg$edges
  
  # Loop through the columns of the edges data frame
  
  edges.sp <- apply(edges, 1, function(row) {
    from_vert <- vert[vert$name == row["from"], ]
    to_vert <- vert[vert$name == row["to"], ]
    
    # Check if either vertex is missing, and skip this edge if so
    if (nrow(from_vert) == 0 || nrow(to_vert) == 0) {
      return(NULL)
    }
    
    as(rbind(from_vert, to_vert), "SpatialLines")
  })
  
  # Remove NULL values from edges list
  edges.sp <- edges.sp[!sapply(edges.sp, is.null)]
  
  # Assign IDs to edges
  edges.sp <- lapply(1:length(edges.sp), function(i) {
    spChFIDs(edges.sp[[i]], as.character(i))
  })
  
  edges.sp <- do.call(rbind, edges.sp)
  
  return(list(graph = g, vert = vert, edges = edges, edges_lines = edges.sp))
}

create.asylum.graph <- function(dt.asylum, country, Year_input, income_level) {
  dt.asylum <- prepare_data()
  
  dt.asylum.filtered <- data.table(dt.asylum[dt.asylum$Country.of.asylum == country & dt.asylum$Year == Year_input, ])
  dt.asylum.filtered <- dt.asylum.filtered[!(dt.asylum.filtered$Country.of.origin == "Unknown " | dt.asylum.filtered$Country.of.asylum == "Unknown " | dt.asylum.filtered$Country.of.origin == "Stateless"  | dt.asylum.filtered$Country.of.asylum == "Stateless"), ]
  
  if (nrow(dt.asylum.filtered) == 0) {
    shinyalert("No Data Available!", "There is no data matching your criteria.", type = "error")}
  
  lon.lat <- function() {
    dt.location.vertices <- data.table(dt.asylum.filtered) 
    dt.location.origin <- dt.location.vertices[, c("Country.of.origin", "Origin_Capital_Lat", "Origin_Capital_Long", "Year", "Origin_Income")]
    dt.location.origin <- rename(dt.location.origin,c("name" = "Country.of.origin", "lat" = "Origin_Capital_Lat", "lon" = "Origin_Capital_Long", "income" = "Origin_Income"))
    dt.location.origin <- dt.location.origin[, list(unique(dt.location.origin), type = TRUE)]
    
    dt.location.asylum <- dt.location.vertices[, c("Country.of.asylum", "Asylum_Capital_Lat", "Asylum_Capital_Long", "Year", "Asylum_Income")]
    dt.location.asylum <- rename(dt.location.asylum,c("name" = "Country.of.asylum", "lat" = "Asylum_Capital_Lat", "lon" = "Asylum_Capital_Long", "income" = "Asylum_Income"))
    dt.location.asylum <- dt.location.asylum[, list(unique(dt.location.asylum), type = FALSE)]
    
    dt.all.locations <- rbind(dt.location.origin, dt.location.asylum)
    dt.all.locations <- dt.all.locations[, list(unique(dt.all.locations))]
    
    return(dt.all.locations)
  }
  
  dt.location.vertices <- lon.lat()
  edges <- dt.asylum.filtered[, c("Country.of.origin", "Country.of.asylum")]
  edges <- rename(edges, c("from" = "Country.of.origin", "to" = "Country.of.asylum"))
  
  
  # If to check if the filtering is for all income levels or specific one
  if (income_level == "All levels"){
    # Create the graph
    g <- graph.data.frame(edges, directed = TRUE, vertices = dt.location.vertices)
    g <- set_edge_attr(g, "weight", value= dt.asylum.filtered$Total.decisions + 0.001)
    weights <- E(g)$weight
    plot(g)
    
    # There is a chosen income level
  } else {
    # Create a new vertex attribute indicating whether the vertex should be included in the income filter
    filtered.vertices <- subset(dt.location.vertices, income %in% c(income_level) | type == FALSE)
    
    # Create a vector with names
    filtered.vertices.vec <- filtered.vertices$name
    
    # Include in the filtered.edges just the filtered vertices
    filtered.edges <- subset(edges, from %in% filtered.vertices.vec & to %in% filtered.vertices.vec)
    # Some asylum countries are doubled so take unique values only
    filtered.edges.idx <- match(paste(filtered.edges$from, filtered.edges$to), paste(edges$from, edges$to))
    filtered.decisions <- dt.asylum.filtered$Total.decisions[filtered.edges.idx]
    
    unique.filtered.edges <- unique(filtered.edges)
    edges <- unique.filtered.edges
    
    g <- graph.data.frame(filtered.edges, directed = TRUE, vertices = filtered.vertices)
    g <- set_edge_attr(g, "weight", value = filtered.decisions + 0.001)
    
    weights <- E(g)$weight
    
    # Plot the filtered graph
    plot(g)
  }
  
  
  gg <- get.data.frame(g, "both")
  gg <- lapply(gg, function(df) df[complete.cases(df), ])
  
  vert <- gg$vertices
  vert <- vert[complete.cases(vert), ]
  coordinates(vert) <- ~lon+lat
  
  edges <- gg$edges
  
  # Loop through the columns of the edges data frame
  edges.sp <- apply(edges, 1, function(row) {
    from_vert <- vert[vert$name == row["from"], ]
    to_vert <- vert[vert$name == row["to"], ]
    
    # Check if either vertex is missing, and skip this edge if so
    if (nrow(from_vert) == 0 || nrow(to_vert) == 0) {
      return(NULL)
    }
    
    as(rbind(from_vert, to_vert), "SpatialLines")
  })
  
  # Remove NULL values from edges list
  edges.sp <- edges.sp[!sapply(edges.sp, is.null)]
  
  # Assign IDs to edges
  edges.sp <- lapply(1:length(edges.sp), function(i) {
    spChFIDs(edges.sp[[i]], as.character(i))
  })
  
  edges.sp <- do.call(rbind, edges.sp)
  
  return(list(graph = g, vert = vert, edges = edges, edges_lines = edges.sp))
}


circular.graph <- function(year) {
  dt.asylum <- prepare_data()
  # Filter out refugees that are stateless or where the origin is unknown
  dt.asylum.filtered <- dt.asylum[!(dt.asylum$Country.of.origin == "Unknown " | dt.asylum$Country.of.asylum == "Unknown " | dt.asylum$Country.of.origin == "Stateless"  | dt.asylum$Country.of.asylum == "Stateless"), ]
  
  df.origin <- data.frame(dt.asylum.filtered %>%
                            subset(Year == year) %>%
                            group_by(Country.of.origin, Year) %>%
                            summarize(Total = sum(Total.decisions)))
  colnames(df.origin) <- c("Country.of.origin", "Year", "Origin_total")
  
  df.asylum.countries <- data.frame(dt.asylum.filtered %>%
                                      subset(Year == year) %>%
                                      group_by(Country.of.asylum, Year) %>%
                                      summarize(Total = sum(Total.decisions)))
  colnames(df.asylum.countries) <- c("Country.of.asylum", "Year", "Country_total")
  
  df.origin.country <- data.frame(dt.asylum.filtered %>%
                                    subset(Year == year) %>%
                                    group_by(Country.of.origin, Country.of.asylum, Year) %>%
                                    summarize(Total = sum(Total.decisions)))
  
  # merge three dataframes into one
  df.total <- merge(df.origin, df.origin.country, by=c("Country.of.origin", "Year"))
  df.total <- merge(df.total, df.asylum.countries, by=c("Country.of.asylum", "Year"))
  colnames(df.total) <- c("Country", "Year", "Origin", "Origin_total", "Total", "Country_total")
  
  # Extract unique country names from df.total dataset
  countries <- unique(df.total$Country)
  
  # Extract unique origin country names from df.total dataset
  origin <- unique(df.total$Origin)
  
  # Combine the two sets of country names
  df.countries.origin <- data.frame(Country = unique(c(countries, origin)))
  
  # Add an ID column to the df.countries.origin dataset
  df.countries.origin <- arrange(df.countries.origin, Country)
  df.countries.origin$id <- seq.int(nrow(df.countries.origin))
  colnames(df.countries.origin) <- c("Country", "id")
  
  df.total <- merge(df.total, df.countries.origin, by.x = "Country", by.y = "Country")
  df.total <- merge(df.total, df.countries.origin, by.x = "Origin", by.y = "Country")
  
  colnames(df.total)[c(7,8)] <- c("To", "From")
  df.total <- df.total[, c(1,8,2,7,3:6)]
  
  #Create a node and edge data frame:
  df.node <- (data.frame(Country = df.countries.origin$Country, 
                         Origin_total = df.total[match(df.countries.origin$Country,
                                                       df.total$Origin), 6]))
  df.node <- data.frame(Country = df.node$Country,
                        Origin_total = df.node$Origin_total,
                        Country_total = df.total[match(df.countries.origin$Country,
                                                       df.total$Country), 8])
  df.node$id <- seq.int(nrow(df.node))
  str(df.node)
  df.node[is.na(df.node)] <- 0
  
  df.node$group <-  ifelse(df.node$Origin_total == 0 & df.node$Country_total >0, "Asylum Country", 
                           ifelse(df.node$Origin_total > 0 & df.node$Country_total == 0, "Refugee Country",
                                  ifelse(df.node$Origin_total > 0 & df.node$Country_total > 0 & df.node$Origin_total/df.node$Country_total > 100, "Mainly Refugee Country",
                                         ifelse(df.node$Origin_total > 0 & df.node$Country_total > 0 & df.node$Origin_total/df.node$Country_total > 10, "Dual Flow Country",
                                                ifelse(df.node$Origin_total > 0 & df.node$Country_total > 0 & df.node$Country_total/df.node$Origin_total > 100, "Mainly Asylum Country", "Dual Flow Country")))))
  
  df.node$value <-  ifelse(df.node$Origin_total > df.node$Country_total, df.node$Origin_total, df.node$Country_total)
  df.node$color <-  ifelse(df.node$Origin_total == 0 & df.node$Country_total >0, "#80CBC4",
                           ifelse(df.node$Origin_total > 0 & df.node$Country_total == 0, "#EF9A9A",
                                  ifelse(df.node$Origin_total > 0 & df.node$Country_total > 0 & df.node$Origin_total/df.node$Country_total > 100, "#FFE082",
                                         ifelse(df.node$Origin_total > 0 & df.node$Country_total > 0 & df.node$Origin_total/df.node$Country_total > 10, "#FFF59D",
                                                ifelse(df.node$Origin_total > 0 & df.node$Country_total > 0 & df.node$Country_total/df.node$Origin_total > 100, "#B2DFDB", "#FFF59D")))))
  df.node$title <- paste0("<p>",df.node$Country," ", year, ":","<br>",
                          "Refugees to ",df.node$Country,": ",
                          df.node$Country_total,"<br>",
                          "Asylum seekers coming from ",df.node$Country,
                          ":",df.node$Origin_total,"</p>", sep="")
  df.node$shadow <- FALSE
  df.node$shape <- ifelse(df.node$Origin_total == 0 & df.node$Country_total > 0, "dot",
                          ifelse(df.node$Origin_total > 0 & df.node$Country_total == 0, "triangle",
                                 ifelse(df.node$Origin_total > 0 & df.node$Country_total > 0 & df.node$Origin_total/df.node$Country_total > 100, "triangle",
                                        ifelse(df.node$Origin_total > 0 & df.node$Country_total > 0 & df.node$Origin_total/df.node$Country_total > 10, "square",
                                               ifelse(df.node$Origin_total > 0 & df.node$Country_total > 0 & df.node$Country_total/df.node$Origin_total > 100, "dot", "square")))))
  
  # Choose relevant columns for nodes
  df.node.relevant <- df.node[, c(4, 1, 5, 6, 10, 8, 7, 9)]
  colnames(df.node.relevant)[2] <- "label"
  
  # Prepare dataframe for edges
  df.edge <- df.total
  df.edge$arrows <- "to"
  df.edge$smooth <- TRUE
  df.edge$shadow <- FALSE
  df.edge$dashes <- FALSE
  df.edge$title <- paste0(df.edge$Origin, " to ", df.edge$Country, ": ", df.edge$Total, sep = "")
  df.edge$label <- c(as.character(df.edge$Total))
  
  
  df.edge.relevant <- df.edge[, c(2, 4, 9, 12, 13, 10, 11)]
  colnames(df.edge.relevant)[c(1,2)] <- c("from", "to")
  
  # create an igraph network
  g.circ <- graph_from_data_frame(df.edge.relevant, directed = TRUE, vertices = df.node.relevant)
  
  # create the same graph with visNetwork for better visualization
  visnetwork.refugees <- visNetwork(df.node.relevant, df.edge.relevant, width = "100%", height = "600px") %>%
    visOptions(nodesIdSelection = TRUE, selectedBy = "group", highlightNearest = list(enabled = TRUE, degree = 1)) %>% 
    visEdges(physics = FALSE, arrows =list(to = list(enabled = TRUE, scaleFactor = 0.5))) %>% 
    visIgraphLayout(type = "full", layout = "layout_in_circle") %>% 
    visInteraction(hover = TRUE, navigationButtons = TRUE) #%>% 
  visnetwork.refugees
  
  V(g.circ)$label
  
  
  # Execute the closeness function within the new environment
  bet <- betweenness(g.circ)
  eigen <- evcent(g.circ)$vector
  close <- closeness(g.circ)
  
  # Create table with centrality statistics
  df.statistics <- data.frame(index = names(bet), country = V(g.circ)$label, betweenness = bet, eigenvector = eigen, closeness = close)
  df.statistics <- df.statistics[!is.nan(df.statistics$closeness),]
  
  
  df.statistics.bet.ordered <- df.statistics[order(-df.statistics$betweenness), c("country", "betweenness")]
  df.statistics.bet.ordered <- df.statistics.bet.ordered[1:10, ]
  
  df.statistics.eigen.ordered <- df.statistics[order(-df.statistics$eigenvector), c("country", "eigenvector")]
  df.statistics.eigen.ordered <- df.statistics.eigen.ordered[1:10, ]
  
  df.statistics.close.ordered <- df.statistics[order(-df.statistics$closeness), c("country", "closeness")]
  df.statistics.close.ordered <- df.statistics.close.ordered[1:10, ]
  
  # create the fourth return which is the three centrality measures
  df.statistics.merged <- data.frame(
    Country_Betweenness = df.statistics.bet.ordered$country,
    Betweenness = format(round(df.statistics.bet.ordered$betweenness, 2)),
    Country_Eigenvector = df.statistics.eigen.ordered$country,
    Eigenvector = format(round(df.statistics.eigen.ordered$eigenvector, 2)),
    Country_Closeness  = df.statistics.close.ordered$country,
    Closeness = format(round(df.statistics.close.ordered$closeness, 4))
  )
  
  return(list(visnetwork.refugees, g.circ, df.statistics.merged, df.statistics))}

create_prediction_graph <- function(country, in_out) {
  dt.asylum <- prepare_data()
  
  lon_lat <- function() {
    dt.location.vertices <- data.table(dt.asylum) 
    dt.location.origin <- dt.location.vertices[, c(
      "Country.of.origin", "Origin_Capital_Lat", "Origin_Capital_Long")]
    dt.location.origin <- rename(dt.location.origin,c(
      "name" = "Country.of.origin", "lat" = "Origin_Capital_Lat",
      "lon" = "Origin_Capital_Long"))
    dt.location.origin <- dt.location.origin[, list(unique(dt.location.origin),
                                                    type = TRUE)]
    
    dt.location.asylum <- dt.location.vertices[, c(
      "Country.of.asylum", "Asylum_Capital_Lat", "Asylum_Capital_Long")]
    dt.location.asylum <- rename(dt.location.asylum,c(
      "name" = "Country.of.asylum", "lat" = "Asylum_Capital_Lat", 
      "lon" = "Asylum_Capital_Long"))
    dt.location.asylum <- dt.location.asylum[, list(unique(dt.location.asylum),
                                                    type = FALSE)]
    
    dt.all.locations <- rbind(dt.location.origin, dt.location.asylum)
    dt.all.locations <- dt.all.locations[!duplicated(name)]
    
    dt.all.locations$index <- seq_len(nrow(dt.all.locations))
    
    return(dt.all.locations)
  }
  
  dt.location.vertices <- lon_lat()
  edges <- dt.asylum[, c("Country.of.origin", "Country.of.asylum")]
  edges <- rename(edges, c("from" = "Country.of.origin", "to" = "Country.of.asylum"))
  
  # Match vertex names to indices in the vertex data frame
  from.idx <- match(edges$from, dt.location.vertices$name)
  to.idx <- match(edges$to, dt.location.vertices$name)
  
  # Create the graph
  g <- graph.data.frame(edges, directed = TRUE, vertices = dt.location.vertices)
  g <- set_edge_attr(g, "weight", value= dt.asylum$Total.decisions + 0.001)
  weights <- E(g)$weight
  
  if (in_out == "Origin"){
    # Calculate the similarity between all pairs of nodes
    m.similarity.matrix <- similarity.jaccard(g, mode = "out")
  } else {
    m.similarity.matrix <- similarity.jaccard(g, mode = "in")
  }
  
  # Set the diagonal to zero
  diag(m.similarity.matrix) <- 0
  
  # Predict the top n edges with highest similarity that don't already exist
  n <- 10
  dt.predicted.edges <- data.frame(as.matrix(which(m.similarity.matrix > 0,
                                                   arr.ind = TRUE)))
  colnames(dt.predicted.edges) <- c("from", "to")
  m.predicted.edges.weights <- m.similarity.matrix[as.matrix(dt.predicted.edges)]
  dt.predicted.edges <- cbind(dt.predicted.edges, m.predicted.edges.weights)
  dt.predicted.edges <- dt.predicted.edges[order(-m.predicted.edges.weights), ]
  dt.predicted.edges <- dt.predicted.edges[!(dt.predicted.edges$from %in% edges$from &
                                               dt.predicted.edges$to %in% edges$to), ]
  
  vertex.lookup <- setNames(dt.location.vertices$name, dt.location.vertices$index)
  dt.predicted.edges$from <- vertex.lookup[dt.predicted.edges$from]
  dt.predicted.edges$to <- vertex.lookup[dt.predicted.edges$to] 
  
  if (in_out == "Origin"){
    dt.predicted.edges <- dt.predicted.edges[(dt.predicted.edges$from == country), ]
  } else {
    dt.predicted.edges <- dt.predicted.edges[(dt.predicted.edges$to == country), ]
  }
  dt.predicted.edges.filter <- dt.predicted.edges[1:n, 1:2]
  
  # Create a new directed graph with the predicted edges
  
  g.predicted.edges <- graph_from_edgelist(as.matrix(dt.predicted.edges.filter),
                                           directed = TRUE)
  
  g.predicted.edges <- set_edge_attr(g.predicted.edges, "weight",
                                     value = dt.predicted.edges$m.predicted.edges.weights[1:n])
  weights <- E(g.predicted.edges)$weight
  
  gg.pred <- get.data.frame(g.predicted.edges, "both")
  gg.vert <- gg.pred$vertices$name
  gg.vert.pred <- dt.location.vertices[dt.location.vertices$name %in% gg.vert, ]
  gg.vert.pred <- gg.vert.pred[complete.cases(gg.vert.pred), ]
  
  coordinates(gg.vert.pred) <- ~lon+lat
  
  edges <- gg.pred$edges
  edges <- edges[(edges$to %in% gg.vert.pred$name) &
                   (edges$from %in% gg.vert.pred$name), ]
  
  # Loop through the columns of the edges data frame
  
  sp.edges <- apply(edges, 1, function(row) {
    from.vert <- gg.vert.pred[gg.vert.pred$name == row["from"], ]
    to.vert <- gg.vert.pred[gg.vert.pred$name == row["to"], ]
    
    # Check if either vertex is missing, and skip this edge if so
    if (nrow(from.vert) == 0 || nrow(to.vert) == 0) {
      return(NULL)
    }
    
    as(rbind(from.vert, to.vert), "SpatialLines")
  })
  
  # Remove NULL values from edges list
  sp.edges <- sp.edges[!sapply(sp.edges, is.null)]
  
  # Assign IDs to edges
  sp.edges <- lapply(1:length(sp.edges), function(i) {
    spChFIDs(sp.edges[[i]], as.character(i))
  })
  
  sp.edges <- do.call(rbind, sp.edges)
  return(list(g.old = g, graph = g.predicted.edges,
              vert = gg.vert.pred, edges = edges, edges.lines = sp.edges))
}

preparation_rejections <- function(){
  dt.asylum <- prepare_data()
  df.top.rejection <- dt.asylum %>%
    group_by(Country.of.asylum) %>%
    summarize(total.decisions = sum(Total.decisions),
              total.rejections = sum(Rejected.decisions),
              rejection.rate = sum(Rejected.decisions) / sum(Total.decisions),
              asylum_lat = first(Asylum_Capital_Lat),
              asylum_long = first(Asylum_Capital_Long))
  return(list(df.top.rejection, dt.asylum))
}

descriptives <- function() {
  rejections <- preparation_rejections()[[1]]
  dt.asylum <- preparation_rejections()[[2]]
  
  ##### top 10 country asylum
  # sort df and get top 5 country asylum
  df.top.asylum.5 <- rejections %>%
    arrange(desc(total.decisions)) %>%
    top_n(5, total.decisions)
  
  # create bar chart for top 5 country asylum
  color.palette <- brewer.pal(n = 5, name = "PuBuGn")
  image.files <- c(
    "data/Flags/de.png", 
    "data/Flags/us.png", 
    "data/Flags/fr.png", 
    "data/Flags/gb.png", 
    "data/Flags/za.png"
  )
  
  # Create a new column in the data frame with the image file paths
  df.top.asylum.5$image.file <- image.files
  
  # Define a function to read the images from file and convert them to grobs
  read.image <- function(file) {
    img <- readPNG(file)
    grob <- rasterGrob(img, interpolate=TRUE)
    return(grob)
  }
  
  # Read the images from file and convert them to grobs
  images <- lapply(image.files, read.image)
  
  # Convert the column to a vector before passing it to reorder()
  df.top.asylum.5$Country.of.asylum <- unlist(df.top.asylum.5$Country.of.asylum)
  
  # Create the plot with images
  plot.top.asylum <- ggplot(df.top.asylum.5, aes(
    x = reorder(Country.of.asylum, -total.decisions), 
    y = total.decisions, fill = Country.of.asylum)) +
    geom_bar(stat = "identity") +
    ggimage::geom_image(aes(x = Country.of.asylum, y = -1, image = image.files), size = 0.08) +
    geom_text(aes(label = paste0(round(total.decisions/1e6, 1), "M")), vjust = -0.5, size = 4) + # Add data labels to bars and convert to millions
    labs(x = "Countries of Asylum", y = "Total Decisions (in millions)") + # Remove x-axis label
    ggtitle("Top 5 Countries of Asylum by Total Decisions") +
    scale_fill_manual(values = color.palette) + # Use the defined color palette
    theme_minimal() +
    theme(plot.title = element_text(size = 16, face = "bold", hjust = 0), # Increase font size of title
          axis.title = element_text(size = 14, face = "bold"), # Increase font size of axis labels
          axis.text = element_text(size = 12), # Increase font size of tick labels
          panel.grid.major = element_blank(), # Remove major grid lines
          panel.grid.minor = element_blank(), # Remove minor grid lines
          axis.line.y = element_blank(), # Remove y-axis line
          axis.ticks.y = element_blank(), # Remove y-axis ticks
          axis.text.y = element_blank(), # Remove y-axis tick labels
          axis.text.x = element_blank(), # Remove x-axis tick labels
          plot.margin = unit(c(1, 1, 1, 3), "lines")) # Add space on the right for x-axis labels
  
  
  ##### top 5 countries of origin
  # create df with country asylum and their total decisions
  df.top.origin <- dt.asylum %>%
    # filter(Year == 2022) %>% # build region filter
    group_by(Country.of.origin) %>%
    summarize(total.decisions = sum(Total.decisions))
  
  # sort df and get top 5 country origin
  df.top.origin.5 <- df.top.origin %>%
    arrange(desc(total.decisions)) %>%
    top_n(5, total.decisions)
  
  color.palette <- brewer.pal(n = 5, name = "PuBuGn")
  image.files2 <- c(
    "data/Flags/un.png", 
    "data/Flags/af.png", 
    "data/Flags/sy.png", 
    "data/Flags/iq.png", 
    "data/Flags/rs.png"
  )
  
  # Create a new column in the data frame with the image file paths
  df.top.origin.5$image.file <- image.files2
  
  # Define a function to read the images from file and convert them to grobs
  read.image <- function(file) {
    img <- readPNG(file)
    grob <- rasterGrob(img, interpolate=TRUE)
    return(grob)
  }
  
  # Read the images from file and convert them to grobs
  images <- lapply(image.files2, read.image)
  
  # Convert the column to a vector before passing it to reorder()
  df.top.origin.5$Country.of.origin <- unlist(df.top.origin.5$Country.of.origin)
  
  # Create the plot with images
  plot.top.origin <- ggplot(df.top.origin.5, aes(x = reorder(Country.of.origin, -total.decisions), 
                                                 y = total.decisions, fill = Country.of.origin)) +
    geom_bar(stat = "identity") +
    ggimage::geom_image(aes(x = Country.of.origin, y = -1, image = image.files2), size = 0.08) +
    geom_text(aes(label = paste0(round(total.decisions/1e6, 1), "M")), vjust = -0.5, size = 4) + # Add data labels to bars and convert to millions
    labs(x = "Countries of Asylum", y = "Total Decisions (in millions)") + # Remove x-axis label
    ggtitle("Top 5 Countries of Origin by Total Decisions") +
    scale_fill_manual(values = color.palette) + # Use the defined color palette
    theme_minimal() +
    theme(plot.title = element_text(size = 16, face = "bold", hjust = 0), # Increase font size of title
          axis.title = element_text(size = 14, face = "bold"), # Increase font size of axis labels
          axis.text = element_text(size = 12), # Increase font size of tick labels
          panel.grid.major = element_blank(), # Remove major grid lines
          panel.grid.minor = element_blank(), # Remove minor grid lines
          axis.line.y = element_blank(), # Remove y-axis line
          axis.ticks.y = element_blank(), # Remove y-axis ticks
          axis.text.y = element_blank(), # Remove y-axis tick labels
          axis.text.x = element_blank(), # Remove x-axis tick labels
          plot.margin = unit(c(1, 1, 1, 3), "lines")) # Add space on the right for x-axis labels
  
  ##### top 5 countries highest rejection 
  # sort df and get top 5 countries with highest rejections
  df.top.rejection5 <- rejections %>%
    arrange(desc(total.rejections)) %>%
    top_n(5, total.rejections)
  
  # create bar chart for top 5 country asylum
  # Define a list of local file paths corresponding to the countries in the plot
  image.files3 <- c(
    "data/Flags/fr.png", 
    "data/Flags/de.png", 
    "data/Flags/gb.png", 
    "data/Flags/us.png", 
    "data/Flags/za.png"
  )
  
  # Create a new column in the data frame with the image file paths
  df.top.rejection5$image.file <- image.files3
  
  # Define a function to read the images from file and convert them to grobs
  read.image <- function(file) {
    img <- readPNG(file)
    grob <- rasterGrob(img, interpolate=TRUE)
    return(grob)
  }
  
  # Read the images from file and convert them to grobs
  images <- lapply(image.files3, read.image)
  
  plot.top.rejection <- ggplot(df.top.rejection5, aes(x = reorder(Country.of.asylum, -total.rejections), 
                                                      y = total.rejections, fill = Country.of.asylum)) +
    geom_bar(stat = "identity") +
    ggimage::geom_image(aes(x = Country.of.asylum, y = -1, image = image.files3), size = 0.08) +
    geom_text(aes(label = paste0(round(total.rejections/1e6, 1), "M")), vjust = -0.5, size = 4) + # Add data labels to bars and convert to millions
    labs(x = "Country with highest absolute rejections", y = "Total Rejections (in millions)") +
    ggtitle("Top 5 Countries with highest rejections") +
    scale_fill_manual(values = color.palette) + # Use the defined color palette
    theme_minimal() +
    theme(plot.title = element_text(size = 16, face = "bold", hjust = +0.5), # Increase font size of title
          axis.title = element_text(size = 14, face = "bold"), # Increase font size of axis labels
          axis.text = element_text(size = 12), # Increase font size of tick labels
          panel.grid.major = element_blank(), # Remove major grid lines
          panel.grid.minor = element_blank(), # Remove minor grid lines
          axis.line.y = element_blank(), # Remove y-axis line
          axis.ticks.y = element_blank(), # Remove y-axis ticks
          axis.text.y = element_blank(), # Remove y-axis tick labels
          axis.text.x = element_blank(), # Remove x-axis tick labels
          plot.margin = unit(c(1, 1, 1, 3), "lines")) # Add space on the right for x-axis labels
  
  
  ###### top 5 countries by rejection rate
  # sort df and get top 5 countries with highest rejections
  df.top.rejection.rate5 <- rejections %>%
    arrange(desc(rejection.rate)) %>%
    top_n(5, rejection.rate)
  
  image.files4 <- c(
    "data/Flags/aw.png", 
    "data/Flags/fm.png", 
    "data/Flags/bs.png", 
    "data/Flags/jp.png", 
    "data/Flags/ky.png"
  )
  
  # Create a new column in the data frame with the image file paths
  df.top.rejection.rate5$image.file <- image.files4
  
  # Read the images from file and convert them to grobs
  images <- lapply(image.files4, read.image)
  
  # Add the images to the plot using geom_image()
  plot.top.rejection.rate <- ggplot(df.top.rejection.rate5, aes(x = reorder(Country.of.asylum, -rejection.rate), 
                                                                y = rejection.rate, fill = Country.of.asylum)) +
    geom_bar(stat = "identity") +
    ggimage::geom_image(aes(x = Country.of.asylum, y = 0, image = image.files4), size = 0.08) +
    geom_text(aes(label = round(rejection.rate, 1)), vjust = -0.5, size = 4) + # Add data labels to bars as percentages
    labs(x = "Countries of Asylum", y = "Rejection Rate") + # Remove x-axis label
    ggtitle("Top 5 Countries of Asylum by Rejection Rate") +
    scale_fill_manual(values = color.palette) + # Use the defined color palette
    theme_minimal() +
    theme(plot.title = element_text(size = 16, face = "bold", hjust = +0.5), # Increase font size of title
          axis.title = element_text(size = 14, face = "bold"), # Increase font size of axis labels
          axis.text = element_text(size = 12), # Increase font size of tick labels
          panel.grid.major = element_blank(), # Remove major grid lines
          panel.grid.minor = element_blank(), # Remove minor grid lines
          axis.line.y = element_blank(), # Remove y-axis line
          axis.ticks.y = element_blank(), # Remove y-axis ticks
          axis.text.y = element_blank(), # Remove y-axis tick labels
          axis.text.x = element_blank(), # Remove x-axis tick labels
          plot.margin = unit(c(1, 1, 1, 3), "lines")) # Add space on the right for x-axis labels
  
  
  
  ##### pie chart with total decisions by income level
  income.levels <- dt.asylum %>%
    group_by(Asylum_Income) %>%
    summarize(total_decisions = sum(Total.decisions))
  
  # Create a pie chart
  income.levels.filtered <- income.levels[income.levels$Asylum_Income %in% 
                                            c("High income", 
                                              "Low income", 
                                              "Lower middle income", 
                                              "Upper middle income"), ]
  
  plot.income.level <- ggplot(income.levels.filtered, aes(x="", y=total_decisions, 
                                                          fill=Asylum_Income)) +
    geom_bar(stat="identity", width=1) +
    coord_polar("y", start=0) +
    labs(fill="Income Level", x=NULL, y=NULL, title="Total Decisions by Income Level") +
    theme_void() +
    geom_text(aes(label=paste0(round(total_decisions/sum(total_decisions)*100),"%")), 
              position=position_stack(vjust=0.5), size=4) +
    scale_fill_manual(values = color.palette) + # Use the defined color palette
    theme(plot.title = element_text(size = 16, face = "bold", hjust = +0.5), # Increase font size of title
          axis.title = element_text(size = 14, face = "bold"), # Increase font size of axis labels
          axis.text = element_blank(), # Remove axis tick labels
          plot.margin = unit(c(1, 1, 1, 1), "lines")) # Add space around the plot
  
  
  # Call reactive element
  df.rejections.map <- rejections
  pal <- colorNumeric(palette = "Blues", domain = df.rejections.map$rejection.rate)
  
  # Create map with rejection rate
  leaflet.rejection <- leaflet(data = df.rejections.map) %>%
    addTiles() %>%
    addCircleMarkers(lng = ~asylum_long, lat = ~asylum_lat,
                     color = ~pal(rejection.rate), fillOpacity = 10,
                     radius = 10,
                     popup = ~paste("Country: ", Country.of.asylum, "<br>",
                                    "Rejection Rate: ", round((rejection.rate * 100), 1), "%", "<br>",
                                    "Total Decisions: ", total.decisions, "<br>",
                                    "Total Rejections: ", total.rejections)) %>%
    addLegend(pal = pal, values = df.rejections.map$rejection.rate,
              title = "Rejection Rate", position = "bottomright")
  
  
  return(list(plot.top.asylum, plot.top.origin, plot.top.rejection, plot.top.rejection.rate, plot.income.level, leaflet.rejection))
}