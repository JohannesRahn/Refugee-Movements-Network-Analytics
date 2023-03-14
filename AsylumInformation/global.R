library(shiny)
library(shinydashboard)
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

options(digits = 4)


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


create_asylum_graph <- function(dt.asylum, country, Year_input, income_level) {
  dt.asylum <- prepare_data()
  dt.asylum.filtered <- data.table(dt.asylum[dt.asylum$Country.of.origin == country & dt.asylum$Year == Year_input, ])
  
  
  lon_lat <- function() {
    location.vertices <- data.table(dt.asylum.filtered) 
    location.origin <- location.vertices[, c("Country.of.origin", "Origin_Capital_Lat", "Origin_Capital_Long", "Year", "Origin_Income")]
    location.origin <- rename(location.origin,c("name" = "Country.of.origin", "lat" = "Origin_Capital_Lat", "lon" = "Origin_Capital_Long", "income" = "Origin_Income"))
    location.origin <- location.origin[, list(unique(location.origin), type = TRUE)]
    
    location.asylum <- location.vertices[, c("Country.of.asylum", "Asylum_Capital_Lat", "Asylum_Capital_Long", "Year", "Asylum_Income")]
    location.asylum <- rename(location.asylum,c("name" = "Country.of.asylum", "lat" = "Asylum_Capital_Lat", "lon" = "Asylum_Capital_Long", "income" = "Asylum_Income"))
    location.asylum <- location.asylum[, list(unique(location.asylum), type = FALSE)]
    
    all.locations <- rbind(location.origin, location.asylum)
    all.locations <- all.locations[, list(unique(all.locations))]
    
    return(all.locations)
  }
  
  location.vertices <- lon_lat()
  edges <- dt.asylum.filtered[, c("Country.of.origin", "Country.of.asylum")]
  edges <- rename(edges, c("from" = "Country.of.origin", "to" = "Country.of.asylum"))
  
  # Match vertex names to indices in the vertex data frame
  from_idx <- match(edges$from, location.vertices$name)
  to_idx <- match(edges$to, location.vertices$name)
  
  # If to check if the filtering is for all income levels or specific one
  if (income_level == "All levels"){
    # Create the graph
    g <- graph.data.frame(edges, directed = TRUE, vertices = location.vertices)
    g <- set_edge_attr(g, "weight", value= dt.asylum.filtered$Total.decisions + 0.001)
    weights <- E(g)$weight
    plot(g)
    
  # There is a chosen income level
  } else {
    # Create a new vertex attribute indicating whether the vertex should be included in the income filter
    filtered_vertices <- subset(location.vertices, income %in% c(income_level) | type)
    
    # Create a vector with names
    filtered_vertices_vec <- filtered_vertices$name
    
    # Include in the filtered_edges just the filtered vertices
    filtered_edges <- subset(edges, from %in% filtered_vertices_vec & to %in% filtered_vertices_vec)
    # Some asylum countries are doubled so take unique values only
    unique_filtered_edges <- unique(filtered_edges)
    edges <- unique_filtered_edges
    
    # Create for income filtered graph
    g <- graph.data.frame(edges, directed = TRUE, vertices = filtered_vertices)
    
    
    # Plot the filtered graph
    plot(g)
  }
  
  gg <- get.data.frame(g, "both")
  gg <- lapply(gg, function(df) df[complete.cases(df), ])
  
  vert <- gg$vertices
  coordinates(vert) <- ~lon+lat
  
  edges <- gg$edges

  # Loop through the columns of the edges data frame
  edges_sp <- apply(edges, 1, function(row) {
    from_vert <- vert[vert$name == row["from"], ]
    to_vert <- vert[vert$name == row["to"], ]
    
    # Check if either vertex is missing, and skip this edge if so
    if (nrow(from_vert) == 0 || nrow(to_vert) == 0) {
      return(NULL)
    }
    
    as(rbind(from_vert, to_vert), "SpatialLines")
  })
  
  # Remove NULL values from edges list
  edges_sp <- edges_sp[!sapply(edges_sp, is.null)]
  
  # Assign IDs to edges
  edges_sp <- lapply(1:length(edges_sp), function(i) {
    spChFIDs(edges_sp[[i]], as.character(i))
  })
  
  edges_sp <- do.call(rbind, edges_sp)

  return(list(graph = g, vert = vert, edges = edges, edges_lines = edges_sp))
}

circular_graph <- function(year) {
  dt.asylum <- prepare_data()

  origin_2015 <- data.frame(dt.asylum %>%
                              subset(Year == year) %>%
                              group_by(Country.of.origin, Year) %>%
                              summarize(Total = sum(Total.decisions)))
  colnames(origin_2015) <- c("Country.of.origin", "Year", "Origin_total")
  
  country_2015 <- data.frame(dt.asylum %>%
                               subset(Year == year) %>%
                               group_by(Country.of.asylum, Year) %>%
                               summarize(Total = sum(Total.decisions)))
  colnames(country_2015) <- c("Country.of.asylum", "Year", "Country_total")
  
  origin_country_2015 <- data.frame(dt.asylum %>%
                                      subset(Year == year) %>%
                                      group_by(Country.of.origin, Country.of.asylum, Year) %>%
                                      summarize(Total = sum(Total.decisions)))
  
  #Data looks fine, merge the three files into one superfile: 
  total_2015 <- merge(origin_2015, origin_country_2015, by=c("Country.of.origin", "Year"))
  total_2015 <- merge(total_2015, country_2015, by=c("Country.of.asylum", "Year"))
  colnames(total_2015) <- c("Country", "Year", "Origin", "Origin_total", "Total", "Country_total")
  
  # Extract unique country names from total_2015 dataset
  countries <- unique(total_2015$Country)
  
  # Extract unique origin country names from total_2015 dataset
  origin <- unique(total_2015$Origin)
  
  # Combine the two sets of country names
  countries_origin <- data.frame(Country = unique(c(countries, origin)))
  
  # Add an ID column to the countries_origin dataset
  countries_origin <- arrange(countries_origin, Country)
  countries_origin$id <- seq.int(nrow(countries_origin))
  colnames(countries_origin) <- c("Country", "id")
  
  total_2015 <- merge(total_2015, countries_origin, by.x = "Country", by.y = "Country")
  total_2015 <- merge(total_2015, countries_origin, by.x = "Origin", by.y = "Country")
  
  colnames(total_2015)[c(7,8)] <- c("To", "From")
  total_2015 <- total_2015[,c(1,8,2,7,3:6)]
  
  #Create a node and edge data frame:
  node_2015 <- (data.frame(Country = countries_origin$Country, 
                           Origin_total = total_2015[match(countries_origin$Country,
                                                           total_2015$Origin), 6]))
  node_2015 <- data.frame(Country = node_2015$Country,
                          Origin_total = node_2015$Origin_total,
                          Country_total = total_2015[match(countries_origin$Country,
                                                           total_2015$Country), 8])
  node_2015$id <- seq.int(nrow(node_2015))
  str(node_2015)
  node_2015[is.na(node_2015)] <- 0
  
  node_2015$group <-  ifelse(node_2015$Origin_total == 0 & node_2015$Country_total >0, "Asylum Country", 
                      ifelse(node_2015$Origin_total > 0 & node_2015$Country_total == 0, "Refugee Country",
                      ifelse(node_2015$Origin_total > 0 & node_2015$Country_total > 0 & node_2015$Origin_total/node_2015$Country_total > 100, "Mainly Refugee Country",
                      ifelse(node_2015$Origin_total > 0 & node_2015$Country_total > 0 & node_2015$Origin_total/node_2015$Country_total > 10, "Dual Flow Country",
                      ifelse(node_2015$Origin_total > 0 & node_2015$Country_total > 0 & node_2015$Country_total/node_2015$Origin_total > 100, "Mainly Asylum Country", "Dual Flow Country")))))
  
  node_2015$value <-  ifelse(node_2015$Origin_total > node_2015$Country_total, node_2015$Origin_total, node_2015$Country_total)
  node_2015$color <-  ifelse(node_2015$Origin_total == 0 & node_2015$Country_total >0, "#80CBC4",
                      ifelse(node_2015$Origin_total > 0 & node_2015$Country_total == 0, "#EF9A9A",
                      ifelse(node_2015$Origin_total > 0 & node_2015$Country_total > 0 & node_2015$Origin_total/node_2015$Country_total > 100, "#FFE082",
                      ifelse(node_2015$Origin_total > 0 & node_2015$Country_total > 0 & node_2015$Origin_total/node_2015$Country_total > 10, "#FFF59D",
                      ifelse(node_2015$Origin_total > 0 & node_2015$Country_total > 0 & node_2015$Country_total/node_2015$Origin_total > 100, "#B2DFDB", "#FFF59D")))))
  node_2015$title <- paste0("<p>",node_2015$Country," ", year, ":","<br>",
                            "Refugees to ",node_2015$Country,": ",
                            node_2015$Country_total,"<br>",
                            "Asylum seekers coming from ",node_2015$Country,
                            ":",node_2015$Origin_total,"</p>", sep="")
  node_2015$shadow <- FALSE
  node_2015$shape <- ifelse(node_2015$Origin_total == 0 & node_2015$Country_total >0, "dot",
                      ifelse(node_2015$Origin_total > 0 & node_2015$Country_total == 0, "triangle",
                      ifelse(node_2015$Origin_total > 0 & node_2015$Country_total > 0 & node_2015$Origin_total/node_2015$Country_total > 100, "triangle",
                      ifelse(node_2015$Origin_total > 0 & node_2015$Country_total > 0 & node_2015$Origin_total/node_2015$Country_total > 10, "square",
                      ifelse(node_2015$Origin_total > 0 & node_2015$Country_total > 0 & node_2015$Country_total/node_2015$Origin_total > 100, "dot", "square")))))
  
  
  node_2015_2 <- node_2015[,c(4, 1, 5, 6, 10, 8, 7, 9)]
  colnames(node_2015_2)[2] <- "label"
  
  #Done with the node, let's do the edges:
  edge_2015 <- total_2015
  edge_2015$arrows <- "to"
  edge_2015$smooth <- TRUE
  edge_2015$shadow <- FALSE
  edge_2015$dashes <- FALSE
  edge_2015$title <- paste0(edge_2015$Origin, " to ", edge_2015$Country, ": ", edge_2015$Total, sep = "")
  edge_2015$label <- c(as.character(edge_2015$Total))
  
  
  edge_2015_2 <- edge_2015[, c(2, 4, 9, 12, 13, 10, 11)]
  colnames(edge_2015_2)[c(1,2)] <- c("from", "to")
  
  # create an igraph network
  g.circ <- graph_from_data_frame(edge_2015_2, directed = TRUE, vertices = node_2015_2)

  # create the same graph with visNetwork for better visualization
  visnetwork_refugees <- visNetwork(node_2015_2, edge_2015_2, width = "100%", height = "600px") %>%
    visOptions(nodesIdSelection = TRUE, selectedBy = "group", highlightNearest = list(enabled = TRUE, degree = 1)) %>% 
    visEdges(physics = FALSE, arrows =list(to = list(enabled = TRUE, scaleFactor = 0.5))) %>% 
    visIgraphLayout(type = "full", layout = "layout_in_circle") %>% 
    visInteraction(hover = TRUE, navigationButtons = TRUE) #%>% 
    visnetwork_refugees
    
  V(g.circ)$label


  # Execute the closeness function within the new environment
  bet <- betweenness(g.circ)
  eigen <- evcent(g.circ)$vector
  close <- round(closeness(g.circ), 4)
  
  df <- data.frame(index = names(bet), country = V(g.circ)$label, betweenness = bet, eigenvector = eigen, closeness = close)
  df.bet.ordered <- df[order(-df$betweenness), c("country", "betweenness")]
  df.bet.ordered <- df.bet.ordered[1:10, ]
  
  df.eigen.ordered <- df[order(-df$eigenvector), c("country", "eigenvector")]
  df.eigen.ordered <- df.eigen.ordered[1:10, ]
  
  df.close.ordered <- df[order(-df$closeness), c("country", "closeness")]
  df.close.ordered <- df.close.ordered[1:10, ]
  
  df.merged <- data.frame(
    Country_Betweenness = df.bet.ordered$country,
    Betweenness = df.bet.ordered$betweenness,
    Country_Eigenvector = df.eigen.ordered$country,
    Eigenvector = df.eigen.ordered$eigenvector,
    Country_Closeness  = df.close.ordered$country,
    Closeness = df.close.ordered$closeness
  )
    
  return(list(visnetwork_refugees, g.circ, df.merged, df))}
  
create_prediction_graph <- function() {
  dt.asylum <- prepare_data()
  
  lon_lat <- function() {
    location.vertices <- data.table(dt.asylum) 
    location.origin <- location.vertices[, c("Country.of.origin", "Origin_Capital_Lat", "Origin_Capital_Long")]
    location.origin <- rename(location.origin,c("name" = "Country.of.origin", "lat" = "Origin_Capital_Lat", "lon" = "Origin_Capital_Long"))
    location.origin <- location.origin[, list(unique(location.origin), type = TRUE)]
    
    location.asylum <- location.vertices[, c("Country.of.asylum", "Asylum_Capital_Lat", "Asylum_Capital_Long")]
    location.asylum <- rename(location.asylum,c("name" = "Country.of.asylum", "lat" = "Asylum_Capital_Lat", "lon" = "Asylum_Capital_Long"))
    location.asylum <- location.asylum[, list(unique(location.asylum), type = FALSE)]
    
    all.locations <- rbind(location.origin, location.asylum)
    all.locations <- all.locations[!duplicated(name)]
    
    all.locations$index <- seq_len(nrow(all.locations))
    
    return(all.locations)
  }
  
  location.vertices <- lon_lat()
  edges <- dt.asylum[, c("Country.of.origin", "Country.of.asylum")]
  edges <- rename(edges, c("from" = "Country.of.origin", "to" = "Country.of.asylum"))
  
  # Match vertex names to indices in the vertex data frame
  from_idx <- match(edges$from, location.vertices$name)
  to_idx <- match(edges$to, location.vertices$name)
  
  # Create the graph
  g <- graph.data.frame(edges, directed = TRUE, vertices = location.vertices)
  g <- set_edge_attr(g, "weight", value= dt.asylum$Total.decisions + 0.001)
  weights <- E(g)$weight
  #plot(g)
  
  # Calculate the similarity between all pairs of nodes
  similarity_matrix <- similarity.jaccard(g, mode = "in")
  
  # Set the diagonal to zero (because we don't want to predict self-loops)
  diag(similarity_matrix) <- 0
  
  # Predict the top n edges with highest similarity that don't already exist
  n <- 10
  predicted_edges <- data.frame(as.matrix(which(similarity_matrix > 0, arr.ind = TRUE)))
  colnames(predicted_edges) <- c("from", "to")
  predicted_edges_weights <- similarity_matrix[as.matrix(predicted_edges)]
  predicted_edges <- cbind(predicted_edges, predicted_edges_weights)
  predicted_edges <- predicted_edges[order(-predicted_edges_weights), ]
  predicted_edges <- predicted_edges[!(predicted_edges$from %in% edges$from &
                                         predicted_edges$to %in% edges$to), ]
  
  vertex_lookup <- setNames(location.vertices$name, location.vertices$index)
  predicted_edges$from <- vertex_lookup[predicted_edges$from]
  predicted_edges$to <- vertex_lookup[predicted_edges$to]
  
  predicted_edges <- predicted_edges[(predicted_edges$from == "Germany"), ]
  predicted_edges <- predicted_edges[1:n, 1:2]
  
  # Create a new directed graph with the predicted edges
  g_predicted_edges <- graph_from_edgelist(as.matrix(predicted_edges), directed = TRUE)
  E(g_predicted_edges)$weight <- predicted_edges_weights[1:n]
  
  # m.predicted.edges <- as.matrix(cocitation(graph_pred) * (1-get.adjacency(graph_pred)))
  # g.predicted.edges <- graph_from_adjacency_matrix(m.predicted.edges,
  #                                                  mode = "directed",
  #                                                  weighted = TRUE)
  # E(g.predicted.edges)$width <- E(g.predicted.edges)$weight * 2
  # plot(g.predicted.edges)
  
  gg_pred <- get.data.frame(g_predicted_edges, "both")
  gg_vert <- gg_pred$vertices$name
  gg_vert_pred <- location.vertices[location.vertices$name %in% gg_vert, ]
  gg_vert_pred <- gg_vert_pred[complete.cases(gg_vert_pred), ]
  
  coordinates(gg_vert_pred) <- ~lon+lat
  
  edges <- gg_pred$edges
  edges <- edges[(edges$to %in% gg_vert_pred$name) & (edges$from %in% gg_vert_pred$name), ]
  
  # Loop through the columns of the edges data frame
  edges_sp <- apply(edges, 1, function(row) {
    from_vert <- gg_vert_pred[gg_vert_pred$name == row["from"], ]
    to_vert <- gg_vert_pred[gg_vert_pred$name == row["to"], ]
    
    # Check if either vertex is missing, and skip this edge if so
    if (nrow(from_vert) == 0 || nrow(to_vert) == 0) {
      return(NULL)
    }
    
    as(rbind(from_vert, to_vert), "SpatialLines")
  })
  
  # Remove NULL values from edges list
  edges_sp <- edges_sp[!sapply(edges_sp, is.null)]
  
  # Assign IDs to edges
  edges_sp <- lapply(1:length(edges_sp), function(i) {
    spChFIDs(edges_sp[[i]], as.character(i))
  })
  
  edges_sp <- do.call(rbind, edges_sp)
  return(list(graph = g_predicted_edges, vert = gg_vert_pred, edges = edges, edges_lines = edges_sp))
}
