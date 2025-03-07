#Assigning lat and long to try and do igraph
library(igraph)
library(ggplot2)
library(ggmap)
library(sf)
library(tigris)
library(dplyr)
library(writexl)
CA_airports_pass_flow <- readRDS("C:\\Users\\eilis\\OneDrive\\Documents\\UCDavis\\R-Projects\\Research_R\\Data\\CA_airports_pass_flow.rds")

airports <- readRDS("C:\\Users\\eilis\\OneDrive\\Documents\\UCDavis\\R-Projects\\Research_R\\Data\\airports.rds")

#Joining tables to inclue 3-letter airport code and airport lats and longs. 
CA_flights_yeah <- left_join(CA_airports_pass_flow, airports, by = join_by(ORIGIN_AIRPORT_ID == AIRPORT_ID)) %>% 
  left_join(airports, by = join_by(DEST_AIRPORT_ID == AIRPORT_ID), suffix = c("_origin", "_destination"))

#Reordering columns to be more readable. 
CA_flights_yeah <- CA_flights_yeah %>%
  select(AIRPORT_origin, AIRPORT_destination, total_passengers, direct_passengers, percent_direct_passengers, connecting_passengers, percent_connecting_passengers, indirect_passengers, percent_indirect_passengers, ORIGIN_AIRPORT_ID, LATITUDE_origin, LONGITUDE_origin, DEST_AIRPORT_ID, LATITUDE_destination, LONGITUDE_destination, DISPLAY_AIRPORT_NAME_origin, DISPLAY_AIRPORT_NAME_destination)

saveRDS(CA_flights_yeah, "C:/Users/eilis/OneDrive/Documents/UCDavis/R-Projects/Research_R/Data/Q2_2024_CA_airport_pair_flow.rds")

write_xlsx(CA_flights_yeah, "Q2_2024_CA_airport_pair_flow.xlsx") 


CA_flights_yeah_filtered <- CA_flights_yeah[CA_flights_yeah$total_passengers > 1000, ]


graph <- graph_from_data_frame(CA_flights_yeah_filtered, directed = TRUE, vertices = data.frame(name = unique(c(CA_flights_yeah_filtered$AIRPORT_origin, CA_flights_yeah_filtered$AIRPORT_destination))))

E(graph)$weight <- CA_flights_yeah_filtered$total_passengers

layout_coords <- layout_with_fr(graph)
scaled_layout <- layout_coords * 0.8

plot(graph, 
     vertex.size = 25, 
     vertex.label.cex = 0.6, 
     vertex.label.color = "black", 
     edge.arrow.size = 0.3, 
     edge.width = E(graph)$weight / 4000,
     edge.color = "blue", 
     layout = layout_on_grid(graph),  # Use force-directed layout for better spacing
     main = NULL)
title(main = "2023 California Air Passenger Network", cex.main = 1) 


#Trying to reposition----
# Define the Northern and Southern California airports in the desired order
northern_airports <- c("STS", "SMF", "OAK", "SFO", "SJC", "FAT")
southern_airports <- c("BUR", "LAX", "ONT", "LGB", "SNA", "SAN")

# Create a data frame with x and y coordinates
manual_positions <- data.frame(
  airport = c(northern_airports, southern_airports),
  column = c(rep(1, length(northern_airports)), rep(2, length(southern_airports))),
  stringsAsFactors = FALSE
)

# Assign x positions (columns)
manual_positions$x <- ifelse(manual_positions$column == 1, 1, 3)  # Column 1 -> x = 1, Column 2 -> x = 3

# Assign y positions (preserve the specified order)
manual_positions$y <- c(rev(seq(length(northern_airports))), rev(seq(length(southern_airports))))  # Reverse to correct order

# Ensure the layout matches the graph vertices
manual_layout <- manual_positions %>%
  filter(airport %in% V(graph)$name) %>%
  arrange(match(airport, V(graph)$name))  # Match order with graph vertices

layout_coords <- as.matrix(manual_layout[, c("x", "y")])

# Plot the graph with the corrected layout
plot(graph,
     layout = layout_coords,
     vertex.size = 25,
     vertex.label.cex = 0.6,
     vertex.label.color = "black",
     edge.arrow.size = 0.3,
     edge.width = E(graph)$weight / 5000,
     edge.color = "blue",
     main = NULL)
title(main = "2023 California Air Passenger Network", cex.main = 1)



##map and igraph----
lat_min <- 32.5   # Minimum latitude (Southern California)
lat_max <- 42.0  # Maximum latitude (Northern California)
lon_min <- -124.5 # Minimum longitude (Western California)
lon_max <- -114.0



# Filter the data based on passengers > 1000
CA_flights_yeah_filtered <- CA_flights_yeah[CA_flights_yeah$total_passengers > 1000, ]

# Create the graph
graph <- graph_from_data_frame(CA_flights_yeah_filtered, directed = TRUE,
                               vertices = data.frame(name = unique(c(CA_flights_yeah_filtered$AIRPORT_origin, 
                                                                     CA_flights_yeah_filtered$AIRPORT_destination))))

# Assign weights to the edges (number of passengers)
E(graph)$weight <- CA_flights_yeah_filtered$total_passengers

# Get unique airports and their corresponding latitude and longitude
unique_airports <- unique(c(CA_flights_yeah_filtered$AIRPORT_origin, CA_flights_yeah_filtered$AIRPORT_destination))

# Create the vertex coordinates data frame
vertex_coords <- CA_flights_yeah_filtered %>%
  filter(AIRPORT_origin %in% unique_airports) %>%
  select(AIRPORT_origin, LATITUDE_origin, LONGITUDE_origin) %>%
  rename(name = AIRPORT_origin, LAT = LATITUDE_origin, LON = LONGITUDE_origin) %>%
  distinct()  # Remove duplicate airports

# Repeat the same for destination airports
destination_coords <- CA_flights_yeah_filtered %>%
  filter(AIRPORT_destination %in% unique_airports) %>%
  select(AIRPORT_destination, LATITUDE_destination, LONGITUDE_destination) %>%
  rename(name = AIRPORT_destination, LAT = LATITUDE_destination, LON = LONGITUDE_destination) %>%
  distinct()  # Remove duplicate airports

# Combine the origin and destination coordinates
vertex_coords <- bind_rows(vertex_coords, destination_coords) %>%
  distinct()  # Remove duplicates in case any airports appear both as origin and destination

# Set the coordinates as the layout for the graph nodes
layout_coords <- vertex_coords[, c("LON", "LAT")]
rownames(layout_coords) <- vertex_coords$name

# Now use ggplot to create the map of California with the igraph network overlaid
# Load California shapefile for basemap
california_shapefile <- st_read("C:\\Users\\eilis\\OneDrive\\Documents\\UCDavis\\HSR\\Data\\Flights\\tigris_states\\california.shp") 

# Plot using ggplot
ggplot() +
  # Add the California map as the background
  geom_sf(data = california_shapefile, fill = "lightblue", color = "black") +
  
  # Add the edges (flights) between airports, weighted by the number of passengers
  geom_segment(data = CA_flights_yeah_filtered, 
               aes(x = LONGITUDE_origin, y = LATITUDE_origin, xend = LONGITUDE_destination, yend = LATITUDE_destination,
                   size = total_passengers), color = "blue", alpha = 0.5) +
  
  # Add airports as points
  geom_point(data = vertex_coords, aes(x = LON, y = LAT), color = "red", size = 4) +
  
  # Add labels for airport codes
  geom_text(data = vertex_coords, aes(x = LON, y = LAT, label = name), hjust = -0.1, vjust = -0.5, size = 3) +
  
  # Scale edge width based on the number of passengers
  scale_size_continuous(range = c(0.1, 1)) +
  xlim(lon_min, lon_max) +
  ylim(lat_min, lat_max) +
  # Adjust plot appearance
  theme_clean() +
  theme(plot.title = element_text(hjust = 0.5, size = 10), 
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none") +
  ggtitle("Airport Network Flow in California (Passengers > 1000)")


install.packages("ggrepel")
library(ggrepel)
library(ggthemes)
#simplified##########################
# Plot using ggplot
ggplot() +
  # Add the California map as the background
  geom_sf(data = california_shapefile, fill = "lightblue", color = "black") +
  
  # Add airports as points
  geom_point(data = vertex_coords, aes(x = LON, y = LAT), color = "red", size = 4) +
  
  # Add labels for airport codes
  geom_label_repel(data = vertex_coords, aes(x = LON, y = LAT, label = name), 
                  size = 3, box.padding = 0.5)  +
  
  # Scale edge width based on the number of passengers
  scale_size_continuous(range = c(0.1, 1)) +
  xlim(lon_min, lon_max) +
  ylim(lat_min, lat_max) +
  # Adjust plot appearance
  theme_clean() +
  theme(plot.title = element_text(hjust = 0.5, size = 10), 
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        legend.position = "none") +
  ggtitle("CA Airports near Proposed High Speed Rail Line")
       
library(ggthemes)
