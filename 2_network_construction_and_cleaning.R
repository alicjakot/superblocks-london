# load the libraries
library(sfnetworks)
library(sf)
library(tidygraph)
library(igraph)
library(ggplot2)
library(sp)
library(here)
library(tmap)
library(dplyr)




###### CONSTRUCT THE NETWORK #######


### DRIVEABLE NETWORK

# read in shp downloaded with osmnx for ULEZ21 in London
edgesShpDrive <- st_read(here("data",
                              'driving',
                              "edges.shp"))

# length is a built in function in R so need to rename it, 
# otherwise it throws error while calculating centralities
edgesShpDrive <- edgesShpDrive %>% 
  dplyr::rename(
    weight = length)

# check for null values in street lengths
sum(is.na(edgesShpDrive$weight))
sum(is.infinite(edgesShpDrive$weight))

# no null values, no infinites
# check the type and a class
typeof(edgesShpDrive$weight)
class(edgesShpDrive$weight)

# choose only the columns I want to use
edgesShpDrive2 <-  edgesShpDrive %>% 
  select(from, to, name, weight, maxspeed, geometry)

# construct a network out of it
gDrive <- as_sfnetwork(edgesShpDrive2, directed = FALSE)
gDrive

# check the network
# class(gDrive)
# gDrive


# extract nodes and edges to an sf object
edgesDrive <- gDrive %>%
  activate("edges") %>%
  st_as_sf()

nodesDrive <- gDrive %>%
  activate("nodes") %>%
  st_as_sf()




####### NETWORK CLEANING AND PRE-PROCESSING #######

# based on this post https://luukvdmeer.github.io/sfnetworks/articles/preprocess_and_clean.html



### simplify network
# get rid of multiple and edges (edges that connect the same pair of nodes, 
# or start and end at the same node)

gDriveSimple = gDrive %>%
  activate("edges") %>%
  arrange(edge_length()) %>%
  filter(!edge_is_multiple()) %>%
  filter(!edge_is_loop())

### smooth pseudo nodes
# get rid of pseudo nodes
gDriveSimple = convert(gDriveSimple, to_spatial_smooth)
gDriveSimple

# extract nodes and edges to an sf object
edgesDriveSimple <- gDriveSimple %>%
  activate("edges") %>%
  st_as_sf()

nodesDriveSimple <- gDriveSimple %>%
  activate("nodes") %>%
  st_as_sf()






