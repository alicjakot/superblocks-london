library(sfnetworks)
library(sf)
library(tidygraph)
library(tidyverse)
library(igraph)
library(ggplot2)
library(sp)
library(here)
library(tmap)
library(viridis)
library(classInt)
library(dplyr)
library(ggthemes) # to style visualisations




############ CLOSENESS CENTRALITY ############


###### 1200 m ######

# calculate closeness centrality for nodes within 1200 m distance
closeness1200 = gDrive %>% 
  activate("nodes") %>%
  mutate(closeness1200 = centrality_closeness(
    weights = weight,
    normalized = TRUE,
    cutoff = 1200
  )) 

# set as an sf object
nodesCloseness1200 <- closeness1200 %>%
  activate("nodes") %>%
  st_as_sf() %>% 
  mutate(id = row_number())

# plot it
tm_shape(nodesCloseness1200)+
  tm_dots(col = 'closeness1200',
          size = 0.0001,
          shape = 2,
          palette = "Blues",
          title = NA)+
  tm_layout(legend.outside = FALSE,
            frame = FALSE)

# trying to calculate an average closeness value for nodes for each street
# so need to modify my edgesDrive df to have a new column with an average closeness
# centrality for nodes

# a new column with a number of nodes within an edge
edgesCloseness1200 <- transform(edgesDrive, closeness1200 = 0) %>% 
  mutate(id = row_number())

# add an id column to nodes
nodesDrive <- nodesDrive %>% 
  mutate(id = row_number())

# iterate over edges and nodes to add an average value of closeness for an edge
for (e in 1:nrow(edgesCloseness1200)){
  node1 <- edgesCloseness1200$from[[e]]
  node2 <- edgesCloseness1200$to[[e]]
  value1 <- nodesCloseness1200$closeness1200[node1]
  value2 <- nodesCloseness1200$closeness1200[node2] 
  valueSum <- value1+value2
  edgesCloseness1200$closeness1200[e] <- valueSum/2
}


# define a function out of it
# a function that calculates an average closeness centrality
# for a street based on two nodes' centralities

# avgCloseness <- function(edgesSF, nodesSF){
#   for (e in 1:nrow(edgesSF)){
#     node1 <- edgesSF$from[[e]]
#     node2 <- edgesSF$to[[e]]
#     value1 <- nodesSF$closeness[node1]
#     value2 <- nodesSF$closeness[node2] 
#     valueSum <- value1+value2
#     edgesSF$closeness[e] <- valueSum/2
#   }
# }

# plot it
tm_shape(edgesCloseness1200)+
  tm_lines(col='closeness1200',
           palette = "Blues",
           lwd="closeness1200",
           style = "pretty")+
  tm_layout(main.title = "Weighted Closeness Centrality 1200",
            legend.outside = TRUE,
            frame = FALSE)



###### 400 m ######

# calculate closeness centrality for nodes within 400 m distance
closeness400 = gDrive %>% 
  activate("nodes") %>%
  mutate(closeness400 = centrality_closeness(
    weights = weight,
    normalized = TRUE,
    cutoff = 400
  )) 

# set as an sf object
nodesCloseness400 <- closeness400 %>%
  activate("nodes") %>%
  st_as_sf()

# plot it
tm_shape(nodesCloseness400)+
  tm_dots(col = 'closeness400',
          size = 0.0001,
          shape = 2,
          palette = "Blues",
          title = NA)+
  tm_layout(legend.outside = FALSE,
            frame = FALSE)


# a new column with a number of nodes within an edge
edgesCloseness400 <- transform(edgesDrive, closeness400 = 0) %>% 
  mutate(id = row_number())

# iterate over edges and nodes to add an average value of closeness for an edge
for (e in 1:nrow(edgesCloseness400)){
  node1 <- edgesCloseness400$from[[e]]
  node2 <- edgesCloseness400$to[[e]]
  value1 <- nodesCloseness400$closeness400[node1]
  value2 <- nodesCloseness400$closeness400[node2] 
  valueSum <- value1+value2
  edgesCloseness400$closeness400[e] <- valueSum/2
}

# plot it
tm_shape(edgesCloseness400)+
  tm_lines(col='closeness400',
           palette = "Blues",
           lwd="closeness400",
           style = "pretty")+
  tm_layout(main.title = "Average Closeness Centrality 400",
            legend.outside = TRUE,
            frame = FALSE)





############ BETWEENNESS CENTRALITY ############


### BETWEENNESS WITH STREET_LENGHTH AS A WEIGHT

edgesDF <- gDriveSimple %>%
  activate("edges") %>%
  st_as_sf()

# calculate betweenness centrality with street length as weight
gDriveEdges = gDrive %>%
  activate("edges") %>%
  mutate(weight = E(gDrive)$weight) %>%
  mutate(betCentLength = centrality_edge_betweenness(weights = weight, directed = FALSE, cutoff = NULL)) %>% 
  st_as_sf()

# scale to 0-1 values (there is a function in tidygraph that does it
# while calculating the centrality)
gDriveEdges$betCentLength <- gDriveEdges$betCentLength %>%
  range01(.)

# find natural breaks in my values for better mapping
# write a function for this bit
classesBet2 <- classIntervals(gDriveEdges$betCentLength, n = 5, style = "fisher")
classesBet2$brks

# Next we’ll create a new column in our sf object
# using the base R cut() function to cut up our variable into distinct groups.
gDriveEdges <- gDriveEdges %>%
  mutate(percent_classBet2 = cut(betCentLength, classesBet2$brks, include.lowest = T))

# plot it
tmap_mode("view")

tm_shape(gDriveEdges)+
  tm_lines(col='percent_classBet2',
           palette = "Blues",
           lwd="betCentLength",
           style = "pretty")+
  tm_layout(main.title = "Weighted Betweenness Centrality",
            legend.outside = TRUE,
            frame = FALSE)
           # title = "Topological Betweenness centrality")
  # tm_scale_bar(breaks = c(0, 0.2, 0.4,0.6,0.8,1),
               # text.size = 1)



### BETWEENNESS CENTRALITY WITH STREET LENGHT AS A WEIGHT AND 1200 m DISTANCE


# calculate it
gDriveEdges = gDrive %>%
  activate("edges") %>%
  mutate(weight = E(gDrive)$weight) %>%
  mutate(betCentLength1200 = centrality_edge_betweenness(weights = weight, directed = FALSE, cutoff = 1200)) %>% 
  st_as_sf()

# scale to 0-1 values (there is a function in tidygraph that does it
# while calculating the centrality)
gDriveEdges$betCentLength1200 <- gDriveEdges$betCentLength1200 %>%
  range01(.)

# find natural breaks in my values for better mapping
# write a function for this bit
classesBet3 <- classIntervals(gDriveEdges$betCentLength1200, n = 5, style = "fisher")
classesBet3$brks

# Next we’ll create a new column in our sf object
# using the base R cut() function to cut up our variable into distinct groups.
gDriveEdges <- gDriveEdges %>%
  mutate(percent_classBet3 = cut(betCentLength1200, classesBet3$brks, include.lowest = T))

# plot it
tm_shape(gDriveEdges)+
  tm_lines(col='percent_classBet3',
           palette = "Blues",
           lwd="betCentLength1200",
           style = "pretty")+
  tm_layout(main.title = "Weighted Betweenness Centrality d 1200",
            legend.outside = TRUE,
            frame = FALSE)
# title = "Topological Betweenness centrality")
# tm_scale_bar(breaks = c(0, 0.2, 0.4,0.6,0.8,1),
# text.size = 1)



# -------------------------------------------------------------------------

# explore the street street length

# find natural breaks in my values for better mapping
classesStreetsLength <- classIntervals(edgesDrive2$length, n = 5, style = "fisher")

# Next we’ll create a new column in our sf object 
# using the base R cut() function to cut up our variable into distinct groups.
edgesDrive2 <- edgesDrive2 %>%
  mutate(streetsLength_class = cut(length, classesStreetsLength$brks, include.lowest = T))

# plot it
tmap_mode('view')

tm_shape(edgesDrive2)+
  tm_lines(col='streetsLength_class',
           palette = "-magma",
           style = "pretty")


