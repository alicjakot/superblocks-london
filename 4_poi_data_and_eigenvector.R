library(tidyverse)
library(BAMMtools) # for jenk breaks
library(ggplot2)
library(ggspatial)


### POI DATA ###


###### READ IN DATA #######

# read in points of interest data
poi <- st_read(here("data", "poiGp", "poi_4097139.gpkg"))

# read in ULEZ21 area boundary
ulez <- st_read(here("data", "ulezshp", "ulez21.shp"))

tmap_mode("view")
qtm(ulez)

# get rid of the poi data  outside Ulez21
poiUlez <- poi[ulez,]

# check on a map
qtm(poiUlez)


###### POINTS OF INTEREST DATA #######

# see how it looks like
sample_n(poiUlez, 10)

# remove unnecessary rows with information I don't need
poiUlez2<-subset(poiUlez, groupname!="Manufacturing and Production" & categoryname!="Air"
                 & categoryname!="Road and Rail" & categoryname!="Water" & classname!="Electrical Features"
                 & classname!="Fire Safety Features" & classname!="Gas Features" & classname!="Letter Boxes"
                 & classname!="Recycling Centres" & classname!="Refuse Disposal Facilities" & classname!="Telecommunications companies"
                 & classname!="Telecommunications Features" & classname!="Utility Companies and Brokers"
                 & classname!="Waste Storage, Processing and Disposal")

# remove unnecessary columns
# stuff I want to keep
toKeep = c('ref_no','name','groupname','categoryname','classname','street_name','admin_boundary','geom')

# select only columns to keep
poiOK <- poiUlez2[toKeep]
poiOK

# visualisation of points to see a pattern

# create new sf with X and Y variables
poiUlez3 <- poiOK %>%
  cbind(st_coordinates(.)) # get X and Y coordinates


# plot
map_poi_density <- ggplot(poiUlez3, aes(x = X, y = Y)) + 
  geom_point(size = 0.45, alpha = 0.25) + 
  coord_equal() + 
  xlab('Longitude') + 
  ylab('Latitude') +
  theme_void()+
  annotation_scale(location = "br", 
                   style = "ticks", 
                   tick_height = 0, 
                   height = unit(0, "mm"), 
                   line_width = 0.7, 
                   pad_x = unit(7, "mm"), 
                   text_cex = 1)

map_poi_density

# save it
ggsave('final_poi_density.png', plot =map_poi_density )



###### ADDING A NUMBER OF POI TO STREETS IN THE NETWORK #######


# add a number of points of interests on a street 
# as an edge attribute

# transform network's crs to british to match points of interest
gDrive <- gDrive %>% 
  st_transform(27700)

# Find indices of nearest nodes
# for each feature in poiOK return the nearest feature in gDrive
nearest_nodes = st_nearest_feature(poiOK, gDrive)
nearest_nodes
class(nearest_nodes)
# it gives back an integer with nodes numbers for each poi

# try with edges

# add an id attrubute to an edge
edgesSF <- edgesSF %>% 
  mutate(id = row_number())

# add information for a point of interest on what edge it lies
poiOK2 <- poiOK %>% 
  mutate(edge = st_nearest_feature(poiOK, edgesSF))

# a simple dataset with info on points and edges
# same as above but simplified
poi_on_edge <- poiOK2 %>% 
  select(ref_no,edge)

# count the occurrences of the pois on each edge
poi_on_edge2 <- poi_on_edge %>% 
  count(edge)

# rename the columns so can do the merge to edgesDrive (defined in previous workbook) 
# and drop geometry
poi_on_edge2 <- poi_on_edge2 %>% 
  st_drop_geometry() %>% 
  rename(id=edge, no_of_pois=n)

# merge into edgesSF2
edgesDrive2=merge(edgesDrive, poi_on_edge2, by = "id", all = TRUE)

# replace NAs with 0
edgesDrive$no_of_pois <- edgesSF2$no_of_pois %>% 
  replace_na(0)

# add a new weight column with a number of pois divided by a length
# of a street
edgesDrive2 <- edgesDrive2 %>% 
  mutate(weight_poi_norm=(no_of_pois/weight))


# see the distribution of this weight 

# histogram - normalised
ggplot(edgesDrive2, aes(x=weight_poi_norm)) + 
  geom_histogram(binwidth=0.5)

# histogram - number of pois
ggplot(edgesDrive2, aes(x=no_of_pois)) + 
  geom_histogram(binwidth=10)

# summary statistics
summary(edgesDrive2$weight_poi_norm)
summary(edgesDrive2$no_of_pois)

# boxplot
boxplot(edgesDrive2$weight_poi_norm,
        ylab = "weighted poi norm")

boxplot(edgesDrive2$no_of_pois,
        ylab = "weighted poi norm")

# histogram suggests the data is very skewed
library(e1071)
skewness(edgesSF2$weight_poi_norm)
skewness(edgesSF2$no_of_pois)


# add this information to the network
gDrive2 <- st_join(gDrive, edgesDrive2)
gDrive2

# extract edges to an sf object
edgesSFpoi <- gDrive2 %>%
  activate("edges") %>%
  st_as_sf()



####### EIGENVECTOR CENTRALITY #######


# calculate eigenvector centrality for nodes
eigenCentr = gDrive2 %>% 
  activate("nodes") %>%
  mutate(eigenCentr = centrality_eigen(
    weights = E(gDrive2)$weightPoiNorm
  ))

nodesEigen <- eigenCentr %>%
  activate("nodes") %>%
  st_as_sf()


#range the eigenvector values 0-1
nodesEigen$eigenCentr <- nodesEigen$eigenCentr %>% 
  range01(.)
  
# see the distribution
# a histogram to see the distribution of the values
ggplot(nodesEigen, aes(x=eigenCentr)) + 
  geom_histogram(bins=1000)


# visualise it on nodes
# plot it
tm_shape(nodesEigen)+
  tm_dots(col = 'percent_class',
          size = 0.0001,
          shape = 2,
          palette = "Blues",
          title = NA,
          style = "pretty")+
  tm_layout(legend.outside = FALSE,
            frame = FALSE)

# find natural breaks in my values for better mapping
# write a function for this bit
classesBetEig <- classIntervals(nodesEigen$eigenCentr, n = 5, style = "fisher")
classesBetEig$brks

# Next we’ll create a new column in our sf object
# using the base R cut() function to cut up our variable into distinct groups.
nodesEigen <- nodesEigen %>%
  mutate(percent_class = cut(eigenCentr, classesBetEig$brks, include.lowest = T))


### VISUALISE ON THE EDGES

# calculate an average eigenvector centrality from two nodes for an edge
edgesEigen <- eigenCentr %>%
  activate("edges") %>%
  # mutate(mean_centrality = (.N()$eigenCentr[from] + .N()$eigenCentr[to])/2) %>% 
  st_as_sf()

# my loop version
# iterate over edges and nodes to add an average value of closeness for an edge
for (e in 1:nrow(edgesEigen)){
  node1 <- edgesEigen$from[[e]]
  node2 <- edgesEigen$to[[e]]
  value1 <- nodesEigen$eigenCentr[node1]
  value2 <- nodesEigen$eigenCentr[node2] 
  valueSum <- value1+value2
  edgesEigen$meanEigen[e] <- valueSum/2
}

# find natural breaks in my values for better mapping
# write a function for this bit
classesEig2 <- classIntervals(edgesEigen$meanEigen, n = 20, style = "fisher")
classesEig2$brks

# Next we’ll create a new column in our sf object
# using the base R cut() function to cut up our variable into distinct groups.
edgesEigen <- edgesEigen %>%
  mutate(percent_class2 = cut(meanEigen, classesEig2$brks, include.lowest = T))


# plot it
tm_shape(edgesEigen)+
  tm_lines(col='percent_class2',
           palette = "Blues",
           # lwd="percent_class2",
           style = "pretty")+
  tm_layout(main.title = "Average Eigenvector Centrality",
            legend.outside = TRUE,
            frame = FALSE)

### easier to calculate the average centrality
# # Get data from the nodes while computing for the edges
# create_notable('bull') %>%
#   activate(nodes) %>%
#   mutate(centrality = centrality_power()) %>%
#   activate(edges) %>%
#   mutate(mean_centrality = (.N()$centrality[from] + .N()$centrality[to])/2)

#disttibution of eigenvector
plot5 <-  edgesEigen %>%
  ggplot(aes(x = "eigenvector", y = meanEigen)) + 
  geom_jitter(width = .02, height = 0, size = 2, alpha = .6,  color = "violet") +
  labs(x = "", y="eigenvector")
plot5




