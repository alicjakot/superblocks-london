library(sf)
library(tmap)

### Now we have all the centralities calculated we need to create one clear 
### dataset for centralities of each street to make the visualisation and clustering
### easier

# one data set with all the centralities
edgesFinal <-  data.frame(edgesSF, gDriveBet$betCentGlobal, gDriveBet1200$betCentLength1200, gDriveBet400$betCentLength400,
                          edgesCloseness1200$closeness1200,edgesCloseness400$closeness400, edgesEigen$meanEigen, 
                          edgesEigenLW$meanEigenLW)

edgesFinal <- st_as_sf(edgesFinal)

# rename the columns
# renaming the column names so we know what we're looking at
edgesFinal <- edgesFinal %>% 
  dplyr::rename(
    bet_global = gDriveBet.betCentGlobal,
    bet_1200=gDriveBet1200.betCentLength1200,
    bet_400=gDriveBet400.betCentLength400,
    cl_1200=edgesCloseness1200.closeness1200,
    cl_400=edgesCloseness400.closeness400,
    ei_global=edgesEigen.meanEigen,
    eiLW=edgesEigenLW.meanEigenLW)

# standardise them 0-1
edgesFinal$bet_global <- range01(edgesFinal$bet_global) 
edgesFinal$bet_1200 <- range01(edgesFinal$bet_1200) 
edgesFinal$bet_400 <- range01(edgesFinal$bet_400) 
edgesFinal$cl_1200 <- range01(edgesFinal$cl_1200) 
edgesFinal$cl_400 <- range01(edgesFinal$cl_400)
edgesFinal$ei_global <- range01(edgesFinal$ei_global)
edgesFinal$eiLW <- range01(edgesFinal$eiLW)
  
########## VISUALISE ALL THE CENTRALITIES ########## 

# the grey network version
tm_shape(edgesFinal)+
  tm_lines(col='cl_400',
           palette = "Greys",
           lwd="cl_400",
           style = "pretty")+
  tm_layout(main.title = "Average Closeness Centrality 400",
            legend.outside = FALSE,
            frame = FALSE)

# the version with black background
tm_shape(edgesFinal)+
  tm_lines(col='bet_400',
           palette = "Oranges",
           lwd="bet_400",
           style = "cont")+
  tm_layout(main.title = "Betwenness 400",
            legend.outside = FALSE,
            frame = FALSE,
            bg.color='black')

### GLOBAL BETWEENNESS

# find natural breaks in my values for better mapping
# write a function for this bit
classesBetGlobal <- classIntervals(edgesFinal$bet_global, n = 5, style = "fisher")
classesBetGlobal$brks

# Next we’ll create a new column in our sf object
# using the base R cut() function to cut up our variable into distinct groups.
edgesFinal <- edgesFinal %>%
  mutate(class_bet_global = cut(bet_global, classesBetGlobal$brks, include.lowest = T))

# add a thickness column based on breaks classes calculated above
edgesFinal <- edgesFinal %>%
  mutate(lwd_bet_global = case_when(
    bet_global<= 0.03993116 ~ 0.5, 
    bet_global<= 0.13202047 ~ 0.75,
    bet_global<= 0.28069899 ~ 1,
    bet_global<= 0.49979322 ~ 1.3,
    bet_global<= 1 ~ 1.5)
  )

# the map
map_global_bet <- tm_shape(edgesFinal)+
  tm_lines(col='class_bet_global',
           breaks=classesBetGlobal$brks,
           title.col = 'Global betweenness centrality',
           legend.title.size=1,
           n=5,
           palette = c('#d6d6d6','black'),
           lwd="lwd_bet_global",
           legend.lwd.show = FALSE,
           scale=2)+
  tm_layout(legend.outside = FALSE,
            frame = FALSE)

map_global_bet
library(tmap)

# seeing the map as interactive one to describe it better
tmap_mode('view')
tm_shape(edgesFinal)+
  tm_lines(col='bet_global',
           title.col = 'Global betweenness centrality',
           legend.title.size=1,
           n=5,
           palette = c('#d6d6d6','black'),
           # lwd="lwd_bet_global",
           legend.lwd.show = FALSE,
           scale=2)+
  tm_layout(legend.outside = FALSE,
            frame = FALSE)



#save it
tmap_save(tm = map_global_bet, filename = "globalBet.png")


### BETWEENNESS 1200

# find natural breaks in my values for better mapping
# write a function for this bit
classesBet1200 <- classIntervals(edgesFinal$bet_1200, n = 5, style = "fisher")
classesBet1200$brks

# Next we’ll create a new column in our sf object
# using the base R cut() function to cut up our variable into distinct groups.
edgesFinal <- edgesFinal %>%
  mutate(class_bet_1200 = cut(bet_1200, classesBet1200$brks, include.lowest = T))

# add a thickness column based on breaks classes calculated above
edgesFinal <- edgesFinal %>%
  mutate(lwd_bet1200 = case_when(
    bet_1200<= 0.04795149  ~ 0.5, 
    bet_1200<= 0.11795050  ~ 0.75,
    bet_1200<= 0.20955062  ~ 1,
    bet_1200<= 0.36242002  ~ 1.3,
    bet_1200<= 1 ~ 1.5)
  )

# the map
map_bet1200 <- tm_shape(edgesFinal)+
  tm_lines(col='class_bet_1200',
           breaks=classesBet1200$brks,
           title.col = 'Betweenness d=1200m',
           legend.title.size=1,
           n=5,
           palette = c('#d6d6d6','black'),
           lwd="lwd_bet1200",
           legend.lwd.show = FALSE,
           scale=2)+
  tm_layout(legend.outside = FALSE,
            frame = FALSE)

map_bet1200

#save it
tmap_save(tm = map_bet1200, filename = "bet1200.png")


### BETWEENNESS 400

# find natural breaks in my values for better mapping
# write a function for this bit
classesBet400 <- classIntervals(edgesFinal$bet_400, n = 5, style = "fisher")
classesBet400$brks

# Next we’ll create a new column in our sf object
# using the base R cut() function to cut up our variable into distinct groups.
edgesFinal <- edgesFinal %>%
  mutate(class_bet_400 = cut(bet_400, classesBet400$brks, include.lowest = T))

# add a thickness column based on breaks classes calculated above
edgesFinal <- edgesFinal %>%
  mutate(lwd_bet400 = case_when(
    bet_400<= 0.05668114  ~ 0.5, 
    bet_400<= 0.11987458  ~ 0.75,
    bet_400<= 0.20670526  ~ 1,
    bet_400<= 0.35045827  ~ 1.3,
    bet_400<= 1 ~ 1.5)
  )

# the map
map_bet400 <- tm_shape(edgesFinal)+
  tm_lines(col='class_bet_400',
           breaks=classesBet400$brks,
           title.col = 'Betweenness d=400m',
           legend.title.size=1,
           n=5,
           palette = c('#d6d6d6','black'),
           lwd="lwd_bet400",
           legend.lwd.show = FALSE,
           scale=2)+
  tm_layout(legend.outside = FALSE,
            frame = FALSE)

map_bet400

#save it
tmap_save(tm = map_bet400, filename = "bet400.png")


### CLOSENESS 1200

# find natural breaks in my values for better mapping
# write a function for this bit
classesCl1200 <- classIntervals(edgesFinal$cl_1200, n = 5, style = "fisher")
classesCl1200$brks

# Next we’ll create a new column in our sf object
# using the base R cut() function to cut up our variable into distinct groups.
edgesFinal <- edgesFinal %>%
  mutate(class_cl_1200 = cut(cl_1200, classesCl1200$brks, include.lowest = T))

# add a thickness column based on breaks classes calculated above
edgesFinal <- edgesFinal %>%
  mutate(lwd_cl1200 = case_when(
    cl_1200<= 0.2360226  ~ 0.5, 
    cl_1200<= 0.3655067  ~ 0.75,
    cl_1200<= 0.4948479  ~ 1,
    cl_1200<= 0.6552345  ~ 1.3,
    cl_1200<= 1 ~ 1.5)
  )

# the map
map_cl1200 <- tm_shape(edgesFinal)+
  tm_lines(col='class_cl_1200',
           breaks=classesCl1200$brks,
           title.col = 'Closeness d=1200m',
           legend.title.size=1,
           n=5,
           palette = c('#d6d6d6','black'),
           lwd="lwd_cl1200",
           legend.lwd.show = FALSE,
           scale=2)+
  tm_layout(legend.outside = FALSE,
            frame = FALSE)

map_cl1200

#save it
tmap_save(tm = map_cl1200, filename = "cl1200.png")

### CLOSENESS 400

# find natural breaks in my values for better mapping
# write a function for this bit
classesCl400 <- classIntervals(edgesFinal$cl_400, n = 5, style = "fisher")
classesCl400$brks

# Next we’ll create a new column in our sf object
# using the base R cut() function to cut up our variable into distinct groups.
edgesFinal2 <- edgesFinal %>%
  mutate(class_cl_400 = cut(cl_400, classesCl400$brks, include.lowest = T))

# add a thickness column based on breaks classes calculated above
edgesFinal2 <- edgesFinal2 %>%
  mutate(lwd_cl400 = case_when(
    cl_400<= 0.1948926 ~ 0.5, 
    cl_400<= 0.3018659 ~ 0.75,
    cl_400<= 0.4160328 ~ 1,
    cl_400<= 0.5681054 ~ 1.3,
    cl_400<= 1 ~ 1.5)
  )

# the map
map_cl400 <- tm_shape(edgesFinal)+
  tm_lines(col='class_cl_400',
           breaks=classesCl400$brks,
           title.col = 'Closeness d=400m',
           legend.title.size=1,
           n=5,
           palette = c('#d6d6d6','black'),
           lwd="lwd_cl400",
           legend.lwd.show = FALSE,
           scale=2)+
  tm_layout(legend.outside = FALSE,
            frame = FALSE)

map_cl400

#save it
tmap_save(tm = map_cl400, filename = "cl400.png")


# the comparison map of closeness 400 with cluster 2, indicating travel corridors
tmap_mode('plot')
map_clos_n_clust2 <- tm_shape(edgesFinal2)+
  tm_lines(col='class_cl_400',
           breaks=classesCl400$brks,
           title.col = 'Closeness d=400m',
           legend.title.size=1,
           n=5,
           palette = c('#d6d6d6','black'),
           lwd="lwd_cl400",
           legend.lwd.show = FALSE,
           scale=2)+
  tm_shape(cluster2DF)+
  tm_lines(col='#d6604d',
           lwd=2)+
  tm_layout(legend.outside = FALSE,
            frame = FALSE)

#save it
tmap_save(tm = map_clos_n_clust2, filename = "map_clos_n_clust2.png")


### EIGENVECTOR

### Version with BAMMtools for better jenks partitions
library(tmap)
library(BAMMtools)

# a copy of my data
dataFinal2 <- dataFinal

### EIGENVECTOR
jenksEigen <- getJenksBreaks(dataFinal$eigen, 6, subset = NULL)
jenksEigen

# add a thickness column based on breaks classes calculated above
dataFinal2 <- dataFinal2 %>%
  mutate(lwd_eigen = case_when(
    eigen<= 0.1685583 ~ 0.3, 
    eigen<= 0.4345444 ~ 0.35,
    eigen<= 0.6196001 ~ 0.5,
    eigen<= 0.8392428 ~ 0.7,
    eigen<= 1 ~ 0.8)
  )


# Next we’ll create a new column in our sf object
# using the base R cut() function to cut up our variable into distinct groups.
dataFinal2 <- dataFinal2 %>%
  mutate(class_eigen = cut(eigen, jenksEigen, include.lowest = T))


map_eigen_new_jenks2 <- tm_shape(dataFinal2)+
  tm_lines(col='class_eigen',
           palette = c('#d6d6d6','black'),
           breaks=jenksEigen,
           title.col = 'Eigenvector global',
           legend.title.size=1,
           n=5,
           lwd="lwd_eigen",
           legend.lwd.show = FALSE,
           scale=2)+
  tm_layout(legend.outside = FALSE,
            frame = FALSE)

map_eigen_new_jenks
tmap_save(tm = map_eigen_new_jenks, filename = "eigenNewJenks.png")
tmap_save(tm = map_eigen_new_jenks2, filename = "eigenNewJenks3.png")
