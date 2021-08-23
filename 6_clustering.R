## K-means ##
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(NbClust) # for a number of clusters
library(ggplot2)
library(dplyr)
library(e1071) # for computing skewness coefficient
library(plotly)
library(GGally)

# prepare a dataset for clustering
clust <- data.frame(
  # edgesFinal$id,
  edgesFinal$bet_global, edgesFinal$bet_1200,
  edgesFinal$bet_400, edgesFinal$cl_1200, edgesFinal$cl_400,
  edgesFinal$eiLW)


# rename the columns
clust <- clust %>% 
  dplyr::rename(
    # id = edgesFinal.id,
    bet_global=edgesFinal.bet_global,
    bet_1200=edgesFinal.bet_1200,
    bet_400=edgesFinal.bet_400,
    cl_1200=edgesFinal.cl_1200,
    cl_400=edgesFinal.cl_400,
    ei_lw=edgesFinal.eiLW
  )




#----------------------- OUTLIERS DETECTION -----------------------
#---------------------and skewness reduction-----------------------

# let's take all the columns into consideration, so first



########### global betweenneess

# summary of that column
summary(clust$bet_global)

# boxplot
boxplot(clust$bet_global,
        ylab = "hwy")

# check for potential outliers with boxplot
boxplot.stats(clust$bet_global)$out

# IQR method
quantile(clust$bet_global)
iqrBG <- iqr(clust$bet_global)
iqrBG

sum(clust$bet_global > 3*iqrBG)

# so if a value is above this its considered an extreme outlier
3*iqrBG

# looking for a value to change the outliers values with
# outliers have values bigger than 3.513.198
upBG <- quantile(clustIQR$bet_global, c(.8, .85, .9, .95))
upBG

percentile85BG <- quantile(clust$bet_global,.85)


# percentiles method based on methods from this website https://statsandr.com/blog/outliers-detection-in-r/
lower_bound <- quantile(clust$bet_global, 0.025)
lower_bound
# if something is below 0 than it is an outlier, but no negative values

upper_bound <- quantile(clust$bet_global, 0.95)
upper_bound
# so if any of the values is bigger than 25688471 it is considered an outlier
# lets check how many values like this are there

# see how many values bigger than 25688471
sum(clust$bet_global > upper_bound)

# there are 3029 values considered as outliers

# let's see what they are
outlier_ind <- which(clust$bet_global < lower_bound | clust$bet_global > upper_bound)
outlier_ind

# let's replace them with 95% value
clust <- clust %>%
  mutate(bet_global = case_when(
    clust$bet_global <= upper_bound ~ clust$bet_global,
    clust$bet_global > upper_bound ~ upper_bound))

# check everything on histogram
ggplot(clust, aes(x=bet_global)) + 
  geom_histogram(bins = 300)

# based on the histogram
# let's check the skewness number
skewness(clust$bet_global)

# the skewness coefficient is 2.470324 which means
# severly positively skewed

# # cube root trans
clust <- clust %>%
  mutate(bet_global = clust$bet_global^(1/3))

# looks better after this


########### betweenneess 1200



# summary of that column
summary(clust$bet_1200)

# boxplot
boxplot(clust$bet_1200)


lower_bound2 <- quantile(clust$bet_1200, 0.025)
lower_bound2

upper_bound2 <- quantile(clust$bet_1200, 0.95)
upper_bound2
# so if any of the values is bigger than 5518.1 it is considered an outlier
# lets check how many values like this are there

# see how many values bigger than 5518.1
sum(clust$bet_1200 > upper_bound2)

# there are 3029 values considered as outliers

# let's replace them with 95% value
clust <- clust %>%
  mutate(bet_1200 = case_when(
    clust$bet_1200 <= upper_bound2 ~ clust$bet_1200,
    clust$bet_1200 > upper_bound2 ~ upper_bound2))

# check everything on histogram
ggplot(clust, aes(x=bet_1200)) + 
  geom_histogram(bins = 300)

skewness(clust$bet_1200)

# the skewness coefficient is 2.166591 which means
# severly positively skewed

# cube root trans
clust <- clust %>%
  mutate(bet_1200 = clust$bet_1200^(1/3))


########### betweenneess 400



# summary of that column
summary(clust$bet_400)

# boxplot
boxplot(clust$bet_400)


upper_bound3 <- quantile(clust$bet_400, 0.95)
upper_bound3
# so if any of the values is bigger than 256.5 it is considered an outlier
# lets check how many values like this are there

# see how many values bigger than 256.5
sum(clust$bet_400 > upper_bound3)

# there are 3029 values considered as outliers

# let's replace them with 95% value
clust <- clust %>%
  mutate(bet_400 = case_when(
    clust$bet_400 <= upper_bound3 ~ clust$bet_1200,
    clust$bet_400 > upper_bound3 ~ upper_bound3))

# check everything on histogram
ggplot(clust, aes(x=bet_400)) + 
  geom_histogram(bins = 300)

# now let's do something with the skewness as it is positively skwewed
# based on the histogram
# let's check the skewness number
skewness(clust$bet_400)

# the skewness coefficient is 2.166591 which means
# severly positively skewed

# # cube root trans
clust <- clust %>%
  mutate(bet_400 = clust$bet_400^(1/3))



########### closeness 1200



# summary of that column
summary(clust$cl_1200)

# boxplot
boxplot(clust$cl_1200)


upper_bound4 <- quantile(clust$cl_1200, 0.95)
upper_bound4
# so if any of the values is bigger than 2.189746e-05 it is considered an outlier
# but this variable looks way better than the other ones

# see how many values bigger than 256.5
sum(clust$cl_1200 > upper_bound4)

# there are 3029 values considered as outliers

# let's replace them with 95% value
clust <- clust %>%
  mutate(cl_1200 = case_when(
    clust$cl_1200 <= upper_bound4 ~ clust$cl_1200,
    clust$cl_1200 > upper_bound4 ~ upper_bound4))

# check everything on histogram
ggplot(clust, aes(x=cl_1200)) + 
  geom_histogram(bins = 300)

skewness(clust$cl_1200)

# the skewness coefficient is 0.2733818 


########### closeness 400



# summary of that column
summary(clust$cl_400)

# boxplot
boxplot(clust$cl_400)
#this one doesnt look that bad, not removing anything

# check everything on histogram
ggplot(clust, aes(x=cl_400)) + 
  geom_histogram(bins = 300)

skewness(clust$cl_4003)


# cube root trans
clust <- clust %>%
  mutate(cl_4003 = clust$cl_400^(1/3))



########### eigenvector



# summary of that column
summary(clust$ei_lw)

# boxplot
boxplot(clust$ei_lw)


upper_bound6 <- quantile(clust$ei_lw, 0.95)
upper_bound6
# so if any of the values is bigger than 2.189746e-05 it is considered an outlier
# but this variable looks way better than the other ones

# see how many values bigger than 256.5
sum(clust$ei_lw > upper_bound6)

# there are 3028 values considered as outliers

# let's replace them with 95% value
clust <- clust %>%
  mutate(ei_lw = case_when(
    clust$ei_lw <= upper_bound6 ~ clust$ei_lw,
    clust$ei_lw > upper_bound6 ~ upper_bound6))

# check everything on histogram
ggplot(clust, aes(x=ei_lw)) + 
  geom_histogram(bins = 300)

skewness(clust$ei_lw)

# the skewness coefficient is 1.964595 

# cube root trans
clust <- clust %>%
  mutate(ei_lw = clust$ei_lw^(1/3))

#-------------------------------------------------------

# remove unneccessary columns
# Dplyr remove multiple columns by name:
clust <- select(clust, -c(cl_4003, cl_400sqrt))

# multiply columns with small numbers so they have bigger values
clust2 <- clust %>%
  mutate(cl_1200=cl_1200*1000000) %>% 
  mutate(cl_400=cl_400*1000000) %>% 
  mutate(ei_lw=ei_lw*1000000)

# standardise values so they have the same range

clust3 <- clust2 %>% 
  mutate(bet_global=range01(bet_global)) %>% 
  mutate(bet_1200=range01(bet_1200)) %>%
  mutate(bet_400=range01(bet_400)) %>%
  mutate(cl_1200=range01(cl_1200)) %>%
  mutate(cl_400=range01(cl_400)) %>% 
  mutate(ei_lw=range01(ei_lw))
  

# check variances
# library(car)
# leveneTest(bet_global ~ bet_1200*bet_400*cl_1200*cl_400*ei_lw, data = clust3)

var(clust3$bet_global)
var(clust3$bet_1200)
var(clust3$bet_400)
var(clust3$cl_1200)
var(clust3$cl_400)
var(clust3$ei_lw)



# ------------------------------------------------------

# Clustering 

# ------------------------------------------------------


####### CHOOSING THE RIGHT NUMBER OF CLUSTERS
#### Finding the right amount of clusters


# the methods I know for finding the right amount of clusters
# do not work for a large amount of data
# therefore I will take a sample of my network centralities data

sample500 <- sample(1:nrow(edgesClusteringLW), 500)
sample2000 <- sample(1:nrow(edgesClusteringLW), 2000)
sample5000 <- sample(1:nrow(edgesClusteringLW), 5000)

# see the sample
clusteringSample500 <- clust3[sample500,]
clusteringSample2000 <- clust3[sample2000,]
clusteringSample5000 <- clust3[sample5000,]



#### choosing the right value of k for the sample

### Elbow method

# for 500 sample
fviz_nbclust(clusteringSample500, kmeans, method = "wss", linecolor = "#ed7d31") +
  geom_vline(xintercept = 3, linetype = 2) + # add line for better visualisation
  labs(subtitle = "Elbow method") # add subtitle
#this method suggests 3 or 4 clusters

# for 2000 sample
fviz_nbclust(clusteringSample2000, kmeans, method = "wss", linecolor = "#ed7d31") +
  geom_vline(xintercept = 3, linetype = 2) + # add line for better visualisation
  labs(subtitle = "Elbow method") # add subtitle
#this method suggests 3 or 4 clusters

# for 5000 sample
fviz_nbclust(clusteringSample5000, kmeans, method = "wss", linecolor = "#ed7d31") +
  geom_vline(xintercept = 3, linetype = 2) + # add line for better visualisation
  labs(subtitle = "Elbow method") # add subtitle
#this method suggests 3 or 4 clusters



### Silhouette method


# 500 sample
fviz_nbclust(clusteringSample500, kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method")
# this one suggests 4 clusters

# 2000 sample
fviz_nbclust(clusteringSample2000, kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method")
# this one suggests 2 clusters, 3 very close

# 5000 sample
fviz_nbclust(clusteringSample5000, kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method")
# this one suggests 2 clusters




### gap statistic method

# 500 sample
# gap statistic second version
set.seed(123)
gap_stat <- clusGap(clusteringSample500, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
# Print the result
print(gap_stat, method = "firstmax")
# suggests 10 on a smaller sample


# 2000 sample
# gap statistic second version
set.seed(123)
gap_stat <- clusGap(clusteringSample2000, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
# Print the result
print(gap_stat, method = "firstmax")
# suggests 10 on a smaller sample


# 5000 sample
# gap statistic second version
set.seed(123)
gap_stat <- clusGap(clusteringSample5000, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
# Print the result
print(gap_stat, method = "firstmax")
# suggests 10 on a smaller sample


########## CLUSTERING ########## 


### CLUSTERING ###

# 3 clusters
set.seed(123)
clustered3 <- kmeans(clust3, 3, nstart = 200)
edgesFinal$cluster3 <- clustered3$cluster
clust3$cluster3 <- clustered3$cluster

# 4 clusters
set.seed(123)
clustered4 <- kmeans(clust3, 4, nstart = 200)
edgesFinal$cluster4 <- clustered4$cluster
clust3$cluster4 <- clustered4$cluster

# 10 clusters
set.seed(123)
clustered10 <- kmeans(clust3, 10, nstart = 200)
edgesFinal$cluster10 <- clustered10$cluster
clust3$cluster10 <- clustered10$cluster


# a map of clusters with ggplot
require(sf)
map_cl3 <- ggplot() +
  geom_sf(data = edgesFinal$geometry,
          aes(color=edgesFinal$cluster3),
          size = .4,
          alpha = .65)+
  theme_void()

map_cl3


map_cl4 <- ggplot() +
  geom_sf(data = edgesFinal$geometry,
          aes(color=edgesFinal$cluster4),
          size = .4,
          alpha = .65)+
  theme_void()

map_cl4

map_cl10 <- ggplot() +
  geom_sf(data = edgesFinal$geometry,
          aes(color=edgesFinal$cluster10),
          size = .4,
          alpha = .65)+
  theme_void()

map_cl10


# ------------------------------------------------------

# Clusters Exploration

# ------------------------------------------------------
# rename
clust3 <- clust3 %>% 
  dplyr::rename(
    eigen = ei_lw)

library(sf)
dataFinal <- st_as_sf(clust3, edgesFinal$geometry, edgesFinal$id)
head(dataFinal)

# rename
dataFinal <- dataFinal %>% 
  dplyr::rename(
    id = '1:60577',
    geometry=edgesFinal.geometry)

clustered3
clustered4
clustered10

# # a basic plot with values
# 
# p <- ggparcoord(data = dataFinal,
#                 columns = c(1:6), 
#                 mapping=aes(color=as.factor(cluster3))) +
#   scale_color_discrete("cluster") +
#   labs(x = "variable", 
#        y = "value", 
#        title = "Clustering")
# 
# ggplotly(p)
# too many values to get something out of it


# Reshape the data
library(tidyr)
center_reshape <- gather(clust3, features, values, 1:6)
head(center_reshape)

library(RColorBrewer)
# Create the palette
hm.palette <-colorRampPalette(rev(brewer.pal(10, 'RdYlGn')),space='Lab')

# Create the palette
hm.palette2 <-colorRampPalette(rev(brewer.pal(10, 'RdGy')),space='Lab')

# Create the palette
hm.palette3 <-colorRampPalette(rev(brewer.pal(10, 'PuOr')),space='Lab')

# Plot the heat map
heat_cluster3 <- ggplot(data = center_reshape, aes(x = features, y = cluster3, fill = values)) +
  scale_y_continuous(breaks = seq(1, 7, by = 1)) +
  geom_tile() +
  xlab(label = "centrality")+
  ylab(label = "cluster")+
  ggtitle(label = "3 clusters")+
  coord_equal() +
  scale_fill_gradientn(colours = hm.palette2(90)) +
  theme_classic()+
  theme(legend.position="none")
heat_cluster3

heat_cluster4 <- ggplot(data = center_reshape, aes(x = features, y = cluster4, fill = values)) +
  scale_y_continuous(breaks = seq(1, 7, by = 1)) +
  geom_tile() +
  xlab(label = "centrality")+
  ylab(label = "cluster")+
  ggtitle(label = "4 clusters")+
  coord_equal() +
  scale_fill_gradientn(colours = hm.palette2(90)) +
  theme_classic()+
  theme(legend.position="none")
heat_cluster4

heat_cluster10 <- ggplot(data = center_reshape, aes(x = features, y = cluster10, fill = values)) +
  scale_y_continuous(breaks = seq(1, 10, by = 1)) +
  geom_tile() +
  xlab(label = "centrality")+
  ylab(label = "cluster")+
  ggtitle(label = "10 clusters")+
  coord_equal() +
  scale_fill_gradientn(colours = hm.palette2(90)) +
  theme_classic()
heat_cluster10


# arrange the plots to display

# library(gridExtra)
# grid.arrange(heat_cluster3, heat_cluster4, heat_cluster10)

library(ggpubr)

lefty <- ggarrange(heat_cluster3, heat_cluster4, 
                  labels = c("A", "B"),
                  ncol = 1, nrow = 2,
                  align = "hv")

righty <- ggarrange(heat_cluster10,  
                   labels = 'C',
                   ncol = 1, nrow = 1)

ggarrange(lefty, righty, 
          ncol = 2, nrow = 1,
          align = "hv")

# ggexport(arr, filename = "clusters_plots.png")


### Okay, going for version with 4 clusters

library(tmap)
tm_shape(dataFinal)+
  tm_lines(col='cluster4',
           palette = "RdGy",
           direction=-1,
           lwd=0.5,
           style = "pretty")+
  tm_layout(main.title = "Clusters",
            legend.outside = FALSE,
            frame = FALSE)


# the map
map_clusters <- tm_shape(edgesFinal)+
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



# subset rows for each cluster
class(dataFinal)

cluster1DF <- filter(dataFinal, cluster4 == 1)
cluster2DF <- filter(dataFinal, cluster4 == 2)
cluster3DF <- filter(dataFinal, cluster4 == 3)
cluster4DF <- filter(dataFinal, cluster4 == 4)


library(ggthemes)
clust_map <- ggplot() +
  geom_sf(data = cluster1DF$geometry,
          inherit.aes = FALSE,
          color = "#8d99ae", #grey
          size = .8) +
  geom_sf(data = cluster2DF$geometry,
          inherit.aes = FALSE,
          color = "#ef233c", #red
          size = .8)+
  geom_sf(data = cluster3DF$geometry,
          inherit.aes = FALSE,
          color = "black", 
          size = .8) +
  geom_sf(data = cluster4DF$geometry,
          inherit.aes = FALSE,
          color = "#F8A7B1",# pink
          size = .6)+
  theme_void()

clust_map +
  ggsave(filename = 'clustering_map_not_finished.png')








