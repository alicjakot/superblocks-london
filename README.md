# superblocks-london

This github respository contains code for street network analysis in London based on centrality metrics, to investigate the potential implementation of the superblocks model. The analysis is based on open data so the procedure could be applied to other cities. 

The files are structured as follows:

**1)** downloading_street_network.ipynb
Getting street network of London with OSMnx package and saving it to shp to work in R

**2)** network_construction_and_cleaning.R
Constrauction of the network and check-up

**3)** betweenness_and_closeness.R
The calculation of betweenness and closeness centralities

**4)** poi_data_and_eigenvector.R
exploration and filtering of POI data, the calculation of eigenvector centrality

**5)** visualising_centralities.R
code for producing maps of centralities

**6)** clustering.R
K-means clustering based on centralities metrics

Data was extracted from the following sources:
ULEZ21 boundary - https://data.london.gov.uk/dataset/ultra_low_emissions_zone_expansion_new
POI - Ordnance Survey - available upon request (can be skipped if not calculating weighted eigenvector)



