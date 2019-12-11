# CS 415 Unsupervised Learning
# Dr. Brant Deppa
# Bradley Erickson
# William Henschell
# 12/10/2019
# Final Project


# Libraries
library(factoextra)
library(cluster)
library(stringr)


# Functions


# Pokemon analysis
## data and other globals
pokemon = read.csv(file.choose(), header=TRUE, sep=",", stringsAsFactors = FALSE)
pokemon <- pokemon %>% mutate(type = paste(type1, type2) %>% str_split(., ' ') %>% lapply(., 'sort') %>%  lapply(., 'paste', collapse=' ') %>% unlist(.))
pokemon <- pokemon %>% mutate(name.type = paste(type, name))
pokemon <- pokemon[1:151,]
rownames(pokemon) <- pokemon$name.type

## cluster analysis
pokemon.mat <- pokemon[2:19]

pokemon.dist <- dist(pokemon.mat)
pokemon.hclust <- hclust(pokemon.dist, method="ward.D")
plot(pokemon.hclust)

fviz_nbclust(pokemon.mat,kmeans,k.max=25,method="wss")
k = 18
pokemon.kmeans = kmeans(pokemon.mat,k)
table(pokemon.kmeans$cluster,pokemon$type1)

fviz_dend(pokemon.hclust, k=k)
fviz_cluster(pokemon.kmeans, data=pokemon.mat, geom="point")