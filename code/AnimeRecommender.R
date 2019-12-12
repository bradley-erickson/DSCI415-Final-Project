### IMPORTS ###

library(recommenderlab)
library(proxy)
library(class)
library(data.table)

## DATA CLEANING
#Load data (Change filepath to Anime.RData location)
load('Anime.RData')

#Loads in TV Ratings data
tv.full = read.csv(file.choose())

#tv without user_id
tv = tv.full[,-c(1,2)]

#Creating Real Ratings Matrix
tv.mat <- as(tv, 'matrix')
tv.mat <- as(tv.mat, 'realRatingMatrix')
dim(tv.mat)


## MODEL SELECTION EVALUATION

#Testing all recommenders
explore.scheme = evaluationScheme(tv.mat, method= 'split', train = 0.85, k = 1, given = 1, goodRating = 7)

#Setting algorithms to test
explore.algorithms <- list(
  'random items' = list(name = 'RANDOM', param = list(normalize = 'Z-score')),
  'popular items' = list(name = 'POPULAR', param = list(normalize = 'Z-score')),
  'user-based CF' = list(name = 'UBCF', param = list(normalize = 'Z-score', method = 'Cosine', nn=50)),
  'item-based CF' = list(name = 'IBCF', param = list(normalize = 'Z-score'))
  )

#Evaluating all algorithms
explore.results = evaluate(explore.scheme, explore.algortihms, n=c(1, 3, 5, 10, 15, 20))

#Plotting results for comparison
plot(explore.results, annotate = 1:4, legend = 'topleft')
plot(explore.results, 'prec/rec', annotate = 2:3)


## FINAL RECOMMENDER MODEL
#Creating test and training data for final model
tv.scheme = evaluationScheme(tv.mat, method= 'split', train = 0.666666, given = 1, goodRating = 7)

#Fitting the popular items recommender on the training data
tv.pop.rec = Recommender(getData(tv.scheme, 'train'), method = 'POPULAR')

#Creating top 10 list for all users
new.users.top10 = predict(tv.pop.rec, getData(tv.scheme, 'known'), n=10)
as(new.users.top10, 'list')

#Creating top 10 list for sample of 10
sample.new.users.top10 = predict(tv.pop.rec, getData(tv.scheme, 'known')[1:10,], n=10)
sample.new.users.top10.list <- as(sample.new.users.top10, 'list')
sample.new.users.top10.list

#Creating and cleaning a string version of the list for the sample of 10
sample.new.users.top10.string <- as.character(sample.new.users.top10.list)
sample.new.users.top10.string <- gsub('c\\(', '', sample.new.users.top10.string)
sample.new.users.top10.string <- gsub('\\)', '', sample.new.users.top10.string)

#Printing the counts of anime TV series that appear in the top 10 list for 10 users
sample.new.users.top10.list.split <- unlist(strsplit(sample.new.users.top10.string, split = ','))
sample.new.users.top10.list.split.dt <- data.table(sample.new.users.top10.list.split)
colnames(sample.new.users.top10.list.split.dt)[colnames(sample.new.users.top10.list.split.dt)=="sample.new.users.top10.list.split"] <- "anime_title"
sample.new.users.top10.anime.counts <- sample.new.users.top10.list.split.dt[, .(count = .N), by = sample.new.users.top10.list.split.dt[,1]]
print(sample.new.users.top10.anime.counts)
