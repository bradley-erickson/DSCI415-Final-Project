# CS 415 Unsupervised Learning
# Dr. Brant Deppa
# Bradley Erickson
# William Henschell
# 12/06/2019
# Final Project


# Libraries
library(syuzhet)
library(dplyr)
library(wordcloud)
library(tm)
library(factoextra)
library(cluster)
library(stringr)

# functions
cbind.all <- function (...) {
  nm <- list(...)
  nm <- lapply(nm, as.matrix)
  n <- max(sapply(nm, nrow))
  do.call(cbind, lapply(nm, function(x) rbind(x, matrix(, n - nrow(x), ncol(x)))))
}


# Seinfeld
show = read.csv(file.choose(), header=TRUE, sep=",", stringsAsFactors = FALSE)

show.title <- "Seinfeld"
seasons <- unique(show$Season)
characters <- unique(show$Character)

# sentiment analysis
show.sentiment <- data.frame()
episodes.sentiment <- data.frame()
for (i in seasons) {
  season.data <- show %>% filter(Season==i)
  episodes <- unique(season.data$EpisodeNo)
  season.sentiment <- data.frame()
  title <- paste(show.title,": Season ",i," sentiment by episode")
  plot(0,0, main=title, xlab = "Normalized Narrative Time", ylab = "Scaled Sentiment", type="n", xlim=c(-1,101), ylim=c(-1.01, 1.01))
  for (j in episodes) {
    episode.data <- season.data %>% filter(EpisodeNo==j)
    episode.sentences <- get_sentences(episode.data$Dialogue)
    episode.sentiment <- get_sentiment(episode.sentences)
    episode.sentiment.values <- get_dct_transform(episode.sentiment, low_pass_size = 5, x_reverse_len = 100, scale_vals = F, scale_range = T)
    season.sentiment <- cbind.all(season.sentiment, episode.sentiment.values)
    episodes.sentiment <- cbind.all(episodes.sentiment, episode.sentiment.values)
    lines(episode.sentiment.values)
  }
  season.sentiment.means <- rowMeans(season.sentiment)
  lines(season.sentiment.means, col="firebrick", lwd = 5)
  show.sentiment <- cbind.all(show.sentiment, season.sentiment.means)
  Sys.sleep(1)
}

title <- paste(show.title,": Sentiment by season")
plot(0,0, main=title, xlab = "Normalized Narrative Time", ylab = "Scaled Sentiment", type="n", xlim=c(-1,101), ylim=c(-1.01, 1.01))
for (i in seasons) {
  lines(show.sentiment[,i], col=i)
}
lines(rowMeans(episodes.sentiment), col="firebrick", lwd = 5)


# word clouds
show.text.clean <- clean.text(show$Dialogue)
show.corpus.stop <- Corpus(VectorSource(show.text.clean))
show.corpus <- tm_map(show.corpus.stop, removeWords, stopwords("en"))
show.TDM <- TermDocumentMatrix(show.corpus)
show.matrix <- as.matrix(show.TDM)
word.freq <- sort(rowSums(show.matrix), decreasing=T)
barplot(word.freq[1:20], xlab="Frequent words", cex.names=0.7, las=2)

for (i in seasons) {
  season.data <- show %>% filter(Season==i)
  season.text.clean <- clean.text(season.data$Dialogue)
  season.corpus.stop <- Corpus(VectorSource(season.text.clean))
  season.corpus <- tm_map(season.corpus.stop, removeWords, stopwords("en"))
  season.TDM <- TermDocumentMatrix(season.corpus)
  season.matrix <- as.matrix(season.TDM)
  season.freq <- sort(rowSums(season.matrix), decreasing=T)
  title <- paste(show.title,": Season ",i," top 20 most used words")
  #barplot(season.freq[1:20], main=title, las=2)
  wordcloud(words=names(season.freq[1:100]), freq=season.freq[1:100], col=rainbow(1000))
}

main.characters <- c("JERRY", "GEORGE", "ELAINE", "KRAMER")
for (i in main.characters) {
  character.data <- show %>% filter(grepl(i,Character))
  character.text.clean <- clean.text(character.data$Dialogue)
  character.corpus.stop <- Corpus(VectorSource(character.text.clean))
  character.corpus <- tm_map(character.corpus.stop, removeWords, stopwords("en"))
  character.TDM <- TermDocumentMatrix(character.corpus)
  character.matrix <- as.matrix(character.TDM)
  character.freq <- rowSums(character.matrix)
  title <- paste(show.title,": ",i," top 20 most used words")
  #barplot(character.freq[1:20], main=title, las=2)
  max.word <- max(character.freq)
  character.freq[i] <- max.word*1.25
  character.freq <- sort(character.freq, decreasing=T)
  wordcloud(words=names(character.freq[1:100]), freq=character.freq[1:100], col=rainbow(1000))
}


# Pokemon cluster analysis
pokemon = read.csv(file.choose(), header=TRUE, sep=",", stringsAsFactors = FALSE)
pokemon <- pokemon %>% mutate(type = paste(type1, type2) %>% str_split(., ' ') %>% lapply(., 'sort') %>%  lapply(., 'paste', collapse=' ') %>% unlist(.))
pokemon <- pokemon %>% mutate(name.type = paste(type, name))
pokemon <- pokemon[1:151,]
rownames(pokemon) <- pokemon$name.type
pokemon.mat <- pokemon[2:19]

pokemon.dist <- dist(pokemon.mat)
pokemon.hclust <- hclust(pokemon.dist, method="ward.D")
plot(pokemon.hclust)


fviz_nbclust(pokemon.mat,kmeans,k.max=25,method="wss")
fviz_nbclust(pokemon.mat,kmeans,k.max=25,method="silhouette")
fviz_nbclust(pokemon.mat,kmeans,k.max=25,method="gap_stat")
k = 18
shooby = kmeans(pokemon.mat,k)
table(shooby$cluster,pokemon$type1)

fviz_dend(pokemon.hclust, k=k)
fviz_cluster(shooby, data=pokemon.mat, geom="point")
