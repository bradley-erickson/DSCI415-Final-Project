# CS 415 Unsupervised Learning
# Dr. Brant Deppa
# Bradley Erickson
# William Henschell
# 12/10/2019
# Final Project


# Libraries
library(syuzhet)
library(dplyr)
library(wordcloud)
library(tm)


# Functions
## used to add additional columns to a data fram
cbind.all <- function (...) {
  nm <- list(...)
  nm <- lapply(nm, as.matrix)
  n <- max(sapply(nm, nrow))
  do.call(cbind, lapply(nm, function(x) rbind(x, matrix(, n - nrow(x), ncol(x)))))
}

## function from Dr. Deppa for cleaning text
clean.text <- function(some_txt){
  some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
  some_txt = gsub("@\\w+", "", some_txt)
  some_txt = gsub("[[:punct:]]", "", some_txt)
  some_txt = gsub("[[:digit:]]", "", some_txt)
  some_txt = gsub("http\\w+", "", some_txt)
  some_txt = gsub("[ \t]{2,}", "", some_txt)
  some_txt = gsub("^\\s+|\\s+$", "", some_txt)
  # define "tolower error handling" function
  try.tolower = function(x)
  {
    y = NA
    try_error = tryCatch(tolower(x), error=function(e) e)
    if (!inherits(try_error, "error"))
      y = tolower(x)
    return(y)
  }
  some_txt = sapply(some_txt, try.tolower)
  some_txt = some_txt[some_txt != ""]
  names(some_txt) = NULL
  return(some_txt)
}


# Seinfeld analysis
## data and other globals
show = read.csv(file.choose(), header=TRUE, sep=",", stringsAsFactors = FALSE)

show.title <- "Seinfeld"
seasons <- unique(show$Season)

## sentiment analysis
show.sentiment <- data.frame()
episodes.sentiment <- data.frame()
for (i in seasons) {
  season.data <- show %>% filter(Season==i)
  episodes <- unique(season.data$EpisodeNo)
  season.sentiment <- data.frame()
  title <- paste(show.title,": Season ",i," sentiment by episode")
  
  ### sentiment of episode plus season overall
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

### average sentiment of episode by season plus overall
title <- paste(show.title,": Sentiment by season")
plot(0,0, main=title, xlab = "Normalized Narrative Time", ylab = "Scaled Sentiment", type="n", xlim=c(-1,101), ylim=c(-1.01, 1.01))
for (i in seasons) {
  lines(show.sentiment[,i], col=i)
}
lines(rowMeans(episodes.sentiment), col="firebrick", lwd = 5)

## word clouds
### overall barplot
show.text.clean <- clean.text(show$Dialogue)
show.corpus.stop <- Corpus(VectorSource(show.text.clean))
show.corpus <- tm_map(show.corpus.stop, removeWords, stopwords("en"))
show.TDM <- TermDocumentMatrix(show.corpus)
show.matrix <- as.matrix(show.TDM)
word.freq <- sort(rowSums(show.matrix), decreasing=T)
barplot(word.freq[1:20], xlab="Frequent words", cex.names=0.7, las=2)

### per season
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

### per main character
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
  wordcloud(words=names(character.freq[1:100]), freq=character.freq[1:100], col=rainbow(1000), scale=c(3.5,0.25))
}

jerry.data <- show %>% filter(grepl("JERRY",Character))
jerry.text.clean <- clean.text(jerry.data$Dialogue)
jerry <- paste(jerry.text.clean, collapse = " ")

george.data <- show %>% filter(grepl("GEORGE",Character))
george.text.clean <- clean.text(george.data$Dialogue)
george <- paste(george.text.clean, collapse = " ")

elaine.data <- show %>% filter(grepl("ELAINE",Character))
elaine.text.clean <- clean.text(elaine.data$Dialogue)
elaine <- paste(elaine.text.clean, collapse = " ")

kramer.data <- show %>% filter(grepl("KRAMER",Character))
kramer.text.clean <- clean.text(kramer.data$Dialogue)
kramer <- paste(kramer.text.clean, collapse = " ")

group <- c(jerry, george, elaine, kramer)
group <- removeWords(group, stopwords("english"))

group.corpus <- Corpus(VectorSource(group))
group.TDM <- TermDocumentMatrix(group.corpus)
group.mat <- as.matrix(group.TDM)
colnames(group.mat) <- c("JERRY", "GEORGE", "ELAINE", "KRAMER")

comparison.cloud(group.mat, random.order=F, colors=c("blue", "red", "green", "purple"), scale=c(4,0.6), title.size=1, max.words=300)
