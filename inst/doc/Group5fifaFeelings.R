## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(fifaFeelings)
library(ggplot2)
library(ggthemes)
library(rtweet)
library(wordcloud2)
library(corpus)
library(purrr)
library(tidyverse)
library(dplyr)
library(stringr)
library(plotly)

## -----------------------------------------------------------------------------

plotWord <- function(word) {
  sub.data <- california_fifa[which(grepl(word,california_fifa$full_text)),]
  
  ggplot() + 
    borders(database="state",
            regions = "california",
            col = "gray95", 
            fill = "gray90") +
    theme_map() + 
    geom_point(data = sub.data, aes(x = lng, y = lat, size = retweet_count))
}

## EXAMPLE
plotWord(word = "goal")


## ---- fig.show='hold'---------------------------------------------------------

# FIFA 2022 color palette
fifaPalette <- c("#1077C3", "#49BCE3", "#FEC310", "#56042C")


## -----------------------------------------------------------------------------
# Function to clean tweets
scrubMe <- function(data){
  corpus <- data$full_text
  corpus <- purrr::map(corpus, function(x) str_split(tolower(x),"\\s+") %>% unlist) #tokenization
  corpus <- purrr::map(corpus, function(x) gsub("[^a-z]","",x))   #remove anything except letters
  corpus <- purrr::map(corpus, function(x) x[!(x %in% stopwords::stopwords("en"))]) #Remove stop-words ("the", "in", etc.)
  corpus <- purrr::map(corpus, function(x) text_tokens(x, stemmer="en") %>% unlist) #stem
  return(corpus)
}

# Test & function to remove rare words
clean_test <- scrubMe(california_fifa)

## -----------------------------------------------------------------------------
# Function to remove rare words
removeRare <- function(corpus){
  
  words <- as.data.frame(sort(table(unlist(corpus)), decreasing = TRUE), stringsAsFactors = FALSE)
  words <- words$Var1[which(words$Freq >= 20)]
  
  return(words)
}

# Test
rare_test <- removeRare(clean_test)


## -----------------------------------------------------------------------------
# Function to create document term matrix
get_dtm <- function(data, dict){
  corpus <- data$full_text
  
  # Cleaning
  corpus <- purrr::map(corpus, function(x) str_split(tolower(x),"\\s+") %>% unlist) 
  corpus <- purrr::map(corpus, function(x) gsub("[^a-z]","",x))
  corpus <- purrr::map(corpus, function(x) x[!(x %in% stopwords::stopwords("en"))])
  corpus <- purrr::map(corpus, function(x) text_tokens(x, stemmer="en") %>% unlist)
  
  corpus <- purrr::map(corpus, function(x) x[x %in% dict]) # keep only uncommon words from above

  dtm <- as.data.frame(matrix(0L, nrow=nrow(data), ncol=length(dict)))
  names(dtm) <- dict
  
  freq <- purrr::map(corpus, table)
  for (i in 1:nrow(dtm)){
    dtm[i, names(freq[[i]])] <- unname(freq[[i]])
  }
  
  return(dtm)
}

# Test
test_dtm <- get_dtm(california_fifa, rare_test)

## -----------------------------------------------------------------------------
buildMyFifaCloud <- function(dtm){
  
  df <- data.frame(word = names(dtm), freq = colSums(dtm))
  df <- df[rev(order(df$freq)), ]
  cloud <- wordcloud2(head(df, 100), size = 3, color = rep_len(fifaPalette, 100))
  
  return(cloud)
}

# Test
buildMyFifaCloud(test_dtm)

## -----------------------------------------------------------------------------
#pos words are those revolving something positive
#neg words are those with a negative, or known as controversial 

# Sentiment score for a single tweet/treating tweets as if it were a single text
getMyScore <- function(words){
  pos_matches <- match(words, pos_words)
  neg_matches <- match(words, neg_words)
  pos_matches <- !is.na(pos_matches)
  neg_matches <- !is.na(neg_matches)
  score <- sum(pos_matches) - sum(neg_matches)
  return(score)
}

# Test
getMyScore(rare_test)

# Another test; Calculate score for each tweet
test_score <- sapply(clean_test, getMyScore)


## -----------------------------------------------------------------------------
# Function to build a FIFA-themed histogram of sentiment scores, no pre-processing necessary
tempCheck <- function(data){
  clean_test <- scrubMe(data)
  rare_test <- removeRare(clean_test)
  test_dtm <- get_dtm(data, rare_test)
  test_score <- sapply(clean_test, getMyScore)
  tweets_hist <- plotly::plot_ly(x = ~test_score, type = "histogram",
                         marker = list(color = fifaPalette[4],
                                       line = list(color = fifaPalette[2],
                                                   width = 2))) %>% 
    plotly::layout(title = "Frequency distribution of sentiment scores",
           yaxis = list(title = "Frequency",
                        zeroline = FALSE),
           xaxis = list(title = "Sentiment score",
                        zeroline = FALSE))
  tweets_hist
}

# Test
tempCheck(california_fifa)



## -----------------------------------------------------------------------------


# Function to generate a time series plot of a given word of california_fifa data
plotWordSeries <- function(word){
  
  sub_data <- california_fifa[which(grepl(word, california_fifa$full_text)), ]
  
  ts_plot(sub_data, "hours") + 
    ggplot2::theme_light() +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
    ggplot2::labs(
      x = NULL, y = NULL,
      title = "Time Series of Tweets Containing this Word by Hours")
}


# Test
plotWordSeries("goal")



