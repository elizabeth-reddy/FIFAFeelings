#' Clean tweets
#' @description This function performs all basic cleaning functions: tokenization, remove punctuation and special characters, remove English stop words like “the”, and stems words using bag-of-words methodology which is the most simple way to look at words as if they have no grammar. Each word is treated independently of its grammar and the way in which it is used. Special note: The column name of the dataframe that contains the scraped tweets must be called “full_text” (so that the whole column that contains the text of the tweets is dataframe$full_text). If it is called anything else, you will need to rename the column with the text from each tweet to be “full_text”.
#'
#' @param data The original dataframe of tweets that was collected using the rtweets package. Can only guarantee that this function works for tweets collected using the rtweets format, not by using any other method of tweet collection.
#'
#' @return list
#' @export
#'
#' @examples scrubMe(california_fifa)
#' @import purrr
#' @import tidyverse
#' @import stringr
#' @import corpus
scrubMe <- function(data){
  library(tidyverse)
  corpus <- data$full_text
  corpus <- purrr::map(corpus, function(x) stringr::str_split(tolower(x),"\\s+") %>% unlist) #tokenization
  corpus <- purrr::map(corpus, function(x) gsub("[^a-z]","",x))   #remove anything except letters
  corpus <- purrr::map(corpus, function(x) x[!(x %in% stopwords::stopwords("en"))]) #Remove stop-words ("the", "in", etc.)
  corpus <- purrr::map(corpus, function(x) corpus::text_tokens(x, stemmer="en") %>% unlist) #stem
  return(corpus)
}
