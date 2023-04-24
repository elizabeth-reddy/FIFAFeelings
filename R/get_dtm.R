#' Create document term matrix
#'
#' @description This function takes as inputs the original dataframe of tweets and the output from removeRare to create a document-term matrix
#'
#' @param data original dataframe of tweets
#' @param dict output from removeRare
#'
#' @return matrix
#' @export
#'
#' @examples get_dtm(california_fifa, removeRare(scrubMe(california_fifa)))
#' @import purrr
#' @import tidyverse
#' @import stringr
#' @import corpus
get_dtm <- function(data, dict){
  corpus <- data$full_text

  # Cleaning
  corpus <- purrr::map(corpus, function(x) stringr::str_split(tolower(x),"\\s+") %>% unlist)
  corpus <- purrr::map(corpus, function(x) gsub("[^a-z]","",x))
  corpus <- purrr::map(corpus, function(x) x[!(x %in% stopwords::stopwords("en"))])
  corpus <- purrr::map(corpus, function(x) corpus::text_tokens(x, stemmer="en") %>% unlist)

  corpus <- purrr::map(corpus, function(x) x[x %in% dict]) # keep only uncommon words from above

  dtm <- as.data.frame(matrix(0L, nrow=nrow(data), ncol=length(dict)))
  names(dtm) <- dict

  freq <- purrr::map(corpus, table)
  for (i in 1:nrow(dtm)){
    dtm[i, names(freq[[i]])] <- unname(freq[[i]])
  }

  return(dtm)
}
