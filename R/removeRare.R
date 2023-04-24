#' Remove rare words
#'
#' @description This function takes the output from the scrubMe function and gets rid of words that appeared less than 20 times. I.e., gets rid of the most rare words, which are kind of like outliers since they don’t represent how most people feel. They can also be words that are unintelligible.
#'
#' @param corpus Object of type “list” where each each row corresponds to a single tweet, and the words used in each tweet have been already been cleaned.
#'
#' @return vector
#'
#' @export
#'
#' @examples removeRare(scrubMe(california_fifa))

removeRare <- function(corpus){
  words <- as.data.frame(sort(table(unlist(corpus)), decreasing = TRUE), stringsAsFactors = FALSE)
  words <- words$Var1[which(words$Freq >= 20)]
  return(words)
}
