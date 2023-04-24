#' Build FIFA-themed word cloud from a Document Term Matrix
#'
#' @description This function takes the document term matrix (the output from get_dtm function) and builds a FIFA-themed word cloud of the top 100-most frequently used words
#'
#' @param dtm document term matrix (the output from get_dtm function)
#'
#' @return NULL
#' @export
#'
#' @examples buildMyFifaCloud(get_dtm(california_fifa, removeRare(scrubMe(california_fifa)))
#' @import wordcloud2
buildMyFifaCloud <- function(dtm){

  df <- data.frame(word = names(dtm), freq = colSums(dtm))
  df <- df[rev(order(df$freq)), ]
  cloud <- wordcloud2::wordcloud2(head(df, 100), size = 3, color = rep_len(fifaPalette, 100))

  return(cloud)
}
