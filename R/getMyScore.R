#' Build sentiment scores from documents
#'
#' @description This function calculates a sentiment score (number of positive word matches - number of negative word matches) from a collection of words. A positive sentiment score means more positive matches were detected than negative ones. A negative sentiment score means more negative matches were detected than positive ones. Special note: To get a sentiment score for each tweet (which could later be used to create a histogram of the frequency of sentiment scores), use sapply(a, b) where a is your output from scrubMe and b is the getMyScore function. To get a sentiment score for the whole collection of words/dictionary without paying attention to which tweet it corresponds to, just use getMyScore() where the input is the output from removeRare.
#'
#' @param words corpus of words to analyze
#'
#' @return numeric
#' @export
#'
#' @examples getMyScore(removeRare(scrubMe(california_fifa)))

getMyScore <- function(words){
  pos_matches <- match(words, pos_words)
  neg_matches <- match(words, neg_words)
  pos_matches <- !is.na(pos_matches)
  neg_matches <- !is.na(neg_matches)
  score <- sum(pos_matches) - sum(neg_matches)
  return(score)
}
