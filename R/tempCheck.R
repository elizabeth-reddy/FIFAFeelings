#' FIFA themed sentiment histogram
#'
#' @description Quickly build a FIFA-themed histogram of sentiment scores without needing to do any prior pre-processing. Just input your raw dataframe collected using rtweet. Like above, the column name of the text of your tweets must be called “full_text”.
#'
#' @param data raw dataframe collected using rtweet (column name of the text of your tweets must be called “full_text”)
#'
#' @return NULL
#' @export
#'
#' @examples tempCheck(california_fifa)
#' @import tidyverse
#' @import plotly
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
