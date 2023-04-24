#' Plot time series of word
#'
#' @description A function that plots the time series of a particular word when it was tweeted in the California FIFA dataset. Input is the word you want to examine. Output is the time series by hour of the number of tweets containing that word.
#'
#' @param word word to build time series
#'
#' @return NULL
#' @export
#'
#' @examples plotWordSeries("goal")

plotWordSeries <- function(word){

  sub_data <- california_fifa[which(grepl(word, california_fifa$full_text)), ]

  rtweet::ts_plot(sub_data, "hours") +
    ggplot2::theme_light() +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
    ggplot2::labs(
      x = NULL, y = NULL,
      title = "Time Series of Tweets Containing this Word by Hours")
}
