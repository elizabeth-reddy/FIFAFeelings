#' Map plot of all Tweets given keyword
#'
#' @description This function lets the user select a word and will then plot the tweets containing that word on a map of California with the size of each point depending on the number of retweets each tweet got. This function is dependent on the ggplot2 package.
#'
#' @param word string indicating keyword for plotting
#'
#' @return NULL
#' @export
#'
#' @examples plotWord(word = "goal")
#' @import ggplot2

plotWord <- function(word) {
  sub.data <- california_fifa[which(grepl(word,california_fifa$full_text)),]

  ggplot2::ggplot() +
    ggplot2::borders(database="state",
            regions = "california",
            col = "gray95",
            fill = "gray90") +
    ggthemes::theme_map() +
    ggplot2::geom_point(data = sub.data, ggplot2::aes(x = lng, y = lat, size = retweet_count))
}
