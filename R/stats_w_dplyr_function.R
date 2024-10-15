#'@title Summary Statistics
#'
#'@description Calculate simple statistics by group level
#'
#'@param x data a data frame
#'@param x x a numeric variable
#'@param ... one or more categorical variables
#'@returns a tibble with n, mean, and standard deviation
#'@export
#'@import dplyr
#'
#'@examples
#'stats(mtcars, x, am)


stats <- function(data, x, ...){
  require(dplyr)

  data %>%
    group_by(...) %>%
    summarize(n = n(),
              mean = mean({{x}}),
              sd = sd({{x}}))
}



