#'@title Quick Scatter Plot
#'@description Scatter plot with line of best fit
#'@param data a data frame
#'@param x a numeric variable
#'@param y a numeric variable
#'@returns a ggplot2 graph
#'@export
#'@import ggplot2
#'@examples
#'qscatter(mtcars, wt, mpg)

#Creating a function for the scatterplot
qscatter <- function(data, x, y) {
  require(ggplot2)
  xname <- as.character(substitute(x))
  yname <- as.character(substitute(y))

  reg <- cor.test(data[[xname]], data[[yname]])
  p <- format.pval(reg$p.value, digits = 3)
  cor <- format(reg$estimate, digits = 3)

  gtitle <- paste("Relationship Between", xname, "and", yname)
  ggplot(data=mtcars)+
    geom_point(aes(x = {{x}}, y = {{y}}))+
    geom_smooth(aes(x = {{x}}, y = {{y}}),
                method = "lm",
                formula = y~x, se = FALSE,
                color = "cornflowerblue")+
    theme_minimal()+
    labs(title = gtitle,
         subtitle = paste("r = ", cor,
                          ", p = ", p))

}




