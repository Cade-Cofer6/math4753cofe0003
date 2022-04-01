#' @title project1 function
#'
#' @param df data frame passed
#' @param x Length
#' @param y weight
#' @param cond Specified condition
#' @param col What variable to use as the color decider
#' @param species filters by given species
#'
#' @return a gg plot of length vs width
#' @export
#'
#' @examples
#' \dontrun {myddt2(df = ddt, x = "LENGTH", y= "WEIGHT",cond = LENGTH > 30, col = "RIVER", species = "CCATFISH")}
myddt2 <- function(df, x, y, cond, col, species){#Varibles needed to be passed

  df#Prints out data frame before subsetting

  df1 <- df %>% filter(grepl(species, ddt$SPECIES))#Filters the Data frame to onyl accept the given speceies

  df1#Prints out subsetted data frame

  result = paste("\\Users\\Cade Cofer\\OneDrive\\Desktop\\Stats\\Project1\\LvsWfor",species, sep = "")#appends the passed species to the file location/name to write the propper csv file


  write.csv(df1,result, row.names = FALSE)#writes the csv file

  g <- ggplot(df1, aes_string(x=x,y=y)) + # Note the use of aes_string
    geom_point(aes_string(color = col )) +
    geom_smooth(formula = y~x +I(x^2), method = "lm")
  g <- g + labs(title = "Cade Cofer")#adds my name to the title of the plot
  print(g)
  head(df1)

}
