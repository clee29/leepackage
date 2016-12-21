

#' Square a number
#' Takes a numeric number and square it
#' @param x a numeric to be squared
#' @return The square of the input
#' @export

sq <- function(x)
{
  return(x^2)
}

#' Takes the data and provides a summary of Time of Arrival for each class
#'
#' @param x a dataset: BST 411, BST 464, or BST 494
#' @return Mean, Median, and Standard Deviation of the Time of Arrival for each class. 1st row = mean, 2nd row = median, 3rd row = sd
#' @export

TimeArr <- function(x)
{
 return(x <- as.matrix(c(mean(x$Time.of.Arrival), median(x$Time.of.Arrival), sd(x$Time.of.Arrival))))
}

#' Takes the data and provides a summary of Time of Departure for each class
#'
#' @param x a dataset: BST 411, BST 464, or BST 494
#' @return Mean, Median, and Standard Deviation of the Time of Departure for each class. 1st row = mean, 2nd row = median, 3rd row = sd
#' @export

TimeDep <- function(x)
{
  return(x <- as.matrix(c(mean(x$Time.of.Departure), median(x$Time.of.Departure), sd(x$Time.of.Departure))))
}

#' Creates boxplots of Time of Arrival for each class
#'
#' @param x a dataset: BST 411, BST 464, or BST 494
#' @return Creates a boxplot of Time of Arrival for BST 411, BST 464, or BST 494
#' @export

plotArr <- function(x)
{
  return(boxplot(x$Time.of.Arrival, main='Time of Arrival', ylab='Time'))
}

#' Creates boxplots of Time of Departure for each class
#'
#' @param x a dataset: BST 411, BST 464, or BST 494
#' @return Creates a boxplot of Time of Departure for BST 411, BST 464, or BST 494
#' @export

plotDep <- function(x)
{
  return(boxplot(x$Time.of.Departure,  main='Time of Departure', ylab='Time'))
}







