#Classic Birthday Probability Problem
#By Michelle Habib
library(ggplot2)

#Calculates the probability of at least one birthday match
birthday <- function(k=23){
  no_birthday_match <- 1
  for(i in 365:(365-k+1)){
    no_birthday_match <- no_birthday_match*i
  }
  no_birthday_match <- no_birthday_match / 365**k
  return(1-no_birthday_match)
}

#Plots the probability of at least one birthday match as a function of k
birthday_plot <- function(k=100){
  bm <- 0
  K <- seq(1,k)
  for(i in 1:k)
    bm[i] <- birthday(i)
  df <- data.frame(bm, K)
  x <- ggplot(data = df) + geom_point(mapping = aes(x=K, y=bm), color = "blue")
  x <- x + labs(
    title = "Probability of at least one birthday match as a function of k",
    x = "k",
    y = "probability of birthday match"
  )
  return(x)
}