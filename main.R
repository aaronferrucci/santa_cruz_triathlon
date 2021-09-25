library(ggplot2)
library(lubridate)

to_seconds <- function(t) {
  s <- seconds(hms(t))
  return(s)
}

process <- function(data) {
  # convert times to absolute seconds
  time_names <- c("Start", "Swim", "T1", "Bike", "T2", "Run", "USAT.Penalty", "Elapsed")
  data[time_names] <- sapply(data[time_names], to_seconds)

  # Elapsed=0 for some teams; I assume that means non-completion. Drop those entries
  data <- data[data$Elapsed > 0,]

  # each value is the end of its interval
  # there's probably some 'clever' way to do these telescoping sums
  data$Swim <- data$Swim + data$Start
  data$T1 <- data$T1 + data$Swim
  data$Bike <- data$Bike + data$T1
  data$T2 <- data$T2 + data$Bike
  data$Run <- data$Run + data$T2
  data$End <- data$Run + data$USAT.Penalty
  return(data)
}

read_data <- function(f) {
  data <- read.csv(f, sep='\t', stringsAsFactors = T)
  data$Division <- as.factor(data$Division)

  return(data)
}

relay_2019 <- process(read_data("relay_2019.csv"))
