
library(lubridate)
to_seconds <- function(t) {
  s <- seconds(hms(t))
  return(s)
}

read_data <- function(f) {
  data <- read.csv(f, sep='\t', stringsAsFactors = T)
  data$Division <- as.factor(data$Division)

  # convert times to absolute seconds
  time_names <- c("Start", "Swim", "T1", "Bike", "T2", "Run", "USAT.Penalty", "Elapsed")
  data[time_names] <-sapply(data[time_names], to_seconds)

  # Elapsed=0 for some teams; I assume that means non-completion. Drop those entries
  data <- data[data$Elapsed > 0,]

  # What to do with non-zero USAT.Penalty?

  # each value except for "Start" is the end of its interval
  return(data)
}

relay_2019 <- read_data("relay_2019.csv")
