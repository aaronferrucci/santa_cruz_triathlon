library(ggplot2)
library(lubridate)

to_seconds <- function(t) {
  s <- as.integer(seconds(hms(t)))
  return(s)
}

process <- function(data) {
  # convert times to absolute seconds
  time_names <- c("Start", "Swim", "T1", "Bike", "T2", "Run", "USAT.Penalty", "Elapsed")
  for (name in time_names) {
    data[name] <- to_seconds(data[[name]])
  }

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

  # distances are swim: 1.5km, bike: 40km, run: 10km
  data$Start.d <- 0
  data$Swim.d <- 1
  data$T1.d <- data$Swim.d
  data$Bike.d <- data$T1.d + 1
  data$T2.d <- data$Bike.d
  data$Run.d <- data$T2.d + 1

  return(data)
}

read_data <- function(f) {
  data <- read.csv(f, sep='\t', stringsAsFactors = F)
  data$Division <- as.factor(data$Division)

  return(data)
}

time_fmt <- function(x) {
  hour <- x %/% 3600
  minute <- (xbreaks %% 3600) %/% 60
  second <- xbreaks %% 60
  return(sprintf("%d:%02d", hour, minute))
}

relay_2019 <- process(read_data("relay_2019.csv"))
xbreaks <- seq(to_seconds("8:30:00"), to_seconds("13:30:00"), to_seconds("0:30:00"))
p1 <- ggplot(relay_2019, aes(color=Team)) +
  scale_y_continuous(breaks=0.5 + seq(0, 2), labels=c("Swim", "Bike", "Run"), limits=c(0, NA)) +
  theme(axis.text.y = element_text(angle = 90, hjust=0.5)) +
  scale_x_continuous(breaks=xbreaks, labels=time_fmt(xbreaks)) +
  xlab("Time") + ylab("Segment") +
  facet_grid(Division ~ ., as.table = FALSE) +
  geom_segment(aes(x=Start, y=Start.d, xend=Swim, yend=Swim.d)) +
  geom_segment(aes(x=Swim, y=Swim.d, xend=T1, yend=T1.d), color="gray") +
  geom_segment(aes(x=T1, y=T1.d, xend=Bike, yend=Bike.d)) +
  geom_segment(aes(x=Bike, y=Bike.d, xend=T2, yend=T2.d), color="gray") +
  geom_segment(aes(x=T2, y=T2.d, xend=Run, yend=Run.d))
