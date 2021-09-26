library(ggplot2)
library(reshape)
library(lubridate)

to_seconds <- function(t) {
  s <- as.integer(seconds(hms(t)))
  return(s)
}

process <- function(data) {
  # start time - not interesting?
  data <- subset(data, select = -c(Start.Time, Chip.Elapsed))

  time_names <- c("Swim", "T1", "Bike", "T2", "Run")

  for (name in time_names) {
    data[name] <- to_seconds(data[[name]])
  }

  # Some data shows 0 for some of the segments; no-show or untidy data? Omit.
  data <- data[data$Swim > 0 & data$Bike > 0 & data$Run > 0,]

  return(data)
}

read_data <- function(f) {
  data <- read.csv(f, sep='\t', stringsAsFactors = F)
  data$Division <- factor(data$Division)
  data$Team.Name <- factor(data$Team.Name)
  # order the team name according to division place, so the bar plot comes out
  # in ascending order, left to right. Feels like an unseemly blending of data
  # and presentation, but it's easy.
  data$Team.Name <- reorder(data$Team.Name, data$Div.Place)

  return(data)
}

time_fmt <- function(x) {
  hour <- x %/% 3600
  minute <- (x %% 3600) %/% 60
  second <- x %% 60
  return(sprintf("%d:%02d", hour, minute))
}

raw <- process(read_data("relay_2021_mixed.csv"))
# reswizzle the data for stacked barplot
relay_2021 <- melt(raw, id=c("Bib", "Team.Name", "City", "Division", "Div.Place"))

# reorder the phases so that they're stacked up in time
relay_2021$variable <- factor(relay_2021$variable, rev(c("Swim", "T1", "Bike", "T2", "Run")))

ybreaks <- seq(to_seconds("0:00:00"), to_seconds("4:00:00"), to_seconds("0:30:00"))

p <- ggplot(relay_2021, aes(fill=variable, y=value, x=Team.Name)) +
  theme(axis.text.x = element_text(angle = 90, hjust=0.95, vjust=0.4)) +
  xlab("Team") + ylab("Time (H:MM)") +
  scale_y_continuous(breaks=ybreaks, labels=time_fmt(ybreaks)) +
  geom_bar(position="stack", stat="identity")

# xbreaks <- seq(to_seconds("8:30:00"), to_seconds("13:30:00"), to_seconds("0:30:00"))
# p1 <- ggplot(relay_2019, aes(color=Team)) +
#   scale_y_continuous(breaks=0.5 + seq(0, 2), labels=c("Swim", "Bike", "Run"), limits=c(0, NA)) +
#   theme(axis.text.y = element_text(angle = 90, hjust=0.5)) +
#   scale_x_continuous(breaks=xbreaks, labels=time_fmt(xbreaks)) +
#   xlab("Time") + ylab("Segment") +
#   facet_grid(Division ~ ., as.table = FALSE) +
#   geom_segment(aes(x=Start, y=Start.d, xend=Swim, yend=Swim.d)) +
#   geom_segment(aes(x=Swim, y=Swim.d, xend=T1, yend=T1.d), color="gray") +
#   geom_segment(aes(x=T1, y=T1.d, xend=Bike, yend=Bike.d)) +
#   geom_segment(aes(x=Bike, y=Bike.d, xend=T2, yend=T2.d), color="gray") +
#   geom_segment(aes(x=T2, y=T2.d, xend=Run, yend=Run.d))
