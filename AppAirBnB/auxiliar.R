data <- read.csv("./data/calendar.csv")
data$price <- lapply(data$price, substring, first=2) # Delete $
data$price <- lapply(data$price, str_remove_all, pattern=",") # Delete ,
data$price <- as.double(data$price)

#data <-data[complete.cases(data), ]
df <- aggregate(data$price, list(data$date), FUN=mean, na.rm=TRUE)
df$date <- as.Date(df$Group.1)
df$price <- as.numeric(df$x)
df <- df[,c("date", "price")]
write.csv(df, "./data/calendarMeanPrice.csv")
