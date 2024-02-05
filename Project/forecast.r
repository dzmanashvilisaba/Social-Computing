install.packages('forecast')
library(forecast)
library(dplyr)
library(ggplot2)

data <- read.csv("C:\\Users\\Saba\\Desktop\\Wanker\\8th Semester\\Computational Social Science\\Data\\IndEmp.csv",header=TRUE, stringsAsFactors=FALSE)

data <- data[,2:9]

datacol = ncol(data) 

columnss <- colnames(data)

columns <- c()
for (string in columnss) {
  string <- chartr('.', " ", string)
  print(string)
  columns <- c(columns, string)
}

rm(columnss)

pred30 <- data.frame(2021:2030)
colnames(pred30) <- "Year"

i <- 1
while (i <= datacol) {
  tsdata <- ts(data[,i]/10000,start = 1991, end = 2020, frequency = 1)
  am <- auto.arima(tsdata)
  fc <- forecast(am, h=10)
  jpeg(paste(columns[i],".jpeg"), quality = 100, height = 480, width = 720)
  set.seed(i)
  plot(fc, shadecols = "oldstyle", main = paste("Forecast for", columns[i]),
       xlab='Time',ylab="Number of People (in Ten Thousands)")
  ddd <- data.frame(fc)
  pred30 <- cbind(pred30,data.frame(ddd$Point.Forecast))
  colnames(pred30)[i+1] <- columns[i]
  dev.off()
  i = i + 1
}

pred30
