library(Metrics)
library(dplyr)
library(data.table)
library(forecast)
library(lubridate)
# Loading in data

setwd('C:/Users/henri/Google Drive/School/2018-2019 Carlson MSBA/2018 Predictive Analytics 6420/HW4/')
train <- fread('train.csv')

store_x <- train %>% filter(Store == 1)

train_train <- store_x[1:900,]
train_test <- store_x[901:942,]

test <- fread('test.csv')

# http://www.dbenson.co.uk/Rparts/subpages/forecastR/

fit <- msts(train_train$Sales, 
            seasonal.periods = c(7,365.25),
            start = decimal_date(as.Date("2013-02-12")))

plot(fit, main='Daily Sales', xlab='day', ylab='Euros')

fit2 <- tbats(fit)
plot(fit2, main="Multiple Season Decomposition")

pred <- as.numeric(predict(fit2,h=42)$mean)
pred[pred < 0] <- 0

rmse(train_test$Sales, pred)

submission <- data.frame(Id, Sales) %>% arrange(Id)
write.csv(submission, 'HW4_Submission_Group3_6420.csv', row.names = F)

