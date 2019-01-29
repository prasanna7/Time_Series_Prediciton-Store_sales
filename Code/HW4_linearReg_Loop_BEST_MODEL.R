library(lubridate)
library(Metrics)
library(dplyr)
library(data.table)
# Loading in data

setwd('C:/Users/henri/Google Drive/School/2018-2019 Carlson MSBA/2018 Predictive Analytics 6420/HW4/')
train <- fread('train.csv')

train <- train %>%
  select(-SchoolHoliday, -StateHoliday, -Customers) %>%
  mutate(Date = as.Date(train$Date),
         Month = factor(month(train$Date)),
         Year = factor(year(train$Date)),
         DayOfWeek = factor(DayOfWeek),
         Promo = factor(Promo)) %>%
  group_by(Store) %>%
  mutate(Trend = row_number()) %>%
  ungroup()

test <- fread('test.csv')

test <- test %>%
  select(-SchoolHoliday, -StateHoliday) %>%
  mutate(Date = as.Date(test$Date),
         Month = factor(month(test$Date)),
         Year = factor(year(test$Date)),
         DayOfWeek = factor(DayOfWeek),
         Open = ifelse(is.na(Open),0,Open),   # IMPORTANT, THERE WERE STORE REMODELS AND THIS LED TO NA VALUES
         Promo = factor(Promo)) %>%
  group_by(Store) %>%
  mutate(Trend = row_number()) %>%
  ungroup()

# check to make sure everyting lines up
colnames(test)
colnames(train)

# initialize submission
Id <- c()
Sales <- c()

retailers <- table(test$Store)
length(retailers)
retailers <- names(retailers)

# define prediction submission function
for (i in retailers){
  
  cat(c('Store number:',i,'\n'))
  
  store_train <- train %>% filter(Store==i)
  store_test <- test %>% filter(Store==i)
  
  Id <- c(Id,store_test$Id)
  
  fit <- lm(formula = Sales ~ Trend + Promo +
              Year + Month + DayOfWeek, data = store_train[store_train$Open==1,])
  
  pred <- rep(0,nrow(store_test))
  pred[store_test$Open==1] <- predict(fit, store_test[store_test$Open==1,])
  Sales <- c(Sales,pred)
  
}


submission <- data.frame(Id, Sales) %>% arrange(Id)
write.csv(submission, 'HW4_Submission_Group3_6420.csv', row.names = F)

