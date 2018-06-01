# setwd('/Users/nanjiang/Dropbox/fraud')
# setwd("D:/Dropbox/fraud")
library(caret)
library(gbm)
library(data.table)
library(hashmap)
library(tfestimators)
##############################

source('DataProcessFunc.R')
data_process_result <- DataProcess('data/train.csv')

clean.data <- data_process_result$clean.data
fraud.record <- data_process_result$fraud.record

fwrite(clean.data, 'cleandata.csv')
save(fraud.record, file = 'fraudrecord.RData')

clean.data <- fread('cleandata.csv')
load(file = 'fraudrecord.RData')

##########
# data seperation
train.ind <- createDataPartition(clean.data$is.fraud, p = 0.9, list = F)
save(train.ind, file = 'trainind.RData')

load('trainind.RData')
train.data <- clean.data[train.ind, ]
test.data <- clean.data[-train.ind, ]




####################
# train data using tf
source('TFtrainFun.R')


model <- TrainModel(train.data, './model/', 1000)
tensorboard(launch_browser = T, log_dir = model_dir(model))

# source('DataFeed.R')
# eval.data <- train.data
# eval.data$weight <- ifelse(train.data$is.fraud == 'YES', 1, 200)


input_test.data <- test.data[1:1000, ]

test_data_feed <- function(data) {
    return(input_fn(
        data,
        features = c('ip', 'app', 'device', 'channel', 'time', 'fraud.score'),
        batch_size = 32
    ))
}

predict.func <- function(input_data){
    result <- predict(model, input_fn = test_data_feed(input_data), predict_keys = 'probabilities')
    y.pred <- unlist(result$probabilities, recursive = F)[seq(from = 1, by = 2, to = 2*length(result$probabilities))]
    return(y.pred)
}

prediction <- predict.func(test.data[2000:3000, ])
prediction.result <- kmeans(prediction, centers = 2)$cluster
prediction <- as.character(ifelse(prediction.result -1 , 'YES', 'NO'))

sum(prediction == 'NO')
ind <- (test.data[2000:3000,]$is.fraud == 'NO')

prediction[ind]

sum(prediction != test.data[2000:3000,]$is.fraud)

###################################
# train using random forest



