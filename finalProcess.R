library(data.table)
library(tfestimators)
sub.clean.data <- fread('subleandata.csv')

feature_cols <- feature_columns(
    column_numeric("ip"),
    column_numeric("app"),
    column_numeric("device"),
    column_numeric("channel"),
    column_numeric("time"),
    column_numeric("fraud.score"))



data_feed <- function(data) {
    input_fn(data, 
             features = c('ip','app','device','channel','time','fraud.score'), 
             #response = "is.fraud",
             batch_size = 64)
}

classifier <- dnn_classifier(
    feature_columns = feature_cols,  
    hidden_units = c(64,32), 
    n_classes = 2, 
    activation_fn = 'sigmoid',
    optimizer = 'SGD',
    label_vocabulary = c('YES','NO'),
    model_dir = './model'
)


n <- length(sub.clean.data$ip)
k <- floor(n/2^12)

y.pred <- list()


for(i in 2000:k-1) {
    test.result <- predict(classifier, input_fn = data_feed(sub.clean.data[(1+i*2^12): (1+2^12 + i*2^12), ]), predict_keys = 'probabilities')
    y.pred[i+1] <- unlist(test.result$probabilities, recursive = F)[seq(from = 1, by = 2, to = 2*length(test.result$probabilities))]
    fwrite(y.pred[i+1], file = paste('data', i,'.csv'))
}



