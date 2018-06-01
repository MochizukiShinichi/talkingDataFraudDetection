TrainModel <- function(data, model.loc, steps) {
    data <- add_data_weight(data)
    
    feature_cols <- construct_feature_columns()
    weight_col <- construct_weight_col()
    data_feed <- construct_data_feed(data)
    classifier <- construct_classifier(feature_cols, weight_col, model.loc)
    
    return(classifier)

}

add_data_weight <- function(data)
{
    data$weight <- ifelse(data$is.fraud == 'YES', 1, 400)
    return(data)
}

construct_feature_columns <- function(){
    return(feature_columns(
        column_numeric("ip"),
        column_numeric('app'),
        column_numeric('device'),
        column_numeric('channel'),
        column_numeric("time"),
        column_numeric("fraud.score")
    ))
}



construct_weight_col <- function(){
    return(column_numeric('weight'))
}


construct_data_feed <- function(data){
    data_feed <- function(data) {
        input_fn(
            data,
            features = c('ip', 'app', 'device', 'channel', 'time', 'fraud.score','weight'),
            response = "is.fraud",
            batch_size = 32,
            epoch = 5
        )
    }
    return(data_feed)}

construct_classifier <- function(feature_cols, weight_col, model.loc){
    return(dnn_classifier(
        feature_columns = feature_cols,
        weight_column = weight_col,
        hidden_units = c(8,8,8,8,4,4),
        n_classes = 2,
        activation_fn = 'sigmoid',
        label_vocabulary = c('YES', 'NO'),
        model_dir = model.loc
    ))
}

