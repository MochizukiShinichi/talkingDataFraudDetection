sub.data <- fread('data/test.csv')


DataProcess <- function(data){

    #########
    # time process
    bar_time <- as.POSIXct('2000-01-01 00:00:00')
    click_time <- unlist(substr(as.character(data$click_time), 12, 19))
    click_time <- as.POSIXct(click_time, tz = 'GMT', format = '%H:%M:%S')
    time_interval <- as.numeric(difftime(bar_time, click_time, units = 'secs'))
    time_interval <- (time_interval - mean(time_interval))/sd(time_interval)
    
    
    ############
    # fraud score query
    # load('fraudRecord.Rdata')
   
    fraud.score <- fraud.record[[data$ip]]
    fraud.score[is.na(fraud.score)] = 0
    
    ## clean data
    return.data <- data.frame(ip = data$ip, app = data$app, device = data$device, channel = data$channel,
                             time = as.numeric(time_interval), fraud.score = fraud.score,
                             stringsAsFactors = F)
    return(return.data)

}


sub.clean.data <- DataProcess(sub.data)

test_data_feed <- function(data) {
    return(input_fn(
        data,
        features = c('ip', 'app', 'device', 'channel', 'time', 'fraud.score'),
        batch_size = 32
    ))
}


library(parallel)
library(doParallel)

# 
# numCores <- detectCores()
# cl <- makeCluster(numCores - 1)
# registerDoParallel(cl)

predict.func <- function(i){
    result <- predict(model, input_fn = test_data_feed(sub.clean.data[1+i*1000:1+(i+1)*1000, ]), predict_keys = 'probabilities')
    y.pred <- unlist(result$probabilities, recursive = F)[seq(from = 1, by = 2, to = 2*length(result$probabilities))]
    return(y.pred)
}

results <-  lapply(1:18789, predict.func)

prediction_gen <- function(data){
    result <- predict(model, input_fn = test_data_feed(data), predict_keys = 'probabilities')
    y.pred <- unlist(result$probabilities, recursive = F)[seq(from = 1, by = 2, to = 2*length(result$probabilities))]
    return(y.pred)
}


result.o <- as.matrix(unlist(results), ncol = 1)
result.f <- as.matrix(unlist(last.pred), ncol = 1)
cmb.result <- rbind(result.o, result.f)
cmb.result[is.na(cmb.result)] <- 1.0
cmb.means <- kmeans(cmb.result, centers = 2)

final.result <- data.frame(click_id = seq(0, length(cmb.result)-1), is_attributed = 2 - cmb.means$cluster)
fwrite(final.result, file = 'submission.csv')

last.result <- predict(model, input_fn = test_data_feed(sub.clean.data[18789001:18790469, ]), predict_keys = 'probabilities')

last.pred <- unlist(last.result$probabilities, recursive = F)[seq(from = 1, by = 2, to = 2*length(last.result$probabilities))]

###########################
ind <- as.array(which(is.na(final.result$is_attributed)))
y.ind <- prediction_gen(sub.clean.data[ind[1], ])


