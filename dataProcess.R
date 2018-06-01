# setwd('/Users/nanjiang/Dropbox/fraud')
setwd("D:/Dropbox/fraud")
library(caret)
library(gbm)
library(data.table)
library(hashmap)
library(DMwR)
#############
# data subset selection
data.raw <- fread('data/train.csv')
data <- data.raw


#########
# time process

bar_time <- as.POSIXct('2000-01-01 00:00:00')
click_time <- unlist(substr(as.character(data$click_time), 12, 19))
click_time <- as.POSIXct(click_time, tz = 'GMT', format = '%H:%M:%S')
time_interval <- as.numeric(difftime(bar_time, click_time, units = 'secs'))
time_interval <- (time_interval - mean(time_interval))/sd(time_interval)

#################
# degrotory mark: use ip fraud proportion as a score



dat <- data.table(ip = data$ip, is_attributed = data$is_attributed)
setkey(dat, ip)library(DMwR)
fraud.prob <- dat[, .(as.numeric(sum(is_attributed))/.N), by = ip][order(-V1)]

# store fraud record in a hash table using ip as key
fraud.record <- hashmap(keys = fraud.prob$ip, values = fraud.prob$V1)
fraud.score <- fraud.record[[data$ip]]

is.fraud = as.character(ifelse(data$is_attributed == 0, 'YES', 'NO'))

#################################
# clean data
clean.data <- data.frame(ip = data$ip, app = data$app, device = data$device, channel = data$channel,
                         time = as.numeric(time_interval), fraud.score = fraud.score,
                         is.fraud = is.fraud, stringsAsFactors = F)
clean.data <- fread('cleandata.csv')
balanced.data <- SMOTE(is.fraud ~., clean.data, perc.over = 2000, perc.under = 80)