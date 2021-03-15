library(dplyr)

finaldata <- read.csv("Revised.csv")

colnames(finaldata)


grp_year_df <- finaldata %>%
  group_by(Year=Year) %>%
  summarise(TotalCount = sum(Count))

fd1 <- grp_year_df[,c("TotalCount")]

is.nan(ts_fd1)

ts_fd1 <- ts(fd1)

class(ts_fd1)

cat("Year: ", start(ts_fd1), "\n")
cat("count : ", frequency(ts_fd1), "\n")
print(summary(ts_fd1)) 

#Exploratory data analysis

frequency(ts_fd1)
cycle(ts_fd1)

#Correcting time series structure
fd1_ts <- ts(fd1, start=c(2014, 1), end = c(2016, 12), frequency=12)

cycle(fd1_ts)

#Checking for frequency
frequency(fd1_ts)

# View the records with NA
na_records <- fd1_ts[!complete.cases(fd1_ts)]
sum(na_records)

# Show data using a plot() function
plot(fd1_ts,
     xlab="Year", 
     ylab = "Count",
     main="Count of patients based on month and year")
abline(reg=lm(fd1_ts~time(fd1_ts)))


plot(aggregate(fd1_ts,FUN=mean))

boxplot(fd1_ts ~ cycle(fd1_ts),
        xlab="", 
        ylab = "year" ,
        main ="Patient count")

seasonal_decomposition <- stl(fd1_ts, s.window="period")
plot(seasonal_decomposition)


#ADF TEST

library(tseries)
suggested_k <- trunc((length(fd1_ts)-1)^(1/3))
suggested_k

adf.test(fd1_ts, alternative = "stationary")


adf.test(fd1_ts, alternative = "stationary", k = 12)

#Test stationary of time series
library(forecast)
acf(fd1_ts)

pacf(fd1_ts)


library(forecast)
nsdiffs(fd1_ts)

log_fd1_ts <- log(fd1_ts)


opar <- par(no.readonly=TRUE)
par(mfrow=c(1,2))
plot(fd1_ts, main = "Original  dataset")
plot(log_fd1_ts, main = "Differenced dataset")
par(opar)


nsdiffs(log_fd1_ts)

diff_fd1_ts <- diff(log_fd1_ts, lag = 12, differences = 2)

opar <- par(no.readonly=TRUE)
par(mfrow=c(1,2))
plot(fd1_ts, main = "Original dataset")
plot(diff_fd1_ts, main = "Differenced  dataset")
par(opar)



nsdiffs(diff_fd1_ts)


adf.test(diff_fd1_ts, alternative = "stationary")


seasonal_decomposition <- stl(fd1_ts, s.window="period")
plot(seasonal_decomposition)


 #simple exponential - models level
 fit <- HoltWinters(fd1_ts, beta=FALSE, gamma=FALSE)
 
# double exponential - models level and trend
fit <- HoltWinters(fd1_ts, gamma=FALSE)

# triple exponential - models level, trend, and seasonal components
fit <- HoltWinters(fd1_ts)

# predictive accuracy
library(forecast)
accuracy(forecast(fit))

# predict next three future values
library(forecast)
forecast(fit, 3)
plot(forecast(fit, 3))

#Testing and Training data

fd1_ts_train <- window(x = fd1_ts, start=c(2014, 1), end=c(2016, 6))
fd1_ts_test <- window(x = fd1_ts, start=c(2016, 7))

fd1_ts_train

fd1_ts_test

fit <- HoltWinters(fd1_ts_train, beta=FALSE, gamma=FALSE)
fit <- HoltWinters(fd1_ts_train, gamma=FALSE)

# triple exponential - models level, trend, and seasonal components

# predictive accuracy
library(forecast)
accuracy(forecast(fit))


forecast(fit, 3)
plot(forecast(fit, 3))

fit <- HoltWinters(fd1_ts_test, beta=FALSE, gamma=FALSE)
fit <- HoltWinters(fd1_ts_test, gamma=FALSE)

accuracy(forecast(fit))

forecast(fit, 12)
plot(forecast(fit, 12))


# # simple exponential - models level
# fit <- HoltWinters(fd1_ts, beta=FALSE, gamma=FALSE)
# 
# # double exponential - models level and trend
# fit <- HoltWinters(fd1_ts, gamma=FALSE)

# triple exponential - models level, trend, and seasonal components
fit <- HoltWinters(fd1_ts)

# predictive accuracy
library(forecast)
accuracy(forecast(fit))

#Research Questions
library(ggplot2)
op_waiting_list_by_year_and_agetype<-finaldata %>% group_by(Year,Adult.Child) %>% summarise(Total=mean(Count))

aggregate_3<-op_waiting_list_by_year_and_agetype
colnames(aggregate_3)<-c("Year","AdultOrChild","Mean")

ggplot(data=op_waiting_list_by_year_and_agetype,aes(x=Year,y=Total,fill=Adult.Child))+
  geom_col(position = "dodge")

#Research Question
op_waiting_list_by_year_and_waitingtime<-finaldata%>% group_by(Year,Time.Bands) %>% summarise(Total=mean(Count))

aggregate_4<-op_waiting_list_by_year_and_waitingtime
colnames(aggregate_4)<-c("Year","Time Bands","Mean")+theme_classic()


ggplot(data=op_waiting_list_by_year_and_waitingtime,aes(x=Year,y=Total,fill=Time.Bands))+geom_col(position="dodge")+theme_classic()
