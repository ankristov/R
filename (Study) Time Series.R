# http://r-statistics.co/Time-Series-Analysis-With-R.html

inputData <- seq(1,100,1)
inputData
ts(inputData, frequency = 4, start = c(1959,2))
ts(inputData, frequency = 4, start = c(1959,3))
ts(inputData, frequency = 12, start = c(1959))
ts(inputData, start = c(2009), end = c(2020), frequency = 1)

# How to extract the trend, seasonality and error?
# The decompose() and forecast::stl() splits the time series into seasonality, trend and error components.
head(EuStockMarkets)
tsData <- EuStockMarkets[,1] # ts of DAX
decomposedRes <- decompose(tsData, type = 'mult') # use type = 'additive' for additive components
decomposedRes
plot(decomposedRes)
stlRes <- stl(tsData, s.window = 'periodic')
stlRes
plot(stlRes)

# How to create lags of a time-series ?
laggedTS <- lag(tsData, 3) # error: `x` must be a vector, not a ts object
laggedTS <- lag(as.vector(tsData), 1) # ok!
leadTS <- lead(as.vector(tsData), 1)
cbind(tsData[1:5], laggedTS[1:5], leadTS[1:5])
# the same with library
install.packages("DataCombine")
library(DataCombine)
df <- as.data.frame(tsData)
head(df)
df <- slide(data = df, Var = 'x', NewVar = 'lag', slideBy = -1)
df <- slide(data = df, Var = 'x', NewVar = 'lead', slideBy = 1)
head(df)

# What is Autocorrelation and Partial-Autocorrelation?
# Autocorrelation is the correlation of a Time Series with lags of itself. This is a significant metric because,
# It shows if the previous states (lagged observations) of the time series has an influence on the current state. In the autocorrelation chart, if the autocorrelation crosses the dashed blue line, it means that specific lag is significantly correlated with current series. For example, in autocorrelation chart of AirPassengers - the top-left chart (below), there is significant autocorrelation for all the lags shown on x-axis.
# It is used commonly to determine if the time series is stationary or not. A stationary time series will have the autocorrelation fall to zero fairly quickly but for a non-stationary series it drops gradually.
# Partial Autocorrelation is the correlation of the time series with a lag of itself, with the linear dependence of all the lags between them removed.
head(AirPassengers)
acfRes <- acf(AirPassengers) # autocorrelation
acf(tsData, lag.max = 1000)
pacfRes <- pacf(AirPassengers) # partial correlation
pacf(tsData)
ccfRes <- ccf(mdeaths, fdeaths, ylab = 'cross-correlation') # cross correlation between two timesires
head(ccfRes[[1]])
ccfRes

# How to de-trend a time series ?
# Use linear regression to model the Time Series data with linear indices (Ex: 1, 2, .. n). The resulting model’s residuals is a representation of the time series devoid of the trend. In case, if some trend is left over to be seen in the residuals (like what it seems to be with ‘JohnsonJohnson’ data below), then you might wish to add few predictors to the lm() call (like a forecast::seasonaldummy, forecast::fourier or may be a lag of the series itself), until the trend is filtered.
head(JohnsonJohnson)
plot(1:length(JohnsonJohnson), JohnsonJohnson, type = 'l')
fit <- lm(JohnsonJohnson ~ c(1:length(JohnsonJohnson)))
ggplot(data = data_frame(x = 1:length(JohnsonJohnson), y = JohnsonJohnson)) +
  geom_line(aes(x = x, y = y)) +
  geom_abline(slope = fit$coefficients[2], intercept = fit$coefficients[1], color = 'red')
ggplot(data = data_frame(res = fit$residuals, x = 1:length(JohnsonJohnson))) +
  geom_line(aes(x = x, y = res)) # still we can see trend...

# How to de-seasonalize a time series in R?
# De-seasonalizing throws insight about the seasonal pattern in the time series and helps to model the data without the seasonal effects. 
# So how to de-seasonalize? Step 1: De-compose the Time series using forecast::stl() Step 2: use seasadj() from ‘forecast’ package
install.packages("forecast")
library(forecast)
head(tsData)
#tsData <- EuStockMarkets[,1]
tsData <- JohnsonJohnson
plot(1:length(tsData), tsData, type = 'l')
tsData_decomposed <- stl(tsData, s.window = 'periodic') # decomposed object
tsData_deseason <- seasadj(tsData_decomposed) # de-seasonalize

tsData_decomposed_df <- as.data.frame(tsData_decomposed$time.series) # dataframe from components
tsData_decomposed_df$x <- 1:nrow(tsData_decomposed_df)
tsData_decomposed_df$y <- tsData
tsData_decomposed_df$deseason <- tsData_deseason
head(tsData_decomposed_df)
# to force ggplot produce legend for components we need to transform data, make them tidy
tsData_decomposed_df_melted <- reshape::melt(tsData_decomposed_df, 
                                             id.vars = 'x', 
                                             measure.vars = c('seasonal','trend', 'remainder','y','deseason'))
head(tsData_decomposed_df_melted)
# visualize components
library(RColorBrewer)
colors <- brewer.pal(n = 5, name = 'Set1')
# 1 in this way we'll not get legend because colors are designated manually outside aes()
ggplot(data = tsData_decomposed_df) +  # how to add legend???
  geom_line(aes(x = x, y = seasonal), color = colors[1]) +
  geom_line(aes(x = x, y = trend), color = colors[2]) +
  geom_line(aes(x = x, y = remainder), color = colors[3]) +
  geom_line(aes(x = x, y = y), color = colors[4]) +
  geom_line(aes(x = x, y = deseason), color = colors[5]) +
  #scale_color_manual(values = c(colors[1], colors[2], colors[3], colors[4]),
  #                   labels = c('seasonal', 'trend', 'remainder', 'y'),
  #                   breaks = c('4','6', '8', '10'), guide = 'legend') +
  labs(x = 'X axis', y = 'Components') 
# 2 with legend (why ggplot doesn't produce legend sometimes https://stackoverflow.com/questions/48768567/reasons-that-ggplot2-legend-does-not-appear)
ggplot(data = tsData_decomposed_df_melted) +
  geom_line(aes(x = x, y = value, color = variable)) +
  scale_color_brewer(palette = 'Set1')
seasonplot(tsData_deseason, 12, col = rainbow(12), year.labels = T, main = 'Seasonal plot')


# How to test if a time series is stationary?
# Use Augmented Dickey-Fuller Test (adf test). A p-Value of less than 0.05 in adf.test() indicates that it is stationary
library(tseries)
adf.test(tsData)  #p-value < 0.05 indicates the TS is stationary
kpss.test(tsData)

# How to make a time series stationary?
# Differencing a time series means, to subtract each data point in the series from its successor. It is commonly used to make a time series stationary. For most time series patterns, 1 or 2 differencing is necessary to make it a stationary series.
# But if the time series appears to be seasonal, a better approach is to difference with respective season’s data points to remove seasonal effect. After that, if needed, difference it again with successive data points. But, How to know how many differencing is needed? the nsdiffs and ndiffs from forecast package can help find out how many seasonal differencing and regular differencing respectively is needed to make the series stationary.
AirPassengers[1:10]
adf.test(AirPassengers)
ggplot(data = data_frame(year = seq(1949, 1960,length.out = length(AirPassengers)), value = AirPassengers)) +
  geom_line(aes(x = year, y = value)) +
  scale_x_continuous(breaks = seq(1949,1960,1))
# Seasonal differencing
nsdiffs(AirPassengers) # number for seasonal differencing needed
# below lag = 12
AirPassengers_seasonaldiff <- diff(AirPassengers, lag = frequency(AirPassengers), differences = 1)
length(AirPassengers)
length(AirPassengers_seasonaldiff) # lag = 12 so length is shorter on 12
lag(as.vector(AirPassengers), 2)[1:24]
plot(AirPassengers_seasonaldiff, type = 'l', main = 'Seasonal Difference') # still there is seasonal effect
ndiffs(AirPassengers_seasonaldiff)
# below lag = 1
AirPassengers_seasonaldiff <- diff(AirPassengers_seasonaldiff, differences = 1)
plot(AirPassengers_seasonaldiff, type = 'l', main = 'Seasonal Difference') # still there is seasonal effect
length(AirPassengers_seasonaldiff)

###############################################################
# Covid-19: Correlation Between Confirmed Cases And Deaths
###############################################################
# https://predictivehacks.com/covid19-correlation-between-confirmed-cases-and-deaths/
# What is the daily correlation of Confirmed versus Death Cases in Covid-19. In other words, the people who have passed away, on average, how many days ago they have been reported (i.e. “Confirmed”) as Covid-19 new cases.
# To answer this question, we can take the correlation between the Daily Confirmed vs Daily Deaths and trying different lag values of the confirmed cases, since the assumption is that it will take some days for someone to pass away since has been diagnosed with Covid-19.
# The problem with the data is that are affected by the number of tests and also during some days like weekends they do not report all the cases. This implies that our analysis is not valid, but we will try to see what get. We will analyze Italy.

# Italy: Correlation Between Confirmed Cases and Deaths
# https://github.com/RamiKrispin/coronavirus























