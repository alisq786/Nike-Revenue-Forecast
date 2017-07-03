###====================================================================================================================###
#  Application      : Nike Revenue Forecast                                                                              #
#  ML Problem       : Timeseries Forecasting Model                                                                       #
#  Model Version    : 1.0                                                                                                #
#  Model Build Date : April 29, 2017                                                                                     #
#  Team             : Data Diggers                                                                                       #
#  Organization     : UPX Academy                                                                                        #
###====================================================================================================================###
#  Load Required Packages                                                                                                #
###--------------------------------------------------------------------------------------------------------------------###

library(dplyr)
library(caret)
library(readxl)
library(forecast)
library(TTR)
library(Metrics)
library(tseries)
library("astsa")

###--------------------------------------------------------------------------------------------------------------------###
#  Read in the input dataset                                                                                             #
#  - Review for its correctness                                                                                          #
#  - If not found OK, create proper dataset and format it as required                                                    #
#  - Clean the data, so that it can used as an Time Series Object                                                        #
###--------------------------------------------------------------------------------------------------------------------###

nike_revenue <- read_excel("Nike_revenue.xlsx",sheet=1,col_names = TRUE, col_types = NULL)
dim(nike_revenue)
View(nike_revenue) #Seems like both train / validation data is provided in the same dataset and some columns name are missing 
nike_data_colnames <- c("Year","Aug-31","Nov-30","Feb-28","May-31","emp1","emp2","Year","Aug-31","Nov-30","Feb-28","May-31")


colnames(nike_revenue) <- nike_data_colnames #Apply Column names to the dataset
View(nike_revenue)
str(nike_revenue)

train_nike_revenue <- nike_revenue[,1:5] #Splitting source data into train dataset
View(train_nike_revenue)
str(train_nike_revenue)
train_nike_revenue

train_nike_revenue_RC <- train_nike_revenue

val_nike_revenue <- nike_revenue[,8:12] #Splitting source data into validation dataset
View(val_nike_revenue)
str(val_nike_revenue) #Seems like it contains missing data in the rows; Also only first row seems to be valid
val_nike_revenue <- head(val_nike_revenue, 1) #selecting rows containing valid data
val_nike_revenue$Year <- as.numeric(val_nike_revenue$Year) #converting to valid column type
val_nike_revenue[,2] <- as.numeric(val_nike_revenue[,2]) #converting to valid column type
View(val_nike_revenue)
str(val_nike_revenue)

val_nike_revenue_RC <- val_nike_revenue

###====================================================================================================================###
#  Reorganize the quarterly data in Dataframe to vectorize, so that a Time Series Object can be created                  #
#  - Convert every row of a dataframe into row of vector                                                                 #
#  - Concatenate all the individual row vectors into a single continuous vector                                          #
#  - Create a Time Series Object with the vectorized row data as a quarterly frequency                                   #
###====================================================================================================================###
#  Convert every row of a dataframe into row of vector                                                                   #
###--------------------------------------------------------------------------------------------------------------------###

tsrdr1 <- as.numeric(train_nike_revenue_RC[1,2:5])
tsrdr2 <- as.numeric(train_nike_revenue_RC[2,2:5])    
tsrdr3 <- as.numeric(train_nike_revenue_RC[3,2:5])
tsrdr4 <- as.numeric(train_nike_revenue_RC[4,2:5])
tsrdr5 <- as.numeric(train_nike_revenue_RC[5,2:5])
tsrdr6 <- as.numeric(train_nike_revenue_RC[6,2:5])
tsrdr7 <- as.numeric(train_nike_revenue_RC[7,2:5])
tsrdr8 <- as.numeric(train_nike_revenue_RC[8,2:5])
tsrdr9 <- as.numeric(train_nike_revenue_RC[9,2:5])
tsrdr10 <- as.numeric(train_nike_revenue_RC[10,2:5])

###--------------------------------------------------------------------------------------------------------------------###
#  Concatenate all the individual row vectors into a single continuous vector                                            #
###--------------------------------------------------------------------------------------------------------------------###

tsrd <- as.numeric(c(tsrdr1,tsrdr2,tsrdr3,tsrdr4,tsrdr5,tsrdr6,tsrdr7,tsrdr8,tsrdr9,tsrdr10))
class(tsrd)
dim(tsrd)
tsrd  

###--------------------------------------------------------------------------------------------------------------------###
#  Create a Time Series Object with the vectorized row data as a quarterly frequency                                     #
###--------------------------------------------------------------------------------------------------------------------###

tsrd_ts <- ts(tsrd, start=1998+2/4, frequency = 4)
tsrd_ts
plot(tsrd_ts)
summary(tsrd_ts)
boxplot(tsrd_ts)

class(tsrd_ts)  # Test to ensure if object created properly as a Time Series Object
length(tsrd_ts) # Test to ensure if object created properly as a Time Series Object
tsrd_ts[40]     # Test to ensure if object created properly as a Time Series Object
dim(tsrd_ts)    # Test to ensure if object created properly as a Time Series Object
nrow(tsrd_ts)   # Test to ensure if object created properly as a Time Series Object
ncol(tsrd_ts)   # Test to ensure if object created properly as a Time Series Object
cycle(tsrd_ts)  # Test to ensure if object created properly as a Time Series Object
time(tsrd_ts)   # Test to ensure if object created properly as a Time Series Object

###--------------------------------------------------------------------------------------------------------------------###
#  Similarly Create Validation Time Series Object with the vectorized row data as a quarterly frequency                  #
###--------------------------------------------------------------------------------------------------------------------###

tsrdrv <- as.numeric(val_nike_revenue_RC[1,2:5])
tsrd_val_ts <- ts(tsrdrv, start=2008+2/4, frequency = 4)
tsrd_val_ts
val_nike_revenue <- tsrd_val_ts
class(val_nike_revenue)
#####================================================================================================================#####
##  Address the case questions                                                                                          ##
##  - Plot the Time Series                                                                                              ##
##  - Time Series Components and Interpretation                                                                         ##
##    - Decompose the Time Series to identify its elements                                                              ##
##    - Interprets the Time Series Data                                                                                 ##
#####================================================================================================================#####
#   - Plot the Time Series                                                                                               #
###--------------------------------------------------------------------------------------------------------------------###

train_nike_revenue <- tsrd_ts

plot.ts(train_nike_revenue, ylab="Quarterly Revenue",main="Nike's Revenue") #plot the time series

###--------------------------------------------------------------------------------------------------------------------###
#   - Decompose the Time Series to identify its elements                                                                 #
###--------------------------------------------------------------------------------------------------------------------###

dts_nr <- decompose(train_nike_revenue) # Decompose time series to identify the elements of time series

plot(dts_nr)
dts_nr$seasonal
dts_nr$trend
dts_nr$random

lines(trendcycle(dts_nr),col="red") # fits to show the trend along the time series data

boxplot(train_nike_revenue~cycle(train_nike_revenue)) # Variance and Mean higher during November end than the other months

sdc <- stl(train_nike_revenue, s.window="periodic")   # Seasonal Trend Decomposition 
sdc

plot(sdc) #Plot revenue time series into three components - seasonal, trend and remainder
monthplot(sdc)

###--------------------------------------------------------------------------------------------------------------------###
#   - Interpret Time Series                                                                                              #
###--------------------------------------------------------------------------------------------------------------------###
#     From the plots, we notice that the time series has Seasonality, Trend and Randomness. Also we notice that the      #
#     random fluctuations are roughly constant in size over time. So this data can be describes as Additive Model.       #
#     Seasonality - We notice a repetition pattern periodically over each fiscal year with high revenues in the          #
#                   beginning of fiscal year and then revenues fall in the next two quarter and rises up until 1st       #
#                   quarter of the next fiscal year.                                                                     #
#     Trend       - We notice a constant pattern of gradual rise an upward pattern in revenues                           #
#     Random      - We notice traces of Randomness with significant high and low spikes along the time period            #
#                                                                                                                        #
#     a. The mean of the time series does not seem to be constant with time. The mean is changing along the time.        #
#     b. The variance of the series seems roughly constant along the time.                                               #
#     c. The covariance of the series seems marginally increases along the time.                                         #
#                                                                                                                        #
#     From the above (a, b, c)  it looks the series is non-stationary time series. Need to validate this.                #
###--------------------------------------------------------------------------------------------------------------------###

acf2(train_nike_revenue)  # Validation for Non-Stationary Time Series. 

###--------------------------------------------------------------------------------------------------------------------###
#  ACF plot shows gradual decline implies the series is non-stationary  as a high number of previous observations are    #
#  correlated with future values. There are many lags above the significant threshold line.                              # 
#                                                                                                                        #
#  Note1: Since the Nike Revenue TS is non-stationary; we need to make it stationary before fitting ARIMA model as 
#         stationarity is the requirment for ARIMA models
#  Note2: Exponential Smoothing models require that the forecast errors (reseduals) are uncorrelated and are normally 
#         distributed with mean zero and constant variance; and it makes no assumptions about the correlations between 
#         successive values of the time series (i.e. auto-correlation)
#                     #
###--------------------------------------------------------------------------------------------------------------------###



#####================================================================================================================#####
##   Part - I : Regression : y=c+mx+e. Where Intercept is c; Slope is m; and Random Error is e                          ##
#####================================================================================================================#####
##           - Perform Time Series Linear Regression - tslm                                                             ##
##           - Review Time Series Linear Regression                                                                     ##
##           - Calculate the RMSE for the Training Set                                                                  ##
##           - Predict / Forecast for the Validation Set                                                                ##
##           - Discuss Forecast - Reasonability                                                                         ##
##           - Calculate the RMSE for the Validation Set                                                                ##
##           - Predict / Forecast for Nike Revenues in Year 2010                                                        ##
#####================================================================================================================#####

plot(train_nike_revenue, ylab="Quarterly Revenue",main="Nike's Revenue") # Plot time series
abline(reg=lm(train_nike_revenue~time(train_nike_revenue)))

nr_ts_lm <- tslm(train_nike_revenue ~ trend + season) # Perform the Linear Regression of the Time Series
class(nr_ts_lm)                                       # Check the class of the model

###--------------------------------------------------------------------------------------------------------------------###
#  Review Time Series Linear Regression                                                                                  #
###--------------------------------------------------------------------------------------------------------------------###

par(mfrow=c(2,2))                                     # Set canvas to graph by 2 Rows and 2 Columns
plot(nr_ts_lm)                                        # Plot the model for its results
par(mfrow=c(1,1))

summary(nr_ts_lm)                                     # Review the summary the time series model
train_nike_revenue                                    # This is actual values
nr_ts_lm$fitted.values                                # This is predicted values
nr_ts_lm$residuals                                    # This is residuals => Actual Values - Predicted Values

par(mfrow=c(3,1))                                     # Canvas to accommodate graphs in 3 Rows of 1 Column
plot(train_nike_revenue)                              # Plot of Actual Values
plot(nr_ts_lm$fitted.values)                          # Plot of Predicted Values
plot(nr_ts_lm$residuals)                              # Plot of Residuals and varies between +400 to -400 over period

par(mfrow=c(2,1))                                     # Plot for residual analysis
acf(nr_ts_lm$residuals)                               # Seems there several significant lags above threshold
                                                      # Also zeroes down at lag 2.25
pacf(nr_ts_lm$residuals)                              # Seems few significant lags above threshold
                                                      # Also zeroes down at multiple lag 1.75 and 3.75
acf2(nr_ts_lm$residuals)                              # from the correlogram it is quite evident that there is significant 
                                                      # evidence of non-zero correlations at various lags
                                                      
par(mfrow=c(1,1))

###--------------------------------------------------------------------------------------------------------------------###
#  Calculate RMSE on the Training Set                                                                                    #
###--------------------------------------------------------------------------------------------------------------------###

rmse(train_nike_revenue,nr_ts_lm$fitted.values)       # RMSE of Training Set with Linear Regression Model = 222.1858



###--------------------------------------------------------------------------------------------------------------------###
#  Predict / Forecast for the Validation Set                                                                             #
###--------------------------------------------------------------------------------------------------------------------###

nr_fclm_2009 <- forecast.lm(nr_ts_lm, h=4, level=c(75,80,85,90,95), biasadj = TRUE, ts=TRUE)
summary(nr_fclm_2009)

#->  Predicted results for validation period - 2009
#->          Point Forecast    Lo 75    Hi 75    Lo 80    Hi 80    Lo 85    Hi 85    Lo 90    Hi 90    Lo 95    Hi 95
#->  2008 Q3         4738.3 4434.989 5041.611 4399.609 5076.991 4356.664 5119.936 4300.207 5176.393 4211.909 5264.691
#->  2008 Q4         4367.8 4064.489 4671.111 4029.109 4706.491 3986.164 4749.436 3929.707 4805.893 3841.409 4894.191
#->  2009 Q1         4449.8 4146.489 4753.111 4111.109 4788.491 4068.164 4831.436 4011.707 4887.893 3923.409 4976.191
#->  2009 Q2         4832.1 4528.789 5135.411 4493.409 5170.791 4450.464 5213.736 4394.007 5270.193 4305.709 5358.491

#->  Review Results with the Validation Data
#->  Year            Actual
#->  2008 Q3           5432
#->  2008 Q4           4590
#->  2009 Q1           4440 
#->  2009 Q2           4713     

#->  From the above forecast seems to reasonable at Hi 85 Confidence Intervals and forecast seems satisfactory

plot(nr_fclm_2009)                                # Plot the forecast for the validation period
lines(nr_fclm_2009$fitted, lwd=2, col="green")    # Plot the fitted values of the forecast 2009
lines(val_nike_revenue, col="red")                # Plot to see actual revenues for year 2009

par(mfrow=c(2,1))
plot(nr_fclm_2009, val_nike_revenue, type = "l")
lines(val_nike_revenue, col="red")
plot(nr_fclm_2009, val_nike_revenue, type = "h")
lines(val_nike_revenue, col="red")
par(mfrow=c(1,1))

nr_fclm_2009$mean
nr_fclm_2009$x
nr_fclm_2009$residuals
nr_fclm_2009$fitted


###--------------------------------------------------------------------------------------------------------------------###
#  Calculate RMSE on the Validation Set                                                                                  #
###--------------------------------------------------------------------------------------------------------------------###

rmse(val_nike_revenue, nr_fclm_2009$mean)             # RMSE of Validation Set with linear Regression model = 369.0777

# Note: Since RMSE for training dataset (222.1858) is much lower compare to RMSE for Validation dataset (369.0777)
#       shows predictive power of linear regression model is not satisfactory

###--------------------------------------------------------------------------------------------------------------------###
#  Predict / Forecast for Nike Revenues in Year 2010                                                                     #
###--------------------------------------------------------------------------------------------------------------------###

nr_fclm_2010 <- forecast.lm(nr_ts_lm, h=8, level=c(75,80,85,90,95), biasadj = TRUE, ts=TRUE)
nr_fclm_2010
summary(nr_fclm_2010)

par(mfrow=c(2,1))
plot(nr_fclm_2010, type = "l")
plot(nr_fclm_2010, type = "h")
par(mfrow=c(1,1))




###--------------------------------------------------------------------------------------------------------------------###
#    Inference: From the above forecast results and considering residuals behaviour and ACF/PACF measures on this        #
#               time series data, Regression would not be appropriate best model for forecast as much information left   #
#               in residuals
###--------------------------------------------------------------------------------------------------------------------###



#####================================================================================================================#####
##   Part - II : Smoothing Methods                                                                                      ##
##   - Identify Smoothing Method for Nike's Revenue Forecasting                                                         ##
##   - Discuss over the selection of Smoothing Method                                                                   ##
#####================================================================================================================#####

#-> From the above Time Series Decompose Plot and STL Plot we have noticed and concluded that there is Seasonal Component, 
#-> Trend Component and Randomness Component in the Nike Revenue Data. Since all the three components are present in the
#-> dataset that implies Triple Exponential Smoothing Method (TES) will be appropriate for nike revenue data. 
#-> TES provide flexibility in modeling Trend and Seasonality. 
#-> Notwithstanding noticing both trend and seasonality in the Nike's reveneu data, let's validate our observations by 
#-> first fitting simple exponentional smoothing. Note we'll estimate smoothing parameter alpha using the given data..

###--------------------------------------------------------------------------------------------------------------------###
#  Simple Exponential Smoothing (SES)                                                                                        #
###--------------------------------------------------------------------------------------------------------------------###

nr_ses <- HoltWinters(train_nike_revenue, beta=FALSE, gamma=FALSE)
nr_ses

# note from above results that alpha is over 0.6 implies that forecasts are largely based on both recent and historical
#  (less recent) values however recent values will have more wights than older values

plot(nr_ses)

# let's check the acuracy of forecast

nr_ses$SSE

# let's forecast for next 8 quarters
nr_ses_f <- forecast.HoltWinters(nr_ses, h=8)
nr_ses_f
plot.forecast(nr_ses_f)

# In above SES forecast, revenue values forecasted from Q3-2010 to Q2-2012. Note in above plot forecasted value are blue line
# without any trend and seasonalityin it; dark shaded are denotes 80% prediction interval and light shaded area extended 
# to 95% of pridection interval.

# Let's do residuals analysis
nr_ses_f$residuals
# residuals are only for insample i.e. actual time period of data as it is the difference of actuals and predicted values
nr_ses_f$residuals<-na.omit(nr_ses_f$residuals) # remove na from residuals
acf2(nr_ses_f$residuals)

# from the correlogram it is quite evident that there is significant evidence of non-zero 
# correlations at various lags

# now to double confirm that data has significat non-zero autocorrelation, we can carry out a Ljung-Box test as follows: 
Box.test(nr_ses_f$residuals, lag=16, type="Ljung-Box")
#from Ljung-Box test resuts above with the p value much lower than 0.05, it is abendently clear that there is significant 
# evidence of non-zero autocorrelations for residual

# Now it is abendently clear from acf/pacf functions and Ljung-box tex that residucals have lots of information left.

# further verify our assumption by fitting Double Exponential Smoothing (DES)

###--------------------------------------------------------------------------------------------------------------------###
#  Double Exponential Smoothing (DES)                                                                                         #
###--------------------------------------------------------------------------------------------------------------------###

nr_des <- HoltWinters(train_nike_revenue, gamma=FALSE)
nr_des

# The high estimated value of alpha (1) implies that forecast is dependent on most recent values of time series; however 
# small value of beta (0.22) implies that slop of the trend component is less dependent on recent values rather on  
# historical values

plot(nr_des)

# let's check the acuracy of forecast

nr_des$SSE

# let's forecast for next 8 quarters
nr_des_f <- forecast.HoltWinters(nr_des, h=8)
nr_des_f
plot.forecast(nr_des_f)

# In above dES forecast, Note the plot of forecasted value is blue line and clearly trend is incorporated in the forecast;
# dark shaded area denotes 80% prediction interval and light shaded area extended to 95% of pridection interval.

# Let's do residuals analysis
nr_des_f$residuals
# residuals are only for insample i.e. actual time period of data as it is the difference of actuals and predicted values
nr_des_f$residuals<-na.omit(nr_des_f$residuals) # remove na from residuals
acf2(nr_des_f$residuals)

# from the correlogram it is quite evident that there is significant evidence of non-zero 
# correlations at various lags

# now to double confirm that data has significat non-zero autocorrelation, we can carry out a Ljung-Box test as follows: 
Box.test(nr_des_f$residuals, lag=16, type="Ljung-Box")
#from Ljung-Box test resuts above with the p value much lower than 0.05, it is abendently clear that there is significant 
# evidence of non-zero autocorrelations for residual

# Now it is abendently clear from acf/pacf functions and Ljung-box tex that residuals have lots of information left in 'em.

# Now further verify our assumption by fitting Triple Exponential Smoothing (TES); As we observed earlier TES likely to fit
# nike revenue data aptly; let check....

###--------------------------------------------------------------------------------------------------------------------###
#  Triple Exponential Smoothing (TES)                                                                                         #
###--------------------------------------------------------------------------------------------------------------------###

nr_tes <- HoltWinters(train_nike_revenue)
nr_tes

# The estimated values of alpha (0.38) implies the foreast level at the current time point is based upon both recent 
# observations and more distant past values, however non-recent values has more weight; beta (0.6) implies that the
# slope b of the trend component, are based largely upon very recent observations in the time series; while the value 
# of gamma (1) indicating that the estimate of the seasonal component at the current time point is just based upon 
# very recent observations



plot(nr_tes) # notice the fitted (forecasted) values, how closely they fit to the actual data

# let's check the acuracy of forecast

nr_tes$SSE # notice how smaller is the value of TES$SSE (310892.9) compared to SES$SSE (3652701) and DES$SSE (5137920)
           
# let's forecast for next 8 quarters
nr_tes_f <- forecast.HoltWinters(nr_tes, h=8)
nr_tes_f
plot.forecast(nr_tes_f)

# In above tES forecast, Note the plot of forecasted value are blue line and clearly trend and sesonality are incorporated
#  in the forecast;


# Let's do residuals analysis
nr_tes_f$residuals
# residuals are only for insample i.e. actual time period of data as it is the difference of actuals and predicted values
nr_tes_f$residuals<-na.omit(nr_tes_f$residuals) # remove na from residuals
acf2(nr_tes_f$residuals)

#The correlogram shows that the autocorrelations for the in-sample forecast errors do not exceed the significance bounds 
# for various lags( note 3rd lag slightly exceeds the significance bound however that can be entirly by chance); hence 
# it is quite evident that there is no significant evidence of non-zero autocorrelations at various lags

# now to double confirm whether data has significat non-zero autocorrelation, we can carry out a Ljung-Box test as follows: 
Box.test(nr_tes_f$residuals, lag=16, type="Ljung-Box")
#from Ljung-Box test resuts above with the p value as big as 0.06, it is barely clear that there is no significant evidence
#  of non-zero autocorrelations for residual
# Now it is abendently clear from acf/pacf functions and Ljung-box tex that residuals have not much information left in 'em.


# Further Residuals Analysis:  in order to check whether the forecast errors have constant variance over time, and are          #
#   normally distributed with mean zero and constant variance, by making a time plot of the forecast errors and a histogram                    # 
#  (with overlaid normal curve):                                                                                         #


# Plot residual plot to check if residuals have constant variance over time

plot.ts(nr_tes_f$residuals)  # by examining the residuals charts, # From the time plot, it appears plausible that the 
                             # forecast errors have constant variance over time except the abnormal drop in 2006

# Now let's check if residubals are normally distributed with mean zero; following function will create a histogram of  #
# forcast error with over laid normal curve                                                                             #


plotForecastErrors <- function(forecasterrors)
{
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mysd <- sd(forecasterrors)
  mymin <- min(forecasterrors) - mysd*5
  mymax <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}


plotForecastErrors(nr_tes_f$residuals)  # histogram of errors with overlaid normal curve


# From the histogram of forecast errors, it seems plausible that the forecast errors are normally distributed with 
# mean zero. Thus,there is little evidence of autocorrelation at various lags for the forecast errors, and the forecast 
# errors appear to be normally distributed with mean zero and constant variance over time. This suggests that Holt-Winters
# exponential smoothing provides an adequate predictive model of the nike reveneu, which probably cannot be improved upon. 



###====================================================================================================================###
#  Part - IV : ARIMA Models                                                                                              #
#  - A. Is the Data Stationary? Explain if Data is Stationary? Apply ideas to Nike's Revenue Data                        #
#  - B. Can Non-Stationary Data be made Stationary Data?                                                                 #
###====================================================================================================================###
# Part - IV : A - Stationary Series                                                                                      #
###--------------------------------------------------------------------------------------------------------------------###

# Stationarity is important in Time Series. Unless data is stationary, it cannot be used to build ARIMA models. 

# For a Time Series to be Stationary 
# - Mean of the Time Series should be steady, it should not be changing over time.
# - Variance of the Time Series should be steady, it should not be changing over time.
# - Covariance of the Time Series should be steady, it  should not be changing over time.

# To make a non-stationary time series stationary:
# - The two most common ways to make a non-stationary time series curve stationary are:
#   - Differencing (Most commenly used technique to make TS stationary; can be of first order or 2nd order or 3rd order)
#   - Transforming (most common transformation is log transformation; However, it is normally suggested that you use 
#     transformation only in case differencing is not working)

# Tests to check Stationarity 
# - ACF and PACF graphs and check significant lags
# - Augmented Dickey-Fuller (ADF) t-statistic test suggest smaller p-value for object to be stationary



###=================================================================================================================###

#  Generally following are steps for ARIMA modeling:
#     - plot and visualize the time series
#     - Stationarize the time series
#     - find Optimal Parameters of ARIMA model generally using ACF/PACF measures
#     - Fit ARIMA model using optimal parameters
#     - Forecast values using Selected/fitted ARIMA models

###=================================================================================================================###


# plot and visualize the Nike Revenue TS

plot.ts(train_nike_revenue)

# from the chart we know that data has trend and seasonality and it seems it is a non-stationary time serie;
# let's fuether check stationarity using ACF/PACF measures

acf2(train_nike_revenue)

# Note from ACF plot that it is slowly and gradually decerasing indicating the nike revenue TS is not stationary; 
# to confirm further let's perform dicky-fuller test 

adf.test(train_nike_revenue, alternative = c("stationary", "explosive"), k = trunc((length(train_nike_revenue)-1)^(1/3)))

# dicky-fuller test further confirms that Nike TS is a non-stationary time series

# let's try making nike TS stationary by differenceing

tnr_diff1 <- diff(train_nike_revenue, differences = 1)
plot.ts(tnr_diff1)

# Examining plot of nike revenues TS's first differences appears to be somewhat stationary i.e. stationary in mean and in
# variance (as the level of the series stays roughly constant over time, and the variance of the series appears roughly 
# constant over time.), and hence an ARIMA(p,1,q) model is probably appropriate for the time series

# since nike revenue also shows seasonal variation, let's do the seasonal difference as well of 1st order difference TS

tnr_diff1sd1 <- diff(tnr_diff1, differences = 4)  # since it is quarterly data it should be diff=4
plot.ts(tnr_diff1sd1, ylab="Quarterly Revenue",main="Nike's Revenue 1st order difference")

# let's check the stationarity of of normal and seasonally differenced series

adf.test(tnr_diff1sd1, alternative = c("stationary", "explosive"), k = trunc((length(tnr_diff1sd1)-1)^(1/3)))

# From DF test it appears that the normal+seasonal differenced (both of 1st order) series is stationary

# now check for optimal parmeter of ARIMA(p,1,q)(p,1,q)s model

acf2(tnr_diff1sd1)


# by examining ACF and PACF of tnr_diff1sd1 series (i.e. first order differenced for both simple and seasonal) 
# limited or no possiblity of any auto regressive (i.e. p=0 or p=1) model plus no possibility of moving average any order  
# i.e. sarima(0,1,0)(0,1,0)  or sarima(1,1,0)(0,1,0) likely be the best fit model

# let's fit both sarima(0,1,0)(0,1,0) and sarima(1,1,0)(0,1,0), and compare

tnr_sarima010010 <- arima(train_nike_revenue, order = c(0,1,0), seasonal = list(order = c(0,1,0), period = 4))
tnr_sarima010010  # ARIMA(0,1,0)(0,1,0)

tnr_sarima110010 <- arima(train_nike_revenue, order = c(1,1,0), seasonal = list(order = c(0,1,0), period = 4))
tnr_sarima110010  # ARIMA(1,1,0)(0,1,0)

# by comparing AIC; ARIMA(1,1,0)(0,1,0) looks better fit model
# let's further check our result by allowing R to fit ARIMA model automatically to our Nike Revenue time series

auto.arima(train_nike_revenue)

# our inference confirmed as R chose the same ARIMA(1,1,0)(0,1,0) as best fit model



# forecasting for next 8 quarters using ARIMA(1,1,0)(0,1,0)

tnr_sarima110010f <- forecast.Arima(tnr_sarima110010, h=8)
tnr_sarima110010f


# Let's plot forecast for next 8 quarters using ARIMA(1,1,0)(0,1,0)

plot.forecast(tnr_sarima110010f)  # forecast for next 8 quarters
tnr_sarima110010f$fitted          # Fitted values by model ARIMA(1,1,0)(0,1,0)
tnr_sarima110010f$residuals       # Residuals values by model ARIMA(1,1,0)(0,1,0)


###===========================================================================================================###
#   Residuals Analyis

# let's examine ACF/PACF measures to check if their any information left in the residuals

acf2(tnr_sarima110010f$residuals)

#The correlogram shows that the autocorrelations for the in-sample forecast errors do not exceed the significance bounds 
# for various lags; hence it is quite evident that there is no significant evidence of non-zero autocorrelations at 
# various lags


# now to double confirm whether the data has any significat non-zero autocorrelation, we can carry out a Ljung-Box test: 
Box.test(tnr_sarima110010f$residuals, lag=16, type="Ljung-Box")

# from Ljung-Box test resuts above with the p value as big as 0.41, it is preety clear that there is no significant 
# evidence of non-zero autocorrelations in  residual

# Now in order to check whether the forecast errors have constant variance over time, and are normally distributed 
# with mean zero, by making a time plot of the forecast errors and a histogram (with overlaid normal curve):

plot.ts(tnr_sarima110010f$residuals)

# From the time plot, it appear perhaps other than year 2005/6 it is plausible that the forecast errors have constant 
# variance over time.


#Now we'll plot the histogram of residubals with an overlaid normal curve that has mean zero and the same standard 
# deviation as the distribution of forecast errors. 
# Note the following plotForecastErrors() is created and defined above

plotForecastErrors(tnr_sarima110010f$residuals)

# It appears from the plot that residuals are aproximatly normally distributed with mean zero.


####### - END - ####################



#####================================================================================================================#####
##  Summary of the Report                                                                                               ##
###--------------------------------------------------------------------------------------------------------------------###
#   Forecasting Approach : Prediction with HoltWinters Triple Exponential Smoothing Method and ARIMA Method              #
#                                                                                                                        #
#   Nike's Revenue data collected for fiscal years 1999 thru 2008 show that there is gradual rise in trend and           #
#   as well seasonal repetitive pattern periodically over each financial year. Repetitive rise and fall pattern          #
#   over the fiscal year is observed.                                                                                    #
#                                                                                                                        #
#   Minimum revenue was recorded at 1913 during 2nd Quarter of 1999 Fiscal Year, while Maximum revenue was               #
#   recorded at 5088 4th Quarter of 2008 Fiscal Year. Average revenues for the period is 3094.                           #
#                                                                                                                        #
#   A simple prediction / forecasting called Regression Analysis was not satifactory due to Seaonal and Trend Variances  #
#                                                                                                                        #
#   Moving ahead with other techniques including HoltWinter TES and ARIMA Methods, lead to signicant improvement in the  #
#   forecasted results. From the plots we see the results aligning the Trend along the seasonality.                      #                                                                                                                    #
#####================================================================================================================#####







