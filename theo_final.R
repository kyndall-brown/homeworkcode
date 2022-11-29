# simpleVAR.R example
#
# 20220117 : Initial version
#
# Patrick T. Brandt
# UT Dallas, EPPS
#

# Load libraries
install.packages(c("fredr", "xts", "vars"))
library(fredr)
library(xts)
library(vars)
library(aTSA)

key <- "f00943b4cc68f3867f1762517ed4d467"

fredr_set_key(key)

install.packages("jsonlite")

# Simple data for a 3 equation VAR for US Economy, quarterly data

# CPI
CPI <- fredr(series_id = "CPIAUCSL", 
             observation_start = as.Date("1954-07-01"),
             frequency = "q", units = "pc1")

CPI <- xts(CPI$value, CPI$date)

dCPI <- (diff(CPI))

plot(diff(CPI))

# U
U <- fredr(series_id = "UNRATE", 
           observation_start=as.Date("1948-01-01"),
           frequency = "q", units="lin")
U <- xts(U$value, U$date)

plot(U)

plot(diff(U))

dU <- diff(U)

# GDP
Y <- fredr(series_id = "GDPC1",
           observation_start = as.Date("1948-01-01"),
           frequency = "q", units="pc1")
Y <- xts(Y$value, Y$date)

plot(Y)

acf(Y)

pacf(Y)

dY <- diff(Y)

plot(dY)

# FFR

R <- fredr(series_id = "FEDFUNDS", observation_start = as.Date("1954-07-01"),
           frequency = "q", units = "lin" )
R <- xts(R$value, R$date)

plot(R)

acf(R)
pacf(R)

dR <- diff(R)

plot(dR)

dR <- diff(log(R))

plot(dR)
# Plot
plot(cbind(dCPI,dU,dY,dR))


###########  theos project   ###########

# compare time series for fit
# arima, sarima and var
# using our national inflation and state personal income data
# test for fit and simplicity
# how well they forecast
# may do states, but focus on country if we cant do states

## import inflation and price data
library(tidyverse)
library(lubridate)
library(gganimate)
library(gifski)
library(usmap)

# import the data: it's all from FRED
# national inflation data
usdf <- read.csv("https://raw.githubusercontent.com/kyndall-brown/homeworkcode/main/inflationdata/national-inflation-data.csv")
# metropolitan statistical area personal income data
msadf <- read.csv("https://raw.githubusercontent.com/kyndall-brown/homeworkcode/main/inflationdata/per-capita-personal-income-msa.csv")
# state personal income data
statedf <- read.csv("https://raw.githubusercontent.com/kyndall-brown/homeworkcode/main/inflationdata/per-capita-personal-income-states.csv")

# data cleaning
msadf[msadf == "#DIV/0!"] <- NA
statedf[statedf == "#DIV/0!"] <- NA
usdf$observation_date <- as_date(dmy(usdf$observation_date))
statedf$observation_date <- as_date(ymd(statedf$observation_date))
Sys.setenv(TZ = 'GMT')
usdf$observation_date <- as.POSIXct(usdf$observation_date, tz="")
statedf$observation_date <- as.POSIXct(statedf$observation_date, tz="")
# drop states observations with no matching national inflation #s
statedf <- statedf %>%
  filter(observation_date >= usdf$observation_date[[1]])

# create empty dataframe for the 50 states
statesmapdf <- data.frame(
  observation_date = Date(),
  state_name = character(),
  personal_income = character(),
  personal_yoy = character()
)
# fill dataframe by iterating through list of states
for (i in 1:50) {
  tempdf <- data.frame (
    observation_date = statedf$observation_date,
    state_name = state.name[i],
    personal_income = statedf[i+1],
    personal_yoy = statedf[i+51]
  )
  # rename the columns to be the same as statesmapdf
  colnames(tempdf) <- c("observation_date", 
                        "state_name", 
                        "personal_income",
                        "personal_yoy")
  # bind them onto the bottom of statesmapdf
  statesmapdf <- rbind(statesmapdf, tempdf)
}

########## creating time series ##########
# for states
for (i in 2:51) {
  statedf[i] <- ts(data = statedf[i])
}
# for country level variables
for (i in 2:ncol(usdf)) {
  usdf[i] <- ts(data = usdf[i])
}

########## testing for stationarity ##########
# for just Texas
adf.test(statedf$TXPCPI)
# pvalues all very close to 1, fail to reject hypothesis

acf_tx <- acf(statedf$TXPCPI, lag.max = 12, plot=FALSE)
plot(acf_tx)

pacf_tx <- pacf(statedf$TXPCPI)

p = ggplot()
p = p + geom_line(data = subset(statesmapdf, 
                                state_name == "Texas", 
                                select = c("observation_date", 
                                           "state_name", 
                                           "personal_income")), 
                  aes(x = observation_date, 
                      y = personal_income)) + xlab("Year") + ylab("Per Capita Personal Income") +
    ggtitle("Income Growth in Texas, 1948 to 2021") + theme_bw()
p


# Set up the sample
Z <- cbind(dR,dY,dU,dCPI)["1954-10-01/2022-04-01",]

# Select a lag length
VARselect(Z, lag.max=8)

# Fit a model
V4x <- VAR(X, 4)

V8x <- VAR(X, 8)

# IRFs
V4.irf <- vars::irf(V4, n.ahead=12)

plot(V4.irf)

V8.irf <- vars::irf(V8, n.ahead=12)

plot(V8.irf)

fevd(V4x, n.ahead = 12)

fevd(V8x, n.ahead = 12)

# Set up the sample
X <- cbind(dCPI,dU,dY,dR)["1954-10-01/2022-04-01",]

# Select a lag length
VARselect(X, lag.max=8)

# Fit a model
V4 <- VAR(X, 4)

V8 <- VAR(X, 8)

# IRFs
V4.irf <- vars::irf(V4, n.ahead=12)

plot(V4.irf)

V8.irf <- vars::irf(V8, n.ahead=12)

plot(V8.irf)