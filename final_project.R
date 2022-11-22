# final project - map of inflation across the U.S. States
library(tidyverse)
library(lubridate)
library(gganimate)
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
dmy(usdf$observation_date)
ymd(statedf$observation_date)
# drop states observations with no matching national inflation #s
statedf <- statedf %>%
  filter(ymd(observation_date) >= dmy(usdf$observation_date[[1]]))

# create empty dataframe for the 50 states
statesmapdf <- data.frame(
  observation_date = character(),
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
  colnames(tempdf) <- c("observation_date", "state_name", "personal_income",
                        "personal_yoy")
  # bind them onto the bottom of statesmapdf
  statesmapdf <- rbind(statesmapdf, tempdf)
}


######## test code, doesn't really work yet #########
# Animating personal income over time
p = ggplot()
for (i in 1:10) {
  p = p + geom_line(data = statesmapdf, aes(x = observation_date,
                                            y = state.name[i]), 
                    color = )
}

#usmap
library(usmap)
plot_usmap(regions = "states", labels = TRUE) +
  labs(title = "U.S. States",
       subtitle = "We will create a heatmap.") +
  theme(panel.background=element_blank()) 

library(usmap)
library(ggplot2)
# create the dataframe to plot the year 2021
# plot_usmap requires a dataframe made of 2 columns, one called "state"
# state must have the abbreviations of the states
st <- data.frame (
  state = c("AL", "AK", "AZ", "AR", "CA", "CO", 
           "CT","DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", 
           "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", 
           "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", 
           "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", 
           "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY"),
  personal_inc = c("")
)
# fill personal_inc with the personal income values of the states
for (i in 1:50) {
  st[["personal_inc"]][[i]] <- 
    # "state_name" must match state #i in state.name and the year we need
    statesmapdf["personal_income"][statesmapdf["state_name"]==state.name[i] 
            & statesmapdf["observation_date"]=="2021-01-01"]
}
# i recommend reading the documentation about usmap, it is pretty helpful:
# https://www.rdocumentation.org/packages/usmap/versions/0.6.1/topics/plot_usmap
plot_usmap(data = st, values = "personal_inc", color = "red") +
  scale_fill_continuous(low = "white", high = "red", 
                        name = "personal income") + 
  theme(legend.position = "right")






