# final project - map of inflation across the U.S. States
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


##### creating graphs #####

# Animating personal income over time - line graph
p = ggplot()
for (i in 1:10) { # 1:10 plots the first 10 states
  p = p + geom_line(data = subset(statesmapdf, 
                                  state_name == state.name[i], 
                                  select = c("observation_date", 
                                             "state_name", 
                                             "personal_income")), 
                    aes(x = observation_date, 
                        y = personal_income,
                        color = state_name))
}
p <- p + scale_color_brewer(name="State", palette = "Paired") + xlab("Year") + 
  ylab("Per Capita Personal Income") + 
  ggtitle("Income Growth by State, 1948 to 2021") + theme_bw()
p
# creates the gganim object we can manipulate
anim_al_ga <- p + transition_reveal(along = observation_date)
# creates the animation as a GIF, doesnt save tho
animate(plot = anim_al_ga,
        width = 600,
        height = 400,
        end_pause = 20,
        res = 120)

#usmap
plot_usmap(regions = "states", labels = TRUE) +
  labs(title = "U.S. States",
       subtitle = "We will create a heatmap.") +
  theme(panel.background=element_blank()) 

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
  personal_inc = c(0)
)
# fill personal_inc with the personal income values of the states
for (i in 1:50) {
  # "state_name" must match state #i in state.name and the year we need
  st[["personal_inc"]][[i]] <- 
    statesmapdf["personal_income"][statesmapdf["state_name"]==state.name[i] 
            & statesmapdf["observation_date"]=="2021-01-01"]
}
# i recommend reading the documentation about usmap, it is pretty helpful:
# https://www.rdocumentation.org/packages/usmap/versions/0.6.1/topics/plot_usmap
plot_usmap(data = st, values = "personal_inc", color = "red") +
  scale_fill_continuous(low = "white", high = "red", 
                        name = "personal income") + 
  theme(legend.position = "right")






