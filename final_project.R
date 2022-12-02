# final project - map of inflation across the U.S. States
library(tidyverse)
library(lubridate)
library(gganimate)
library(transformr)
library(RColorBrewer)
library(gifski)
library(usmap)
library(maps)
library(mapdata)

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
usdf <- usdf %>%
  filter(observation_date <= statedf$observation_date[[74]])
statedf <- inner_join(statedf, usdf)

# rename columns to states and inflation names
# columns 2-51 are state data
# columns 52-101 are state wage growth
# columns 102-106 are national inflation
# i renamed the inflation columns as follows
# cpi_total_gr = Consumer Price Index: Total All Items for the United 
#     States, Growth rate previous period, Annual, Not Seasonally 
#     Adjusted (CPALTT01USM657N)
# cpi_sticky_fe = Sticky Price Consumer Price Index less Food and Energy,
#     Percent Change from Year Ago, Annual, Seasonally Adjusted
#     (CORESTICKM159SFRBATL)
# cpi_urban_fe = Consumer Price Index for All Urban Consumers: All Items 
#     Less Food and Energy in U.S. City Average, Percent Change, Annual, 
#     Seasonally Adjusted (CPILFESL_PCH)
# personal_con_exp = Personal Consumption Expenditures: Chain-type Price 
#     Index, Percent Change, Annual, Seasonally Adjusted
#     (PCEPI_PCH)
# cpi_urban = Consumer Price Index for All Urban Consumers: All Items in 
#     U.S. City Average, Percent Change, Annual, Seasonally Adjusted
#     (CPIAUCSL_PCH)
namelist <- c("observation_date", state.name, 
              paste0(state.name, "_yoy"),
              "cpi_total_gr", "cpi_sticky_fe", "cpi_urban_fe",
              "personal_con_exp", "cpi_urban")
colnames(statedf) <- namelist
names(statedf) <- tolower(names(statedf))

# create empty dataframe for the 50 states
statesmapdf <- data.frame(
  observation_date = Date(),
  state_name = character(),
  personal_income = character(),
  personal_yoy = character(),
  cpi_total_gr = integer(),
  cpi_sticky_fe = integer(),
  cpi_urban_fe = integer(),
  personal_con_exp = integer(),
  cpi_urban = integer()
)
# fill dataframe by iterating through list of states
for (i in 1:50) {
  tempdf <- data.frame (
    observation_date = statedf$observation_date,
    state_name = tolower(state.name[i]),
    personal_income = statedf[i+1],
    personal_yoy = statedf[i+51],
    cpi_total_gr = statedf$cpi_total_gr,
    cpi_sticky_fe = statedf$cpi_sticky_fe,
    cpi_urban_fe = statedf$cpi_urban_fe,
    personal_con_exp = statedf$personal_con_exp,
    cpi_urban = statedf$cpi_urban
  )
  # rename the columns to be the same as statesmapdf
  colnames(tempdf) <- c("observation_date", 
                        "state_name", 
                        "personal_income",
                        "personal_yoy", 
                        "cpi_total_gr",
                        "cpi_sticky_fe",
                        "cpi_urban_fe",
                        "personal_con_exp",
                        "cpi_urban")
  # bind them onto the bottom of statesmapdf
  statesmapdf <- rbind(statesmapdf, tempdf)
}


##### creating graphs #####

# Animating personal income over time - line graph
p = ggplot()
for (i in 1:5) { # 1:10 plots the first 10 states
  p = p + geom_line(data = subset(statesmapdf, 
                                  state_name == state.name[i], 
                                  select = c("observation_date", 
                                             "state_name", 
                                             "personal_income")), 
                    aes(x = observation_date, 
                        y = personal_income,
                        color = state_name))
}
p <- p + scale_color_brewer(name="State", 
                            palette = "Paired") +
         xlab("Year") +
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


##### creating the year by year animation #####
##### merging the price data and state polygon data
statesf <- map_data('state')
colnames(statesf) <- c("long", "lat", "group", "order", "state_name", 
                       "subregion")
animate_mapdf <- left_join(statesf, statesmapdf, by = "state_name")
animate_map <- ggplot(data = animate_mapdf, aes(x = long, y = lat, 
                              fill = personal_income,
                              group = group)) +
  geom_polygon(color = 'white') +
  scale_color_brewer(type="div", palette=1) +
  theme_bw() + 
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), axis.title.y=element_blank(), 
        axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  coord_fixed(1.3) +
  ggtitle("Personal Income in {closest_state}") +
  transition_states(observation_date)
animate(animate_map, fps=18)

##### making a single frame for one year

##########################################################
########## deprecated code - don't breathe this ##########
##########################################################
# using the usmap package
plot_usmap(regions = "states", labels = TRUE) +
  labs(title = "U.S. States",
       subtitle = "We will create a heatmap.") +
  theme(panel.background=element_blank()) 


# create the dataframe to plot the year 2021
# plot_usmap requires a dataframe made of 2 columns, one called "state"
# state must have the abbreviations of the states
st <- data.frame (
  state = state.name,
  personal_inc = c(0)
)
# fill personal_inc with the personal income values of the states
for (i in 1:50) {
  # "state_name" must match state #i in state.name and the year we need
  st[["personal_inc"]][[i]] <- 
    statesmapdf["personal_income"][statesmapdf["state_name"]==state.name[i] 
                                   & statesmapdf["observation_date"]=="2021-01-01"]
}
usmap2021 <- plot_usmap(data = st, values = "personal_inc", 
                        color = "grey18") +
  scale_fill_brewer(palette = "Spectral") + 
  ggtitle("U.S. Median Per Capita Personal Income in {closest_state}") +
  theme(legend.position = "right", ) 
usmap2021

