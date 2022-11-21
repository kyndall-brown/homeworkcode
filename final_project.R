# final project - map of inflation across the U.S. States

# import the data
# national inflation data
usdf <- read.csv("https://raw.githubusercontent.com/kyndall-brown/homeworkcode/main/inflationdata/national-inflation-data.csv")
# metropolitan statistical area personal income data
msadf <- read.csv("https://raw.githubusercontent.com/kyndall-brown/homeworkcode/main/inflationdata/per-capita-personal-income-msa.csv")
# state personal income data
statedf <- read.csv("https://raw.githubusercontent.com/kyndall-brown/homeworkcode/main/inflationdata/per-capita-personal-income-states.csv")

# create dataframe for the 50 states
statesmapdf <- data.frame(
  observation_date = c(""),
  state_name = c(""),
  personal_income = c(""),
  personal_yoy = c("")
)

for (i in 1:50) {
  tempdf <- data.frame (
    observation_date = statedf$observation_date,
    state_name = state.name[i],
    personal_income = statedf[colnames(statedf)[i]],
    personal_yoy = statedf[colnames(statedf)[i+50]]
  )
  for (j in 1:nrow(tempdf)) {
    statesmapdf[nrow(statesmapdf)+1,] = tempdf[j]
  }
}



