############

#datatable-ggplot hw
#Authors: Kovács Ádám József (FRBICG), Nguyen Nam Son (GES0XD)
#date created: 18-09-2020

############
#EDA

library(data.table)

setwd("F:\\Dropbox\\!ELTECON BA (18'-21')\\5th Semester (39 credits)\\Data Science (Regional Economics)\\hw")
dt = fread("All World Cup team summary stats.csv")

summary(dt) #values are strictly non-negative, and no outliers identified

removeNaRows = function(dt) {
  dt <<- na.omit(dt)
}

renameColumns = function(dt) {
  names(dt) <<- c("year", 
           "team", 
           "ISO_code",
           "goals_for",
           "goals_against",
           "penalties",
           "nmatches",
           "shots_on_goal",
           "shots_wide",
           "free_kicks",
           "offsides",
           "corners",
           "won",
           "draw",
           "lost")
}

missingIsoCode = function(dt) {
  dt[ISO_code == "-", ISO_code := NA]
}

# fixing the encoding of French letters
frenchLetters = function(dt) {
  dt[team == "CÃ´te d'Ivoire", team := "Cote d'Ivoire"]
}

removeDuplicates = function(dt) {
  dt = unique(dt)
}

removeNaRows(dt)
renameColumns(dt)
missingIsoCode(dt)
frenchLetters(dt)
removeDuplicates(dt)

# looking for the ISo codes which are assigned to multiple countries                 
isounique = dt[, .(unique(team), uniqueN(team)>1), by = ISO_code]
isounique = isounique[isounique$V2 == TRUE]
View(isounique)

# correcting the ISO codes (fyi: those countries which are ignored are either correct or doesn't have ISO codes)
fixIsoCode = function(dt) {
  dt[team == "Italy", ISO_code := "IT"]
  dt[team %in% c("England", "Scotland", "Northern Ireland", "Wales"), ISO_code := "GB"]
}

fixIsoCode(dt)

#final cleaned datatable
View(dt)

#############
#DATAVIZ PART 1 and 2

#ggplot
library(ggplot2)

ggplot(data = dt) +
  geom_histogram(aes(goals_for), fill = 'blue', binwidth = 3) +
  labs(title = "Distribution of scored goals",
       xlab = "Goals Scored") +
  coord_cartesian(xlim = c(0, 35))

ggplot(data = dt) +
  geom_col(aes(shots_on_goal, nrow(dt)))

ggplot(data = dt) +
  geom_bar(aes(shots_on_goal))

ggplot(data = dt) +
  geom_jitter(aes(shots_on_goal, goals_for))

#showing connection between shots on goal and goal scored
ggplot(data = dt) +
  geom_point(aes(shots_on_goal, goals_for, color = won), alpha = 0.3) +
  geom_hline(yintercept = 0) +
  labs(title = "Conversion rate",
       subtitle = "Observing the conncetion between shots on target and goal scored",
       caption = "made by K.Á and N.N.S.",
       xlab = "Shots on target",
       ylab = "Goals scored",
       color = "Won matches") +
  coord_cartesian(xlim = c(0,20), ylim = c(0, 10))
  
ggplot(data = dt) +
  geom_point(aes(shots_on_goal, goals_for), alpha = 0.3) +
  facet_wrap(~ won)

ggplot(data = dt) +
  geom_violin(aes(shots_on_goal, goals_for)) +
  coord_flip() +
  scale_y_continuous(breaks = seq(0, 30, 5)) +
  scale_x_continuous(labels = function(x){paste(x, "$")})
  
#END OF CODE
###########