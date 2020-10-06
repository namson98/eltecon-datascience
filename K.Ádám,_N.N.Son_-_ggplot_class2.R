############

#ggplot class
#Authors: Kovács Ádám József (FRBICG), Nguyen Nam Son (GES0XD)
#date created: 06-10-2020

############

# Class 4 -----------------------------------------------------------------
library(patchwork)
library(data.table)
library(ggplot2)
library(dplyr)

wd = file.path("~", "eltecon-datascience")
setwd(wd)

wc = fread("wc_summary_cleaned.csv")

p = ggplot(aes(year, conversion_rate, group = 1, color = factor(ISO_code)), data = wc) +
  geom_line() +
  theme_grey() +
  scale_fill_brewer(palette = "YlOrRd", labels = seq(1:length(unique(dt$ISO_code)))) 
pg = ggplot_build(p)
head(pg$data[[1]], 6)

extendPlot = function(plot,x,y, ttl = "Extended plot", sub = "Subtitle"){
  plot + 
    coord_cartesian(xlim = x, ylim = y) +
    labs(title = ttl,
         subtitle = sub)
}

cr_plot = ggplot(aes(shots_on_goal, goals_for), data = wc) +
  geom_jitter() + 
  geom_smooth(method = 'lm') +
  coord_cartesian(xlim = c(0,20), ylim = c(0, 10))

p + cr_plot

histplot = function(data, var, rangex, rangey){
  p <<- ggplot(aes_string(var), data = data) +
    geom_histogram() +
    coord_cartesian(xlim = rangex, ylim = rangey)
  
  if(max(p$coordinates$limits$x, na.rm = TRUE) > 40 | max(p$coordinates$limits$y, na.rm = TRUE) > 40){
    a = "The plot range is above 40"
  } else{
    a = "The plot range is below 40"
  }
  
  pg = p + labs(subtitle = a)
  
  return(pg)
}
###########  
#END OF CODE
###########