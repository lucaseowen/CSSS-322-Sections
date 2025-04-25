#### PREAMBLE ####
library(dplyr)
library(ggplot2)
library(readr)

#### LOAD DATA ####

mov <- read_csv(unz("data/322/imdb/movies_initial.csv.zip", "movies_initial.csv"))

gap <- read.csv("data/321/gapminder.csv")
pol <- read.csv("data/321/polity2.csv")


#### JOIN DATA ####

max(gap$year, na.rm=T)
max(pol$year, na.rm=T)
#let's set gap and pol to 2007
gap <- gap[gap$year==2007,]
pol <- pol[pol$year==2007,]

#let's set mov to less than 2007 as well
class(mov$year)
mov$year <- as.numeric(mov$year)
mov <- mov[mov$year<=2007,]

#Subset imdb to only movies
unique(mov$type)
mov <- mov[mov$type=="movie",]
#remove shorts - some are just one minute!
mov <- mov[!grepl("Short", mov$genre), ]
#let's do a dataset just by country to match with gapminder and polity
mov <- mov %>% 
  group_by(country) %>% 
  summarize(number_of_movies = n())

#clean country names
sort(unique(mov$country))
sort(unique(gap$country))
sort(unique(pol$country))
mov$country[mov$country=="USA"] <- "United States"
mov$country[mov$country=="UK"] <- "United Kingdom"
#any others?

#now let's join
df <- left_join(gap, pol, by="country")
df <- left_join(df, mov, by="country")

rm(mov, pol, gap) #now let's remove objects so they don't take up space!
gc() #still need to clear RAM

#### PLOTS ####

#let's make some nice plots!