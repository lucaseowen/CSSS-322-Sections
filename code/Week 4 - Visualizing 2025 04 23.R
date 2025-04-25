#### PREAMBLE ####
library(dplyr)
library(ggplot2)
library(readr)

#### LOAD DATA ####

mov <- read_csv(unz("data/322/imdb/movies_initial.csv.zip", "movies_initial.csv"))

gap <- read.csv("data/321/gapminder.csv")
pol <- read.csv("data/321/polity2.csv")


#### JOIN DATA ####

#Subset imdb to only movies
unique(mov$type)
mov <- mov[mov$type=="movie",]
#remove shorts - some are just one minute!
mov <- mov[!grepl("Short", mov$genre), ]
#group by year and country to match with gapminder and polity
mov_sum <- mov %>% 
  group_by(country, year) %>% 
  summarize(number_of_movies = n())
#let's do a dataset just by country too
mov_countries <- mov %>% 
  group_by(country) %>% 
  summarize(number_of_movies = n())
#and let's drop observations with commas
mov_countries <- mov_countries[!grepl(",", mov_countries$country), ]


#clean country names
sort(unique(mov_sum$country))
sort(unique(gap$country))
sort(unique(pol$country))
mov_sum$country[mov_sum$country=="USA"] <- "United States"
mov_sum$country[mov_sum$country=="UK"] <- "United Kingdom"
#any others?

#now let's join
df <- left_join(gap, pol, by=c("country", "year"))
# df <- left_join(df, mov_sum, by=c("country", "year")) #got an error!
class(df$year)
class(mov_sum$year)
mov_sum$year <- as.numeric(mov_sum$year)
df <- left_join(df, mov_sum, by=c("country", "year"))

#we want to set NAs to zero in this case only because we know we had a pretty full dataset of movies!
df$number_of_movies[is.na(df$number_of_movies)] <- 0

rm(mov, pol, gap, mov_sum) #now let's remove objects so they don't take up space!
gc() #still need to clear RAM

#### PLOTS ####

#let's make some nice plots!