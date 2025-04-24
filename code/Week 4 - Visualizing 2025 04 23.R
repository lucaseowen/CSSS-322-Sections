#### PREAMBLE ####
library(dplyr)
library(ggplot2)

#### LOAD DATA ####

mov <- read.csv("data/322/movie_data.csv")

head(mov)

#### CLEAN ####

#check NA values - #N/A, etc.
mov <- read.csv("data/322/movie_data.csv", na.strings = c("", "NA", "#N/A"))
mov$Country[mov$Country=="Russian Federation"] <- "Russia"

#### SUBSET ####

#Type
unique(mov$Type)
mov <- mov[mov$Type %in% c("movie", "tvMovie"),]


#### IMDB v. LETTERBOXD RATINGS PLOT ####

ggplot(data=mov, aes(x=imdb.rating, y=Letterboxd.Rating))+
  geom_point()

#lets make it nice!

#### TOP COUNTRIES PLOT ####



