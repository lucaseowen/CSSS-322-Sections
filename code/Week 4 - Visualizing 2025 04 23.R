#### PREAMBLE ####
library(dplyr)
library(ggplot2)
library(readr)

#### SET PLOT THEME ####

my_theme <- theme_minimal() +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.caption.position = "plot",
    plot.caption = element_text(hjust = 0)
  )
theme_set(my_theme)


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

rm(pol, gap) #now let's remove objects so they don't take up space!
gc() #still need to clear RAM

movies <- read_csv(unz("data/322/imdb/movies_initial.csv.zip", "movies_initial.csv"))

#### PLOTS ####

ggplot(data=movies, aes(x=imdbRating))+
  geom_histogram()+
  xlab("IMDB Rating")

t <- as.data.frame(table(movies$imdbRating))
mode <- t$Var1[t$Freq==max(t$Freq, na.rm=T)]
mode <- 7.2
#let's make some nice plots!
rating_mean <- mean(movies$imdbRating, na.rm=T)
ggplot(data=movies, aes(x=imdbRating))+
  
  #create geometric layers
  geom_density(fill="lightgrey", alpha=0.3, linewidth=1.1)+
  geom_vline(xintercept=mode, linetype=2, color="purple")+
  geom_vline(xintercept=rating_mean, linetype=2, color="darkorange")+
  annotate("text", x = 7.2, y = 0.05, label = "Mode", color = "purple", angle = 90, vjust = -0.5)+
  annotate("text", x = rating_mean, y = 0.05, label = "Mean", color = "darkorange", angle = 90, vjust = -0.5)+
  
  #edit general plot visuals
  xlab("IMDB Rating")+
  ylab("Proportion of movies")+
  scale_x_continuous(limits=c(1,10), breaks=c(seq(from=1, to=10, by=1), 7.2, round(rating_mean, 1)))+
  labs(caption="The distribution of imdb ratings for all 46,014 movies in our data. The lowest score a movie can get is 1, and the highest is 10.")+
  theme_minimal()+
  theme(
    panel.grid.minor.x = element_blank(),   # remove minor vertical grid lines
    panel.grid.minor.y = element_blank(),   # remove minor vertical grid lines
    plot.caption.position = "plot",
    plot.caption = element_text(hjust = 0)
    )
ggsave("imdb_rating_density.pdf", width=7, height=5)

#### PIE CHART ####

library(ggplot2)
library(dplyr)

# Example: count movies by country
movies_pie <- movies %>%
  count(type) %>%
  arrange(desc(n)) %>%
  mutate(prop = n / sum(n),
         label = paste0(type, " (", round(prop * 100, 1), "%)"))

# Create the pie chart
ggplot(movies_pie, aes(x = "", y = prop, fill = type)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  labs(x = NULL, y = NULL, fill = "Country") +
  theme_void() +  # removes background and axes
  theme(legend.position = "right")




