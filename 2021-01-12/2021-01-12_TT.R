library(tidyverse)
library(dplyr)
library(ggplot2)
library(gganimate)
library(viridis)
library(wbstats)
library(png)
library(gifski)
library(tidytuesdayR)
library(lubridate)
library(ggthemes)
library(readxl)

library(ggpubr)
library(hrbrthemes)
library(viridis)


## Import data
tuesdata <- tidytuesdayR::tt_load('2021-01-12')
artwork <- tuesdata$artwork

artists <- tuesdata$artists
num_artists <- unique(artists$name)
## 3,532 artisits. I really want to look at race data, but that would take a
## long time to compile. Maybe I could build out the race data for those born in 
## the US? What proportion of artists were born in different countries?

## Seperate out city and country of artist's birth
artists <- artists %>%
  mutate(country = unlist(lapply(strsplit(placeOfBirth, ", "), function(x) x[2])), 
         placeOfBirth = gsub(",.*", "", placeOfBirth))

countries <- unique(artists$country)
## 90 countries represented

## How many artists from each country? 
artists_country <- data.frame(table(artists$country))
artists_country <- arrange(artists_country, desc(Freq))
artists_country[2, ]
artists_country$prop <- (artists_country$Freq/3532)*100

## There are so many countries, let's look at countries that have 10 or more artists represented: 
artists_country <- filter(artists_country, Freq > 9)
ggplot(artists_country, aes(x = reorder(Var1, -Freq), y = Freq, color = Var1)) + 
  geom_bar(stat = "identity") + 
  scale_color_viridis(
    discrete = TRUE, name = "Name", option = "viridis", guide = FALSE)

## There are 331 artists listed in the US. Is that feasible to get race data
## on each person? Let's try?! 

us_artists <- filter(artists, country == "United States")
us_artists_names <- us_artists$name

## Race data for 331 US artists represented in the Tate Modern
## I googled every 331 US born artist and looked at their image. If I couldn't
## find a photo quickly enough, I marked as "unknown". There might be errors. 

us_artists2 <- read_excel("us_artists2.xlsx")

## Lets bind the new race data into the us_artists data

us_artists <- cbind(us_artists, us_artists2$race)

us_artists <- rename(us_artists, race = 'us_artists2$race')

## Factor the race
us_artists$race <- 
  factor(us_artists$race, 
         levels=c("White", "Black", "Unknown"),
         labels=c("White", # Reference
                  "Black", 
                  "Unknown"))

## Merge dataframes 
us_artists <- rename(us_artists, artistId = id)
merged <- merge(artwork, us_artists, by = "artistId")

## Plot the acquisition year by gender & race
merged_f <- filter(merged, gender == "Female")
merged_m <- filter(merged, gender == "Male")

f <- count(merged, artist, acquisitionYear, race)

g <- ggplot(f, aes(x = acquisitionYear, y = n, color = race)) + 
geom_point() + 
  labs(x = "Year of Acquisition",                
       y = "Pieces of artwork aquired per artist", 
       main = "Who's Art Are We Collecting?") + 
  theme_economist() +
  scale_color_economist(name = NULL)

g + scale_color_manual(values = c("#708098", "#F2E3EA", "#DCE3EF"))

## Plot the number of pieces of artwork acquired by year and artist race
h <- ggplot(blerp, aes(acquisitionYear, y = n, color = race)) +
  geom_density(adjust=1.5, alpha=.4) +
  theme_ipsum()

black <- filter(f, race == "Black")

blerp %>%
  ggplot( aes(x=acquisitionYear, y=n, group=race, color=race)) +
  geom_area() + 
  scale_color_viridis(discrete = TRUE) +
  ggtitle("US Born Artist's Artwork Acquired by Race") +
  theme_ipsum() +
  ylab("Number of artowrks acquired") + 
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8),
    plot.title = element_text(size=14)
  ) 

## Stream plot? 
blerp <- filter(blerp, acquisitionYear >1950 & race != "Unknown")

blerp <- blerp  %>%
  group_by(acquisitionYear, race) %>%
  summarise(x = sum(n)) %>%
  mutate(percentage = x / sum(x))

g <- ggplot(blerp, aes(x=acquisitionYear, y=percentage, fill=race)) + 
  geom_area(alpha=0.8 , size=1, colour="white") + 
  ## scale_fill_viridis(discrete = T, option = "cividis") +
  scale_fill_manual(values = c("#192F5E", "#77B2C5")) + 
  theme_void() + 
  ggtitle("Proportion of US born artisits' artwork acquired over time by race") + 
  xlab("Acquisition Year") + ylab("") + 
  labs(fill = "Race", 
       caption = "Data Source: Tate Art Museum (github.com/tategallery/collection) | Design: Jessica Dyer")

ggsave(filename = "TATE_race.png", plot = g)




  