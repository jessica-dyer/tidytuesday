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

## There are 331 artists listed in the US. Is that feasible to get racial data
## on each person? Let's try? 

us_artists <- filter(artists, country == "United States")
us_artists_names <- us_artists$name



