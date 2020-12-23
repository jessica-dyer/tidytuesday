library(tidyverse)
library(ggplot2)
library(gganimate)
library(viridis)
library(wbstats)
library(png)
library(gifski)
library(tidytuesdayR)
library(lubridate)

## Import data
tuesdata <- tidytuesdayR::tt_load('2020-12-22')
big_mac <- tuesdata$`big-mac`

## Variables of interest
bubble_variables <- c("date", "name", "dollar_price", "gdp_dollar")
indicator <- c("dollar_price", "gdp_dollar")

## Create data frame
df <- select(big_mac, all_of(bubble_variables))
keep <- complete.cases(df)
df <- subset(df, keep == "TRUE")
df$year <- year(df$date)
df <- select(df, -(date))
df$gdp_log <- log(df$gdp_dollar)

## Look for duplicate entries by country name and year
df_dist <- distinct(df, name, year, .keep_all = TRUE )

## Create bubble plot 
p <- ggplot(df_dist, aes(gdp_log, dollar_price, size = dollar_price, color = name, label = name)) + 
  geom_point(alpha = 0.5) +  
  scale_size(range = c(.1, 16), guide = FALSE) + 
  
  scale_color_viridis(
  discrete = TRUE, name = "Name", option = "viridis", guide = FALSE) + 

  geom_text(check_overlap = TRUE, size = 5) +

  labs(x = "Log GDP per capita in USD",                
        y = "Cost of Big Mac in USD", 
        main = "Stuff") + 
  
  theme_classic() +  
  
  ggplot2::geom_text(aes(x = 8, y = 8, label = year), 
                     size = 14, color = 'lightgrey') +
  
  transition_states(year, transition_length = 1, 
                    state_length = 1) + 
  gganimate::ease_aes('cubic-in-out')


## Animate
animate(p, renderer = gifski_renderer())
anim_save("big_mac.gif", path = "C:/Users/jessd9/Repositories/tidytuesday/2020-12-22")


