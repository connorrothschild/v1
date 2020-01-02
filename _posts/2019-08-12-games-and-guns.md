---
title: "Games and Guns"
date: "2019-08-12"
category: R
tags: [r, visualization]
comments: true
---

Is there a connection between video games and gun violence, as Republicans suggest?



### Load Packages


{% highlight r %}
library(readr)
library(readxl)
library(tidyverse)
library(ggplot2)
library(showtext)
library(emojifont)
library(cr)
conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("scale_colour_discrete", "cr")

set_cr_theme(font = "lato")
{% endhighlight %}

### Load Data

Data regarding gun deaths per capita comes from the [Institute for Health Metrics and Evaluation](https://vizhub.healthdata.org/gbd-compare/)


{% highlight r %}
guns <- read_csv("IHME-guns.csv")

guns <- guns %>% select(Location, Value)
{% endhighlight %}

Data regarding video game sales per capita comes from this [Google Spreadsheet](https://docs.google.com/spreadsheets/d/1n7VPylBiHov7gWwM4F070m5JZsB7fGOMMQsoAOIh5RA/edit?usp=sharing) which was pulled from NewZoo, a gaming analytics company.


{% highlight r %}
games <- read_excel("GameRevenueByCountry.xlsx")

games <- games %>% 
  rename(revenue = `PER CAPITA REVENUE`) %>% 
  select(Country, revenue)
{% endhighlight %}

### Merge and Clean Data


{% highlight r %}
joined <- left_join(games, guns, by = c("Country" = "Location"))
{% endhighlight %}

Next, we clean `games` dataset so that `Country` matches the `Location` column from `guns`.


{% highlight r %}
games <- games %>% 
  mutate(Country = case_when(Country == "Republic of Korea" ~ "South Korea",
                             Country == "Brunei Darussalam" ~ "Brunei",
                             #Country == "Macao" ~ ,
                             #Country == "Hong Kong, China" ~ ,
                             Country == "Lucembourg" ~ "Luxembourg",
                             Country == "Kuwair" ~ "Kuwait",
                             Country == "UAE" ~ "United Arab Emirates",
                             Country == "TFYR Macedonia" ~ "Macedonia",
                             Country == "Joran" ~ "Jordan",
                             Country == "Republic of Moldova" ~ "Moldova",
                             TRUE ~ as.character(Country)))
{% endhighlight %}


{% highlight r %}
joined <- left_join(games, guns, by = c("Country" = "Location"))
{% endhighlight %}

There are 98 countries with full data present. 

We should also create a dummy variable for each country depending on whether it is an OECD country or not.


{% highlight r %}
Country <- c(
"Austria",
"Belgium",
"Canada",
"Denmark",
"France",
"Greece",
"Iceland",
"Ireland",
"Italy",
"Luxembourg",
"Netherlands",
"Norway",
"Portugal",
"Spain",
"Sweden",
"Switzerland",
"Turkey",
"United Kingdom",
"United States",
"West Germany",
"Australia",
"Finland",
"Japan",
"New Zealand")

OECD <- "OECD"

oecd <- data.frame(Country, OECD)
{% endhighlight %}


{% highlight r %}
oecd_joined <- left_join(joined, oecd, by = "Country")

oecd_joined <- oecd_joined %>% 
  mutate(OECD = ifelse(is.na(OECD), "Not OECD", "OECD"))
{% endhighlight %}

### Visualize

This allows us to plot each country in a scatterplot, with point colour corresponding to OECD status:


{% highlight r %}
oecd_joined %>% 
  ggplot(aes(x = revenue, y = Value, colour = factor(OECD))) +
  geom_point() +
  geom_text(aes(label = ifelse(Country == "United States", as.character(Country),''), vjust = -1), show.legend = FALSE) +
  geom_text(aes(label = ifelse(Value > 40, as.character(Country),''), vjust = -1), show.legend = FALSE) +
  geom_text(aes(label = ifelse(revenue > 150, as.character(Country),''), vjust = -1), show.legend = FALSE) +
  labs(x = "Video Game Revenue per Capita (US $)",
       y = "Violent Gun Deaths per 100k",
       title = "Gun Deaths vs Game Sales",
       colour = element_blank(),
       caption = "\nSources: Institute for Health Metrics and Evaluation, NewZoo") +
  scale_y_continuous(limits = c(0, 45)) +
  theme(plot.caption = element_text(face = "italic", hjust = 0), 
        legend.position = "top", legend.direction = "horizontal") +
  drop_axis(axis = "y")
{% endhighlight %}

![center](/figs/2019-08-12-games-and-guns/unnamed-chunk-9-1.png)

Finally, we can focus on only OECD countries:


{% highlight r %}
oecd_joined %>% 
  dplyr::filter(OECD == "OECD") %>% 
  ggplot(aes(x = revenue, y = Value)) +
  geom_point() +
  #geom_smooth() +
  geom_text(aes(label = ifelse(Country == "United States", as.character(Country),''), vjust = -1)) +
  labs(x = "Video Game Revenue per Capita (US $)",
       y = "Violent Gun Deaths per 100k",
       title = "Gun Deaths vs Game Sales",
       subtitle = "OECD Countries",
       caption = "\nSources: Institute for Health Metrics and Evaluation, NewZoo") +
  scale_y_continuous(limits = c(0, 5)) +
  theme(plot.caption = element_text(face = "italic", hjust = 0)) +
  drop_axis(axis = "y")
{% endhighlight %}

![center](/figs/2019-08-12-games-and-guns/unnamed-chunk-10-1.png)

To conclude, let's add an emoji to fully capture our skepticism with the [newfound argument](https://www.cnn.com/2019/08/05/politics/kevin-mccarthy-mass-shootings-video-games/index.html) linking video games to violence.


{% highlight r %}
oecd_joined %>% 
  dplyr::filter(OECD == "OECD") %>% 
  ggplot(aes(x = revenue, y = Value)) +
  geom_point() +
  #geom_smooth() +
  geom_text(aes(label = ifelse(Country == "United States", as.character(Country),''), vjust = -1)) +
  labs(x = "Video Game Revenue per Capita (US $)",
       y = "Violent Gun Deaths per 100k",
       title = "Gun Deaths vs Game Sales",
       subtitle = "OECD Countries",
       caption = "\nSources: Institute for Health Metrics and Evaluation, NewZoo") +
  scale_y_continuous(limits = c(0, 5)) +
  theme(plot.caption = element_text(face = "italic", hjust = 0)) +
  drop_axis(axis = "y") +
  geom_text(y = 4.85, x = 107.5, size = 7, label = emoji('thinking'), family = "EmojiOne")
{% endhighlight %}

![center](/figs/2019-08-12-games-and-guns/unnamed-chunk-11-1.png)
