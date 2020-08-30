library(gapminder)
library(dplyr)
library(tidyverse)
library(ggplot2)

fraction_by_continent <- gapminder %>%
  filter(year == 2002) %>%
  group_by(continent) %>%
  summarize(total_pop = sum(pop)) %>%
  mutate(mundial_pop = sum(total_pop), fraction = total_pop / mundial_pop)

ggplot(fraction_by_continent, aes(x = continent, y = total_pop)) +
  geom_col() +
  ggtitle("Total population by continent (2002)") 

pop_by_continent <- gapminder %>%
  group_by(continent, year) %>%
  summarize(total_pop = sum(pop))

ggplot(pop_by_continent, aes(x = year, y = total_pop, color = continent)) +
  geom_line() +
  expand_limits(y = 0) +
  ggtitle ("Total population by continent (1950-2007)")

gapminder_2007 <- gapminder %>%
  group_by(continent) %>%
  filter(year == 2007)
  

ggplot(gapminder_2007, aes(x = gdpPercap, y = pop, color= continent)) +
  geom_point() +
  scale_y_log10() +
  scale_x_log10() +
  ggtitle ("GDP & Population no correlation (2007)")

ggplot(gapminder_2007, aes(x = gdpPercap, y = lifeExp, color = continent, size = pop)) +
  geom_point() +
  scale_x_log10() +
  ggtitle ("GDP & life expectancy linear relationship (2007)") +
  facet_wrap(~continent)

gapminder_America <- gapminder %>%
  filter(country %in% c("Argentina", "Brazil", "Uruguay", "Chile", "Bolivia", "Paraguay", "Colombia", "Ecuador"))

ggplot(gapminder_America, aes(x = year, y = lifeExp, color = country)) +
  geom_line() +
  ggtitle("Life expectancy growth in South America (1950-2007)")

gapminder_selected <- gapminder %>% 
  filter(country %in% c("Japan", "Argentina", "China", "United States"))

ggplot(gapminder_selected, aes(x = year, y = gdpPercap, color = country)) +
  geom_line() +
  ggtitle("GDP growth in selected countries (1950-2007)")

#Countries with the largest life expectancy per continent (2007)
gapminder %>%
  group_by(continent) %>%
  filter(year == 2007) %>%
  top_n (1, lifeExp) %>%
  arrange(desc(lifeExp))

#Percent of global population by country (2007)
gapminder %>%
  filter(year == 2007) %>%
  transmute (country, continent, year, pop, total_pop = sum(pop), fraction = pop / total_pop) %>%
  arrange (desc(fraction))

#Country with the highest percent of population by continent (1952)
  gapminder %>%
    filter(year == 1952) %>%
    transmute (country, continent, year, pop, total_pop = sum(pop), fraction = pop / total_pop) %>%
    arrange (desc(fraction)) %>%
    group_by(continent) %>%
    top_n(1, fraction)

#Country with the highest percent of population by continent (2007)
gapminder %>%
  filter(year == 2007) %>%
  transmute (country, continent, year, pop, total_pop = sum(pop), fraction = pop / total_pop) %>%
  arrange (desc(fraction)) %>%
  group_by(continent) %>%
  top_n(1, fraction)

#Gapminder arrange by GDP Per capita (2007)
gapminder %>%
  filter(year == 2007) %>%
  group_by(continent) %>%
  arrange(desc(gdpPercap))
  
         