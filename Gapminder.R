library(gapminder)
library(dplyr)
library(tidyverse)

fraction_by_continent <- gapminder %>%
  filter(year == 2002) %>%
  group_by(continent) %>%
  summarize(total_pop = sum(pop)) %>%
  mutate(mundial_pop = sum(total_pop), fraction = total_pop / mundial_pop)

ggplot(fraction_by_continent, aes(x = continent, y = total_pop)) +
  geom_col() +
  ggtitle("Población total por continente (2002)")

pop_by_continent <- gapminder %>%
  group_by(continent, year) %>%
  summarize(total_pop = sum(pop))

ggplot(pop_by_continent, aes(x = year, y = total_pop, color = continent)) +
  geom_line() +
  expand_limits(y = 0)

gapminder_2007 <- gapminder %>%
  group_by(continent) %>%
  filter(year == 2007)
  

ggplot(gapminder_2007, aes(x = gdpPercap, y = pop, color= continent)) +
  geom_point() +
  scale_y_log10() +
  scale_x_log10()

ggplot(gapminder_2007, aes(x = gdpPercap, y = lifeExp, color= continent)) +
  geom_point() +
  scale_x_log10()

gapminder_America <- gapminder %>%
  filter(country %in% c("Argentina", "Brazil", "Uruguay", "Chile", "Bolivia", "Paraguay", "Colombia", "Ecuador"))

gapminder_America

ggplot(gapminder_America, aes(x = year, y = lifeExp, color = country)) +
  geom_line()

gapminder_selected <- gapminder %>% 
  filter(country %in% c("Japan", "Argentina", "China", "United States"))

ggplot(gapminder_selected, aes(x = year, y = gdpPercap, color = country)) +
  geom_line()


         