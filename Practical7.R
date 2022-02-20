library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)


gapminder_orig <- read_csv("gapminder.csv")
seps_orig <- read_csv('seps.csv')

gapminder <- gapminder_orig
seps <- seps_orig

#QUESTION 1
#Q1a
q1a <- gapminder %>%
       filter(year==2007,continent=="Americas") %>%
       select(country,lifeExp)

#Q1b
q1b <- gapminder %>% arrange(gdpPercap)

#Q1c
q1c <- gapminder %>% arrange(desc(gdpPercap))


#Q1d
q1d <- gapminder %>% mutate(gdp = (pop*gdpPercap))

#Q1e
q1e <- gapminder %>%
       group_by(continent) %>%
       summarize(mean_life_exp = mean(lifeExp))

#Q1f
q1f <- gapminder %>%
       group_by(year) %>%
       summarize(mean_life_exp = mean(lifeExp))

#Q1g
q1g <- gapminder %>%
       group_by(continent) %>%
       filter(lifeExp > mean(lifeExp)) %>%
       summarize(n = n())

#Q1h
q1h <- gapminder %>% mutate(mean_life_exp = mean(lifeExp), high_life_expectancy = if_else(lifeExp > mean_life_exp, 1,0))

#QUESTION 2
q2a <- seps %>%
       gather(year,value,FY1993:FY1998)

q2b <- spread(q2a,Field,value)