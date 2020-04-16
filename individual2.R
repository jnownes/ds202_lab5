#LAB 5
library(lubridate)
library(dplyr)
library(tidyverse)

acc <- read.csv("https://raw.githubusercontent.com/xdaiISU/ds202materials/master/hwlabs/fars2017/accident.csv", stringsAsFactors = FALSE)

person <- read.csv("https://raw.githubusercontent.com/xdaiISU/ds202materials/master/hwlabs/fars2017/person.csv", stringsAsFactors = FALSE)

View(acc)
View(person)


#PART ONE:

#Question 1
table(acc$DAY_WEEK)
#Most accidents seem to occur on during the weekend like on friday saturday sunday.
names(acc)


acc %>%
  group_by(DAY_WEEK) %>%
  summarise(total= sum(DAY_WEEK)) %>%
  ggplot(acc, mapping = aes(x=DAY_WEEK, y=total)) + geom_col() + xlab("Day of week") + ylab("Total number of accidents") + ggtitle("Total number of accidents based on weekday")

#Question 2


#PART 2:

#Question 5:
combined <- acc %>%
  full_join(person, by = c('STATE', 'ST_CASE', 'COUNTY', 'DAY', 'MONTH', 'HOUR', 'MINUTE'))

View(person)
names(combined)

#Question 6
combined$SEX <- as.factor(combined$SEX)

combined %>%
  filter(SEX == 1 | SEX ==2) %>%
  filter(HOUR <= 24) %>%
  ggplot(combined, mapping =aes(x= HOUR, fill=wday(DAY_WEEK, label = TRUE))) + geom_bar() + facet_wrap(~SEX) + labs(fill = "Day of Week") + xlab('Hour of Day') + ylab('Number of accidents') + ggtitle('Accidents by Day of week, hour, and sex')


combined %>%
  filter(SEX == 1 | SEX == 2) %>%
  filter(HOUR < 24) %>%
  ggplot(combined, mapping =aes(x= HOUR, fill=SEX)) + geom_bar() + facet_wrap(~wday(DAY_WEEK, label = TRUE)) + scale_fill_discrete(name = "SEX", labels = c('Male', 'Female')) + xlab('Hour of Day') + ylab('Number of accidents') + ggtitle('Accidents by Day of week, hour, and sex')

#PART THREE:

#Question 7/8
library("readxl")

US_GLC<- readxl::read_xlsx('FRPP_GLC_UnitedStatesFeb132020.xlsx')
county<- map_data('county')


#2
US_GLC2 <- US_GLC %>%
  mutate(`State Code` = as.integer(`State Code`), `County Code` = as.integer(`County Code`), `City Code` = as.integer(`City Code`)) %>%
  inner_join(combined, c(`County Code` = 'COUNTY', `State Code` = 'STATE', `City Code` = 'CITY'))

ggplot(county, mapping=aes(x=long, y=lat)) + geom_polygon(mapping = aes(group=group, fill=)) + geom_point(US_GLC2, mapping = aes(x=LONGITUD, y=LATITUDE), color='lightgreen', alpha=.2, size=.02) + xlim(c(-130, -60)) + ylim(c(20,50))

View(US_GLC2)


#Question 9
ggplot(county, mapping = aes(x=long, y=lat)) + geom_polygon(mapping=aes(group=group)) + geom_point(US_GLC2 %>% filter(MONTH %in% c(12,1,2)), mapping=aes(x=LONGITUD, y= LATITUDE), color='light green', alpha=.2, size=0.02) + xlim(c(-130, -60)) + ylim(c(20,50))

ggplot(county, mapping = aes(x=long, y=lat)) + geom_polygon(mapping=aes(group=group)) + geom_point(US_GLC2 %>% filter(MONTH %in% c(6,7,8)), mapping=aes(x=LONGITUD, y= LATITUDE), color='light green', alpha=.2, size=0.02) + xlim(c(-130, -60)) + ylim(c(20,50))


accident <- acc %>%
  filter(WEATHER<98) 

weather <- accident %>%
  group_by(STATE, WEATHER) %>%
  summarise(n = n()) 

ggplot(weather, mapping = aes(x=STATE, y=n, fill = WEATHER)) + geom_bar(stat='identity') +xlim(c(0,60)) + ylim(c(0, 3500))


summer <- acc %>%
  filter(MONTH == 6 | MONTH == 7 | MONTH == 8) 

weather <- summer %>%
  group_by(STATE, WEATHER) %>%
  summarise(n = n()) 

#SUMMER
US_GLC2 %>%
  filter(MONTH == 6 | MONTH == 7 | MONTH == 8) %>% 
  filter(WEATHER < 98) %>%
  group_by(`State Name`, WEATHER, MONTH) %>%
  summarise(n = n()) %>%
  ggplot(acc, mapping = aes(x=`State Name`, y=n, fill = WEATHER)) + geom_col() + coord_flip() + xlab('STATE') + ylab('NUMBER OF ACCIDENTS') + ggtitle("Accidents by State during the Summer Season")

#WINTER
US_GLC2 %>%
  filter(MONTH == 12 | MONTH == 1 | MONTH == 2) %>% 
  filter(WEATHER < 98) %>%
  group_by(`State Name`, WEATHER, MONTH) %>%
  summarise(n = n()) %>%
  ggplot(acc, mapping = aes(x=`State Name`, y=n, fill = WEATHER)) + geom_col() + coord_flip() + xlab('STATE') + ylab('NUMBER OF ACCIDENTS') + ggtitle("Accidents by State during the Winter Season")








