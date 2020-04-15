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

US_GLC<- readxl::read_xlsx('FRPP GLC - United StatesFeb132020.xlsx')
#US_territory<- read_excel('FRPP_GLC_US_Territories12419.xlsx')

US_GLC <- US_GLC %>%
  select('State Name', STATE = 'State Code', COUNTY = 'County Code', 'County Name') %>%
  unique()

US_GLC$STATE <- as.integer(US_GLC$STATE)
US_GLC$COUNTY <- as.integer(US_GLC$STATE)
US_GLC$`State Name` <- tolower(US_GLC$`State Name`)
US_GLC$`County Name` <- tolower(US_GLC$`County Name`)

join<- acc %>%
  select(STATE, ST_CASE, COUNTY) %>%
  group_by(STATE, COUNTY) %>%
  summarize(total_accidents= n()) %>%
  left_join(US_GLC, by = c('STATE', 'COUNTY'))

county<- map_data('county')

county <- county %>%
  mutate(`State Name` = region, `County Name` = subregion) %>%
  select(`State Name`, `County Name`, long, lat, group, order)

join <- join %>%
  full_join(county, by = c('State Name', 'County Name'))

join<- join[order(join$order), ]

ggplot(join, aes(long, lat)) + geom_polygon(aes(group = group, fill = total_accidents)) 
