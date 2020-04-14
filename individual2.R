#LAB 5
library(lubridate)
library(dplyr)
library(tidyverse)

acc <- read.csv("https://raw.githubusercontent.com/xdaiISU/ds202materials/master/hwlabs/fars2017/accident.csv", stringsAsFactors = FALSE)

person <- acc <- read.csv("https://raw.githubusercontent.com/xdaiISU/ds202materials/master/hwlabs/fars2017/person.csv", stringsAsFactors = FALSE)

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

View(combined)


#Question 6
