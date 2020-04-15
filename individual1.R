library(lubridate)
library(dplyr)
library(tidyverse)

acc = read.csv("https://raw.githubusercontent.com/xdaiISU/ds202materials/master/hwlabs/fars2017/accident.csv", stringsAsFactors = FALSE)
person = read.csv("https://raw.githubusercontent.com/xdaiISU/ds202materials/master/hwlabs/fars2017/person.csv", stringsAsFactors = FALSE)

str(acc)


# 1
table(acc$DAY_WEEK)
ggplot(acc, mapping = aes(x=wday(DAY_WEEK, label = TRUE))) + geom_bar() + xlab("Day of week") + ylab("Total number of accidents") + ggtitle("Total number of accidents based on weekday")

# 2
table(acc$HOUR)
acc %>%
  filter(HOUR < 24) %>%
  ggplot(mapping = aes(x=factor(as.factor(paste(HOUR,":00",sep="")), levels= c("0:00","1:00","2:00","3:00","4:00","5:00","6:00","7:00","8:00","9:00","10:00","11:00","12:00","13:00","14:00","15:00","16:00","17:00","18:00","19:00","20:00","21:00","22:00","23:00")))) + geom_bar() + xlab("Hour of Day (24-hour clock)") + ylab("Total number of accidents") + ggtitle("Total number of accidents based on time of day") + theme(axis.text.x = element_text(angle = 90, vjust = .4))

# 3
nrow(acc %>%
  filter(DRUNK_DR > 0))

# 4
person = person %>%
  filter(PER_TYP == 1)

# 5
combined <- acc %>%
  full_join(person, by = c('STATE', 'ST_CASE', 'COUNTY', 'DAY', 'MONTH', 'HOUR', 'MINUTE'))

# 6
combined$SEX <- factor(combined$SEX, labels = c("Male","Female", "Not Reported", "Unknown"))

combined %>%
  filter(SEX == 'Male' | SEX == 'Female') %>%
  filter(HOUR <= 24) %>%
  ggplot(aes(x=factor(as.factor(paste(HOUR,":00",sep="")), levels= c("0:00","1:00","2:00","3:00","4:00","5:00","6:00","7:00","8:00","9:00","10:00","11:00","12:00","13:00","14:00","15:00","16:00","17:00","18:00","19:00","20:00","21:00","22:00","23:00")), fill=wday(DAY_WEEK, label = TRUE))) + geom_bar() + facet_wrap(~SEX) + labs(x= "Hour of Day", y= "Number of accidents",fill = "Day of Week", title="Accidents by day of week, hour, and sex") + theme(axis.text.x = element_text(angle = 90, vjust = .4)) + scale_x_discrete(breaks=c("0:00","2:00","4:00","6:00","8:00","10:00","12:00","14:00","16:00","18:00","20:00","22:00"))


combined %>%
  filter(SEX == "Male" | SEX == "Female") %>%
  filter(HOUR <= 24) %>%
  ggplot(aes(x=factor(as.factor(paste(HOUR,":00",sep="")), levels= c("0:00","1:00","2:00","3:00","4:00","5:00","6:00","7:00","8:00","9:00","10:00","11:00","12:00","13:00","14:00","15:00","16:00","17:00","18:00","19:00","20:00","21:00","22:00","23:00")), fill=SEX)) + geom_bar() + facet_wrap(~wday(DAY_WEEK, label = TRUE), nrow=3) + xlab("Hour of Day (24-hour clock)") + ylab('Number of accidents') + ggtitle('Accidents by day of week, hour, and sex') + theme(axis.text.x = element_text(angle = 90, vjust = .4)) + scale_x_discrete(breaks=c("0:00","2:00","4:00","6:00","8:00","10:00","12:00","14:00","16:00","18:00","20:00","22:00"))

# 7-8
library("readxl")

US_GLC<- readxl::read_xlsx('FRPP_GLC_UnitedStatesFeb132020.xlsx')
#US_territory<- read_excel('FRPP_GLC_US_Territories12419.xlsx')

US_GLC <- US_GLC %>%
  select('State Name', STATE = 'State Code', COUNTY = 'County Code', 'County Name') %>%
  unique()

US_GLC$STATE <- as.integer(US_GLC$STATE)
US_GLC$COUNTY <- as.integer(US_GLC$STATE)
US_GLC$`State Name` <- tolower(US_GLC$`State Name`)
US_GLC$`County Name` <- tolower(US_GLC$`County Name`)


# Problem here
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