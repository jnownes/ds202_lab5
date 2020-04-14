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
