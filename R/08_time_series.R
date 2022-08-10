# --------------------------------------------------#
# Scientific computing
# ICTP/Serrapilheira 2022
# First version 2022-07-21
# Class 09: Manipulate dates in R using the package lubridate
# --------------------------------------------------#

###Time-series
library(dplyr)
library(ggplot2)
library(lubridate)
library(zoo)

#use Covid-19 data from Recife in Pernambuco, Brazil
covid <- read.csv("data/raw/covid19-dd7bc8e57412439098d9b25129ae6f35.csv")

#Converting into date format
# First checking the class
class(covid$date)

# Changing to date format
covid$date <- as_date(covid$date)
# Checking the class
class(covid$date)
summary(covid$date)

# Now we can make numeric operations
range(covid$date)

#Plotting a time-series
ggplot(covid) +
  geom_line(aes(x = date, y = new_confirmed)) +
  theme_minimal()

#Oops. We have negative cases and will substitute the negative values per zero.
covid$new_confirmed[covid$new_confirmed < 0] <- 0

ggplot(covid) +
  geom_line(aes(x = date, y = new_confirmed)) +
  theme_minimal() +
  scale_x_date(breaks = "4 months", date_labels = "%Y %m" ) +
  labs(x = "Date", y = "New cases")

#Rolling mean
covid$roll_mean <- zoo::rollmean(covid$new_confirmed, 14, fill = NA)

ggplot(covid) +
  geom_line(aes(x = date, y = new_confirmed)) +
  theme_minimal() +
  scale_x_date(breaks = "4 months", date_labels = "%Y %m" ) +
  labs(x = "Date", y = "New cases") +
  geom_line(aes(x= date, y= roll_mean, color = "red"))
