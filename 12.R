# Import the datasets of covid: 

confirmedraw <- read.csv("/Users/me-mac/Desktop/Study folder/Adv. Statistcs/covid-19/confirm.csv")
View(confirmedraw) # Check latest date at the end of confirmed data

deathsraw <- read.csv("/Users/me-mac/Desktop/Study folder/Adv. Statistcs/covid-19/deaths.csv")
View(deathsraw) # Check latest date at the end of death data

recoveredraw <- read.csv("/Users/me-mac/Desktop/Study folder/Adv. Statistcs/covid-19/recovered.csv")
View(recoveredraw) # Check latest date at the end of recovered data

# Note differences in the number of rows/columns

# DATA CLEANING: To create country level and global combined data
# Convert each dataset from wide to long then aggregate with country level
library(tidyr)
library(dplyr) # for common data manipulation 

confirmed <- confirmedraw %>% gather(key="date", value="confirmed", -c(Country.Region, Province.State, Lat, Long)) %>% group_by(Country.Region, date) %>% summarize(confirmed=sum(confirmed))
deaths <- deathsraw %>% gather(key="date", value="deaths", -c(Country.Region, Province.State, Lat, Long)) %>% group_by(Country.Region, date) %>% summarize(deaths=sum(deaths))
recovered <- recoveredraw %>% gather(key="date", value="recovered", -c(Country.Region, Province.State, Lat, Long)) %>% group_by(Country.Region, date) %>% summarize(recovered=sum(recovered))
summary(confirmed)

# Final data: combine all three
country <- full_join(confirmed, deaths) %>% full_join(recovered) # tables joined by extra col.

# fix date variable then convert character to date

country$date <- country$date %>% sub("X", "", .) %>% as.Date("%m.%d.%y")
View(country) # check date and if all country are joined

# Extract specific country: Italy
egypt <- country %>% filter(Country.Region=="Egypt")
egypt$date <- egypt$date %>% sub("X", "", .) %>% as.Date("%m.%d.%y")
View(egypt)

# SUMMARY STATISTICS
summary(country) # summary for all countries in total statistics

by(country$confirmed, country$Country.Region, summary)# specific summary for each country
by(country$deaths, country$Country.Region, summary)
by(country$recovered, country$Country.Region, summary)
summary(egypt)

countrytotal <- country %>% group_by(Country.Region) %>% summarize(cumconfirmed=sum(confirmed), 
cumdeaths=sum(deaths), cumrecovered=sum(recovered))
View(countrytotal)


# graphs of cases over time
library(ggplot2)
# World confirmed
ggplot(world, aes(x=date, y=confirmed)) + geom_bar(stat="identity", width=0.1) +
  theme_classic() +
  labs(title = "Covid-19 Global Confirmed Cases", x= "Date", y= "Daily confirmed cases") +
  theme(plot.title = element_text(hjust = 0.5))

# egypt confirmed
ggplot(egypt, aes(x=date, y=confirmed)) + geom_bar(stat="identity", width=0.1) +
  theme_classic() +
  labs(title = "Covid-19 Confirmed Cases in Egypt", x= "Date", y= "Daily confirmed cases") +
  theme(plot.title = element_text(hjust = 0.5))

# World confirmed, deaths and recovered
str(world)
world %>% gather("Type", "Cases", -c(date, days)) %>%
  ggplot(aes(x=date, y=Cases, colour=Type)) + geom_bar(stat="identity", width=0.2, fill="white") +
  theme_classic() +
  labs(title = "Covid-19 Global Cases", x= "Date", y= "Daily cases") +
  theme(plot.title = element_text(hjust = 0.5))


# Line graph of cases over time
# World confirmed
ggplot(world, aes(x=days, y=confirmed)) + geom_line() +
  theme_classic() +
  labs(title = "Covid-19 Global Confirmed Cases", x= "Days", y= "Daily confirmed cases") +
  theme(plot.title = element_text(hjust = 0.5))

# World confirmed with counts in log10 scale
ggplot(world, aes(x=days, y=confirmed)) + geom_line() +
  theme_classic() +
  labs(title = "Covid-19 Global Confirmed Cases", x= "Days", y= "Daily confirmed cases  (log scale)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(trans="log10")

# Combine basemap data to covid data
countrytotal$Country.Region[!countrytotal$Country.Region %in% World$name]
list <- which(!countrytotal$Country.Region %in% World$name)
countrytotal$country <- as.character(countrytotal$Country.Region)
countrytotal$country[list] <-
  c("Egypt",
    "South Africa","South Sudan", "S. Sudan", "Zambia",
    "United States", "Palestine", "Yemen","Syria")
countrytotal$Country.Region[!countrytotal$country %in% World$name]
World$country <- World$name
worldmap <- left_join(World, countrytotal, by="country")
worldmap$cumconfirmed[is.na(worldmap$cumconfirmed)] <- 0

ggplot(data = worldmap) + geom_sf(aes(fill=cumconfirmed), color="black") +
  ggtitle("World Map of Confirmed Covid Cases",
          subtitle="Total Cases on April 20, 2020") +
  theme_bw()

#------------------------------------------------




