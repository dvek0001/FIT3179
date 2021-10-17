library(tidyverse)
library(lubridate)

##################################################
flights <- read_csv("flights.csv")

flights <- flights %>% filter(ORIGIN_AIRPORT == "ATL")

origin <- flights %>% count(DESTINATION_AIRPORT) %>% arrange(-n) %>% top_n(100) %>% pull(DESTINATION_AIRPORT)

flights <- flights %>% filter(DESTINATION_AIRPORT %in% origin)

flights <- flights %>% mutate(DATE = dmy(paste(DAY, MONTH, "2015", sep = "/")),
                              ARRIVAL_DELAY = if_else(ARRIVAL_DELAY < 0, 0, ARRIVAL_DELAY),
                              DEPARTURE_DELAY = if_else(DEPARTURE_DELAY < 0, 0, DEPARTURE_DELAY))

flights2 <- flights %>% 
  select(DATE, AIRLINE, ORIGIN_AIRPORT, DESTINATION_AIRPORT, DEPARTURE_DELAY,
         ARRIVAL_DELAY, CANCELLATION_REASON, AIR_SYSTEM_DELAY,
         SECURITY_DELAY, AIRLINE_DELAY, LATE_AIRCRAFT_DELAY, WEATHER_DELAY) %>% 
  filter(ORIGIN_AIRPORT != "SJU", DESTINATION_AIRPORT != "SJU") %>% 
  filter(ORIGIN_AIRPORT != "ECP", DESTINATION_AIRPORT != "ECP")

flights2 <- flights2 %>% 
  group_by(AIRLINE, ORIGIN_AIRPORT, DESTINATION_AIRPORT) %>% 
  mutate(DEPARTURE_DELAY = if_else(is.na(DEPARTURE_DELAY), mean(DEPARTURE_DELAY, na.rm = T), DEPARTURE_DELAY),
         ARRIVAL_DELAY = if_else(is.na(ARRIVAL_DELAY), mean(ARRIVAL_DELAY, na.rm = T), ARRIVAL_DELAY)) %>% 
  ungroup()


flights3 <- flights2 %>% mutate(CANCELLATION_REASON = if_else(is.na(CANCELLATION_REASON), "Not Cancelled", CANCELLATION_REASON)) %>% 
  mutate(CANCELLATION_REASON = if_else(CANCELLATION_REASON == "A", "Airline/Carrier", CANCELLATION_REASON),
         CANCELLATION_REASON = if_else(CANCELLATION_REASON == "B", "Weather", CANCELLATION_REASON),
         CANCELLATION_REASON = if_else(CANCELLATION_REASON == "C", "National Air System", CANCELLATION_REASON))

flights3 <- flights3 %>% mutate(MONTH = month(DATE))

flights3 <- flights3 %>% rename(Weather = WEATHER_DELAY,
                                `Air System` = AIR_SYSTEM_DELAY,
                                Airline = AIRLINE_DELAY,
                                Security = SECURITY_DELAY,
                                `Late Aircraft` = LATE_AIRCRAFT_DELAY)

flights3 <- flights3 %>% 
  mutate(Weather = if_else(is.na(Weather),0,Weather),
         `Air System` = if_else(is.na(`Air System`),0,`Air System`),
         Airline = if_else(is.na(Airline),0,Airline),
         Security = if_else(is.na(Security),0,Security),
         `Late Aircraft` = if_else(is.na(`Late Aircraft`),0,`Late Aircraft`))

#flights3 <- read_csv("data/flights.csv")
#write_csv(flights3, "data/flights.csv")

flights_b1 <- flights3 %>% 
  group_by(AIRLINE, month(DATE, label = T, abbr = T)) %>% 
  summarise(DEPARTURE_DELAY = mean(DEPARTURE_DELAY, na.rm = T)) %>% 
  ungroup() %>% 
  rename(MONTH = `month(DATE, label = T, abbr = T)`) %>% 
  group_by(MONTH) %>% 
  mutate(rank = rank(-DEPARTURE_DELAY)) %>% 
  pivot_longer(cols = DEPARTURE_DELAY, names_to = "DELAY_TYPE", values_to = "DELAY")

flights_b2 <- flights3 %>% 
  group_by(AIRLINE, month(DATE, label = T, abbr = T)) %>% 
  summarise(ARRIVAL_DELAY = mean(ARRIVAL_DELAY, na.rm = T)) %>% 
  ungroup() %>% 
  rename(MONTH = `month(DATE, label = T, abbr = T)`) %>% 
  group_by(MONTH) %>% 
  mutate(rank = rank(-ARRIVAL_DELAY)) %>% 
  pivot_longer(cols = ARRIVAL_DELAY, names_to = "DELAY_TYPE", values_to = "DELAY")

flights_bump <- bind_rows(flights_b1, flights_b2)
#write_csv(flights_bump, "data/flights_bump.csv")
##################################################

flights_bar_all <- flights3 %>% count(AIRLINE,CANCELLATION_REASON) %>% 
  left_join(count(flights3,AIRLINE), by = c("AIRLINE")) %>% 
  mutate(percentage = n.x/n.y) %>% 
  filter(CANCELLATION_REASON != "Not Cancelled") %>% 
  mutate(MONTH = "All") %>% 
  select(MONTH, AIRLINE, CANCELLATION_REASON, percentage)

flights_bar_month <- flights3 %>% count(MONTH,AIRLINE,CANCELLATION_REASON) %>% 
  left_join(count(flights3,MONTH,AIRLINE), by = c("MONTH", "AIRLINE")) %>% 
  mutate(percentage = n.x/n.y) %>% 
  filter(CANCELLATION_REASON != "Not Cancelled") %>% 
  select(MONTH, AIRLINE, CANCELLATION_REASON, percentage)

flights_bar <- rbind(flights_bar_all, flights_bar_month)

#write_csv(flights_bar, file = "data/flights_bar.csv")


airlines <- read_csv("data/airlines.csv")

airlines <- airlines %>% 
  mutate(AIRLINE = str_remove(AIRLINE," Inc.")) %>% 
  mutate(AIRLINE = str_replace(AIRLINE, "Air Lines", "Airlines")) %>% 
  mutate(AIRLINE = str_remove(AIRLINE, " Co."))

#write_csv(airlines, "data/airlines.csv")
