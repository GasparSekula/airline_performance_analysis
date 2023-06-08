### Project 2

install.packages("dplyr")
install.packages("ggplot2")

library("dplyr")
library("ggplot2")
require(maps)
require(viridis)
options(stringsAsFactors = FALSE)

### loading data
# flights


parentDir <- dirname(getwd())
dfList <- list()

for (i in 1987:2008){
  path <- paste(parentDir, "/data/flights/", i, ".csv.bz2", sep="")
  df <- read.csv(path)
  dfList[[i-1986]] <- df
}

flights <- bind_rows(dfList)

write.csv(flights, "flights_cocnatenated.csv", row.names = FALSE)

flights <- read.csv("flights_cocnatenated.csv")


# airports

airports <- read.csv(paste(dirname(getwd()), "/data/airports.csv", sep=""))

# carriers
carriers <- read.csv(paste(dirname(getwd()), "/data/carriers.csv", sep=""))

# plane data
plane_data <- read.csv(paste(dirname(getwd()), "/data/plane-data.csv", sep=""))

# variable descriptions
variable_descriptions <- read.csv(paste(dirname(getwd()), "/data/variable-descriptions.csv", sep=""))

################################################################################

### Hub and Spoke vs. Point to Point analysis.

big_hubs <- function(flights){
  
  big_hubs <- filter(flights, UniqueCarrier %in% c("UA", "AA", "DL", "CO", "NW", "US", "B6", "AS")) %>%
    select(Origin, DepTime) %>%
    group_by(Origin) %>%
    summarise(TotalDepartures = length(DepTime)) %>%
    arrange(desc(TotalDepartures)) %>%
    slice_head(n=11)
  
  result <- big_hubs[,1]
  return(result)
}

# vector of big hubs
hubs <- unlist(big_hubs(flights))

# vector of other airports
not_hubs <- filter(airports, iata != "ATL" & iata != "ORD" & iata != "DFW" &
                     iata != "DEN" & iata != "LAX" & iata != "DTW" & iata != "MSP" &
                     iata != "SFO" & iata != "IAH" & iata != "EWR" & iata != "CLT") %>%
              select(iata)
not_hubs_vector <- unlist(not_hubs)


secondary_airports_prepare <- function(flights, n_hubs){
  result <- flights %>%
    filter(Year %in% 1988:2007) %>%
    filter( Origin %in% n_hubs & Dest %in% n_hubs) %>%
    select(Origin, Year, DepTime) %>%
    group_by(Year, Origin) %>%
    summarise(TotalDepartures = length(DepTime)) %>%
    filter(TotalDepartures >= 1000) %>%
    group_by(Year) %>%
    summarise(NumberOfAirports = n_distinct(Origin),
              AverageAnnualNumberOfDepartures = sum(TotalDepartures) / n_distinct(Origin)) %>%
    arrange(Year)
  
  return(result)
}

big_hubs_prepare <- function(flights, h){
  result <- flights %>%
    filter(Year %in% 1988:2007) %>%
    filter(Origin %in% h | Dest %in% h) %>%
    select(Year, DepTime) %>%
    group_by(Year) %>%
    summarize(AverageAnnualNumberOfDepartures = length(DepTime) / 11) %>%
    arrange(Year)
  
  return(result)
}

big_hubs_prepared_df <- big_hubs_prepare(flights, hubs)
secondary_airports_prepared_df <- secondary_airports_prepare(flights, not_hubs_vector)
combined_hubs_secondary <-cbind(big_hubs_prepared_df, secondary_airports_prepared_df)
colnames(combined_hubs_secondary)[2] <- "AverageAnnualNumberOfDeparturesHubs"
colnames(combined_hubs_secondary)[3] <- "Year2"

### chart: hubs - Average Annual Number Of Departures

ggplot(big_hubs_prepared_df, aes(x = Year, y = AverageAnnualNumberOfDepartures)) +
  geom_line(color="darkred") +
  labs(x = "rok", y = "średnia roczna liczba odlotów",
       title =  "Średnia roczna liczba odlotów przypadająca na jedno lotnisko typu hub.") +
  scale_x_continuous(breaks = 1988:2007,
                     labels = 1988:2007) +
  scale_y_continuous(breaks = c(200000, 250000, 300000, 350000, 400000)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

### chart: secondary - Average Annual Number Of Departures

ggplot(secondary_airports_prepared_df, aes(x = Year, y = AverageAnnualNumberOfDepartures)) +
  geom_line(color="darkred") +
  labs(x = "rok", y = "średnia roczna liczba odlotów",
       title =  "Średnia roczna liczba odlotów przypadająca na jedno lotnisko regionalne.") +
  scale_x_continuous(breaks = 1988:2007,
                     labels = 1988:2007) +
  scale_y_continuous(breaks = c(15000, 17500, 20000)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

################################################################################

### distance and delay dependency

dist_delay_dependency_prep <- function(flights){
  result <- flights %>% 
    select(Distance, ArrDelay) %>%
    filter(!is.na(ArrDelay)) %>%
    filter(!is.na(Distance)) %>%
    mutate(DistanceInterval = cut(Distance, breaks = seq(0, floor(max(Distance)), by = 100))) %>%
    group_by(DistanceInterval) %>%
    summarize(DelayedFlightsPercentage = mean(ArrDelay > 15) * 100)
  return(result)
}

dist_delay_dependency_df <- dist_delay_dependency_prep(flights)

### chart

ggplot(dist_delay_dependency_df, aes(x = DistanceInterval, y=DelayedFlightsPercentage)) + 
  geom_jitter(aes(color = DistanceInterval), alpha = 0.7, size = 2.5) +
  scale_color_discrete() +
  labs(x = "Odległość", y = "Opóźnienia", title = "Zależność opóźnień od odległośći.") + 
  guides(color = FALSE) +
  scale_x_discrete(breaks = function(x) x[seq(1, length(x), 4)]) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

################################################################################

### airlines with most scheduled flights

# airlines_most_flights <- function(flights, airlines) {
#   flights_modified <- flights %>%
#     select(Year, CRSDepTime, UniqueCarrier) %>%
#     group_by(Year, UniqueCarrier) %>%
#     summarise(TotalAnnualFlights = length(CRSDepTime)) %>%
#     slice_head(n=10)
#   
#   result <- inner_join(flights_modified, airlines, 
#                        join_by(UniqueCarrier == Code), na_matches = "never") %>%
#     select(Year, Description, TotalAnnualFlights)
#   
#   return(result)
#   
# }
# 
# airlines_most_flights_df <- airlines_most_flights(flights, carriers)
# write.csv(airlines_most_flights_df, "airlines_most_flights.csv", row.names = FALSE)

################################################################################

### most scheduled airlines

most_scheduled_airlines <- (flights %>% 
                                    select(UniqueCarrier, CRSDepTime) %>%
                                    group_by(UniqueCarrier) %>%
                                    summarise(TotalFlights = length(CRSDepTime)) %>%
                                    arrange(desc(TotalFlights)) %>%
                                    slice_head(n=15) %>% 
                                    inner_join(carriers, join_by(UniqueCarrier == Code), na_matches = "never") %>%
                                    select(Description, TotalFlights) %>%
                                    arrange(desc(TotalFlights)) %>%
                                    mutate(Description = recode(Description, "US Airways Inc. (Merged with America West 9/05. Reporting for both starting 10/07.)" = "US Airways Inc.")) %>%
                                    mutate(Description = recode(Description, "America West Airlines Inc. (Merged with US Airways 9/05. Stopped reporting 10/07.)" = "America West Airlines Inc.")) 
                            
                                    )
most_scheduled_airlines_vector <- unlist(most_scheduled_airlines[,1])

### chart

ggplot(most_scheduled_airlines, aes(x = Description, y = TotalFlights)) + 
  geom_bar(stat="identity", width = 0.5, fill="tomato2") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "przewoźnik", y = "sumaryczna liczba lotów w badanym okresie", title = "Przewoźnicy z największą liczbą lotów w latach 1987-2008.") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

################################################################################

### fleet age vs. airline age 

fleet_vs_airline_age <- function(flights, planes, airlines) {
  
  flights_prep <- flights %>%
    select(UniqueCarrier, Year, TailNum) %>%
    filter(!is.na(TailNum) & !is.na(UniqueCarrier)) %>%
    filter(UniqueCarrier %in% c("DL", "WN", "AA", "US", "UA", "CO", "TW", "NW", "AS", 
                                "OH", "F9", "B6", "HP", "JI (1)", "ML (1)"))
  
  planes_prep <- planes %>%
    select(tailnum, year) %>%
    filter(!is.na(tailnum) & !is.na(year))
  
  x <- inner_join(flights_prep, planes_prep, join_by(TailNum == tailnum), na_matches = "never") %>%
    mutate(PlaneAge = as.numeric(Year) - as.numeric(year)) %>%
    filter(!is.na(PlaneAge)) %>%
    select(UniqueCarrier, Year, PlaneAge) %>%
    group_by(UniqueCarrier, Year) %>%
    summarise(AverageFleetAge = mean(PlaneAge)) %>%
    inner_join(airlines, join_by(UniqueCarrier == Code), na_matches = "never") %>%
    select(UniqueCarrier, Description, Year, AverageFleetAge)
  
  result <- x %>% filter(!is.na(AverageFleetAge)) %>%
    mutate(type = ifelse(UniqueCarrier %in% c("DL", "AA", "US", "UA", "CO", "TW", "NW", "AS"), "przed 1967,", "po 1967.")) %>%
    mutate(Description = recode(Description, "US Airways Inc. (Merged with America West 9/05. Reporting for both starting 10/07.)" = "US Airways Inc.")) %>%
    mutate(Description = recode(Description, "America West Airlines Inc. (Merged with US Airways 9/05. Stopped reporting 10/07.)" = "America West Airlines Inc.")) 
  
  
  return(result)
    
}

fleet_vs_airline_age_df <- fleet_vs_airline_age(flights, plane_data, carriers)

### chart

ggplot(data = fleet_vs_airline_age_df, aes(x = Description, y = AverageFleetAge, )) +
  geom_violin(scale = "width", aes(fill = type)) +
  geom_boxplot(width = 0.2, coef = 100) +
  coord_flip() +
  labs(x = "przewoźnik", y = "wiek floty [lata]", 
       title = "Wiek floty dla wybranych przewoźników", 
       subtitle = "z podziałem na linie powstałe przed i po 1967.") +
  guides(fill=guide_legend(title="Rozpoczęcie działalności")) 

################################################################################

### delays by month 

delays_by_month <- flights %>%
      select(Year, Month, ArrDelay) %>%
      filter(!is.na(ArrDelay)) %>%
      filter(Year %in% 1988:2007) %>%
      mutate(type = ifelse(ArrDelay <= 15, 
                           "do 15 minut,", ifelse(ArrDelay <= 45, "od 15 do 45 minut,", "powyżej 45 minut."))) %>%
      inner_join(flights_by_month_df, by = "Month", na_matches = "never") %>%
      group_by(Month, type) %>%
      summarise(number = n()) %>%
      left_join(flights_by_month_df, by = "Month") %>%
      mutate(percentage = number / AveFlightsByMonth)

### chart delays by month 
custom_colors <- c("do 15 minut," = "limegreen", "od 15 do 45 minut," = "darkgoldenrod1", "powyżej 45 minut." = "tomato2")

ggplot(delays_by_month_df, aes(x = Month, y = percentage*100, fill = type)) +
  geom_bar(position="stack", stat="identity") +
  labs(x = "miesiące", y = "średni procent wszystkich lotów w danym miesiącu", 
       title = "Występowanie opóźnień z podziałem na miesiące i rodzaje opóźnień") + 
  scale_fill_manual(values = custom_colors) + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  scale_x_continuous( breaks = 1:12,
                      labels = polish_month_names) +
  guides(fill=guide_legend(title="Rodzaj opóźnienia:"))
      

### average flights by month

flights_by_month_df <- flights %>%
  select(Year, Month, DepTime) %>%
  filter(!is.na(DepTime)) %>%
  filter(Year %in% 1988:2007) %>%
  group_by(Month) %>%
  summarise(NumOfMonths = n_distinct(Year), AveFlightsByMonth = length(DepTime)/NumOfMonths)

### chart
polish_month_names <- c("Styczeń", "Luty", "Marzec", "Kwiecień", "Maj", "Czerwiec", 
                        "Lipiec", "Sierpień", "Wrzesień", "Październik", "Listopad", "Grudzień")

ggplot(flights_by_month_df, aes(Month, AveFlightsByMonth)) + 
  geom_bar(stat="identity", width = 0.5, fill="tomato2") + 
  labs(x = "miesiące", y = "średnia liczba lotów", title = "Średnia liczba lotów z podziałem na miesiące") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  scale_x_continuous( breaks = 1:12,
                      labels = polish_month_names)


### cancellations by month

cancellations_by_month <- flights %>%
  select(Year, Month, Cancelled) %>%
  filter(!is.na(Cancelled)) %>%
  filter(Year %in% 1988:2007) %>%
  inner_join(flights_by_month_df, by = "Month", na_matches = "never") %>%
  group_by(Month, Cancelled) %>%
  summarise(num = n()) %>%
  left_join(flights_by_month_df, by = "Month") %>%
  mutate(percentage = num / AveFlightsByMonth) %>%
  filter(Cancelled == 1)

ggplot(cancellations_by_month, aes(Month, percentage)) + 
  geom_bar(stat="identity", width = 0.5, fill = "purple3") +
  labs(x = "miesiące", y= "średnia liczba odwołanych lotów [jako procent wszystkich lotów]",
       title = "Średnia liczba odwołanych lotów z podziałem na miesiące.") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  scale_x_continuous(breaks = 1:12, labels = polish_month_names)

### only 2001

flights_by_month_df_01 <- flights %>%
  select(Year, Month, CRSDepTime) %>%
  filter(!is.na(CRSDepTime)) %>%
  filter(Year == 2001) %>%
  group_by(Month) %>%
  summarise(NumOfMonths = n_distinct(Year), AveFlightsByMonth = length(CRSDepTime))

cancellations_by_month_2001 <- flights %>%
  select(Year, Month, Cancelled) %>%
  filter(!is.na(Cancelled)) %>%
  filter(Year == 2001) %>%
  inner_join(flights_by_month_df_01, by = "Month", na_matches = "never") %>%
  group_by(Month, Cancelled) %>%
  summarise(num = n()) %>%
  left_join(flights_by_month_df_01, by = "Month") %>%
  mutate(percentage = num / AveFlightsByMonth) %>%
  filter(Cancelled == 1)

ggplot(cancellations_by_month_2001, aes(Month, percentage)) + 
  geom_bar(stat="identity", width = 0.5, fill = "hotpink4") +
  labs(x = "miesiące", y= "średnia liczba odwołanych lotów [jako ułamek wszystkich lotów]",
       title = "Średnia liczba odwołanych lotów w roku 2001 z podziałem na miesiące.") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  scale_x_continuous(breaks = 1:12, labels = polish_month_names)

### without 2001

cancellations_by_month_no_2021 <- flights %>%
  select(Year, Month, Cancelled) %>%
  filter(!is.na(Cancelled)) %>%
  filter(Year %in% c(1988:2000, 2002:2007)) %>%
  inner_join(flights_by_month_df, by = "Month", na_matches = "never") %>%
  group_by(Month, Cancelled) %>%
  summarise(num = n()) %>%
  left_join(flights_by_month_df, by = "Month") %>%
  mutate(percentage = num / AveFlightsByMonth) %>%
  filter(Cancelled == 1)

ggplot(cancellations_by_month_no_2021, aes(Month, percentage)) + 
  geom_bar(stat="identity", width = 0.5, fill = "purple4") +
  labs(x = "miesiące", y= "średnia liczba odwołanych lotów [jako procent wszystkich lotów]",
       title = "Średnia liczba odwołanych lotów w latach 1988-2007 (bez 2001) z podziałem na miesiące.") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  scale_x_continuous(breaks = 1:12, labels = polish_month_names)

################################################################################

### NYC flights

usa_map <- map_data("state")

nyc_destinations <- function(flights, airports, usa){
  x <- flights %>%
    select(Origin, Dest) %>%
    filter(Origin %in% c("JFK", "EWR", "LGA")) %>%
    group_by(Dest) %>%
    summarise(TotalFlights = n()) %>%
    inner_join(airports, join_by(Dest == iata), na_matches = "never") %>%
    select(Dest, TotalFlights, lat, long, state, city) %>%
    arrange(desc(TotalFlights)) 
  
  state_mapping <- data.frame(state = state.abb, full_name = state.name)
  
  x$state <- state_mapping$full_name[match(x$state, state_mapping$state)]
  x$state <- tolower(x$state)
  
  almost_result <- x %>% 
    right_join(usa, join_by(state == region)) %>%
    select(Dest, TotalFlights, lat.x, long.x, state, city, group)
  
  result <- almost_result %>%
    select(TotalFlights, state, long.x, lat.x, group) %>%
    group_by(state) %>%
    summarise(TotalFlightsByState = sum(TotalFlights), long = mean(long.x), lat = mean(lat.x), group = mean(group))

  return(result)
}


nyc_destinations_df <- nyc_destinations(flights, airports, usa_map)

map_data <- merge(usa_map, nyc_destinations_df, by.x = "region", by.y = "state")

ggplot(map_data, aes(long.x, lat.x, group = group.y)) +
  geom_polygon(aes(fill = TotalFlightsByState), color = "white") +
  scale_fill_viridis_c(option = "C") +
  coord_map("mercator") + 
  labs(x = "długość geograficzna", y = "szerokość geograficzna", title = "Sumaryczna liczba lotów krajowych w USA startujących z Nowego Jorku",
  subtitle = "w latach 1987-2007 z podziałem na stany") + 
  guides(fill=guide_legend(title="liczba lotów"))



### function generating data for specific airport

airport_destinations <- function(flights, airports, usa, airport){
  x <- flights %>%
    select(Year, Origin, Dest) %>%
    filter(Origin == airport ) %>%
    filter(Year %in% 2003:2007) %>%
    group_by(Dest) %>%
    summarise(TotalFlights = n()) %>%
    inner_join(airports, join_by(Dest == iata), na_matches = "never") %>%
    select(Dest, TotalFlights, lat, long, state, city) %>%
    arrange(desc(TotalFlights)) 
  
  state_mapping <- data.frame(state = state.abb, full_name = state.name)
  
  x$state <- state_mapping$full_name[match(x$state, state_mapping$state)]
  x$state <- tolower(x$state)
  
  almost_result <- x %>% 
    right_join(usa, join_by(state == region)) %>%
    select(Dest, TotalFlights, lat.x, long.x, state, city, group)
  
  result <- almost_result %>%
    select(TotalFlights, state, long.x, lat.x, group) %>%
    group_by(state) %>%
    summarise(TotalFlightsByState = sum(TotalFlights), long = mean(long.x), lat = mean(lat.x), group = mean(group))
  
  return(result)
}

### JFK

jfk_dest <- airport_destinations(flights, airports, usa_map, "JFK")
jfk_data <- merge(usa_map, jfk_dest, by.x = "region", by.y = "state")

ggplot(jfk_data, aes(long.x, lat.x, group = group.y)) +
  geom_polygon(aes(fill = TotalFlightsByState), color = "white") +
  scale_fill_viridis_c(option = "C") +
  coord_map("mercator") + 
  labs(x = "długość geograficzna", y = "szerokość geograficzna", 
       title = "Sumaryczna liczba lotów krajowych w USA startujących z lotniska JFK", 
       subtitle = "w latach 2003-2007 z podziałem na stany.") + 
  guides(fill=guide_legend(title="liczba lotów"))

### EWR

ewr_dest <- airport_destinations(flights, airports, usa_map, "EWR")
ewr_data <- merge(usa_map, ewr_dest, by.x = "region", by.y = "state")

ggplot(ewr_data, aes(long.x, lat.x, group = group.y)) +
  geom_polygon(aes(fill = TotalFlightsByState), color = "white") +
  scale_fill_viridis_c(option = "C") +
  coord_map("mercator") + 
  labs(x = "długość geograficzna", y = "szerokość geograficzna", 
       title = "Sumaryczna liczba lotów krajowych w USA startujących z lotniska Newark (EWR)", 
       subtitle = "w latach 2003-2007 z podziałem na stany.") + 
  guides(fill=guide_legend(title="liczba lotów"))


### LGA

lga_dest <- airport_destinations(flights, airports, usa_map, "LGA")
lga_data <- merge(usa_map, lga_dest, by.x = "region", by.y = "state")

ggplot(lga_data, aes(long.x, lat.x, group = group.y)) +
  geom_polygon(aes(fill = TotalFlightsByState), color = "white") +
  scale_fill_viridis_c(option = "C") +
  coord_map("mercator") + 
  labs(x = "długość geograficzna", y = "szerokość geograficzna", 
       title = "Sumaryczna liczba lotów krajowych w USA startujących z lotniska La Guardia (LGA)", 
       subtitle = "w latach 2003-2007 z podziałem na stany.") + 
  guides(fill=guide_legend(title="liczba lotów"))
  
