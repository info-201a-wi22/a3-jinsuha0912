# Name: Jinsu Ha
# Assignemtn 3: Incarceration

# bring in all the packages needed for the codes
library(tidyverse) # tidyverse package contains dplyr, tibble, and ggplot2
library(stringr)
library(usdata)
library(maps)

# bring out the datasets (csv files)
incarceration_trends <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

# check what kinds of information are in the dataset
colnames(incarceration_trends)

# Summary Information & Introduction: introduce the observation that I will be
# conducting and a brief summary of the dataset.
# Summarize the data by comparing the population in the U.S. VS population in the jail
# this will give a relative perspective as to how the population in the jail and 
# the population in the U.S. differ, similar, or the same. (people age from 15-64)
# rate will be shown in percent(%)

# select only the columns that I need for the observation & get rid of NA 
incarceration_trends <- incarceration_trends %>%
  na.omit(year)%>%
  select(year, state, county_name, total_pop, total_pop_15to64, total_jail_pop, aapi_jail_pop, 
         black_jail_pop, latinx_jail_pop, native_jail_pop, white_jail_pop, 
         aapi_pop_15to64, black_pop_15to64, latinx_pop_15to64, native_pop_15to64, 
         white_pop_15to64) 

# what is the population rate of which is in jail per county
incarceration_trends$pop_rate_in_jail <- round(incarceration_trends$total_jail_pop /
                                        incarceration_trends$total_pop, 6) * 100

# what is the most recent average jail population rate? 
pop_rate_jail <- incarceration_trends %>%
  filter(year == max(year, na.rm = TRUE))%>%
  pull(pop_rate_in_jail)

ave_pop_rate_jail <- round(mean(pop_rate_jail, na.rm = TRUE), 2)

# what is the most recent average population of White people in the total county
# population of people?
incarceration_trends$white_pop_rate <- round(incarceration_trends$white_pop_15to64 / 
                                      incarceration_trends$total_pop_15to64, 6) * 100

white_pop_rate <- incarceration_trends%>%
  filter(year == max(year, na.rm = TRUE)) %>%
  pull(white_pop_rate)

ave_white_pop_rate <- round(mean(white_pop_rate, na.rm = TRUE), 2) 

# what is the most recent average population of White people in the total county 
# population of people that are in the jail?
incarceration_trends$white_pop_rate_jail <- round(incarceration_trends$white_jail_pop/
                                            incarceration_trends$total_jail_pop, 6) * 100

white_pop_rate_jail <- incarceration_trends%>%
  filter(year == max(year, na.rm = TRUE))%>%
  pull(white_pop_rate_jail)

ave_white_pop_rate_jail <- round(mean(white_pop_rate_jail, na.rm = TRUE), 2)

# what is the most recent average population of Black people (age 15-64) in the total 
# county population of people (age 15-64)?
incarceration_trends$black_pop_rate <- round(incarceration_trends$black_pop_15to64 /
                                      incarceration_trends$total_pop_15to64, 6) * 100

black_pop_rate <- incarceration_trends%>%
  filter(year == max(year, na.rm = TRUE))%>%
  pull(black_pop_rate)

ave_black_pop_rate <- round(mean(black_pop_rate, na.rm = TRUE), 2)

# what is the most recent average population of Black people (age 15-64) in the 
# county population that are in the jail?
incarceration_trends$black_pop_rate_jail <- round(incarceration_trends$black_jail_pop/
                                            incarceration_trends$total_jail_pop, 6) * 100


black_pop_rate_jail <- incarceration_trends %>%
  filter(year == max(year, na.rm = TRUE))%>%
  pull(black_pop_rate_jail)
ave_black_pop_rate_jail <- round(mean(black_pop_rate_jail, na.rm = TRUE), 2)

# what is the most recent average population of Latinx people (age 15-64) in the 
# total county population of people(age 15-64)?
incarceration_trends$latinx_pop_rate <- round(incarceration_trends$latinx_pop_15to64 /
                                        incarceration_trends$total_pop_15to64, 6) * 100

latinx_pop_rate <- incarceration_trends%>%
  filter(year == max(year, na.rm = TRUE)) %>%
  pull(latinx_pop_rate)
ave_latinx_pop_rate <- round(mean(latinx_pop_rate, na.rm = TRUE), 2)

# what is the most recent average population of Latinx people in the total county 
# population that are in the jail?
incarceration_trends$latinx_pop_rate_jail <- round(incarceration_trends$latinx_jail_pop/
                                            incarceration_trends$total_jail_pop, 6) * 100

latinx_pop_rate_jail <- incarceration_trends%>%
  filter(year == max(year, na.rm = TRUE))%>%
  pull(latinx_pop_rate_jail)
ave_latinx_pop_rate_jail <- round(mean(latinx_pop_rate_jail, na.rm = TRUE), 2)

# what is the most recent average population of AAPI people in the total county 
# population of people?
incarceration_trends$aapi_pop_rate <- round(incarceration_trends$aapi_pop_15to64/
                                      incarceration_trends$total_pop_15to64, 6) * 100

aapi_pop_rate <- incarceration_trends%>%
  filter(year == max(year, na.rm = TRUE))%>%
  pull(aapi_pop_rate)
ave_aapi_pop_rate <- round(mean(aapi_pop_rate, na.rm = TRUE), 2)

# what is the most recent average population of AAPI people in the total county 
# population that are in the jail?
incarceration_trends$aapi_pop_rate_jail <- round(incarceration_trends$aapi_jail_pop/
                                          incarceration_trends$total_jail_pop, 6) * 100

aapi_pop_rate_jail <- incarceration_trends%>%
  filter(year == max(year, na.rm = TRUE))%>%
  pull(aapi_pop_rate_jail)
ave_aapi_pop_rate_jail <- round(mean(aapi_pop_rate_jail, na.rm = TRUE), 2)

# what is the most recent average population of Native people in the total county 
# population of people?
incarceration_trends$native_pop_rate <- round(incarceration_trends$native_pop_15to64/
                                        incarceration_trends$total_pop_15to64, 6) * 100

native_pop_rate <- incarceration_trends%>%
  filter(year == max(year, na.rm = TRUE))%>%
  pull(native_pop_rate)
ave_native_pop_rate <- round(mean(native_pop_rate, na.rm = TRUE), 2)
# what is the most recent average population of Native people in the total county
# population of people that are in the jail?
incarceration_trends$native_pop_rate_jail <- round(incarceration_trends$native_jail_pop /
                                            incarceration_trends$total_jail_pop, 6) * 100

native_pop_rate_jail <- incarceration_trends%>%
  filter(year == max(year, na.rm = TRUE))%>%
  pull(native_pop_rate_jail)
ave_native_pop_rate_jail <- round(mean(native_pop_rate_jail, na.rm = TRUE), 2)

# Time trends chart
# Here, I will describe the trend over time in different counties in CA state 
# of black people in jail. I thought that New York would be a good state observe 
# since it is a diverse state. 

# what are the most populated counties in CA in the most recent year?
most_populated_count_CA <- incarceration_trends%>%
  filter(state == "CA")%>%
  filter(year == max(year, na.rm = TRUE))%>%
  arrange(-total_pop)%>%
  select(county_name)

# Out of the top 13 counties in CA, I will use 8 counties to show the trends:
# San Diego, Riverside, San Bernardino, Alameda, Sacramento, Fresno, Kern, SF

# create a data frame taht contains each counties's information with the year and 
# black population in jail and the name of the county to create a line graph 

top_eight_county_ca_incarceration <- incarceration_trends%>%
  filter(state == "CA") %>%
  filter(county_name %in% c("San Diego County", "San Francisco County", "Riverside County",
                            "San Bernardino County", "Alameda County", "Sacramento County",
                            "Fresno County", "Kern County"))%>%
  select(year, county_name, black_pop_rate_jail)


# draw the line graph to show the trends
time_plot <- ggplot(data = top_eight_county_ca_incarceration) +
            geom_line(mapping = aes(x = year, y = black_pop_rate_jail, color = county_name), 
                      size = 1) +
            labs(
              title = "Rate of Black People in Jail in CA's 8 Most Populated Counties over Time",
              x = "Year",
              y = "Rate of Black People in Jail (in %)",
              color = "County"
            )


# a chart that compares two varibales to one another
# I will be comparing the black population rate in jail VS white population rate in jail
# in 5 states with the most population in the most recent years. 
# I want to do this to observe if a city with larger population would have a less 
# discrimination against one race or others.

# find five states with the most population
most_populated_states <- incarceration_trends%>%
  filter(year == max(year, na.rm = TRUE))%>%
  arrange(-total_pop)%>%
  select(state)


# most populated states are NY, CA, WA, NV, FL

black_jail_rate_in_big_states <- incarceration_trends%>%
  group_by(state)%>%
  filter(year == max(year, na.rm = TRUE))%>%
  filter(state %in% c("NY", "CA", "WA", "NV", "FL"))%>%
  mutate(jail_pop = mean(black_pop_rate_jail, na.rm = TRUE))%>%
  mutate(race = "Black")%>%
  select(year, state, jail_pop, race)

white_jail_rate_in_big_states <- incarceration_trends%>%
  group_by(state)%>%
  filter(year == max(year, na.rm = TRUE))%>%
  filter(state %in% c("NY", "CA", "WA", "NV", "FL"))%>%
  mutate(jail_pop = mean(white_pop_rate_jail, na.rm = TRUE))%>%
  mutate(race = "White")%>%
  select(year, state, jail_pop, race)

# combine the two data frames above to create another chart
black_white_jail_rate_in_big_state <- rbind(black_jail_rate_in_big_states
                                            , white_jail_rate_in_big_states)

variable_chart <- ggplot(data = black_white_jail_rate_in_big_state) +
  geom_col(mapping = aes(x = state, y = jail_pop, fill = race), size = 1, 
           position = "dodge") + 
          labs(
            title = "Most Recent Average Rate of Jail Population by Race in 5 Biggest States in the U.S.",
            x = "State",
            y = "Rate of Population in Jail (in %)",
            color = "Race"
          )


# Map
# I will be creating a map to show the mean of variable: population of AAPI people
# in the jail in each state

# most current data of the population of AAPI people in the jail across the states
latinx_state_incarceration <- incarceration_trends%>%
  group_by(state)%>%
  filter(year == max(year, na.rm = TRUE))%>%
  summarize(latinx_pop_rate_jail = mean(latinx_pop_rate_jail))

latinx_state_incarceration[is.na(latinx_state_incarceration)] <- 0


# join the latinx state incarceration data to the U.S. shapefile
state_shape <- map_data("state") %>%
  rename(state = region)%>%
  mutate(state = state2abbr(state))%>%
  left_join(latinx_state_incarceration, by = "state")

# give it a blank theme for the minimalistic theme as mentioned in the assignment
# description
# Define a minimalist theme for maps
blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),        # remove axis lines
    axis.text = element_blank(),        # remove axis labels
    axis.ticks = element_blank(),       # remove axis ticks
    axis.title = element_blank(),       # remove axis titles
    plot.background = element_blank(),  # remove gray background
    panel.grid.major = element_blank(), # remove major grid lines
    panel.grid.minor = element_blank(), # remove minor grid lines
    panel.border = element_blank()      # remove border around plot
  )

# draw the map setting the fill of each state using latinx population in the jail
map_chart <- ggplot(state_shape) +
            geom_polygon(
              mapping = aes(x = long, y = lat, group = group, fill = latinx_pop_rate_jail),
              color = "white",
              size = 0.1
            ) +
            coord_map() +
            scale_fill_continuous(low = "#89CFF0", high = "#301934") +
            labs(
              fill = "Latinx Population in the Jail",
              title = "U.S. Average Population Rate of Latinx in Jail by Most Recent Year") + 
            blank_theme


