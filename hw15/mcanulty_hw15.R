## Ryan McAnulty
## HW 15: Analysis of COVID-19 Data

# Loading Libraries -------------------------------------------------------

library(tidyverse)
library(here)
library(lubridate)
library(sf)
library(patchwork)
library(gghighlight)
library(ggthemes)


# Define Constants --------------------------------------------------------

first_US_case <- "19 Jan 2020"
first_MO_case <- "08 Mar 2020"
lower_48 <- c("Alabama", "Arizona", "Arkansas", "California",
              "Colorado", "Connecticut", "Delaware", "Florida",
              "Georgia", "Idaho", "Illinois", "Indiana", "Iowa",
              "Kansas", "Kentucky", "Louisiana", "Maine",
              "Maryland", "Massachusetts", "Michigan", "Minnesota",
              "Mississippi", "Missouri", "Montana", "Nebraska",
              "Nevada", "New Hampshire", "New Jersey", "New Mexico",
              "New York", "North Carolina", "North Dakota", "Ohio",
              "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island",
              "South Carolina", "South Dakota", "Tennessee", "Texas",
              "Utah", "Vermont", "Virginia", "Washington",
              "West Virginia", "Wisconsin", "Wyoming")

Northeast_FIPS <- c(9, 23, 25, 33, 34, 36, 42, 44, 50)
Midwest_FIPS <- c(17, 18, 19, 20, 26, 27, 29, 31, 38, 39, 46, 55)
South_FIPS <- c(1, 5, 10, 11, 12, 13, 21, 22, 24, 28, 37, 40, 45, 
                47, 48, 51, 54)
West_FIPS <- c(2, 4, 6, 8, 15, 16, 30, 32, 35, 41, 49, 53, 56)

state_population <- county_pop_raw %>%
  filter(countyFIPS != 0)

# Functions ---------------------------------------------------------------




# Importing ---------------------------------------------------------------

covid_confirmed_raw <- read_csv(here("data",
                                     "covid_confirmed_usafacts.csv"))
covid_deaths_raw <- read_csv(here("data",
                                  "covid_deaths_usafacts.csv"))
county_pop_raw <- read_csv(here("data",
                                "covid_county_population_usafacts.csv"))
SEMO_raw <- read_csv(here("data",
                          "semo_county_enrollment.csv"),
                     skip = 1)



# Initial Wrangle ---------------------------------------------------------
covid_cases <- covid_confirmed_raw %>% 
  filter(countyFIPS != 0,
         stateFIPS != 0) %>% 
  pivot_longer(c(`1/22/20`:`7/31/20`),
               names_to = "date",
               values_to = "cases") %>% 
  mutate(date = mdy(date)) %>% 
  filter(date >= dmy(first_MO_case))
covid_deaths <- covid_deaths_raw %>% 
  filter(countyFIPS != 0,
         stateFIPS != 0) %>% 
  pivot_longer(c(`1/22/20`:`7/31/20`),
               names_to = "date", values_to = "cases") %>% 
  mutate(date = mdy(date)) %>% 
  filter(date >= dmy(first_MO_case)) %>% 
  rename("deaths" = cases)
county_pop <- county_pop_raw %>% 
  filter(countyFIPS != 0) %>% 
  rename("County_Name" = `County Name`)
SEMO_county_pop <- SEMO_raw %>% 
  rename("County" = X1)

# Plot 1 ----------------------------------------------------
covid_data_raw <- left_join(covid_cases,
                               covid_deaths)
covid_region <- covid_data_raw %>% 
  filter(date >= dmy(first_MO_case)) %>% 
  mutate(Region = case_when(
    stateFIPS %in% Northeast_FIPS ~ "Northeast",
    stateFIPS %in% Midwest_FIPS ~ "Midwest",
    stateFIPS %in% South_FIPS ~ "South",
    stateFIPS %in% West_FIPS ~ "West"
  )) %>% 
  group_by(Region,
           date) %>% 
  summarise(total_cases = sum(cases,
                              na.rm=TRUE),
            total_deaths = sum(deaths,
                               na.rm=TRUE),
            .groups="drop")
p_c <- covid_region %>% 
  ggplot()+
  geom_line(aes(x = date,
                y = total_cases,
                color = Region),
            size = 0.7) +
  labs(x = NULL,
       y = "Total Cases") +
  theme_test()+
  theme(legend.position = "bottom")
p_d <- covid_region %>% 
  ggplot()+
  geom_line(aes(x = date,
                y = total_deaths,
                color = Region), size=0.7) +
  labs(x = NULL,
       y = "Total Deaths") +
  theme_test() +
  theme(legend.position = "none")
p_c + p_d + plot_layout(ncol = 2)


# Plot 2 ------------------------------------------------------------------

MO_covid_cases <- covid_cases %>% 
  filter(State == "MO",
         date >= dmy(first_MO_case)) %>% 
  mutate(`County Name` = str_replace(`County Name`,
                                     " County$",""),
         `County Name`= str_replace(`County Name`,
                                   "^Jackson ","")) %>% 
  group_by(`County Name`,
           date) %>% 
  summarise(total_confirmed = sum(cases,
                                  na.rm = TRUE),
            .groups='drop') %>% 
  rename("County" = `County Name`)

SEMO_counties <- SEMO_county_pop %>% 
  mutate(`County` = str_replace(`County`,
                                "De Kalb",
                                "DeKalb"),
         `County` = str_replace(`County`,
                              "^Sainte ",
                              "Ste\\. "),
         `County` = str_replace(`County`,
                                "^Saint ",
                                "St\\. "),
         `County` = str_replace(`County`,
                                "^St. Louis City$",
                                "City of St. Louis")) %>% 
  select(-c("2015":"2018")) 

MO_covid_SEMO <- left_join(MO_covid_cases,
                           SEMO_counties)

MO_covid_SEMO %>% 
  ggplot()+
  geom_line(aes(x = date,
                y = total_confirmed,
                color = County)) +
  labs(x = NULL,
       y = "Total Confirmed Cases") +
  gghighlight(`2019` >= 200,
              use_direct_label = FALSE) +
  scale_x_date(date_labels = "%d %b")


# Plot 3 ------------------------------------------------------------------

state_population <- county_pop_raw %>%
  filter(countyFIPS != 0)
apr_july_ca <- covid_cases %>%
  filter(date %in% c(dmy("01 Apr 2020"):dmy("30 Apr 2020"),
                     c(dmy("01 Jul 2020"):dmy("30 Jul 2020")))) %>% 
  mutate(month = month(date)) %>% 
  group_by(State, month) %>% 
  summarise(total_cases_county = sum(cases,
                                     na.rm = TRUE)) %>% 
left_join(state_population)  %>% 
  mutate(rate_county = (total_cases_county / population)
         )
apr_july_ca
a <- apr_july_ca %>%
  group_by(State, month) %>%
  summarise(avg_rate = sum(total_cases_county) / sum(population))

a
