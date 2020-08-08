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

new_cases <- function(value) {                     # Function for plot # 4  
  length_date <- length(value)                             
  previous_day <- c(0, 
                    value[1:length_date - 1])
  diff <- value - previous_day
  return(diff)
} 


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
covid_data_raw <- left_join(covid_cases,           # Joinging cases and deaths
                               covid_deaths)
covid_region <- covid_data_raw %>% 
  filter(date >= dmy(first_MO_case)) %>%           # Tidying by region
  mutate(Region = case_when(
    stateFIPS %in% Northeast_FIPS ~ "Northeast",
    stateFIPS %in% Midwest_FIPS ~ "Midwest",
    stateFIPS %in% South_FIPS ~ "South",
    stateFIPS %in% West_FIPS ~ "West")) %>% 
  group_by(Region,                                 # Grouping by region and date 
           date) %>% 
  summarise(total_cases = sum(cases,               # Summing cases
                              na.rm = TRUE),
            total_deaths = sum(deaths,             # Summing deaths
                               na.rm = TRUE),
            .groups = "drop")                        # Dropping groups
p_c <- covid_region %>%                            # Plotting date vs total cases by region 
  ggplot()+
  geom_line(aes(x = date,
                y = total_cases,
                color = Region),
            size = 0.7) +                          # Setting size of line
  labs(x = NULL,                                   # Renaming axes  
       y = "Total Cases") +
  theme_test()+                                    # Using test theme
  theme(legend.position = "bottom")                # Placing legend on bottom
p_d <- covid_region %>% 
  ggplot()+
  geom_line(aes(x = date,                          # Plotting date vs. deaths vs. region
                y = total_deaths,
                color = Region), 
            size=0.7) +                            # Setting size of line      
  labs(x = NULL,                                   # Renaming axes
       y = "Total Deaths") +
  theme_test() +                                   # Test theme
  theme(legend.position = "none")                  # Removing legend
p_c + p_d + plot_layout(ncol = 2)                  # Displaying plot 1: cases next to deaths 


# Plot 2 ------------------------------------------------------------------

MO_covid_cases <- covid_cases %>% 
  filter(State == "MO",
         date >= dmy(first_MO_case)) %>%                # Filtering by Missouri
  mutate(`County Name` = str_replace(`County Name`,
                                     " County$",""),    
         `County Name`= str_replace(`County Name`,      # Fixing county nomenclature
                                   "^Jackson ","")) %>% 
  group_by(`County Name`,                               # Grouping by County name and date
           date) %>% 
  summarise(total_confirmed = sum(cases,
                                  na.rm = TRUE),        # summing Mo cases
            .groups='drop') %>% 
  rename("County" = `County Name`)                      # Renaming County Name column

SEMO_counties <- SEMO_county_pop %>%                    # Fixing county names
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
  select(-c("2015":"2018"))                            # Selecting 2019 year 

MO_covid_SEMO <- left_join(MO_covid_cases,             # Joining mo and semo
                           SEMO_counties)

MO_covid_SEMO %>% 
  ggplot()+
  geom_line(aes(x = date,                              # Plotting date vs case # vy county  
                y = total_confirmed,
                color = County)) +
  labs(x = NULL,
       y = "Total Confirmed Cases") +                  # Renaming axes
  gghighlight(`2019` >= 200,
              use_direct_label = FALSE) +              # Highlighting counties with 200 or more students
  scale_x_date(date_labels = "%d %b")                  # Setting up scale


# Plot 3 ------------------------------------------------------------------

covid_confirmed_raw3 <- read_csv(here("data",
                                     "covid_confirmed_usafacts.csv"))

covid_cases_3 <- covid_confirmed_raw3 %>% 
  pivot_longer(c(`1/22/20`:`7/31/20`),
               names_to = "date",
               values_to = "cases") %>%
  mutate(date = mdy(date)) %>% 
  filter(date%in%c(ymd("2020-04-01"):ymd("2020-04-30"),
                 ymd("2020-07-01"):ymd("2020-07-30"))) %>% 
  mutate(date = month(date))
  group_by(State, date, cases) %>% 
  summarise(total_cases = sum(cases)) %>% 
  mutate(date = month(date))

view(covid_cases_3)

view(covid_cases_3)
mutate(month = month(date)) %>%
  group_by(State,`County Name`, month) %>%
  summarise(total_cases_county = sum(cases,
                                     na.rm = TRUE)) %>% 
  mutate(rate_county = sum(total_cases_county))
covid_cases_3

## I just could not figure this one out and ran out of time. You win some....

# Plot #4 -----------------------------------------------------------------

MO_daily_cases <- MO_covid_cases %>%            # Using Mo covid data from before
  group_by(date) %>% 
  summarise(cases = sum(total_confirmed,        # Calculating daily cases in Missouri 
                                na.rm=TRUE),
            .groups='drop') %>% 
  mutate(daily = new_cases(`cases`))            # Using function to calculate new daily cases

MO_daily_cases$roll_mean <-                     # Calculating rolling mean, for the week, aligning right
  data.table::frollmean(MO_daily_cases$daily,
                        7,
                        align = "right") %>% 
  replace_na(0)                                 # Replacing na with 0

MO_daily_cases %>%                              # Plotting
  ggplot(aes(x = date,                          # Setting axes
             y = daily)) +
  geom_col(color = "grey88",                    # setting colors for columns
           fill = "grey68") +
  geom_line(aes(x = date,                       # Adding in rolling mean line with "Cardiac Red" at size = 0.75
                y = roll_mean),
            color = "#9D2235",
            size = 0.75) +
  geom_col(data = filter(MO_daily_cases,        # Adding in "Missouri reopened" line
                         date == dmy("16 June 2020")),
           mapping = aes(x = date, 
                         y = daily),
           color = "gray68",
           fill = "#C8102E") +                  # Color "Southeast Red"
  scale_x_date(date_labels = "%b%d",
               date_breaks = "2 weeks") +       # Setting date ticks at bottom
  theme_test()+
  annotate(geom = "text",
           x = mdy("Jun 16 2020"),              # Adding in annotation date
           y = 228,
           label = "Missouri reopened\n16 June 2020", 
           color = "#C8102E", 
           fill = "C8102E") +
  labs(x = NULL, 
       y = "Daily New Cases")                   # setting plot axes labels


# Plot #5 -----------------------------------------------------------------


