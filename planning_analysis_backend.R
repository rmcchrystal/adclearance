

###########################################################################
###########################################################################
###                                                                     ###
###                ADCLEARANCE | DEVELOPING ANALYSIS SCRIPT             ###
###                      RYAN MCCHRYSTAL | 40205088                     ###
###                                                                     ###
###########################################################################
###########################################################################

# Working directory
setwd(choose.dir())


# Packages
library(tidyverse)
library(readxl)
library(lubridate)
library(hms)


# Removing scientific notation to display INHAL_ID fully
options(scipen = 100)


# Importing and removing NA entries
data <- 
  read_excel("data.xlsx") %>% 
  filter(!is.na(PATIENTNUMBER) 
         & grepl(paste0(IDs, collapse = "|"), PATIENTNUMBER))


# IDs of participants with at least one year of data
IDs <- c("R01008", "R01011", "R01012", "R03003", "R03005", "R06001", "R06002", 
         "R06008", "R06009", "R06011", "R06012", "R06013", "R06015", "R06016", 
         "R07003", "R08005", "R08007", "R11004", "R11014", "R11015", "R11016", 
         "R11018", "R15005", "R15009")

##================================================================
##                        DATA WRANGLING                         =
##================================================================

# Extracting datetime to readable format
# Filtering to trial dates (27th June 2018 onward)
# Arranging by ID, date and time for easier reading
# Conditional indexing of entries for appropriate subsets
data_datetime <- 
  data %>%
  mutate(
    DATE = strtrim(INHAL_ID, 8),
    TIME = substr(INHAL_ID, 9, 14),
    DATE = as_date(ymd(DATE)),
    TIME = gsub('(..)(?=.)', '\\1:', TIME, perl=TRUE),
    TIME = as_hms(TIME)) %>%
  filter(!is.na(DATE) & DATE > "2018-06-27") %>%
  select(-c(INHAL_ID, SERIALNO)) %>%
  group_by(PATIENTNUMBER) %>%
  mutate(WEEK = week(DATE),
         MONTH = month(DATE),
         YEAR = year(DATE)) %>% ungroup() %>%
  select(PATIENTNUMBER, YEAR, WEEK, MONTH, DATE, TIME, everything(.)) %>%
  arrange(PATIENTNUMBER, DATE, TIME) %>%
  mutate(TYPE = case_when(INHAL_INTERRUPTION == 4 ~ "Nebulisation",
                          
                          INHAL_INTERRUPTION == 104
                          & VALID_INHAL != 0 ~ "Easycare Neb",
                          
                          INHAL_INTERRUPTION == 104
                          & VALID_INHAL == 0 ~ "Easycare",
                          
                          INHAL_INTERRUPTION %in% c(1:3, 5:8) ~ "Errors",
                          
                          INHAL_INTERRUPTION %in% c(101:103, 105:107) ~ "Easycare Error"))


# Adding visit windows with conditional indexes based on number of enrolled weeks
data_windows <- 
  data_datetime %>%
  group_by(PATIENTNUMBER, YEAR) %>%
  filter(!duplicated(WEEK)) %>%
  group_by(PATIENTNUMBER) %>%
  mutate(ENROLLED_WEEK = row_number(),
         WINDOW = case_when(ENROLLED_WEEK == 1 ~ "Visit 1",
                            ENROLLED_WEEK == 2 ~ "Visit 2",
                            ENROLLED_WEEK > 2 & ENROLLED_WEEK <= 8 ~ "Visit 3",
                            ENROLLED_WEEK > 8 & ENROLLED_WEEK <= 26 ~ "Visit 4",
                            ENROLLED_WEEK > 26 & ENROLLED_WEEK <= 52 ~ "Visit 5",
                            ENROLLED_WEEK > 52 ~ "Visit 6")) %>%
  select(PATIENTNUMBER, YEAR, WEEK, WINDOW) %>%
  right_join(., data_datetime) %>%
  arrange(PATIENTNUMBER, DATE)


# Hypertonic saline nebulisation subset
# Filtering to relevant entries
# Adding missing dates from start to end of enrolment data
# Calculating time difference between nebulisations to gauge adherence
# Conditionally indexing adherence based on method criteria
subset_HTS <- 
  data_windows %>%
  filter(TYPE %in% c("Nebulisation", "Easycare Neb")) %>%
  group_by(PATIENTNUMBER) %>%
  complete(DATE = seq.Date(min(DATE), max(DATE), "days")) %>%
  mutate(TYPE = ifelse(is.na(VALID_INHAL), "Missed Day", TYPE)) %>%
  group_by(PATIENTNUMBER, DATE) %>%
  mutate(WEEK = week(DATE),
         MONTH = month(DATE),
         YEAR = year(DATE),
         TIMEDIFF = round((unclass(TIME) - unclass(lag(TIME)))/3600,1),
         ADHERENT = case_when(n() == 2
                              & INHAL_PERIOD >= 40
                              & TIMEDIFF >= 1
                              & VALID_INHAL %in% c(1,2) ~ 1,
                              TRUE ~ 0)) %>% ungroup()


# Re-adding visit windows to sequenced dates
# Re-joining to original hypertonic saline subset
subset_HTS2 <- 
  subset_HTS %>%
  group_by(PATIENTNUMBER, YEAR) %>%
  filter(!duplicated(WEEK)) %>%
  group_by(PATIENTNUMBER) %>%
  mutate(ENROLLED_WEEK = row_number(),
         WINDOW = case_when(ENROLLED_WEEK == 1 ~ "Visit 1",
                            ENROLLED_WEEK == 2 ~ "Visit 2",
                            ENROLLED_WEEK > 2 & ENROLLED_WEEK <= 8 ~ "Visit 3",
                            ENROLLED_WEEK > 8 & ENROLLED_WEEK <= 26 ~ "Visit 4",
                            ENROLLED_WEEK > 26 & ENROLLED_WEEK <= 52 ~ "Visit 5",
                            ENROLLED_WEEK > 52 ~ "Visit 6")) %>%
  select(PATIENTNUMBER, YEAR, WEEK, WINDOW2 = WINDOW) %>%
  right_join(., subset_HTS) %>%
  select(-c(WINDOW)) %>%
  rename(WINDOW = WINDOW2) %>%
  arrange(PATIENTNUMBER, DATE) %>%
  filter(WINDOW == "Visit 5")


# Easycare cleaning sessions subset
# Filtering to days with less than 2 daily sessions to extract real cleaning sessions
# and exclude nebulisations performed in easycare mode
# Counting the total number of monthly easycare sessions for each participant
# Calculating the mean number of monthly easycare sessions per participant
subset_clean <- 
  data_windows %>%
  group_by(PATIENTNUMBER, DATE) %>%
  filter(TYPE == "Easycare") %>%
  filter(n() < 2) %>%
  group_by(PATIENTNUMBER, MONTH) %>%
  mutate(COUNT = n()) %>%
  group_by(PATIENTNUMBER) %>%
  mutate(MEAN = round(mean(COUNT),1)) %>%
  filter(!duplicated(MEAN)) %>% ungroup()
  

# Hypertonic saline nebulisation errors subset
# Conditionally indexing errors to worded responses
subset_errors <- 
  data_windows %>%
  filter(TYPE %in% c("Errors")) %>%
  mutate(CODE = case_when(INHAL_INTERRUPTION == 1 ~ "Lost supply voltage",
                           INHAL_INTERRUPTION == 2 ~ "Aerosol head disconnected",
                           INHAL_INTERRUPTION == 3 ~ "Medication missing",
                           INHAL_INTERRUPTION == 5 ~ "Manual shutdown",
                           INHAL_INTERRUPTION == 6 ~ "Battery empty",
                           INHAL_INTERRUPTION == 7 ~ "Timeout",
                           INHAL_INTERRUPTION == 8 ~ "Paused Timeout"))

##================================================================
##                      ADHERENCE TO HTS                         =
##================================================================

# Daily mean adherence to nebulisations
# Counting the daily number of nebulisations performed everyday
# Calculating the mean number of nebulisations performed daily by each participant
adherence_dailyMean <- 
  subset_HTS2 %>%
  group_by(PATIENTNUMBER, DATE) %>%
  mutate(DAILY_N = n()) %>%
  group_by(PATIENTNUMBER) %>%
  summarise(DAILY_MEAN = round(mean(DAILY_N, na.rm = T),2)) %>% ungroup()


# Daily count of nebulisations
adherence_daily <- 
  subset_HTS2 %>%
  group_by(PATIENTNUMBER, DATE) %>%
  mutate(DAILY_N = n()) %>%
  filter(!duplicated(DAILY_N)) %>%
  mutate(DAILY_N = ifelse(is.na(VALID_INHAL), 0, DAILY_N)) %>%
  select(PATIENTNUMBER, DATE, DAILY_N, WINDOW) %>% ungroup()


# Cumulative adherence rate
# Distinguishing missing days from duplicates among daily entries
# Selecting adherent days, missing days and nonadherent days
# Adding a day counter for cumulative adherence calculations
# Calculating cumulative adherence rate based on the cumulative
# summary of adherence days across cumulative enrolment
adherence_cumulative <- 
  subset_HTS2 %>%
  group_by(PATIENTNUMBER, DATE) %>%
  mutate(ADHERENT = ifelse(TYPE == "Missed Day", NA_real_, ADHERENT)) %>%
  filter(ADHERENT %in% c(1, NA_real_) | sum(ADHERENT) == 0) %>%
  group_by(PATIENTNUMBER) %>%
  mutate(ADHERENT = ifelse(is.na(ADHERENT), 0, ADHERENT),
         DAY = row_number(),
         CUMUL = cumsum(ADHERENT)/DAY * 100)


# Weekly adherence
# Counting the number of adherent days per week per participant
# Conditionally indexing adherent weeks as those with at least 5
# of 7 days being adherent
# Calculating a weekly adherence rate based on adherent weeks
adherence_weekly <- 
  subset_HTS2 %>%
  group_by(PATIENTNUMBER, WEEK, YEAR) %>%
  mutate(WEEK_N = sum(ADHERENT)) %>%
  filter(!duplicated(DATE)) %>%
  mutate(WEEK_ADH = ifelse(WEEK_N/length(WEEK) > 0.7, 1, 0)) %>%
  group_by(PATIENTNUMBER, YEAR) %>%
  filter(!duplicated(WEEK)) %>%
  group_by(PATIENTNUMBER) %>%
  mutate(WEEKS = row_number(),
         WEEKLY = round(sum(WEEK_ADH)/length(WEEKS)*100,2)) %>%
  filter(!duplicated(WEEKLY)) %>%
  select(PATIENTNUMBER, WEEKLY) %>% ungroup()

# Monthly adherence
# Calculating monthly adherence from the number of adherent
# weeks in a month
# Calculating the mean monthly adherence per patient
adherence_monthly <- 
  subset_HTS2 %>%
  group_by(PATIENTNUMBER, WEEK, YEAR) %>%
  mutate(WEEK_N = sum(ADHERENT)) %>%
  filter(!duplicated(DATE)) %>%
  mutate(WEEK_ADH = ifelse(WEEK_N/length(WEEK) > 0.7, 1, 0)) %>%
  group_by(PATIENTNUMBER, YEAR) %>%
  filter(!duplicated(WEEK)) %>%
  group_by(PATIENTNUMBER, MONTH) %>%
  mutate(MONTHLY = round(sum(WEEK_ADH)/length(MONTH)*100,2)) %>%
  filter(!duplicated(MONTHLY)) %>%
  group_by(PATIENTNUMBER) %>%
  mutate(MONTHLY = round(mean(MONTHLY),2)) %>%
  filter(!duplicated(MONTHLY)) %>%
  select(PATIENTNUMBER, MONTHLY) %>% ungroup()

##================================================================
##                GATHERING DASHBOARD STATISTICS                 =
##================================================================

# Patient IDs for individual entries of dashboard dataset
dashboard_IDs <- unique(subset_HTS$PATIENTNUMBER)

# Monthly cleaning data
cleaning <- subset_clean %>% select(PATIENTNUMBER, MEAN)

# Most commonly occurring hypertonic saline error
errors <- 
  subset_errors %>%
  group_by(PATIENTNUMBER) %>% 
  count(CODE, name = "OCCURRENCE") %>% 
  slice(which.max(OCCURRENCE)) %>% ungroup()


# Total enrolled days
dashboard_overall <- 
  adherence_cumulative %>%
  group_by(PATIENTNUMBER) %>%
  mutate(ENROLLED_DAYS = length(PATIENTNUMBER)) %>%
  filter(!duplicated(ENROLLED_DAYS)) %>%
  select(PATIENTNUMBER, ENROLLED_DAYS) %>% ungroup()

# Total adherent days
dashboard_days <- 
  subset_HTS2 %>%
  group_by(PATIENTNUMBER) %>%
  filter(ADHERENT == 1) %>%
  mutate(ADHERENT_DAYS = n()) %>%
  filter(!duplicated(ADHERENT_DAYS)) %>%
  select(PATIENTNUMBER, ADHERENT_DAYS) %>% ungroup()

# Total nonadherent days
dashboard_missed <- 
  subset_HTS2 %>%
  group_by(PATIENTNUMBER, DATE) %>%
  filter(sum(ADHERENT) == 0) %>%
  group_by(PATIENTNUMBER) %>%
  count(name = "MISSED_DAYS") %>% ungroup()

# Gathering data of interest into a dashboard dataset
# Participant IDs
# Total enrolled days
# Total adherent days
# Daily adherence rate calculated by dividing the number of
# adherent days by total enrolled days
# Total nonadherent days (incl. missed days) calculated by
# subtracting the daily adherence rate from 100%
# Weekly and monthly adherence rates
# Mean daily nebulisations
# Average monthly Easycare cleaning sessions
# Most common nebulisation error
# Calculating overall adherence from the mean of daily,
# weekly and monthly adherence rates
dashboard_data <- 
  data.frame(PATIENTNUMBER = dashboard_IDs) %>%
  full_join(., dashboard_overall) %>%
  full_join(., dashboard_days) %>%
  mutate(DAILY_ADH = round(ADHERENT_DAYS/ENROLLED_DAYS*100),
         PCNT_MISSED = round(100 - DAILY_ADH)) %>%
  full_join(., adherence_weekly) %>%
  full_join(., adherence_monthly) %>%
  full_join(., adherence_dailyMean) %>%
  full_join(., cleaning) %>%
  full_join(., error) %>%
  replace(is.na(.), 0) %>%
  mutate(OVERALL = round((DAILY_ADH + WEEKLY + MONTHLY)/3,1))


##================================================================
##                        VISUALISATIONS                         =
##================================================================

# Test daily nebulisations plot
# Area plot showing changes in daily nebulisations over enrolment
adherence_daily %>%
  filter(PATIENTNUMBER == "R01011") %>%
  ggplot(aes(DATE, DAILY_N)) +
  geom_area(fill = "skyblue", colour = "black") +
  labs(x = "Date", y = "Daily nebulisations performed") +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d %Y") +
  scale_y_continuous(breaks = seq(0, max(adherence_daily$DAILY_N))) +
  geom_hline(aes(yintercept = 2, colour = "Target"), linetype = "dashed", size = 2) +
  theme_bw(base_size = 20) +
  theme(axis.text.x = element_text(angle = 70, hjust = 1)) +
  scale_colour_manual(name = NULL, values = c("green"))

# Test cumulative adherence plot
# Line plot showing changes in adherence rates throughout enrolment
adherence_cumulative %>%
  filter(PATIENTNUMBER == "R01011") %>%
  ggplot(aes(DATE, CUMUL)) +
  geom_line(size = 1.5) +
  labs(x = "Date", y = "Cumulative Adherence Rate (%)") +
  scale_y_continuous(limits = c(0,100), n.breaks = 10) +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d %Y") +
  geom_hline(aes(yintercept = 80, colour = "Target"), linetype = "dashed", size = 2) +
  theme_bw(base_size = 20) +
  theme(axis.text.x = element_text(angle = 70, hjust = 1)) +
  scale_colour_manual(name = NULL, values = c("green", "red"))
