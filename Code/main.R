#### OUR MAIN CODE WILL GO HERE ####

#load required libraries
library(tidyverse)
library(lubridate)

#import datasets

events1 <- read_csv("https://github.com/bigdatacup/Big-Data-Cup-2025/releases/download/Data/2024-10-25.Team.H.@.Team.G.-.Events.csv")
shifts1 <- read_csv("https://github.com/bigdatacup/Big-Data-Cup-2025/releases/download/Data/2024-10-25.Team.H.@.Team.G.-.Shifts.csv")
tracking1 <- read_csv("https://github.com/bigdatacup/Big-Data-Cup-2025/releases/download/Data/2024-10-25.Team.H.@.Team.G.-.Tracking.csv")

events2 <- read_csv("https://github.com/bigdatacup/Big-Data-Cup-2025/releases/download/Data/2024-11-15.Team.D.@.Team.C.-.Events.csv")
shifts2 <- read_csv("https://github.com/bigdatacup/Big-Data-Cup-2025/releases/download/Data/2024-11-15.Team.D.@.Team.C.-.Shifts.csv")
tracking2 <- read_csv("https://github.com/bigdatacup/Big-Data-Cup-2025/releases/download/Data/2024-11-15.Team.D.@.Team.C.-.Tracking.csv")

events3 <- read_csv("https://github.com/bigdatacup/Big-Data-Cup-2025/releases/download/Data/2024-11-16.Team.F.@.Team.E.-.Events.csv")
shifts3 <- read_csv("https://github.com/bigdatacup/Big-Data-Cup-2025/releases/download/Data/2024-11-16.Team.F.@.Team.E.-.Shifts.csv")
tracking3 <- read_csv("https://github.com/bigdatacup/Big-Data-Cup-2025/releases/download/Data/2024-11-16.Team.F.@.Team.E.-.Tracking.csv")

camera_orientations <- read_csv("https://github.com/bigdatacup/Big-Data-Cup-2025/releases/download/Data/camera_orientations.csv")

#view datasets
View(events1)
View(shifts1)
View(tracking1)

View(events2)
View(shifts2)
View(tracking2)

View(events3)
View(shifts3)
View(tracking3)

View(camera_orientations)

##create game_id variable for each dataset

#list of datasets
dataset_names <- c(
  "events1", "shifts1", "tracking1",
  "events2", "shifts2", "tracking2",
  "events3", "shifts3", "tracking3"
)

#create for loop

for (name in dataset_names) {
  
  #extract the number at the end of the dataset (1, 2, 3)
  game_num <- gsub(".*([0-9])$", "\\1", name)
  
  #get the dataset object
  df <- get(name)
  
  #add the game_id column
  #df$game_id <- game_id_value
  df$game_id <- game_num
  df$game_id <- as.double(df$game_id)
  
  #assign it back to the original object name
  assign(name, df)
}

##merge datatsets by stacking them

#to combine events data together, Player_ID_2 in events3 dataset must be converted to double type to match events1 and events2 dataset
#one observation in Player_ID_2 in events3 dataset is "Go" instead of a number which gets converted to an NA value
# sapply(events1, class)
# sapply(events2, class)
# sapply(events3, class)
# 
# unique(events3$Player_ID_2)

#change player id2 in events 1 and 2 dataset to character
events1$Player_Id_2 <- as.character(events1$Player_Id_2)
events2$Player_Id_2 <- as.character(events2$Player_Id_2)

#put events, shifts and tracking data into separate lists then row bind them
events_list   <- list(events1, events2, events3)
shifts_list   <- list(shifts1, shifts2, shifts3)
tracking_list <- list(tracking1, tracking2, tracking3)

#combine dataset types together 
events_data <- bind_rows(events_list) 
shifts_data   <- bind_rows(shifts_list)
tracking_data <- bind_rows(tracking_list)

#view datasets
View(events_data)
View(shifts_data)
View(tracking_data)

glimpse(events_data)
glimpse(shifts_data)
glimpse(tracking_data)

##renaming variables with underscore between spaces for tracking data and converting all variable names to lowercase
tracking_data_clean <- tracking_data %>% rename(image_id = "Image Id",
                                                game_clock = "Game Clock",
                                                player_or_puck = "Player or Puck",
                                                player_id = "Player Id",
                                                rink_location_x_feet = "Rink Location X (Feet)",
                                                rink_location_y_feet = "Rink Location Y (Feet)",
                                                rink_location_z_feet = "Rink Location Z (Feet)",
                                                goal_score = "Goal Score") %>% 
  rename_all(tolower)
  

events_data_clean <- events_data %>% rename_all(tolower)
shifts_data_clean <- shifts_data %>% rename_all(tolower)

##convert clock to seconds counting up

#in each dataset, extra 0s are added to the end of each time (example: 20:00 is 20:00:00) which makes it seem like hms when it should be ms

#first converting clock variables to character types to get rid of the last ":00"
events_data_clean <- events_data_clean %>%
  mutate(clock = as.character(clock))

shifts_data_clean <-  shifts_data_clean %>%
  mutate(across(c(start_clock, end_clock, shift_length), as.character))

tracking_data_clean <-  tracking_data_clean %>%
  mutate(game_clock = as.character(game_clock))

#getting rid of the last ":00"
events_data_clean <- events_data_clean %>%
  mutate(clock = sub(":00$", "", clock))

shifts_data_clean <- shifts_data_clean %>%
  mutate(across(c(start_clock, end_clock, shift_length), ~ sub(":00$", "", .)))

tracking_data_clean <- tracking_data_clean %>%
  mutate(game_clock = sub(":00$", "", game_clock))

glimpse(events_data_clean)
glimpse(shifts_data_clean)
glimpse(tracking_data_clean)

#calculate the running clock seconds

#for events data
events_data_clean <- events_data_clean %>%
  mutate(clock_seconds_remaining = seconds(ms(clock))) %>%    #convert clock from "MM:SS" to seconds
  mutate(period_length = 20 * 60) %>%    #calculate period length in seconds (20 mins periods)
  mutate(clock_seconds_elapsed = period_length - clock_seconds_remaining) %>%   #convert to seconds elapsed in the period
  group_by(game_id) %>% #calculate running clock per game
  arrange(period, desc(clock_seconds_remaining), .by_group = TRUE) %>%
  mutate(running_clock_seconds = clock_seconds_elapsed + 
           (period - 1) * period_length) %>%
  ungroup()

events_data_clean <- events_data_clean %>%            #removing the intermediate variables needed to calculate running_clock_seconds
  select(-clock_seconds_remaining, -period_length, -clock_seconds_elapsed) %>%
  mutate(running_clock_seconds = as.double(running_clock_seconds))   #converting from seconds format/type to a number

View(data.frame(events_data_clean$clock, events_data_clean$running_clock_seconds,  events_data_clean$period, events_data_clean$game_id))

#for shifts data - just converting everything to seconds (not calculating a running clock since shifts are in intervals of time)
shifts_data_clean <- shifts_data_clean %>%
  mutate(start_clock_seconds = seconds(ms(start_clock)),
         end_clock_seconds =  seconds(ms(end_clock)),
         shift_length_seconds =  seconds(ms(shift_length))) %>%
  mutate(across(c(start_clock_seconds, end_clock_seconds, shift_length_seconds), as.double))

glimpse(shifts_data_clean)

#for tracking data
tracking_data_clean <- tracking_data_clean %>%
  mutate(clock_seconds_remaining = seconds(ms(game_clock))) %>%    #convert clock from "MM:SS" to seconds
  mutate(period_length = 20 * 60) %>%    #calculate period length in seconds (20 mins periods)
  mutate(clock_seconds_elapsed = period_length - clock_seconds_remaining) %>%   #convert to seconds elapsed in the period
  group_by(game_id) %>% #calculate running clock per game
  arrange(period, desc(clock_seconds_remaining), .by_group = TRUE) %>%
  mutate(running_clock_seconds = clock_seconds_elapsed + 
           (period - 1) * period_length) %>%
  ungroup()

tracking_data_clean <- tracking_data_clean %>%            #removing the intermediate variables needed to calculate running_clock_seconds
  select(-clock_seconds_remaining, -period_length, -clock_seconds_elapsed) %>%
  mutate(running_clock_seconds = as.double(running_clock_seconds))   #converting from seconds format/type to a number

View(data.frame(tracking_data_clean$game_clock, tracking_data_clean$running_clock_seconds,  tracking_data_clean$period, tracking_data_clean$game_id))

# clean tracking data by aggregating/averaging the players coordinates at each second
agg_tracking <- tracking_data_clean %>%
  filter(!(is.na(rink_location_x_feet) | is.na(rink_location_y_feet | is.na(rink_location_z_feet)))) %>%
  select(-image_id, -game_clock) %>%
  group_by(game_id, running_clock_seconds, player_or_puck, team, player_id) %>%
  summarise(across(where(is.numeric), mean), .groups = "drop")
