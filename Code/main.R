#### OUR MAIN CODE WILL GO HERE ####

#load required libraries
library(tidyverse)
library(lubridate)

#### IMPORT DATASETS ####

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

##view datasets
# View(events1)
# View(shifts1)
# View(tracking1)
# 
# View(events2)
# View(shifts2)
# View(tracking2)
# 
# View(events3)
# View(shifts3)
# View(tracking3)
#
#View(camera_orientations)

##view missing clock data
# View(tracking1[303550:303970,])
# View(events1[events1$Period==2,])
# 
# View(tracking1[625560:626700,])
# View(events1[events1$Period==3,])

#### MERGE DATASETS ####
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
# View(events_data)
# View(shifts_data)
# View(tracking_data)

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

#### ADD RUNNING SECONDS VARIABLE ####
##extract image id
tracking_data_clean$image_id <- as.numeric(str_extract(tracking_data_clean$image_id, "\\d+$"))


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

#View(data.frame(events_data_clean$clock, events_data_clean$running_clock_seconds,  events_data_clean$period, events_data_clean$game_id))

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

#View(data.frame(tracking_data_clean$game_clock, tracking_data_clean$running_clock_seconds,  tracking_data_clean$period, tracking_data_clean$game_id))

#### AGGREGATE TRACKING DATA ####
# clean tracking data by aggregating/averaging the players coordinates at each second
agg_tracking <- tracking_data_clean %>%
  filter(!(is.na(rink_location_x_feet) | is.na(rink_location_y_feet | is.na(rink_location_z_feet)))) %>%
  select(-image_id) %>%
  group_by(game_id, period, running_clock_seconds, game_clock, player_or_puck, team, player_id) %>%
  summarise(across(where(is.numeric), mean), .groups = "drop")


#### FEATURE ENGINEERING ####
##create zones as locations using the x, y coordinates (zones in a hockey rink: defending zone, neutral zone, attacking zone)

#from camera_orientations.csv - teams switch sides each period
# Game 1: Team G (home team) and Team H (away team)
# Period 1 - Team G on right side and Team H on left side
# Period 2 - Team H on right side and Team G on left side
# Period 3 - Team G on right side and Team H on left side
# 
# Game 2: Team C (home team) and Team D (away team)
# Period 1 - Team D on right side and Team C on left side
# Period 2 - Team C on right side and Team D on left side
# Period 3 - Team D on right side and Team C on left side
# 
# Game 3: Team E (home team) and Team F (away team)
# Period 1 - Team F on right side and Team E on left side
# Period 2 - Team E on right side and Team F on left side
# Period 3 - Team F on right side and Team E on left side

#create table for team on right in period 1 (based on camera_orientations.csv)
game_orientation <- tibble(
  game_id = c(1, 2, 3),
  team_on_right_p1 = c("Team G", "Team D", "Team F")
)

#events data
#create variable indicating which direction the teams are moving in (attacking_direction) and create transformed x coordinate so all teams attack towards +x
events_zones <- events_data_clean %>%
  left_join(game_orientation, by = "game_id") %>%
  mutate(
    attacking_direction = case_when(
      #Periods 1 & 3
      period %in% c(1, 3) & team == team_on_right_p1 ~ "left",
      period %in% c(1, 3) & team != team_on_right_p1 ~ "right",
      #Period 2 (flipped)
      period == 2 & team == team_on_right_p1 ~ "right",
      period == 2 & team != team_on_right_p1 ~ "left",
      TRUE ~ NA_character_)    #for any row that didn’t match any of the conditions above, assign NA (just in case)
    ) %>%
    mutate( #if team is attacking towards left, multiply x and y coordinate by -1
      x1_att = if_else(attacking_direction == "left", -x_coordinate,  x_coordinate),
      y1_att = if_else(attacking_direction == "left", -y_coordinate,  y_coordinate),
      x2_att = if_else(attacking_direction == "left", -x_coordinate_2, x_coordinate_2),
      y2_att = if_else(attacking_direction == "left", -y_coordinate_2, y_coordinate_2)
    ) %>%
  mutate(
    x1_zone = case_when(
      x1_att < -89 ~ "BND",
      x1_att >= -89 & x1_att < -25 ~ "DZ",
      x1_att >= -25 & x1_att <  25 ~ "NZ",
      x1_att >=  25 & x1_att <= 89 ~ "OZ",
      x1_att > 89 ~ "BNO",
      TRUE ~ NA_character_),
    x2_zone = case_when(
      x2_att < -89 ~ "BND2",
      x2_att >= -89 & x2_att < -25 ~ "DZ2",
      x2_att >= -25 & x2_att <  25 ~ "NZ2",
      x2_att >=  25 & x2_att <= 89 ~ "OZ2",
      x2_att > 89 ~ "BNO2",
      TRUE ~ NA_character_)
    ) %>%
  select(-team_on_right_p1) #keeping attacking_direction in dataset since it will be used when calculating the distance for shots and goals

#tracking data

#add new variable called team_name for which team the row is referring to
game_teams <- tibble(
  game_id = c(1, 2, 3),
  home_team = c("Team G", "Team C", "Team E"),
  away_team = c("Team H", "Team D", "Team F")
)

agg_tracking_zones <- agg_tracking %>%
  left_join(game_teams, by = "game_id") %>%
  mutate(
    team_name = case_when(
      team == "Home" ~ home_team,
      team == "Away" ~ away_team,
      TRUE ~ NA_character_
    )
  )

#create variable indicating which direction the teams are moving in (attacking_direction) and create transformed x coordinate so all teams attack towards +x
agg_tracking_zones <- agg_tracking_zones %>%
  left_join(game_orientation, by = "game_id") %>%
  mutate(
    attacking_direction = case_when(
      #Periods 1 & 3
      period %in% c(1, 3) & team_name == team_on_right_p1 ~ "left",
      period %in% c(1, 3) & team_name != team_on_right_p1 ~ "right",
      #Period 2 (flipped)
      period == 2 & team_name == team_on_right_p1 ~ "right",
      period == 2 & team_name != team_on_right_p1 ~ "left",
      TRUE ~ NA_character_)    #for any row that didn’t match any of the conditions above, assign NA (just in case)
  ) %>%
  mutate( #if team is attacking towards left, multiply x and y coordinate by -1
    x_att = if_else(attacking_direction == "left", -rink_location_x_feet, rink_location_x_feet),
    y_att = if_else(attacking_direction == "left", -rink_location_y_feet, rink_location_y_feet)
  ) %>%
  mutate(
    zone = case_when(
      x_att < -89 ~ "BND",
      x_att >= -89 & x_att < -25 ~ "DZ",
      x_att >= -25 & x_att <  25 ~ "NZ",
      x_att >=  25 & x_att <= 89 ~ "OZ",
      x_att > 89 ~ "BNO",
      TRUE ~ NA_character_)
  ) %>%
 select(-home_team, -away_team, -team_on_right_p1, -attacking_direction)

#in tracking data, difficult to put what location puck is in since zones depend on what team we are looking at (offensive or defensive zone)

#double check length and width of rink ()
agg_tracking_zones %>%
  filter(player_or_puck == "Puck") %>%
  ggplot(aes(x = rink_location_x_feet,
             y = rink_location_y_feet)) +
  geom_point(alpha = 0.05) +
  coord_fixed()

##calculate distance for passes and shots

calculate_distance_coordinates <- function(x1, y1, x2, y2) {
  diff_sqr_x <- (x2 - x1)^2
  diff_sqr_y <- (y2 - y1)^2
  distance <- sqrt(diff_sqr_x + diff_sqr_y)
  
  return(distance)
}

#distance of passes, shots and goals

#need to add coordinates of goal nets to shots and goals - goal line is 11 feet from the end boards so left net is (-89, 0) and right net is at (89, 0)
#creating two new variables called new_x_coords_2 and new_y_coords_2 that incorporates both goal net coordinates and x_coordinate_2, y_coordinate_2
events_distance <- events_zones %>% 
  mutate(
    new_x_coords_2 = case_when(
      #creating new variable which takes on coordinates of left net if team's playing direction is left and right net if team's playing direction is right
      event %in% c("Shot", "Goal") & attacking_direction == "left"  ~ -89,
      event %in% c("Shot", "Goal") & attacking_direction == "right" ~  89,
      
      #copies over x_coordinate_2 from events play and incomplete play
      event %in% c("Play", "Incomplete Play") ~ x_coordinate_2,
      
      TRUE ~ NA_real_
    ),
    new_y_coords_2 = case_when(
      #y coordinate of nets are 0 (centre)
      event %in% c("Shot", "Goal") ~ 0,
      
      #copies over y_coordinate_2 from events play and incomplete play
      event %in% c("Play", "Incomplete Play") ~ y_coordinate_2,
      
      TRUE ~ NA_real_
    )
  )

#calculating distance for shots, goals, plays and incomplete plays
events_distance <- events_distance %>%
  rowwise() %>%
  mutate(
    distance = if (
      event %in% c("Shot", "Goal", "Play", "Incomplete Play") &&
      !is.na(new_x_coords_2) &&
      !is.na(new_y_coords_2)
    ) {
      calculate_distance_coordinates(
        x1 = x_coordinate,
        y1 = y_coordinate,
        x2 = new_x_coords_2,
        y2 = new_y_coords_2
      )
    } else {
      NA_real_
    }
  ) %>%
  ungroup() %>%
  select(-new_x_coords_2, -new_y_coords_2)


#create threshold variables for distance for shots and goals, and for passes and incomplete passes

events_play <- events_distance %>%
  filter(event %in% c("Play", "Incomplete Play"))

events_shots_goals <- events_distance %>%
  filter(event %in% c("Shot", "Goal"))

#events_distance_thresholds <- events_zones 

quantiles_passes <- quantile(events_play$distance, probs = c(0, 0.33, 0.66, 1), na.rm = TRUE)
quantiles_shots_goals <- quantile(events_shots_goals$distance, probs = c(0, 0.33, 0.66, 1), na.rm = TRUE)

quantiles_passes
quantiles_shots_goals

events_distance <- events_distance %>%
  mutate(
    pass_dist_threshold = case_when(
      event %in% c("Play", "Incomplete Play") ~ cut(
        distance,
        breaks = quantiles_passes,
        labels = c("Short", "Medium", "Long"),
        include.lowest = TRUE
      ),
      TRUE ~ NA_character_
    )) %>%
    mutate(
      shots_dist_threshold = case_when(
        event %in% c("Shot", "Goal") ~ cut(
          distance,
          breaks = quantiles_shots_goals,
          labels = c("Short", "Medium", "Long"),
          include.lowest = TRUE
        ),
        TRUE ~ NA_character_
      ))

#Fix True and False values in detail_3 and detail_4
events_distance <- events_distance %>%
  mutate(
  new_detail_3 = case_when(
    event %in% c("Shot", "Goal") & detail_3 == TRUE ~ "traffic",
    event %in% c("Shot", "Goal") & detail_3 == FALSE ~ "no_traffic",
    TRUE ~ NA_character_)    #for any row that didn’t match any of the conditions above, assign NA (just in case)
  ) %>%
  mutate(
    new_detail_4 = case_when(
      event %in% c("Shot", "Goal") & detail_4 == TRUE ~ "one_timer",
      event %in% c("Shot", "Goal") & detail_4 == FALSE ~ "not_one_timer",
      TRUE ~ NA_character_)    #for any row that didn’t match any of the conditions above, assign NA (just in case)
  ) 


## create direction variable for passes
events_distance <- events_distance %>%
  mutate(
    line1_y =  (x2_att - x1_att) + y1_att,
    line2_y = -(x2_att - x1_att) + y1_att,
    
    pass_direction = case_when(
      !event %in% c("Play", "Incomplete Play") ~ NA_character_, 
      x1_att == x2_att & y1_att == y2_att ~ "nm",
      x1_att == x2_att ~ "lat",
      y2_att <= line1_y & y2_att >= line2_y ~ "fw",
      y2_att >= line1_y & y2_att <= line2_y ~ "bw",
      x2_att > x1_att ~ "lfw",
      x2_att < x1_att ~ "lbw",
      TRUE ~ NA_character_
    )
  ) %>%
  select(-line1_y, -line2_y)

#add score state variable
events_distance <- events_distance %>%
  mutate(
    score_state = case_when(
      team == home_team & home_team_goals > away_team_goals ~ "lead",
      team == home_team & home_team_goals < away_team_goals ~ "trail",
      team == away_team & away_team_goals > home_team_goals ~ "lead",
      team == away_team & away_team_goals < home_team_goals ~ "trail",
      TRUE                                                  ~ "tied"
    )
  )


##add angle of shots
events_distance <- events_distance %>%
  mutate(post_angle = case_when(
    event %in% c("Shot", "Goal") ~ {
      up_dis <- calculate_distance_coordinates(x1_att, y1_att, 89,  3)
      down_dis <- calculate_distance_coordinates(x1_att, y1_att, 89, -3)
      posts_dis <- 6
        
      cos_value <- ((up_dis^2 + down_dis^2 - posts_dis^2) /
                        (2 * up_dis * down_dis))
      
      ifelse(x1_att == 89 & y1_att >= -3 & y1_att <= 3, 180,
             ifelse(x1_att == 89 & (y1_att < -3 | y1_att > 3), 0,
                    acos(cos_value) * 180 / pi))
      },
    TRUE ~ NA_real_))

summary(events_distance$post_angle)

events_distance <- events_distance %>%
  mutate(
    angle_threshold = case_when(
      post_angle <= 10 ~ "Ang(N)",
      post_angle <= 20 ~ "Ang(M)",
      post_angle > 20  ~ "Ang(W)",
      TRUE ~ NA_character_
    )
  )

#### CREATING POSSESSION ID ####

end_poss_events <- c("Shot", "Goal", "Penalty Taken", "Incomplete Play")

#drop unnecessary columns and ensure data is ordered
events_poss_id <- events_distance %>%
  select(-date,-clock) %>%
  arrange(game_id, period, running_clock_seconds) %>%
  mutate(end_prev = lag(event %in% end_poss_events, default = FALSE),
         time_gap = running_clock_seconds - lag(running_clock_seconds),
         new_possession = team != lag(team) | period != lag(period) |
           game_id != lag(game_id) | end_prev | time_gap > 10,
         new_possession = if_else(is.na(new_possession), TRUE, new_possession),
         possession_id = cumsum(new_possession)) %>%
  select(-new_possession, -end_prev)

#check number of distinct poss ids
n_distinct(events_poss_id$possession_id)

#number of possession chains of each size
events_poss_id %>% group_by(possession_id) %>%
  summarise(chain_size = n(), .groups = "drop") %>%
  count(chain_size, name = "num_possessions")

#keep only possession chains with at least two events
events_poss_id <- events_poss_id %>%
  group_by(possession_id) %>%
  filter(n() >= 2) %>%
  ungroup()

#check number of distinct poss ids
n_distinct(events_poss_id$possession_id)

#### TEST - looking at gaps in events per possession #########
#check for max time gap between events in a possession chain
possession_gaps <- events_poss_id %>%
  arrange(game_id, possession_id, running_clock_seconds) %>%
  group_by(game_id, possession_id) %>%
  mutate(
    gap_seconds = running_clock_seconds - lag(running_clock_seconds)
  ) %>%
  ungroup()

#get the max gap per possession
max_gap_per_possession <- possession_gaps %>%
  group_by(game_id, possession_id) %>%
  summarise(
    max_gap_seconds = max(gap_seconds, na.rm = TRUE),
    .groups = "drop"
  )

#look at the max gap
max(max_gap_per_possession$max_gap_seconds, na.rm = TRUE)

#look at all the gaps
test1 <- max_gap_per_possession %>%
  arrange(max_gap_seconds)

#look at events where gap seconds >= 9
problem_possessions <- max_gap_per_possession %>%
  filter(max_gap_seconds >= 9)

test2 <- possession_gaps %>% filter(gap_seconds >= 9)

test2 <- test2 %>% 
  select(game_id, possession_id, period, event, gap_seconds) %>%
  arrange(gap_seconds)
#View(test2)

#check number of distinct poss ids
n_distinct(events_poss_id$possession_id)

#### EXPLORATORY DATA ANALYSIS ####

#install.packages("patchwork")
library(patchwork)

#events categories
event_counts <- events_distance %>%
  count(event, sort = TRUE)

event_categories <- ggplot(event_counts, aes(x = reorder(event, n), y = n)) +
  geom_bar(stat = "identity", fill = "#53B8B8") +
  geom_text(aes(label = n), hjust = -0.1, size = 3.5) +  # Add counts
  coord_flip() +  # Flip for horizontal bars
  theme_minimal() +
  labs(
    title = "Number of Events by Type",
    x = "Event",
    y = "Count"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12, face = "bold")
  ) +
  expand_limits(y = max(event_counts$n) * 1.1)  # Make space for labels

event_categories

#histogram of distances for passes and incomplete passes distance
events_complete_play <- events_play %>% filter(event == "Play")

hist_distance_complete_passes <- ggplot(events_complete_play, aes(x = distance)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "black") +
  labs(
    title = "(a)",
    subtitle = "Complete Plays",
    x = "Distance (ft)",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 14)
  )

hist_distance_complete_passes

events_incomplete_play <- events_play %>% filter(event == "Incomplete Play")

hist_distance_incomplete_passes <- ggplot(events_incomplete_play, aes(x = distance)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "black") +
  labs(
    title = "(b)",
    subtitle = "Incomplete Plays",
    x = "Distance (ft)",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 14)
  )

hist_distance_incomplete_passes

distribution_charts_passes <- hist_distance_complete_passes + hist_distance_incomplete_passes +
  plot_annotation(
    title = "Distribution of Distances for Complete and Incomplete Plays",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    )
  )

distribution_charts_passes

#histogram of distances for shots and goals distance

events_shots <- events_shots_goals %>% filter(event == "Shot")

hist_distance_shots <- ggplot(events_shots, aes(x = distance)) +
  geom_histogram(binwidth = 5, fill = "#7Bd9F6", color = "black") +
  labs(
    title = "(a)",
    subtitle = "Shots",
    x = "Distance (ft)",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 14)
  )

hist_distance_shots

events_goals <- events_shots_goals %>% filter(event == "Goal")

hist_distance_goals <- ggplot(events_goals, aes(x = distance)) +
  geom_histogram(binwidth = 5, fill = "#7Bd9F6", color = "black") +
  labs(
    title = "(b)",
    subtitle = "Goals",
    x = "Distance (ft)",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 14)
  )

hist_distance_goals

distribution_charts_shots <- hist_distance_shots + hist_distance_goals +
  plot_annotation(
    title = "Distribution of Distances for Shots and Goals",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    )
  )

distribution_charts_shots


#histogram of angles for shots and goals
hist_angles_shots <- ggplot((events_distance %>% filter(event %in% c("Shot", "Goal"))), 
       aes(x = post_angle)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
  labs(
    title = "Distribution of Shot and Goal Angles",
       x = "Angle",
       y = "Frequency") +
  theme_minimal()  +
  theme(
    plot.title = element_text(hjust = 0.5),
  )

hist_angles_shots
  
#plot shots and goals location
location_shots <- ggplot(events_distance %>% filter(event %in% c("Shot", "Goal")),
                         aes(x = x1_att, y = y1_att)) +
  # shots in background
  geom_point(
    data = events_distance %>% filter(event == "Shot"),
    aes(color = "Shot"),
    alpha = 0.5, size = 1
  ) +
  # goals on top
  geom_point(
    data = events_distance %>% filter(event == "Goal"),
    aes(color = "Goal"),
    alpha = 0.8, size = 2
  ) +
  # rink outline (no legend)
  geom_rect(xmin = 25, xmax = 100,
            ymin = -42.5, ymax = 42.5,
            fill = NA, color = "black") +
  # goal mouth -> include in legend
  geom_segment(
    aes(color = "Net"),
    x = 89, xend = 89,
    y = -3, yend = 3,
    linewidth = 0.8
  ) +
  coord_fixed() +
  theme_minimal() +
  scale_color_manual(
    values = c("Shot" = "grey40",
               "Goal" = "blue",
               "Net"  = "red"),
    breaks = c("Shot", "Goal"),
    name = NULL
  ) +
  labs(
    title = "(a)",
    subtitle = "Shot and Goal Locations",
    x = "X coordinate",
    y = "Y coordinate",
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 14),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )

location_shots

#plot shot location by angle category
location_angle_shots <- ggplot(events_distance %>% filter(event %in% c("Shot", "Goal")),
       aes(x = x1_att, y = y1_att, color = angle_theshold)) +
  geom_point(alpha = 0.6, size = 1) +
  geom_rect(
    xmin = 25, xmax = 100,
    ymin = -42.5, ymax = 42.5,
    fill = NA, color = "black"
  ) +
  geom_segment(
    x = 89, xend = 89,
    y = -3, yend = 3,
    color = "red",
    linewidth = 0.8
  ) +
  coord_fixed() +
  theme_minimal() +
  labs(
    title = "(b)", 
    subtitle = "Shot and Goal Locations \nby Angle Category",
    x = "X coordinate",
    y = "Y coordinate",
    color = NULL
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 14),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )

location_angle_shots

#plot shot location by distance category
shot_distance <- ggplot(events_distance %>% filter(event %in% c("Shot", "Goal")),
       aes(x = x1_att, y = y1_att, color = shots_dist_threshold)) +
  geom_point(alpha = 0.6, size = 1) +
  geom_rect(
    xmin = 25, xmax = 100,
    ymin = -42.5, ymax = 42.5,
    fill = NA, color = "black"
  ) +
  geom_segment(
    x = 89, xend = 89,
    y = -3, yend = 3,
    color = "red",
    linewidth = 0.8
  ) +
  coord_fixed() +
  theme_minimal() +
  labs(
    title = "(c)",
    subtitle = "Shot and Goal Locations \nby Distance Category",
    x = "X coordinate",
    y = "Y coordinate",
    color = NULL
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 14),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )

shot_distance

location_charts <- location_shots + location_angle_shots + shot_distance + 
  plot_annotation(
    title = "Locations of Shots and Goals",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    )
  )
location_charts

#check correlation between angle and distance
cor(events_distance$post_angle, events_distance$distance,
    use = "complete.obs") #cor = -0.68 (strong negative correlation)

#look at number of events in a possession chain
possession_counts <- events_poss_id %>%
  count(possession_id) %>%     
  arrange(possession_id)      

possession_events <- ggplot(possession_counts, aes(x = n)) +
  geom_histogram(binwidth = 1, fill = "#6C88C4", color = "black") +
  theme_minimal() +
  labs(
    title = "Distribution of Events per Possession",
    x = "Number of Events in Possession",
    y = "Count of Possessions"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10)
  )

possession_events

#visualize a possession chain

library(jpeg)
library(grid)

#Filter events for possession 1
possession1 <- events_poss_id %>%
  filter(possession_id == 1)

rink_img <- readJPEG("/Users/alyssalavergne/Documents/hockey_rink.jpg")
rink_grob <- rasterGrob(rink_img, width=unit(1,"npc"), height=unit(1,"npc"))

ggplot(possession1, aes(x = x_coordinate, y = y_coordinate)) +
  # rink background
  annotation_custom(rink_grob, xmin = -100, xmax = 100, ymin = -42.5, ymax = 42.5) +
  # events
  geom_point(aes(color = event), size = 3) +
  geom_path(aes(group = possession_id), color = "black",
            arrow = arrow(type = "closed", length = unit(0.15, "inches"))) +
  # optional: label events
  geom_text(aes(label = event), vjust = -1, size = 3) +
  coord_fixed() +
  scale_x_continuous(limits = c(-100, 100)) +
  scale_y_continuous(limits = c(-42.5, 42.5)) +
  theme_minimal() +
  labs(
    title = "Possession Chain: Possession 1",
    x = "X coordinate",
    y = "Y coordinate",
    color = "Event"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom"
  )

##### WHOLE DATASET SPADE #####
#### CLEAN INTO SEQUENCE DATASET ####
library(arulesSequences) # run the sequence mining algorithm

#quick values cleanup
events_poss_abbr <- events_poss_id %>%
  mutate(event = dplyr::recode(event,
                               "Faceoff Win"     = "FoW",
                               "Puck Recovery"   = "PRec",
                               "Zone Entry"      = "ZEnt",
                               "Incomplete Play" = "IPlay",
                               "Dump In/Out"     = "DIO",
                               "Takeaway"        = "TAw",
                               "Penalty Taken"   = "PenT")) %>%
  mutate(across(c(detail_1, detail_2), ~ str_replace_all(., " ", "_")))

saveRDS(events_poss_abbr, file="events_poss")

#group together all event items and add event size
events_seq <- events_poss_abbr %>% 
  group_by(possession_id) %>% arrange(running_clock_seconds) %>%
  mutate(eventID = row_number()) %>% 
  unite(col = "items", event, x1_zone, x2_zone, angle_threshold,
        pass_dist_threshold, shots_dist_threshold, pass_direction, 
        detail_1, detail_2, new_detail_3, new_detail_4, sep = ",", na.rm = TRUE) %>% 
  ungroup() %>%
  mutate(SIZE = str_count(items, ",") + 1) %>%
  select(possession_id, eventID, SIZE, items) %>%
  mutate(across(everything(), as.factor)) %>%
  rename(sequenceID = possession_id) %>%
  arrange(sequenceID, eventID)

head(events_seq, 10)

#### cSpade Pre-process ####

# Convert to transaction matrix data type
write.table(events_seq, "hockey_seq.txt", sep=",", row.names = FALSE, col.names = FALSE, quote = FALSE)
seq_matrix <- read_baskets("hockey_seq.txt", sep = ",", info = c("sequenceID","eventID","SIZE"))
inspect(head(seq_matrix,10))

#### APPLY CSPADE AND VIEW RESULTS ####

#apply cSPADE algorithm with support = 0.1
itemsets_seq <- cspade(seq_matrix, 
                   parameter = list(support = 0.1), #freq sequs that occurs in at least 10% of all sequs
                   control = list(verbose = TRUE))
inspect(head(itemsets_seq,10))
summary(itemsets_seq)

#### FIND AND INTERPRETE TEMPORAL RULES ####
# Get induced temporal rules from frequent itemsets
r1 <- as(ruleInduction(itemsets_seq, confidence = 0.8, control = list(verbose = TRUE)), "data.frame")
head(r1)

# Separate LHS and RHS rules
r1$rulecount <- as.character(r1$rule)
max_col <- max(sapply(strsplit(r1$rulecount,' => '),length))
r_sep <- separate(data = r1, col = rule, into = paste0("Time",1:max_col), sep = " => ")
r_sep$Time2 <- substring(r_sep$Time2,3,nchar(r_sep$Time2)-2)
head(r_sep)

# Strip LHS baskets
max_time1 <- max(sapply(strsplit(r_sep$Time1,'},'),length))
r_sep$TimeClean <- substring(r_sep$Time1,3,nchar(r_sep$Time1)-2)
r_sep$TimeClean <- gsub("\\},\\{", "zzz", r_sep$TimeClean)
r_sep_items <- separate(data = r_sep, col = TimeClean, into = paste0("Previous_Items",1:max_time1), sep = "zzz")
head(r_sep_items)

# Get cleaned temporal rules: time reads sequentially from left to right

r_shift_na <- r_sep_items

for (i in seq(1, nrow(r_shift_na))){
  for (col in seq(8, (6+max_time1))){
    if (is.na(r_shift_na[i,col])==TRUE){
      r_shift_na[i,col] <- r_shift_na[i,col-1]
      r_shift_na[i,col-1] <- NA  
    }
  }
}
names(r_shift_na)[2] <- "Predicted_Items"

cols <- c(7:(6+max_time1), 2:5)
temporal_rules <- r_shift_na[,cols]
temporal_rules <- temporal_rules[order(-temporal_rules$lift, -temporal_rules$confidence, 
                                       -temporal_rules$support, temporal_rules$Predicted_Items),]

#write.csv(as.data.frame(temporal_rules), file = "TemporalRules.csv", row.names = FALSE, na="")

# Get unique frequent itemsets existing in rules (subset of those in s1.df)
baskets_only <- temporal_rules[,1:(ncol(temporal_rules)-3)]
basket_mat <- as.vector(as.matrix(baskets_only))
freq_itemsets_in_rules <- unique(basket_mat[!is.na(basket_mat)])
#write.csv(as.data.frame(freq_itemsets_in_rules), file = "FreqItemsetsInRules.csv", row.names = FALSE)

#### RESULTS ####
#convert cSPADE results to a tibble
feq_seq <- as(itemsets_seq, "data.frame") %>% as_tibble()

#add a column containing the length of the pattern
feq_seq$pattern.length <- (str_count(feq_seq$sequence, ",") + 1)

#sort by highest to lowest support
feq_seq <- feq_seq[order(-feq_seq$support),] # descending

head(feq_seq,10)

#keep the top 2 patterns with highest support for each pattern length
c <- feq_seq %>% group_by(pattern.length) %>% slice_max(order_by = support, n = 2)
head(c,10)

#add a column containing the number of events in the pattern
feq_seq$seq.event.length <- (str_count(feq_seq$sequence, "\\}") + 1)

#keep the top 2 patterns with highest support for each pattern length
c2 <- feq_seq %>% group_by(seq.event.length) %>% slice_max(order_by = support, n = 2)
head(c2,10)


##### SPLIT BY SCORE STATE #####

#create df with score state at the start of each possession
poss_start_state <- events_poss_id %>%
  arrange(game_id, period, running_clock_seconds) %>%
  group_by(possession_id) %>%
  summarise(
    start_score_state = first(score_state),
    .groups = "drop"
  )

#join back to data
events_poss_state <- events_poss_id %>%
  left_join(poss_start_state, by = "possession_id")

#split by score state
poss_trail  <- events_poss_state %>% filter(start_score_state == "trail")
poss_tied <- events_poss_state %>% filter(start_score_state == "tied")
poss_lead <- events_poss_state %>% filter(start_score_state == "lead")


#group together all event items and add event size
poss_trail_seq <- poss_trail %>% 
  group_by(possession_id) %>% arrange(running_clock_seconds) %>%
  mutate(eventID = row_number()) %>% 
  unite(col = "items", event, x1_zone, x2_zone, angle_threshold,
        pass_dist_threshold, shots_dist_threshold, pass_direction, 
        detail_1, detail_2, new_detail_3, new_detail_4, sep = ",", na.rm = TRUE) %>% 
  ungroup() %>%
  mutate(SIZE = str_count(items, ",") + 1) %>%
  select(possession_id, eventID, SIZE, items) %>%
  mutate(across(everything(), as.factor)) %>%
  rename(sequenceID = possession_id) %>%
  arrange(sequenceID, eventID)

poss_tied_seq <- poss_tied %>% 
  group_by(possession_id) %>% arrange(running_clock_seconds) %>%
  mutate(eventID = row_number()) %>% 
  unite(col = "items", event, x1_zone, x2_zone, angle_threshold,
        pass_dist_threshold, shots_dist_threshold, pass_direction, 
        detail_1, detail_2, new_detail_3, new_detail_4, sep = ",", na.rm = TRUE) %>% 
  ungroup() %>%
  mutate(SIZE = str_count(items, ",") + 1) %>%
  select(possession_id, eventID, SIZE, items) %>%
  mutate(across(everything(), as.factor)) %>%
  rename(sequenceID = possession_id) %>%
  arrange(sequenceID, eventID)

poss_lead_seq <- poss_lead %>% 
  group_by(possession_id) %>% arrange(running_clock_seconds) %>%
  mutate(eventID = row_number()) %>% 
  unite(col = "items", event, x1_zone, x2_zone, angle_threshold,
        pass_dist_threshold, shots_dist_threshold, pass_direction, 
        detail_1, detail_2, new_detail_3, new_detail_4, sep = ",", na.rm = TRUE) %>% 
  ungroup() %>%
  mutate(SIZE = str_count(items, ",") + 1) %>%
  select(possession_id, eventID, SIZE, items) %>%
  mutate(across(everything(), as.factor)) %>%
  rename(sequenceID = possession_id) %>%
  arrange(sequenceID, eventID)

#### cSpade Pre-process ####

# Convert trail to transaction matrix data type
write.table(poss_trail_seq, "hockey_seq_trail.txt", sep=",", row.names = FALSE, col.names = FALSE, quote = FALSE)
seq_matrix_trail <- read_baskets("hockey_seq_trail.txt", sep = ",", info = c("sequenceID","eventID","SIZE"))

# Convert tied to transaction matrix data type
write.table(poss_tied_seq, "hockey_seq_tied.txt", sep=",", row.names = FALSE, col.names = FALSE, quote = FALSE)
seq_matrix_tied <- read_baskets("hockey_seq_tied.txt", sep = ",", info = c("sequenceID","eventID","SIZE"))

# Convert lead to transaction matrix data type
write.table(poss_lead_seq, "hockey_seq_lead.txt", sep=",", row.names = FALSE, col.names = FALSE, quote = FALSE)
seq_matrix_lead <- read_baskets("hockey_seq_lead.txt", sep = ",", info = c("sequenceID","eventID","SIZE"))


#### APPLY CSPADE AND VIEW RESULTS ####

#apply cSPADE algorithm to trail with support = 0.02
itset_seq_trail <- cspade(seq_matrix_trail, 
                       parameter = list(support = 0.04), #freq sequs that occurs in at least 2% of all sequs
                       control = list(verbose = TRUE))
summary(itset_seq_trail)

#apply cSPADE algorithm to tied with support = 0.02
itset_seq_tied <- cspade(seq_matrix_tied, 
                          parameter = list(support = 0.04), #freq sequs that occurs in at least 2% of all sequs
                          control = list(verbose = TRUE))
summary(itset_seq_tied)

#apply cSPADE algorithm to lead with support = 0.02
itset_seq_lead <- cspade(seq_matrix_lead, 
                          parameter = list(support = 0.04), #freq sequs that occurs in at least 2% of all sequs
                          control = list(verbose = TRUE))
summary(itset_seq_lead)


#### FIND AND INTERPRETE TEMPORAL RULES ####

## FOR TRAIL POSSESSIONS

# Get induced temporal rules from frequent itemsets
r1_trail <- as(ruleInduction(itset_seq_trail, confidence = 0.8, control = list(verbose = TRUE)), "data.frame")
head(r1_trail)

# Separate LHS and RHS rules
r1_trail$rulecount <- as.character(r1_trail$rule)
max_col_trail <- max(sapply(strsplit(r1_trail$rulecount,' => '),length))
r_sep_trail <- separate(data = r1_trail, col = rule, into = paste0("Time",1:max_col_trail), sep = " => ")
r_sep_trail$Time2 <- substring(r_sep_trail$Time2,3,nchar(r_sep_trail$Time2)-2)
head(r_sep_trail)

# Strip LHS baskets
max_time1_trail <- max(sapply(strsplit(r_sep_trail$Time1,'},'),length))
r_sep_trail$TimeClean <- substring(r_sep_trail$Time1,3,nchar(r_sep_trail$Time1)-2)
r_sep_trail$TimeClean <- gsub("\\},\\{", "zzz", r_sep_trail$TimeClean)
r_sep_items_trail <- separate(data = r_sep_trail, col = TimeClean, into = paste0("Previous_Items",1:max_time1_trail), sep = "zzz")
head(r_sep_items_trail)

# Get cleaned temporal rules: time reads sequentially from left to right

r_shift_na_trail <- r_sep_items_trail

for (i in seq(1, nrow(r_shift_na_trail))){
  for (col in seq(8, (6+max_time1_trail))){
    if (is.na(r_shift_na_trail[i,col])==TRUE){
      r_shift_na_trail[i,col] <- r_shift_na_trail[i,col-1]
      r_shift_na_trail[i,col-1] <- NA  
    }
  }
}
names(r_shift_na_trail)[2] <- "Predicted_Items"

cols <- c(7:(6+max_time1_trail), 2:5)
temporal_rules_trail <- r_shift_na_trail[,cols]
temporal_rules_trail <- temporal_rules_trail[order(-temporal_rules_trail$lift, -temporal_rules_trail$confidence, 
                                       -temporal_rules_trail$support, temporal_rules_trail$Predicted_Items),]

write.csv(as.data.frame(temporal_rules_trail), file = "TemporalRulesTrail.csv", row.names = FALSE, na="")

# Get unique frequent itemsets existing in rules (subset of those in s1.df)
baskets_only_trail <- temporal_rules_trail[,1:(ncol(temporal_rules_trail)-3)]
basket_mat_trail <- as.vector(as.matrix(baskets_only_trail))
freq_itemsets_in_rules_trail <- unique(basket_mat_trail[!is.na(basket_mat_trail)])
write.csv(as.data.frame(freq_itemsets_in_rules_trail), file = "FreqItemsetsInRulesTrail.csv", row.names = FALSE)



## FOR TIED POSSESSIONS

# Get induced temporal rules from frequent itemsets
r1_tied <- as(ruleInduction(itset_seq_tied, confidence = 0.8, control = list(verbose = TRUE)), "data.frame")
head(r1_tied)

# Separate LHS and RHS rules
r1_tied$rulecount <- as.character(r1_tied$rule)
max_col_tied <- max(sapply(strsplit(r1_tied$rulecount,' => '), length))
r_sep_tied <- separate(data = r1_tied, col = rule, into = paste0("Time", 1:max_col_tied), sep = " => ")
r_sep_tied$Time2 <- substring(r_sep_tied$Time2, 3, nchar(r_sep_tied$Time2) - 2)
head(r_sep_tied)

# Strip LHS baskets
max_time1_tied <- max(sapply(strsplit(r_sep_tied$Time1,'},'), length))
r_sep_tied$TimeClean <- substring(r_sep_tied$Time1, 3, nchar(r_sep_tied$Time1) - 2)
r_sep_tied$TimeClean <- gsub("\\},\\{", "zzz", r_sep_tied$TimeClean)
r_sep_items_tied <- separate(data = r_sep_tied, col = TimeClean,
                             into = paste0("Previous_Items", 1:max_time1_tied),
                             sep = "zzz")
head(r_sep_items_tied)

# Get cleaned temporal rules: time reads sequentially from left to right
r_shift_na_tied <- r_sep_items_tied

for (i in seq(1, nrow(r_shift_na_tied))) {
  for (col in seq(8, (6 + max_time1_tied))) {
    if (is.na(r_shift_na_tied[i, col]) == TRUE) {
      r_shift_na_tied[i, col] <- r_shift_na_tied[i, col - 1]
      r_shift_na_tied[i, col - 1] <- NA  
    }
  }
}

names(r_shift_na_tied)[2] <- "Predicted_Items"

cols <- c(7:(6 + max_time1_tied), 2:5)
temporal_rules_tied <- r_shift_na_tied[, cols]
temporal_rules_tied <- temporal_rules_tied[
  order(-temporal_rules_tied$lift,
        -temporal_rules_tied$confidence,
        -temporal_rules_tied$support,
        temporal_rules_tied$Predicted_Items), ]

write.csv(as.data.frame(temporal_rules_tied),
          file = "TemporalRulesTied.csv",
          row.names = FALSE,
          na = "")

# Get unique frequent itemsets existing in rules (subset of those in s1.df)
baskets_only_tied <- temporal_rules_tied[, 1:(ncol(temporal_rules_tied) - 3)]
basket_mat_tied <- as.vector(as.matrix(baskets_only_tied))
freq_itemsets_in_rules_tied <- unique(basket_mat_tied[!is.na(basket_mat_tied)])

write.csv(as.data.frame(freq_itemsets_in_rules_tied),
          file = "FreqItemsetsInRulesTied.csv",
          row.names = FALSE)



## FOR LEAD POSSESSIONS

# Get induced temporal rules from frequent itemsets
r1_lead <- as(ruleInduction(itset_seq_lead, confidence = 0.8, control = list(verbose = TRUE)), "data.frame")
head(r1_lead)

# Separate LHS and RHS rules
r1_lead$rulecount <- as.character(r1_lead$rule)
max_col_lead <- max(sapply(strsplit(r1_lead$rulecount,' => '), length))
r_sep_lead <- separate(data = r1_lead, col = rule, into = paste0("Time", 1:max_col_lead), sep = " => ")
r_sep_lead$Time2 <- substring(r_sep_lead$Time2, 3, nchar(r_sep_lead$Time2) - 2)
head(r_sep_lead)

# Strip LHS baskets
max_time1_lead <- max(sapply(strsplit(r_sep_lead$Time1,'},'), length))
r_sep_lead$TimeClean <- substring(r_sep_lead$Time1, 3, nchar(r_sep_lead$Time1) - 2)
r_sep_lead$TimeClean <- gsub("\\},\\{", "zzz", r_sep_lead$TimeClean)
r_sep_items_lead <- separate(data = r_sep_lead, col = TimeClean,
                             into = paste0("Previous_Items", 1:max_time1_lead),
                             sep = "zzz")
head(r_sep_items_lead)

# Get cleaned temporal rules: time reads sequentially from left to right
r_shift_na_lead <- r_sep_items_lead

for (i in seq(1, nrow(r_shift_na_lead))) {
  for (col in seq(8, (6 + max_time1_lead))) {
    if (is.na(r_shift_na_lead[i, col]) == TRUE) {
      r_shift_na_lead[i, col] <- r_shift_na_lead[i, col - 1]
      r_shift_na_lead[i, col - 1] <- NA  
    }
  }
}

names(r_shift_na_lead)[2] <- "Predicted_Items"

cols <- c(7:(6 + max_time1_lead), 2:5)
temporal_rules_lead <- r_shift_na_lead[, cols]
temporal_rules_lead <- temporal_rules_lead[
  order(-temporal_rules_lead$lift,
        -temporal_rules_lead$confidence,
        -temporal_rules_lead$support,
        temporal_rules_lead$Predicted_Items), ]

write.csv(as.data.frame(temporal_rules_lead),
          file = "TemporalRulesLead.csv",
          row.names = FALSE,
          na = "")

# Get unique frequent itemsets existing in rules (subset of those in s1.df)
baskets_only_lead <- temporal_rules_lead[, 1:(ncol(temporal_rules_lead) - 3)]
basket_mat_lead <- as.vector(as.matrix(baskets_only_lead))
freq_itemsets_in_rules_lead <- unique(basket_mat_lead[!is.na(basket_mat_lead)])

write.csv(as.data.frame(freq_itemsets_in_rules_lead),
          file = "FreqItemsetsInRulesLead.csv",
          row.names = FALSE)

##### SPLIT BY END OF POSSESSION EVENTS #####

#create df with event at the end of each possession
poss_end_event <- events_poss_id %>%
  arrange(game_id, period, running_clock_seconds) %>%
  group_by(possession_id) %>%
  summarise(
    last_poss_event = last(event),
    .groups = "drop"
  )
 
#join back to data
poss_poss_end_event <- events_poss_id %>%
  left_join(poss_end_event, by = "possession_id")

#split by score state
poss_trail  <- events_poss_state %>% filter(start_score_state == "trail")
poss_tied <- events_poss_state %>% filter(start_score_state == "tied")
poss_lead <- events_poss_state %>% filter(start_score_state == "lead")


#group together all event items and add event size
poss_trail_seq <- poss_trail %>% 
  group_by(possession_id) %>% arrange(running_clock_seconds) %>%
  mutate(eventID = row_number()) %>% 
  unite(col = "items", event, x1_zone, x2_zone, angle_threshold,
        pass_dist_threshold, shots_dist_threshold, pass_direction, 
        detail_1, detail_2, new_detail_3, new_detail_4, sep = ",", na.rm = TRUE) %>% 
  ungroup() %>%
  mutate(SIZE = str_count(items, ",") + 1) %>%
  select(possession_id, eventID, SIZE, items) %>%
  mutate(across(everything(), as.factor)) %>%
  rename(sequenceID = possession_id) %>%
  arrange(sequenceID, eventID)

poss_tied_seq <- poss_tied %>% 
  group_by(possession_id) %>% arrange(running_clock_seconds) %>%
  mutate(eventID = row_number()) %>% 
  unite(col = "items", event, x1_zone, x2_zone, angle_threshold,
        pass_dist_threshold, shots_dist_threshold, pass_direction, 
        detail_1, detail_2, new_detail_3, new_detail_4, sep = ",", na.rm = TRUE) %>% 
  ungroup() %>%
  mutate(SIZE = str_count(items, ",") + 1) %>%
  select(possession_id, eventID, SIZE, items) %>%
  mutate(across(everything(), as.factor)) %>%
  rename(sequenceID = possession_id) %>%
  arrange(sequenceID, eventID)

poss_lead_seq <- poss_lead %>% 
  group_by(possession_id) %>% arrange(running_clock_seconds) %>%
  mutate(eventID = row_number()) %>% 
  unite(col = "items", event, x1_zone, x2_zone, angle_threshold,
        pass_dist_threshold, shots_dist_threshold, pass_direction, 
        detail_1, detail_2, new_detail_3, new_detail_4, sep = ",", na.rm = TRUE) %>% 
  ungroup() %>%
  mutate(SIZE = str_count(items, ",") + 1) %>%
  select(possession_id, eventID, SIZE, items) %>%
  mutate(across(everything(), as.factor)) %>%
  rename(sequenceID = possession_id) %>%
  arrange(sequenceID, eventID)

#### cSpade Pre-process ####

# Convert trail to transaction matrix data type
write.table(poss_trail_seq, "hockey_seq_trail.txt", sep=",", row.names = FALSE, col.names = FALSE, quote = FALSE)
seq_matrix_trail <- read_baskets("hockey_seq_trail.txt", sep = ",", info = c("sequenceID","eventID","SIZE"))

# Convert tied to transaction matrix data type
write.table(poss_tied_seq, "hockey_seq_tied.txt", sep=",", row.names = FALSE, col.names = FALSE, quote = FALSE)
seq_matrix_tied <- read_baskets("hockey_seq_tied.txt", sep = ",", info = c("sequenceID","eventID","SIZE"))

# Convert lead to transaction matrix data type
write.table(poss_lead_seq, "hockey_seq_lead.txt", sep=",", row.names = FALSE, col.names = FALSE, quote = FALSE)
seq_matrix_lead <- read_baskets("hockey_seq_lead.txt", sep = ",", info = c("sequenceID","eventID","SIZE"))


#### APPLY CSPADE AND VIEW RESULTS ####

#apply cSPADE algorithm to trail with support = 0.02
itset_seq_trail <- cspade(seq_matrix_trail, 
                          parameter = list(support = 0.02), #freq sequs that occurs in at least 2% of all sequs
                          control = list(verbose = TRUE))
summary(itset_seq_trail)

#apply cSPADE algorithm to tied with support = 0.02
itset_seq_tied <- cspade(seq_matrix_tied, 
                         parameter = list(support = 0.02), #freq sequs that occurs in at least 2% of all sequs
                         control = list(verbose = TRUE))
summary(itset_seq_tied)

#apply cSPADE algorithm to lead with support = 0.02
itset_seq_lead <- cspade(seq_matrix_lead, 
                         parameter = list(support = 0.02), #freq sequs that occurs in at least 2% of all sequs
                         control = list(verbose = TRUE))
summary(itset_seq_lead)


#### FIND AND INTERPRETE TEMPORAL RULES ####

## FOR TRAIL POSSESSIONS

# Get induced temporal rules from frequent itemsets
r1_trail <- as(ruleInduction(itset_seq_trail, confidence = 0.8, control = list(verbose = TRUE)), "data.frame")
head(r1_trail)

# Separate LHS and RHS rules
r1_trail$rulecount <- as.character(r1_trail$rule)
max_col_trail <- max(sapply(strsplit(r1_trail$rulecount,' => '),length))
r_sep_trail <- separate(data = r1_trail, col = rule, into = paste0("Time",1:max_col_trail), sep = " => ")
r_sep_trail$Time2 <- substring(r_sep_trail$Time2,3,nchar(r_sep_trail$Time2)-2)
head(r_sep_trail)

# Strip LHS baskets
max_time1_trail <- max(sapply(strsplit(r_sep_trail$Time1,'},'),length))
r_sep_trail$TimeClean <- substring(r_sep_trail$Time1,3,nchar(r_sep_trail$Time1)-2)
r_sep_trail$TimeClean <- gsub("\\},\\{", "zzz", r_sep_trail$TimeClean)
r_sep_items_trail <- separate(data = r_sep_trail, col = TimeClean, into = paste0("Previous_Items",1:max_time1_trail), sep = "zzz")
head(r_sep_items_trail)

# Get cleaned temporal rules: time reads sequentially from left to right

r_shift_na_trail <- r_sep_items_trail

for (i in seq(1, nrow(r_shift_na_trail))){
  for (col in seq(8, (6+max_time1_trail))){
    if (is.na(r_shift_na_trail[i,col])==TRUE){
      r_shift_na_trail[i,col] <- r_shift_na_trail[i,col-1]
      r_shift_na_trail[i,col-1] <- NA  
    }
  }
}
names(r_shift_na_trail)[2] <- "Predicted_Items"

cols <- c(7:(6+max_time1_trail), 2:5)
temporal_rules_trail <- r_shift_na_trail[,cols]
temporal_rules_trail <- temporal_rules_trail[order(-temporal_rules_trail$lift, -temporal_rules_trail$confidence, 
                                                   -temporal_rules_trail$support, temporal_rules_trail$Predicted_Items),]

write.csv(as.data.frame(temporal_rules_trail), file = "TemporalRulesTrail.csv", row.names = FALSE, na="")

# Get unique frequent itemsets existing in rules (subset of those in s1.df)
baskets_only_trail <- temporal_rules_trail[,1:(ncol(temporal_rules_trail)-3)]
basket_mat_trail <- as.vector(as.matrix(baskets_only_trail))
freq_itemsets_in_rules_trail <- unique(basket_mat_trail[!is.na(basket_mat_trail)])
write.csv(as.data.frame(freq_itemsets_in_rules_trail), file = "FreqItemsetsInRulesTrail.csv", row.names = FALSE)



## FOR TIED POSSESSIONS

# Get induced temporal rules from frequent itemsets
r1_tied <- as(ruleInduction(itset_seq_tied, confidence = 0.8, control = list(verbose = TRUE)), "data.frame")
head(r1_tied)

# Separate LHS and RHS rules
r1_tied$rulecount <- as.character(r1_tied$rule)
max_col_tied <- max(sapply(strsplit(r1_tied$rulecount,' => '), length))
r_sep_tied <- separate(data = r1_tied, col = rule, into = paste0("Time", 1:max_col_tied), sep = " => ")
r_sep_tied$Time2 <- substring(r_sep_tied$Time2, 3, nchar(r_sep_tied$Time2) - 2)
head(r_sep_tied)

# Strip LHS baskets
max_time1_tied <- max(sapply(strsplit(r_sep_tied$Time1,'},'), length))
r_sep_tied$TimeClean <- substring(r_sep_tied$Time1, 3, nchar(r_sep_tied$Time1) - 2)
r_sep_tied$TimeClean <- gsub("\\},\\{", "zzz", r_sep_tied$TimeClean)
r_sep_items_tied <- separate(data = r_sep_tied, col = TimeClean,
                             into = paste0("Previous_Items", 1:max_time1_tied),
                             sep = "zzz")
head(r_sep_items_tied)

# Get cleaned temporal rules: time reads sequentially from left to right
r_shift_na_tied <- r_sep_items_tied

for (i in seq(1, nrow(r_shift_na_tied))) {
  for (col in seq(8, (6 + max_time1_tied))) {
    if (is.na(r_shift_na_tied[i, col]) == TRUE) {
      r_shift_na_tied[i, col] <- r_shift_na_tied[i, col - 1]
      r_shift_na_tied[i, col - 1] <- NA  
    }
  }
}

names(r_shift_na_tied)[2] <- "Predicted_Items"

cols <- c(7:(6 + max_time1_tied), 2:5)
temporal_rules_tied <- r_shift_na_tied[, cols]
temporal_rules_tied <- temporal_rules_tied[
  order(-temporal_rules_tied$lift,
        -temporal_rules_tied$confidence,
        -temporal_rules_tied$support,
        temporal_rules_tied$Predicted_Items), ]

write.csv(as.data.frame(temporal_rules_tied),
          file = "TemporalRulesTied.csv",
          row.names = FALSE,
          na = "")

# Get unique frequent itemsets existing in rules (subset of those in s1.df)
baskets_only_tied <- temporal_rules_tied[, 1:(ncol(temporal_rules_tied) - 3)]
basket_mat_tied <- as.vector(as.matrix(baskets_only_tied))
freq_itemsets_in_rules_tied <- unique(basket_mat_tied[!is.na(basket_mat_tied)])

write.csv(as.data.frame(freq_itemsets_in_rules_tied),
          file = "FreqItemsetsInRulesTied.csv",
          row.names = FALSE)



## FOR LEAD POSSESSIONS

# Get induced temporal rules from frequent itemsets
r1_lead <- as(ruleInduction(itset_seq_lead, confidence = 0.8, control = list(verbose = TRUE)), "data.frame")
head(r1_lead)

# Separate LHS and RHS rules
r1_lead$rulecount <- as.character(r1_lead$rule)
max_col_lead <- max(sapply(strsplit(r1_lead$rulecount,' => '), length))
r_sep_lead <- separate(data = r1_lead, col = rule, into = paste0("Time", 1:max_col_lead), sep = " => ")
r_sep_lead$Time2 <- substring(r_sep_lead$Time2, 3, nchar(r_sep_lead$Time2) - 2)
head(r_sep_lead)

# Strip LHS baskets
max_time1_lead <- max(sapply(strsplit(r_sep_lead$Time1,'},'), length))
r_sep_lead$TimeClean <- substring(r_sep_lead$Time1, 3, nchar(r_sep_lead$Time1) - 2)
r_sep_lead$TimeClean <- gsub("\\},\\{", "zzz", r_sep_lead$TimeClean)
r_sep_items_lead <- separate(data = r_sep_lead, col = TimeClean,
                             into = paste0("Previous_Items", 1:max_time1_lead),
                             sep = "zzz")
head(r_sep_items_lead)

# Get cleaned temporal rules: time reads sequentially from left to right
r_shift_na_lead <- r_sep_items_lead

for (i in seq(1, nrow(r_shift_na_lead))) {
  for (col in seq(8, (6 + max_time1_lead))) {
    if (is.na(r_shift_na_lead[i, col]) == TRUE) {
      r_shift_na_lead[i, col] <- r_shift_na_lead[i, col - 1]
      r_shift_na_lead[i, col - 1] <- NA  
    }
  }
}

names(r_shift_na_lead)[2] <- "Predicted_Items"

cols <- c(7:(6 + max_time1_lead), 2:5)
temporal_rules_lead <- r_shift_na_lead[, cols]
temporal_rules_lead <- temporal_rules_lead[
  order(-temporal_rules_lead$lift,
        -temporal_rules_lead$confidence,
        -temporal_rules_lead$support,
        temporal_rules_lead$Predicted_Items), ]

write.csv(as.data.frame(temporal_rules_lead),
          file = "TemporalRulesLead.csv",
          row.names = FALSE,
          na = "")

# Get unique frequent itemsets existing in rules (subset of those in s1.df)
baskets_only_lead <- temporal_rules_lead[, 1:(ncol(temporal_rules_lead) - 3)]
basket_mat_lead <- as.vector(as.matrix(baskets_only_lead))
freq_itemsets_in_rules_lead <- unique(basket_mat_lead[!is.na(basket_mat_lead)])

write.csv(as.data.frame(freq_itemsets_in_rules_lead),
          file = "FreqItemsetsInRulesLead.csv",
          row.names = FALSE)

