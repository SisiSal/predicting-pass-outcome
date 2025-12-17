#### OUR MAIN CODE WILL GO HERE ####

#load required libraries
library(tidyverse)
library(lubridate)

#### Import datasets ####

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
    mutate( #if team is attacking towards left, multiply x coordinate by -1
      x1_attacking = if_else(attacking_direction == "left", -x_coordinate, x_coordinate),
      x2_attacking = if_else(attacking_direction == "left", -x_coordinate_2, x_coordinate_2)
    ) %>%
  mutate(
    x1_zone = case_when(
      x1_attacking < -89 ~ "BND",
      x1_attacking >= -89 & x1_attacking < -25 ~ "DZ",
      x1_attacking >= -25 & x1_attacking <  25 ~ "NZ",
      x1_attacking >=  25 & x1_attacking <= 89 ~ "OZ",
      x1_attacking > 89 ~ "BNO",
      TRUE ~ NA_character_),
    x2_zone = case_when(
      x2_attacking < -89 ~ "BND",
      x2_attacking >= -89 & x2_attacking < -25 ~ "DZ",
      x2_attacking >= -25 & x2_attacking <  25 ~ "NZ",
      x2_attacking >=  25 & x2_attacking <= 89 ~ "OZ",
      x2_attacking > 89 ~ "BNO",
      TRUE ~ NA_character_)
    ) %>%
  select(-team_on_right_p1, -x1_attacking, -x2_attacking) #keeping attacking_direction in dataset since it will be used when calculating the distance for shots and goals

#View(events_zones)

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
  mutate( #if team is attacking towards left, multiply x coordinate by -1
    x_attacking = if_else(attacking_direction == "left", -rink_location_x_feet, rink_location_x_feet)
  ) %>%
  mutate(
    zone = case_when(
      x_attacking < -89 ~ "BND",
      x_attacking >= -89 & x_attacking < -25 ~ "DZ",
      x_attacking >= -25 & x_attacking <  25 ~ "NZ",
      x_attacking >=  25 & x_attacking <= 89 ~ "OZ",
      x_attacking > 89 ~ "BNO",
      TRUE ~ NA_character_)
  ) %>%
 select(-home_team, -away_team, -team_on_right_p1, -attacking_direction, -x_attacking)

#View(agg_tracking_zones)
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
  ungroup()
	
#View(events_distance)

#create threshold variables for distance for shots and goals, and for passes and incomplete passes

events_play <- events_distance %>%
  filter(event %in% c("Play", "Incomplete Play"))

events_shots_goals <- events_distance %>%
  filter(event %in% c("Shot", "Goal"))

#histogram of distances for passes and incomplete passes distance
ggplot(events_play, aes(x = distance)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "black") +
  labs(
    title = "Distribution of Distance for Plays and Incomplete Plays",
    x = "Distance",
    y = "Count"
  ) +
  theme_minimal()

#histogram of distances for shots and goals distance
ggplot(events_shots_goals, aes(x = distance)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "black") +
  labs(
    title = "Distribution of Distance for Shots and Goals",
    x = "Distance",
    y = "Count"
  ) +
  theme_minimal()

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

#View(events_distance)

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

#View(events_distance)


## create direction variable for passes
events_distance <- events_distance %>%
  mutate(
    x1_att = if_else(attacking_direction == "left", -x_coordinate,  x_coordinate),
    y1_att = if_else(attacking_direction == "left", -y_coordinate,  y_coordinate),
    x2_att = if_else(attacking_direction == "left", -x_coordinate_2, x_coordinate_2),
    y2_att = if_else(attacking_direction == "left", -y_coordinate_2, y_coordinate_2),
    
    line1_y =  (x2_att - x1_att) + y1_att,
    line2_y = -(x2_att - x1_att) + y1_att,
    
    pass_direction = case_when(
      !event %in% c("Play", "Incomplete Play") ~ NA_character_, 
      x1_att == x2_att & y1_att == y2_att ~ "nm",
      x1_att == x2_att ~ "lat",
      y2_att < line1_y & y2_att > line2_y ~ "fw",
      y2_att > line1_y & y2_att < line2_y ~ "bw",
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


#### CREATING POSSESSION ID ####

end_poss_events <- c("Shot", "Goal", "Penalty Taken")

#drop unnecessary columns and ensure data is ordered
events_poss_id <- events_distance %>%
  select(-date,-clock) %>%
  arrange(game_id, period, running_clock_seconds) %>%
  mutate(end_prev = lag(event %in% end_poss_events, default = FALSE),
         new_possession = team != lag(team) | period != lag(period) |
           game_id != lag(game_id) | end_prev,
         new_possession = if_else(is.na(new_possession), TRUE, new_possession),
         possession_id = cumsum(new_possession)) %>%
  select(-new_possession, -end_prev)

#check number of distinct poss ids
n_distinct(events_poss_id$possession_id)

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

#group together all event items and add event size
events_seq <- events_poss_abbr %>% 
  group_by(possession_id) %>% arrange(running_clock_seconds) %>%
  mutate(eventID = row_number()) %>% 
  unite(col = "items", event, x1_zone, x2_zone, 
        pass_dist_threshold, shots_dist_threshold, pass_direction, score_state, 
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
head(as(seq_matrix, "data.frame"))

#### APPLY CSPADE AND VIEW RESULTS ####

#apply cSPADE algorithm with support = 0.01
itemsets_seq <- cspade(seq_matrix, 
                   parameter = list(support = 0.01), #freq sequs that occurs in at least 1% of all sequs
                   control = list(verbose = TRUE))
inspect(head(itemsets_seq,10))

itemsets_seq_df <- as(itemsets_seq, "data.frame")
head(itemsets_seq_df)
summary(itemsets_seq)

# Get induced temporal rules from frequent itemsets
r1 <- as(ruleInduction(itemsets_seq, confidence = 0.5, control = list(verbose = TRUE)), "data.frame")

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

