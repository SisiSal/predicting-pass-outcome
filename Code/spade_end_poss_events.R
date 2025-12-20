#### CSPADE FOR END EVENTS ####

#load required libraries
library(tidyverse)
library(arulesSequences)

events_poss <- readRDS(file="events_poss.rds")


##### SPLIT BY END OF POSSESSION EVENTS #####

#create df with event at the end of each possession
poss_end_event <- events_poss %>%
  arrange(game_id, period, running_clock_seconds) %>%
  group_by(possession_id) %>%
  summarise(
    last_poss_event = last(event),
    .groups = "drop"
  )

table(poss_end_event$last_poss_event)

#join back to data
poss_poss_end_event <- events_poss %>%
  left_join(poss_end_event, by = "possession_id")

#split by end of possession events
poss_ip  <- poss_poss_end_event %>% filter(last_poss_event == "IPlay")
poss_sh <- poss_poss_end_event %>% filter(last_poss_event == "Shot")
poss_go <- poss_poss_end_event %>% filter(last_poss_event == "Goal")


#group together all event items and add event size
poss_ip <- poss_ip %>% 
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

poss_sh <- poss_sh %>% 
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

poss_go <- poss_go %>% 
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
write.table(poss_ip, "hockey_ip.txt", sep=",", row.names = FALSE, col.names = FALSE, quote = FALSE)
seq_matrix_ip <- read_baskets("hockey_ip.txt", sep = ",", info = c("sequenceID","eventID","SIZE"))

# Convert tied to transaction matrix data type
write.table(poss_sh, "hockey_sh.txt", sep=",", row.names = FALSE, col.names = FALSE, quote = FALSE)
seq_matrix_sh <- read_baskets("hockey_sh.txt", sep = ",", info = c("sequenceID","eventID","SIZE"))

# Convert lead to transaction matrix data type
write.table(poss_go, "hockey_go.txt", sep=",", row.names = FALSE, col.names = FALSE, quote = FALSE)
seq_matrix_go <- read_baskets("hockey_go.txt", sep = ",", info = c("sequenceID","eventID","SIZE"))


#### APPLY CSPADE AND VIEW RESULTS ####

# apply cSPADE algorithm to IP with support = 0.1
itset_seq_ip <- cspade(seq_matrix_ip, 
                       parameter = list(support = 0.1), # freq seqs that occur in at least 10% of all seqs
                       control = list(verbose = TRUE))
summary(itset_seq_ip)

# apply cSPADE algorithm to tied with support = 0.1
itset_seq_sh <- cspade(seq_matrix_sh, 
                         parameter = list(support = 0.1),
                         control = list(verbose = TRUE))
summary(itset_seq_sh)

# apply cSPADE algorithm to lead with support = 0.3
itset_seq_go <- cspade(seq_matrix_go, 
                         parameter = list(support = 0.3), # freq seqs that occur in at least 30% of all seqs
                         control = list(verbose = TRUE))
summary(itset_seq_go)


#### FIND AND INTERPRETE TEMPORAL RULES ####

## FOR IP POSSESSIONS

# Get induced temporal rules from frequent itemsets
r1_ip <- as(ruleInduction(itset_seq_ip, confidence = 0.8,
                          control = list(verbose = TRUE)), "data.frame")
head(r1_ip)

# Separate LHS and RHS rules
r1_ip$rulecount <- as.character(r1_ip$rule)
max_col_ip <- max(sapply(strsplit(r1_ip$rulecount, ' => '), length))
r_sep_ip <- separate(data = r1_ip, col = rule,
                     into = paste0("Time", 1:max_col_ip),
                     sep = " => ")
r_sep_ip$Time2 <- substring(r_sep_ip$Time2, 3,
                            nchar(r_sep_ip$Time2) - 2)
head(r_sep_ip)

# Strip LHS baskets
max_time1_ip <- max(sapply(strsplit(r_sep_ip$Time1, '},'), length))
r_sep_ip$TimeClean <- substring(r_sep_ip$Time1, 3,
                                nchar(r_sep_ip$Time1) - 2)
r_sep_ip$TimeClean <- gsub("\\},\\{", "zzz", r_sep_ip$TimeClean)
r_sep_items_ip <- separate(data = r_sep_ip, col = TimeClean,
                           into = paste0("Previous_Items", 1:max_time1_ip),
                           sep = "zzz")
head(r_sep_items_ip)

# Get cleaned temporal rules: time reads sequentially from left to right
r_shift_na_ip <- r_sep_items_ip

for (i in seq(1, nrow(r_shift_na_ip))) {
  for (col in seq(8, (6 + max_time1_ip))) {
    if (is.na(r_shift_na_ip[i, col]) == TRUE) {
      r_shift_na_ip[i, col] <- r_shift_na_ip[i, col - 1]
      r_shift_na_ip[i, col - 1] <- NA  
    }
  }
}

names(r_shift_na_ip)[2] <- "Predicted_Items"

cols <- c(7:(6 + max_time1_ip), 2:5)
temporal_rules_ip <- r_shift_na_ip[, cols]
temporal_rules_ip <- temporal_rules_ip[
  order(-temporal_rules_ip$lift,
        -temporal_rules_ip$confidence,
        -temporal_rules_ip$support,
        temporal_rules_ip$Predicted_Items), ]

write.csv(as.data.frame(temporal_rules_ip),
          file = "TemporalRulesIP.csv",
          row.names = FALSE,
          na = "")

# Get unique frequent itemsets existing in rules (subset of those in s1.df)
baskets_only_ip <- temporal_rules_ip[, 1:(ncol(temporal_rules_ip) - 3)]
basket_mat_ip <- as.vector(as.matrix(baskets_only_ip))
freq_itemsets_in_rules_ip <- unique(basket_mat_ip[!is.na(basket_mat_ip)])

write.csv(as.data.frame(freq_itemsets_in_rules_ip),
          file = "FreqItemsetsInRulesIP.csv",
          row.names = FALSE)

#### FIND AND INTERPRETE TEMPORAL RULES ####

## FOR SH POSSESSIONS

# Get induced temporal rules from frequent itemsets
r1_sh <- as(ruleInduction(itset_seq_sh, confidence = 0.8,
                          control = list(verbose = TRUE)), "data.frame")
head(r1_sh)

# Separate LHS and RHS rules
r1_sh$rulecount <- as.character(r1_sh$rule)
max_col_sh <- max(sapply(strsplit(r1_sh$rulecount, ' => '), length))
r_sep_sh <- separate(data = r1_sh, col = rule,
                     into = paste0("Time", 1:max_col_sh),
                     sep = " => ")
r_sep_sh$Time2 <- substring(r_sep_sh$Time2, 3,
                            nchar(r_sep_sh$Time2) - 2)
head(r_sep_sh)

# Strip LHS baskets
max_time1_sh <- max(sapply(strsplit(r_sep_sh$Time1, '},'), length))
r_sep_sh$TimeClean <- substring(r_sep_sh$Time1, 3,
                                nchar(r_sep_sh$Time1) - 2)
r_sep_sh$TimeClean <- gsub("\\},\\{", "zzz", r_sep_sh$TimeClean)
r_sep_items_sh <- separate(data = r_sep_sh, col = TimeClean,
                           into = paste0("Previous_Items", 1:max_time1_sh),
                           sep = "zzz")
head(r_sep_items_sh)

# Get cleaned temporal rules: time reads sequentially from left to right
r_shift_na_sh <- r_sep_items_sh

for (i in seq(1, nrow(r_shift_na_sh))) {
  for (col in seq(8, (6 + max_time1_sh))) {
    if (is.na(r_shift_na_sh[i, col]) == TRUE) {
      r_shift_na_sh[i, col] <- r_shift_na_sh[i, col - 1]
      r_shift_na_sh[i, col - 1] <- NA  
    }
  }
}

names(r_shift_na_sh)[2] <- "Predicted_Items"

cols <- c(7:(6 + max_time1_sh), 2:5)
temporal_rules_sh <- r_shift_na_sh[, cols]
temporal_rules_sh <- temporal_rules_sh[
  order(-temporal_rules_sh$lift,
        -temporal_rules_sh$confidence,
        -temporal_rules_sh$support,
        temporal_rules_sh$Predicted_Items), ]

write.csv(as.data.frame(temporal_rules_sh),
          file = "TemporalRulesSH.csv",
          row.names = FALSE,
          na = "")

# Get unique frequent itemsets existing in rules (subset of those in s1.df)
baskets_only_sh <- temporal_rules_sh[, 1:(ncol(temporal_rules_sh) - 3)]
basket_mat_sh <- as.vector(as.matrix(baskets_only_sh))
freq_itemsets_in_rules_sh <- unique(basket_mat_sh[!is.na(basket_mat_sh)])

write.csv(as.data.frame(freq_itemsets_in_rules_sh),
          file = "FreqItemsetsInRulesSH.csv",
          row.names = FALSE)

## FOR GO POSSESSIONS

# Get induced temporal rules from frequent itemsets
r1_go <- as(ruleInduction(itset_seq_go, confidence = 0.8,
                          control = list(verbose = TRUE)), "data.frame")
head(r1_go)

# Separate LHS and RHS rules
r1_go$rulecount <- as.character(r1_go$rule)
max_col_go <- max(sapply(strsplit(r1_go$rulecount, ' => '), length))
r_sep_go <- separate(data = r1_go, col = rule,
                     into = paste0("Time", 1:max_col_go),
                     sep = " => ")
r_sep_go$Time2 <- substring(r_sep_go$Time2, 3,
                            nchar(r_sep_go$Time2) - 2)
head(r_sep_go)

# Strip LHS baskets
max_time1_go <- max(sapply(strsplit(r_sep_go$Time1, '},'), length))
r_sep_go$TimeClean <- substring(r_sep_go$Time1, 3,
                                nchar(r_sep_go$Time1) - 2)
r_sep_go$TimeClean <- gsub("\\},\\{", "zzz", r_sep_go$TimeClean)
r_sep_items_go <- separate(data = r_sep_go, col = TimeClean,
                           into = paste0("Previous_Items", 1:max_time1_go),
                           sep = "zzz")
head(r_sep_items_go)

# Get cleaned temporal rules: time reads sequentially from left to right
r_shift_na_go <- r_sep_items_go

for (i in seq(1, nrow(r_shift_na_go))) {
  for (col in seq(8, (6 + max_time1_go))) {
    if (is.na(r_shift_na_go[i, col]) == TRUE) {
      r_shift_na_go[i, col] <- r_shift_na_go[i, col - 1]
      r_shift_na_go[i, col - 1] <- NA  
    }
  }
}

names(r_shift_na_go)[2] <- "Predicted_Items"

cols <- c(7:(6 + max_time1_go), 2:5)
temporal_rules_go <- r_shift_na_go[, cols]
temporal_rules_go <- temporal_rules_go[
  order(-temporal_rules_go$lift,
        -temporal_rules_go$confidence,
        -temporal_rules_go$support,
        temporal_rules_go$Predicted_Items), ]

write.csv(as.data.frame(temporal_rules_go),
          file = "TemporalRulesGO.csv",
          row.names = FALSE,
          na = "")

# Get unique frequent itemsets existing in rules (subset of those in s1.df)
baskets_only_go <- temporal_rules_go[, 1:(ncol(temporal_rules_go) - 3)]
basket_mat_go <- as.vector(as.matrix(baskets_only_go))
freq_itemsets_in_rules_go <- unique(basket_mat_go[!is.na(basket_mat_go)])

write.csv(as.data.frame(freq_itemsets_in_rules_go),
          file = "FreqItemsetsInRulesGO.csv",
          row.names = FALSE)
