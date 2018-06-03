#big takeaways found in exploration (interesting), graphical exploration,
#what to do with this knowledge, what are kind of important, appendix

library(tidyverse)
library(stringr)

#remove unnecessary columns, factorize certain columns
shot_dat <- read_csv("data/unprocessed/shot_logs.csv") %>%
  select(-CLOSEST_DEFENDER_PLAYER_ID, -player_id, -GAME_ID) %>%
  mutate(LOCATION = factor(LOCATION, levels = c("A", "H")),
         W = factor(W, levels = c("W", "L")),
         SHOT_RESULT = factor(SHOT_RESULT, levels = c("made", "missed")),
         PTS_TYPE = factor(PTS_TYPE, levels = c(2, 3)),
         PERIOD = factor(PERIOD, levels = c(1, 2, 3, 4, 5, 6, 7)))

#create opposing team variable
shot_dat <- shot_dat %>%
  mutate(opposing_team = factor(str_sub(MATCHUP, start = -3)))

#check for 30 teams
shot_dat %>% distinct(opposing_team)

#na in each column? only NA's in shot clock
# names(shot_dat)
# sum(is.na(shot_dat$MATCHUP))
# sum(is.na(shot_dat$LOCATION))
# sum(is.na(shot_dat$W))
# sum(is.na(shot_dat$FINAL_MARGIN))
# sum(is.na(shot_dat$SHOT_NUMBER))
# sum(is.na(shot_dat$PERIOD))
# sum(is.na(shot_dat$GAME_CLOCK))
# sum(is.na(shot_dat$SHOT_CLOCK))
# sum(is.na(shot_dat$DRIBBLES))
# sum(is.na(shot_dat$TOUCH_TIME))
# sum(is.na(shot_dat$SHOT_DIST))
# sum(is.na(shot_dat$PTS_TYPE))
# sum(is.na(shot_dat$SHOT_RESULT))
# sum(is.na(shot_dat$CLOSEST_DEFENDER))
# sum(is.na(shot_dat$CLOSE_DEF_DIST))
# sum(is.na(shot_dat$FGM))
# sum(is.na(shot_dat$PTS))
# sum(is.na(shot_dat$player_name))
# sum(is.na(shot_dat$opposing_team))

#replace na's with 0
shot_dat <- shot_dat %>% replace_na(replace = list(SHOT_CLOCK = 0))

#change game clock to just seconds
shot_dat <- shot_dat %>%
  mutate(min = as.numeric(str_sub(GAME_CLOCK, start = 1, end = 2)),
         sec = as.numeric(str_sub(GAME_CLOCK, start = 4, end = 5)),
         GAME_CLOCK_sec = 60*min + sec)
shot_dat %>% View()

#set seed
set.seed(1)

#leave out 20% of data
shot_dat_train <- shot_dat %>% sample_frac(.8)
shot_dat_test <- shot_dat %>% setdiff(shot_dat_train)

#shot clock distribution
shot_dat_train %>%
  ggplot(aes(x = SHOT_CLOCK)) +
  geom_histogram(binwidth = 1)
#we can see there is a fairly normal distribution with the exception being a large number of shots taken near the expiration of the shot clock and a large number of shots taken at the start of the shot clock.

#makes
shot_dat_train %>%
  filter(FGM == 1) %>%
  ggplot(aes(x = SHOT_CLOCK)) +
  geom_histogram(binwidth = 1)

#misses
shot_dat_train %>%
  filter(FGM == 0) %>%
  ggplot(aes(x = SHOT_CLOCK)) +
  geom_histogram(binwidth = 1)
#more misses when shot clock is near 0, more makes when shot clock near 24

#dribbles distribution
shot_dat_train %>%
  ggplot(aes(x = DRIBBLES)) +
  geom_histogram(binwidth = 1)

#makes
shot_dat_train %>%
  filter(FGM == 1) %>%
  ggplot(aes(x = DRIBBLES)) +
  geom_histogram(binwidth = 1)

#misses
shot_dat_train %>%
  filter(FGM == 0) %>%
  ggplot(aes(x = DRIBBLES)) +
  geom_histogram(binwidth = 1)
#doesn't seem to have that much of a difference, vast majority of shots come from very few dribbles

#shot distance distribution
shot_dat_train %>%
  ggplot(aes(x = SHOT_DIST)) +
  geom_histogram(binwidth = 1)

#makes
shot_dat_train %>%
  filter(FGM == 1) %>%
  ggplot(aes(x = SHOT_DIST)) +
  geom_histogram(binwidth = 1)

#misses
shot_dat_train %>%
  filter(FGM == 0) %>%
  ggplot(aes(x = SHOT_DIST)) +
  geom_histogram(binwidth = 1)
#bimodal distribution: more misses at long range, more makes a short range, relatively few midrange shots

#shot number and average makes
shot_dat_train %>%
  group_by(SHOT_NUMBER) %>%
  summarize(shot_perc = mean(FGM)) %>%
  ggplot(aes(x = SHOT_NUMBER, y = shot_perc)) +
  geom_bar(stat = "identity")
#not a huge difference but higher shot number does seem to have slightly higher percentages, could suggest that players who shoot a lot of shots may be having a good game

#average makes by quarter
shot_dat_train %>%
  group_by(PERIOD) %>%
  summarize(shot_perc = mean(FGM)) %>%
  ggplot(aes(x = PERIOD, y = shot_perc)) +
  geom_bar(stat = "identity")
#does seem to have a slight downward trend in field goal percentage as time progresses, suggesting fatigue could be a factor

#shot percentage by minute of period
shot_dat_train %>%
  group_by(min) %>%
  summarize(shot_perc = mean(FGM)) %>%
  ggplot(aes(x = min, y = shot_perc)) +
  geom_bar(stat = "identity")
#interesting trend how players shoot the worst towards the begnning and end of a period

#average closest defender distance on makes and misses
shot_dat_train %>%
  group_by(FGM) %>%
  summarize(avg_def_dist = mean(CLOSE_DEF_DIST))

shot_dat_train %>%
  group_by(gr = cut(CLOSE_DEF_DIST, breaks = seq(-1, 57, by = 2))) %>%
  summarize(shot_perc = mean(FGM)) %>%
  ggplot(aes(x = gr, y = shot_perc)) +
  geom_bar(stat = "identity")

#average shot percentage: .452
mean(shot_dat_train$FGM)

#top defenders
shot_dat_train %>%
  group_by(CLOSEST_DEFENDER) %>%
  summarize(count = n(),
            shot_perc = mean(FGM)) %>%
  filter(count >= 150) %>%
  arrange(shot_perc) %>% head()
