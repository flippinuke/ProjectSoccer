# prepare data for regs ====
# libraries
library(dplyr)
library(XML) # need this for xmlToList
library(magrittr) # need this for %<>%
library(ggplot2)

###########################################################################################
##################### DF 1: HIGHEST LEVEL SUMMARY STATS: no XML data ###################### ====
###########################################################################################

# reduce match to only necessary variables [no XML, has more observations]
matchMB <- match %>% 
  select(country_id, league_id, season, stage, date, match_api_id,
         home_team_api_id, away_team_api_id, home_team_goal, away_team_goal)

matchMB1 <- matchMB # to create the df that we will add to
matchMB2 <- matchMB # to create the df that we will be adding to matchMB1

# change column names ====
matchMB1 %<>% rename(team = home_team_api_id, goals = home_team_goal, opponent = away_team_api_id,
                     opponent_goals = away_team_goal)

matchMB2 %<>% rename(team = away_team_api_id, goals = away_team_goal, opponent = home_team_api_id,
                     opponent_goals = home_team_goal)

# add column to matchMB1 - home_or_away = H
# add column to matchMB2 - home_or_away = A
matchMB1$home_or_away <- c('H') #
matchMB2$home_or_away <- c('A') # 

# create "doubled" data set:
matchMB3 <- dplyr::bind_rows(matchMB1, matchMB2)

# add calculated fields: ====
matchMB3 %<>%
  mutate(win = ifelse(goals > opponent_goals, 1, 0), loss = ifelse(goals < opponent_goals, 1, 0),
         draw = ifelse(goals == opponent_goals, 1, 0), goal_dif = goals - opponent_goals,
         points = ifelse(goals > opponent_goals, 3,
                         ifelse(goals == opponent_goals, 1, 0)))

# stats df (no XML data) ==== 
statsMB3 <-
  matchMB3 %>%
  group_by(team, season, league_id) %>% # if add "season" here, it affects the ggplot (and we probably want to)
  summarise(sgoals = sum(goals), soppgoals = sum(opponent_goals), swins = sum(win),
            slosses = sum(loss), sdraws = sum(draw), sgdiff = sum(goal_dif),
            spoints = sum(points), maxStage = (ifelse(max(stage) < 31, '30 matches',
                                                      ifelse(max(stage) > 30 & max(stage) < 35, '34 matches',
                                                             ifelse(max(stage) > 34 & max(stage) < 37, '36 matches', '38 matches'))))) # max(stage) instead of max(real_stage) (consider, since we added 'season' to group_by()


# this shows that the dispersion is similar, regardless of how many games a league plays ====
ggplot(data = statsMB3, aes(sgdiff, spoints, col = factor(maxStage))) + geom_point() +
  labs(title = "Points vs Goal Differential") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Goal Differential", y = "Points") +
  labs(colour = "Season Length")

# the following code shows use that the variable 'stage' means 'game' or 'match' ====
# English premier league has 38 games in a season. 19 home and 19 away games per team

# match %>% # match 1
#   filter(stage == 2, league_id == 1729, season == '2015/2016') %>%
#   select(home_team_api_id, away_team_api_id) %>%
#   arrange(home_team_api_id)
# 
# match %>% # match 2
#   filter(stage == 2, league_id == 1729, season == '2015/2016') %>% 
#   select(home_team_api_id, away_team_api_id) %>% 
#   arrange(away_team_api_id)

###########################################################################################
############################# DF 2+: ALL VARIABLES & XML data ############################# ====
###########################################################################################

# Create OG data frame and XML data ====
# include only desired variables
matchALL <- match %>%
  select(country_id, league_id, season, stage, date, match_api_id,
         home_team_api_id, away_team_api_id, home_team_goal, away_team_goal,
         shoton, shotoff, foulcommit, card, cross, corner, possession)

# create test df ====
testALL <- matchALL

# remove rows that don't have data or have incomplete data ====
testALL <- testALL[complete.cases(testALL$possession),] # removes all rows in matchAD1$possession with NA - size: 25979 rows to 14217 rows
testALL <- testALL[!(testALL$possession == "<possession />"),] # remove rows where test$possession contains only "<possession />" - size: 14217 rows to 8419 rows
testALL <- testALL[!(testALL$card == "<card />"),] # remove rows where test$card contains only "<possession />" - # size: from 8419 to 8125 
testALL <- testALL[!(testALL$corner == "<corner />"),] # remove rows where test$corner contains only "<corner />" # size: from 8125 to 8124

# Extract necessary data from nested XML data: ====
# Time consuming - takes about 12 hours to run

# length of possession for home team (by percentage of game) ==== (< 13 minutes)
for (i in 1:length(testALL$possession)){
  testALL$homePoss[i] <- as.character(as.data.frame(do.call(rbind, xmlToList(as.character(testALL$possession[i])))) %>%
                                        filter(row_number() == n()) %>% 
                                        select(homepos))
}

# homePoss should always be a 2 digit number
# this shows us that 614 of the variables from above were brought in incorrectly
testALL$homePoss[which(nchar(testALL$homePoss) > 10)] # once code is fixed, this will case to show the 614 observations

# dplyr version:
testALL %>% 
  select(homePoss) %>% 
  filter(nchar(homePoss) > 10) %>% 
  count

# length of possession for away team, by percentage of game (need this to clean homePoss df) (< 12 minutes) ====
for (i in 1:length(testALL$possession)){
  testALL$awayPoss[i] <- as.character(as.data.frame(do.call(rbind, xmlToList(as.character(testALL$possession[i])))) %>%
                                        filter(row_number() == n()) %>% 
                                        select(awaypos))
}

# replace the wrong homePoss values with the correct values
# dplyr/magrittr version:
testALL %<>%
  mutate(homePoss = ifelse(nchar(homePoss > 10), awayPoss, homePoss))

# remove excess characters:
testALL$homePoss <- as.integer(substr(testALL$homePoss, start = 7, stop = 8))

# create away team possession variable:
testALL$awayPoss <- 100 - testALL$homePoss

# number of shots on goal - home team against away team (~35 minutes to run) ====
for (i in 1: length(testALL$shoton)){
  testALL$HTshoton[i] <- nrow(as.data.frame(do.call(rbind, xmlToList(as.character(testALL$shoton[i])))) %>% 
                                filter(player1 == testALL$home_team_api_id[i] |
                                         sortorder == testALL$home_team_api_id[i] |
                                         team == testALL$home_team_api_id[i] |
                                         n == testALL$home_team_api_id[i]))
}

# number of shots on goal - away team against home team ( < 36 minutes) ====
for (i in 1: length(testALL$shoton)){
  testALL$ATshoton[i] <- nrow(as.data.frame(do.call(rbind, xmlToList(as.character(testALL$shoton[i])))) %>% 
                                filter(player1 == testALL$home_team_api_id[i] |
                                         sortorder == testALL$away_team_api_id[i] |
                                         team == testALL$away_team_api_id[i] |
                                         n == testALL$away_team_api_id[i]))
}

# number of shots off goal - home team against away team ( < 44 minutes) ====
for (i in 1: length(testALL$shotoff)){
  testALL$HTshotoff[i] <- nrow(as.data.frame(do.call(rbind, xmlToList(as.character(testALL$shotoff[i])))) %>% 
                                 filter(player1 == testALL$home_team_api_id[i] |
                                          sortorder == testALL$home_team_api_id[i] |
                                          team == testALL$home_team_api_id[i] |
                                          n == testALL$home_team_api_id[i]))
}

# number of shots off goal - away team against home team ( < 24 minutes) ====
for (i in 1: length(testALL$shotoff)){
  testALL$ATshotoff[i] <- nrow(as.data.frame(do.call(rbind, xmlToList(as.character(testALL$shotoff[i])))) %>% 
                                 filter(player1 == testALL$home_team_api_id[i] |
                                          sortorder == testALL$away_team_api_id[i] |
                                          team == testALL$away_team_api_id[i] |
                                          n == testALL$away_team_api_id[i]))
}

# number of fouls - home team against away team # ( < 1 hr 30 minutes) ====
for (i in 1: length(testALL$foulcommit)){
  temp <- as.data.frame(do.call(rbind, xmlToList(as.character(testALL$foulcommit[i]))))
  if('player1' %in% colnames(temp)) {
    temp %<>%
      filter(player1 == testALL$home_team_api_id[i] |
               sortorder == testALL$home_team_api_id[i] |
               team == testALL$home_team_api_id[i] |
               n == testALL$home_team_api_id[i]) } else {
                 temp %<>%
                   filter(sortorder == testALL$home_team_api_id[i] |
                            team == testALL$home_team_api_id[i] |
                            n == testALL$home_team_api_id[i])}
  testALL$HTfouls[i] <- nrow(temp)}

# number of fouls - away team against home team # ( < 1 hr 30 minutes) ====
for (i in 1: length(testALL$foulcommit)){
  temp <- as.data.frame(do.call(rbind, xmlToList(as.character(testALL$foulcommit[i]))))
  if('player1' %in% colnames(temp)) {
    temp %<>%
      filter(player1 == testALL$away_team_api_id[i] |
               sortorder == testALL$away_team_api_id[i] |
               team == testALL$away_team_api_id[i] |
               n == testALL$away_team_api_id[i]) } else {
                 temp %<>%
                   filter(sortorder == testALL$away_team_api_id[i] |
                            team == testALL$away_team_api_id[i] |
                            n == testALL$away_team_api_id[i])}
  testALL$ATfouls[i] <- nrow(temp)}

# number of yellow cards for the home team ( < 28 minutes) ====
for (i in 1: length(testALL$card)){
  testALL$htYcard[i] <- nrow(as.data.frame(do.call(rbind, xmlToList(as.character(testALL$card[i])))) %>% 
                               filter(sortorder == testALL$home_team_api_id[i] |
                                        team == testALL$home_team_api_id[i] |
                                        n == testALL$home_team_api_id[i],
                                      comment == "y"))
}

# number of red cards for the home team ==== ( < 30 minutes) ====
for (i in 1: length(testALL$card)){
  testALL$htRcard[i] <- nrow(as.data.frame(do.call(rbind, xmlToList(as.character(testALL$card[i])))) %>% 
                               filter(sortorder == testALL$home_team_api_id[i] |
                                        team == testALL$home_team_api_id[i] |
                                        n == testALL$home_team_api_id[i],
                                      comment == "r"))
}

# number of yellow cards for the away team ( < 30 minutes) ====
for (i in 1: length(testALL$card)){
  testALL$atYcard[i] <- nrow(as.data.frame(do.call(rbind, xmlToList(as.character(testALL$card[i])))) %>% 
                               filter(sortorder == testALL$away_team_api_id[i] |
                                        team == testALL$away_team_api_id[i] |
                                        n == testALL$away_team_api_id[i],
                                      comment == "y"))
}

# number of red cards for the away team ( < 33 minutes) ====
for (i in 1: length(testALL$card)){
  testALL$atRcard[i] <- nrow(as.data.frame(do.call(rbind, xmlToList(as.character(testALL$card[i])))) %>% 
                               filter(sortorder == testALL$away_team_api_id[i] |
                                        team == testALL$away_team_api_id[i] |
                                        n == testALL$away_team_api_id[i],
                                      comment == "r"))
}

# number of crosses performed by home team ====
for (i in 1: length(testALL$cross)){
  temp <- as.data.frame(do.call(rbind, xmlToList(as.character(testALL$cross[i]))))
  if('player1' %in% colnames(temp)) {
    temp %<>%
      filter(player1 == testALL$home_team_api_id[i] |
               sortorder == testALL$home_team_api_id[i] |
               team == testALL$home_team_api_id[i] |
               n == testALL$home_team_api_id[i]) } else {
                 temp %<>%
                   filter(sortorder == testALL$home_team_api_id[i] |
                            team == testALL$home_team_api_id[i] |
                            n == testALL$home_team_api_id[i])}
  testALL$HTcross[i] <- nrow(temp)}

# number of crosses performed by away team ==== 
for (i in 1: length(testALL$cross)){
  temp <- as.data.frame(do.call(rbind, xmlToList(as.character(testALL$cross[i]))))
  if('player1' %in% colnames(temp)) {
    temp %<>%
      filter(player1 == testALL$away_team_api_id[i] |
               sortorder == testALL$away_team_api_id[i] |
               team == testALL$away_team_api_id[i] |
               n == testALL$away_team_api_id[i]) } else {
                 temp %<>%
                   filter(sortorder == testALL$away_team_api_id[i] |
                            team == testALL$away_team_api_id[i] |
                            n == testALL$away_team_api_id[i])}
  testALL$ATcross[i] <- nrow(temp)}

# number of corners performed by home team (~ 45 minutes) ====
for (i in 1: length(testALL$corner)){
  temp <- as.data.frame(do.call(rbind, xmlToList(as.character(testALL$corner[i]))))
  if('player1' %in% colnames(temp)) {
    temp %<>%
      filter(player1 == testALL$home_team_api_id[i] |
               sortorder == testALL$home_team_api_id[i] |
               team == testALL$home_team_api_id[i] |
               n == testALL$home_team_api_id[i]) } else {
                 temp %<>%
                   filter(sortorder == testALL$home_team_api_id[i] |
                            team == testALL$home_team_api_id[i] |
                            n == testALL$home_team_api_id[i])}
  testALL$HTcorners[i] <- nrow(temp)}

tail(testALL$HTcorners, n = 1000)

# number of corners performed by away team (~ 45 minutes?) ====
for (i in 1: length(testALL$corner)){
  temp <- as.data.frame(do.call(rbind, xmlToList(as.character(testALL$corner[i]))))
  if('player1' %in% colnames(temp)) {
    temp %<>%
      filter(player1 == testALL$away_team_api_id[i] |
               sortorder == testALL$away_team_api_id[i] |
               team == testALL$away_team_api_id[i] |
               n == testALL$away_team_api_id[i]) } else {
                 temp %<>%
                   filter(sortorder == testALL$away_team_api_id[i] |
                            team == testALL$away_team_api_id[i] |
                            n == testALL$away_team_api_id[i])}
  testALL$ATcorners[i] <- nrow(temp)}


# calculated variables ====
# goal differential
testALL$HTgoaldiff <- testALL$home_team_goal - testALL$away_team_goal # for home team
testALL$ATgoaldiff <- testALL$away_team_goal - testALL$home_team_goal # for away team

# win binary variable
testALL %<>%
  mutate(HTwin = ifelse(home_team_goal > away_team_goal, 1, 0))

# draw binary variable
testALL %<>%
  mutate(HTdraw = ifelse(home_team_goal == away_team_goal, 1, 0))

# create HT points variable
testALL$HTpoints <- ifelse(testALL$HTwin == 1, 3,
                           ifelse(testALL$HTdraw == 1, 1, 0))

# create AT points variable
testALL %<>%
  mutate(ATpoints = ifelse(HTwin == 1, 0,
                           ifelse(HTdraw == 1, 1, 3)))

# cards total variable
testALL$HTcard <- testALL$htRcard + testALL$htYcard
testALL$ATcard <- testALL$atRcard + testALL$atYcard
rm(dragon, DragonStats1, DoubleDragon1, DoubleDragon2, DoubleDragon3)

# dragon - removes the nested XML data that we no longer need ====
dragon <- testALL %>% 
  select(country_id, league_id, season, stage, date, match_api_id,
         home_team_api_id, away_team_api_id, home_team_goal, away_team_goal,
         homePoss, awayPoss,
         HTshoton, HTshotoff, HTfouls, HTcard, htYcard, htRcard, HTcross, HTcorners,
         ATshoton, ATshotoff, ATfouls, ATcard, atYcard, atRcard, ATcross, ATcorners,
         HTgoaldiff, HTwin, HTdraw, HTpoints,
         ATgoaldiff, ATpoints)

# Use dragon to get basic summary information ====

# No. of matches
nrow(dragon)

# Sum variables for home and away teams
dragon %>% 
  select(htYcard, htRcard, home_team_goal, away_team_goal,
         HTshoton, HTshotoff, HTfouls, HTcard, htYcard, htRcard, HTcross, HTcorners,
         ATshoton, ATshotoff, ATfouls, ATcard, atYcard, atRcard, ATcross, ATcorners,
         HTgoaldiff, HTwin, HTdraw, HTpoints,
         ATgoaldiff, ATpoints) %>% 
  summarise_each(funs(sum))
names(dragon)

# mean of possession time for home and away teams
dragon %>%
  summarise(mean(homePoss, na.rm = TRUE),
            mean(awayPoss, na.rm = TRUE))

# summary stats df (Single Dragon) ====
DragonStats1 <-
  dragon %>%
  group_by(home_team_api_id, season, league_id) %>%
  summarise(sHTgoals = sum(home_team_goal), sATgoals = sum(away_team_goal), sHTwins = sum(HTwin),
            sHTdraws = sum(HTdraw), sHTgdiff = sum(HTgoaldiff), sATgdiff = sum(ATgoaldiff),
            mHTposs = mean(homePoss), mATposs = mean(awayPoss),
            
            sHTson = sum(HTshoton), sHTsof = sum(HTshotoff), sHTf = sum(HTfouls),
            sHTcard = sum(HTcard), shtYc = sum(htYcard), shtRc = sum(htRcard),
            sHTcross = sum(HTcross), sHTcorner = sum(HTcorners), sHTpoints = sum(HTpoints),
            
            sATson = sum(ATshoton), sATsof = sum(ATshotoff), sATf = sum(ATfouls),
            sATcard = sum(ATcard), satYc = sum(atYcard), satRc = sum(atRcard),
            sATcross = sum(ATcross), sATcorner = sum(ATcorners), sATpoints = sum(ATpoints),
            maxStage = max(stage))

# Create DoubleDragon stats df ====
DoubleDragon1 <- dragon # this is the 1st df
DoubleDragon2 <- dragon # bind this to the 1st df

# change column names
DoubleDragon1 %<>%
  rename(team = home_team_api_id, opponent = away_team_api_id,
         goals = home_team_goal, oppgoals = away_team_goal, poss = homePoss, oppPoss = awayPoss,
         shotson = HTshoton, shotsoff = HTshotoff, fouls = HTfouls, cards = HTcard,
         Ycards = htYcard, Rcards = htRcard, crosses = HTcross, corners = HTcorners,
         oppShotson = ATshoton, oppShotsoff = ATshotoff, oppFouls = ATfouls, oppCards = ATcard,
         oppYcards = atYcard, oppRcards = atRcard, oppCrosses = ATcross, oppCorners = ATcorners,
         goalDiff = HTgoaldiff, wins = HTwin, draws = HTdraw, points = HTpoints,
         oppGoalDiff = ATgoaldiff, oppPoints = ATpoints)

DoubleDragon2 %<>%
  rename(team = away_team_api_id, opponent = home_team_api_id,
         goals = away_team_goal, oppgoals = home_team_goal, poss = awayPoss, oppPoss = homePoss,
         shotson = ATshoton, shotsoff = ATshotoff, fouls = ATfouls, cards = ATcard,
         Ycards = atYcard, Rcards = atRcard, crosses = ATcross, corners = ATcorners,
         oppShotson = HTshoton, oppShotsoff = HTshotoff, oppFouls = HTfouls, oppCards = HTcard,
         oppYcards = htYcard, oppRcards = htRcard, oppCrosses = HTcross, oppCorners = HTcorners,
         goalDiff = ATgoaldiff, points = ATpoints,
         oppGoalDiff = HTgoaldiff, oppPoints = HTpoints)

# Add home (H) or away (A)identifier
DoubleDragon1$home_or_away <- c('H')
DoubleDragon2$home_or_away <- c('A')

# bind together
DoubleDragon3 <- dplyr::bind_rows(DoubleDragon1, DoubleDragon2) # if doesn't work, run //DoubleDragon3 <- DoubleDragon1// first
DoubleDragon3$HTwin <- NULL
DoubleDragon3$HTdraw <- NULL

# add calculated fields: ====
DoubleDragon3 %<>%
  mutate(wins = ifelse(goals > oppgoals, 1, 0), losses = ifelse(goals < oppgoals, 1, 0),
         draws = ifelse(goals == oppgoals, 1, 0), goalDiff = goals - oppgoals,
         points = ifelse(goals > oppgoals, 3,
                         ifelse(goals == oppgoals, 1, 0)))

# Use DoubleDragon3 to find summary stats grouped by wins, losses, and draws ====

# summary stats by wins
DoubleDragon3 %>% 
  filter(wins == 1) %>% 
  select(Ycards, Rcards, fouls, goals, crosses, corners, shotson, shotsoff) %>%
  summarise_each(funs(sum))

# summary stats by draws
DoubleDragon3 %>% 
  filter(draws == 1) %>%
  select(Ycards, Rcards, fouls, goals, crosses, corners, shotson, shotsoff) %>%
  summarise_each(funs(sum))

# summary stats by losses
DoubleDragon3 %>% 
  filter(losses == 1) %>% 
  select(Ycards, Rcards, fouls, goals, crosses, corners, shotson, shotsoff) %>%
  summarise_each(funs(sum))

#check work:
sum(DoubleDragon3$Rcards)

# number of matches that were won by 1 goal
DoubleDragon3 %>% 
  filter(goalDiff == 1) %>% 
  nrow()

# number of red cards in matches that were won by 1 goal
DoubleDragon3 %>% 
  filter(abs(goalDiff) == 1) %>% 
  select(Rcards) %>% 
  summarise_each(funs(sum))

DoubleDragon3 %>% 
  filter(goalDiff == -1) %>% 
  select(Rcards) %>% 
  summarise_each(funs(sum))

# number of red cards in matches that had a win/loss outcome
DoubleDragon3 %>% 
  filter(abs(goalDiff) > 0) %>% 
  select(Rcards) %>% 
  summarise_each(funs(sum))

# number of red cards in matches where the team lost
DoubleDragon3 %>% 
  filter(goalDiff < 0) %>% 
  select(Rcards) %>% 
  summarise_each(funs(sum))

# stats df ====

# stats for "doubled" data frame
DDStats <-
  DoubleDragon3 %>% 
  group_by(team, season, league_id) %>% # removing season and league_id will affect the ggplots and analyses
  summarise(sgoals = sum(goals), soppgoals = sum(oppgoals), avgposs = mean(poss), avgoppPoss = mean(oppPoss),
            sshotson = sum(shotson), sshotsoff = sum(shotsoff),
            sfouls = sum(fouls), scards = sum(cards), sYcards = sum(Ycards), sRcards = sum(Rcards),
            scrosses = sum(crosses), scorners = sum(corners), spoints = sum(points),
            
            soppShotson = sum(oppShotson), soppShotsoff = sum(oppShotsoff),
            soppFouls = sum(oppFouls), soppCards = sum(oppCards), soppYcards = sum(oppYcards), soppRcards = sum(oppRcards),
            soppCrosses = sum(oppCrosses), soppCorners = sum(oppCorners), soppPoints = sum(oppPoints),
            
            swins = sum(wins), sdraws = sum(draws), slosses = sum(losses), sgdiff = sum(goalDiff),
            maxStage = ifelse(max(stage) < 31, 30,
                              ifelse(max(stage) > 30 & max(stage) < 35, 34,
                                     ifelse(max(stage) > 35 & max(stage) < 37, 36, 38))))

# 4 plots - English Premier League - goalDiff vs Cards (BOTH home & away games) - DDStats ====

# (1) scatterplot only - goal dif vs points, English Premier League:
ggplot(subset(DDStats, season %in% c("2015/2016") & league_id %in% ("1729"))) +
  geom_point(aes(sgdiff, spoints), colour="blue", size = 2) +
  labs(title = "Goal Differential & Points, '15/16 Season") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Goal Differential", y = "Total Points")

# (2) with trendline
ggplot(subset(DDStats, season %in% c("2015/2016") & league_id %in% ("1729"))) +
  geom_point(aes(sgdiff, spoints), colour="blue", size = 2) +
  labs(title = "Goal Differential & Points, '15/16 Season") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Goal Differential", y = "Total Points") +
  geom_smooth(aes(sgdiff, spoints))

# (3) without trendlines, English Premier League:
ggplot(subset(DDStats, season %in% c("2015/2016") & league_id %in% ("1729"))) +
  geom_point(aes(sgdiff, spoints), colour="blue", size = 2) +
  labs(title = "Number of Cards Received") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Team", y = "Total Cards") +
  geom_bar(aes(x = sgdiff, y = scards), position = "dodge", stat = "identity", alpha = .5)

# (4) with trendlines, English Premier League:
ggplot(subset(DDStats, season %in% c("2015/2016") & league_id %in% ("1729"))) +
  geom_point(aes(sgdiff, spoints), colour="blue", size = 2) +
  labs(title = "Number of Cards Received") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Team", y = "Total Cards") +
  geom_bar(aes(x = sgdiff, y = scards), position = "dodge", stat = "identity", alpha = .5) +
  geom_smooth(aes(sgdiff, spoints)) +
  geom_smooth(aes(sgdiff, scards), se = FALSE, col = "dark gray")

# Same plot (4) for all leagues with available data ====
# these plots show:
# 1. English Premier league is much more competitive (teams are closer to each other in skill than other leagues)
# 2. As teams win more games, they get carded less (for home games)

# create labeller:
team_names <- c(
  '1' = "Belgium",
  '1729' = "England",
  '4769' = "France",
  '7809' = "Germany",
  '10257' = "Italy",
  '13274' = "Netherlands",
  '15722' = "Poland",
  '17642' = "Portugal",
  '19694' = "Scotland",
  '21518' = "Spain",
  '24558' = "Switzerland"
)

# facet wrap by league (BOTH away and home games)
# 15/16 season
ggplot(subset(DDStats, season %in% c("2015/2016"))) +
  geom_point(aes(sgdiff, spoints), colour="blue", size = 2) +
  labs(title = "Goal Differential & Points, '15/16 Season") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Goal Differential", y = "Total Points") +
  geom_bar(aes(x = sgdiff, y = scards), position = "dodge", stat = "identity", alpha = .5) +
  geom_smooth(aes(sgdiff, spoints), se = FALSE, col = "blue") +
  geom_smooth(aes(sgdiff, scards), se = FALSE) +
  facet_wrap(~league_id, ncol = 3, labeller = as_labeller(team_names))

# 14/15 season
ggplot(subset(DDStats, season %in% c("2014/2015"))) +
  geom_point(aes(sgdiff, spoints), colour="blue", size = 2) +
  labs(title = "Goal Differential & Points, '14/15 Season") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Goal Differential", y = "Total Points") +
  geom_bar(aes(x = sgdiff, y = scards), position = "dodge", stat = "identity", alpha = .5) +
  geom_smooth(aes(sgdiff, spoints), se = FALSE, col = "blue") +
  geom_smooth(aes(sgdiff, scards), se = FALSE) +
  facet_wrap(~league_id, ncol = 3, labeller = as_labeller(team_names))

# Other Variables: Show frequency of other variables, in order from least to most winning teams ====

# first - order from least to most winning teams
# Teams with fewest points on the left, and teams with the most points on the right.
# these are also scatter plots. Instead of plotting a y and x axis, we ordered
# the data by number of points, and "plotted" the variable against its order of winningest team

# remove strange outlier
DDStatsSort <- DDStatsSort[!(DDStatsSort$team == "208931"),]
# sort
DDStatsSort %<>% arrange(spoints)
DDStatsSort$rowID <- seq.int(nrow(DDStatsSort))

# (1) corners
s1 <- ggplot(DDStatsSort, aes(rowID, scorners)) +
  geom_point(alpha = .2) +
  labs(title = "Corner Kicks") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "", y = "")

# (2) shots on goal
s2 <- ggplot(DDStatsSort, aes(rowID, sshotson)) +
  geom_point(alpha = .2) +
  labs(title = "Shots On") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "", y = "")

# (3) shots off goal
s3 <- ggplot(DDStatsSort, aes(rowID, sshotsoff)) +
  geom_point(alpha = .2) +
  labs(title = "Shots Off") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "", y = "")

# (4) possession
# interesting- teams that won more had less possession
s4 <- ggplot(DDStatsSort, aes(rowID, avgposs)) +
  geom_point(alpha = .2) +
  labs(title = "Possession") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "", y = "")

# (5) fouls
s5 <- ggplot(DDStatsSort, aes(rowID, sfouls)) +
  geom_point(alpha = .2) +
  labs(title = "Fouls") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "", y = "")

# (6) crosses
s6 <- ggplot(DDStatsSort, aes(rowID, scrosses)) +
  geom_point(alpha = .2) +
  labs(title = "Crosses") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "", y = "")

source("http://peterhaschke.com/Code/multiplot.R")
multiplot(s1, s2, s3, s4, s5, s6, cols = 3)


# Regression Analysis ====

# check for multicollinearity:
dragon_numeric <- dragon[, sapply(dragon, is.numeric)]
cor(dragon_numeric)
# none found. the only highly correlated variables are "goals," "points," and "goal differential."
# this makes sense, since they are based on each other. The regression only uses goal differential.

# somewhat correlated variables include "home team corner kick" and "home team shots on goal"
# It does not approach a level of concern, but the correlation makes sense, as corner kicks
# very frequently lead to a shot on goal.

# using dragon ====

# create training set use all data up to season 2014/2015
dtrain <- dragon[!(dragon$season == "2015/2016"),]
# create testing set
dtest <- dragon[(dragon$season == "2015/2016"),]

# dmodels 1 & 2 - dragon data set ====

# model1 - regress HTgoaldiff against all variables - R2 = .1351
dmodel1 <- lm(HTgoaldiff ~ homePoss + HTshoton + HTshotoff + HTcross + HTcorners +
                ATshoton + ATshotoff + ATcross + ATcorners + 
                HTfouls + htYcard + htRcard + ATfouls + atYcard + atRcard,
              data = dtrain)
summary(dmodel1)

# model2 - without ATfouls & HTshotoff- R2 = .1353
dmodel2 <- lm(HTgoaldiff ~ homePoss + HTshoton + HTcross + HTcorners +
                ATshoton + ATshotoff + ATcross + ATcorners + 
                HTfouls + htYcard + htRcard + atYcard + atRcard,
              data = dtrain)
summary(dmodel2)


# now used dtest (test data set)
predictTest = predict(dmodel2, newdata = dtest)
predictTest

# Check how close the r2 is:
SSE = sum((dtest$HTgoaldiff - predictTest)^2)
SST = sum((dtest$HTgoaldiff - mean(dragon$HTgoaldiff))^2)
1 - SSE/SST
# original adj-R2: .1353. Against the testing set: .10097. Not bad?

