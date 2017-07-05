# prepare data for regs ====
# libraries
library(dplyr)
library(XML) # need this for xmlToList
library(magrittr) # need this for %<>%
library(ggplot2)

###########################################################################################
##################### DF 1: HIGHEST LEVEL SUMMARY STATS: no XML data ###################### ====
########################################################################################### ====

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

# the following code shows use that the variable 'stage' means 'game' or 'match'
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
############################# DF 2+: ALL VARIABLES, XML data, ############################# ====
################################### and Calculated Fields #################################
###########################################################################################

# Create OG data frame and XML data
# include only desired variables
matchALL <- match %>%
  select(country_id, league_id, season, stage, date, match_api_id,
         home_team_api_id, away_team_api_id, home_team_goal, away_team_goal,
         shoton, shotoff, foulcommit, card, cross, corner, possession)

# create test df
testALL <- matchALL

# remove rows that don't have data or have incomplete data
testALL <- testALL[complete.cases(testALL$possession),] # removes all rows in matchAD1$possession with NA - size: 25979 rows to 14217 rows
testALL <- testALL[!(testALL$possession == "<possession />"),] # remove rows where test$possession contains only "<possession />" - size: 14217 rows to 8419 rows
testALL <- testALL[!(testALL$card == "<card />"),] # remove rows where test$card contains only "<possession />" - # size: from 8419 to 8125 
testALL <- testALL[!(testALL$corner == "<corner />"),] # remove rows where test$corner contains only "<corner />" # size: from 8125 to 8124

# Extract necessary data from nested XML data:
# Time consuming - takes about 12 hours to run

# length of possession for home team (by percentage of game) (< 13 minutes)
for (i in 1:length(testALL$possession)){
  testALL$homePoss[i] <- as.character(as.data.frame(do.call(rbind, xmlToList(as.character(testALL$possession[i])))) %>%
                                        filter(row_number() == n()) %>% 
                                        select(homepos))
}

# homePoss should always be a 2 digit number
# this shows us that 614 of the variables from above were brought in incorrectly
# once code is fixed, this will cease to show the 614 observations:
testALL %>% 
  select(homePoss) %>% 
  filter(nchar(homePoss) > 10) %>% 
  count

# length of possession for away team, by percentage of game (need this to clean homePoss df) (< 12 minutes)
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

# number of shots on goal - home team against away team (~35 minutes to run)
for (i in 1: length(testALL$shoton)){
  testALL$HTshoton[i] <- nrow(as.data.frame(do.call(rbind, xmlToList(as.character(testALL$shoton[i])))) %>% 
                                filter(player1 == testALL$home_team_api_id[i] |
                                         sortorder == testALL$home_team_api_id[i] |
                                         team == testALL$home_team_api_id[i] |
                                         n == testALL$home_team_api_id[i]))
}

# number of shots on goal - away team against home team ( < 36 minutes)
for (i in 1: length(testALL$shoton)){
  testALL$ATshoton[i] <- nrow(as.data.frame(do.call(rbind, xmlToList(as.character(testALL$shoton[i])))) %>% 
                                filter(player1 == testALL$home_team_api_id[i] |
                                         sortorder == testALL$away_team_api_id[i] |
                                         team == testALL$away_team_api_id[i] |
                                         n == testALL$away_team_api_id[i]))
}

# number of shots off goal - home team against away team ( < 44 minutes)
for (i in 1: length(testALL$shotoff)){
  testALL$HTshotoff[i] <- nrow(as.data.frame(do.call(rbind, xmlToList(as.character(testALL$shotoff[i])))) %>% 
                                 filter(player1 == testALL$home_team_api_id[i] |
                                          sortorder == testALL$home_team_api_id[i] |
                                          team == testALL$home_team_api_id[i] |
                                          n == testALL$home_team_api_id[i]))
}

# number of shots off goal - away team against home team ( < 24 minutes)
for (i in 1: length(testALL$shotoff)){
  testALL$ATshotoff[i] <- nrow(as.data.frame(do.call(rbind, xmlToList(as.character(testALL$shotoff[i])))) %>% 
                                 filter(player1 == testALL$home_team_api_id[i] |
                                          sortorder == testALL$away_team_api_id[i] |
                                          team == testALL$away_team_api_id[i] |
                                          n == testALL$away_team_api_id[i]))
}

# number of fouls - home team against away team # ( < 1 hr 30 minutes)
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

# number of fouls - away team against home team # ( < 1 hr 30 minutes)
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

# number of yellow cards for the home team ( < 28 minutes)
for (i in 1: length(testALL$card)){
  testALL$htYcard[i] <- nrow(as.data.frame(do.call(rbind, xmlToList(as.character(testALL$card[i])))) %>% 
                               filter(sortorder == testALL$home_team_api_id[i] |
                                        team == testALL$home_team_api_id[i] |
                                        n == testALL$home_team_api_id[i],
                                      comment == "y"))
}

# number of red cards for the home team ( < 30 minutes)
for (i in 1: length(testALL$card)){
  testALL$htRcard[i] <- nrow(as.data.frame(do.call(rbind, xmlToList(as.character(testALL$card[i])))) %>% 
                               filter(sortorder == testALL$home_team_api_id[i] |
                                        team == testALL$home_team_api_id[i] |
                                        n == testALL$home_team_api_id[i],
                                      comment == "r"))
}

# number of yellow cards for the away team ( < 30 minutes)
for (i in 1: length(testALL$card)){
  testALL$atYcard[i] <- nrow(as.data.frame(do.call(rbind, xmlToList(as.character(testALL$card[i])))) %>% 
                               filter(sortorder == testALL$away_team_api_id[i] |
                                        team == testALL$away_team_api_id[i] |
                                        n == testALL$away_team_api_id[i],
                                      comment == "y"))
}

# number of red cards for the away team ( < 33 minutes)
for (i in 1: length(testALL$card)){
  testALL$atRcard[i] <- nrow(as.data.frame(do.call(rbind, xmlToList(as.character(testALL$card[i])))) %>% 
                               filter(sortorder == testALL$away_team_api_id[i] |
                                        team == testALL$away_team_api_id[i] |
                                        n == testALL$away_team_api_id[i],
                                      comment == "r"))
}

# number of crosses performed by home team
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

# number of crosses performed by away team
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

# number of corners performed by home team (~ 45 minutes)
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

# number of corners performed by away team (~ 45 minutes)
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

# calculated variables
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

# add calculated fields:
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

# 4 plots - goalDiff vs Cards (BOTH home & away games) - DDStats ====

# For English Premier League:

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

# For all leagues with available data

# (first create labeller):
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

# GoalDiff vs Cards, all leagues, '15/16 season
ggplot(subset(DDStats, season %in% c("2015/2016"))) +
  geom_point(aes(sgdiff, spoints), colour="blue", size = 2) +
  labs(title = "Goal Differential & Points, '15/16 Season") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Goal Differential", y = "Total Points") +
  geom_bar(aes(x = sgdiff, y = scards), position = "dodge", stat = "identity", alpha = .5) +
  geom_smooth(aes(sgdiff, spoints), se = FALSE, col = "blue") +
  geom_smooth(aes(sgdiff, scards), se = FALSE) +
  facet_wrap(~league_id, ncol = 3, labeller = as_labeller(team_names))

# GoalDiff vs Cards, all leagues, '14/15 season
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

# create the sorted df
DDStatsSort <- DDStats
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

# (7) yellow cards
s7 <- ggplot(DDStatsSort, aes(rowID, sYcards)) +
  geom_point(alpha = .2) +
  labs(title = "Yello Cards") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "", y = "")

# (8) red cards
s8 <- ggplot(DDStatsSort, aes(rowID, sRcards)) +
  geom_point(alpha = .2) +
  labs(title = "Red Cards") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "", y = "")


source("http://peterhaschke.com/Code/multiplot.R")
multiplot(s1, s2, s3, s4, s5, s6, s7, s8, cols = 4)


# Regression Analysis ====

# check for correlation, potential multicollinearity:
dragon_numeric <- dragon[, sapply(dragon, is.numeric)]
cor(dragon_numeric)

# Linear Regression ====
# using dragon:

# create training set use all data up to season 2014/2015
dtrain <- dragon[!(dragon$season == "2015/2016"),]
# create testing set
dtest <- dragon[(dragon$season == "2015/2016"),]

# dmodels 1 & 2 - dragon data set

# dmodel1 - regress HTgoaldiff against all variables - R2 = .1351, SSE = 6194.164, Test AdjR = 0.1016596
dmodel1 <- lm(HTgoaldiff ~ homePoss + HTshoton + HTshotoff + HTcross + HTcorners +
                ATshoton + ATshotoff + ATcross + ATcorners + 
                HTfouls + htYcard + htRcard + ATfouls + atYcard + atRcard,
              data = dtrain)
summary(dmodel1)

# now used dtest (test data set)
predictTest1 = predict(dmodel1, newdata = dtest)
predictTest1

# Check how close the r2 is:
SSE1 = sum((dtest$HTgoaldiff - predictTest1)^2)
SSE1
sqrt(SSE1)

SST1 = sum((dtest$HTgoaldiff - mean(dragon$HTgoaldiff))^2)
1 - SSE1/SST1

# mean squared errors:
MSE1 = mean((dtest$HTgoaldiff - predictTest1)^2)
sqrt(MSE1)

# dmodel2 - without ATfouls & HTshotoff- R2 = .1353, SSE = 6198.931, Test AdjR = 0.1009683
dmodel2 <- lm(HTgoaldiff ~ homePoss + HTshoton + HTcross + HTcorners +
                ATshoton + ATshotoff + ATcross + ATcorners + 
                HTfouls + htYcard + htRcard + atYcard + atRcard,
              data = dtrain)
summary(dmodel2)

# now used dtest (test data set)
predictTest2 = predict(dmodel2, newdata = dtest)
predictTest2

# Check how close the r2 is:
SSE2 = sum((dtest$HTgoaldiff - predictTest2)^2)
SSE2
sqrt(SSE2)
SST2 = sum((dtest$HTgoaldiff - mean(dragon$HTgoaldiff))^2)
1 - SSE2/SST2

# mean squared errors:
MSE2 = mean((dtest$HTgoaldiff - predictTest2)^2)
sqrt(MSE2)


# dmodel3 - with interaction variable: HTcorners & HTshoton, and ATcorner & ATshoton - R2 = .1363
# SSE = 6194.613, Test AdjR = 0.1015945
dmodel3 <- lm(HTgoaldiff ~ homePoss + HTshoton + HTcross + HTcorners + HTshoton*HTcorners +
                ATshoton + ATshotoff + ATcross + ATcorners + ATshoton*ATcorners +
                HTfouls + htYcard + htRcard + atYcard + atRcard,
              data = dtrain)
summary(dmodel3)

# now used dtest (test data set)
predictTest3 = predict(dmodel3, newdata = dtest)
predictTest3

# Check how close the r2 is:
SSE3 = sum((dtest$HTgoaldiff - predictTest3)^2)
SSE3
sqrt(SSE3)
SST3 = sum((dtest$HTgoaldiff - mean(dragon$HTgoaldiff))^2)
1 - SSE3/SST3

# mean squared errors:
MSE3 = mean((dtest$HTgoaldiff - predictTest3)^2)
sqrt(MSE3)

# dmodel4 - remove HTcorner and ATcorner - R2 = .1303, SSE = 6193.573, Test AdjR = 0.1016003
dmodel4 <- lm(HTgoaldiff ~ homePoss + HTshoton + HTcross +
                ATshoton + ATshotoff + ATcross +
                HTfouls + htYcard + htRcard + atYcard + atRcard,
              data = dtrain)
summary(dmodel4)

# now used dtest (test data set)
predictTest4 = predict(dmodel4, newdata = dtest)
predictTest4

# Check how close the r2 is:
SSE4 = sum((dtest$HTgoaldiff - predictTest3)^2)
SSE4
sqrt(SSE4)
SST4 = sum((dtest$HTgoaldiff - mean(dragon$HTgoaldiff))^2)
1 - SSE4/SST4

# mean squared errors:
MSE4 = mean((dtest$HTgoaldiff - predictTest4)^2)
sqrt(MSE4)

# original adj-R2: .1353. Against the testing set: .10097. Not bad?

# dmodel5 - regress HTgoaldiff against few variables - AdjR2 = .05449, SSE = 6618.55, Test AdjR = 0.04011094
dmodel5 <- lm(HTgoaldiff ~
                HTshoton + ATshoton + 
                HTcard + ATcard,
              data = dtrain)
summary(dmodel5)

# now used dtest (test data set)
predictTest5 = predict(dmodel5, newdata = dtest)
predictTest5

# Check how close the r2 is:
SSE5 = sum((dtest$HTgoaldiff - predictTest5)^2)
SSE5
sqrt(SSE5)

SST5 = sum((dtest$HTgoaldiff - mean(dragon$HTgoaldiff))^2)
1 - SSE5/SST5

# mean squared errors:
MSE5 = mean((dtest$HTgoaldiff - predictTest5)^2)
sqrt(MSE5)

# dmodel6 - regress HTgoaldiff against few variables - AdjR2 = .07194, SSE = 6493.912, Test AdjR = 0.05818721
dmodel6 <- lm(HTgoaldiff ~
                HTshoton + ATshoton + 
                htYcard + htRcard + atYcard + atRcard,
              data = dtrain)
summary(dmodel6)

# now used dtest (test data set)
predictTest6 = predict(dmodel6, newdata = dtest)
predictTest6

# Check how close the r2 is:
SSE6 = sum((dtest$HTgoaldiff - predictTest6)^2)
SSE6
sqrt(SSE6)

SST6 = sum((dtest$HTgoaldiff - mean(dragon$HTgoaldiff))^2)
1 - SSE6/SST6

# mean squared errors:
MSE6 = mean((dtest$HTgoaldiff - predictTest6)^2)
sqrt(MSE6)


# Logistic Regression Models ====

# First Create Baseline Models ====

# Baseline model 1: 
# proportion of wins 6070 wins, 10178 non-wins
table(DoubleDragon3$wins)
# proportion of draws
table(DoubleDragon3$draws)

# proportion of wins: .3735844
6070/(6070+10178)
# proportion of draws: .2528311
4108/(4108+12140)

# what if we break it down by home and away games? (useful for hd logistic regressions)

# Baseline model 2:
# home game wins: 0=4383, 1=3741
DoubleDragon3 %>% 
  filter(home_or_away == "H") %>% 
  select(wins) %>% 
  table()
# proportion of home game wins: .4604874
3741/(3741+4383)

# away game wins. 0=5795, 1=2329
DoubleDragon3 %>% 
  filter(home_or_away == "A") %>% 
  select(wins) %>% 
  table()
# proportion of away game wins: .2866814
2329/(2329+5795)

# home game draws. 0=6070, 1=2054
DoubleDragon3 %>% 
  filter(home_or_away == "H") %>% 
  select(draws) %>% 
  table()
# proportion of home game draws: .2528311
2054/(2054+6070)

# away game draws. 0=6070, 1=2054
DoubleDragon3 %>% 
  filter(home_or_away == "A") %>% 
  select(draws) %>% 
  table()
# proportion of away game draws: .2528311
2054/(2054+6070)

# Logistic Regression - Initial Models (Using doubled data - 'dd' models): ====
# model1 - model8. model1 & model2 are a pair, model3 and model4 are a pair, etc.

# create training set use all data up to season 2014/2015
ddtrain <- DoubleDragon3[!(DoubleDragon3$season == "2015/2016"),]
# create testing set
ddtest <- DoubleDragon3[(DoubleDragon3$season == "2015/2016"),]

# ddmodel1 - regress HTgoaldiff against all variables. AIC: 14810, 
ddmodel1 <- glm(wins ~ shotson + shotsoff + crosses + corners +
                oppShotson + oppShotsoff + oppCrosses + oppCorners + 
                fouls + Ycards + Rcards + oppYcards + oppRcards,
                family = binomial(link = 'logit'), data = ddtrain)
summary(ddmodel1)

ddpredict1 = predict(ddmodel1, type="response")
summary(ddpredict1)
tapply(ddpredict1, ddtrain$wins, mean)

# ddmodel2 - regress HTgoaldiff against all variables. AIC: 13553, 
ddmodel2 <- glm(draws ~ shotson + shotsoff + crosses + corners +
                  oppShotson + oppShotsoff + oppCrosses + oppCorners + 
                  fouls + Ycards + Rcards + oppYcards + oppRcards,
                family = binomial(link = 'logit'), data = ddtrain)
summary(ddmodel2)

ddpredict2 = predict(ddmodel2, type="response")
summary(ddpredict2)
tapply(ddpredict2, ddtrain$wins, mean)

# ddmodel3 - regress HTgoaldiff against all variables. AIC: 15448
ddmodel3 <- glm(wins ~ crosses + oppCrosses + 
                  fouls + Ycards + Rcards + oppYcards + oppRcards,
                family = binomial(link = 'logit'), data = ddtrain)
summary(ddmodel3)

ddpredict3 = predict(ddmodel3, type="response")
summary(ddpredict3)
tapply(ddpredict3, ddtrain$wins, mean)

# ddmodel4 - regress HTgoaldiff against all variables. AIC: 13548
ddmodel4 <- glm(draws ~ crosses + oppCrosses + 
                  fouls + Ycards + Rcards + oppYcards + oppRcards,
                family = binomial(link = 'logit'), data = ddtrain)
summary(ddmodel4)

ddpredict4 = predict(ddmodel4, type="response")
summary(ddpredict4)
tapply(ddpredict4, ddtrain$wins, mean)

# ddmodel5 - regress HTgoaldiff against all variables. AIC: 14683
ddmodel5 <- glm(wins ~ shotson + shotsoff + crosses +
                  oppShotson + oppShotsoff + oppCrosses +
                  Ycards + Rcards + oppYcards + oppRcards + home_or_away,
                family = binomial(link = 'logit'), data = ddtrain)
summary(ddmodel5)

ddpredict5 = predict(ddmodel5, type="response")
summary(ddpredict5)
tapply(ddpredict5, ddtrain$wins, mean)

# ddmodel6 - regress HTgoaldiff against all variables. AIC: 13573, 
ddmodel6 <- glm(draws ~ shotson + shotsoff + crosses +
                  oppShotson + oppShotsoff + oppCrosses +
                  Ycards + Rcards + oppYcards + oppRcards,
                family = binomial(link = 'logit'), data = ddtrain)
summary(ddmodel6)

ddpredict6 = predict(ddmodel6, type="response")
summary(ddpredict6)
tapply(ddpredict6, ddtrain$wins, mean)

# ddmodel7 - regress HTgoaldiff against all variables. AIC: 15011
ddmodel7 <- glm(wins ~ crosses +
                  oppCrosses +
                  Ycards + Rcards + oppYcards + oppRcards +
                  home_or_away,
                family = binomial(link = 'logit'), data = ddtrain)
summary(ddmodel7)

ddpredict7 = predict(ddmodel7, type="response")
summary(ddpredict7)
tapply(ddpredict7, ddtrain$wins, mean)

# ddmodel8 - regress HTgoaldiff against all variables. AIC: 13569
ddmodel8 <- glm(draws ~ crosses +
                  oppCrosses +
                  Ycards + Rcards + oppYcards + oppRcards +
                  home_or_away,
                family = binomial(link = 'logit'), data = ddtrain)
summary(ddmodel8)

ddpredict8 = predict(ddmodel8, type="response")
summary(ddpredict8)
tapply(ddpredict8, ddtrain$wins, mean)


# Logistic Regression - Secondary Models (NOT using doubled data - 'hd' models): ====
# model1 - model8. model1 & model2 are a pair, model3 and model4 are a pair, etc.

# create training set use all data up to season 2014/2015
hdtrain <- dragon[!(dragon$season == "2015/2016"),]
# create testing set
hdtest <- dragon[(dragon$season == "2015/2016"),]

# hdmodel1 - regress HTgoaldiff against all variables. AIC: 
hdmodel1 <- glm(HTwin ~ HTshoton + HTshotoff + HTcross + HTcorners + # hdmodel1 is "Qualitylog"
                  ATshoton + ATshotoff + ATcross + ATcorners + 
                  HTfouls + htYcard + htRcard + ATfouls + atYcard + atRcard,
                data = hdtrain, family = binomial)
summary(hdmodel1)

# in-sample prediction
hdpredict1 = predict(hdmodel1, type="response")
summary(hdpredict1)
tapply(hdpredict1, hdtrain$HTwin, mean)

# out of sample accuracy
hdpredictTest1 = predict(hdmodel1, type = "response", newdata = hdtest)
summary(hdpredictTest1)
tapply(hdpredictTest1, hdtest$HTwin, mean)

# hdmodel2 - regress HTgoaldiff against all variables. AIC: 
hdmodel2 <- glm(HTdraw ~ HTshoton + HTshotoff + HTcross + HTcorners +
                  ATshoton + ATshotoff + ATcross + ATcorners + 
                  HTfouls + htYcard + htRcard + ATfouls + atYcard + atRcard,
                data = hdtrain, family = binomial)
summary(hdmodel2)

# in-sample prediction
hdpredict2 = predict(hdmodel2, type="response")
summary(hdpredict2)
tapply(hdpredict2, hdtrain$HTdraw, mean)

# out of sample accuracy
hdpredictTest2 = predict(hdmodel2, type = "response", newdata = hdtest)
summary(hdpredictTest2)
tapply(hdpredictTest2, hdtest$HTdraw, mean)

# hdmodel3 - regress HTgoaldiff against all variables. AIC:
hdmodel3 <- glm(HTwin ~ HTcross + ATcross +
                  HTfouls + ATfouls +
                  htYcard + htRcard + atYcard + atRcard,
                data = hdtrain, family = binomial)
summary(hdmodel3)

# in-sample prediction
hdpredict3 = predict(hdmodel3, type="response")
summary(hdpredict3)
tapply(hdpredict3, hdtrain$HTwin, mean)

# out of sample accuracy
hdpredictTest3 = predict(hdmodel3, type = "response", newdata = hdtest)
summary(hdpredictTest3)
tapply(hdpredictTest3, hdtest$HTwin, mean)


# hdmodel4 - regress HTgoaldiff against all variables. AIC: 
hdmodel4 <- glm(HTdraw ~ HTcross + ATcross +
                  HTfouls + ATfouls +
                  htYcard + htRcard + atYcard + atRcard,
                data = hdtrain, family = binomial)
summary(hdmodel4)

# in-sample prediction
hdpredict4 = predict(hdmodel4, type="response")
summary(hdpredict4)
tapply(hdpredict4, hdtrain$HTdraw, mean)

# out of sample accuracy
hdpredictTest4 = predict(hdmodel4, type = "response", newdata = hdtest)
summary(hdpredictTest4)
tapply(hdpredictTest4, hdtest$HTdraw, mean)

# hdmodel5 - regress HTgoaldiff against all variables. AIC: 
hdmodel5 <- glm(HTwin ~ HTcross + ATcross +
                  htYcard + htRcard + atYcard + atRcard,
                data = hdtrain, family = binomial)
summary(hdmodel5)

# in-sample prediction
hdpredict5 = predict(hdmodel5, type="response")
summary(hdpredict5)
tapply(hdpredict5, hdtrain$HTwin, mean)

# out of sample accuracy
hdpredictTest5 = predict(hdmodel5, type = "response", newdata = hdtest)
summary(hdpredictTest5)
tapply(hdpredictTest5, hdtest$HTwin, mean)

# hdmodel6 - regress HTgoaldiff against all variables. AIC: 
hdmodel6 <- glm(HTdraw ~ HTcross + ATcross +
                  htYcard + htRcard + atYcard + atRcard,
                data = hdtrain, family = binomial)
summary(hdmodel6)

# in-sample prediction
hdpredict6 = predict(hdmodel6, type="response")
summary(hdpredict6)
tapply(hdpredict6, hdtrain$HTdraw, mean)

# out of sample accuracy
hdpredictTest6 = predict(hdmodel6, type = "response", newdata = hdtest)
summary(hdpredictTest6)
tapply(hdpredictTest6, hdtest$HTdraw, mean)

# hdmodel7 - regress HTgoaldiff against all variables. AIC: 
hdmodel7 <- glm(HTwin ~ htYcard + htRcard + atYcard + atRcard,
                data = hdtrain, family = binomial)
summary(hdmodel7)

# in-sample prediction
hdpredict7 = predict(hdmodel7, type="response")
summary(hdpredict7)
tapply(hdpredict7, hdtrain$HTwin, mean)

# out of sample accuracy
hdpredictTest7 = predict(hdmodel7, type = "response", newdata = hdtest)
summary(hdpredictTest7)
tapply(hdpredictTest7, hdtest$HTwin, mean)

# hdmodel8 - regress HTgoaldiff against all variables. AIC: 
hdmodel8 <- glm(HTdraw ~ htYcard + htRcard + atYcard + atRcard,
                data = hdtrain, family = binomial)
summary(hdmodel8)

# in-sample prediction
hdpredict8 = predict(hdmodel8, type="response")
summary(hdpredict8)
tapply(hdpredict8, hdtrain$HTdraw, mean)

# out of sample accuracy
hdpredictTest8 = predict(hdmodel8, type = "response", newdata = hdtest)
summary(hdpredictTest8)
tapply(hdpredictTest8, hdtest$HTdraw, mean)