# Moneyball approach
library(dplyr) # need this for pipe operator %>% 

### Find out who won each season (stage)[[data inspection]] ====
# these are our tables:
# country, league, match, player, playerattributes, teams, teamattributes, sqlitesequence

# note: 'match' data can be grouped into 4 subgroups - (1) base data, (2) player data, (3) XML data, (4) betting house odds
# (1) base data: this is the variables contained in 'matchMB' # this is all we need for moneyball.R
# (2) player data: variables such as 'home_player_X1' - gives position of player and player id at start of match
# (3) XML data: goal, shoton, shotoff, foulcommit, card, cross, corner, possession - details of shots and gameplay
# (4) betting house odds: odds placed on games by various betting houses

# we will be working a lot with 'match', so let's first reduce it to only necessary variables
# dplyr version
matchMB <- match %>% 
  select(country_id, league_id, season, stage, date, match_api_id,
         home_team_api_id, away_team_api_id, home_team_goal, away_team_goal)

# From here, we go as we outline in the notebook. Use moneyball.R for code reference if necessary

matchMB1 <- matchMB # to create the df that we will add to
matchMB2 <- matchMB # to create the df that we will be adding to matchMB1

# change column names in both. matchMB1 will have $home_team_api_id become 'team' and $away_team_api_id become 'opponent'
# matchMB2 will have $away_team_api_id become 'team' and $home_team_api_id become 'opponent'
# matchMB1: $home_team_goal will become 'goals' and $away_team_goals will become 'opponent_goals'
# matchMB2: $away_team_goals will become 'goals' and $home_team_goals will become 'opponent_goals'
# also, let's drop 'X' and 'id'. We might need a new 'id' variable anyways

matchMB1$team <- matchMB1$home_team_api_id
matchMB1$goals <- matchMB1$home_team_goal
matchMB1$opponent <- matchMB1$away_team_api_id
matchMB1$opponent_goals <- matchMB1$away_team_goal

matchMB2$team <- matchMB1$away_team_api_id
matchMB2$goals <- matchMB1$away_team_goal
matchMB2$opponent <- matchMB1$home_team_api_id
matchMB2$opponent_goals <- matchMB1$home_team_goal

# the drop the original variables names to keep it clean
matchMB1 <- matchMB1 %>% 
  select(-c(home_team_api_id, away_team_api_id, home_team_goal, away_team_goal))

matchMB2 <- matchMB2 %>% 
  select(-c(home_team_api_id, away_team_api_id, home_team_goal, away_team_goal))

# add column to matchMB1 - home_or_away = H
# add column to matchMB2 - home_or_away = A
matchMB1$home_or_away <- c('H') # we may need to return and make it 1, 0, or home  = true/false
matchMB2$home_or_away <- c('A') # but we'll start with these two

# now we need to bind mb2 columns to mb1 columns. to keep it clean, let's create mb3 <- mb1, and bind
# to mb3

matchMB3 <- matchMB1

# now bind
rm(matchMB3)
matchMB3 <- dplyr::bind_rows(matchMB1, matchMB2)

# Now we have the start of our data set
# add calculated fields:

# win column
matchMB3$win <- ifelse(matchMB3$goals > matchMB3$opponent_goals, 1, 0)
# dplyr way. don't use for now:
# matchMB3$win <- matchMB3 %>% 
#   mutate(win = ifelse(goals > opponent_goals, 1, 0))

# loss column
matchMB3$loss <- ifelse(matchMB3$goals < matchMB3$opponent_goals, 1, 0)
# draw column
matchMB3$draw <- ifelse(matchMB3$goals == matchMB3$opponent_goals, 1, 0)
# goal differential
matchMB3$goal_dif <- matchMB3$goals - matchMB3$opponent_goals
# points won (win==3, draw==1, loss==0)
matchMB3$points <- ifelse(matchMB3$goals > matchMB3$opponent_goals, 3,
                          ifelse(matchMB3$goals == matchMB3$opponent_goals, 1, 0))

# real stage. 1:30=1, 31:34=2, 35:36=3, 37:38=4
matchMB3 %>% 
  mutate(real_stage = ifelse(stage %in% c(1:30), 1,
                             ifelse(stage %in% c(31:34), 2,
                                    ifelse(stage %in% c(35:36), 3,
                                           ifelse(stage %in% c(37:38), 4, NA)))))



# now create stats df, and add one variable "furthest stage (1, 2, 3, or 4)"

# create stage 4 team df
teamsST4 <- matchMB3 %>% 
  filter(real_stage == 4) %>% 
  select(team) %>% 
  distinct()

# create MaxStage = 4 identifier for all teams in real_stage == 4)
teamsST4 <- teamsST4 %>% 
  mutate(maxStage = 4)

# create stage 3 team df
teamsST3 <- matchMB3 %>% 
  filter(real_stage == 3) %>% 
  select(team) %>% 
  distinct()

# pass identifier = 4 in teamsST4 to df (passedKEY) with teams in real_stage == 3, then change NA's to 3
passedKEY4to3 <- dplyr::left_join(teamsST3, teamsST4, by = "team")
# changes NA's to 3
passedKEY4to3[is.na(passedKEY4to3)] <- 3

# create stage 2 team df
teamsST2 <- matchMB3 %>% 
  filter(real_stage == 2) %>% 
  select(team) %>% 
  distinct()

# create df with teams in max_stage = 4, 3, and 2
# left_join passedKey vars into teamsST
passedKEY3to2 <- dplyr::left_join(teamsST2, passedKEY4to3, by = "team")
# replace NA's with 2's
passedKEY3to2[is.na(passedKEY3to2)] <- 2

# create stage 1 team df
teamsST1 <- matchMB3 %>% 
  filter(real_stage == 1) %>% 
  select(team) %>% 
  distinct() # it worked

# create df with teams in maxStage = 4, 3, 2, 1
passedKEY2to1 <- dplyr::left_join(teamsST1, passedKEY3to2, by = "team")
# replace NA's with 2's
passedKEY2to1[is.na(passedKEY2to1)] <- 1
unique(passedKEY2to1$maxStage)
# make it a clean variable name
passedKEY <- passedKEY2to1

# now we can (1) make the stats df and (2) pass the max stage variable into stats

# max stats df
statsMB3 <-
  matchMB3 %>%
  group_by(team) %>% 
  summarise(sgoals = sum(goals), soppgoals = sum(opponent_goals), swins = sum(win),
            slosses = sum(loss), sdraws = sum(draw), sgdiff = sum(goal_dif),
            spoints = sum(points))

# left join passedKEY into statsMB3
statsMB3 <- dplyr::left_join(statsMB3, passedKEY, by = "team")
unique(statsMB3$maxStage)

# now we can do plot, ggplot, and regressions
# note: we may want go back and add variable 'game' (value for each match is 1)
# and variable homeORaway where home = 1 and away = 0
names(statsMB3)
head(statsMB3)

# plotting
library(ggplot2)

# clearly, scoring more points than your opponents helps win more games
ggplot(data = statsMB3, aes(sgdiff, swins)) + geom_point() +
  labs(title = "Goal Differential vs Wins") + theme(plot.title = element_text(hjust = 0.5))

# color by furthest stage reached
ggplot(data = statsMB3, aes(sgdiff, swins, col = factor(maxStage))) + geom_point() +
  labs(title = "Goal Differential, Wins, Stage Reached") + theme(plot.title = element_text(hjust = 0.5))

ggplot(data = statsMB3, aes(sgdiff, swins, col = factor(maxStage))) + geom_point() +
  labs(title = "Goal Differential vs Wins")

# with LOESS smooth:
ggplot(data = statsMB3, aes(sgdiff, swins, col = maxStage)) +
  geom_point() + scale_colour_discrete() + geom_smooth()

# with OLS linear model:
ggplot(data = statsMB3, aes(x = sgdiff, y = swins, col = factor(maxStage))) +
  geom_point() + scale_colour_discrete() + geom_smooth(method = "lm")

# goals vs wins
ggplot(data = statsMB3, aes(sgoals, swins)) + geom_point()
# goals vs wins, color by stage reached
ggplot(data = statsMB3, aes(sgoals, swins, col = factor(maxStage))) + geom_point()
# points vs wins, color by stage reached. wow, super linear relationship.
ggplot(data = statsMB3, aes(spoints, swins, col = factor(maxStage))) + geom_point()
# why is it more linear that goals or goal differential?

# just goals on x, color by stage, add verticle lines
statsMB3$statID <- seq.int(nrow(statsMB3))
ggplot(data = statsMB3, aes(sgoals, statID, col = factor(maxStage))) + geom_point()

# goal dif on x, color by stage
ggplot(data = statsMB3, aes(sgdiff, statID, col = factor(maxStage))) + geom_point()

# points on x, color by stage
ggplot(data = statsMB3, aes(spoints, statID, col = factor(maxStage))) + geom_point()
