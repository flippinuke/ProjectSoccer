# Moneyball approach
library(dplyr) # need this for pipe operator %>% 
library(magrittr) # need this for %<>%
library(ggplot)

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

# change column names ====
# matchMB1 will have $home_team_api_id become 'team' and $away_team_api_id become 'opponent'
# matchMB2 will have $away_team_api_id become 'team' and $home_team_api_id become 'opponent'
# matchMB1: $home_team_goal will become 'goals' and $away_team_goals will become 'opponent_goals'
# matchMB2: $away_team_goals will become 'goals' and $home_team_goals will become 'opponent_goals'
# also, let's drop 'X' and 'id'. We might need a new 'id' variable anyways
library(magrittr)
matchMB1 %<>% rename(team = home_team_api_id, goals = home_team_goal, opponent = away_team_api_id,
                     opponent_goals = away_team_goal)

matchMB2 %<>% rename(team = away_team_api_id, goals = away_team_goal, opponent = home_team_api_id,
                     opponent_goals = home_team_goal)

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

# add calculated fields: ====
library(magrittr)
matchMB3 %<>%
  mutate(win = ifelse(goals > opponent_goals, 1, 0), loss = ifelse(goals < opponent_goals, 1, 0),
         draw = ifelse(goals == opponent_goals, 1, 0), goal_dif = goals - opponent_goals,
         points = ifelse(goals > opponent_goals, 3,
                        ifelse(goals == opponent_goals, 1, 0)))

# this was done separately from above because, when done together, R produced an error message
matchMB3 %<>% 
  mutate(real_stage = ifelse(stage %in% c(1:30), 1,
                             ifelse(stage %in% c(31:34), 2,
                                    ifelse(stage %in% c(35:36), 3,
                                           ifelse(stage %in% c(37:38), 4, NA)))))

# now create stats df, and add one variable "furthest stage (1, 2, 3, or 4)"

# create the passed key variable (likely replaced by max() in dplyr) ====
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
# replace NA's with 1's
passedKEY2to1[is.na(passedKEY2to1)] <- 1
unique(passedKEY2to1$maxStage)
# make it a clean variable name
passedKEY <- passedKEY2to1

# now we can (1) make the stats df and (2) pass the max stage variable into stats

# stats df ====
statsMB3 <-
  matchMB3 %>%
  group_by(team) %>% 
  summarise(sgoals = sum(goals), soppgoals = sum(opponent_goals), swins = sum(win),
            slosses = sum(loss), sdraws = sum(draw), sgdiff = sum(goal_dif),
            spoints = sum(points))

# left join passedKEY into statsMB3
statsMB3 <- dplyr::left_join(statsMB3, passedKEY, by = "team")
unique(statsMB3$maxStage)

# create league_id key ====
leagueKEY <-
  matchMB3 %>% 
  select(team, league_id) %>% 
  distinct()

# left join league_id into statsMB3
statsMB3 <- dplyr::left_join(statsMB3, leagueKEY, by = "team")

# now we can do plot, ggplot, and regressions
# note: we may want go back and add variable 'game' (value for each match is 1)
# and variable homeORaway where home = 1 and away = 0


## we have restructured the data as per mentor recommendation
## Now, do the ggplots, and see if you can do the sort by points plot(season = 2010/2011 etc)

# plotting ====
library(ggplot2)

# need to add identifier column.
statsMB3$statID <- seq.int(nrow(statsMB3))

# (1) view if teams with more points get to stage four. Also - not very good because this compares
# all teams' points, both those that only did stage 1 and those that got to stage 4
# interesting, because many teams with very low point made it to stage 4
ggplot(data = statsMB3, aes(spoints, statID, col = factor(maxStage))) + geom_point() +
  labs(title = "Points and Maximum Stage") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Points", y = "Teams")
# add a sort
# see MB1011.R for this (too many observations in current data set for visually appealing ggplot

# (2) view if teams with higher goal differential get to stage four
# interesting, because many teams with a negative goal differential made it to stage 4
ggplot(data = statsMB3, aes(sgdiff, statID, col = factor(maxStage))) + geom_point() +
  labs(title = "Goal Differential and Maximum Stage") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Goal Differential", y = "Teams")

# filtering by season = 2008/2009, for less clutter. Then sort ====
test <- matchMB3 %>% 
  filter(season == "2008/2009") %>% # can also add to filter: real_stage == "1". but still shows jumbled mess
  group_by(team) %>%
  summarise(sgoals = sum(goals), soppgoals = sum(opponent_goals), swins = sum(win),
            slosses = sum(loss), sdraws = sum(draw), sgdiff = sum(goal_dif),
            spoints = sum(points), maxStage = max(real_stage))
# holy shit, we just added 'maxStage = max(real_stage), and I think it successfully replaced
# all the code in MB1011 (and here) where we created a passedKEY variable

names(test)
unique(test$maxStage)

# add identifier column # skip
# test$statID <- seq.int(nrow(test))

# add maximum stage reached identifier column # no longer needed
# test <- dplyr::left_join(test, passedKEY, by = "team")

# sort by points:
head(test1)

test1 <- test
test1 %<>% arrange(spoints)
test1 %<>% mutate(team = factor(team, levels = test1$team))

# points acquired for each team, colored by maximum stage reached
# still all over the place
ggplot(data = test1, aes(spoints, team, col = factor(maxStage))) + geom_point() +
  labs(title = "Points, 2008/2009") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Points", y = "Teams")
# nice, now just need to fix the y axis labels

# wins for each team, teams colored by maximum stage reached (also ggplot) ====
# this is an interesting chart, because the data is still sorted by points acquired, but now
# the teams (sorted by points) are plotted against wins. This suggests (kind of obviously) that more
# wins equals more points. The strange part is that there are still teams with few wins that make it
# to stage 4, an teams with many wins that stay in stage 1
# perhaps this is because for certain leagues, there is no stage 3 or "champions" league that teams
# progress to?
ggplot(data = test1, aes(swins, team, col = factor(maxStage))) + geom_point() +
  labs(title = "Games won, 2008/2009") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Wins", y = "Teams")


# x/y plots ====

# plot points acquired against goal differential, all stages
ggplot(data = statsMB3, aes(sgdiff, spoints, col = factor(maxStage))) + geom_point() +
  labs(title = "Points vs Goal Differential") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Goal Differential", y = "Points")

# same, but with geom_smooth()
ggplot(data = statsMB3, aes(sgdiff, spoints, col = factor(maxStage))) + geom_point() +
  labs(title = "Points vs Goal Differential") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Goal Differential", y = "Points") + geom_smooth(span = .7, level = 0)

# plot points acquired against goal differential, season 2008/2009 (can also do == real_stage 1)
ggplot(data = test, aes(sgdiff, spoints, col = factor(maxStage))) + geom_point() +
  labs(title = "Pts & Goal Diff, Stage 1") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Goal Differential", y = "Points") + geom_smooth(method = "lm", level = 0)

# regressions ====

# linear: i don't like
winLM <- lm(swins~sgdiff, data = statsMB3)
summary(winLM)

winLM <- lm(spoints~sgdiff, data = statsMB3)
summary(winLM)

winLM <- lm(swins~sgoals, data = statsMB3)
summary(winLM)

# these are kind of obvious

# would not make sense to include sgdiff, sgoals, as independent variables together, as they are too closely related
# makes more sense to run linear regression on matchMB3, where we can include variables such as
# home_or_away, and if possible to add - number of red cards, yellow cards, shots on goal, etc.


# Logistic regression for Whether a team passes from stage 1 to stage 2 ====

stats4log <- matchMB3 %>% 
  filter(real_stage == 1, season == "2008/2009") %>%
  group_by(team) %>%
  summarise(sgoals = sum(goals), soppgoals = sum(opponent_goals), swins = sum(win),
            slosses = sum(loss), sdraws = sum(draw), sgdiff = sum(goal_dif),
            spoints = sum(points))

# add maximum stage reached identifier column
stats4log <- dplyr::left_join(stats4log, passedKEY, by = "team")

# make binary outcome variable == 1 if passed stage 1, 0 if not
stats4log$passStage1 <- ifelse(stats4log$maxStage == 1, 0, 1)

# logistic regression
passLog <- glm(passStage1~sgdiff, data = stats4log, family = "binomial")
summary(passLog)

# while this suggests a correlation between goal differential and points scored (games won), it
# also seems to show that goal differential doesn't seem to matter when it comes to passing to
# stage 2 (not significant).
ggplot(stats4log, aes(sgdiff, spoints, col = factor(passStage1))) + geom_point()
