# for this we can begin with match again, and keep only what we want
# but we'll keep some of the XML data, such as goals and fouls, to see if we can introduce
# additional variables helpful for the regressions

# some variables are available or can be easily created
# let's start with those
# then we will go to composite variables (such as player_rating for that date, or XML data that is summed)

# analysis based on easy variables

# create data frames for analysis ====
library(dplyr)
matchAN1 <- match %>%
  select(country_id, league_id, season, stage, date, match_api_id,
         home_team_api_id, away_team_api_id, home_team_goal, away_team_goal) %>% 
  rename(team = home_team_api_id, opponent = away_team_api_id, goals = home_team_goal,
         oppgoals = away_team_goal) %>% 
  mutate(homeORaway = "H")

matchAN2 <- match %>%
  select(country_id, league_id, season, stage, date, match_api_id,
         home_team_api_id, away_team_api_id, home_team_goal, away_team_goal) %>% 
  rename(team = away_team_api_id, opponent = home_team_api_id, goals = away_team_goal,
         oppgoals = home_team_goal) %>% 
  mutate(homeORaway = "A")

matchAN3 <- rbind(matchAN1, matchAN2)

names(matchAN3)

# let's keep wins, goals, or gdiff as the dependent variable instead of points, since points
# is a direct result of whether a team wins, draws, or loses. we don't need to add an additional step

# add goal differential, win, loss, and draw columns to data

matchAN4 <- matchAN3 %>% 
  mutate(gdiff = goals - oppgoals,
         win = ifelse(goals > oppgoals, 1,0), loss = ifelse(goals < oppgoals, 1, 0), draw = ifelse(goals == oppgoals, 1, 0),
         outcome = ifelse(goals > oppgoals, 1,
                          ifelse(goals < oppgoals, -1, 0)))
names(matchAN4)


# win/loss table and chart ====

# out of curiosity, let's create a table showing the gross number of wins, losses, and draws in the data
# here we want to use matchAN1 (not the doubled data - matchAN4). this will give us number (and proportion)
# of wins for home teams. we can then subtract draws and see number (and proportion) of wins for away teams
matchAN1 <- matchAN1 %>%
  mutate(outcome = ifelse(goals > oppgoals, 1, ifelse(goals == oppgoals, 0, -1)))

# out of 25979 games - 6596 draws and 19383 win/loss
table(matchAN1$outcome)

# .2539 of the games are draws
6596/25979

# .7461 of the games are win/loss
1-.2539

# knowing absolutely no other variables, but straight probability, that means that if we guess that
# a team will always win, we will be right .3731 of the time (37.31%)
.7461/2

# by simply adding whether a game is playing home or away, we can increase our probability of guessing
# "win" correctly from .3731 to .4587
11917/25979 # (home team wins)

# this is an increase of  0.0856, or an 8.5% increase in being right
.4587-.3731

### if we always guess home game win, we won't be right .5 of the time

# Knowing whether a team is home or away, these are our probabilities:
# if we always guess home game win, we will be right .4587 of the time
# if we always guess away game los, we will be right .4587 of the time
11917/25979
# if we always guess home game los, we will be right .2874 of the time
# if we always guess away game win, we will be right .2874 of the time
7466/25979
# if we always guess draw, we will be right .2539 of the time
6596/25979
# these are the numbers we'll try to beat with our regressions

# aside: create chart to see number of wins, losses, draws
# create chart same as above, but divided into home/away games

# losses, draw
library(ggplot2)
library(tidyr)

ggplot(matchAN1, aes(outcome)) + geom_bar()

# Regressions ====

# we have 1 data set. we'll want to split it into a training and a testing data set
# we'll need to install a packages to be able to do this
## install.packages("caTools")
## library(caTools)
# pick CRAN mirror close to me

# now to split the data set:
# set.seed(88) #(or any number)
# split <- sample.split(matchAN1$outcome, SplitRatio = 0.75) # (or whichever ratio, perhaps 50/50)
## NOTE: since our outcome variables has 3 outcomes, are we splitting correctly?
## we could also use our 3 binary variables - win/loss/draw if we want binary outcome
# (win or not, loss or not, draw or not)

# so let's continue this using win, since it is binary outcome. can return to categorical 'outcome' later if necessary
# split <- sample.split(matchAN1$win, SplitRatio = 0.75) # (or whichever ratio, perhaps 50/50)
# split
# matchAN1train = subset(matchAN1, split == TRUE)
# matchAN1test = subset(matchAN1, split == FALSE)
# 
# nrow(matchAN1train)
# nrow(matchAN1test)
# 
# matchAN1log <- glm(win~homeORaway + other variables, data == matchAN1train, family = binomial)
# #NOTE: matchAN1 doesn't have homeORaway
#  - will need to switch to matchAN4 for this. otherwise, remove homeORaway)
# 
# summary(matchAN1log)
# 
# AIC is a measure of the quality of our model. it canonly be compared between models on same data set
# preferred model is the one with the lower AIC
# 
# predictTrain <- predict(matchAN1log, type = "response")
# summary(predictTrain)
# 
# tapply(predictTrain, matchAN1train, mean) # this will give us the average prediction for each outcome

# Linear to start
ANLinReg <- lm(outcome~homeORaway, data = matchAN4)
summary(ANLinReg)

# logistic regression: win or not
ANwin <- glm(win~homeORaway, data = matchAN4, family = "binomial")
summary(ANwin) # why do we have negative intercept if outcomes are either 0 or 1?

ANloss <- glm(loss~homeORaway, data = matchAN4, family = "binomial")
summary(ANloss) # i'm doing something wrong, becuase homeORaway should have 2 groups,
# and should give a value for 'H' and value for 'A'


# logistic regression to see effect of home or away on outcome