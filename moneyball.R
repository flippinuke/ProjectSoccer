# Moneyball approach
library(plyr) # need this for count variable
library(dplyr) # need this for pipe operator %>% 
library(ggplot2) # for graphing

### Find out who won each season (stage) ====
# these are our tables:
# country, league, match, player, playerattributes, teams, teamattributes, sqlitesequence

# note: 'match' data can be grouped into 4 subgroups - (1) base data, (2) player data, (3) XML data, (4) betting house odds
# (1) base data: this is the variables contained in 'matchMB' # this is all we need for moneyball.R
# (2) player data: variables such as 'home_player_X1' - gives position of player and player id at start of match
# (3) XML data: goal, shoton, shotoff, foulcommit, card, cross, corner, possession - details of shots and gameplay
# (4) betting house odds: odds placed on games by various betting houses

# we will be working a lot with 'match', so let's first reduce it to only necessary variables
matchMB <- match[c("X", "id", "country_id", "league_id", "season", "stage", "date", "match_api_id",
                   "home_team_api_id", "away_team_api_id", "home_team_goal", "away_team_goal")]

# next, let's break match down further to only 2008/2009 season data
# note: we've broken by each season and viewed str() for each season to count num observations
# to ensure that we haven't missed anything

## ensuring nothing missed:
# variable for each season # good candidate for a function
matchMB0809 <- dplyr::filter(matchMB, season == "2008/2009")

unique(matchMB0809$stage) # 38 stages total
# tells us how many times each stage appears in data
# also shows us that there are really 4 main stages

# next, add totals of above to see if it equals # observations, and which teams are in each

# stages 1-30 each appear 94 times # 94 matches played / stage
# stages 31-34 each appear 78 times # 78 matchesplayed / stage
# stages 35-36 each appear 51 times # 51 matches played / stage
# stages 37-38 each appear 46 times # 46 matches played / stage
# no data beyond this, likely teams are selected to move forward to Eurocup of whichever championships

# Note: Explanation of stage structure
# Stages 37-38 each appear 46 times. At this point, there are ~90 teams total left
# The same ~90 teams appear in stage 37 and in stage 38:
# - All of the home teams in stage 37 become the away teams in stage 38 (mostly)
# - All of the away teams in stage 38 become the home teams in stage 37
# - However, they don't play the same teams twice (e.g. team 10281 plays one team in 37, and different team in 38)
# My assumption is that each of the previous stages is structured similarly

# viewing the teams (remove or #)
# function

view_teams <- function(data, stg){
  object <- data %>% filter(stage == stg)
  
  return(list(
    sort(unique(object$home_team_api_id)), # to see teams
    sort(object$home_team_api_id), # to see teams (not unique)
    sort(unique(object$away_team_api_id)) # to see teams
  ))
}

view_teams(matchMB0809, 1)
view_teams(matchMB0809, 2)
view_teams(matchMB0809, 3)
view_teams(matchMB0809, 4)
view_teams(matchMB0809, 5)
view_teams(matchMB0809, 6)
view_teams(matchMB0809, 7)
view_teams(matchMB0809, 8)
view_teams(matchMB0809, 9)
view_teams(matchMB0809, 10)
view_teams(matchMB0809, 11)
view_teams(matchMB0809, 12)
view_teams(matchMB0809, 13)
view_teams(matchMB0809, 14)
view_teams(matchMB0809, 15)
view_teams(matchMB0809, 16)
view_teams(matchMB0809, 17)
view_teams(matchMB0809, 18)
view_teams(matchMB0809, 19)
view_teams(matchMB0809, 20)
view_teams(matchMB0809, 21)
view_teams(matchMB0809, 22)
view_teams(matchMB0809, 23)
view_teams(matchMB0809, 24)
view_teams(matchMB0809, 25)
view_teams(matchMB0809, 26)
view_teams(matchMB0809, 27)
view_teams(matchMB0809, 28)
view_teams(matchMB0809, 29)
view_teams(matchMB0809, 30)

# Stages 31-34 (156 teams playing)(remove) ====
# Here, each team has 2 home games and two away games (and sometimes 3 of one and 1 of the other)

view_teams(matchMB0809, 31) # keep
view_teams(matchMB0809, 32) # keep
view_teams(matchMB0809, 33) # keep
view_teams(matchMB0809, 34) # keep

matchMB0809s3134 <- dplyr::filter(matchMB0809, stage >= 31 & stage <= 34) # includes stages 31-34
sort(unique(matchMB0809s3134$home_team_api_id)) # as you can see ...
sort(unique(matchMB0809s3134$away_team_api_id)) # ... the teams are the same

# sort without unique()

sort(matchMB0809s3134$home_team_api_id) # as you can see, the teams are the same ...
sort(matchMB0809s3134$away_team_api_id) # except 9906 & 8597 (play at home twice) and 8596 & 8633 (play away twice)

# stages 35-36 (100 teams playing) (remove) ====

view_teams(matchMB0809, 35) # to see teams (home_team_api_id vs away_team_api_id)
view_teams(matchMB0809, 36) # different than previous, but match sort from stage == 35

matchMB0809s3536 <- dplyr::filter(matchMB0809, stage >= 35 & stage <= 36) # includes stages 37 and 38
sort(unique(matchMB0809s3536$home_team_api_id)) # as you can see ...
sort(unique(matchMB0809s3536$away_team_api_id)) # ... the teams are the same
# sort without unique()
sort(matchMB0809s3536$home_team_api_id) # as you can see, the teams are the same ...
sort(matchMB0809s3536$away_team_api_id) # except 9906 & 8597 (play at home twice) and 8596 & 8633 (play away twice)
# otherwise, there are no repeats

# stages 37-38 (91 teams playing) (remove) ====
# these lines based on view_teams function
view_teams(matchMB0809, 37) # keep
view_teams(matchMB0809, 38) # keep

### (Method to calculate team total goals - as home team and as away team)
# begin with stage (I): 1-30 ====
# [[this is in moneyballX.R]] It might be worth returning to

# this data set contains data for 11 soccer leagues
# in matchMB, all teams are present. However, matchMB doesn't have a column for league identifier. is this important?
# maybe need to add one
# head(teams)
# head(matchMB)
# unique(matchMB$home_team_api_id)

# need to add column "real_stage" where 1 = 1:30, 2 = 31:34, 3 = 35:36, 4 = 37:38
matchMB$real_stage <- ifelse(matchMB$stage %in% c(1:30), 1,
                             ifelse(matchMB$stage %in% c(31:34), 2,
                                    ifelse(matchMB$stage %in% c(35:36), 3,
                                           ifelse(matchMB$stage %in% c(37:38), 4, NA))))
                                    

# now add home team and away team goal differential
matchMB$htgd <- matchMB$home_team_goal - matchMB$away_team_goal
matchMB$atgd <- matchMB$away_team_goal - matchMB$home_team_goal

# dplyr way:
matchMB %>%
  mutate(htgd = home_team_goal - away_team_goal)

matchMB %>% 
  mutate(atgd = away_team_goal - home_team_goal)


# now add home team win and away team win (so we can later compare it to goal differential)

matchMB$htw <- ifelse(matchMB$home_team_goal > matchMB$away_team_goal, 1, 0)
matchMB$atw <- ifelse(matchMB$away_team_goal > matchMB$home_team_goal, 1, 0)
#dplyer way
matchMB %>% 
  mutate(htw = ifelse(home_team_goal > away_team_goal, 1, 0))
matchMB %>% 
  mutate(atw = ifelse(away_team_goal > home_team_goal, 1, 0))


# now add binary variable == 1 if team progressed to stage 2 and == 0 if not # having difficulty

unique(matchMB0809$home_team_api_id)
unique(matchMB0809[,"home_team_api_id", "real_stage"==1])
count(unique(matchMB0809[,'home_team_api_id', "real_stage"==1]))

unique(matchMB0809[,"home_team_api_id", "real_stage"==2])
count(unique(matchMB0809[,"home_team_api_id", "real_stage"==2]))

# maybe this instead
# matchMB$passed <- ifelse(matchMB0809[,"home_team_api_id", "real_stage==1" <<also exists in>> "real_stage"==2, 1,0)
# return to later

# what I want is a df with stage end stats - for each team, their number of goals, opponent goals, goals dif, wins, ties, etc.
# start here:
stats0809 <-
  matchMB0809 %>%
  group_by(home_team_api_id) %>% 
  filter(real_stage == 1) %>% 
  summarise(shtg = sum(home_team_goal), satg = sum(away_team_goal), shtw =sum(htw),
            satw = sum(atw), shtgd = sum(htgd), satgd = sum(atgd))

plot(stats0809$shtgd, stats0809$shtw)
# ggplot version
library(ggplot2)
# these are two interesting plots showing home games vs away games (goal differential against wins)
#keep
ggplot(data = stats0809, aes(shtgd, shtw)) + geom_point() # find way to color dots by pregression to next stage
ggplot(data = stats0809, aes(satgd, satw)) + geom_point() # find way to color dots by pregression to next stage

##### (aside) #####
# (aside) I did it again for all seasons and stages, kind of interesting:
statsALL <-
  matchMB %>%
  group_by(home_team_api_id) %>% 
  summarise(sum(home_team_goal), sum(away_team_goal), sum(htw), sum(atw), sum(htgd), sum(atgd))

# some ggpot

ggplot(data = statsALL, aes(`sum(htgd)`, `sum(htw)`)) + geom_point()
ggplot(data = statsALL, aes(`sum(atgd)`, `sum(atw)`)) + geom_point()

##### (end aside) #####

# next: find that moneyball graph that you liked:
# includes: create variable == 1 if team progressed to next real_stage

# and now I can move on to linear models and logistic models

# We want to make a chart of all teams in stage one. chart by goal differential
# color by if they made it to next stage
# x-axis is goal differential
# y axis is team (each team represented by a dot)
# color of dot represents if they made it to next stage

# Regressions here:

winLM <- lm(shtw~shtgd, data = stats0809)
summary(winLM)

# next: create regression that tells which factors contribute to goals scored
# goalLM <- lm(shtg~(shots on, shots off, red cards, yellow cards...etc...)

# next: find swirl lesson for dplyr and pipe operator
# Find out which leagues our teams are in
# Also add column for number of points won by each team in a match --> home_team_points, away_team_points
# then once added, go through the code on this page again and change as needed

########## keep for reference, likely will use                                  
matchMB %>% 
  group_by(season, stage, home_team_api_id) %>% 
  summarise(goals = sum(home_team_goal), matches = n())

mutate(gpm = goals/matches)
##########
matchMB0809s01 %>%
  group_by(home_team_api_id) %>% 
  summarise(sum(home_team_goal))

# and if you want to give a title to the colum called `sum(home_team_goal)`<int>, then:

matchMB %>%
  group_by(season, stage, home_team_api_id) %>% 
  summarise(goals = sum(home_team_goal))
##########