# Moneyball approach
library(plyr) # need this for count variable
library(dplyr) # need this for pipe operator %>% 
library(ggplot2) # for graphing

### Find out who won each season (stage)[[data inspection]] ====
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

# Stages 31-34 (156 teams playing)(remove)[[data inspection]] ====
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

# stages 35-36 (100 teams playing) (remove)[[data inspection]]====

view_teams(matchMB0809, 35) # to see teams (home_team_api_id vs away_team_api_id)
view_teams(matchMB0809, 36) # different than previous, but match sort from stage == 35

matchMB0809s3536 <- dplyr::filter(matchMB0809, stage >= 35 & stage <= 36) # includes stages 37 and 38
sort(unique(matchMB0809s3536$home_team_api_id)) # as you can see ...
sort(unique(matchMB0809s3536$away_team_api_id)) # ... the teams are the same
# sort without unique()
sort(matchMB0809s3536$home_team_api_id) # as you can see, the teams are the same ...
sort(matchMB0809s3536$away_team_api_id) # except 9906 & 8597 (play at home twice) and 8596 & 8633 (play away twice)
# otherwise, there are no repeats

# stages 37-38 (91 teams playing) (remove)[[data inspection]] ====
# these lines based on view_teams function
view_teams(matchMB0809, 37) # keep
view_teams(matchMB0809, 38) # keep

### (Method to calculate team total goals - as home team and as away team)
# begin with stage (I): 1-30 ====
# [[moveda a code chunk here to moneyballX.R]] It might be worth returning to

# this data set contains data for 11 soccer leagues ====
# in matchMB, all teams are present. However, matchMB doesn't have a column for league identifier. is this important?
# maybe need to add one
# head(teams)
# head(matchMB)
# unique(matchMB$home_team_api_id)

# Add variables (calculated fields) ====

# add column "real_stage" where 1 = 1:30, 2 = 31:34, 3 = 35:36, 4 = 37:38
matchMB$real_stage <- ifelse(matchMB$stage %in% c(1:30), 1,
                             ifelse(matchMB$stage %in% c(31:34), 2,
                                    ifelse(matchMB$stage %in% c(35:36), 3,
                                           ifelse(matchMB$stage %in% c(37:38), 4, NA))))
                                    
# dplyr version
matchMB %>% 
  mutate(real_stage = ifelse(stage %in% c(1:30), 1,
                              ifelse(stage %in% c(31:34), 2,
                                     ifelse(stage %in% c(35:36), 3,
                                            ifelse(stage %in% c(37:38), 4, NA)))))

# now add home team and away team goal differential
matchMB$htgd <- matchMB$home_team_goal - matchMB$away_team_goal
matchMB$atgd <- matchMB$away_team_goal - matchMB$home_team_goal

# dplyr way:
matchMB %>%
  mutate(htgd = home_team_goal - away_team_goal)

matchMB %>% 
  mutate(atgd = away_team_goal - home_team_goal)

# now add home team win and away team win (so we can later compare it to goal differential)
# I think this is the right way to do it.
matchMB$htw <- ifelse(matchMB$home_team_goal > matchMB$away_team_goal, 1, 0)
matchMB$atw <- ifelse(matchMB$away_team_goal > matchMB$home_team_goal, 1, 0)
# In the end we want to calculate number of wins, losses, and goal dif. If we do it like this:
# Alternate: 1 for win, 0 for draw, -1 for loss, helps calculate goal dif. But then will need
# to 'ifelse' in order to calculate number of wins, draws, losses

#dplyer way
matchMB %>% 
  mutate(htw = ifelse(home_team_goal > away_team_goal, 1, 0))
matchMB %>% 
  mutate(atw = ifelse(away_team_goal > home_team_goal, 1, 0))

# Now calculate field for points won per game
# this requires us to
# (1) identify which league each team belongs to
# (2) determine how many points each league awards for a win/draw/loss
# (2a) (because points determine stage progression)
# (3) create the calculated field

# (1) Identify which team belongs to which league

# (i) this allows us to determine that 'match' references team by api_id, NOT by fifa_api_id
sort(unique(match$home_team_api_id))
sort(unique(teams$team_api_id))

head(teams$team_long_name)
unique(match$league_id)

# (ii) change column name of teams from 'team_api_id' to 'home_team_api_id' (need for later join)
colnames(teams)[which(names(teams) == "team_api_id")] <- "home_team_api_id"
names(teams)

# (iii) create mini df's of data we want to research on google
teamsub <- teams %>% 
  select(home_team_api_id, team_long_name) # Polonia Bytom listed twice: 8031 & 8020

matchsub <- match %>% 
  select(league_id, home_team_api_id)

# (iv) create df with league id's assigned to teams # this is giving me trouble - 
# not creating what it originally made (it's repeating teams based on number of matches - 
# but we only need unique teams)
leagueDF <- dplyr::left_join(teamsub, matchsub, by = "home_team_api_id")
unique(leagueDF) # this better
# (v) write csv file, search leagues
write.csv((leagueDF %>% 
             select(team_long_name, league_id) %>%
             group_by(team_long_name, league_id) %>% 
             summarise()), file = "side/searchLeagues.csv")
#interesting. we are missing 3 teams it seems (or other data set has 3 misspelled teams)

# (vi) at this point I used Excel to sort the league ID's, identified which league ID's corresponded
# to which league by searching the team name in Google. 
# I then verified that all leagues in data follow standard point scoring rules:
# win - 3 points. draw - 1 point. loss - 0 points.
# there is some additional rules for ties that can differ somewhat from league to leage. in general:
# 1. points scored 2. goal differential 3. goals scored.  4. maybe something else
# We will return to the additional progression rules later if necessary
# No need to add league names to data

# (vii) change the column ID back to its previous name:
colnames(teams)[which(names(teams) == "home_team_api_id")] <- "team_api_id"

# now we can add a points earned variable to the data

matchMB$homePts <- ifelse(matchMB$home_team_goal > matchMB$away_team_goal, 3,
                          ifelse(matchMB$home_team_goal == matchMB$away_team_goal, 1, 0))

matchMB$awayPts <- ifelse(matchMB$away_team_goal > matchMB$home_team_goal, 3,
                          ifelse(matchMB$away_team_goal == matchMB$home_team_goal, 1, 0))

# dplyr way
matchMB %>% 
  mutate(homePts = ifelse(home_team_goal > away_team_goal, 3,
                          ifelse(home_team_goal == away_team_goal, 1, 0)))

matchMB %>% 
  mutate(awayPts = ifelse(away_team_goal > home_team_goal, 3,
                          ifelse(away_team_goal == home_team_goal, 1, 0)))

# now add binary variable == 1 if team progressed to stage 2 and == 0 if not # having difficulty

# create stage 1 team df

teamsST1 <- matchMB0809 %>% 
  group_by(home_team_api_id) %>% 
  filter(real_stage == 1) %>%
  select(home_team_api_id) %>%
  summarise()

# create stage 2 team df

teamsST2 <- matchMB0809 %>% 
  group_by(home_team_api_id) %>% 
  filter(real_stage == 2) %>%
  summarise()

# create passed key data set

# (1) create "identifier" = 1 for all teams in stage 2
teamsST2 <- teamsST2 %>% 
  mutate(pass = 1)

# (2) left join of teams in stage 1 (teamsST1) with teams in stage 2 (teamsST2)
# this will bring over all "1's" from previous code where teams exist, and create 'NA' where DNE
passedKEY <- dplyr::left_join(teamsST1, teamsST2, by = "home_team_api_id")
# replace all NA's with 0 - thus 1 = passed and 0 = did not pass
passedKEY[is.na(passedKEY)] <- 0

# Now bring over these 1's and 0's from passedKEy to matchMB0809 (and eventually matchMB)
# do this again with a left join

matchMB0809 <- dplyr::left_join(matchMB0809, passedKEY, by = "home_team_api_id")

# check that there are teams with both 1's and 0's:
unique(matchMB0809[matchMB0809$real_stage == 2, "home_team_api_id"])
unique(matchMB0809$pass)

matchMB0809 %>% 
  group_by(home_team_api_id) %>% 
  select(pass) %>% 
  summarise()

# (this gets my to view teams the way I want, but I don't know why it includes an 'n' column)
matchMB0809 %>% 
  select(home_team_api_id, pass) %>% 
  count(home_team_api_id, pass)

# I couldn't figure out how to sum the # of teams that passed all at once, so I did it like this:
fine <- matchMB0809 %>% 
  select(home_team_api_id, pass) %>% 
  count(home_team_api_id, pass)

sum(fine$pass) # 156 teams passed. checks out

# next, view teams that passed and didn't in matchMB0809, and color scatterplot by it

# these can be used to create identifiers for stage 3 and stage 4:

matchMB0809 %>% 
  group_by(home_team_api_id) %>% 
  filter(real_stage == 3) %>%
  summarise(matches = n())

matchMB0809 %>% 
  group_by(home_team_api_id) %>% 
  filter(real_stage == 4) %>%
  summarise(matches = n())

# end #

# Create df that has end stage/end season stats for each team - their number of goals,
# opponent goals, goals dif, wins, ties, etc.

# start here:
# 2008/2009 season, later do for all seasons
stats0809 <-
  matchMB0809 %>%
  group_by(home_team_api_id) %>% 
  filter(real_stage == 1) %>% 
  summarise(shtg = sum(home_team_goal), satg = sum(away_team_goal), shtw = sum(htw),
            satw = sum(atw), shtgd = sum(htgd), satgd = sum(atgd), spass = sum(pass))

# I was unable to just stick "pass" into 'stats0809 <- ...', so I summed it. now ...
# Fix spass: 
stats0809$spass <- ifelse(stats0809$spass > 0, 1, 0)

# Plotting ====
plot(stats0809$shtgd, stats0809$shtw)
# ggplot version
library(ggplot2)
# these are two interesting plots showing home games vs away games (goal differential against wins)
#keep

# NEXT: find way to color plots based on passed to next level

# ggplot stuff
ggplot(data = stats0809, aes(shtgd, shtw)) + geom_point() # find way to color dots by pregression to next stage
ggplot(data = stats0809, aes(satgd, satw)) + geom_point() # find way to color dots by pregression to next stage

### (aside) ###
# (aside) I did it again for all seasons and stages, kind of interesting:
statsALL <-
  matchMB %>%
  group_by(home_team_api_id) %>% 
  summarise(sum(home_team_goal), sum(away_team_goal), sum(htw), sum(atw), sum(htgd), sum(atgd))

# some ggpot

ggplot(data = statsALL, aes(`sum(htgd)`, `sum(htw)`)) + geom_point()
ggplot(data = statsALL, aes(`sum(atgd)`, `sum(atw)`)) + geom_point()

### (end aside) ###

# next: find that moneyball graph that you liked:
# includes: create variable == 1 if team progressed to next real_stage

# and now I can move on to linear models and logistic models

# We want to make a chart of all teams in stage one. chart by goal differential
# color by if they made it to next stage
# x-axis is goal differential
# y axis is team (each team represented by a dot)
# color of dot represents if they made it to next stage


# next: create regression that tells which factors contribute to goals scored ====
# goalLM <- lm(shtg~(shots on, shots off, red cards, yellow cards...etc...)

# Regressions:

winLM <- lm(shtw~shtgd, data = stats0809)
summary(winLM)

# next: find swirl lesson for dplyr and pipe operator
# Find out which leagues our teams are in
# Also add column for number of points won by each team in a match --> home_team_points, away_team_points
# then once added, go through the code on this page again and change as needed

########## keep for reference ##########
matchMB %>% 
  group_by(season, stage, home_team_api_id) %>% 
  summarise(goals = sum(home_team_goal), matches = n())

mutate(gpm = goals/matches)
### . ### . ### . ### . ###
matchMB0809s01 %>%
  group_by(home_team_api_id) %>% 
  summarise(sum(home_team_goal))

# and if you want to give a title to the colum called `sum(home_team_goal)`<int>, then:

matchMB %>%
  group_by(season, stage, home_team_api_id) %>% 
  summarise(goals = sum(home_team_goal))
### . ### . End . ### . ###