# Moneyball approach

### Find out who won each season (stage) ====
# these are our tables:
# country, league, match, player, playerattributes, teams, teamattributes, sqlitesequence

# we will be working a lot with 'match', so let's first reduce it to only necessary variables
names(match)
matchMB <- match[c("X", "id", "country_id", "league_id", "season", "stage", "date", "match_api_id",
                   "home_team_api_id", "away_team_api_id", "home_team_goal", "away_team_goal")]
# note: 'match' data can be grouped into 4 subgroups - base data, player data, XML data, betting house odds
# (1) base data: this is the variables contained in 'matchMB'
# (2) player data: variables such as 'home_player_X1' - gives position of player and player id at start of match
# (3) XML data: goal, shoton, shotoff, foulcommit, card, cross, corner, possession - details of shots and gameplay
# (4) betting house odds: odds placed on games by various betting houses

names(matchMB)
head(matchMB)
str(matchMB)

# next, let's break match down further to only 2008/2009 season data
# note: we've broken by each season and viewed str() for each season to count num observations
# to ensure that we haven't missed anything

## ensuring nothing missed:
# variable for each season
matchMB0809 <- dplyr::filter(matchMB, season == "2008/2009")
matchMB0910 <- dplyr::filter(matchMB, season == "2009/2010")
matchMB1011 <- dplyr::filter(matchMB, season == "2010/2011")
matchMB1112 <- dplyr::filter(matchMB, season == "2011/2012")
matchMB1213 <- dplyr::filter(matchMB, season == "2012/2013")
matchMB1314 <- dplyr::filter(matchMB, season == "2013/2014")
matchMB1415 <- dplyr::filter(matchMB, season == "2014/2015")
matchMB1516 <- dplyr::filter(matchMB, season == "2015/2016")

# structure for each season, # observations recorded
str(matchMB0809) #3326 observations
str(matchMB0910) #3230 observations
str(matchMB1011) #3260 observations
str(matchMB1112) #3220 observations
str(matchMB1213) #3260 observations
str(matchMB1314) #3032 observations
str(matchMB1415) #3325 observations
str(matchMB1516) #3326 observations

3326+3230+3260+3220+3260+3032+3325+3326 # total: 25979
str(matchMB) # 25979 observations
# they match

# but all we really need to continue at this point is:
matchMB0809 <- dplyr::filter(matchMB, season == "2008/2009")
str(matchMB0809) # small enough that we don't need to subset further (setseed() or anything) at this point

head(matchMB0809)
unique(matchMB0809$stage) # 38 stages total
library(plyr) # need this for count variable

# tells us how many times each stage appears in data
# also shows us that there are really 4 main stages
count(matchMB0809$stage)

# next, add totals of above to see if it equals # observations, and which teams are in each

# stages 1-30 each appear 94 times # 94 matches played
# stages 31-34 each appear 78 times # 78 matchesplayed
# stages 35-36 each appear 51 times # 51 matches played
# stages 37-38 each appear 46 times # 46 matches played
# no data beyond this, likely teams are selected to move forward to Eurocup of whichever championships

30*94 # 2820
4*78 # 312
2*51 # 102
2*46 # 92
2820+312+102+92 # checks out

# Note: Explanation of stage structure
# Stages 37-38 each appear 46 times. At this point, there are ~90 teams total left
# The same ~90 teams appear in stage 37 and in stage 38:
# - All of the home teams in stage 37 become the away teams in stage 38 (mostly)
# - All of the away teams in stage 38 become the home teams in stage 37
# - However, they don't play the same teams twice (e.g. team 10281 plays one team in 37, and different team in 38)
# My assumption is that each of the previous stages is structured similarly

# I wonder if we can treat stages 1-30 as equal level, then 31-34 as equal level, then 35-36, then 37-38
# Let's work backwards, starting with last stages

# stages 37-38 (91 teams playing) ====
matchMB0809 <- dplyr::filter(matchMB, season == "2008/2009")
matchMB0809
str(matchMB0809)

matchMB0809s37 <- dplyr::filter(matchMB0809, stage == 37)
sort(unique(matchMB0809s37$home_team_api_id)) # to see teams
sort(unique(matchMB0809s37$away_team_api_id)) # to see teams, they are different than previous line

matchMB0809s38 <- dplyr::filter(matchMB0809, stage == 38)
sort(unique(matchMB0809s38$home_team_api_id)) # to see teams
sort(unique(matchMB0809s38$away_team_api_id)) # different than previous, but match sort from stage == 37

matchMB0809s3738 <- dplyr::filter(matchMB0809, stage >= 37) # includes stages 37 and 38
sort(unique(matchMB0809s3738$home_team_api_id)) # as you can see ...
sort(unique(matchMB0809s3738$away_team_api_id)) # ... the teams are the same

# Now let's look at number of goals scored by the teams and number of goals against
# (# viewing home team, away team, and goals scored each)
subset(matchMB0809s3738, select = c("home_team_api_id", "home_team_goal",
                                    "away_team_api_id", "away_team_goal"))

# stages 35-36 (100 teams playing) ====

matchMB0809s35 <- dplyr::filter(matchMB0809, stage == 35)
sort(unique(matchMB0809s35$home_team_api_id)) # to see teams
sort(unique(matchMB0809s35$away_team_api_id)) # to see teams, they are different than previous line

matchMB0809s36 <- dplyr::filter(matchMB0809, stage == 36)
sort(unique(matchMB0809s36$home_team_api_id)) # to see teams
sort(unique(matchMB0809s36$away_team_api_id)) # different than previous, but match sort from stage == 35

matchMB0809s3536 <- dplyr::filter(matchMB0809, stage >= 35 & stage <= 36) # includes stages 37 and 38
sort(unique(matchMB0809s3536$home_team_api_id)) # as you can see ...
sort(unique(matchMB0809s3536$away_team_api_id)) # ... the teams are the same
# sort without unique()
sort(matchMB0809s3536$home_team_api_id) # as you can see, the teams are the same ...
sort(matchMB0809s3536$away_team_api_id) # except 9906 & 8597 (play at home twice) and 8596 & 8633 (play away twice)
# otherwise, there are no repeats

# Now let's look at number of goals scored by the teams and number of goals against
# (# viewing home team, away team, and goals scored each)
subset(matchMB0809s3536, select = c("home_team_api_id", "home_team_goal",
                                    "away_team_api_id", "away_team_goal"))

# Stages 31-34 (156 teams playing) ====
# Here, each team has 2 home games and two away games (and sometimes 3 of one and 1 of the other)

matchMB0809s31 <- dplyr::filter(matchMB0809, stage == 31)
sort(unique(matchMB0809s31$home_team_api_id)) # to see teams
sort(matchMB0809s31$home_team_api_id) # to see teams (not unique)
sort(unique(matchMB0809s31$away_team_api_id)) # to see teams, they are different than previous line

matchMB0809s32 <- dplyr::filter(matchMB0809, stage == 32)
sort(unique(matchMB0809s32$home_team_api_id)) # to see teams
sort(matchMB0809s32$home_team_api_id) # to see teams (not unique)
sort(unique(matchMB0809s32$away_team_api_id)) # different than previous, but match sort from stage == 35

matchMB0809s33 <- dplyr::filter(matchMB0809, stage == 33)
sort(unique(matchMB0809s33$home_team_api_id)) # to see teams
sort(matchMB0809s33$home_team_api_id) # to see teams (not unique)
sort(unique(matchMB0809s33$away_team_api_id)) # to see teams, they are different than previous line

matchMB0809s34 <- dplyr::filter(matchMB0809, stage == 34)
sort(unique(matchMB0809s34$home_team_api_id)) # to see teams
sort(matchMB0809s34$home_team_api_id) # to see teams
sort(unique(matchMB0809s34$away_team_api_id)) # different than previous, but match sort from stage == 35

matchMB0809s3134 <- dplyr::filter(matchMB0809, stage >= 31 & stage <= 34) # includes stages 31-34
sort(unique(matchMB0809s3134$home_team_api_id)) # as you can see ...
sort(unique(matchMB0809s3134$away_team_api_id)) # ... the teams are the same

# sort without unique()

sort(matchMB0809s3134$home_team_api_id) # as you can see, the teams are the same ...
sort(matchMB0809s3134$away_team_api_id) # except 9906 & 8597 (play at home twice) and 8596 & 8633 (play away twice)

# Stages 1-30 (not very necessary - I ran through this to see if there were anomolies) (skip) ====
# (stages 1-30 individually)
# Here, each team has 2 home games and two away games (and sometimes 3 of one and 1 of the other)

matchMB0809s01 <- dplyr::filter(matchMB0809, stage == 1)
sort(unique(matchMB0809s01$home_team_api_id)) # to see teams
sort(matchMB0809s01$home_team_api_id) # to see teams (not unique)
sort(unique(matchMB0809s01$away_team_api_id)) # to see teams

# viewing columns (teams and goals scored)
subset(matchMB0809s01, select = c("home_team_api_id", "home_team_goal",
                                  "away_team_api_id", "away_team_goal"))

# then summing goals (won't sum because each team appears once)

# this shows home team and home team goals (goals scored for) # Cols 1 & 2 of Viewing Columns above
aggregate(matchMB0809s01$home_team_goal, by=list(Category=matchMB0809s01$home_team_api_id), FUN=sum)
# this shows away team and away team goals (goals scored "for" - not very important)
aggregate(matchMB0809s01$away_team_goal, by=list(Category=matchMB0809s01$away_team_api_id), FUN=sum)
# this shows home team and away team goals (goals scored against) # Cols 1 & 4 of Viewing Columns above
aggregate(matchMB0809s01$away_team_goal, by=list(Category=matchMB0809s01$home_team_api_id), FUN=sum)


matchMB0809s02 <- dplyr::filter(matchMB0809, stage == 2)
sort(unique(matchMB0809s02$home_team_api_id)) # to see teams
sort(matchMB0809s02$home_team_api_id) # to see teams (not unique)
sort(unique(matchMB0809s02$away_team_api_id)) # to see teams

matchMB0809s03 <- dplyr::filter(matchMB0809, stage == 3)
sort(unique(matchMB0809s03$home_team_api_id)) # to see teams
sort(matchMB0809s03$home_team_api_id) # to see teams (not unique)
sort(unique(matchMB0809s03$away_team_api_id)) # to see teams

matchMB0809s04 <- dplyr::filter(matchMB0809, stage == 4)
sort(unique(matchMB0809s04$home_team_api_id)) # to see teams
sort(matchMB0809s04$home_team_api_id) # to see teams
sort(unique(matchMB0809s04$away_team_api_id)) # to see teams

matchMB0809s05 <- dplyr::filter(matchMB0809, stage == 5)
sort(unique(matchMB0809s05$home_team_api_id)) # to see teams
sort(matchMB0809s05$home_team_api_id) # to see teams (not unique)
sort(unique(matchMB0809s05$away_team_api_id)) # to see teams

matchMB0809s06 <- dplyr::filter(matchMB0809, stage == 6)
sort(unique(matchMB0809s06$home_team_api_id)) # to see teams
sort(matchMB0809s06$home_team_api_id) # to see teams (not unique)
sort(unique(matchMB0809s06$away_team_api_id)) # to see teams

matchMB0809s07 <- dplyr::filter(matchMB0809, stage == 7)
sort(unique(matchMB0809s07$home_team_api_id)) # to see teams
sort(matchMB0809s07$home_team_api_id) # to see teams (not unique)
sort(unique(matchMB0809s07$away_team_api_id)) # to see teams

matchMB0809s08 <- dplyr::filter(matchMB0809, stage == 8)
sort(unique(matchMB0809s08$home_team_api_id)) # to see teams
sort(matchMB0809s08$home_team_api_id) # to see teams
sort(unique(matchMB0809s08$away_team_api_id)) # to see teams

###

matchMB0809s09 <- dplyr::filter(matchMB0809, stage == 9)
sort(unique(matchMB0809s09$home_team_api_id)) # to see teams
sort(matchMB0809s09$home_team_api_id) # to see teams (not unique)
sort(unique(matchMB0809s09$away_team_api_id)) # to see teams

matchMB0809s10 <- dplyr::filter(matchMB0809, stage == 10)
sort(unique(matchMB0809s10$home_team_api_id)) # to see teams
sort(matchMB0809s10$home_team_api_id) # to see teams (not unique)
sort(unique(matchMB0809s10$away_team_api_id)) # to see teams

matchMB0809s11 <- dplyr::filter(matchMB0809, stage == 11)
sort(unique(matchMB0809s11$home_team_api_id)) # to see teams
sort(matchMB0809s11$home_team_api_id) # to see teams (not unique)
sort(unique(matchMB0809s11$away_team_api_id)) # to see teams

matchMB0809s12 <- dplyr::filter(matchMB0809, stage == 12)
sort(unique(matchMB0809s12$home_team_api_id)) # to see teams
sort(matchMB0809s12$home_team_api_id) # to see teams
sort(unique(matchMB0809s12$away_team_api_id)) # to see teams

matchMB0809s13 <- dplyr::filter(matchMB0809, stage == 13)
sort(unique(matchMB0809s13$home_team_api_id)) # to see teams
sort(matchMB0809s13$home_team_api_id) # to see teams (not unique)
sort(unique(matchMB0809s13$away_team_api_id)) # to see teams

matchMB0809s14 <- dplyr::filter(matchMB0809, stage == 14)
sort(unique(matchMB0809s14$home_team_api_id)) # to see teams
sort(matchMB0809s14$home_team_api_id) # to see teams (not unique)
sort(unique(matchMB0809s14$away_team_api_id)) # to see teams

matchMB0809s15 <- dplyr::filter(matchMB0809, stage == 15)
sort(unique(matchMB0809s15$home_team_api_id)) # to see teams
sort(matchMB0809s15$home_team_api_id) # to see teams
sort(unique(matchMB0809s15$away_team_api_id)) # to see teams

###

matchMB0809s16 <- dplyr::filter(matchMB0809, stage == 16)
sort(unique(matchMB0809s16$home_team_api_id)) # to see teams
sort(matchMB0809s16$home_team_api_id) # to see teams (not unique)
sort(unique(matchMB0809s16$away_team_api_id)) # to see teams

matchMB0809s17 <- dplyr::filter(matchMB0809, stage == 17)
sort(unique(matchMB0809s17$home_team_api_id)) # to see teams
sort(matchMB0809s17$home_team_api_id) # to see teams (not unique)
sort(unique(matchMB0809s17$away_team_api_id)) # to see teams

matchMB0809s18 <- dplyr::filter(matchMB0809, stage == 18)
sort(unique(matchMB0809s18$home_team_api_id)) # to see teams
sort(matchMB0809s18$home_team_api_id) # to see teams (not unique)
sort(unique(matchMB0809s18$away_team_api_id)) # to see teams

matchMB0809s19 <- dplyr::filter(matchMB0809, stage == 19)
sort(unique(matchMB0809s19$home_team_api_id)) # to see teams
sort(matchMB0809s19$home_team_api_id) # to see teams
sort(unique(matchMB0809s19$away_team_api_id)) # to see teams

matchMB0809s20 <- dplyr::filter(matchMB0809, stage == 20)
sort(unique(matchMB0809s20$home_team_api_id)) # to see teams
sort(matchMB0809s20$home_team_api_id) # to see teams (not unique)
sort(unique(matchMB0809s20$away_team_api_id)) # to see teams

matchMB0809s21 <- dplyr::filter(matchMB0809, stage == 21)
sort(unique(matchMB0809s21$home_team_api_id)) # to see teams
sort(matchMB0809s21$home_team_api_id) # to see teams (not unique)
sort(unique(matchMB0809s21$away_team_api_id)) # to see teams

matchMB0809s22 <- dplyr::filter(matchMB0809, stage == 22)
sort(unique(matchMB0809s22$home_team_api_id)) # to see teams
sort(matchMB0809s22$home_team_api_id) # to see teams (not unique)
sort(unique(matchMB0809s22$away_team_api_id)) # to see teams

matchMB0809s23 <- dplyr::filter(matchMB0809, stage == 23)
sort(unique(matchMB0809s23$home_team_api_id)) # to see teams
sort(matchMB0809s23$home_team_api_id) # to see teams
sort(unique(matchMB0809s23$away_team_api_id)) # to see teams

###

matchMB0809s24 <- dplyr::filter(matchMB0809, stage == 24)
sort(unique(matchMB0809s24$home_team_api_id)) # to see teams
sort(matchMB0809s24$home_team_api_id) # to see teams (not unique)
sort(unique(matchMB0809s24$away_team_api_id)) # to see teams

matchMB0809s25 <- dplyr::filter(matchMB0809, stage == 25)
sort(unique(matchMB0809s25$home_team_api_id)) # to see teams
sort(matchMB0809s25$home_team_api_id) # to see teams (not unique)
sort(unique(matchMB0809s25$away_team_api_id)) # to see teams

matchMB0809s26 <- dplyr::filter(matchMB0809, stage == 26)
sort(unique(matchMB0809s26$home_team_api_id)) # to see teams
sort(matchMB0809s26$home_team_api_id) # to see teams (not unique)
sort(unique(matchMB0809s26$away_team_api_id)) # to see teams

matchMB0809s27 <- dplyr::filter(matchMB0809, stage == 27)
sort(unique(matchMB0809s27$home_team_api_id)) # to see teams
sort(matchMB0809s27$home_team_api_id) # to see teams
sort(unique(matchMB0809s27$away_team_api_id)) # to see teams

matchMB0809s28 <- dplyr::filter(matchMB0809, stage == 28)
sort(unique(matchMB0809s28$home_team_api_id)) # to see teams
sort(matchMB0809s28$home_team_api_id) # to see teams (not unique)
sort(unique(matchMB0809s28$away_team_api_id)) # to see teams

matchMB0809s29 <- dplyr::filter(matchMB0809, stage == 29)
sort(unique(matchMB0809s29$home_team_api_id)) # to see teams
sort(matchMB0809s29$home_team_api_id) # to see teams (not unique)
sort(unique(matchMB0809s29$away_team_api_id)) # to see teams

matchMB0809s30 <- dplyr::filter(matchMB0809, stage == 29)
sort(unique(matchMB0809s30$home_team_api_id)) # to see teams
sort(matchMB0809s30$home_team_api_id) # to see teams (not unique)
sort(unique(matchMB0809s30$away_team_api_id)) # to see teams





### Stages 1-30 together (188 teams playing) ====
matchMB0809s0130 <- dplyr::filter(matchMB0809, stage >= 1 & stage <= 30) # includes stages 31-34
sort(unique(matchMB0809s0130$home_team_api_id)) # as you can see ...
sort(unique(matchMB0809s0130$away_team_api_id)) # ... the teams are the same
# number of teams playing:
count(unique(matchMB0809s0130$home_team_api_id))


# adding goals scored for each team stages 1-30 ====
aggregate(matchMB0809s0130$home_team_goal, by=list(Category=matchMB0809s0130$home_team_api_id), FUN=sum)
aggregate(matchMB0809s0130$away_team_goal, by=list(Category=matchMB0809s0130$home_team_api_id), FUN=sum)
aggregate(matchMB0809s0130$away_team_goal, by=list(Category=matchMB0809s0130$away_team_api_id), FUN=sum)
# later we need to calculate goals against for each team

### (Skip - Poor method) Calculate goals for/against each team, and the differential ====
# (1) this shows home team (as target team) and home team goals (goals scored for)
aggregate(matchMB0809s0130$home_team_goal, by=list(Category=matchMB0809s0130$home_team_api_id), FUN=sum)
# (2) this shows home team (as target team) and away team goals (goals scored against)
aggregate(matchMB0809s0130$away_team_goal, by=list(Category=matchMB0809s0130$home_team_api_id), FUN=sum)
# (3) this shows away team (as target team) and away team goals (goals scored for)
aggregate(matchMB0809s0130$away_team_goal, by=list(Category=matchMB0809s0130$away_team_api_id), FUN=sum)
# (4) this shows away team (as target team) and home team goals (goals scored against)
aggregate(matchMB0809s0130$home_team_goal, by=list(Category=matchMB0809s0130$away_team_api_id), FUN=sum)

# next, we need to add 1 & 3 for each team, and subtract the 2 & 4 for each team
# this adds 1 & 3
aggregate(matchMB0809s0130$home_team_goal, by=list(Category=matchMB0809s0130$home_team_api_id), FUN=sum)+
  aggregate(matchMB0809s0130$away_team_goal, by=list(Category=matchMB0809s0130$away_team_api_id), FUN=sum)

# this adds 2 & 4
aggregate(matchMB0809s0130$away_team_goal, by=list(Category=matchMB0809s0130$home_team_api_id), FUN=sum)+
  aggregate(matchMB0809s0130$home_team_goal, by=list(Category=matchMB0809s0130$away_team_api_id), FUN=sum)

# unfortunately, it also adds the api_id numbers
# next, (1+3)-(2+4)

# this gives us total goals scored for minus total goals scored against
# however, it also adds/subtracts the api_id number. how to avoid that?
aggregate(matchMB0809s0130$home_team_goal, by=list(Category=matchMB0809s0130$home_team_api_id), FUN=sum)+
  aggregate(matchMB0809s0130$away_team_goal, by=list(Category=matchMB0809s0130$away_team_api_id), FUN=sum)-
  (aggregate(matchMB0809s0130$away_team_goal, by=list(Category=matchMB0809s0130$home_team_api_id), FUN=sum)+
     aggregate(matchMB0809s0130$home_team_goal, by=list(Category=matchMB0809s0130$away_team_api_id), FUN=sum))

# what if instead, we create two new columns - (1) home team goal differential (2) away team goal differential
# we can then sum these using the prevoius code an avoid "adding" or "subtracting" the team_api_id's

### (Better method) begin with stage (I): 1-30 ====

head(matchMB0809s0130)
home_team_goal_difT <- matchMB0809s0130$home_team_goal - matchMB0809s0130$away_team_goal
away_team_goal_difT <- matchMB0809s0130$away_team_goal - matchMB0809s0130$home_team_goal

# it works. check:
subset(matchMB0809s01, select = c("home_team_api_id", "home_team_goal",
                                  "away_team_api_id", "away_team_goal"))

head(home_team_goal_difT)
head(away_team_goal_difT)

# adding two rows # testing with matchMB0809s0130, can later do it for matchMB or even just match

matchMB0809s0130a <- dplyr::mutate(matchMB0809s0130, home_team_goal_difT = home_team_goal - away_team_goal)
matchMB0809s0130a <- dplyr::mutate(matchMB0809s0130a, away_team_goal_difT = away_team_goal - home_team_goal)

# (1) this shows home team (as target team) and goal differential
aggregate(matchMB0809s0130a$home_team_goal_difT, by=list(Category=matchMB0809s0130$home_team_api_id), FUN=sum)
# (2) this shows away team (as target team) and goal differential
aggregate(matchMB0809s0130a$away_team_goal_difT, by=list(Category=matchMB0809s0130a$away_team_api_id), FUN=sum)

# Next make a single col using (1) & (2) above, then aggregate() again, and that gives us total stage goalsdif per team

homecol <- aggregate(matchMB0809s0130a$home_team_goal_difT, by=list(Category=matchMB0809s0130$home_team_api_id), FUN=sum)
awaycol <- aggregate(matchMB0809s0130a$away_team_goal_difT, by=list(Category=matchMB0809s0130a$away_team_api_id), FUN=sum)
onecol <- dplyr::bind_rows(homecol, awaycol)

aggregate(onecol$x, by=list(Category=onecol$Category), FUN=sum)
head(homecol)
# NOTE: It seems aggregate() loses the column names when you save it as a variable