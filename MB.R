# Moneyball approach
library(dplyr) # need this for pipe operator %>% 
library(magrittr) # need this for %<>%
library(ggplot)

### This section creates a summary data set with very high level data ====
# our tables: country, league, match, player, playerattributes, teams, teamattributes, sqlitesequence

# note: 'match' data can be grouped into 4 subgroups - (1) base data, (2) player data, (3) XML data, (4) betting house odds
# (1) base data: this is the variables contained in 'matchMB' # this is all we need for moneyball.R
# (2) player data: variables such as 'home_player_X1' - gives position of player and player id at start of match
# (3) XML data: goal, shoton, shotoff, foulcommit, card, cross, corner, possession - details of shots and gameplay
# (4) betting house odds: odds placed on games by various betting houses

# we will be working a lot with 'match', so let's first reduce it to only necessary variables [add XML here? or later?]
# dplyr version
# note: original moved to MBexcess: replaced with dragon

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
matchMB1 %<>% rename(team = home_team_api_id, goals = home_team_goal, opponent = away_team_api_id,
                     opponent_goals = away_team_goal)

matchMB2 %<>% rename(team = away_team_api_id, goals = away_team_goal, opponent = home_team_api_id,
                     opponent_goals = home_team_goal)

# add column to matchMB1 - home_or_away = H
# add column to matchMB2 - home_or_away = A
matchMB1$home_or_away <- c('H') # perhaps make 1/0 or T/F later, but keep as is for easy identification
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

# stats df ==== maybe KEEP
statsMB3 <-
  matchMB3 %>%
  group_by(team, season, league_id) %>% # if add "season" here, it affects the ggplot (and we probably want to)
  summarise(sgoals = sum(goals), soppgoals = sum(opponent_goals), swins = sum(win),
            slosses = sum(loss), sdraws = sum(draw), sgdiff = sum(goal_dif),
            spoints = sum(points), maxStage = (ifelse(max(stage) < 31, 1,
                                                      ifelse(max(stage) > 30 & max(stage) < 35, 2,
                                                             ifelse(max(stage) > 34 & max(stage) < 37, 3, 4))))) # max(stage) instead of max(real_stage) (consider, since we added 'season' to group_by()

# create league_id key - likely KEEP ====
leagueKEY <-
  matchMB3 %>% 
  select(team, league_id) %>% 
  distinct()

# left join league_id into statsMB3
statsMB3 <- dplyr::left_join(statsMB3, leagueKEY, by = "team")

# now we can do plot, ggplot, and regressions

# plotting ====
# add identifier column.
statsMB3$statID <- seq.int(nrow(statsMB3))

# x/y plots ====
#NOTE: these are affected by whether we include "season" in the statsMB3 <- matchMB3 ... code

# maybe KEEP
# plot points acquired against goal differential, all stages
# this shows that the dispersion is similar, regardless of how many games a league plays
ggplot(data = statsMB3, aes(sgdiff, spoints, col = factor(maxStage))) + geom_point() +
  labs(title = "Points vs Goal Differential") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Goal Differential", y = "Points") +
  labs(colour = "Stage Reached")

names(statsMB3)

# do the above with DragonStats1
ggplot(data = DoubleDragon3, aes(goalDiff, points, col = factor(maxStage))) + geom_point() +
  labs(title = "Points vs Goal Differential") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Goal Differential", y = "Points") +
  labs(colour = "Stage Reached")


# same, but with geom_smooth() (need to do this for only stage 1, pick one season)
# while stage reached does not show progression, it can be used to illustrate that a
# team's success rate remains relatively constant throughout stages
ggplot(data = statsMB3, aes(sgdiff, spoints, col = factor(maxStage))) + geom_point() +
  labs(title = "Points vs Goal Differential") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Goal Differential", y = "Points") + geom_smooth(span = .7, level = 0) +
  labs(colour = "Stages Played")
# can we add yellow/red cards on top of this? bc each point corresponds to a team

# regressions ====

# basic linear regression: one input variable and one output variable

# input variable: goal differential sum, all seasons
# output variable: sum of wins, all seasons
winLM <- lm(swins~sgdiff, data = statsMB3)
summary(winLM)
# we should probably do this and restrict it to stage 1

# input variable: goal differential sum, all seasons
# output variable: sum of points, all seasons
winLM <- lm(spoints~sgdiff, data = statsMB3)
summary(winLM)

# input variable: sum of goals, all seasons
# ouput variable: sum of wins, all seasons
winLM <- lm(swins~sgoals, data = statsMB3)
summary(winLM)
# very clearly shows that the more goals you score, the more wins you get

# these are kind of obvious, but they provide a good set up. Next:
# we want to determine - which factors contribute most to a goal?
# probably add XML data here

# would not make sense to include sgdiff, sgoals, as independent variables together, as they are too closely related
# makes more sense to run linear regression on matchMB3, where we can include variables such as
# home_or_away, and if possible to add - number of red cards, yellow cards, shots on goal, etc.

#### Multiple x/y plots ####

# Stage 1, Goal Differential & Points, all seasons
stats6log <- matchMB3 %>% 
  filter(real_stage == 1) %>%
  group_by(team, league_id, season) %>% # strange - if i remove 'season' and run ggplot2 for all seasons, the charts look different
  summarise(sgoals = sum(goals), soppgoals = sum(opponent_goals), swins = sum(win),
            slosses = sum(loss), sdraws = sum(draw), sgdiff = sum(goal_dif),
            spoints = sum(points))

# left join leagueKEY to stats4lof to get league name column
stats6log <- dplyr::left_join(stats6log, leagueKEY, by = "league_id")
names(stats6log)

# Stage 1, Goal Differential & Points, all seasons. # funky (unless add 'season' to group_by above, then ok)
ggplot(stats6log, aes(sgdiff, spoints)) + geom_point(colour="#000099") + # removed: col = factor(league_id)
  labs(title = "Goal Differential & Points, All Seasons") + # plot title
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Goal Differential", y = "Total Points") +
  facet_wrap(~country, ncol = 4)

# easier way to view individual seasons
ggplot(subset(stats6log, season %in% c("2015/2016"))) + geom_point(aes(sgdiff, spoints), colour="blue") +
  labs(title = "Goal Differential & Points, '15/16 Season") + # plot title
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Goal Differential", y = "Total Points") +
  facet_wrap(~country, ncol = 4)

# FOCUS: same as above chart, but for only one league
ggplot(subset(stats6log, season %in% c("2015/2016") & league_id %in% ("1729"))) +
  geom_point(aes(sgdiff, spoints), colour="blue") +
  labs(title = "Goal Differential & Points, '15/16 Season") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Goal Differential", y = "Total Points")

# original DragonStats scatter plot, Goal Diff vs Points
# first, create labeller:
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
# this is very interesting, because the lowest number of cards happens for the
# winningest teams:
ggplot(subset(DragonStats1, season %in% c("2015/2016"))) +
  geom_point(aes(sHTgdiff, sHTpoints), colour="blue", size = 2) +
  labs(title = "Goal Differential & Points, '15/16 Season") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Goal Differential", y = "Total Points") +
  geom_bar(aes(sHTgdiff, sHTcard), stat = "identity", alpha = .5) +
  facet_wrap(~league_id, ncol = 3, labeller = as_labeller(team_names))

# without trendlines, English Premier League:
ggplot(subset(DragonStats1, season %in% c("2015/2016") & league_id %in% ("1729"))) +
         geom_point(aes(sHTgdiff, sHTpoints), colour="blue", size = 2) +
         labs(title = "Goal Differential & Points, '15/16 Season") +
         theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Goal Differential", y = "Total Points") +
  geom_bar(aes(x = sHTgdiff, y = sHTcard), stat = "identity", alpha = .5)

# with trendlines, English Premier League:
ggplot(subset(DragonStats1, season %in% c("2015/2016") & league_id %in% ("1729"))) +
  geom_point(aes(sHTgdiff, sHTpoints), colour="blue", size = 2) +
  labs(title = "Goal Differential & Points, '15/16 Season") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Goal Differential", y = "Total Points") +
  geom_bar(aes(x = sHTgdiff, y = sHTcard), stat = "identity", alpha = .5) +
  geom_smooth(aes(sHTgdiff, sHTpoints), se = FALSE) +
  geom_smooth(aes(sHTgdiff, sHTcard), se = FALSE, col = "dark gray")

# this chart shows:
# 1. English Premier league is much more competitive (teams are closer to each other in skill than other leagues)
# 2. As teams win more games, they get carded less (for home games)
# 3. If refs get paid off, then Spain is the most corrupt league
# facet wrap each league:
ggplot(subset(DragonStats1, season %in% c("2015/2016"))) +
  geom_point(aes(sHTgdiff, sHTpoints), colour="blue", size = 2) +
  labs(title = "Goal Differential & Points, '15/16 Season") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Goal Differential", y = "Total Points") +
  geom_bar(aes(x = sHTgdiff, y = sHTcard), stat = "identity", alpha = .5) +
  geom_smooth(aes(sHTgdiff, sHTpoints), se = FALSE, col = "blue") +
  geom_smooth(aes(sHTgdiff, sHTcard), se = FALSE) +
  facet_wrap(~league_id, ncol = 3, labeller = as_labeller(team_names))
#  + labs(1729 = "Country")

# Let's see if we can do a similar chart for yellow and red cards.
# (though I think I'd prefer bar, and then superimpose it over the previous, or have
# it directly underneath)


# dragon removes the nested XML data that we no longer need
dragon <- testALL %>% 
  select(country_id, league_id, season, stage, date, match_api_id,
         home_team_api_id, away_team_api_id, home_team_goal, away_team_goal,
         homePoss, awayPoss,
         HTshoton, HTshotoff, HTfouls, HTcard, htYcard, htRcard, HTcross, HTcorners,
         ATshoton, ATshotoff, ATfouls, ATcard, atYcard, atRcard, ATcross, ATcorners,
         HTgoaldiff, HTwin, HTdraw, HTpoints,
         ATgoaldiff, ATpoints)

# From here, we go as we outline in the notebook. Use moneyball.R for code reference if necessary

# stats df ====
names(dragon)

DragonStats1 <-
  dragon %>%
  group_by(home_team_api_id, season, league_id) %>% # if add "season" or "league_id" here, it affects the ggplot (and we probably want to)
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

# Now create bar chart of # of cards
# HT cards vs HT goal differential. This one is a bit more interesting (then plotting against HT point)
# we perhaps want to summarize by league_id and season in above DragonStats1 code for this?:
ggplot(DragonStats1, aes(sHTcard, sHTgdiff)) + geom_point() + geom_smooth()
ggplot(DragonStats1, aes(sATcard, sATgdiff)) + geom_point() + geom_smooth()
# AT cards vs AT goal differential. Even more of a negative slope. Interesting
ggplot(DragonStats1, aes(sATcard, sATgdiff)) + geom_point() + geom_smooth()

# CONTINUE HERE

# now, group_by(home_team_api_id) only:
# refer to row 685 of MBexcess for guidance:

DragonStats2 <- dragon %>%
  filter(season == "2008/2009") %>% 
  group_by(home_team_api_id) %>% # if add "season" or "league_id" here, it affects the ggplot (and we probably want to)
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

names(DragonStats2)

DragonStats2 <- DragonStats2[!(DragonStats2$home_team_api_id == "208931"),]
max(DragonStats2$home_team_api_id)

# this
DragonStats2 %<>% arrange(sHTcard)
DragonStats2$statID <- seq.int(nrow(DragonStats2))

head(DragonStats2$sHTpoints)

# seems to be positive corellation between number cards receivedand goal differential?
# problem is that is only grouped by home team - need to group by league and year for higher accuracy
ggplot(data = DragonStats2, aes(sHTgdiff, statID)) + geom_point()

# +
#   labs(title = "Points, 2008/2009") + theme(plot.title = element_text(hjust = 0.5)) +
#   labs(x = "Points", y = "Teams") + scale_y_discrete(breaks = c(0)) +
#   labs(colour = "Stage Reached")






# try subset stage == 1
# interesting, bc low scoring teams don't get carded much, but if you remove teams with less than 10 points, then you have a negative trend
# again - likely necessary to group_by league and season
ggplot(subset(DragonStats1, maxStage %in% c(1:30))) + geom_point(aes(sHTcard, sHTgdiff))
ggplot(subset(DragonStats1, league_id %in% c("1729", "4769", "10257", "19694", "21518"))) +
  geom_point(aes(sHTcard, sHTgdiff)) + geom_smooth(aes(sHTcard, sHTgdiff))
ggplot(subset(DragonStats1, league_id %in% c("1729", "4769", "10257", "19694", "21518"))) +
  geom_point(aes(sATcard, sATgdiff)) + geom_smooth(aes(sHTcard, sHTgdiff))



# AT cards vs AT points
ggplot(DragonStats1, aes(sATcard, sATpoints)) + geom_point()
# appears to be a positive relationship, but we need to control for number of games played, and maybe season and league


# this is also a good plot, because it contains more data - it doesn't require the XML data (PERHAPS ADD LATER)
ggplot(subset(stats6log, season %in% c("2015/2016"))) + geom_point(aes(sgdiff, spoints), colour="blue") +
  labs(title = "Goal Differential & Points, '15/16 Season") + # plot title
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Goal Differential", y = "Total Points") +
  facet_wrap(~country, ncol = 4)

# we don't have (1) losses (2) points

# Creating DoubleDragon:

names(dragon)

DoubleDragon1 <- dragon # to create the df that we will add to
DoubleDragon2 <- dragon # to create the df that we will be adding to matchMB1

# change column names ====
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
DoubleDragon1$home_or_away <- c('H') # perhaps make 1/0 or T/F later, but keep as is for easy identification
DoubleDragon2$home_or_away <- c('A') # but we'll start with these two

# now we need to bind mb2 columns to mb1 columns. to keep it clean, let's create mb3 <- mb1, and bind
# to mb3

DoubleDragon3 <- DoubleDragon1

# now bind
# rm(matchMB3) # why?
DoubleDragon3 <- dplyr::bind_rows(DoubleDragon1, DoubleDragon2)

# add calculated fields: ====
library(magrittr)
DoubleDragon3 %<>%
  mutate(win = ifelse(goals > oppgoals, 1, 0), loss = ifelse(goals < oppgoals, 1, 0),
         draw = ifelse(goals == oppgoals, 1, 0), goal_dif = goals - oppgoals,
         points = ifelse(goals > oppgoals, 3,
                         ifelse(goals == oppgoals, 1, 0)))

names(DoubleDragon3)
head(DoubleDragon3)

