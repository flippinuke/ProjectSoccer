#### (0) First Inspection ====

class(country)

# to view
sapply(country, class)
sapply(league, class)
sapply(match, class)
sapply(player, class)
sapply(playerattributes, class)
sapply(teams, class)
sapply(teamattributes, class)
sapply(sqlitesequence, class)

# export the above to excel (skip) ====
# In retrospect, this is unnecessary. As a new R user, it helped me view the data
# in Excel - something that I am very familiar with - and see the comparison
varscountry <- sapply(country, class)
varsleague <- sapply(league, class)
varsmatch <- sapply(match, class)
varsplayer <- sapply(player, class)
varsplayerattributes <- sapply(playerattributes, class)
varsteams <- sapply(teams, class)
varsteamattributes <- sapply(teamattributes, class)
varssqlitesequence <- sapply(sqlitesequence, class)

write.csv(varscountry, file = "varscountry.csv")
write.csv(varsleague, file = "varsleague.csv")
write.csv(varsmatch, file = "varsmatch.csv")
write.csv(varsplayer, file = "varsplayer.csv")
write.csv(varsplayerattributes, file = "varsplayerattributes.csv")
write.csv(varsteams, file = "varsteams.csv")
write.csv(varsteamattributes, file = "varsteamattributes.csv")
write.csv(varssqlitesequence, file = "varssqlitesequence.csv")


sapply(tables, head)




# (I) Inspecting the variables (head()) ====
head(country)
head(league)
head(match)
head(playerattributes)
head(teams)
head(teamattributes)
head(sqlitesequence)
sqlitesequence

## (II) More inspection: Playing with levels, trying to see levels of data ====

# str() all together
tables <- (c(country, league, match, playerattributes, teams, teamattributes, sqlitesequence))
sapply(tables, str) # need to run "tables <-" command earlier for this to work

# str() individually
str(country)
str(league)
str(match)
str(player)
unique(player$player_fifa_api_id)
str(playerattributes)
str(teams)
str(teamattributes)

# levels
str(teamattributes, max.level = 0)
str(teamattributes, max.level = 1)
str(teamattributes, max.level = 2) # same as above
str(teamattributes, list.len = 0)
str(teamattributes, list.len = 1)
str(teamattributes, list.len = 2)
str(teamattributes, max.level = 2, list.len = 4)

str(sqlitesequence)

# (III) Inspection: see all variable names ====
names(country)
names(league)
names(match)
names(teams)
names(teamattributes)
names(player)
names(playerattributes)
names(sqlitesequence)

# this accomplishes the previous 8 lines of code, but unnecessary because:
# 1. too much output
# 2. Ugly format
tables <- (c(country, league, match, player, playerattributes, teams, teamattributes, sqlitesequence))
sapply(tables, names)

# continue

### Inspecting Player Attributes playerattributes() ====
# Note: I understand that much of this is repetitive. I am keeping it because it displays
# my learning process
head(playerattributes)
head(playerattributes$aggression)
str(playerattributes)
levels(playerattributes)
dim(playerattributes)
names(playerattributes)

library(plyr)
count(playerattributes$aggression) # need library(plyr)

### (VIII) Understanding other variables, this is where we realize match$goal is XML ====
# We also realize that data can be broken down into seasons and match stages
unique(match$season)
unique(match$stage)
unique(match$goal)
head(match$goal)
tail(match$goal, n=100)
head(match$home_team_goal, n=20)

str(player)
str(match)

# how to subset to view only certain columns of a table ====
head(subset(match, select = c("B365H", "BWH", "IWH")))
# or
subset(match, select = c("B365H", "BWH", "IWH"))

### (IX) Inspecting goal data more thoroughly (and other nested data) ====
# Note: boring: mostly head() and str() type functions
### This is where I discovered that the data was in XML format
### I understand that I wrote a csv file to a folder - I did this so I could view it in Excel
### I understand that this is an R course, but soemtimes it's still easier for me just to
### look at it in Excel. I saw the same thing here ... it helped me figure it out eventually

match.goal <- (match$goal)
write.csv(match.goal, file = "PlayData/match.goal.csv")
getwd()

# more subsetting
tail(subset(match, select = c("home_team_goal", "away_team_goal")), n=100)
tail(subset(match, select = c("goal")), n=100)
tail(match$goal, n=100)

str(match$possession)
str(match$possession[1730])
head(match$possession)
match$possession[1730]

# making another csv for same reason
match.possession <- (match$possession)
match.test <- subset(match, select = c("possession", "goal"))

write.csv(match.possession, file = "PlayData/match.possession.csv")
write.csv(match.test, file = "PlayData/match.test.csv")

# making a subsetted data set of match without XML cells and writing to csv ====
match.subset1 <- subset(match, select = c("X", "id", "country_id", "league_id", "season", "stage",
                                          "date", "match_api_id", "home_team_api_id", "away_team_api_id",
                                          "home_team_goal", "away_team_goal", "goal", "shoton", "shotoff",
                                          "foulcommit", "card", "cross", "corner", "possession"))
write.csv(match.subset1, file = "PlayData/match.subset1.csv")


