### Convert SQLite data to something readable in R

# (I) Install Packages ====
rm(list=ls())
library(DBI)
install.packages("RSQLite")
library(RSQLite)

# (II) Explore SQLite data using above

fdb = dbConnect(SQLite(), dbname="originalDB.sqlite")
dbListTables(fdb)

# dbListFields() is kind of like names()
dbListFields(fdb, 'Country')
dbListFields(fdb, 'League')
dbListFields(fdb, 'Match')
dbListFields(fdb, 'Player')
dbListFields(fdb, 'Player_Attributes')
dbListFields(fdb, 'Team')
dbListFields(fdb, 'Team_Attributes')
dbListFields(fdb, 'sqlite_sequence')

# see head() for each df:

dbGetQuery(fdb, 'SELECT * FROM Country')
head(dbGetQuery(fdb, 'SELECT * FROM League'))
head(dbGetQuery(fdb, 'SELECT * FROM Match'))
tail(dbGetQuery(fdb, 'SELECT goal FROM Match'), n=100) # same as tail(match$goal, n=100)
head(dbGetQuery(fdb, 'SELECT * FROM Player'))
head(dbGetQuery(fdb, 'SELECT * FROM Player_Attributes'))
head(dbGetQuery(fdb, 'SELECT * FROM Team'))
head(dbGetQuery(fdb, 'SELECT * FROM Team_Attributes'))
head(dbGetQuery(fdb, 'SELECT * FROM sqlite_sequence'))

### (II) change sqlite tables to csv files ====

# (1) create variable for each table
country.table <- dbGetQuery(fdb, 'SELECT * FROM Country')
league.table <- dbGetQuery(fdb, 'SELECT * FROM League')
match.table <- dbGetQuery(fdb, 'SELECT * FROM Match')
player.table <- dbGetQuery(fdb, 'SELECT * FROM Player')
player.attributes.table <- dbGetQuery(fdb, 'SELECT * FROM Player_Attributes')
team.table <- dbGetQuery(fdb, 'SELECT * FROM Team')
team.attributes.table <- dbGetQuery(fdb, 'SELECT * FROM Team_Attributes')
sqlite.sequence.table <- dbGetQuery(fdb, 'SELECT * FROM sqlite_sequence')

# (2) change that variable to csv file that exists in project folder (Desktop/Soccer)
write.csv(country.table, file = "country.csv")
write.csv(league.table, file = "league.csv")
write.csv(match.table, file = "match.csv")
write.csv(player.table, file = "player.csv")
write.csv(player.attributes.table, file = "playerattributes.csv")
write.csv(team.table, file = "teams.csv")
write.csv(team.attributes.table, file = "teamattributes.csv")
write.csv(sqlite.sequence.table, file = "sqlitesequence.csv")

### (III) Create variable for each csv file ====

read.csv('country.csv')

# variables/df's created here (from csv files created earlier)
country <- read.csv('country.csv')
league <- read.csv('league.csv')
match <- read.csv('match.csv') # this might take a while to load
player <- read.csv('player.csv')
playerattributes <- read.csv('playerattributes.csv')
teams <- read.csv('teams.csv')
teamattributes <- read.csv('teamattributes.csv')
sqlitesequence <- read.csv('sqlitesequence.csv')

### after this we can inspect, play, do everything else, etc. ====
#end