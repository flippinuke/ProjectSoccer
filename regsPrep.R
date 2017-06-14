# prepare data for regs ====
# bringing in additional variables to test on

names(match)

# the data that is brought in, we will want to add to the matchAN variables
# including XML data (excpet goal - likely don't need)

# decrease the data set size at first ====
library(dplyr)
matchAD1 <- match %>% 
  filter(season == "2011/2012") %>%
  select(country_id, league_id, season, stage, date, match_api_id,
         home_team_api_id, away_team_api_id, home_team_goal, away_team_goal,
         shoton, shotoff, foulcommit, card, cross, corner, possession)

library(XML) # need this for xmlToList
# library(plyr) # after done, use this to unload plyr: detach("package:vegan", unload=TRUE)
# need plyr for our thing to work below
# no longer need plyr

# inspect the various XML variables, and see what we want
# shoton, shotff, foulcommit, card, cross, corner, possession
# let's start with possession

head(matchAD1[order(match$possession),])

# create test df ====
test <- matchAD1

# NOTE:
# test is for 2011/2012 (moved to regsExcess.R)
# testALL is for all seasons

# remove rows that don't have data (consider imputation later) ====

# removes all rows in matchAD1$possession with NA
test <- test[complete.cases(test$possession),]

# get rid of rows where test$possession contains only "<possession />"
test <- test[!(test$possession == "<possession />"),]

# and get rid of rows where test$possession contains only "<possession />"
test <- test[!(test$card == "<card />"),] # removes 40 more rows

# this works. don't touch it
# gives length of possession by game for home team (by percentage of game)
for (i in 1:length(test$possession)){
  test$homePoss[i] <- as.character(as.data.frame(do.call(rbind, xmlToList(as.character(test$possession[i])))) %>%
                                     filter(row_number() == n()) %>% 
                                     select(homepos))
}

# the above code occasionally grabs the wrong value. In all cases that I searched through (well over 1000),
# it always moved the correct value one column to the left, to "awaypos." In those cases, the value in the
# last row of "awaypos" should actually be under "homepos". I fixed that first by creating a column
# test$awayPoss, then switching the necessary variables with the following:

# length of possession by game for away team (by percentage of game)
for (i in 1:length(test$possession)){
  test$awayPoss[i] <- as.character(as.data.frame(do.call(rbind, xmlToList(as.character(test$possession[i])))) %>%
                                     filter(row_number() == n()) %>% 
                                     select(awaypos))
}

# replace the wrong homePoss value with the correct value
# dplyr/magrittr version:
test %<>%
  mutate(homePoss = ifelse(nchar(homePoss > 10), awayPoss, homePoss))
# base R version:
test$homePoss <- ifelse(nchar(test$homePoss > 10), test$awayPoss, test$homePoss)

# remove excess characters:
test$homePoss <- as.integer(substr(test$homePossReal, start = 7, stop = 8))

# create away team possession variable:
test$awayPoss <- 100 - test$homePoss

# next -
# (1) inspect other xml cells ====
# (2) create for loop for other xml cells

### Add the rest of the XML variables: ====

# number of shots on goal - both teams summed
for (i in 1:length(test$shoton)){
  test$nshoton[i] <- nrow(do.call(rbind, xmlToList(as.character(test$shoton[i]))))
}

# number of shots on goal - home team against away team
for (i in 1: length(test$shoton)){
  test$HTshoton[i] <- nrow(as.data.frame(do.call(rbind, xmlToList(as.character(test$shoton[i])))) %>% 
                            filter(player1 == test$home_team_api_id[i] |
                                     sortorder == test$home_team_api_id[i] |
                                     team == test$home_team_api_id[i] |
                                     n == test$home_team_api_id[i]))
}

# number of shots on goal - away team against home team
for (i in 1: length(test$shoton)){
  test$ATshoton[i] <- nrow(as.data.frame(do.call(rbind, xmlToList(as.character(test$shoton[i])))) %>% 
                             filter(player1 == test$home_team_api_id[i] |
                                      sortorder == test$away_team_api_id[i] |
                                      team == test$away_team_api_id[i] |
                                      n == test$away_team_api_id[i]))
}

# number of shots off goal - both teams summed
for (i in 1:length(test$shotoff)){
  test$nshotoff[i] <- nrow(do.call(rbind, xmlToList(as.character(test$shotoff[i]))))
}

# number of shots off goal - home team against away team
for (i in 1: length(test$shotoff)){
  test$HTshotoff[i] <- nrow(as.data.frame(do.call(rbind, xmlToList(as.character(test$shotoff[i])))) %>% 
                             filter(player1 == test$home_team_api_id[i] |
                                      sortorder == test$home_team_api_id[i] |
                                      team == test$home_team_api_id[i] |
                                      n == test$home_team_api_id[i]))
}

# number of shots off goal - away team against home team
for (i in 1: length(test$shotoff)){
  test$ATshotoff[i] <- nrow(as.data.frame(do.call(rbind, xmlToList(as.character(test$shotoff[i])))) %>% 
                             filter(player1 == test$home_team_api_id[i] |
                                      sortorder == test$away_team_api_id[i] |
                                      team == test$away_team_api_id[i] |
                                      n == test$away_team_api_id[i]))
}

# number of fouls committed - both teams summed # this takes a while to run
for (i in 1:length(test$foulcommit)){
  test$nfoulcommit[i] <- nrow(do.call(rbind, xmlToList(as.character(test$foulcommit[i]))))
}

# number of fouls committed - home team against away team
for (i in 1: length(test$foulcommit)){
  test$HTfouls[i] <- nrow(as.data.frame(do.call(rbind, xmlToList(as.character(test$foulcommit[i])))) %>% 
                              filter(player1 == test$home_team_api_id[i] |
                                       sortorder == test$home_team_api_id[i] |
                                       team == test$home_team_api_id[i] |
                                       n == test$home_team_api_id[i]))
}

# number of fouls committed - away team against home team
for (i in 1: length(test$foulcommit)){
  test$ATfouls[i] <- nrow(as.data.frame(do.call(rbind, xmlToList(as.character(test$foulcommit[i])))) %>% 
                              filter(player1 == test$home_team_api_id[i] |
                                       sortorder == test$away_team_api_id[i] |
                                       team == test$away_team_api_id[i] |
                                       n == test$away_team_api_id[i]))
}

# number of cards given - both yellow and red for both teams, summed
for (i in 1:length(test$card)){
  test$ncard[i] <- nrow(do.call(rbind, xmlToList(as.character(test$card[i]))))
}

# number of home team cards (both yellow and red)
for (i in 1:length(test$card)){
  test$HTcard[i] <- nrow(as.data.frame(do.call(rbind, xmlToList(as.character(test$card[i])))) %>% 
                            filter(sortorder == test$home_team_api_id[i] |
                                     team == test$home_team_api_id[i] |
                                     n == test$home_team_api_id[i]))
}

# number of yellow cards for the home team
for (i in 1: length(test$card)){
  test$htYcard[i] <- nrow(as.data.frame(do.call(rbind, xmlToList(as.character(test$card[i])))) %>% 
                            filter(sortorder == test$home_team_api_id[i] |
                                     team == test$home_team_api_id[i] |
                                     n == test$home_team_api_id[i],
                                   comment == "y"))
}

# number of red cards for the home team
for (i in 1: length(test$card)){
  test$htRcard[i] <- nrow(as.data.frame(do.call(rbind, xmlToList(as.character(test$card[i])))) %>% 
                            filter(sortorder == test$home_team_api_id[i] |
                                     team == test$home_team_api_id[i] |
                                     n == test$home_team_api_id[i],
                                   comment == "r"))
}

# number of away team cards (both yellow and red)
for (i in 1:length(test$card)){
  test$ATcard[i] <- nrow(as.data.frame(do.call(rbind, xmlToList(as.character(test$card[i])))) %>% 
                           filter(sortorder == test$away_team_api_id[i] |
                                    team == test$away_team_api_id[i] |
                                    n == test$away_team_api_id[i]))
}

# number of yellow cards for the away team
for (i in 1: length(test$card)){
  test$atYcard[i] <- nrow(as.data.frame(do.call(rbind, xmlToList(as.character(test$card[i])))) %>% 
                            filter(sortorder == test$away_team_api_id[i] |
                                     team == test$away_team_api_id[i] |
                                     n == test$away_team_api_id[i],
                                   comment == "y"))
}

# number of red cards for the away team
for (i in 1: length(test$card)){
  test$atRcard[i] <- nrow(as.data.frame(do.call(rbind, xmlToList(as.character(test$card[i])))) %>% 
                            filter(sortorder == test$away_team_api_id[i] |
                                     team == test$away_team_api_id[i] |
                                     n == test$away_team_api_id[i],
                                   comment == "r"))
}

### number of crosses, both teams summed # take a while to run # cleaner, but still some bad formatting
for (i in 1:length(test$cross)){
  test$ncross[i] <- nrow(do.call(rbind, xmlToList(as.character(test$cross[i]))))
}

# number of crosses performed by home team
for (i in 1:length(test$cross)){
  test$HTcross[i] <- nrow(as.data.frame(do.call(rbind, xmlToList(as.character(test$cross[i])))) %>% 
                              filter(player1 == test$home_team_api_id[i] |
                                       sortorder == test$home_team_api_id[i] |
                                       team == test$home_team_api_id[i] |
                                       n == test$home_team_api_id[i]))
}

# number of crosses performed by away team
for (i in 1:length(test$cross)){
  test$ATcross[i] <- nrow(as.data.frame(do.call(rbind, xmlToList(as.character(test$cross[i])))) %>% 
                            filter(player1 == test$away_team_api_id[i] |
                                     sortorder == test$away_team_api_id[i] |
                                     team == test$away_team_api_id[i] |
                                     n == test$away_team_api_id[i]))
}

# number of corners, both teams summed
for (i in 1:length(test$corner)){
  test$ncorner[i] <- nrow(do.call(rbind, xmlToList(as.character(test$corner[i]))))
}
names(test)

# number of corners performed by home team
for (i in 1:length(test$corner)){
  test$HTcorners[i] <- nrow(as.data.frame(do.call(rbind, xmlToList(as.character(test$corner[i])))) %>% 
                                         filter(player1 == test$home_team_api_id[i] |
                                                  sortorder == test$home_team_api_id[i] |
                                                  team == test$home_team_api_id[i] |
                                                  n == test$home_team_api_id[i]))
}

# number of corners performed by away team
for (i in 1:length(test$corner)){
  test$ATcorners[i] <- nrow(as.data.frame(do.call(rbind, xmlToList(as.character(test$corner[i])))) %>% 
                              filter(player1 == test$away_team_api_id[i] |
                                       sortorder == test$away_team_api_id[i] |
                                       team == test$away_team_api_id[i] |
                                       n == test$away_team_api_id[i]))
}



# a bunch of head()s ====

head(test$nshoton, n = 50)
head(test$HTshoton, n = 50)
head(test$ATshoton, n = 50)

head(test$nshotoff, n = 50)
head(test$HTshotoff, n = 50)
head(test$ATshotoff, n = 50)

head(test$nfoulcommit, n = 50)
head(test$HTfouls, n = 50)
head(test$ATfouls, n = 50)

head(test$HTcard, n = 50)
head(test$htYcard, n = 50)
head(test$htRcard, n = 50)
head(test$ATcard, n = 50)
head(test$atYcard, n = 50)
head(test$atRcard, n = 50)

head(test$HTcross, n = 50) # seems to have worked
head(test$ATcross, n = 50)
head(test$HTcorners, n = 50)
head(test$ATcorners, n = 50)

names(test)

test$win <- ifelse(test$home_team_goal > test$away_team_goal, 1, 0)
test$draw <- ifelse(test$home_team_goal == test$away_team_goal, 1, 0)
test$goaldiff <- test$home_team_goal - test$away_team_goal

# create other variables, such as:
# win/loss/draw - this can be made with 2 variables (win/not, draw/not)
#  - and the combination of those two binary variables tells you whether it was a win, draw, or loss
# goal differential
# maybe: points acquired
# 
# And then think about whether you want to "double" the data set

###########################################################################################
################################ VARIABLES FOR ALL SEASONS ################################ ====
###########################################################################################


# decrease the data set size - only  include desired variables ====
library(dplyr)
matchALL <- match %>%
  select(country_id, league_id, season, stage, date, match_api_id,
         home_team_api_id, away_team_api_id, home_team_goal, away_team_goal,
         shoton, shotoff, foulcommit, card, cross, corner, possession)

library(XML) # need this for xmlToList

# create test df ====
testALL <- matchALL

# remove rows that don't have data (consider imputation later) ====

# removes all rows in matchAD1$possession with NA - size: 25979 rows to 14217 rows
testALL <- testALL[complete.cases(testALL$possession),]

# get rid of rows where test$possession contains only "<possession />" - size: 14217 rows to 8419 rows
testALL <- testALL[!(testALL$possession == "<possession />"),]

# and get rid of rows where test$possession contains only "<possession />"
testALL <- testALL[!(testALL$card == "<card />"),] # removes 40 more rows from 8419 to 8125


# gives length of possession by during game for home team (by percentage of game) ====
# this works. don't touch it (left to let this run overnight)
for (i in 1:length(testALL$possession)){
  testALL$homePoss[i] <- as.character(as.data.frame(do.call(rbind, xmlToList(as.character(testALL$possession[i])))) %>%
                                     filter(row_number() == n()) %>% 
                                     select(homepos))
}

# this shows us that 614 of the variables from above were brought in incorrectly
testALL$homePoss[which(nchar(testALL$homePoss) > 10)]

# dplyr version:
testALL %>% 
  select(homePoss) %>% 
  filter(nchar(homePoss) > 10) %>% 
  count

# 1. replace incorrect variables with proper ones
# 2. remove excess characters

# Start by finding away team possesion:
# length of possession by game for away team (by percentage of game) ====
for (i in 1:length(testALL$possession)){
  testALL$awayPoss[i] <- as.character(as.data.frame(do.call(rbind, xmlToList(as.character(testALL$possession[i])))) %>%
                                     filter(row_number() == n()) %>% 
                                     select(awaypos))
}

# replace the wrong homePoss values with the correct values
# dplyr/magrittr version:
testALL %<>%
  mutate(homePoss = ifelse(nchar(homePoss > 10), awayPoss, homePoss))
# base R version:
testALL$homePoss <- ifelse(nchar(testALL$homePoss > 10), testALL$awayPoss, testALL$homePoss)

# remove excess characters:
testALL$homePoss <- as.integer(substr(testALL$homePoss, start = 7, stop = 8))

# create away team possession variable:
testALL$awayPoss <- 100 - testALL$homePoss

# next -
# (1) inspect other xml cells ====
# (2) create for loop for other xml cells

### Add the rest of the XML variables: ====

# number of shots on goal - both teams summed ====
for (i in 1:length(testALL$shoton)){
  testALL$nshoton[i] <- nrow(do.call(rbind, xmlToList(as.character(testALL$shoton[i]))))
}

# number of shots on goal - home team against away team ====
for (i in 1: length(testALL$shoton)){
  testALL$HTshoton[i] <- nrow(as.data.frame(do.call(rbind, xmlToList(as.character(testALL$shoton[i])))) %>% 
                             filter(player1 == testALL$home_team_api_id[i] |
                                      sortorder == testALL$home_team_api_id[i] |
                                      team == testALL$home_team_api_id[i] |
                                      n == testALL$home_team_api_id[i]))
}

# number of shots on goal - away team against home team ====
for (i in 1: length(testALL$shoton)){
  testALL$ATshoton[i] <- nrow(as.data.frame(do.call(rbind, xmlToList(as.character(testALL$shoton[i])))) %>% 
                             filter(player1 == testALL$home_team_api_id[i] |
                                      sortorder == testALL$away_team_api_id[i] |
                                      team == testALL$away_team_api_id[i] |
                                      n == testALL$away_team_api_id[i]))
}

# number of shots off goal - both teams summed ====
for (i in 1:length(testALL$shotoff)){
  testALL$nshotoff[i] <- nrow(do.call(rbind, xmlToList(as.character(testALL$shotoff[i]))))
}

# number of shots off goal - home team against away team ====
for (i in 1: length(testALL$shotoff)){
  testALL$HTshotoff[i] <- nrow(as.data.frame(do.call(rbind, xmlToList(as.character(testALL$shotoff[i])))) %>% 
                              filter(player1 == testALL$home_team_api_id[i] |
                                       sortorder == testALL$home_team_api_id[i] |
                                       team == testALL$home_team_api_id[i] |
                                       n == testALL$home_team_api_id[i]))
}

# number of shots off goal - away team against home team ====
for (i in 1: length(testALL$shotoff)){
  testALL$ATshotoff[i] <- nrow(as.data.frame(do.call(rbind, xmlToList(as.character(testALL$shotoff[i])))) %>% 
                              filter(player1 == testALL$home_team_api_id[i] |
                                       sortorder == testALL$away_team_api_id[i] |
                                       team == testALL$away_team_api_id[i] |
                                       n == testALL$away_team_api_id[i]))
}

# number of fouls committed - both teams summed # this takes a while to run ====
for (i in 1:length(testALL$foulcommit)){
  testALL$nfoulcommit[i] <- nrow(do.call(rbind, xmlToList(as.character(testALL$foulcommit[i]))))
}

# number of fouls committed - home team against away team ====
for (i in 1: length(testALL$foulcommit)){
  testALL$HTfouls[i] <- nrow(as.data.frame(do.call(rbind, xmlToList(as.character(testALL$foulcommit[i])))) %>% 
                            filter(player1 == testALL$home_team_api_id[i] |
                                     sortorder == testALL$home_team_api_id[i] |
                                     team == testALL$home_team_api_id[i] |
                                     n == testALL$home_team_api_id[i]))
}

# number of fouls committed - away team against home team ====
for (i in 1: length(testALL$foulcommit)){
  testALL$ATfouls[i] <- nrow(as.data.frame(do.call(rbind, xmlToList(as.character(testALL$foulcommit[i])))) %>% 
                            filter(player1 == testALL$home_team_api_id[i] |
                                     sortorder == testALL$away_team_api_id[i] |
                                     team == testALL$away_team_api_id[i] |
                                     n == testALL$away_team_api_id[i]))
}

# number of cards given - both yellow and red for both teams, summed ====
for (i in 1:length(testALL$card)){
  testALL$ncard[i] <- nrow(do.call(rbind, xmlToList(as.character(testALL$card[i]))))
}

# number of home team cards (both yellow and red) ====
for (i in 1:length(testALL$card)){
  testALL$HTcard[i] <- nrow(as.data.frame(do.call(rbind, xmlToList(as.character(testALL$card[i])))) %>% 
                           filter(sortorder == testALL$home_team_api_id[i] |
                                    team == testALL$home_team_api_id[i] |
                                    n == testALL$home_team_api_id[i]))
}

# number of yellow cards for the home team ====
for (i in 1: length(testALL$card)){
  testALL$htYcard[i] <- nrow(as.data.frame(do.call(rbind, xmlToList(as.character(testALL$card[i])))) %>% 
                            filter(sortorder == testALL$home_team_api_id[i] |
                                     team == testALL$home_team_api_id[i] |
                                     n == testALL$home_team_api_id[i],
                                   comment == "y"))
}

# number of red cards for the home team ====
for (i in 1: length(testALL$card)){
  testALL$htRcard[i] <- nrow(as.data.frame(do.call(rbind, xmlToList(as.character(testALL$card[i])))) %>% 
                            filter(sortorder == testALL$home_team_api_id[i] |
                                     team == testALL$home_team_api_id[i] |
                                     n == testALL$home_team_api_id[i],
                                   comment == "r"))
}

# number of away team cards (both yellow and red) ====
for (i in 1:length(testALL$card)){
  testALL$ATcard[i] <- nrow(as.data.frame(do.call(rbind, xmlToList(as.character(testALL$card[i])))) %>% 
                           filter(sortorder == testALL$away_team_api_id[i] |
                                    team == testALL$away_team_api_id[i] |
                                    n == testALL$away_team_api_id[i]))
}

# number of yellow cards for the away team ====
for (i in 1: length(testALL$card)){
  testALL$atYcard[i] <- nrow(as.data.frame(do.call(rbind, xmlToList(as.character(testALL$card[i])))) %>% 
                            filter(sortorder == testALL$away_team_api_id[i] |
                                     team == testALL$away_team_api_id[i] |
                                     n == testALL$away_team_api_id[i],
                                   comment == "y"))
}

# number of red cards for the away team ====
for (i in 1: length(testALL$card)){
  testALL$atRcard[i] <- nrow(as.data.frame(do.call(rbind, xmlToList(as.character(testALL$card[i])))) %>% 
                            filter(sortorder == testALL$away_team_api_id[i] |
                                     team == testALL$away_team_api_id[i] |
                                     n == testALL$away_team_api_id[i],
                                   comment == "r"))
}

### number of crosses, both teams summed # take a while to run # cleaner, but still some bad formatting ====
for (i in 1:length(testALL$cross)){
  testALL$ncross[i] <- nrow(do.call(rbind, xmlToList(as.character(testALL$cross[i]))))
}

# number of crosses performed by home team ====
for (i in 1:length(testALL$cross)){
  testALL$HTcross[i] <- nrow(as.data.frame(do.call(rbind, xmlToList(as.character(testALL$cross[i])))) %>% 
                            filter(player1 == testALL$home_team_api_id[i] |
                                     sortorder == testALL$home_team_api_id[i] |
                                     team == testALL$home_team_api_id[i] |
                                     n == testALL$home_team_api_id[i]))
}

# number of crosses performed by away team ====
for (i in 1:length(testALL$cross)){
  testALL$ATcross[i] <- nrow(as.data.frame(do.call(rbind, xmlToList(as.character(testALL$cross[i])))) %>% 
                            filter(player1 == testALL$away_team_api_id[i] |
                                     sortorder == testALL$away_team_api_id[i] |
                                     team == testALL$away_team_api_id[i] |
                                     n == testALL$away_team_api_id[i]))
}

# number of corners, both teams summed ====
for (i in 1:length(testALL$corner)){
  testALL$ncorner[i] <- nrow(do.call(rbind, xmlToList(as.character(testALL$corner[i]))))
}

# number of corners performed by home team ====
for (i in 1:length(testALL$corner)){
  testALL$HTcorners[i] <- nrow(as.data.frame(do.call(rbind, xmlToList(as.character(testALL$corner[i])))) %>% 
                              filter(player1 == testALL$home_team_api_id[i] |
                                       sortorder == testALL$home_team_api_id[i] |
                                       team == testALL$home_team_api_id[i] |
                                       n == testALL$home_team_api_id[i]))
}

# number of corners performed by away team ====
for (i in 1:length(testALL$corner)){
  testALL$ATcorners[i] <- nrow(as.data.frame(do.call(rbind, xmlToList(as.character(testALL$corner[i])))) %>% 
                              filter(player1 == testALL$away_team_api_id[i] |
                                       sortorder == testALL$away_team_api_id[i] |
                                       team == testALL$away_team_api_id[i] |
                                       n == testALL$away_team_api_id[i]))
}

## I let this run overnight. It was running for 10 hours when I left for work. My guess is it took about 12 hours

# create goal differential variable ====
testALL$goaldiff <- testALL$home_team_goal - testALL$away_team_goal

# win binary variable ====

testALL %<>%
  mutate(win = ifelse(home_team_goal > away_team_goal, 1, 0))

# draw binary variable ====
testALL %<>%
  mutate(draw = ifelse(home_team_goal == away_team_goal, 1, 0))

## xx ##  
  
# use this code type to check individual XML cells/df's if necessary
do.call(rbind, xmlToList(as.character(testALL$possession[73])))

# play with regressions ====

sort(names(testALL))

testLinReg <- lm(goaldiff~HTcorners + HTcross + HTshoton +
                   ATcorners + ATcross + ATshoton +
                   htYcard + htRcard + atYcard + atRcard, data = testALL)

# this model shows us that HTshotoff, ATshotoff, HTfouls, and ATfouls are not significant at any meaningful levels
testLinReg <- lm(goaldiff~homePoss +
                   HTshoton + ATshoton + HTshotoff + ATshotoff +
                   HTfouls + ATfouls + HTcard + ATcard +
                   HTcross + ATcross + HTcorners + ATcorners, data = testALL)

# this model has variables all with signficance past the * level
# the R-squared values are low, but that makes sense given the unpredictability of humans
testLinReg <- lm(goaldiff~homePoss +
                   HTshoton + ATshoton +
                   HTcard + ATcard +
                   HTcross + ATcross + HTcorners + ATcorners, data = testALL)

summary(testLinReg)

# creating some plots will help show why the R-squared is low:
library(ggplot2)
testALL0809 <- testALL %>%
  filter(season == "2008/2009")

# plots ====
# maybe plots can explain the low R2?
# because the x and y axis points are all low number integers (y axis is about -5 to 7,
# and x axis is about 0 to 8)
ggplot(testALL0809, aes(HTcard, goaldiff)) + geom_point() + geom_jitter() + geom_smooth()
ggplot(testALL0809, aes(ATcard, goaldiff)) + geom_point() + geom_jitter() + geom_smooth()
# interesting, because more shots on for home team doesn't seem to help their goal differential
# maybe that's because we're plotting against goaldiff? let's try straight goals (below)
ggplot(testALL0809, aes(HTshoton, goaldiff)) + geom_point() + geom_jitter() + geom_smooth()
ggplot(testALL0809, aes(ATshoton, goaldiff)) + geom_point() + geom_jitter() + geom_smooth()
ggplot(testALL0809, aes(HTcross, goaldiff)) + geom_point() + geom_jitter() + geom_smooth()
ggplot(testALL0809, aes(ATcross, goaldiff)) + geom_point() + geom_jitter() + geom_smooth()

ggplot(testALL0809, aes(HTcard, home_team_goal)) + geom_point() + geom_jitter() + geom_smooth()
ggplot(testALL0809, aes(ATcard, home_team_goal)) + geom_point() + geom_jitter() + geom_smooth()
# try this:
# (still not an upward trend. rather, between 5 and 10 shots on seems to be best)
# perhaps it has to do with the quality of shots on?
ggplot(testALL0809, aes(HTshoton, home_team_goal)) + geom_point() + geom_jitter() + geom_smooth()
ggplot(testALL0809, aes(ATshotoff, home_team_goal)) + geom_point() + geom_jitter() + geom_smooth()
ggplot(testALL0809, aes(HTcross, home_team_goal)) + geom_point() + geom_jitter() + geom_smooth()
ggplot(testALL0809, aes(ATcross, home_team_goal)) + geom_point() + geom_jitter() + geom_smooth()

# plots for all years:
# (a few too many points; a little longer to run)
ggplot(testALL, aes(HTcard, goaldiff)) + geom_point() + geom_jitter() + geom_smooth()
ggplot(testALL, aes(ATcard, goaldiff)) + geom_point() + geom_jitter() + geom_smooth()
ggplot(testALL, aes(HTshoton, goaldiff)) + geom_point() + geom_jitter() + geom_smooth()
ggplot(testALL, aes(ATshoton, goaldiff)) + geom_point() + geom_jitter() + geom_smooth()
ggplot(testALL, aes(HTcross, goaldiff)) + geom_point() + geom_jitter() + geom_smooth()
ggplot(testALL, aes(ATcross, goaldiff)) + geom_point() + geom_jitter() + geom_smooth()

ggplot(testALL, aes(HTcard, home_team_goal)) + geom_point() + geom_jitter() + geom_smooth()
ggplot(testALL, aes(ATcard, home_team_goal)) + geom_point() + geom_jitter() + geom_smooth()
ggplot(testALL, aes(HTshoton, home_team_goal)) + geom_point() + geom_jitter() + geom_smooth()
ggplot(testALL, aes(ATshotoff, home_team_goal)) + geom_point() + geom_jitter() + geom_smooth()
ggplot(testALL, aes(HTcross, home_team_goal)) + geom_point() + geom_jitter() + geom_smooth()
ggplot(testALL, aes(ATcross, home_team_goal)) + geom_point() + geom_jitter() + geom_smooth()

# maybe they don't seem useful because there are multiple dots stacked on each
# other, and we need to do some sort of jitter to show how many teams are above
# goaldiff = 0 and below




ANLinReg <- lm(outcome~homeORaway, data = matchAN4)
summary(ANLinReg)

names(testALL)



