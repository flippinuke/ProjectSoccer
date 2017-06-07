# prepare data for regs, play ====
# bringing in additional variables to test on

names(match)

# the data that is brought in, we will want to add to the matchAN variables
# including XML data, except for "goal"

# decrease the data set size at first ====
library(dplyr)
matchAD1 <- match %>% 
  filter(season == "2011/2012") %>%
  select(country_id, league_id, season, stage, date, match_api_id,
         home_team_api_id, away_team_api_id, home_team_goal, away_team_goal,
         shoton, shotoff, foulcommit, card, cross, corner, possession)

library(XML) # need this for xmlToList
library(plyr) # after done, use this to unload plyr: detach("package:vegan", unload=TRUE)
# need plyr for our thing to work below


# but first, lets inspect the various XML variables, and see what we want
# shoton, shotff, foulcommit, card, cross, corner, possession
# let's start with possession

head(matchAD1[order(match$possession),])

# create test df ====
test <- matchAD1

# remove rows that don't have data (consider imputation later) ====

# removes all rows in matchAD1$possession with NA
test <- test[complete.cases(test$possession),]

# get rid of rows where test$possession contains only "<possession />"
test <- test[!(test$possession == "<possession />"),]

# and get rid of rows where test$possession contains only "<possession />"
test <- test[!(test$card == "<card />"),] # removes 40 more rows

# this works. don't touch it
# gives length of possession by during game for home team (by percentage of game)
for (i in 1:length(test$possession)){
  test$homePoss[i] <- as.character(as.data.frame(do.call(rbind, xmlToList(as.character(test$possession[i])))) %>%
                                     filter(row_number() == n()) %>% 
                                     select(homepos))
}

# this shows us that 80 of the variables from above were brought in incorrectly
test$homePoss[which(nchar(test$homePoss) > 10)]
# 1. remove excess characters
# 2. replace incorrect variables with proper ones

# 1. remove excess characters:
test$homePoss <- as.integer(substr(test$homePoss, start = 7, stop = 8))

# 2. replace incorrect variables
# note: incorrect character identified by running test$homePoss BEFORE excess characters were removed:
# rows:



nchar(test$homePoss[2])


ifelse(nchar(test$homePoss) == 11, print)

# d version to strip excess text:
library(stringr)
test1 <- test
test1 <- test1 %>% mutate(homePoss = str_replace_all(test1$homePoss, "list", ""))
# seems to work. still need to remove the (" before and ") after: (this seems to work much better)
test1$homePoss <- as.integer(substr(test1$homePoss, start = 3, stop = 4))
head(test1$homePoss)

# and then strip the excess text
head(as.integer(substr(test$homePoss, start = 7, stop = 8)), n = 66)

# it produces error message - but it still adds the column we want
# next -
# (1) inspect other xml cells
# (2) create for loop for other xml cells
# (3) fix by hand or excel those cells
# at this point, there are about 80-100 errors that we can fix by hand


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

# number of cards given -both yellow and red for both teams, summed
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

# maybe
# str_extract_all(test$homePoss, simplify = TRUE)

# D version ====
for (i in 1:length(test$possession)){
  temp[i] <- as.character(as.data.frame(do.call(rbind, xmlToList(as.character(test$possession[i])))) %>%
                                     filter(row_number() == n()) %>% 
                                     select(homepos))
  if(i == 1){out <- temp}
  else{out <- rbind(out, temp)}
}

length(test$possession)
head(out)
#

# maybe apply this thinking to the above.
# that way we put all variables into one loop that we run for like an hour
test3 <- test2 %>% 
  mutate(homePoss <- ldply(xmlToList(as.character(test2$possession)), data.frame %>% 
                             filter(row_number() == n()) %>% 
                             select(homepos)),
         awayPoss<- ldply(xmlToList(as.character(test2$possession)), data.frame %>% 
                            filter(row_number() == n()) %>% 
                            select(awaypos)))
names(test3)

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