#Data Validation for data set per game (Santiago)


#Verify for NAs
sum(is.na(data_per_game))

#Check for possible typos on the league names
unique(data_per_game$league)

#Check for possible typos on the year the season began
unique(data_per_game$year)

#Check for possible typos on the home/away variable
unique(data_per_game$stadium)

#Summary of the expected goals + checking the extreme values
summary(data_per_game$exp_goal)
View(filter(data_per_game, exp_goal > 6))

#Summary of the no_pen_exp_goal variable
summary(data_per_game$no_pen_exp_goal)

#Summary of the deep variable
summary(data_per_game$deep)

#Summary of the goals scored + checking the extreme values
summary(data_per_game$scored)
View(filter(data_per_game, scored > 8))
#The values were verified to be correct

#Summary of the expected points
summary(data_per_game$exp_pts)

#Check for possible typos on the result variable
unique(data_per_game$result)

#Summary of the date
summary(data_per_game$date)

#Verifying there is no overcounting on wins
unique(data_per_game$wins)

#Verifying there is no overcounting on draws
unique(data_per_game$draws)

#Verifying there is no overcounting on loses
unique(data_per_game$loses)

#Verifying there is no apparent error on points
unique(data_per_game$pts)

#Summary of the no_pen_exp_goal_diff variable
summary(data_per_game$no_pen_exp_goal_diff)

#Summary of the ppda_coef variable
summary(data_per_game$ppda_coef)

#Verifying there is no typos in the names of the teams
sort(unique(data_per_game$team))

#Summary of the exp_goal_diff variable
summary(data_per_game$exp_goal_diff)

#Summary of the exp_pts_diff variable 
summary(data_per_game$exp_pts_diff)

#Verifying there is the correct number of teams for each league's seasons
data_per_game |> 
  select(league, year, team) |> 
  unique() |> 
  group_by(league, year) |> 
  summarize(number_of_teams = n()) |> 
View()

#Verifying the information is complete (all the matches of all seasons)
data_per_game |> 
  group_by(league, year) |> 
  summarize(number_of_matches = n()/2) |> 
View()
#We can see that there are 5 matches missing for the 2019/2020 Serie A.
#These matches were played on 02/08/20, and the dataset is updated until 01/08/20
#It's decided to keep the dashboard with this missing information, 
#awaiting for a future update to include the 5 extra matches.

#Verifying that no_pen_exp_goal is not greater than exp_goal
data_per_game |> 
  filter(no_pen_exp_goal > exp_goal) |> 
View()

#Verifying that no_pen_exp_goal_against is not greater than exp_goal_against
data_per_game |> 
  filter(no_pen_exp_goal_against > exp_goal_against) |> 
View()

#Verifying coherence between scores, results and points
data_per_game |> 
  filter(wins == 1, scored <= conceded) |> 
View()

data_per_game |> 
  filter(draws == 1, scored != conceded) |> 
View()

data_per_game |> 
  filter(loses == 1, scored >= conceded) |> 
View()

data_per_game |> 
  filter(pts != wins*3 + draws*1) |> 
View()

#Verifying the values of no_pen_exp_goal_diff
data_per_game |> 
  filter(no_pen_exp_goal_diff - (no_pen_exp_goal - no_pen_exp_goal_against) > 0.01) |> 
View()

#Verifying the values of exp_goal_diff
data_per_game |> 
  filter(exp_goal_diff - (exp_goal - scored) > 0.01) |> 
View()

#Verifying the values of exp_goal_against_diff
data_per_game |> 
  filter(exp_goal_against_diff - (exp_goal_against - conceded) > 0.01) |> 
View()

#Verifying the values of exp_pts_diff
data_per_game |> 
  filter(exp_pts_diff - (exp_pts - as.numeric(as.character(pts))) > 0.01) |> 
View()



#Data Validation for data set per club


#Verify for NAs
sum(is.na(data_per_club))

##Check for possible typos on the leagues' names
unique(data_per_club$league)

##Check for possible typos on the year
unique(data_per_club$year)

##Check for possible typos on the finishing position
unique(data_per_club$position)

##Check for possible typos on the teams' names
sort(unique(data_per_club$team))

##Check for possible errors on the number of matches played
unique(data_per_club$matches)
View(table(data_per_club$matches))
#The Bundesliga has 18 teams for a total of 34 matches per team.
#The other leagues have 20 teams, for a total of 38 matches per team.
#The 37 value accounts for the 10 teams of the 5 missing matches from 02/08/20
#The 27 and 28 values correspond to the 2019/20 Ligue 1 season, which did not play
#all the matches due to covid

#Summary of matches won
summary(data_per_club$wins)

#Summary of matches drawn
summary(data_per_club$draws)

#Summary of matches lost
summary(data_per_club$loses)

#Summary of scored goals
summary(data_per_club$scored)

#Summary of conceded goals
summary(data_per_club$conceded)

#Summary of pts
summary(data_per_club$pts)

#Summary of expected goals
summary(data_per_club$exp_goal)

#Summary of the exp_goal_diff variable
summary(data_per_club$exp_goal_diff)

#Summary of expected goals conceded
summary(data_per_club$exp_goal_against)

#Summary of the exp_goal_against_diff variable
summary(data_per_club$exp_goal_against_diff)

#Summary of the no_pen_exp_goal variable
summary(data_per_club$no_pen_exp_goal)

#Summary of the no_pen_exp_goal_against variable
summary(data_per_club$no_pen_exp_goal_against)

#Summary of the no_pen_exp_goal_diff variable
summary(data_per_club$no_pen_exp_goal_diff)

#Summary of the ppda_coef variable
summary(data_per_club$ppda_coef)

#Summary of the oppda_coef variable
summary(data_per_club$oppda_coef)

#Summary of the deep variable
summary(data_per_club$deep)

#Summary of the deep_allowed variable
summary(data_per_club$deep_allowed)

#Summary of the expected pts
summary(data_per_club$exp_pts)

#Summary of the exp_pts_diff variable
summary(data_per_club$exp_pts_diff)


#Verify there is no repeated position in a given season for any league
data_per_club |> 
  group_by(league, year, position) |> 
  summarise(frequency = n()) |> 
  filter(frequency != 1) |> 
View()

#Verify the number of teams that has a determined number of matches in a season
data_per_club |> 
  group_by(league, year, matches) |> 
  summarise(frequency = n()) |> 
View()
#The Bundesliga has 18 teams for a total of 34 matches per team.
#The other leagues have 20 teams, for a total of 38 matches per team.
#The 37 value accounts for the 10 teams of the 5 missing matches from 02/08/20
#The 27 and 28 values correspond to the 2019/20 Ligue 1 season, which did not play
#all the matches due to covid

#Verify that all the matches are counted in a result outcome
data_per_club |> 
  filter(matches - (wins + draws + loses) > 0.01) |> 
View()

#Verify the points obtained are calculated properly
data_per_club |> 
  filter(pts - (wins*3 + draws*1) > 0.01) |> 
View()

#Verifying the values of exp_goal_diff
data_per_club |> 
  filter(exp_goal_diff - (exp_goal - scored) > 0.01) |> 
View()

#Verifying the values of exp_goal_against_diff
data_per_club |> 
  filter(exp_goal_against_diff - (exp_goal_against - conceded) > 0.01) |> 
View()

#Verifying that no_pen_exp_goal is not greater than exp_goal
data_per_club |> 
  filter(no_pen_exp_goal > exp_goal) |> 
View()

#Verifying that no_pen_exp_goal_against is not greater than exp_goal_against
data_per_club |> 
  filter(no_pen_exp_goal_against > exp_goal_against) |> 
View()

#Verifying the values of no_pen_exp_goal_diff
data_per_club |> 
  filter(no_pen_exp_goal_diff - (no_pen_exp_goal - no_pen_exp_goal_against) > 0.01) |> 
View()

#Verifying the values of exp_pts_diff
data_per_club |> 
  filter(exp_pts_diff - (exp_pts - pts) > 0.01) |> 
View()
