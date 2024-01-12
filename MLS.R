getwd()
setwd('/Users/kabirwagle/Downloads/Senior Project/Data')

#Reading in MLS Player Salary data 2007-2017
wages2007 = read.csv('mls-salaries-2007.csv')
wages2008 = read.csv('mls-salaries-2008.csv')
wages2009 = read.csv('mls-salaries-2009.csv')
wages2010 = read.csv('mls-salaries-2010.csv')
wages2011 = read.csv('mls-salaries-2011.csv')
wages2012 = read.csv('mls-salaries-2012.csv')
wages2013 = read.csv('mls-salaries-2013.csv')
wages2014 = read.csv('mls-salaries-2014.csv')
wages2015 = read.csv('mls-salaries-2015.csv')
wages2016 = read.csv('mls-salaries-2016.csv')
wages2017 = read.csv('mls-salaries-2017.csv')

#Viewing data to ensure column names are the same
head(wages2007)
head(wages2008)
head(wages2009)
head(wages2010)
head(wages2011)
head(wages2012)
head(wages2013)
head(wages2014)
head(wages2015)
head(wages2016)
head(wages2017)

#Checking for missing values
sum(is.na(wages2007))
sum(is.na(wages2008)) #2 missing values found
sum(is.na(wages2009))
sum(is.na(wages2010))
sum(is.na(wages2011))
sum(is.na(wages2012))
sum(is.na(wages2013))
sum(is.na(wages2014))
sum(is.na(wages2015))
sum(is.na(wages2016))
sum(is.na(wages2017))

#Filling the missing salary with his 2008 salary. 
wages2008[189,]$base_salary = 300000.00
wages2008[189,]$guaranteed_compensation = 300000.00

#Adding a year column to each 
wages2007$year = 2007
wages2008$year = 2008
wages2009$year = 2009
wages2010$year = 2010
wages2011$year = 2011
wages2012$year = 2012
wages2013$year = 2013
wages2014$year = 2014
wages2015$year = 2015
wages2016$year = 2016
wages2017$year = 2017

#Compiling all the data into one data frame
wagesMLS  = rbind(wages2007, wages2008, wages2009, wages2010, wages2011, wages2012, 
      wages2013, wages2014, wages2015, wages2016, wages2017)
head(wagesMLS)

#Cleaning club column
unique(wagesMLS$club)
wagesMLS = wagesMLS[wagesMLS$club != "Pool",]
wagesMLS = wagesMLS[wagesMLS$club != "POOL",]
wagesMLS = wagesMLS[wagesMLS$club != "None",]
wagesMLS[wagesMLS$club == "",]

#There are numerous entries missing clubs, I will now manually fill them in
wagesMLS[wagesMLS$club == "" & wagesMLS$last_name == "Sassano",]$club = "KC"
wagesMLS[wagesMLS$club == "" & wagesMLS$last_name == "White",]$club = "NE"
wagesMLS[wagesMLS$club == "" & wagesMLS$last_name == "Araujo Jr.",]$club = "VAN"
wagesMLS[wagesMLS$club == "" & wagesMLS$last_name == "Dunfield",]$club = "TFC"
wagesMLS[wagesMLS$club == "" & wagesMLS$last_name == "Muniz",]$club = "RSL"
wagesMLS[wagesMLS$club == "" & wagesMLS$last_name == "Borja",]$club = "CHV"
wagesMLS[wagesMLS$club == "" & wagesMLS$last_name == "Gardner",]$club = "KC"
wagesMLS[wagesMLS$club == "" & wagesMLS$last_name == "Melia",]$club = "CHV"
wagesMLS[wagesMLS$club == "" & wagesMLS$last_name == "Dike",]$club = "TFC"
wagesMLS[wagesMLS$club == "" & wagesMLS$last_name == "Hurtado",]$club = "CHI"
wagesMLS[wagesMLS$club == "" & wagesMLS$last_name == "Lochhead",]$club = "CHV"
wagesMLS[wagesMLS$club == "" & wagesMLS$last_name == "Tshuma",]$club = "POR"
wagesMLS[wagesMLS$club == "" & wagesMLS$last_name == "Gargan",]$club = "LA"
wagesMLS[wagesMLS$club == "" & wagesMLS$last_name == "Martinez",]$club = "COL"
wagesMLS[wagesMLS$club == "" & wagesMLS$last_name == "Moore",]$club = "TFC"
wagesMLS[wagesMLS$club == "" & wagesMLS$last_name == "Nyassi",]$club = "SJ"
wagesMLS[wagesMLS$club == "" & wagesMLS$last_name == "Ovalle",]$club = "RSL"
wagesMLS[wagesMLS$club == "" & wagesMLS$last_name == "Babouli",]$club = "TFC"
wagesMLS[wagesMLS$club == "",]$club = "ORL"
unique(wagesMLS$club) #Now the only club names are the actual clubs

#Final check for missing values
sum(is.na(wagesMLS$club))
sum(is.na(wagesMLS$last_name))
sum(is.na(wagesMLS$first_name))
sum(is.na(wagesMLS$position))
sum(is.na(wagesMLS$base_salary))
sum(is.na(wagesMLS$guaranteed_compensation))
sum(is.na(wagesMLS$year))
head(wagesMLS)

#Read in matches data
matches = read.csv('matches.csv')
head(matches)
ncol(matches)

#Get rid of unnecessary information
matches = matches[,1:15]
matches = matches[,c( "year", "league", "part_of_competition", "home", "away", 
           "home_score", "away_score")]
matches = matches[matches$year >= 2007 & matches$year <= 2017,]

#Check for missing values (more efficiently)
lapply(lapply(matches, is.na), sum)

#Splitting the data into regular and post season
unique(matches$part_of_competition)
regSeason = matches[grepl("Regular Season", matches$part_of_competition) == TRUE,]
postSeason = matches[grepl("Regular Season", matches$part_of_competition) == FALSE,]


#Calculate points for each team
regSeason$home_points = ifelse(regSeason$home_score > regSeason$away_score, 3, 1)
regSeason$home_points = ifelse(regSeason$home_score < regSeason$away_score, 0, regSeason$home_points)
regSeason$away_points = ifelse(regSeason$home_score < regSeason$away_score, 3, 1)
regSeason$away_points = ifelse(regSeason$home_score > regSeason$away_score, 0, regSeason$away_points)

#Splitting data into home and away
homePoints = regSeason[,c("year", "part_of_competition", "home", "home_score", "away_score", "home_points")]
awayPoints = regSeason[,c("year", "part_of_competition", "away", "home_score", "away_score", "away_points")]

#Calculating point totals by team
#I organized the data manually in order to match the salary data format
library(dplyr)
homePoints07 = homePoints[homePoints$year == '2007',]
homePoints08 = homePoints[homePoints$year == '2008',]
homePoints09 = homePoints[homePoints$year == '2009',]
homePoints10 = homePoints[homePoints$year == '2010',]
homePoints11 = homePoints[homePoints$year == '2011',]
homePoints12 = homePoints[homePoints$year == '2012',]
homePoints13 = homePoints[homePoints$year == '2013',]
homePoints14 = homePoints[homePoints$year == '2014',]
homePoints15 = homePoints[homePoints$year == '2015',]
homePoints16 = homePoints[homePoints$year == '2016',]
homePoints17 = homePoints[homePoints$year == '2017',]

homeTotal07 = data.frame(homePoints07 %>% group_by(home) %>% summarise(home_total = sum(home_points)))
homeTotal08 = data.frame(homePoints08 %>% group_by(home) %>% summarise(home_total = sum(home_points)))
homeTotal09 = data.frame(homePoints09 %>% group_by(home) %>% summarise(home_total = sum(home_points)))
homeTotal10 = data.frame(homePoints10 %>% group_by(home) %>% summarise(home_total = sum(home_points)))
homeTotal11 = data.frame(homePoints11 %>% group_by(home) %>% summarise(home_total = sum(home_points)))
homeTotal12 = data.frame(homePoints12 %>% group_by(home) %>% summarise(home_total = sum(home_points)))
homeTotal13 = data.frame(homePoints13 %>% group_by(home) %>% summarise(home_total = sum(home_points)))
homeTotal14 = data.frame(homePoints14 %>% group_by(home) %>% summarise(home_total = sum(home_points)))
homeTotal15 = data.frame(homePoints15 %>% group_by(home) %>% summarise(home_total = sum(home_points)))
homeTotal16 = data.frame(homePoints16 %>% group_by(home) %>% summarise(home_total = sum(home_points)))
homeTotal17 = data.frame(homePoints17 %>% group_by(home) %>% summarise(home_total = sum(home_points)))

awayPoints07 = awayPoints[awayPoints$year == '2007',]
awayPoints08 = awayPoints[awayPoints$year == '2008',]
awayPoints09 = awayPoints[awayPoints$year == '2009',]
awayPoints10 = awayPoints[awayPoints$year == '2010',]
awayPoints11 = awayPoints[awayPoints$year == '2011',]
awayPoints12 = awayPoints[awayPoints$year == '2012',]
awayPoints13 = awayPoints[awayPoints$year == '2013',]
awayPoints14 = awayPoints[awayPoints$year == '2014',]
awayPoints15 = awayPoints[awayPoints$year == '2015',]
awayPoints16 = awayPoints[awayPoints$year == '2016',]
awayPoints17 = awayPoints[awayPoints$year == '2017',]

awayTotal07 = data.frame(awayPoints07 %>% group_by(away) %>% summarise(away_total = sum(away_points)))
awayTotal08 = data.frame(awayPoints08 %>% group_by(away) %>% summarise(away_total = sum(away_points)))
awayTotal09 = data.frame(awayPoints09 %>% group_by(away) %>% summarise(away_total = sum(away_points)))
awayTotal10 = data.frame(awayPoints10 %>% group_by(away) %>% summarise(away_total = sum(away_points)))
awayTotal11 = data.frame(awayPoints11 %>% group_by(away) %>% summarise(away_total = sum(away_points)))
awayTotal12 = data.frame(awayPoints12 %>% group_by(away) %>% summarise(away_total = sum(away_points)))
awayTotal13 = data.frame(awayPoints13 %>% group_by(away) %>% summarise(away_total = sum(away_points)))
awayTotal14 = data.frame(awayPoints14 %>% group_by(away) %>% summarise(away_total = sum(away_points)))
awayTotal15 = data.frame(awayPoints15 %>% group_by(away) %>% summarise(away_total = sum(away_points)))
awayTotal16 = data.frame(awayPoints16 %>% group_by(away) %>% summarise(away_total = sum(away_points)))
awayTotal17 = data.frame(awayPoints17 %>% group_by(away) %>% summarise(away_total = sum(away_points)))

total07 = cbind(homeTotal07, awayTotal07)
total08 = cbind(homeTotal08, awayTotal08)
total09 = cbind(homeTotal09, awayTotal09)
total10 = cbind(homeTotal10, awayTotal10)
total11 = cbind(homeTotal11, awayTotal11)
total12 = cbind(homeTotal12, awayTotal12)
total13 = cbind(homeTotal13, awayTotal13)
total14 = cbind(homeTotal14, awayTotal14)
total15 = cbind(homeTotal15, awayTotal15)
total16 = cbind(homeTotal16, awayTotal16)
total17 = cbind(homeTotal17, awayTotal17)

total07$year = 2007
total08$year = 2008
total09$year = 2009
total10$year = 2010
total11$year = 2011
total12$year = 2012
total13$year = 2013
total14$year = 2014
total15$year = 2015
total16$year = 2016
total17$year = 2017

pointsMLS = rbind(total07, total08, total09, total10, total11, total12,
                 total13, total14, total15, total16, total17)

#Restructuring data
pointsMLS = pointsMLS[,c( "year", "home", "home_total", "away_total")]
pointsMLS$total_points = (pointsMLS$home_total + pointsMLS$away_total)

#Adding total games and points per game
pointsMLS$total_games = ifelse((pointsMLS$year) <= 2010, 30, 34)
pointsMLS$PPG = pointsMLS$total_points / pointsMLS$total_games
head(pointsMLS)

#Comparing data sets
head(pointsMLS)
head(wagesMLS)
wagesMLS = wagesMLS[,c( "year", "club", "last_name", "first_name", "position", 
                      "base_salary", "guaranteed_compensation")]
colnames(pointsMLS) = c( "year", "club", "home_total", "away_total", 
                         "total_points", "total_games", "PPG")
unique(pointsMLS$club)
unique(wagesMLS$club)

#Matching the team names
pointsMLS$club = ifelse(pointsMLS$club == "Chicago Fire FC", "CHI", pointsMLS$club)
pointsMLS$club = ifelse(pointsMLS$club == "Chivas USA", "CHV", pointsMLS$club)
pointsMLS$club = ifelse(pointsMLS$club == "Colorado Rapids", "CLB", pointsMLS$club)
pointsMLS$club = ifelse(pointsMLS$club == "Columbus Crew SC", "COL", pointsMLS$club)
pointsMLS$club = ifelse(pointsMLS$club == "FC Dallas", "DAL", pointsMLS$club)
pointsMLS$club = ifelse(pointsMLS$club == "DC United", "DC", pointsMLS$club)
pointsMLS$club = ifelse(pointsMLS$club == "Houston Dynamo", "HOU", pointsMLS$club)
pointsMLS$club = ifelse(pointsMLS$club == "Sporting Kansas City", "KC", pointsMLS$club)
pointsMLS$club = ifelse(pointsMLS$club == "LA Galaxy", "LA", pointsMLS$club)
pointsMLS$club = ifelse(pointsMLS$club == "New England Revolution", "NE", pointsMLS$club)
pointsMLS$club = ifelse(pointsMLS$club == "New York Red Bulls", "NYRB", pointsMLS$club)
pointsMLS$club = ifelse(pointsMLS$club == "Real Salt Lake", "RSL", pointsMLS$club)
pointsMLS$club = ifelse(pointsMLS$club == "Toronto FC", "TFC", pointsMLS$club)
pointsMLS$club = ifelse(pointsMLS$club == "San Jose Earthquakes", "SJ", pointsMLS$club)
pointsMLS$club = ifelse(pointsMLS$club == "Seattle Sounders FC", "SEA", pointsMLS$club)
pointsMLS$club = ifelse(pointsMLS$club == "Philadelphia Union", "PHI", pointsMLS$club)
pointsMLS$club = ifelse(pointsMLS$club == "Portland Timbers", "POR", pointsMLS$club)
pointsMLS$club = ifelse(pointsMLS$club == "Vancouver Whitecaps", "VAN", pointsMLS$club)
pointsMLS$club = ifelse(pointsMLS$club == "Montreal Impact", "MTL", pointsMLS$club)
pointsMLS$club = ifelse(pointsMLS$club == "New York City FC", "NYCFC", pointsMLS$club)
pointsMLS$club = ifelse(pointsMLS$club == "Orlando City SC", "ORL", pointsMLS$club)
pointsMLS$club = ifelse(pointsMLS$club == "Atlanta United FC", "ATL", pointsMLS$club)
pointsMLS$club = ifelse(pointsMLS$club == "Minnesota United FC" , "MNUFC", pointsMLS$club)
wagesMLS$club = ifelse(wagesMLS$club == "TOR", "TFC", wagesMLS$club)
wagesMLS$club = ifelse(wagesMLS$club == "NY", "NYRB", wagesMLS$club)

#Final checks
wagesMLS = wagesMLS[wagesMLS$club != "LAFC",] #Removed for only having 2 obs
unique(pointsMLS$club)
unique(wagesMLS$club)
head(pointsMLS)
head(wagesMLS)

#Gini Coefficient

library(DescTools)
library(rlist)
library(dineq)
library(hhi)


salaryMLS = wagesMLS[c(1, 2, 7)]
colnames(salaryMLS) = c('year', 'club', 'salary')
head(salaryMLS)

# Order data sets by year and alphabetically. 
pointsMLS = pointsMLS[order(pointsMLS$year, pointsMLS$club),]
salaryMLS = salaryMLS[order(salaryMLS$year, salaryMLS$club),]

#Removing missing years
salaryMLS = salaryMLS[salaryMLS$club != "SEA" | salaryMLS$year != 2008,]
salaryMLS = salaryMLS[salaryMLS$club != "NYCFC" | salaryMLS$year != 2014,]
salaryMLS = salaryMLS[salaryMLS$club != "ORL" | salaryMLS$year != 2014,]
salaryMLS = salaryMLS[salaryMLS$club != "CHV" | salaryMLS$year != 2015,]
salaryMLS = salaryMLS[salaryMLS$club != "ATL" | salaryMLS$year != 2016,]

count(unique(pointsMLS[1:2]))
count(unique(salaryMLS[1:2]))

#Create a new data frame containing the Gini coefficients for each team, for each year

#Gini_calculate
Gini_calculate <- function(salaries, club) {
  df = data.frame(1, 2, 3, 4, 5, 6, 7)
  colnames(df) = c('club', 'gini', 'numPlayers', 'totalSalaries','CV', 'thiel', 'hhi')
  df$club = club
  df$gini = Gini(salaries)
  df$numPlayers = length(salaries)
  df$totalSalaries = sum(salaries)
  df$CV = sd(salaries) / mean(salaries)
  df$thiel = theil.wtd(salaries)
  hhi = 0
  for (x in salaries) {
    hhi = (hhi + (10000 * (x / sum(salaries))^2) )
  }
  df$hhi = hhi
  return(df)
}

#Gini Function
giniMLS = data.frame(1, 2, 3, 4, 5, 6, 7)
colnames(giniMLS) = c('club', 'gini', 'numPlayers', 'totalSalaries', 'CV', 'thiel', 'hhi')
salary_list = c()
i = 1
club = 'CHI'
for (value in salaryMLS$club) {
  if (value == club) {
    salary_list = append(salary_list, salaryMLS[i,]$salary)
  } else {
    giniMLS = rbind(giniMLS, Gini_calculate(salary_list, club))
    club = value
    salary_list = c()
    salary_list = append(salary_list, salaryMLS[i,]$salary)
  }
  i = i + 1
} 
giniMLS = rbind(giniMLS, Gini_calculate(salary_list, club))

giniMLS
giniMLS = giniMLS[-c(1),]
MLS = cbind(pointsMLS, giniMLS)
MLS #Final Data Set

#Adding start year
head(MLS)
MLS$start_year = 1996
MLS$start_year = ifelse(MLS$club == 'CHI', 1998, MLS$start_year)
MLS$start_year = ifelse(MLS$club == 'CHV', 2005, MLS$start_year)
MLS$start_year = ifelse(MLS$club == 'CLB', 1996, MLS$start_year)
MLS$start_year = ifelse(MLS$club == 'COL', 1996, MLS$start_year)
MLS$start_year = ifelse(MLS$club == 'DAL', 1996, MLS$start_year)
MLS$start_year = ifelse(MLS$club == 'DC', 1996, MLS$start_year)
MLS$start_year = ifelse(MLS$club == 'HOU', 2006, MLS$start_year)
MLS$start_year = ifelse(MLS$club == 'KC', 1996, MLS$start_year)
MLS$start_year = ifelse(MLS$club == 'LA', 1996, MLS$start_year)
MLS$start_year = ifelse(MLS$club == 'NE', 1996, MLS$start_year)
MLS$start_year = ifelse(MLS$club == 'NYRB', 1996, MLS$start_year)
MLS$start_year = ifelse(MLS$club == 'RSL', 2005, MLS$start_year)
MLS$start_year = ifelse(MLS$club == 'TFC', 2007, MLS$start_year)
MLS$start_year = ifelse(MLS$club == 'SJ', 1996, MLS$start_year)
MLS$start_year = ifelse(MLS$club == 'SEA', 2009, MLS$start_year)
MLS$start_year = ifelse(MLS$club == 'PHI', 2010, MLS$start_year)
MLS$start_year = ifelse(MLS$club == 'POR', 2011, MLS$start_year)
MLS$start_year = ifelse(MLS$club == 'VAN', 2011, MLS$start_year)
MLS$start_year = ifelse(MLS$club == 'MTL', 2012, MLS$start_year)
MLS$start_year = ifelse(MLS$club == 'NFCFC', 2015, MLS$start_year)
MLS$start_year = ifelse(MLS$club == 'ORL', 2015, MLS$start_year)
MLS$start_year = ifelse(MLS$club == 'ATL', 2017, MLS$start_year)
MLS$start_year = ifelse(MLS$club == 'MNUFC', 2017, MLS$start_year)

MLS$years_in_MLS = MLS$year - MLS$start_year
head(MLS)

#Adding salary averages
MLS$leagueAVG = 0
MLS$leagueAVG =  ifelse(MLS$year == 2007, mean(MLS[MLS$year == 2007,]$totalSalaries), MLS$leagueAVG)
MLS$leagueAVG =  ifelse(MLS$year == 2008, mean(MLS[MLS$year == 2008,]$totalSalaries), MLS$leagueAVG)
MLS$leagueAVG =  ifelse(MLS$year == 2009, mean(MLS[MLS$year == 2009,]$totalSalaries), MLS$leagueAVG)
MLS$leagueAVG =  ifelse(MLS$year == 2010, mean(MLS[MLS$year == 2010,]$totalSalaries), MLS$leagueAVG)
MLS$leagueAVG =  ifelse(MLS$year == 2011, mean(MLS[MLS$year == 2011,]$totalSalaries), MLS$leagueAVG)
MLS$leagueAVG =  ifelse(MLS$year == 2012, mean(MLS[MLS$year == 2012,]$totalSalaries), MLS$leagueAVG)
MLS$leagueAVG =  ifelse(MLS$year == 2013, mean(MLS[MLS$year == 2013,]$totalSalaries), MLS$leagueAVG)
MLS$leagueAVG =  ifelse(MLS$year == 2014, mean(MLS[MLS$year == 2014,]$totalSalaries), MLS$leagueAVG)
MLS$leagueAVG =  ifelse(MLS$year == 2015, mean(MLS[MLS$year == 2015,]$totalSalaries), MLS$leagueAVG)
MLS$leagueAVG =  ifelse(MLS$year == 2016, mean(MLS[MLS$year == 2016,]$totalSalaries), MLS$leagueAVG)
MLS$leagueAVG =  ifelse(MLS$year == 2017, mean(MLS[MLS$year == 2017,]$totalSalaries), MLS$leagueAVG)

MLS$relative_salary = MLS$totalSalaries / MLS$leagueAVG
head(MLS)

MLS = MLS[,c(1, 2, 7, 9, 10, 11, 12, 13, 14, 16, 18)]
head(MLS)
colnames(MLS) = c('year', 'club', 'points_per_game', 'gini', 'squad_size', 
              'total_salary', "cv", 'thiel', 'hhi', "years_in_MLS", 'relative_salary')
head(MLS)
new_order = c('year', 'club', 'points_per_game', 'gini', 'cv', 'thiel', 'hhi',
              'total_salary', 'relative_salary', 'squad_size', 'years_in_MLS')

MLS = MLS[, new_order]
head(MLS)

#Statistical Analysis

#Summary Statistics
library(skimr) 
MLSx = MLS[,c(1, 2, 4, 5, 6, 7)]
skim(MLSx)
MLSy = MLS[,c(1, 2, 3)]
skim(MLSy)
MLSc = MLS[,c(1, 2, 9, 10, 11)]
skim(MLSc)

#Scatterplots
library(ggplot2)
library(ggthemes)
head(MLS)
plot(points_per_game ~ gini,data = MLS)
plot(points_per_game ~ cv,data = MLS)
plot(points_per_game ~ thiel,data = MLS)
plot(points_per_game ~ hhi,data = MLS)

ggplot(data=MLS, aes(x=gini, y=points_per_game)) + 
  geom_point(color = 'black') +
  geom_smooth(method = "loess", color = 'blue') + 
  ggtitle("Scatterplot of Gini Coefficient vs. Points Per Game\n") +
  labs(x="Gini Coefficient",y="Points Per Game\n") + 
  theme(plot.title=element_text(family='', face='bold', colour='black', size=7))

ggplot(data=MLS, aes(x=cv, y=points_per_game)) + 
  geom_point(color = 'black') +
  geom_smooth(method = "loess", color = 'blue') + 
  ggtitle("Scatterplot of CV vs. Points Per Game\n") +
  labs(x="Coefficient of Variation",y="Points Per Game\n") + 
  theme(plot.title=element_text(family='', face='bold', colour='black', size=7))

ggplot(data=MLS, aes(x=thiel, y=points_per_game)) + 
  geom_point(color = 'black') +
  geom_smooth(method = "loess", color = 'blue') + 
  ggtitle("Scatterplot of Thiel Index vs. Points Per Game\n") +
  labs(x="Thiel Index",y="Points Per Game\n") + 
  theme(plot.title=element_text(family='', face='bold', colour='black', size=7))

ggplot(data=MLS, aes(x=hhi, y=points_per_game)) + 
  geom_point(color = 'black') +
  geom_smooth(method = "loess", color = 'blue') + 
  ggtitle("Scatterplot of HHI Index vs. Points Per Game\n") +
  labs(x="HHI Index",y="Points Per Game\n") + 
  theme(plot.title=element_text(family='', face='bold', colour='black', size=7))

plot(data = MLS, relative_salary ~ gini)
ggplot(data=MLS, aes(x=gini, y=relative_salary)) + 
  geom_point(color = 'black') +
  ggtitle("Scatterplot of Gini Coefficient vs. Relative Salary\n") +
  labs(x="Gini Coefficient",y="Relative Salary\n") + 
  theme(plot.title=element_text(family='', face='bold', colour='black', size=18))

#Fitting the model
head(MLS)
fit1 = lm(points_per_game ~ gini, data = MLS)
summary(fit1)

fit2 = lm(points_per_game ~ gini + year + relative_salary +
            squad_size + years_in_MLS, data = MLS)
summary(fit2)

fit2a = lm(points_per_game ~ cv + year + relative_salary +
            squad_size + years_in_MLS, data = MLS)
summary(fit2a)

fit2b = lm(points_per_game ~ thiel + year + relative_salary +
             squad_size + years_in_MLS, data = MLS)
summary(fit2b)

#MLS$hhi = MLS$hhi / 1000
fit2c = lm(points_per_game ~ hhi + year + relative_salary +
             squad_size + years_in_MLS, data = MLS)
summary(fit2c)


fit3 = lm((points_per_game^2) ~ gini + year + log(relative_salary) +
            squad_size + years_in_MLS, data = MLS)
summary(fit3)

fit3a = lm((points_per_game^2) ~ cv + year + log(relative_salary) +
            squad_size + years_in_MLS, data = MLS)
summary(fit3a)

fit3b = lm((points_per_game^2) ~ thiel + year + log(relative_salary) +
            squad_size + years_in_MLS, data = MLS)
summary(fit3b)

fit3c = lm((points_per_game^2) ~ hhi + year + log(relative_salary) +
             squad_size + years_in_MLS, data = MLS)
summary(fit3c)

fit4 = lm(points_per_game ~ gini + log(relative_salary) +
            + years_in_MLS, data = MLS)
summary(fit4)

fit4a = lm(points_per_game ~ cv + log(relative_salary) +
             + years_in_MLS, data = MLS)
summary(fit4a)

fit4b = lm(points_per_game ~ thiel + log(relative_salary) +
             + years_in_MLS, data = MLS)
summary(fit4b)

fit4c = lm(points_per_game ~ hhi + log(relative_salary) +
             + years_in_MLS, data = MLS)
summary(fit4c)



  
