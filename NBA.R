setwd("C:/Users/abhi1/OneDrive/Documents/NCSUSAC/NBA")

#Installed libraries
#install.packages("rvest")
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("hablar")
#install.packages("janitor")
#install.packages("tidyverse")
#install.packages('xml2')
#install.packages('sqldf')
#install.packages('ggrepel')
#install.packages('Hmisc')
#install.packages("ggthemes")
#devtools::install_github("abresler/nbastatR", force = TRUE)
#install.packages("grid")
#install.packages("png")
#install.packages("jpeg")
#install.packages("patchwork")



#Included libraries
library('xml2')
library(rvest)
library(dplyr)
library(ggplot2)
library(hablar)
library(janitor)
library(tidyverse)
library(sqldf)
library(ggrepel)
library(Hmisc)
library(ggthemes)
library(nbastatR)
library(png)
library(grid)
library("jpeg")
library("patchwork")

#NBA Season Stats
Season_Stats <- "https://www.basketball-reference.com/leagues/NBA_2022_per_game.html"

url <- Season_Stats
pageobj <- read_html(url, as.data.frame=T, stringsAsFactors = TRUE)

pageobj %>%  
  html_nodes("table") %>% 
  .[[1]] %>% 
  html_table(fill=T) -> Season_Stats

Season_Stats <- row_to_names(Season_Stats, 0)

Season_Stats <- clean_names(Season_Stats)
names(Season_Stats)

colnames(Season_Stats) <- c('ID', 'Player', 'Pos', 'Age', 'Team', 
                           'GP', 'GS', 'MPG', 'FG', 'FGA', 'FGPct', '3P',
                           '3PA', '3PPct', '2P', '2PA','2PPct', 'EFGPct',
                           'FT', 'FTA', 'FTPct', 'ORB', 'DRB', 'TRB',
                           'AST', 'STL', 'BLK', 'TOV', 'PF', 'PPG')

Season_Stats<-Season_Stats[!(Season_Stats$ID=="Rk")  , ]

i <- c(1, 4, 6:30) 

Season_Stats[ , i] <- apply(Season_Stats[ , i], 2,            # Specify own function within apply
                    function(x) as.numeric(as.character(x)))
Season_Stats
max(Season_Stats$PPG)

Season_Stats
view(Season_Stats)

Position_Count <- Season_Stats %>% group_by(Pos) %>% tally() %>% filter()
view(Position_Count)

Age_Count <- Season_Stats %>% group_by(Age) %>% tally() %>% filter()
view(Age_Count)

Team_Size <- Season_Stats %>% group_by(Team) %>% tally() %>% filter()
view(Team_Size)

Advanced_Stats <- sqldf("SELECT Player, Pos, Age, Team, MPG, AST, TRB, PPG,
                         (AST + TRB + PPG) AS Totals, 
                         (PPG/(2*(FGA +.44 *FTA))) AS TSPct
                         FROM Season_Stats WHERE GP > 10 ORDER BY (AST + TRB + PPG) DESC;")
view(Advanced_Stats)

AS_Leaders <- head(Advanced_Stats, 60)
view(AS_Leaders)

options(ggrepel.max.overlaps = Inf)

Efficiency <- ggplot(AS_Leaders,aes(x = Totals, y = TSPct, color = Team)) + 
  geom_point(position = "jitter") +
  geom_label_repel(aes(label = Player, fill = Team, color = Team),
                   color = "white", size = 2, fontface = "bold") +
  labs(title = "NBA Player Efficiency 2021-2022 Season", 
       x = "Combined Points/Rebounds/Assists", 
       y = "True Shooting Percentage", 
       caption = "Data Viz by Abhi Joshi | Data from SportsReference | 10/19/21-1/5/22") +
  theme_dark() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        legend.position = "none") +
  scale_fill_manual(values=c("ATL" = "#E03A3E", "BOS" = "#007A33", "BRK" = "#000000", 
                             "CHI" = "#CE1141", "CHO" = "#1D1160", "CLE" = "#860038", 
                             "DAL" = "#00538C", "DEN" = "#0E2240", "DET" = "#C8102E", 
                             "GSW" = "#FFC72C", "HOU" = "#CE1141", "IND" = "#FDBB30", 
                             "LAC" = "#C8102E", "LAL" = "#552583", "MEM" = "#5D76A9",
                             "MIA" = "#98002E", "MIL" = "#00471B", "MIN" = "#236192", 
                             "NOP" = "#0C2340", "NYK" = "#F58426", "OKC" = "#007AC1",
                             "ORL" = "#0077C0", "PHI" = "#ED174C", "PHO" = "#E56020", 
                             "POR" = "#000000", "SAC" = "#63727A", "SAS" = "#C4CED4", 
                             "TOR" = "#CE1141", "UTA" = "#002B5C", "WAS" = "#002B5C")) +
  scale_color_manual(values=c("ATL" = "#E03A3E", "BOS" = "#007A33", "BRK" = "#000000", 
                              "CHI" = "#CE1141", "CHO" = "#1D1160", "CLE" = "#860038", 
                              "DAL" = "#00538C", "DEN" = "#0E2240", "DET" = "#C8102E", 
                              "GSW" = "#FFC72C", "HOU" = "#CE1141", "IND" = "#FDBB30", 
                              "LAC" = "#C8102E", "LAL" = "#552583", "MEM" = "#5D76A9",
                              "MIA" = "#98002E", "MIL" = "#00471B", "MIN" = "#236192", 
                              "NOP" = "#0C2340", "NYK" = "#F58426", "OKC" = "#007AC1",
                              "ORL" = "#0077C0", "PHI" = "#ED174C", "PHO" = "#E56020", 
                              "POR" = "#000000", "SAC" = "#63727A", "SAS" = "#C4CED4", 
                              "TOR" = "#CE1141", "UTA" = "#002B5C", "WAS" = "#002B5C"))

Efficiency

#ggsave(filename = 'NBA_Player_Efficiency_1-5-22.png', plot = Efficiency, width=7.5, height=5, units="in", dpi=300)

Shooting_Stats <- sqldf("SELECT Player, Pos, Age, Team, GP, MPG, FG, FGA, FGPct, 
                        `3P`, `3PA`,  `3PPct`, `2P`, `2PA`, `2PPct`, EFGPct, FT, FTA,
                         FTPct, PPG, (PPG/(2*(FGA +.44 *FTA))) AS TSPct
                         FROM Season_Stats WHERE (GP >= 10) & (`3P` >= 1.5) & (`3PPct` >= .350)
                         ORDER BY `3PPct` DESC;")
view(Shooting_Stats)

Shooting_Leaders <- head(Shooting_Stats, 50)
view(Shooting_Leaders)

options(ggrepel.max.overlaps = Inf)

Shooting <- ggplot(Shooting_Stats,aes(y = `3PPct`, x = `3PA`, color = Team)) + 
  geom_point(position = "jitter") +
  geom_label_repel(aes(label = Player, fill = Team, color = Team),
                   color = "white", size = 2, fontface = "bold") +
  labs(title = "Top Long-Distance Shooters 2021-2022 Season", 
       y = "Three Point Percentage", 
       x = "Three Point Attempts", 
       caption = "Data Viz by Abhi Joshi | Data from SportsReference | 10/19/21-1/5/22") +
  theme_dark() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        legend.position = "none") +
  scale_fill_manual(values=c("ATL" = "#E03A3E", "BOS" = "#007A33", "BRK" = "#000000", 
                             "CHI" = "#CE1141", "CHO" = "#1D1160", "CLE" = "#860038", 
                             "DAL" = "#00538C", "DEN" = "#0E2240", "DET" = "#C8102E", 
                             "GSW" = "#FFC72C", "HOU" = "#CE1141", "IND" = "#FDBB30", 
                             "LAC" = "#C8102E", "LAL" = "#552583", "MEM" = "#5D76A9",
                             "MIA" = "#98002E", "MIL" = "#00471B", "MIN" = "#236192", 
                             "NOP" = "#0C2340", "NYK" = "#F58426", "OKC" = "#007AC1",
                             "ORL" = "#0077C0", "PHI" = "#ED174C", "PHO" = "#E56020", 
                             "POR" = "#000000", "SAC" = "#63727A", "SAS" = "#C4CED4", 
                             "TOR" = "#CE1141", "UTA" = "#002B5C", "WAS" = "#002B5C")) +
  scale_color_manual(values=c("ATL" = "#E03A3E", "BOS" = "#007A33", "BRK" = "#000000", 
                              "CHI" = "#CE1141", "CHO" = "#1D1160", "CLE" = "#860038", 
                              "DAL" = "#00538C", "DEN" = "#0E2240", "DET" = "#C8102E", 
                              "GSW" = "#FFC72C", "HOU" = "#CE1141", "IND" = "#FDBB30", 
                              "LAC" = "#C8102E", "LAL" = "#552583", "MEM" = "#5D76A9",
                              "MIA" = "#98002E", "MIL" = "#00471B", "MIN" = "#236192", 
                              "NOP" = "#0C2340", "NYK" = "#F58426", "OKC" = "#007AC1",
                              "ORL" = "#0077C0", "PHI" = "#ED174C", "PHO" = "#E56020", 
                              "POR" = "#000000", "SAC" = "#63727A", "SAS" = "#C4CED4", 
                              "TOR" = "#CE1141", "UTA" = "#002B5C", "WAS" = "#002B5C"))

Shooting

#ggsave(filename = 'NBA_3Point_Shooting_Leaders_1-5-22.png', plot = Shooting, width=7.5, height=5, units="in", dpi=300)

Scoring_Stats <- Season_Stats %>% arrange(., desc(Season_Stats$PPG), )
view(Scoring_Stats)

Scoring_Leaders <- head(sqldf("SELECT * FROM Scoring_Stats WHERE GP >= 10"),n=50)
view(Scoring_Leaders)

Scoring <- ggplot(Scoring_Leaders,aes(x = PPG, y = EFGPct, color = Team)) + 
  geom_point(position = "jitter") +
  geom_label_repel(aes(label = Player, fill = Team, color = Team),
                   color = "white", size = 2, fontface = "bold") +
  labs(title = "Top Scorers 2021-2022 Season", 
       x = "Points Per Game", 
       y = "Effective Field Goal Percentage", 
       caption = "Data Viz by Abhi Joshi | Data from SportsReference | 10/19/21-1/5/22") +
  theme_dark() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        legend.position = "none") +
  scale_fill_manual(values=c("ATL" = "#E03A3E", "BOS" = "#007A33", "BRK" = "#000000", 
                             "CHI" = "#CE1141", "CHO" = "#1D1160", "CLE" = "#860038", 
                             "DAL" = "#00538C", "DEN" = "#0E2240", "DET" = "#C8102E", 
                             "GSW" = "#FFC72C", "HOU" = "#CE1141", "IND" = "#FDBB30", 
                             "LAC" = "#C8102E", "LAL" = "#552583", "MEM" = "#5D76A9",
                             "MIA" = "#98002E", "MIL" = "#00471B", "MIN" = "#236192", 
                             "NOP" = "#0C2340", "NYK" = "#F58426", "OKC" = "#007AC1",
                             "ORL" = "#0077C0", "PHI" = "#ED174C", "PHO" = "#E56020", 
                             "POR" = "#000000", "SAC" = "#63727A", "SAS" = "#C4CED4", 
                             "TOR" = "#CE1141", "UTA" = "#002B5C", "WAS" = "#002B5C")) +
  scale_color_manual(values=c("ATL" = "#E03A3E", "BOS" = "#007A33", "BRK" = "#000000", 
                              "CHI" = "#CE1141", "CHO" = "#1D1160", "CLE" = "#860038", 
                              "DAL" = "#00538C", "DEN" = "#0E2240", "DET" = "#C8102E", 
                              "GSW" = "#FFC72C", "HOU" = "#CE1141", "IND" = "#FDBB30", 
                              "LAC" = "#C8102E", "LAL" = "#552583", "MEM" = "#5D76A9",
                              "MIA" = "#98002E", "MIL" = "#00471B", "MIN" = "#236192", 
                              "NOP" = "#0C2340", "NYK" = "#F58426", "OKC" = "#007AC1",
                              "ORL" = "#0077C0", "PHI" = "#ED174C", "PHO" = "#E56020", 
                              "POR" = "#000000", "SAC" = "#63727A", "SAS" = "#C4CED4", 
                              "TOR" = "#CE1141", "UTA" = "#002B5C", "WAS" = "#002B5C"))

Scoring

#ggsave(filename = 'Top_Scorers_1-5-22.png', plot = Scoring, width=7.5, height=5, units="in", dpi=300)

Assist_to_turnover <- sqldf("SELECT *, (AST/TOV) AS ATR FROM Season_Stats WHERE (GP >= 10) &
                             (AST >= 4) ORDER BY (AST/TOV)  DESC;")

ATRatio <- ggplot(Assist_to_turnover,aes(x = AST, y = ATR, color = Team)) + 
  geom_point(position = "jitter") +
  geom_label_repel(aes(label = Player, fill = Team, color = Team),
                   color = "white", size = 2, fontface = "bold") +
  labs(title = "Assist to Turnover Ratio Leaders 2021-2022 Season", 
       x = "Assists", 
       y = "Assist to Turnover Ratio", 
       caption = "Data Viz by Abhi Joshi | Data from SportsReference | 10/19/21-1/5/22") +
  theme_gray() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        legend.position = "none") +
  scale_fill_manual(values=c("ATL" = "#E03A3E", "BOS" = "#007A33", "BRK" = "#000000", 
                             "CHI" = "#CE1141", "CHO" = "#1D1160", "CLE" = "#860038", 
                             "DAL" = "#00538C", "DEN" = "#0E2240", "DET" = "#C8102E", 
                             "GSW" = "#FFC72C", "HOU" = "#CE1141", "IND" = "#FDBB30", 
                             "LAC" = "#C8102E", "LAL" = "#552583", "MEM" = "#5D76A9",
                             "MIA" = "#98002E", "MIL" = "#00471B", "MIN" = "#236192", 
                             "NOP" = "#0C2340", "NYK" = "#F58426", "OKC" = "#007AC1",
                             "ORL" = "#0077C0", "PHI" = "#ED174C", "PHO" = "#E56020", 
                             "POR" = "#000000", "SAC" = "#63727A", "SAS" = "#C4CED4", 
                             "TOR" = "#CE1141", "UTA" = "#002B5C", "WAS" = "#002B5C")) +
  scale_color_manual(values=c("ATL" = "#E03A3E", "BOS" = "#007A33", "BRK" = "#000000", 
                              "CHI" = "#CE1141", "CHO" = "#1D1160", "CLE" = "#860038", 
                              "DAL" = "#00538C", "DEN" = "#0E2240", "DET" = "#C8102E", 
                              "GSW" = "#FFC72C", "HOU" = "#CE1141", "IND" = "#FDBB30", 
                              "LAC" = "#C8102E", "LAL" = "#552583", "MEM" = "#5D76A9",
                              "MIA" = "#98002E", "MIL" = "#00471B", "MIN" = "#236192", 
                              "NOP" = "#0C2340", "NYK" = "#F58426", "OKC" = "#007AC1",
                              "ORL" = "#0077C0", "PHI" = "#ED174C", "PHO" = "#E56020", 
                              "POR" = "#000000", "SAC" = "#63727A", "SAS" = "#C4CED4", 
                              "TOR" = "#CE1141", "UTA" = "#002B5C", "WAS" = "#002B5C"))

ATRatio

#ggsave(filename = 'ATR_Leaders_1-5-22.png', plot = ATRatio, width=7.5, height=5, units="in", dpi=300)

Age_Distribution <- ggplot(Season_Stats, aes(x=Age)) + 
  geom_histogram(binwidth=2, color = 'white', fill = '#17408B') +
  labs(title = "NBA Player Age Distribution 2021-2022 Season", 
       x = "Age", 
       y = "# of Players", 
       caption = "Data Viz by Abhi Joshi | Data from Basketball Reference | 1/5/22") +
  theme_gray()+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        legend.position = "none") 
Age_Distribution

#ggsave(filename = 'Player_Age_Distribution.png', plot = Age_Distribution, width=7.5, height=5, units="in", dpi=300)

Three_Point_Distribution <- ggplot(Team_Standings_Offense, aes(x=`3P%`)) + 
  geom_histogram(binwidth=.01, color = 'red', fill = '#17408B') +
  labs(title = "NBA Team 3 Point % Distribution 2021-2022 Season", 
       x = "3 Point %", 
       y = "# of Teams", 
       caption = "Data Viz by Abhi Joshi | Data from Basketball Reference | 10/19/21-1/5/22") +
  theme_dark()+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        legend.position = "none") 
Three_Point_Distribution

#ggsave(filename = 'Three_Point_Distribution_1-5-22.png', plot = Three_Point_Distribution, width=7.5, height=5, units="in", dpi=300)


#Credits Formula = (Points)+(Rebounds)+(Assists)+(Steal)+(Blocks)-(Field Goals Missed)-(Free Throws Missed)-(Turnovers)
#AV Formula = (Credits^(3/4))/21
#Trade Value Formula=[(Approximate Value- 27-0.75*Age)^2(27-0.75*Age +1)*Approximate Value]/190+(Approximate Value)*2/13
colnames(Season_Stats)
Credits <- (Season_Stats$PPG)+(Season_Stats$TRB)+(Season_Stats$AST)+(Season_Stats$STL)+(Season_Stats$BLK)-((Season_Stats$FGA)-(Season_Stats$FG))-((Season_Stats$FTA-Season_Stats$FT))-(Season_Stats$TOV)
Credits
AV_Formula <- (Credits^(3/4))/21
AV_Formula
Season_Stats$Trade_Value <- ((AV_Formula-27-(3/4)*(Season_Stats$Age))^(2)*(27-(3/4)*(Season_Stats$Age)+1))/190+(AV_Formula)*(2/13)
Season_Stats
view(Season_Stats)

Season_Stats$Efficiency <- (Season_Stats$PPG)+(Season_Stats$TRB)+(Season_Stats$AST)+(Season_Stats$STL)+(Season_Stats$BLK)-((Season_Stats$TOV)-(Season_Stats$FGA)-(Season_Stats$FG))-((Season_Stats$FTA-Season_Stats$FT))
Season_Stats
view(Season_Stats)

Most_Efficient <- head(sqldf("SELECT * FROM Season_Stats WHERE GP >=10 ORDER BY Efficiency DESC "), n=60)
view(Most_Efficient)

options(ggrepel.max.overlaps = Inf)

TV_vs_Efficiency <- ggplot(Most_Efficient,aes(x = Efficiency, y = Trade_Value, color = Team)) + 
  geom_point(position = "jitter") +
  geom_label_repel(aes(label = Player, fill = Team, color = Team),
                   color = "white", size = 2, fontface = "bold") +
  labs(title = "Player Efficiency vs Trade Value 2021-2022 Season", 
       x = "Player Efficiency", 
       y = "Trade Value", 
       caption = "Data Viz by Abhi Joshi | Data from Basketball Reference | 10/19/21-1/5/22") +
  theme_dark() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        legend.position = "none") +
  scale_fill_manual(values=c("ATL" = "#E03A3E", "BOS" = "#007A33", "BRK" = "#000000", 
                             "CHI" = "#CE1141", "CHO" = "#1D1160", "CLE" = "#860038", 
                             "DAL" = "#00538C", "DEN" = "#0E2240", "DET" = "#C8102E", 
                             "GSW" = "#FFC72C", "HOU" = "#CE1141", "IND" = "#FDBB30", 
                             "LAC" = "#C8102E", "LAL" = "#552583", "MEM" = "#5D76A9",
                             "MIA" = "#98002E", "MIL" = "#00471B", "MIN" = "#236192", 
                             "NOP" = "#0C2340", "NYK" = "#F58426", "OKC" = "#007AC1",
                             "ORL" = "#0077C0", "PHI" = "#ED174C", "PHO" = "#E56020", 
                             "POR" = "#000000", "SAC" = "#63727A", "SAS" = "#C4CED4", 
                             "TOR" = "#CE1141", "UTA" = "#002B5C", "WAS" = "#002B5C")) +
  scale_color_manual(values=c("ATL" = "#E03A3E", "BOS" = "#007A33", "BRK" = "#000000", 
                              "CHI" = "#CE1141", "CHO" = "#1D1160", "CLE" = "#860038", 
                              "DAL" = "#00538C", "DEN" = "#0E2240", "DET" = "#C8102E", 
                              "GSW" = "#FFC72C", "HOU" = "#CE1141", "IND" = "#FDBB30", 
                              "LAC" = "#C8102E", "LAL" = "#552583", "MEM" = "#5D76A9",
                              "MIA" = "#98002E", "MIL" = "#00471B", "MIN" = "#236192", 
                              "NOP" = "#0C2340", "NYK" = "#F58426", "OKC" = "#007AC1",
                              "ORL" = "#0077C0", "PHI" = "#ED174C", "PHO" = "#E56020", 
                              "POR" = "#000000", "SAC" = "#63727A", "SAS" = "#C4CED4", 
                              "TOR" = "#CE1141", "UTA" = "#002B5C", "WAS" = "#002B5C"))

TV_vs_Efficiency

#ggsave(filename = 'TV_vs_Efficiency_1-5-22.png', plot = TV_vs_Efficiency, width=7.5, height=5, units="in", dpi=300)

Team_Standings <- "https://www.basketball-reference.com/leagues/NBA_2022.html"

team_url <- Team_Standings
pageobj <- read_html(team_url, as.data.frame=T, stringsAsFactors = TRUE)

pageobj %>%  
  html_nodes("table") %>% 
  .[[5]] %>% 
  html_table(fill=T) -> Team_Standings_Offense

view(Team_Standings_Offense)

pageobj %>%  
  html_nodes("table") %>% 
  .[[6]] %>% 
  html_table(fill=T) -> Team_Standings_Defense

view(Team_Standings_Defense)

options(ggrepel.max.overlaps = Inf)


Team_Efficiency <- ggplot(Team_Standings_Offense, aes(x = PTS, y = `3P%`, 
                              color = Team)) + 
  geom_point() +
  geom_smooth(formula = `3P%` ~ PTS, method = "lm", se = FALSE) +
  geom_label_repel(aes(label = Team, fill = Team, color = Team),
                   color = "white", size = 2, fontface = "bold") +
  geom_abline() +
  labs(title = "Most Points Scored vs 3 Point Percentage 2021-2022 Season", 
       x = "Points Scored", 
       y = "3 Point Percentage", 
       caption = "Data Viz by Abhi Joshi | Data from Basketball Reference | 10/19/21-1/5/22") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        legend.position = "none") +
  scale_fill_manual(values=c("Atlanta Hawks" = "#E03A3E", "Boston Celtics" = "#007A33", 
                             "Brooklyn Nets" = "#000000", "Chicago Bulls" = "#CE1141", 
                             "Charlotte Hornets" = "#1D1160", "Cleveland Cavaliers" = "#860038", 
                             "Dallas Mavericks" = "#00538C", "Denver Nuggets" = "#0E2240", 
                             "Detroit Pistons" = "#C8102E", "Golden State Warriors" = "#FFC72C", 
                             "Houston Rockets" = "#CE1141", "Indiana Pacers" = "#FDBB30", 
                             "Los Angeles Clippers" = "#C8102E", "Los Angeles Lakers" = "#552583", 
                             "Memphis Grizzlies" = "#5D76A9", "Miami Heat" = "#98002E", 
                             "Milwaukee Bucks" = "#00471B", "Minnesota Timberwolves" = "#236192", 
                             "New Orleans Pelicans" = "#0C2340", "New York Knicks" = "#F58426", 
                             "Oklahoma City Thunder" = "#007AC1", "Orlando Magic" = "#0077C0", 
                             "Philadelphia 76ers" = "#ED174C", "Phoenix Suns" = "#E56020", 
                             "Portland Trail Blazers" = "#000000", "Sacramento Kings" = "#63727A", 
                             "San Antonio Spurs" = "#C4CED4", "Toronto Raptors" = "#CE1141",
                             "Utah Jazz" = "#002B5C", "Washington Wizards" = "#002B5C",
                             "League Average" = "#17408B")) +
  scale_color_manual(values=c("Atlanta Hawks" = "#E03A3E", "Boston Celtics" = "#007A33", 
                              "Brooklyn Nets" = "#000000", "Chicago Bulls" = "#CE1141", 
                              "Charlotte Hornets" = "#1D1160", "Cleveland Cavaliers" = "#860038", 
                              "Dallas Mavericks" = "#00538C", "Denver Nuggets" = "#0E2240", 
                              "Detroit Pistons" = "#C8102E", "Golden State Warriors" = "#FFC72C", 
                              "Houston Rockets" = "#CE1141", "Indiana Pacers" = "#FDBB30", 
                              "Los Angeles Clippers" = "#C8102E", "Los Angeles Lakers" = "#552583", 
                              "Memphis Grizzlies" = "#5D76A9", "Miami Heat" = "#98002E", 
                              "Milwaukee Bucks" = "#00471B", "Minnesota Timberwolves" = "#236192", 
                              "New Orleans Pelicans" = "#0C2340", "New York Knicks" = "#F58426", 
                              "Oklahoma City Thunder" = "#007AC1", "Orlando Magic" = "#0077C0", 
                              "Philadelphia 76ers" = "#ED174C", "Phoenix Suns" = "#E56020", 
                              "Portland Trail Blazers" = "#000000", "Sacramento Kings" = "#63727A", 
                              "San Antonio Spurs" = "#C4CED4", "Toronto Raptors" = "#CE1141",
                              "Utah Jazz" = "#002B5C", "Washington Wizards" = "#002B5C",
                              "League Average" = "#17408B")) 
Team_Efficiency

#ggsave(filename = 'PS_3PPct_1-5-22.png', plot = Team_Efficiency, width=7.5, height=5, units="in", dpi=300)


Team_Defense <- ggplot(Team_Standings_Defense, 
                       aes(x = PTS, y = TOV, color = Team)) + 
  geom_point() +
  geom_smooth(formula = `3P%` ~ PTS, method = "lm", se = FALSE) +
  geom_label_repel(aes(label = Team, fill = Team, color = Team),
                   color = "white", size = 2, fontface = "bold") +
  labs(title = "Points Allowed vs Turnovers Forced 2021-2022 Season", 
       x = "Points Allowed Per Game", 
       y = "Turnovers Forced Per Game", 
       caption = "Data Viz by Abhi Joshi | Data from Basketball Reference | 10/19/21-1/5/22") +
  theme_dark() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        legend.position = "none") +
  scale_fill_manual(values=c("Atlanta Hawks" = "#E03A3E", "Boston Celtics" = "#007A33", 
                             "Brooklyn Nets" = "#000000", "Chicago Bulls" = "#CE1141", 
                             "Charlotte Hornets" = "#1D1160", "Cleveland Cavaliers" = "#860038", 
                             "Dallas Mavericks" = "#00538C", "Denver Nuggets" = "#0E2240", 
                             "Detroit Pistons" = "#C8102E", "Golden State Warriors" = "#FFC72C", 
                             "Houston Rockets" = "#CE1141", "Indiana Pacers" = "#FDBB30", 
                             "Los Angeles Clippers" = "#C8102E", "Los Angeles Lakers" = "#552583", 
                             "Memphis Grizzlies" = "#5D76A9", "Miami Heat" = "#98002E", 
                             "Milwaukee Bucks" = "#00471B", "Minnesota Timberwolves" = "#236192", 
                             "New Orleans Pelicans" = "#0C2340", "New York Knicks" = "#F58426", 
                             "Oklahoma City Thunder" = "#007AC1", "Orlando Magic" = "#0077C0", 
                             "Philadelphia 76ers" = "#ED174C", "Phoenix Suns" = "#E56020", 
                             "Portland Trail Blazers" = "#000000", "Sacramento Kings" = "#63727A", 
                             "San Antonio Spurs" = "#C4CED4", "Toronto Raptors" = "#CE1141",
                             "Utah Jazz" = "#002B5C", "Washington Wizards" = "#002B5C",
                             "League Average" = "#17408B")) +
  scale_color_manual(values=c("Atlanta Hawks" = "#E03A3E", "Boston Celtics" = "#007A33", 
                              "Brooklyn Nets" = "#000000", "Chicago Bulls" = "#CE1141", 
                              "Charlotte Hornets" = "#1D1160", "Cleveland Cavaliers" = "#860038", 
                              "Dallas Mavericks" = "#00538C", "Denver Nuggets" = "#0E2240", 
                              "Detroit Pistons" = "#C8102E", "Golden State Warriors" = "#FFC72C", 
                              "Houston Rockets" = "#CE1141", "Indiana Pacers" = "#FDBB30", 
                              "Los Angeles Clippers" = "#C8102E", "Los Angeles Lakers" = "#552583", 
                              "Memphis Grizzlies" = "#5D76A9", "Miami Heat" = "#98002E", 
                              "Milwaukee Bucks" = "#00471B", "Minnesota Timberwolves" = "#236192", 
                              "New Orleans Pelicans" = "#0C2340", "New York Knicks" = "#F58426", 
                              "Oklahoma City Thunder" = "#007AC1", "Orlando Magic" = "#0077C0", 
                              "Philadelphia 76ers" = "#ED174C", "Phoenix Suns" = "#E56020", 
                              "Portland Trail Blazers" = "#000000", "Sacramento Kings" = "#63727A", 
                              "San Antonio Spurs" = "#C4CED4", "Toronto Raptors" = "#CE1141",
                              "Utah Jazz" = "#002B5C", "Washington Wizards" = "#002B5C",
                              "League Average" = "#17408B")) 
Team_Defense


Most_PPG <- ggplot(Team_Standings_Offense, aes(y=reorder(Team, +PTS), x=PTS)) +
  geom_bar(stat='identity') + 
  labs(title="Most PPG", 
       caption = "Data Viz by Abhi Joshi | Data from Basketball Reference | 10/19/21-1/5/22",
       y='Teams', x='Points Per Game')+theme_light() +
  scale_fill_manual(values=c("Atlanta Hawks" = "#E03A3E", "Boston Celtics" = "#007A33", 
                             "Brooklyn Nets" = "#000000", "Chicago Bulls" = "#CE1141", 
                             "Charlotte Hornets" = "#1D1160", "Cleveland Cavaliers" = "#860038", 
                             "Dallas Mavericks" = "#00538C", "Denver Nuggets" = "#0E2240", 
                             "Detroit Pistons" = "#C8102E", "Golden State Warriors" = "#FFC72C", 
                             "Houston Rockets" = "#CE1141", "Indiana Pacers" = "#FDBB30", 
                             "Los Angeles Clippers" = "#C8102E", "Los Angeles Lakers" = "#552583", 
                             "Memphis Grizzlies" = "#5D76A9", "Miami Heat" = "#98002E", 
                             "Milwaukee Bucks" = "#00471B", "Minnesota Timberwolves" = "#236192", 
                             "New Orleans Pelicans" = "#0C2340", "New York Knicks" = "#F58426", 
                             "Oklahoma City Thunder" = "#007AC1", "Orlando Magic" = "#0077C0", 
                             "Philadelphia 76ers" = "#ED174C", "Phoenix Suns" = "#E56020", 
                             "Portland Trail Blazers" = "#000000", "Sacramento Kings" = "#63727A", 
                             "San Antonio Spurs" = "#C4CED4", "Toronto Raptors" = "#CE1141",
                             "Utah Jazz" = "#002B5C", "Washington Wizards" = "#002B5C",
                             "League Average" = "#17408B")) +
  scale_color_manual(values=c("Atlanta Hawks" = "#E03A3E", "Boston Celtics" = "#007A33", 
                              "Brooklyn Nets" = "#000000", "Chicago Bulls" = "#CE1141", 
                              "Charlotte Hornets" = "#1D1160", "Cleveland Cavaliers" = "#860038", 
                              "Dallas Mavericks" = "#00538C", "Denver Nuggets" = "#0E2240", 
                              "Detroit Pistons" = "#C8102E", "Golden State Warriors" = "#FFC72C", 
                              "Houston Rockets" = "#CE1141", "Indiana Pacers" = "#FDBB30", 
                              "Los Angeles Clippers" = "#C8102E", "Los Angeles Lakers" = "#552583", 
                              "Memphis Grizzlies" = "#5D76A9", "Miami Heat" = "#98002E", 
                              "Milwaukee Bucks" = "#00471B", "Minnesota Timberwolves" = "#236192", 
                              "New Orleans Pelicans" = "#0C2340", "New York Knicks" = "#F58426", 
                              "Oklahoma City Thunder" = "#007AC1", "Orlando Magic" = "#0077C0", 
                              "Philadelphia 76ers" = "#ED174C", "Phoenix Suns" = "#E56020", 
                              "Portland Trail Blazers" = "#000000", "Sacramento Kings" = "#63727A", 
                              "San Antonio Spurs" = "#C4CED4", "Toronto Raptors" = "#CE1141",
                              "Utah Jazz" = "#002B5C", "Washington Wizards" = "#002B5C",
                              "League Average" = "#17408B"))
Most_PPG

Least_Second_Chances_Given <- ggplot(Team_Standings_Defense, 
                               aes(x = PTS, y = DRB, color = Team)) + 
  geom_point() +
  geom_smooth(formula = DRB ~ PTS, method = "lm", se = FALSE) +
  geom_label_repel(aes(label = Team, fill = Team, color = Team),
                   color = "white", size = 2, fontface = "bold") +
  labs(title = "Defensive Rebounding vs Points Allowed 2021-2022 Season", 
       x = "Points Allowed Per Game", 
       y = "Defensive Rebounds Per Game", 
       caption = "Data Viz by Abhi Joshi | Data from Basketball Reference | 10/19/21-1/5/22") +
  theme_dark() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        legend.position = "none") +
  scale_fill_manual(values=c("Atlanta Hawks" = "#E03A3E", "Boston Celtics" = "#007A33", 
                             "Brooklyn Nets" = "#000000", "Chicago Bulls" = "#CE1141", 
                             "Charlotte Hornets" = "#1D1160", "Cleveland Cavaliers" = "#860038", 
                             "Dallas Mavericks" = "#00538C", "Denver Nuggets" = "#0E2240", 
                             "Detroit Pistons" = "#C8102E", "Golden State Warriors" = "#FFC72C", 
                             "Houston Rockets" = "#CE1141", "Indiana Pacers" = "#FDBB30", 
                             "Los Angeles Clippers" = "#C8102E", "Los Angeles Lakers" = "#552583", 
                             "Memphis Grizzlies" = "#5D76A9", "Miami Heat" = "#98002E", 
                             "Milwaukee Bucks" = "#00471B", "Minnesota Timberwolves" = "#236192", 
                             "New Orleans Pelicans" = "#0C2340", "New York Knicks" = "#F58426", 
                             "Oklahoma City Thunder" = "#007AC1", "Orlando Magic" = "#0077C0", 
                             "Philadelphia 76ers" = "#ED174C", "Phoenix Suns" = "#E56020", 
                             "Portland Trail Blazers" = "#000000", "Sacramento Kings" = "#63727A", 
                             "San Antonio Spurs" = "#C4CED4", "Toronto Raptors" = "#CE1141",
                             "Utah Jazz" = "#002B5C", "Washington Wizards" = "#002B5C",
                             "League Average" = "#17408B")) +
  scale_color_manual(values=c("Atlanta Hawks" = "#E03A3E", "Boston Celtics" = "#007A33", 
                              "Brooklyn Nets" = "#000000", "Chicago Bulls" = "#CE1141", 
                              "Charlotte Hornets" = "#1D1160", "Cleveland Cavaliers" = "#860038", 
                              "Dallas Mavericks" = "#00538C", "Denver Nuggets" = "#0E2240", 
                              "Detroit Pistons" = "#C8102E", "Golden State Warriors" = "#FFC72C", 
                              "Houston Rockets" = "#CE1141", "Indiana Pacers" = "#FDBB30", 
                              "Los Angeles Clippers" = "#C8102E", "Los Angeles Lakers" = "#552583", 
                              "Memphis Grizzlies" = "#5D76A9", "Miami Heat" = "#98002E", 
                              "Milwaukee Bucks" = "#00471B", "Minnesota Timberwolves" = "#236192", 
                              "New Orleans Pelicans" = "#0C2340", "New York Knicks" = "#F58426", 
                              "Oklahoma City Thunder" = "#007AC1", "Orlando Magic" = "#0077C0", 
                              "Philadelphia 76ers" = "#ED174C", "Phoenix Suns" = "#E56020", 
                              "Portland Trail Blazers" = "#000000", "Sacramento Kings" = "#63727A", 
                              "San Antonio Spurs" = "#C4CED4", "Toronto Raptors" = "#CE1141",
                              "Utah Jazz" = "#002B5C", "Washington Wizards" = "#002B5C",
                              "League Average" = "#17408B")) 
Least_Second_Chances_Given

Most_Second_Chances <- ggplot(Team_Standings_Offense, 
                                     aes(x = PTS, y = ORB, color = Team)) + 
  geom_point() +
  geom_smooth(formula = ORB ~ PTS, method = "lm", se = FALSE) +
  geom_label_repel(aes(label = Team, fill = Team, color = Team),
                   color = "white", size = 2, fontface = "bold") +
  labs(title = "The Benefit of Second Chances 2021-2022 Season", 
       x = "Points Scored Per Game", 
       y = "Second Chances", 
       caption = "Data Viz by Abhi Joshi | Data from Basketball Reference | 10/19/21-1/5/22") +
  theme_economist() + scale_fill_economist() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        legend.position = "none") +
  scale_fill_manual(values=c("Atlanta Hawks" = "#E03A3E", "Boston Celtics" = "#007A33", 
                             "Brooklyn Nets" = "#000000", "Chicago Bulls" = "#CE1141", 
                             "Charlotte Hornets" = "#1D1160", "Cleveland Cavaliers" = "#860038", 
                             "Dallas Mavericks" = "#00538C", "Denver Nuggets" = "#0E2240", 
                             "Detroit Pistons" = "#C8102E", "Golden State Warriors" = "#FFC72C", 
                             "Houston Rockets" = "#CE1141", "Indiana Pacers" = "#FDBB30", 
                             "Los Angeles Clippers" = "#C8102E", "Los Angeles Lakers" = "#552583", 
                             "Memphis Grizzlies" = "#5D76A9", "Miami Heat" = "#98002E", 
                             "Milwaukee Bucks" = "#00471B", "Minnesota Timberwolves" = "#236192", 
                             "New Orleans Pelicans" = "#0C2340", "New York Knicks" = "#F58426", 
                             "Oklahoma City Thunder" = "#007AC1", "Orlando Magic" = "#0077C0", 
                             "Philadelphia 76ers" = "#ED174C", "Phoenix Suns" = "#E56020", 
                             "Portland Trail Blazers" = "#000000", "Sacramento Kings" = "#63727A", 
                             "San Antonio Spurs" = "#C4CED4", "Toronto Raptors" = "#CE1141",
                             "Utah Jazz" = "#002B5C", "Washington Wizards" = "#002B5C",
                             "League Average" = "#17408B")) +
  scale_color_manual(values=c("Atlanta Hawks" = "#E03A3E", "Boston Celtics" = "#007A33", 
                              "Brooklyn Nets" = "#000000", "Chicago Bulls" = "#CE1141", 
                              "Charlotte Hornets" = "#1D1160", "Cleveland Cavaliers" = "#860038", 
                              "Dallas Mavericks" = "#00538C", "Denver Nuggets" = "#0E2240", 
                              "Detroit Pistons" = "#C8102E", "Golden State Warriors" = "#FFC72C", 
                              "Houston Rockets" = "#CE1141", "Indiana Pacers" = "#FDBB30", 
                              "Los Angeles Clippers" = "#C8102E", "Los Angeles Lakers" = "#552583", 
                              "Memphis Grizzlies" = "#5D76A9", "Miami Heat" = "#98002E", 
                              "Milwaukee Bucks" = "#00471B", "Minnesota Timberwolves" = "#236192", 
                              "New Orleans Pelicans" = "#0C2340", "New York Knicks" = "#F58426", 
                              "Oklahoma City Thunder" = "#007AC1", "Orlando Magic" = "#0077C0", 
                              "Philadelphia 76ers" = "#ED174C", "Phoenix Suns" = "#E56020", 
                              "Portland Trail Blazers" = "#000000", "Sacramento Kings" = "#63727A", 
                              "San Antonio Spurs" = "#C4CED4", "Toronto Raptors" = "#CE1141",
                              "Utah Jazz" = "#002B5C", "Washington Wizards" = "#002B5C",
                              "League Average" = "#17408B")) 
Most_Second_Chances
#ggsave(filename = 'PS_Vs_2nd_Chance_Opportunities.png', plot = Most_Second_Chances, width=7.5, height=5, units="in", dpi=300)


all_nba_players <- 
  get_nba_players_ids(league = "NBA",
                      active_only = F)

players_1998 <-
  get_nba_season_players(
    year.season_start = 1998,
    include_only_rostered_players = F,
    return_message = T
  )
brooklyn_nets_2016_roster <- 
  get_nba_team_season_roster(team = "Brooklyn Nets", year_season_end = 2016)

players_1998_2015 <- get_nba_seasons_players(years = 1998:2014,
                                             only_on_roster = T,
                                             message = F)

profiles_2015_season <-
  get_season_player_profiles(year.season_start = 2014, include_headline_stats = T,
                             only_rostered_players = T,
                             message = T)


view(all_nba_players)

Sixers_Logo <- ("C:/Users/abhi1/OneDrive/Documents/NCSUSAC/NBA/NBATeamLogos/76ers.gif")
Bucks_Logo <- ("C:/Users/abhi1/OneDrive/Documents/NCSUSAC/NBA/NBATeamLogos/Bucks.gif")
Bulls_Logo <- ("C:/Users/abhi1/OneDrive/Documents/NCSUSAC/NBA/NBATeamLogos/Bulls.gif")
Cavs_Logo <- ("C:/Users/abhi1/OneDrive/Documents/NCSUSAC/NBA/NBATeamLogos/Cavaliers.gif")
Celtics_Logo <- ("C:/Users/abhi1/OneDrive/Documents/NCSUSAC/NBA/NBATeamLogos/Celtics.gif")
Clippers_Logo <- ("C:/Users/abhi1/OneDrive/Documents/NCSUSAC/NBA/NBATeamLogos/Clippers.gif")
Grizzlies_Logo <- ("C:/Users/abhi1/OneDrive/Documents/NCSUSAC/NBA/NBATeamLogos/Grizzlies.gif")
Hawks_Logo <- ("C:/Users/abhi1/OneDrive/Documents/NCSUSAC/NBA/NBATeamLogos/Hawks.gif")
Heat_Logo <- ("C:/Users/abhi1/OneDrive/Documents/NCSUSAC/NBA/NBATeamLogos/Heat.gif")
Hornets_Logo <- ("C:/Users/abhi1/OneDrive/Documents/NCSUSAC/NBA/NBATeamLogos/Hornets.gif")
Jazz_Logo <- ("C:/Users/abhi1/OneDrive/Documents/NCSUSAC/NBA/NBATeamLogos/Jazz.gif")
Kings_Logo <- ("C:/Users/abhi1/OneDrive/Documents/NCSUSAC/NBA/NBATeamLogos/Kings.gif")
Knicks_Logo <- ("C:/Users/abhi1/OneDrive/Documents/NCSUSAC/NBA/NBATeamLogos/Knicks.gif")
Lakers_Logo <- ("C:/Users/abhi1/OneDrive/Documents/NCSUSAC/NBA/NBATeamLogos/Lakers.gif")
Magic_Logo <- ("C:/Users/abhi1/OneDrive/Documents/NCSUSAC/NBA/NBATeamLogos/Magic.gif")
Mavs_Logo <- ("C:/Users/abhi1/OneDrive/Documents/NCSUSAC/NBA/NBATeamLogos/Mavericks.gif")
Nets_Logo <- ("C:/Users/abhi1/OneDrive/Documents/NCSUSAC/NBA/NBATeamLogos/Nets.gif")
Nuggets_Logo <- ("C:/Users/abhi1/OneDrive/Documents/NCSUSAC/NBA/NBATeamLogos/Nuggets.gif")
Pacers_Logo <- ("C:/Users/abhi1/OneDrive/Documents/NCSUSAC/NBA/NBATeamLogos/Pacers.gif")
Pelicans_Logo <- ("C:/Users/abhi1/OneDrive/Documents/NCSUSAC/NBA/NBATeamLogos/Pelicans.gif")
Pistons_Logo <- ("C:/Users/abhi1/OneDrive/Documents/NCSUSAC/NBA/NBATeamLogos/Pistons.gif")
Raptors_Logo <- ("C:/Users/abhi1/OneDrive/Documents/NCSUSAC/NBA/NBATeamLogos/Raptors.gif")
Rockets_Logo <- ("C:/Users/abhi1/OneDrive/Documents/NCSUSAC/NBA/NBATeamLogos/Rocketss.gif")
Spurs_Logo <- ("C:/Users/abhi1/OneDrive/Documents/NCSUSAC/NBA/NBATeamLogos/Spurs.gif")
Suns_Logo <- ("C:/Users/abhi1/OneDrive/Documents/NCSUSAC/NBA/NBATeamLogos/Suns.gif")
Thunder_Logo <- ("C:/Users/abhi1/OneDrive/Documents/NCSUSAC/NBA/NBATeamLogos/Thunder.gif")
Wolves_Logo <- ("C:/Users/abhi1/OneDrive/Documents/NCSUSAC/NBA/NBATeamLogos/Timberwolves.gif")
Blazers_Logo <- ("C:/Users/abhi1/OneDrive/Documents/NCSUSAC/NBA/NBATeamLogos/TrailBlazers.gif")
Warriors_Logo <- ("C:/Users/abhi1/OneDrive/Documents/NCSUSAC/NBA/NBATeamLogos/Warriors.gif")
Wizards_Logo <- ("C:/Users/abhi1/OneDrive/Documents/NCSUSAC/NBA/NBATeamLogos/Wizards.gif")
NBA_Logo <- ("C:/Users/abhi1/OneDrive/Documents/NCSUSAC/NBA/NBATeamLogos/NBA.png")

Logos <- data.frame(Sixers_Logo, Bucks_Logo, Bulls_Logo, Cavs_Logo, Celtics_Logo, 
              Clippers_Logo, Grizzlies_Logo, Hawks_Logo, Heat_Logo, Hornets_Logo,
              Jazz_Logo, Kings_Logo, Knicks_Logo, Lakers_Logo, Magic_Logo, Mavs_Logo,
              Nets_Logo, Nuggets_Logo, Pacers_Logo, Pelicans_Logo, Pistons_Logo, 
              Raptors_Logo, Rockets_Logo, Spurs_Logo, Suns_Logo, Thunder_Logo, 
              Wolves_Logo, Blazers_Logo, Warriors_Logo, Wizards_Logo, NBA_Logo)
view(Logos)

