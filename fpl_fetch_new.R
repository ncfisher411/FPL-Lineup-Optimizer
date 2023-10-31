# V2: Developing a shiny app to predict FPL points by player based on current
# and historic performance against opposition.

# Uses the following Github repo as a resource for gameweek data:
# https://github.com/vaastav/Fantasy-Premier-League/tree/master

## Step 1.1: Prepare the historic data
### Using fplr for supplementary data
packages <- c("tidyverse", "stringi", "fplr", "fplscrapR", "stringr", "car", "rstudioapi")

for (package in packages) {
  if (!requireNamespace(package, quietly = T)) {
    install.packages(package, dependencies = T)
  }
  library(package, character.only = T)
  cat(paste(package, "package loaded.\n"))
}

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

one <- read.csv("gameweeks/gw1617.csv") %>% 
  mutate(name=gsub("_", " ", name)) %>%
  mutate(name=gsub("-", " ", name)) %>%
  mutate(name=stri_trans_general(name, "Latin-ASCII")) %>%
  mutate(Season=2017)
two <- read.csv("gameweeks/gw1718.csv") %>% 
  mutate(name=gsub("_", " ", name)) %>%
  mutate(name=gsub("-", " ", name)) %>%
  mutate(name=trimws(name, "right")) %>%
  mutate(name=stri_trans_general(name, "Latin-ASCII")) %>%
  mutate(Season=2018)
three <- read.csv("gameweeks/gw1819.csv") %>%
  mutate(name=sub("^(.*?_.*?_).*", '\\1', name)) %>%
  mutate(name=gsub("_", " ", name)) %>%
  mutate(name=gsub("-", " ", name)) %>%
  mutate(name=trimws(name, "right")) %>%
  mutate(name=stri_trans_general(name, "Latin-ASCII")) %>%
  mutate(Season=2019)
four <- read.csv("gameweeks/gw1920.csv", encoding = "UTF-8") %>%
  mutate(name=sub("^(.*?_.*?_).*", '\\1', name)) %>%
  mutate(name=gsub("_", " ", name)) %>%
  mutate(name=gsub("-", " ", name)) %>%
  mutate(name=trimws(name, "right")) %>%
  mutate(name=stri_trans_general(name, "Latin-ASCII")) %>%
  mutate(Season=2020)
five <- read.csv("gameweeks/gw2021.csv", encoding="UTF-8") %>%
  mutate(name=stri_trans_general(name, "Latin-ASCII")) %>%
  mutate(name=gsub("-", " ", name)) %>%
  mutate(Season=2021)
six <- read.csv("gameweeks/gw2122.csv", encoding="UTF-8") %>%
  mutate(name=stri_trans_general(name, "Latin-ASCII")) %>%
  mutate(name=gsub("-", " ", name)) %>%
  mutate(Season=2022)
seven <- read.csv("gameweeks/gw2223.csv", encoding="UTF-8") %>%
  mutate(name=stri_trans_general(name, "Latin-ASCII")) %>%
  mutate(name=gsub("-", " ", name)) %>%
  mutate(Season=2023)

gws <- bind_rows(one, two, three, four, five, six, seven) %>%
  select(name, Season, everything()) %>%
  mutate(name=stringr::str_replace_all(name, "\\s", " ")) %>%
  mutate(name=ifelse(grepl("De Gea", name), "David de Gea", name)) %>%
  mutate(name=ifelse(grepl("-", name), gsub("-", " ", name), name))

teams <- read.csv("https://raw.githubusercontent.com/vaastav/Fantasy-Premier-League/master/data/master_team_list.csv") %>%
  mutate(Season=ifelse(grepl("2016", season), 2017, NA)) %>%
  mutate(Season=ifelse(grepl("2017", season), 2018, Season)) %>%
  mutate(Season=ifelse(grepl("2018", season), 2019, Season)) %>%
  mutate(Season=ifelse(grepl("2019", season), 2020, Season)) %>%
  mutate(Season=ifelse(grepl("2020", season), 2021, Season)) %>%
  mutate(Season=ifelse(grepl("2021", season), 2022, Season)) %>%
  mutate(Season=ifelse(grepl("2022", season), 2023, Season)) %>%
  select(Season, team, team_name)

temp <- read.csv("https://raw.githubusercontent.com/vaastav/Fantasy-Premier-League/master/data/2022-23/teams.csv") %>%
  mutate(Season=2023) %>%
  select(Season, id, name) %>%
  rename(team=id, team_name=name)

teams <- rbind(teams, temp)

scrape <- data.frame()
for(i in 16:20){
  scrape <- get_player_info(season=i) %>%
    mutate(Season=as.numeric(paste0(20,i+1))) %>%
    left_join(teams, by=c("Season","team")) %>% 
    mutate(playername=stri_trans_general(playername, "Latin-ASCII"))
  assign(paste0("scrape", i), scrape)
}

temp <- list(scrape16, scrape17, scrape18, scrape19, scrape20)
common <- Reduce(intersect, lapply(temp, colnames))
temp <- lapply(temp, function(scrape) scrape[, common, drop=F])
scrape16 <- temp[[1]]
scrape17 <- temp[[2]]
scrape18 <- temp[[3]]
scrape19 <- temp[[4]]
scrape20 <- temp[[5]]

scrape <- rbind(scrape16, scrape17, scrape18, scrape19, scrape20) %>%
  select(playername, team_name, Season) %>%
  mutate(playername=ifelse(grepl("-", playername), gsub("-", " ", playername), playername)) %>%
  mutate(playername=ifelse(grepl("De Gea", playername), "David de Gea", playername))

historic <- gws %>% left_join(scrape, by=c("name"="playername", "Season"="Season")) %>%
  mutate(team_name=ifelse(is.na(team_name) & !is.na(team), team, team_name)) %>%
  left_join(teams, by=c("Season", "opponent_team"="team")) %>%
  select(-team, -kickoff_time, -kickoff_time_formatted, -loaned_in, -loaned_out,
         -selected, -opponent_team, -transfers_balance, -transfers_in, transfers_out,
         -element, -value) %>%
  rename(opponent_team=team_name.y, team_name=team_name.x) %>%
  select(name, Season, team_name, opponent_team, total_points, everything()) %>%
  mutate(name=ifelse(grepl("Eddie", name), gsub("Eddie", "Edward", name), name)) %>%
  mutate(name=ifelse(grepl("-", name), gsub("-", " ", name), name))

#### Step 1.2: Get custom fixture difficulty for historic data
diff_rk <- historic %>% group_by(Season, opponent_team) %>%
  summarize(total_points=sum(total_points, na.rm=T)) %>%
  ungroup() %>%
  group_by(Season) %>%
  arrange(total_points) %>%
  mutate(fdr=rank(-total_points)) %>%
  ungroup() %>%
  select(-total_points)

historic <- historic %>% left_join(diff_rk, by=c("opponent_team", "Season"))

## Step 2.1: Get the current GWs for the current season that have already
## been completed. 
current <- read.csv("https://raw.githubusercontent.com/vaastav/Fantasy-Premier-League/master/data/2023-24/gws/merged_gw.csv",
                    encoding="UTF-8") %>%
  mutate(Season=2024) %>%
  mutate(name=stri_trans_general(name, "Latin-ASCII"))

### Step 2.2: Get the current match difficulty ratings and join
teams <- read.csv("https://raw.githubusercontent.com/vaastav/Fantasy-Premier-League/master/data/2023-24/teams.csv") %>%
  mutate(strength=(strength_overall_home+strength_overall_away)/2) %>%
  mutate(fdr=rank(strength)) %>%
  select(id, name, fdr) %>% rename(team_name=2)

current <- current %>% left_join(teams, by=c("opponent_team"="id")) %>%
  mutate(opponent_team=team_name) %>% select(-team_name) %>%
  rename(team_name=team) %>%
  mutate(name=gsub("-", " ", name)) %>%
  mutate(name=ifelse(grepl("Eddie", name), gsub("Eddie", "Edward", name), name)) %>%
  mutate(name=ifelse(grepl("-", name), gsub("-", " ", name), name))

df <- bind_rows(historic, current)

### Step 2.3: Get the current season probabilities, join these later
season <- df %>% select(name, Season, team_name, opponent_team, minutes,
                             goals_scored, goals_conceded, assists,
                             clean_sheets, saves, yellow_cards, red_cards,
                             penalties_missed, penalties_saved) %>%
  mutate(played=ifelse(minutes>0,1,0)) %>%
  mutate(played60=ifelse(minutes>59,1,0)) %>%
  mutate(against=1) %>%
  group_by(name, team_name, Season) %>%
  summarize(played=sum(played, na.rm = T), played60=sum(played60, na.rm = T),
            against=sum(against, na.rm = T), goals_scored=sum(goals_scored, na.rm = T),
            goals_conceded=sum(goals_conceded, na.rm = T),
            assists=sum(assists, na.rm = T), clean_sheets=sum(clean_sheets, na.rm = T),
            saves=sum(saves, na.rm = T), yellow_cards=sum(yellow_cards, na.rm = T),
            red_cards=sum(red_cards, na.rm = T), 
            penalties_missed=sum(penalties_missed, na.rm = T),
            penalties_saved=sum(penalties_saved, na.rm = T),
            minutes=sum(minutes, na.rm = T)) %>%
  ungroup() %>%
  group_by(name, Season, team_name) %>%
  mutate(played_S=sum(played)/sum(against)) %>%
  mutate(played60_S=sum(played60)/sum(against)) %>%
  mutate(goals_scored_S=(sum(goals_scored)*90)/minutes) %>%
  mutate(goals_conceded_S=(sum(goals_conceded)*90)/minutes) %>%
  mutate(assists_S=(sum(assists)*90)/minutes) %>%
  mutate(clean_sheets_S=(sum(clean_sheets)*90)/minutes) %>%
  mutate(saves_S=(sum(saves)*90)/minutes) %>%
  mutate(yellow_cards_S=(sum(yellow_cards)*90)/minutes) %>%
  mutate(red_cards_S=(sum(red_cards)*90)/minutes) %>%
  mutate(penalties_missed_S=(sum(penalties_missed)*90)/minutes) %>%
  mutate(penalties_saved_S=(sum(penalties_saved)*90)/minutes) %>%
  ungroup() %>%
  select(-against, -played, -played60, -goals_conceded, -goals_scored,
         -assists, -clean_sheets, -saves, -yellow_cards, -red_cards,
         -penalties_missed, -penalties_saved, -minutes, -Season) %>%
  mutate(across(everything(), ~ ifelse(is.nan(.), NA, .))) %>%
  mutate(across(everything(), ~ ifelse(is.na(.), 0, .))) %>%
  group_by(name, team_name) %>%
  summarize(across(ends_with("_S"), mean))

## Step 4.1: Calculate probabilities based on team-team performance, join later
team_counts <- df %>%
  select(name, Season, team_name, opponent_team, minutes, goals_scored, 
         goals_conceded, assists, clean_sheets, saves, yellow_cards, 
         red_cards, penalties_missed, penalties_saved) %>%
  group_by(team_name, opponent_team) %>%
  summarize(goals_scored=sum(goals_scored, na.rm = T),
            goals_conceded=sum(goals_conceded, na.rm = T),
            assists=sum(assists, na.rm = T), clean_sheets=sum(clean_sheets, na.rm = T),
            saves=sum(saves, na.rm = T), yellow_cards=sum(yellow_cards, na.rm = T),
            red_cards=sum(red_cards, na.rm = T), 
            penalties_missed=sum(penalties_missed, na.rm = T),
            penalties_saved=sum(penalties_saved, na.rm = T),
            minutes=sum(minutes, na.rm = T)) %>%
  ungroup() %>%
  group_by(team_name, opponent_team) %>%
  mutate(goals_scored_T=(sum(goals_scored)*90)/minutes) %>%
  mutate(goals_conceded_T=(sum(goals_conceded)*90)/minutes) %>%
  mutate(assists_T=(sum(assists)*90)/minutes) %>%
  mutate(clean_sheets_T=(sum(clean_sheets)*90)/minutes) %>%
  mutate(saves_T=(sum(saves)*90)/minutes) %>%
  mutate(yellow_cards_T=(sum(yellow_cards)*90)/minutes) %>%
  mutate(red_cards_T=(sum(red_cards)*90)/minutes) %>%
  mutate(penalties_missed_T=(sum(penalties_missed)*90)/minutes) %>%
  mutate(penalties_saved_T=(sum(penalties_saved)*90)/minutes) %>%
  ungroup() %>%
  select(-goals_conceded, -goals_scored, -assists, -clean_sheets, -saves, 
         -yellow_cards, -red_cards, -penalties_missed, -penalties_saved,
         -minutes) %>%
  filter(team_name != opponent_team) %>%
  mutate(across(everything(), ~ ifelse(is.nan(.), NA, .))) %>%
  mutate(across(everything(), ~ ifelse(is.na(.), 0, .)))

### Step 4.2: Get player-team historical performance probabilities, join later
player_counts <- df %>%
  select(name, Season, team_name, opponent_team, total_points, minutes,
         goals_scored, goals_conceded, assists, clean_sheets, saves, 
         yellow_cards, red_cards, penalties_missed, penalties_saved) %>%
  mutate(played=ifelse(minutes>0,1,0)) %>%
  mutate(played60=ifelse(minutes>59,1,0)) %>%
  mutate(against=1) %>%
  group_by(name, team_name, opponent_team) %>%
  summarize(played=sum(played, na.rm = T), played60=sum(played60, na.rm = T),
            against=sum(against, na.rm = T), goals_scored=sum(goals_scored, na.rm = T),
            goals_conceded=sum(goals_conceded, na.rm = T),
            assists=sum(assists, na.rm = T), clean_sheets=sum(clean_sheets, na.rm = T),
            saves=sum(saves, na.rm = T), yellow_cards=sum(yellow_cards, na.rm = T),
            red_cards=sum(red_cards, na.rm = T), 
            penalties_missed=sum(penalties_missed, na.rm = T),
            penalties_saved=sum(penalties_saved, na.rm = T),
            minutes=sum(minutes, na.rm = T),
            total_points=mean(total_points, na.rm = T)) %>%
  ungroup() %>%
  group_by(name, team_name, opponent_team) %>%
  mutate(played_P=sum(played)/sum(against)) %>%
  mutate(played60_P=sum(played60)/sum(against)) %>%
  mutate(goals_scored_P=(sum(goals_scored)*90)/minutes) %>%
  mutate(goals_conceded_P=(sum(goals_conceded)*90)/minutes) %>%
  mutate(assists_P=(sum(assists)*90)/minutes) %>%
  mutate(clean_sheets_P=(sum(clean_sheets)*90)/minutes) %>%
  mutate(saves_P=(sum(saves)*90)/minutes) %>%
  mutate(yellow_cards_P=(sum(yellow_cards)*90)/minutes) %>%
  mutate(red_cards_P=(sum(red_cards)*90)/minutes) %>%
  mutate(penalties_missed_P=(sum(penalties_missed)*90)/minutes) %>%
  mutate(penalties_saved_P=(sum(penalties_saved)*90)/minutes) %>%
  ungroup() %>%
  select(-against, -played, -played60, -goals_conceded, -goals_scored,
         -assists, -clean_sheets, -saves, -yellow_cards, -red_cards,
         -penalties_missed, -penalties_saved, -minutes) %>%
  filter(team_name != opponent_team) %>%
  mutate(across(everything(), ~ ifelse(is.nan(.), NA, .))) %>%
  mutate(across(everything(), ~ ifelse(is.na(.), 0, .)))

### Step 4.3: Get the 2024 per 90 stats that are listed in season for current
### season.
two4 <- df %>% select(name, Season, team_name, opponent_team, minutes,
                        goals_scored, goals_conceded, assists,
                        clean_sheets, saves, yellow_cards, red_cards,
                        penalties_missed, penalties_saved) %>%
  filter(Season==2024) %>%
  mutate(played=ifelse(minutes>0,1,0)) %>%
  mutate(played60=ifelse(minutes>59,1,0)) %>%
  mutate(against=1) %>%
  group_by(name, team_name, Season) %>%
  summarize(played=sum(played, na.rm = T), played60=sum(played60, na.rm = T),
            against=sum(against, na.rm = T), goals_scored=sum(goals_scored, na.rm = T),
            goals_conceded=sum(goals_conceded, na.rm = T),
            assists=sum(assists, na.rm = T), clean_sheets=sum(clean_sheets, na.rm = T),
            saves=sum(saves, na.rm = T), yellow_cards=sum(yellow_cards, na.rm = T),
            red_cards=sum(red_cards, na.rm = T), 
            penalties_missed=sum(penalties_missed, na.rm = T),
            penalties_saved=sum(penalties_saved, na.rm = T),
            minutes=sum(minutes, na.rm = T)) %>%
  ungroup() %>%
  group_by(name, Season, team_name) %>%
  mutate(played_S=sum(played)/sum(against)) %>%
  mutate(played60_S=sum(played60)/sum(against)) %>%
  mutate(goals_scored_S=(sum(goals_scored)*90)/minutes) %>%
  mutate(goals_conceded_S=(sum(goals_conceded)*90)/minutes) %>%
  mutate(assists_S=(sum(assists)*90)/minutes) %>%
  mutate(clean_sheets_S=(sum(clean_sheets)*90)/minutes) %>%
  mutate(saves_S=(sum(saves)*90)/minutes) %>%
  mutate(yellow_cards_S=(sum(yellow_cards)*90)/minutes) %>%
  mutate(red_cards_S=(sum(red_cards)*90)/minutes) %>%
  mutate(penalties_missed_S=(sum(penalties_missed)*90)/minutes) %>%
  mutate(penalties_saved_S=(sum(penalties_saved)*90)/minutes) %>%
  ungroup() %>%
  select(-against, -played, -played60, -goals_conceded, -goals_scored,
         -assists, -clean_sheets, -saves, -yellow_cards, -red_cards,
         -penalties_missed, -penalties_saved, -minutes, -Season) %>%
  group_by(name, team_name) %>%
  summarize(across(ends_with("_S"), mean)) %>%
  mutate(across(everything(), ~ ifelse(is.nan(.), NA, .))) %>%
  mutate(across(everything(), ~ ifelse(is.na(.), 0, .)))

### Step 4.4: Combine these probabilities/per 90 stats into one dataframe
one <- diff_rk %>% group_by(opponent_team) %>%
  summarize(fdr=mean(fdr))

df2 <- player_counts %>%
  left_join(season, by=c("name", "team_name"))  %>%
  left_join(one, by="opponent_team") %>%
  left_join(team_counts, by=c("team_name", "opponent_team")) %>%
  ungroup() %>%
  distinct(name, team_name, opponent_team, .keep_all = T) %>%
  mutate(across(everything(), ~ ifelse(is.na(.), 0, .))) %>%
  mutate(across(everything(), ~ ifelse(is.infinite(.), 0, .)))

## Step 5.1: Run the ols regression to estimate the model
## In the future, will want to work on comparing more regression types
vars <- colnames(df2)
vars
vars <- vars[-c(4)]
vars
formula <- formula(paste("total_points ~", paste(vars, collapse = " + ")))

ols <- lm(formula, data=df2)
temp <- summary(ols)
metrics <- data.frame(r2 = temp$r.squared, rmse = temp$sigma^2)

## Step 6.1: Load the upcoming fixtures that have not been completed
fixtures <- fpl_get_fixtures() %>%
  rename(GW=event, away=team_a, home=team_h) %>%
  filter(!is.na(GW)) %>%
  left_join(teams, by=c("away"="id")) %>%
  mutate(away=team_name) %>%
  select(-team_name, -fdr) %>%
  left_join(teams, by=c("home"="id")) %>%
  mutate(home=team_name) %>%
  select(GW, away, home) %>%
  rename(team_name=2, opponent_team=3)

temp <- fixtures %>% select(GW, opponent_team, team_name) %>%
  rename(team_name=2, opponent_team=3)    
fixtures <- bind_rows(fixtures, temp) %>%
  arrange(GW, team_name)

positions <- df %>% distinct(name, position, Season) %>%
  filter(Season==2024) %>%
  filter(!is.na(position)) %>%
  select(name, position)

### Step 6.2: Join model dataframe to upcoming fixtures
temp <- df %>% filter(Season==2024) %>% distinct(name, team_name) %>%
  mutate(check=paste(name, team_name, sep="_"))

one <- df2 %>% distinct(name, team_name) %>%
  mutate(check=paste(name, team_name, sep="_")) %>%
  filter(check %in% temp$check)

df3 <- fixtures %>%
  left_join(one, by="team_name") %>%
  left_join(player_counts, by=c("name", "team_name", "opponent_team")) %>%
  left_join(team_counts, by=c("team_name", "opponent_team")) %>%
  left_join(two4, by=c("name", "team_name")) %>%
  ungroup() %>%
  distinct(name, team_name, opponent_team, GW, .keep_all = T) %>%
  mutate(across(everything(), ~ ifelse(is.na(.), 0, .))) %>%
  mutate(across(everything(), ~ ifelse(is.infinite(.), 0, .))) %>%
  select(-check) %>%
  mutate(played_P=ifelse(total_points==0, played_S, played_P)) %>%
  mutate(played60_P=ifelse(total_points==0, played60_S, played60_P)) %>%
  mutate(goals_scored_P=ifelse(total_points==0, goals_scored_S, goals_scored_P)) %>%
  mutate(goals_conceded_P=ifelse(total_points==0, goals_conceded_S, goals_conceded_P)) %>%
  mutate(assists_P=ifelse(total_points==0, assists_S, assists_P)) %>%
  mutate(clean_sheets_P=ifelse(total_points==0, clean_sheets_S, clean_sheets_P)) %>%
  mutate(saves_P=ifelse(total_points==0, saves_S, saves_P)) %>%
  mutate(yellow_cards_P=ifelse(total_points==0, yellow_cards_S, yellow_cards_P)) %>%
  mutate(red_cards_P=ifelse(total_points==0, red_cards_S, red_cards_P)) %>%
  mutate(red_cards_P=ifelse(total_points==0, red_cards_S, red_cards_P)) %>%
  mutate(penalties_missed_P=ifelse(total_points==0, penalties_missed_S, penalties_missed_P)) %>%
  mutate(penalties_saved_P=ifelse(total_points==0, penalties_saved_S, penalties_saved_P))
  

## Step 7.1: Format the data for prediction using the ols model
temp <- intersect(names(df2), names(df3))
df4 <- df3[, temp] %>%
  select(name, everything()) %>%
  data.frame() %>% 
  left_join(teams, by=c("opponent_team"="team_name"))

### Step 7.2: Get prediction
temp <- df3 %>% select(name, GW, team_name, opponent_team)
prediction <- predict(ols, newdata = df4) %>% data.frame() %>%
  rename(Predicted_points=1) %>% cbind(temp)

    ##### May have a colinearity problem here
      vif(ols) ### Have variables that are perfectly colinear?
      
      temp <- df4 %>%
        mutate(name=as.numeric(factor(name))) %>%
        mutate(team_name=as.numeric(factor(team_name))) %>%
        mutate(opponent_team=as.numeric(factor(opponent_team)))
      temp <- cor(temp)
      
      one <- list()
      
      for (i in 1:ncol(temp)) {
        correlated_vars <- colnames(temp)[temp[, i] == 1]
        one[[colnames(temp)[i]]] <- correlated_vars
      }
      
      print(one)
      
      ###### Not seeing multicolinearity given these results. Will have to
      ###### investigate further in the future as refined
      
      
      
### Step 7.3: Get current form to apply to the predicted points in the application
one <- fpl_get_player_all() %>%
  mutate(name=paste(first_name, second_name, sep=" ")) %>%
  mutate(name=ifelse(grepl("-", name), gsub("-", " ", name), name)) %>%
  mutate(name=stri_trans_general(name, "Latin-ASCII")) %>%
  mutate(transferred = ifelse(grepl("Transfer", news), 1, 0)) %>%
  mutate(transferred = ifelse(grepl("transfer", news),1,transferred)) %>%
  mutate(transferred = ifelse(grepl("Loan", news), 1, transferred)) %>%
  mutate(transferred = ifelse(grepl("loan", news), 1, transferred)) %>%
  mutate(transferred = ifelse(grepl("Left", news), 1, transferred)) %>%
  mutate(out=ifelse(grepl("unknown", news),0,1)) %>%
  mutate(out=ifelse(grepl("Unknown", news), 0, out)) %>%
  mutate(out=ifelse(grepl("expected", news), 0, out)) %>%
  mutate(out=ifelse(grepl("Expected", news), 0, out)) %>%
  mutate(out=ifelse(grepl("25%", news), 0, out)) %>%
  mutate(out=ifelse(grepl("Suspended", news), 0, out)) %>%
  mutate(out=ifelse(grepl("50%", news), 0.5, out)) %>%
  mutate(out=ifelse(grepl("75%", news), 0.85, out)) %>%
  select(name, web_name, form, transferred, out) %>%
  mutate(name=ifelse(grepl("Eddie", name), gsub("Eddie", "Edward", name), name)) %>%
  mutate(name=ifelse(grepl("-", name), gsub("-", " ", name), name))

### Step 7.4: Join to the dataframe, divide form by 10 and multiply by points
final <- prediction %>%   
  left_join(one, by="name") %>%
  left_join(positions, by="name") %>%
  mutate(name=ifelse(is.na(web_name), name, web_name)) %>%
  mutate(Predicted_points=ifelse(is.na(Predicted_points),0,Predicted_points)) %>%
  mutate(Predicted_points=Predicted_points+((form/10)*Predicted_points)) %>%
  mutate(Predicted_points=round(Predicted_points*out)) %>%
  filter(transferred==0) %>%
  select(-web_name, -out, -transferred) %>%
  select(name, position, Predicted_points, opponent_team, everything()) %>%
  mutate(position=ifelse(position=="GK", "GKP", position)) %>%
  group_by(GW) %>%
  mutate(z_score_overall=round((Predicted_points-mean(Predicted_points))/sd(Predicted_points), 2)) %>%
  ungroup() %>% group_by(GW, position) %>%
  mutate(z_score_position=round((Predicted_points-mean(Predicted_points))/sd(Predicted_points), 2)) %>%
  ungroup()
  

write.csv(final, "FPL_Lineup_Optimizer/export_optimizer.csv", row.names = F)

### Validation
temp <- df %>% filter(Season==2024) %>%
  select(name, GW, total_points)

validation <- fpl_get_player_all() %>%
  mutate(name=paste(first_name, second_name, sep=" ")) %>%
  mutate(name=stri_trans_general(name, "Latin-ASCII")) %>%
  select(name, web_name) %>%
  left_join(final, by=c('web_name' = 'name')) %>%
  filter(GW %in% temp$GW) %>%
  select(name, web_name, GW, Predicted_points) %>%
  left_join(temp, by=c("name", "GW")) %>%
  select(web_name, GW, Predicted_points, total_points) %>%
  group_by(web_name) %>%
  summarize(Predicted_points=mean(Predicted_points), total_points=mean(total_points)) %>%
  ungroup() %>%
  mutate(difference=Predicted_points-total_points) %>%
  mutate(difference=ifelse(is.na(difference),0,difference)) %>%
  mutate(row=row_number()) %>% arrange(-total_points)

one <- mean(validation$difference)
metrics <- metrics %>% mutate(`Avg Error`=round(one, 2))
metrics
write.csv(metrics, "metrics.csv", row.names = F)
      
  #### Visualize for further validation
    plot <- ggplot(validation, aes(x=row, y=log(Predicted_points), color="Predicted points")) +
      geom_point() +
      geom_point(aes(y=log(total_points), color="Actual points")) +
      geom_smooth(aes(y=log(Predicted_points), color="Smoothed prediction")) +
      geom_smooth(aes(y=log(total_points), color="Smoothed actual")) +
      ggtitle("Comparing average model performance for 2023/24 Season") +
      xlab("Player") + ylab("Log Points") +
      theme(axis.text.x = element_blank(), legend.title = element_blank())
    dev.new()
    ggsave(plot, filename = "plots/validation.png", width = 13, height = 7)
    plot
    
  #### Rough fit but somewhat consistent error which says the model is at least consistent
  #### Will want to validate further as more data becomes available

