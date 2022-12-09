library(worldfootballR)
library(tidyverse)

# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Set season/country/tier
season <- 2023
country <- "ENG"
tier <- "1st"

# Load functions and data for model ---------------------------------------------------------
double_games <- readRDS("Simulation Backup/Functions/double_games.rds")
compute_league_ranks <- readRDS("Simulation Backup/Functions/compute_league_ranks.rds")
load_schedules <- readRDS("Simulation Backup/Functions/load_schedules.rds")
process_games <- readRDS("Simulation Backup/Functions/process_games.rds")
simulate_round <- readRDS("Simulation Backup/Functions/sim_round.rds")
sim_season <- readRDS("Simulation Backup/Functions/sim_season.rds")


## Load team ratings; update team names to match squad -------------------
priors <- readRDS(glue::glue("Stats/Power Ratings/tm_priors_{country}_{tier}_{season}.rds"))
divisions <- readRDS("Simulation Backup/Schedules/all_divisions.rds")
team_list <- readRDS(glue::glue("Stats/Teams/teams_{country}_{tier}_{season}.rds")) %>% 
  select(-logoURL)

### Create priors for updating model ----------------------------------
priors <- priors %>% 
  mutate(Team = str_remove_all(name, "Team_")) %>% 
  mutate(Team = str_remove_all(Team, "opponent_")) %>% 
  mutate(Team = str_replace_all(Team, "_", " ")) %>% 
  left_join(team_list, by = "Team") %>% 
  mutate(team = case_when(
    is.na(Squad) ~ name,
    TRUE ~ Squad)) %>% 
  relocate(team) %>% 
  select(-c(Team, Squad)) %>% 
  mutate(team = case_when(
    str_detect(name, "Team_") ~ paste0("team_", team),
    str_detect(name, "opponent_") ~ paste0("opponent_", team),
    TRUE ~ name)) %>% 
  mutate(team = str_replace_all(team, " ", "_")) %>% 
  select(-name) %>% 
  rename(name = team)

### Teams data frame with Off/Def/Base/Home field advantage -----------------------
ratings <- priors %>% 
  filter(str_detect(name, "team_")) %>% 
  mutate(team = str_remove_all(name, c("team_")),
         team = str_replace_all(team, "_", " ")) %>% 
  rename(OffRtg = value) %>% 
  left_join(priors %>% 
              filter(str_detect(name, "opponent_")) %>% 
              mutate(team = str_remove_all(name, c("opponent_")),
                     team = str_replace_all(team, "_", " ")) %>% 
              rename(DefRtg = value), by = "team") %>% 
  select(team, OffRtg, DefRtg) %>% 
  mutate(hfa = priors$value[priors$name == "isHome"],
         base = priors$value[priors$name == "(Intercept)"])

## Create poisson model based on xScore -----------------------------------------------------
train_data <- plyr::rbind.fill(readRDS(glue::glue("Stats/Adjusted Data/summary_{country}_{tier}_{season - 1}.rds")),
                               readRDS(glue::glue("Stats/Adjusted Data/summary_{country}_{tier}_{season - 2}.rds")),
                               readRDS(glue::glue("Stats/Adjusted Data/summary_{country}_{tier}_{season - 3}.rds")),
                               readRDS(glue::glue("Stats/Adjusted Data/summary_{country}_{tier}_{season - 4}.rds")))

### Home/away models --------
home.mod <- glm(npG ~ Home_xScore, family = poisson(link = "log"),
                data = train_data %>%
                  rename(Home_xScore = xScore))

away.mod <- glm(npG ~ Away_xScore, family = poisson(link = "log"),
                data = train_data %>%
                  rename(Away_xScore = xScore))

### Get st. deviation of xScore --------
xScore.sd <- train_data %>% 
  mutate(xSc_rt = sqrt(xScore)) %>% 
  summarise(sd = sd(xSc_rt)) %>% 
  as.double()

sched <- load_schedules(country_code = country)

############################################Simulate an entire season#####################################################
test <- sim_season(season = {season},
                   process_games = process_games,
                   country = {country},
                   if_ended_today = FALSE,
                   fresh_season = TRUE,
                   teams = ratings,
                   games = sched,
                   test_week = 3,
                   home.mod = home.mod,
                   away.mod = away.mod,
                   xScore.sd = {xScore.sd},
                   simulations = 1000)

simulate_round(sim_round = 1,
               sims_per_round = 100,
               simulations = 20,
               schedule = sched,
               weeks_to_sim = weeks_to_sim,
               process_games = process_games,
               teams = ratings)
