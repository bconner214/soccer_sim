library(worldfootballR)
library(tidyverse)

# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Negate in formula
`%!in%` = Negate(`%in%`)

# Set season/country/tier
season <- 2023
country <- "ENG"
tier <- "1st"

# Double games function ------------------------------------------------------
double_games <- function(g){
  g1 <- g %>%
    rename(team = Away, G=AwayGoals,
           opponent = Home, opp_G = HomeGoals) %>%
    mutate(isHome = FALSE,
           result=-1*result)
  g2 <- g %>%
    rename(team = Home, G = HomeGoals,
           opponent = Away, opp_G = AwayGoals) %>% 
    mutate(isHome = TRUE)
  g <- bind_rows(g1,g2) %>%
    arrange(Date, Time, isHome) %>% 
    mutate(outcome = case_when(
      result > 0 ~ 1,
      result == 0 ~ 0.5,
      result < 0 ~ 0
    ))
  return(g)
}

saveRDS(double_games, "Functions/double_games.rds")

# Compute Ranks -----------------------------------------------------------------------------
compute_league_ranks <- function(games,
                                 teams = NULL,
                                 .debug = FALSE) {
  
  required_vars <- c(
    "sim",
    "Wk",
    "Away",
    "Home",
    "result"
  )
  
  if (!sum(names(games) %in% required_vars, na.rm = TRUE) >= 5 | !is.data.frame(games)) {
    stop(
      "The argument `games` has to be a data frame including ",
      "all of the following variables: ",
      glue::glue_collapse(required_vars, sep = ", ", last = " and "),
      "!"
    )
  }
  
  if (is.null(teams)) { # compute teams df from games df
    pivot_games <- games %>%
      select(sim, Home, Away) %>%
      pivot_longer(cols = c("Home", "Away"), values_to = "team") %>%
      select(sim, team)
    
    teams <- bind_rows(
      data.frame(team = unique(games$Away)),
      data.frame(team = unique(games$Home))
    ) %>%
      distinct() %>%
      mutate(country = {country}, tier = {tier}) %>%
      left_join(pivot_games, by = "team") %>%
      select(sim, everything()) %>%
      distinct() %>%
      arrange(country, team, sim)
  }
  
  # double games
  games_doubled <- double_games(games)
  
  # record of each team
  teams <- teams %>%
    inner_join(games_doubled, by = c("sim", "team")) %>%
    group_by(sim, country, tier, team) %>%
    summarize(
      games = n(),
      wins = sum(outcome),
      true_wins = sum(outcome == 1),
      losses = sum(outcome == 0),
      draws = sum(outcome == 0.5),
      points = (3 * true_wins) + draws,
      G = sum(G),
      GA = sum(opp_G)
    ) %>%
    mutate(GD = G - GA) %>%
    ungroup() %>% 
    arrange(sim, country, tier, desc(points), desc(G), desc(GD)) %>% 
    with_groups(.groups = c(sim, country, tier), mutate, Rk = row_number())
  
  max_week <- max(games$Wk, na.rm = TRUE)
  
  return(list(standings = teams))
}

saveRDS(compute_league_ranks, "Functions/compute_league_ranks.rds")

# Load schedules -------------------------------------------------------------------------------
load_schedules <- function(country_code = "ENG"){
  # catch invalid input
  if (!isTRUE(country_code %in% c("ENG", "ESP", "FRA", "GER", "ITA"))) {
    stop(
      "The argument `country` has to be",
      "one of the following:",
      "ENG, ESP, FRA, GER, ITA"
    )
  }
  
  schedule <- readRDS(glue::glue("Simulation Backup/Schedules/schedules_{country_code}.rds"))
  
  return(schedule)
}

saveRDS(load_schedules, "Simulation Backup/Functions/load_schedules.rds")

# Update ratings function ----------------------------------------------------------------------------
#update_ratings <- function(df, seed, wgt, priors){
#  # Create model data frame
#  model.data <- df %>%
#    select(xScore = Home_xScore, Date, team = Home, opponent = Away) %>% 
#    mutate(isHome = TRUE) %>% 
#    bind_rows(df %>% 
#                select(xScore = Away_xScore, Date, team = Away, opponent = Home) %>% 
#                mutate(isHome = FALSE)) %>% 
#    filter(!is.na(xScore)) %>% 
#    with_groups(.groups = team, arrange, desc(Date)) %>% 
#    with_groups(.groups = team, mutate,
#                game = row_number(),
#                weight = {wgt} ^ game) %>% 
#    arrange(Date) %>% 
#    select(-c(game, Date)) %>% 
#    # Add underscores because I hate spaces in titles
#    mutate(team = str_replace_all(team, " ", "_"),
#           opponent = str_replace_all(opponent, " ", "_")) %>%
#    # Create dummy columns for all teams
#    fastDummies::dummy_cols(select_columns = c("team", "opponent"), remove_first_dummy = FALSE) %>% 
#    # Remove team and opponent names
#    select(-c(team, opponent)) %>% 
#    relocate(weight, .after = last_col())
  
  
#  # select y and x
#  y <- model.data$xScore
#  
#  x <- data.matrix(model.data[, 2:(length(model.data)-1)])
#  
#  weight <- model.data$weight
#  
#  # Create glmnet model
#  model <- glmnet::glmnet(x, y, weights = weight, alpha = 0)
#  
#  #perform k-fold cross-validation to find optimal lambda value
#  set.seed(241)
#  
#  cv_model <- glmnet::cv.glmnet(x, y, weights = weight, alpha = 0)
#  
#  #find optimal lambda value that minimizes test MSE
#  best_lambda <- cv_model$lambda.min
#  
#  # Use best lambda to create model
#  best_model <- glmnet::glmnet(x, y, weights = weight, alpha = 0, lambda = best_lambda)
#  
#  # Convert coeffecient values into data frame
#  raw.coeff <- as.data.frame(as.matrix(coef(best_model))) %>% 
#   rownames_to_column(var = "name")
#  
#  names(raw.coeff) <- c("name", "value")
#  
#  # Create data frame for weights by game
#  weight.df <- data.frame(game_no = seq.int(from = 1,
#                                            to = (length(teams$Team)-1) * 2,
#                                            by = 1)) %>% 
#    mutate(weight = {wgt} ^ game_no,
#           weight = weight / sum(weight),
#           cum_weight = cumsum(weight))
#  
#  
#  currents <- raw.coeff %>% 
#    mutate(cum_weight = weight.df$cum_weight[weight.df$game_no == week_num])
#  
#  tm_ratings <- currents %>% 
#    # Add priors with weights
#    rbind(priors %>% 
#            with_groups(name, mutate, cum_weight = 1 - sum(currents$cum_weight[currents$name == name]))) %>%
#    # Summarize based on weighted mean
#    with_groups(name, summarise, value = weighted.mean(x = value, w = cum_weight))
#  
#  return(tm_ratings)
#}
#
# saveRDS(update_ratings, "Functions/update_ratings.rds")

# Process games function ----------------------------------------------------------
process_games <- function(teams, games, week_num, ...) {
  # Required for teams data frame: 
  # OffRtg; DefRtg; hfa; base
  # Also include in params:
  # home.mod; away.mod; standard deviation (sqrt(xScore))
  
  
  # pull ratings from teams data
  ratings <- teams %>% select(sim, team, OffRtg, DefRtg, hfa, base)
  
  # mark estimate, wp, and result for games
  games <- games %>%
    dplyr::inner_join(ratings %>% select(-hfa), by = c("sim" = "sim", "Away" = "team")) %>%
    dplyr::rename(away_off = OffRtg, away_def = DefRtg) %>%
    dplyr::inner_join(ratings %>% select(-base), by = c("sim" = "sim", "Home" = "team")) %>%
    dplyr::rename(home_off = OffRtg, home_def = DefRtg) %>% 
    dplyr::mutate(home_base = home_off + away_def + base + hfa,
                  away_base = away_off + home_def + base) %>%
    dplyr::mutate(home_base = sqrt(home_base),
                  away_base = sqrt(away_base)) %>% 
    dplyr::mutate(Home_xScore = case_when(
      is.na(Home_xScore) & Wk == week_num  ~ (rnorm(n(), mean = home_base, sd = {xScore.sd})^2),
      TRUE ~ Home_xScore),
      Away_xScore = case_when(
        is.na(Away_xScore) & Wk == week_num  ~ (rnorm(n(), mean = away_base, sd = {xScore.sd})^2),
        TRUE ~ Away_xScore)) %>% 
    dplyr::mutate(predG_home = predict(home.mod, ., type = "response"),
                  predG_away = predict(away.mod, ., type = "response")) %>% 
    dplyr::mutate(homeprob_0 = dpois(0, lambda = predG_home), homeprob_1 = dpois(1, lambda = predG_home),
                  homeprob_2 = dpois(2, lambda = predG_home), homeprob_3 = dpois(3, lambda = predG_home),
                  homeprob_4 = dpois(4, lambda = predG_home), homeprob_5 = dpois(5, lambda = predG_home),
                  homeprob_6 = dpois(6, lambda = predG_home), homeprob_7 = dpois(7, lambda = predG_home),
                  awayprob_0 = dpois(0, lambda = predG_away), awayprob_1 = dpois(1, lambda = predG_away),
                  awayprob_2 = dpois(2, lambda = predG_away), awayprob_3 = dpois(3, lambda = predG_away),
                  awayprob_4 = dpois(4, lambda = predG_away), awayprob_5 = dpois(5, lambda = predG_away),
                  awayprob_6 = dpois(6, lambda = predG_away), awayprob_7 = dpois(7, lambda = predG_away)) %>%
    dplyr::mutate_at(vars(homeprob_0:awayprob_7), ~replace_na(., .01)) %>% 
    dplyr::with_groups(.groups = c(Home, Away), mutate, 
                       HomeGoals = case_when(
                         !is.na(HomeGoals) ~ HomeGoals,
                         is.na(HomeGoals) & Wk == week_num ~ sample(seq.int(from = 0, to = 7, by = 1), size = 1,
                                                                    replace = TRUE,
                                                                    prob = c(homeprob_0, homeprob_1, homeprob_2,
                                                                             homeprob_3, homeprob_4, homeprob_5,
                                                                             homeprob_6, homeprob_7))),
                       AwayGoals = case_when(
                         !is.na(AwayGoals) ~ AwayGoals,
                         is.na(AwayGoals) & Wk == week_num ~ sample(seq.int(from = 0, to = 7, by = 1), size = 1,
                                                                    replace = TRUE,
                                                                    prob = c(awayprob_0, awayprob_1, awayprob_2,
                                                                             awayprob_3, awayprob_4, awayprob_5,
                                                                             awayprob_6, awayprob_7)))) %>% 
    dplyr::mutate(result = HomeGoals - AwayGoals) %>% 
    dplyr::mutate(off_adj_home = Home_xScore - base - hfa - away_def,
                  def_adj_home = Away_xScore - base - home_def,
                  off_adj_away = Away_xScore - base - home_def,
                  def_adj_away = Home_xScore - base - hfa - away_def) %>% 
    dplyr::select(-c(away_off, home_off, home_def, away_def, predG_home, predG_away, base, hfa, home_base, away_base,
                     contains("homeprob_"), contains("awayprob_")))
  
  # Set weight based on week number
  wgt <- ifelse(week_num <= 3, 0.999, 0.97)
  
  
  weight.df <- data.frame(game_no = seq.int(from = 1,
                                            to = (length(teams$team)-1) * 2,
                                            by = 1)) %>% 
    mutate(weight = {wgt} ^ game_no,
           weight = weight / sum(weight),
           cum_weight = cumsum(weight)) %>% 
    filter(game_no == 1) %>% 
    select(cum_weight) %>% 
    as.numeric()
  
  # apply OffRtg and DefRtg shifts
  teams <- teams %>%
    dplyr::left_join(games %>%
                       filter(Wk == week_num) %>%
                       select(sim, Away, off_adj = off_adj_away, def_adj = def_adj_away),
                     by = c("sim" = "sim", "team" = "Away")) %>%
    dplyr::mutate(weight = weight.df,
                  OffRtg = ifelse(!is.na(off_adj), ((1 - weight) * OffRtg) + (weight * off_adj), OffRtg),
                  DefRtg = ifelse(!is.na(def_adj), ((1 - weight) * DefRtg) + (weight * def_adj), DefRtg)) %>%
    dplyr::select(-c(off_adj, def_adj, weight)) %>%
    dplyr::left_join(games %>%
                       filter(Wk == week_num) %>%
                       select(sim, Home, off_adj = off_adj_home, def_adj = def_adj_home),
                     by = c("sim" = "sim", "team" = "Home")) %>%
    dplyr::mutate(weight = weight.df,
                  OffRtg = ifelse(!is.na(off_adj), ((1 - weight) * OffRtg) + (weight * off_adj), OffRtg),
                  DefRtg = ifelse(!is.na(def_adj), ((1 - weight) * DefRtg) + (weight * def_adj), DefRtg)) %>%
    dplyr::select(-c(off_adj, def_adj, weight))
  
  # remove elo shift
  games <- games %>%
    dplyr::select(-c(off_adj_home, def_adj_home,
                     off_adj_away, def_adj_away))
  
  return(list(teams = teams, games = games))
}

saveRDS(process_games, "Functions/process_games.rds")

# Simulate season ---------------------------------------------------------------------------
sim_season <- function(season = NULL,
                       process_games = NULL,
                       country = NULL,
                       ...,
                       if_ended_today = FALSE,
                       fresh_season = FALSE,
                       test_week = NULL,
                       simulations = 1000,
                       sims_per_round = max(ceiling(simulations / future::availableCores() * 2), 100),
                       .debug = FALSE,
                       print_summary = FALSE) {
  
  # Define simple estimate and simulate functions
  
  if (is.null(process_games)) {
    process_games <- function(teams, games, week_num, ...) {
      # teams = teams data
      # games = games data
      #
      # this example estimates at PK/0 and 50%
      # estimate = is the median spread expected (positive = home team favored)
      # wp = is the probability of the team winning the game
      #
      # only simulate games through week week_num
      # only simulate games with is.na(result)
      # result = how many points home team won by
      
      # round out (away from zero)
      round_out <- function(x) {
        x[!is.na(x) & x < 0] <- floor(x[!is.na(x) & x < 0])
        x[!is.na(x) & x > 0] <- ceiling(x[!is.na(x) & x > 0])
        return(x)
      }
      
      # get elo if not in teams data already
      if (!("elo" %in% colnames(teams))) {
        args <- list(...)
        if ("elo" %in% names(args)) {
          # pull from custom arguments
          teams <- teams %>%
            dplyr::inner_join(args$elo %>% select(team, elo), by = c("team" = "team"))
        } else {
          # start everyone at a random default elo
          ratings <- tibble(
            team = unique(teams$team),
            elo = rnorm(length(unique(team)), 1500, 150)
          )
          teams <- teams %>%
            dplyr::inner_join(ratings, by = "team")
        }
      }
      
      # pull ratings from teams data
      ratings <- teams %>% select(sim, team, elo)
      
      # mark estimate, wp, and result for games
      games <- games %>%
        dplyr::inner_join(ratings, by = c("sim" = "sim", "away_team" = "team")) %>%
        dplyr::rename(away_elo = elo) %>%
        dplyr::inner_join(ratings, by = c("sim" = "sim", "home_team" = "team")) %>%
        dplyr::rename(home_elo = elo) %>%
        dplyr::mutate(
          elo_diff = home_elo - away_elo,
          elo_diff = elo_diff + ifelse(location == "Home", 20, 0),
          elo_diff = elo_diff + (home_rest - away_rest) / 7 * 25,
          elo_diff = elo_diff * ifelse(game_type == "REG", 1, 1.2),
          wp = 1 / (10^(-elo_diff / 400) + 1),
          estimate = elo_diff / 25,
          result = case_when(
            is.na(result) & week == week_num ~
              as.integer(round_out(rnorm(n(), estimate, 13))),
            TRUE ~ as.integer(result)
          ),
          outcome = case_when(
            is.na(result) ~ NA_real_,
            result > 0 ~ 1,
            result < 0 ~ 0,
            TRUE ~ 0.5
          ),
          elo_input = case_when(
            is.na(result) ~ NA_real_,
            result > 0 ~ elo_diff * 0.001 + 2.2,
            result < 0 ~ -elo_diff * 0.001 + 2.2,
            TRUE ~ 1.0,
          ),
          elo_mult = log(pmax(abs(result), 1) + 1.0) * 2.2 / elo_input,
          elo_shift = 20 * elo_mult * (outcome - wp)
        ) %>%
        dplyr::select(
          -away_elo, -home_elo, -elo_diff, -wp, -estimate,
          -outcome, -elo_input, -elo_mult
        )
      
      # apply elo shifts
      teams <- teams %>%
        dplyr::left_join(games %>%
                           filter(week == week_num) %>%
                           select(sim, away_team, elo_shift),
                         by = c("sim" = "sim", "team" = "away_team")
        ) %>%
        dplyr::mutate(elo = elo - ifelse(!is.na(elo_shift), elo_shift, 0)) %>%
        dplyr::select(-elo_shift) %>%
        dplyr::left_join(games %>%
                           filter(week == week_num) %>%
                           select(sim, home_team, elo_shift),
                         by = c("sim" = "sim", "team" = "home_team")
        ) %>%
        dplyr::mutate(elo = elo + ifelse(!is.na(elo_shift), elo_shift, 0)) %>%
        dplyr::select(-elo_shift)
      
      # remove elo shift
      games <- games %>%
        dplyr::select(-elo_shift)
      
      return(list(teams = teams, games = games))
    }
  }
  
  # Catch invalid input
  
  if (!is.function(process_games)) {
    stop("The parameter `process_games` has to be a function!")
  }
  
  if (season < 2018) {
    stop("The earliest season that can be simulated is 2018.")
  }
  
  #### LOAD DATA ####
  
  # load games data
  print("Loading games data")
  schedule <- load_schedules(country_code = country) 
  
  if (is.null(season)) {
    season <- max(schedule$Season_End_Year)
  }
  
  schedule <- schedule %>%
    filter(Season_End_Year == season) %>%
    select(-Season_End_Year)
  
  if (nrow(schedule) == 0)
  {
    tryCatch({
      schedule <- worldfootballR::fb_match_results(country = {country},
                                                   gender = "M",
                                                   season_end_year = {season},
                                                   tier = "1st") %>% 
        mutate(result = HomeGoals - AwayGoals)
    }, error = function(cond) {
      stop("Unable to locate a schedule for ", season)
    })
  }
  
  #### PREPROCESSING ####
  
  # if simulating fresh season, clear out all results and playoff games
  if (isTRUE(fresh_season)) {
    schedule <- schedule %>%
      mutate(result = NA_real_,
             HomeGoals = NA_real_,
             Home_xG = NA_real_,
             Home_xScore = NA_real_,
             AwayGoals = NA_real_,
             Away_xG = NA_real_,
             Away_xScore = NA_real_)
  }
  
  # if ended today just needs one simulation
  if (isTRUE(if_ended_today)) {
    schedule <- schedule %>%
      filter(!is.na(result))
    simulations <- 1
  }
  
  # weeks to sim
  weeks_to_sim <- schedule %>%
    filter(is.na(result)) %>%
    pull(Wk) %>%
    unique() %>%
    sort()
  
  #### SET UP SIMULATIONS ####
  sim_rounds <- ceiling(simulations / sims_per_round)
  if (!is.null(test_week)) {
    sim_rounds <- 1
  }
  
  if (sim_rounds > 1 && is_sequential()) {
    sim_info(c(
      "Computation in multiple rounds can be accelerated with parallel processing.",
      "You should consider calling a `future::plan()`. Please see the function documentation for further information.",
      "Will go on sequentially..."
    ))
  }
  
  print(glue::glue("Beginning simulation of {simulations} seasons in {sim_rounds} {ifelse(sim_rounds == 1, 'round', 'rounds')}"))
  
  p <- progressr::progressor(along = seq_len(sim_rounds))
  
  run <- quote({
    all <- furrr::future_map(
      .x = seq_len(sim_rounds),
      .f = simulate_round,
      sim_rounds = sim_rounds,
      sims_per_round = sims_per_round,
      schedule = schedule,
      simulations = simulations,
      weeks_to_sim = weeks_to_sim,
      process_games = process_games,
      ...,
      test_week = test_week,
      .debug = .debug,
      p = p,
      .options = furrr::furrr_options(seed = TRUE)
    )
  })
  
  if (isTRUE(.debug)) eval(run) else suppressMessages(eval(run))
  
  if (!is.null(test_week)) {
    print(glue::glue(
      "Aborting and returning your `process_games` function's results from Week {test_week}"
    ))
    return(all[[1]])
  }
  
  print("Combining simulation data")
  
  all_teams <- furrr::future_map_dfr(all, ~ .x$teams)
  all_games <- furrr::future_map_dfr(all, ~ .x$games)
  
  print("Aggregating across simulations")
  
  # Set qualification levels for countries
  CLqual <- case_when(
    country %in% c("ENG", "ESP", "ITA", "GER") ~ 4,
    country %in% c("FRA") ~ 3
  )
  
  releg <- case_when(
    country %in% c("ENG", "ESP", "ITA", "GER") ~ 3,
    country %in% c("FRA") & season == 2023 ~ 4,
    country %in% c("FRA") & season < 2023 ~ 3
  )
  
  overall <- all_teams %>%
    group_by(conf, division, team) %>%
    summarize(
      wins = mean(true_wins),
      points = mean(points),
      G = mean(G),
      GA = mean(GA),
      GD = G - GA,
      win_lg = mean(Rk == 1),
      CL = mean(Rk >= CLqual),
      relegated = mean(Rk < (length(teams$team) - releg))
    ) %>%
    ungroup()
  
  team_wins <-
    tibble(
      team = rep(sort(unique(all_teams$team)), each = max(all_teams$games) * 2 + 1),
      wins = rep(seq(0, max(all_teams$games), 0.5), length(unique(all_teams$team)))
    ) %>%
    inner_join(
      all_teams %>% select(team, true_wins),
      by = c("team")
    ) %>%
    group_by(team, wins) %>%
    summarize(
      over_prob = mean(true_wins > wins),
      under_prob = mean(true_wins < wins)
    ) %>%
    ungroup()
  
  game_summary <-
    all_games %>%
    group_by(Wk, Away, Home) %>%
    summarise(
      away_wins = sum(result < 0),
      home_wins = sum(result > 0),
      draws = sum(result == 0),
      result = mean(result),
      # != number of simulations in the postseason
      games_played = away_wins + home_wins + draws,
      away_percentage = (away_wins + 0.5 * ties) / games_played,
      home_percentage = (home_wins + 0.5 * ties) / games_played
    ) %>%
    ungroup() %>%
    arrange(Wk)
  
  if (isTRUE(print_summary)) print(overall)
  
  out <- structure(
    list(
      "teams" = all_teams,
      "games" = all_games,
      "overall" = overall,
      "team_wins" = team_wins,
      "game_summary" = game_summary,
      "sim_params" = list(
        "season" = season,
        "if_ended_today" = if_ended_today,
        "fresh_season" = fresh_season,
        "test_week" = test_week,
        "simulations" = simulations,
        "sims_per_round" = sims_per_round,
        ".debug" = .debug,
        "print_summary" = print_summary,
        "sim_include" = sim_include
      )
    ),
    class = "soccer_simulation"
  )
  
  out
  }

saveRDS(sim_season, "Simulation Backup/Functions/sim_season.rds")


# Simulate round/week functions ---------------------------------------
simulate_round <- function(sim_round = 1,
                           sim_rounds,
                           sims_per_round,
                           schedule,
                           simulations,
                           weeks_to_sim,
                           process_games,
                           ...,
                           test_week,
                           .debug,
                           p) {
  
  # iteration sims
  iter_sims <- sims_per_round * (sim_round - 1) + seq_len(sims_per_round)
  iter_sims <- iter_sims[iter_sims <= simulations]
  iter_sims_num <- length(iter_sims)
  
  # games have copies per sim
  sched_rows <- nrow(schedule)
  games <- schedule[rep(seq_len(sched_rows), each = iter_sims_num), ] %>%
    mutate(sim = rep(iter_sims, sched_rows)) %>%
    select(sim, everything())
  
  # teams starts as divisions data
  teams <- divisions %>%
    select(-Team) %>% 
    filter(team %in% schedule$Away | team %in% schedule$Home)
  teams <- teams[rep(seq_len(nrow(teams)), iter_sims_num), ] %>%
    mutate(sim = rep(iter_sims, each = nrow(teams))) %>%
    select(sim, everything())
  
  # function to simulate a week
  simulate_week <- function(teams, games, week_num, test_week, ...) {
    
    # recall old data for comparison
    old_teams <- teams
    old_games <- games %>%
      rename(.old_result = result)
    
    # estimate and simulate games
    return_value <- process_games(teams, games, week_num, ...)
    
    # testing?
    if (!is.null(test_week) && week_num == test_week) {
      return(return_value)
    }
    
    # did we get the right data back?
    problems <- c()
    if (typeof(return_value) != "list") {
      problems[length(problems) + 1] <- "the returned value was not a list"
    } else {
      if (!("teams" %in% names(return_value))) {
        problems[length(problems) + 1] <- "`teams` was not in the returned list"
      } else {
        teams <- return_value$teams
        if (!is_tibble(teams)) {
          problems[length(problems) + 1] <- "`teams` was not a tibble"
        } else {
          if (nrow(teams) != nrow(old_teams)) {
            problems[length(problems) + 1] <- paste(
              "`teams` changed from", nrow(old_teams), "to",
              nrow(teams), "rows",
              collapse = " "
            )
          }
          for (cname in colnames(old_teams)) {
            if (!(cname %in% colnames(teams))) {
              problems[length(problems) + 1] <- paste(
                "`teams` column `", cname, "` was removed"
              )
            }
          }
        }
      }
      if (!("games" %in% names(return_value))) {
        problems[length(problems) + 1] <- "`games` was not in the returned list"
      } else {
        games <- return_value$games
        if (!is_tibble(games)) {
          problems[length(problems) + 1] <- "`games` was not a tibble"
        } else {
          if (nrow(games) != nrow(old_games)) {
            problems[length(problems) + 1] <- paste(
              "`games` changed from", nrow(old_games), "to",
              nrow(games), "rows",
              collapse = " "
            )
          }
          for (cname in colnames(old_games)) {
            if (!(cname %in% colnames(games)) && cname != ".old_result") {
              problems[length(problems) + 1] <- paste(
                "`teams` column `", cname, "` was removed"
              )
            }
          }
        }
      }
    }
    
    # report data structure problems
    problems <- paste(problems, collapse = ", ")
    if (problems != "") {
      stop(
        "During Week ", week_num, ", your `process_games()` function had the ",
        "following issues: ", problems, ". "
      )
    }
    
    # identify improper results values
    problems <- old_games %>%
      inner_join(games, by = intersect(colnames(old_games), colnames(games))) %>%
      mutate(problem = case_when(
        week == week_num & is.na(result) ~
          "a result from the current week is missing",
        week != week_num & !is.na(.old_result) & is.na(result) ~
          "a known result outside the current week was blanked out",
        week != week_num & is.na(.old_result) & !is.na(result) ~
          "a result outside the current week was entered",
        week != week_num & .old_result != result ~
          "a known result outside the current week was updated",
        !is.na(.old_result) & is.na(result) ~
          "a known result was blanked out",
        TRUE ~ NA_character_
      )) %>%
      filter(!is.na(problem)) %>%
      pull(problem) %>%
      unique() %>%
      paste(collapse = ", ")
    
    # report result value problems
    if (problems != "") {
      stop(
        "During Week ", week_num, ", your `process_games()` function had the",
        "following issues: ", problems, ". Make sure you only change results ",
        "when week == week_num & is.na(result)"
      )
    }
    
    return(list(teams = teams, games = games))
  }
  
  # simulate remaining regular season games
  for (week_num in weeks_to_sim)
  {
    return_value <-
      simulate_week(teams, games, week_num, test_week, ...)
    if (!is.null(test_week) && week_num == test_week) {
      return(return_value)
    }
    list[teams, games] <- return_value
  }
  
  #### FIND DIVISIONAL STANDINGS AND PLAYOFF SEEDINGS ####
  
  standings_and_h2h <- games %>%
    compute_league_ranks(
      tiebreaker_depth = tiebreaker_depth,
      .debug = .debug
    )
  
  teams <- teams %>%
    inner_join(standings_and_h2h$Rk,
               by = intersect(colnames(teams), colnames(standings_and_h2h$Rk))
    )
  h2h_df <- standings_and_h2h$h2h
  
  if (!is_tibble(teams)){teams <- teams$Rk
    teams <- teams %>%
      dplyr::select(
        dplyr::any_of(c(
          "sim", "team", "country", "tier", "games",
          "wins", "true_wins", "losses", "draws", "points", "G",
          "GA", "GD"
        ))
      )
  }
  
  p(sprintf("finished sim round %g", sim_round))
  
  list("teams" = teams, "games" = games)
}

saveRDS(simulate_round, "Simulation Backup/Functions/sim_round.rds")
