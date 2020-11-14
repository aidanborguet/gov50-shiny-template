library(shiny) # you may need to install.packages() this
library(tidyverse)
library(devtools)
library(dplyr)
library(gganimate)
library(ggforce)
library(ggplot2)
library(readr)
library(shiny)
library(fec16)
library(patchwork)
library(shinythemes)

source_url("https://raw.githubusercontent.com/asonty/ngs_highlights/master/utils/scripts/data_utils.R")
source_url("https://raw.githubusercontent.com/asonty/ngs_highlights/master/utils/scripts/plot_utils.R")


plot_play_frame <- function(play_data_, frame_, velocities_=F, voronoi_=F, caption_=T) {
  
  if(is.null(play_data_)) {
    print("error: need to provide play data")
    return()
  }
  if(is.null(frame_)) {
    print("error: need to provide frame of play to visualize")
    return()
  }
  
  # * get play metadata ----
  play_desc <- play_data_$playDescription %>% .[1]
  play_dir <- play_data_$playDirection %>% .[1]
  yards_togo <- play_data_$yardsToGo %>% .[1]
  los <- play_data_$absoluteYardlineNumber %>% .[1]
  togo_line <- if(play_dir=="left") los-yards_togo else los+yards_togo
  first_frame <- play_data_ %>%
    filter(event == "line_set") %>% 
    distinct(frame) %>% 
    slice_max(frame) %>% 
    pull()
  final_frame <- play_data_ %>% 
    filter(event == "tackle" | event == "touchdown" | event == "out_of_bounds") %>% 
    distinct(frame) %>% 
    slice_max(frame) %>% 
    pull() + 10
  
  # * separate player and ball tracking data ----
  player_data <- play_data_ %>% 
    filter(frame == frame_) %>% 
    select(frame, homeTeamFlag, teamAbbr, displayName, jerseyNumber, position, positionGroup,
           x, y, s, o, dir, event) %>% 
    filter(displayName != "ball")
  ball_data <- play_data_ %>% 
    filter(frame == frame_) %>% 
    select(frame, homeTeamFlag, teamAbbr, displayName, jerseyNumber, position, positionGroup,
           x, y, s, o, dir, event) %>% 
    filter(displayName == "ball")
  
  # * get team details ----
  h_team <- play_data_ %>% filter(homeTeamFlag == 1) %>% distinct(teamAbbr) %>% pull()
  a_team <- play_data_ %>% filter(homeTeamFlag == 0) %>% distinct(teamAbbr) %>% pull()
  team_colors <- fetch_team_colors(h_team_ = h_team, a_team_ = a_team)
  h_team_color1 <- team_colors[1]
  h_team_color2 <- team_colors[2]
  a_team_color1 <- team_colors[3]
  a_team_color2 <- team_colors[4]
  
  # * compute velocity components ----
  #  velocity angle in radians
  player_data$dir_rad <- player_data$dir * pi / 180
  
  #  velocity components
  player_data$v_x <- sin(player_data$dir_rad) * player_data$s
  player_data$v_y <- cos(player_data$dir_rad) * player_data$s
  
  # * create plot ----
  if (voronoi_ == T) {
    div_team_colors <- fetch_team_colors(h_team_ = h_team, a_team_ = a_team, diverge_ = T)
    
    colors_df <- tibble(a = div_team_colors[1], b = div_team_colors[3])
    colnames(colors_df) <- c(h_team, a_team)
    
    play_frame_plot <- plot_field(field_color = "white", line_color = "#343a40") +
      geom_voronoi_tile(
        data = player_data %>% filter(x >= 0, x <= 120, y >= 0, y <= 160/3), 
        bound = c(0, 120, 0, 160/3),
        mapping = aes(x = x, y = y, fill = teamAbbr, group = -1L),
        colour = "white",
        size = 0.5,
        alpha = 0.5
      ) +
      scale_fill_manual(values = colors_df, name = "Team")
  } else {
    play_frame_plot <- plot_field()
  }
  
  play_frame_plot <- play_frame_plot +
    # line of scrimmage
    annotate(
      "segment",
      x = los, xend = los, y = 0, yend = 160/3,
      colour = "#0d41e1"
    ) +
    # 1st down marker
    annotate(
      "segment",
      x = togo_line, xend = togo_line, y = 0, yend = 160/3,
      colour = "#f9c80e"
    )
  
  if (velocities_ == T) {
    play_frame_plot <- play_frame_plot +
      # away team velocities
      geom_segment(
        data = player_data %>% filter(teamAbbr == a_team),
        mapping = aes(x = x, y = y, xend = x + v_x, yend = y + v_y),
        colour = a_team_color1, size = 1, arrow = arrow(length = unit(0.01, "npc"))
      ) + 
      # home team velocities
      geom_segment(
        data = player_data %>% filter(teamAbbr == h_team),
        mapping = aes(x = x, y = y, xend = x + v_x, yend = y + v_y),
        colour = h_team_color1, size = 1, arrow = arrow(length = unit(0.01, "npc"))
      ) 
  }
  
  play_frame_plot <- play_frame_plot +
    # away team locs and jersey numbers
    geom_point(
      data = player_data %>% filter(teamAbbr == a_team),
      mapping = aes(x = x, y = y),
      fill = "#ffffff", color = a_team_color2,
      shape = 21, alpha = 1, size = 6
    ) +
    geom_text(
      data = player_data %>% filter(teamAbbr == a_team),
      mapping = aes(x = x, y = y, label = jerseyNumber),
      color = a_team_color1, size = 3.5, #family = "mono"
    ) +
    # home team locs and jersey numbers
    geom_point(
      data = player_data %>% filter(teamAbbr == h_team),
      mapping = aes(x = x, y = y),
      fill = h_team_color1, color = h_team_color2,
      shape = 21, alpha = 1, size = 6
    ) +
    geom_text(
      data = player_data %>% filter(teamAbbr == h_team),
      mapping = aes(x = x, y = y, label = jerseyNumber),
      color = h_team_color2, size = 3.5, #family = "mono"
    ) +
    # ball
    geom_point(
      data = ball_data,
      mapping = aes(x = x, y = y),
      fill = "#935e38", color = "#d9d9d9",
      shape = 21, alpha = 1, size = 4
    ) +
    NULL
  
  play_frame_plot <- play_frame_plot +
    labs(
      subtitle = paste("Frame: ", frame_)
    )
  
  if (caption_ == T) {
    play_frame_plot <- play_frame_plot +
      labs(
        caption = "Source: NFL Next Gen Stats"
      )
  }
  
  return(play_frame_plot)
}


bal_highlights <- fetch_highlights_list(team_ = "BAL", season_ = 2019)


teams <- c(
  "ARI", "ATL", "BAL", "BUF", "CAR", "CHI", "CIN", "CLE",
  "DAL", "DEN", "DET", "GB", "HOU", "IND", "JAX", "KC",
  "LA", "LAC", "MIA", "MIN", "NE", "NO", "NYG", "NYJ", 
  "OAK", "PHI", "PIT", "SEA", "SF", "TB", "TEN", "WAS",
  "AFC", "NFC", "LV"
)

plays <- tibble(teams = teams, 
       highlights_2018 = map(teams, ~ fetch_highlights_list(team_ = ., season_ = 2018)), 
       highlights_2019 = map(teams, ~ fetch_highlights_list(team_ = ., season_ = 2019)))
plays_2018 <- plays %>%
  unnest(highlights_2018) %>%
  select(teams:playId)

plays_2019 <- plays %>%
  unnest(highlights_2019) %>%
  select(teams:playId)

master_highlights <- bind_rows(plays_2018, plays_2019)

master_h <- master_highlights %>%
  mutate(touchdown = str_detect(playDesc, "TOUCHDOWN")) %>%
  mutate(left_play = str_detect(playDesc, "left")) %>%
  mutate(right_play = str_detect(playDesc, "right")) %>%
  mutate(middle_play = str_detect(playDesc, "middle"))
 # mutate(pass_play = str_detect(playDesc, "pass")) %>%
 # mutate(run_play = str_detect(playDesc, "run"))

master_h %>%
  group_by()
