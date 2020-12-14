library(shiny) 
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
library(rstanarm)
library(gtsummary)
library(broom.mixed)
library(gt)

source("final_proj_code2.R")
source_url("https://raw.githubusercontent.com/asonty/ngs_highlights/master/utils/scripts/data_utils.R")
source_url("https://raw.githubusercontent.com/asonty/ngs_highlights/master/utils/scripts/plot_utils.R")

# these here were used to select the plays that I needed for the plots
bal_highlights <- fetch_highlights_list(team_ = "BAL", season_ = 2019)
bal_play_data <- fetch_play_data(playKey_ = 242)

kc_highlights <- fetch_highlights_list(team_ = "KC", season_ = 2019)
kc_play_data <- fetch_play_data(playKey_ = 370)

no_highlights <- fetch_highlights_list(team_ = "NO", season_ = 2019)
no_play_data <- fetch_play_data(playKey_ = 449)



plot_play_frame <- function(play_data_, frame_, velocities_=F, voronoi_=F, caption_=T) {
  
  if(is.null(play_data_)) {
    print("error: need to provide play data")
    return()
  }
  if(is.null(frame_)) {
    print("error: need to provide frame of play to visualize")
    return()
  }
  
  
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
  
  
  h_team <- play_data_ %>% filter(homeTeamFlag == 1) %>% distinct(teamAbbr) %>% pull()
  a_team <- play_data_ %>% filter(homeTeamFlag == 0) %>% distinct(teamAbbr) %>% pull()
  team_colors <- fetch_team_colors(h_team_ = h_team, a_team_ = a_team)
  h_team_color1 <- team_colors[1]
  h_team_color2 <- team_colors[2]
  a_team_color1 <- team_colors[3]
  a_team_color2 <- team_colors[4]
  
  
  player_data$dir_rad <- player_data$dir * pi / 180
  
  
  player_data$v_x <- sin(player_data$dir_rad) * player_data$s
  player_data$v_y <- cos(player_data$dir_rad) * player_data$s
  
  
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
    
    annotate(
      "segment",
      x = los, xend = los, y = 0, yend = 160/3,
      colour = "#0d41e1"
    ) +
    
    annotate(
      "segment",
      x = togo_line, xend = togo_line, y = 0, yend = 160/3,
      colour = "#f9c80e"
    )
  
  if (velocities_ == T) {
    play_frame_plot <- play_frame_plot +
      
      geom_segment(
        data = player_data %>% filter(teamAbbr == a_team),
        mapping = aes(x = x, y = y, xend = x + v_x, yend = y + v_y),
        colour = a_team_color1, size = 1, arrow = arrow(length = unit(0.01, "npc"))
      ) + 
      
      geom_segment(
        data = player_data %>% filter(teamAbbr == h_team),
        mapping = aes(x = x, y = y, xend = x + v_x, yend = y + v_y),
        colour = h_team_color1, size = 1, arrow = arrow(length = unit(0.01, "npc"))
      ) 
  }
  
  play_frame_plot <- play_frame_plot +
    
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


#Panels where I showed graphs and explained with text

ui <- fluidPage(
  navbarPage(
  "NFL's Best Plays",
  theme = shinytheme("slate"),
  tabPanel("Play Graphics",
    sidebarPanel(
      h3("Welcome to my Football Analysis"),
      p("This data shows successful NFL plays that occured at different points
        in the 2019 football season. This data is used by teams and statisticians
        around the league. It helps teams determine which plays worked best in different
        situations. The graphs will reveal what kinds of positions each player
        was in when a successful play was made, which can give you an idea
       as to what plays may be good for them in the future."), 
      
      tags$img(src = "https://www.psdcovers.com/wp-content/uploads/2012/07/NFL-vector-logos.jpg", height = 160, width = 200)
   # picture of the NFL logo
     ),
    
    #the order in which the tab panels are put in here correlates to the app
    mainPanel(
      #These were the videos that did not work
     # HTML('<iframe width="400" height="215" src="https://www.youtube.com/embed/n2Z29ukhcl0" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'), 
     # HTML('<iframe width="400" height="215" src="https://www.youtube.com/embed/Iif2NWLiZZI" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
     # HTML('<iframe width="400" height="215" src="https://www.youtube.com/embed/0EPfo2daaHA" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
     #tags$video(id="video2", type = "video/mp4", src = "sample_video.mp4", controls = "controls", height = 200, width = 200),
      
      plotOutput("bal_plot"),
      p("The plot above shows a play run by the Baltimore that went for a touchdown. 
        The arrows show the directions of the players during the sequence. 
        The brown dot represents the football in Lamar Jackson's hands as he
        runs for a score. "),
      plotOutput("kc_plot"),
      p("This plot depicts a pass from Patrick Mahomes to Mecole Hardman of the Superbowl winning
      Kansas City Chiefs. It shows what kind of play was run and where all the respective
        players were when a touchdown occured."), 
      plotOutput("no_plot"),
      p("This play shows a 15 yard touchdown run by the Saints' running back,
        Alvin Kamara. It is easy to see why he is regarded as one of the best running backs
        in the NFL. As you can see, he is swarmed by red jerseys all around him and
         is still able to score, which is what makes him special. ")
    )
  ),
  tabPanel("Play Analysis", 
           p("This graph shows an analysis of every single NFL
             team and their highlight touchdowns. This will
             show each and every team's tendencies. Using this
             graph, we can conclude which plays were most effective 
             for each team. This will show the probability for 
             each type of play for each team that resulted
             in a highlight touchdown."),
  plotOutput("play_analysis"),
  tableOutput("play_model"), 
  p("This regression model shows the effect of each of the different
    play types on the outcome of scoring a touchdown. It uses the variables 
    observed in the graph the be able to predict values for each play. 
    It also provides a 95% confidence interval that the values of
    each play will fall in between the numbers expressed. The main idea is
    that it takes the real life observations, and predicts based
    on the variables, also known as the play directions.")), 
  
  tabPanel("About",
           h3("Hello!"),
           p("My name is Aidan Borguet.
                I am a football player studying government at Harvard, so I had a
                natural inclination to study the next gen statistics
                related to football. This project contains information
                collected by next gen, which tracks a multitude of data in
                different sports. For football, they keep track of
                different plays, time, yardage, speed and so on. By
                using these statistics, we can determine which types of
                plays worked for certain teams during the NFL season."),
           tags$img(src = "https://thespun.com/wp-content/uploads/2019/04/GettyImages-856335926-775x465.jpg", height = 365, width = 675) )
)
)

server <- function(input, output, session) {
  
  output$bal_plot <- renderPlot({
    plot_play_frame(play_data_ = bal_play_data, frame_ = 242, velocities = TRUE)
  })
  
  output$kc_plot <- renderPlot({
    # kansas city plot, had to pickt he right frame to show correct timing
    plot_play_frame(play_data_ = kc_play_data, frame_ = 200, velocities = TRUE)
  })
  
  output$no_plot <- renderPlot({
  
    plot_play_frame(play_data_ = no_play_data, frame_ = 155, velocities = TRUE)
  })
  #model that I had used to create the regression 
  output$play_analysis <- renderPlot(play_plot)
    output$play_model <- renderTable({
      stan_glm(touchdown ~ left_play + right_play + middle_play,
               data = master_h, 
               refresh = 0) %>%
        tbl_regression(intercept = TRUE) %>%
        as_gt() %>%
        tab_header(title = "Regression of NFL Highlight Plays",
                   subtitle = "Relationship between the touchdowns and direction")
    })
  
  
  
}

shinyApp(ui, server)
