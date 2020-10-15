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

source_url("https://raw.githubusercontent.com/asonty/ngs_highlights/master/utils/scripts/data_utils.R")
source_url("https://raw.githubusercontent.com/asonty/ngs_highlights/master/utils/scripts/plot_utils.R")


bal_highlights <- fetch_highlights_list(team_ = "BAL", season_ = 2019)
bal_play_data <- fetch_play_data(playKey_ = 242)
first_frame <- bal_play_data %>%
  filter(event == "line_set") %>% 
  distinct(frame) %>% 
  slice_max(frame) %>% 
  pull()
final_frame <- bal_play_data %>% 
  filter(event == "tackle" | event == "touchdown" | event == "out_of_bounds") %>% 
  distinct(frame) %>% 
  slice_max(frame) %>% 
  pull()


kc_highlights <- fetch_highlights_list(team_ = "KC", season_ = 2019)
kc_play_data <- fetch_play_data(playKey_ = 370)
first_frame_kc <- kc_play_data %>%
  filter(event == "line_set") %>% 
  distinct(frame) %>% 
  slice_max(frame) %>% 
  pull()
final_frame_kc <- kc_play_data %>% 
  filter(event == "tackle" | event == "touchdown" | event == "out_of_bounds") %>% 
  distinct(frame) %>% 
  slice_max(frame) %>% 
  pull()





# This is just a normal object

state.names <- c("CA", "NY", "KS")

# Make change to your dataset
results_house <- results_house %>%
  select(-footnotes)

######################################################################################
######################################################################################
#
# 1. Shiny Apps have two basic parts to them
#
#   - The user interface (UI) defines how the app should look.
#
#     -- For example, the text on the page, the placement of the elements, etc.
#
#   - The server defines how the app should behave.
#
#     -- This is how live elements are updated - like selecting a state from a list.
#
#   - Those two pieces are combined by running shinyApp(ui, server) to create the app.
#
#      -- You can also click the green "Run App" button on the top right or
#         run runApp() in the console

ui <- fluidPage(navbarPage(
  "Shiny Example",
  
  tabPanel(
    "Main",
    
    # - UIs are built from "panel" functions, which specify areas of your page.
    #
    #   -- There is a "main panel," a "sidebar," a "title," etc.
    
    # Here is a sidebar!
    
    sidebarPanel(
      selectInput(
        inputId = "selected_state",                 # a name for the value you choose here
        label = "Choose a state from this list!",   # the name to display on the slider
        choices = state.names                       # your list of choices to choose from
      ),
      
      sliderInput(
        inputId = "selected_size",                  # a name for the value you choose here
        label = "Choose a number as a point size:", # the label to display above the slider
        min = 0,                                    # the min, max, and initial values
        max = 5,
        value = 2 
      )
      
    ),
    
    
    # And here is your "main panel" for the page.
    
    mainPanel(
      # - You can also make your UI more complicated with UI elements.
      #
      #   -- In general, these are defined by functions that you give arguments to 
      #      (e.g. min and max values).
      #
      # - These include:
      #
      #   -- selectInput() to choose from multiple options.
      #
      #   -- sliderInput() lets you choose a value from a slider of values you define.
      #
      #   -- radioButtons() let you choose a button from a number of options
      #
      #   -- textInput() lets you enter whatever text you want.
      #
      #   -- Lots of other options, like entering a date. Look at the resources for 
      #      other choices!
      #
      # - You then assign these inputs to a value and use those values in other places, 
      #   like in plots!
      #
      # - All of these functions have their own arguments. For example:
      
      radioButtons(
        inputId = "selected_color",             # a name for the value you choose here
        label = "Choose a color!",              # the label to display above the buttons
        choices = c("red", "blue", "green")     # the button values to choose from
      ),
      
      textInput(
        inputId = "entered_text",               # a name for the value you choose here
        label = "Place your title text here:",  # a label above the text box
        value = "Example Title"                 # an initial value for the box
      ),
      
      textOutput("state_message"),              # load a text object called "state_message"
      textOutput("size_message"),
      textOutput("color_message"),
      textOutput("text_message"),
      plotOutput("bal_plot"),
      plotOutput("kc_plot")
    )
  ),
  tabPanel("About",
             h3("Hello! My name is Aidan Borguet. 
                I am a football player studying government at Harvard, so I had a 
                natural inclination to study the next gen statistics 
                related to football. This project contains information 
                collected by next gen, which tracks a multitude of data in 
                different sports. For football, they keep track of
                different plays, time, yardage, speed and so on. By 
                using these statistics, we can determine which types of 
                plays worked for certain teams during the NFL season."))
  )
)

server <- function(input, output, session) {
  # - Then, you use these named objects to update the data on your site via the input object.
  #
  #   -- render() functions are what show content that will change live on your site.
  #
  #   -- so here, renderText() is updating live text based on your choice.
  
  output$state_message <- renderText({
    paste0("This is the state you chose: ", # this is just a string, so it will never change
           input$selected_state, "!")       # this is based on your input, selected_state defined above.
  })
  
  output$size_message <- renderText({
    paste0("This is the size you chose: ", # this is just a string, so it will never change
           input$selected_size, "!")       # this is based on your input, selected_state defined above.
  })
  
  output$color_message <- renderText({
    paste0("This is the color you chose: ", # this is just a string, so it will never change
           input$selected_color, "!")       # this is based on your input, selected_state defined above.
  })
  
  output$text_message <- renderText({
    paste0("This is the label you typed: ", # this is just a string, so it will never change
           input$entered_text, "!")       # this is based on your input, selected_state defined above.
  })
  
  # This line makes our dataset reactive.
  # That is, we can update it based on the values of input that define above.
  
  results <- reactive({
    results_house
  })
  
  # Just like renderText(), we can renderPlot()!
  
  output$bal_plot <- renderPlot({
    # we need to use () here after the name of our dataset because it is reactive!
    bal_play_data <- fetch_play_data(playKey_ = 242) 
    plot_play_frame(play_data_ = bal_play_data, frame_ = 180)
    plot_play_frame(play_data_ = bal_play_data, frame_ = 200, velocities_ = T)
  })
  
  output$kc_plot <- renderPlot({
    # we need to use () here after the name of our dataset because it is reactive!
    kc_play_data <- fetch_play_data(playKey_ = 370) 
    plot_play_frame(play_data_ = kc_play_data, frame_ = 180)
    plot_play_frame(play_data_ = kc_play_data, frame_ = 200, velocities_ = T)
  })
  
  
}

shinyApp(ui, server)