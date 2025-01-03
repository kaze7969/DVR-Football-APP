library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(tidyverse)
library(randomForest)

# Load the datasets
attempts_goals_merged <- read.csv("attempts_goals_merged.csv")
merged_data <- read.csv("merged_data.csv")

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Football Analysis App"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "intro"),
      menuItem("Goals Analysis", tabName = "goals"),
      menuItem("Assists Analysis", tabName = "assists"),
      menuItem("Player Details", tabName = "player_details"),
      downloadButton("download_data", "Download Predicted Data")
    )
  ),
  dashboardBody(
    tabItems(
      # Introduction Tab
      # Introduction Tab Content
      tabItem(tabName = "intro",
              h2("Welcome to the Football Analysis App"),
              p("This app is your ultimate destination for football statistics analysis, allowing you to explore detailed player metrics, predict performance using machine learning models, and visualize the results through interactive charts and tables."),
              h3("Features"),
              tags$ul(
                tags$li("Analyze player performance across various metrics like goals, assists, and accuracy."),
                tags$li("Predict future performance using advanced machine learning models."),
                tags$li("Compare actual vs. predicted results for key stats."),
                tags$li("Easily browse player details and download processed data.")
              ),
              h3("How to Use"),
              tags$ol(
                tags$li("Navigate through the sidebar to explore different sections such as Goals Analysis, Assists Analysis, and Player Details."),
                tags$li("Select players from the data tables to view detailed predictions and visualizations."),
                tags$li("Download processed data for further analysis.")
              ),
              h3("Creators"),
              p("This app was developed by:"),
              tags$ul(
                tags$li("Aryan Tinker"),
                tags$li("Devesh Jangid")
              ),
              h4("We hope you enjoy using the Football Analysis App!")
      ),
      
      
      # Goals Tab
      tabItem(tabName = "goals",
              h2("Goals Analysis"),
              DTOutput("player_table_goals"),
              DTOutput("predicted_table_goals"),
              plotOutput("goal_prediction_plot")),
      
      # Assists Tab
      tabItem(tabName = "assists",
              h2("Assists Analysis"),
              DTOutput("player_table_assists"),
              DTOutput("predicted_table_assists"),
              plotOutput("assist_prediction_plot")),
      
      # Player Details Tab
      tabItem(tabName = "player_details",
              h2("Player Details"),
              tabsetPanel(
                tabPanel("Goals Dataset", DTOutput("goals_details_table")),
                tabPanel("Assists Dataset", DTOutput("assists_details_table"))
              ))
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Goal Prediction Model (using Random Forest)
  goal_model <- randomForest(goals ~ total_attempts + on_target + off_target + blocked + match_played + right_foot + left_foot + headers + others + inside_area + outside_areas + penalties, data = attempts_goals_merged, ntree = 500, mtry = 4)
  
  # Assists Prediction Model (using Random Forest)
  assist_model <- randomForest(assists ~ corner_taken + offsides + dribbles + position + pass_accuracy + pass_attempted + pass_completed + cross_accuracy + cross_attempted + cross_completed + freekicks_taken + match_played, data = merged_data, ntree = 500, mtry = 4)
  
  # Goals Analysis Tab - Render Player Table
  output$player_table_goals <- renderDT({
    datatable(attempts_goals_merged[, c("player_name", "club")], selection = "multiple")
  })
  
  # Render Goal Prediction Plot (Double Bar Chart: Actual vs Predicted Goals)
  output$goal_prediction_plot <- renderPlot({
    selected_players <- input$player_table_goals_rows_selected
    
    # If no player is selected, return nothing
    if (length(selected_players) == 0) {
      return(NULL)
    }
    
    # Predict goals for selected players using Random Forest
    predicted_goals <- predict(goal_model, attempts_goals_merged[selected_players, ])
    
    # Create a data frame for actual vs predicted goals
    goal_data <- attempts_goals_merged[selected_players, ]
    goal_data$predicted_goals <- predicted_goals
    
    # Reshape data to long format for ggplot (for easy side-by-side plotting)
    goal_data_long <- goal_data %>%
      gather(key = "Type", value = "Goals", goals, predicted_goals)
    
    # Create the double bar chart with adjacent bars for Actual vs Predicted Goals
    ggplot(goal_data_long, aes(x = player_name, y = Goals, fill = Type)) +
      geom_bar(stat = "identity", position = "dodge") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(x = "Player Name", y = "Goals", fill = "Legend") +
      scale_fill_manual(values = c("goals" = "blue", "predicted_goals" = "red")) +
      theme(legend.position = "top")
  })
  
  # Render Predicted Values Table (Goals)
  output$predicted_table_goals <- renderDT({
    selected_players <- input$player_table_goals_rows_selected
    
    # If no player is selected, return an empty table
    if (length(selected_players) == 0) {
      return(datatable(data.frame(Message = "No players selected.")))
    }
    
    # Predict goals for selected players using Random Forest
    predicted_goals <- predict(goal_model, attempts_goals_merged[selected_players, ])
    
    # Create a data frame for actual vs predicted goals
    goal_data <- attempts_goals_merged[selected_players, ]
    goal_data$predicted_goals <- predicted_goals
    
    # Return the predicted data
    datatable(goal_data[, c("player_name", "club", "goals", "predicted_goals")])
  })
  
  # Assists Analysis Tab - Render Player Table
  output$player_table_assists <- renderDT({
    datatable(merged_data[, c("player_name", "club")], selection = "multiple")
  })
  
  # Render Assist Prediction Plot (Double Bar Chart: Actual vs Predicted Assists)
  output$assist_prediction_plot <- renderPlot({
    selected_players <- input$player_table_assists_rows_selected
    
    # If no player is selected, return nothing
    if (length(selected_players) == 0) {
      return(NULL)
    }
    
    # Predict assists for selected players using Random Forest
    predicted_assists <- predict(assist_model, merged_data[selected_players, ])
    
    # Create a data frame for actual vs predicted assists
    assist_data <- merged_data[selected_players, ]
    assist_data$predicted_assists <- predicted_assists
    
    # Reshape data to long format for ggplot (for easy side-by-side plotting)
    assist_data_long <- assist_data %>%
      gather(key = "Type", value = "Assists", assists, predicted_assists)
    
    # Create the double bar chart with adjacent bars for Actual vs Predicted Assists
    ggplot(assist_data_long, aes(x = player_name, y = Assists, fill = Type)) +
      geom_bar(stat = "identity", position = "dodge") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(x = "Player Name", y = "Assists", fill = "Legend") +
      scale_fill_manual(values = c("assists" = "green", "predicted_assists" = "orange")) +
      theme(legend.position = "top")
  })
  
  # Render Predicted Values Table (Assists)
  output$predicted_table_assists <- renderDT({
    selected_players <- input$player_table_assists_rows_selected
    
    # If no player is selected, return an empty table
    if (length(selected_players) == 0) {
      return(datatable(data.frame(Message = "No players selected.")))
    }
    
    # Predict assists for selected players using Random Forest
    predicted_assists <- predict(assist_model, merged_data[selected_players, ])
    
    # Create a data frame for actual vs predicted assists
    assist_data <- merged_data[selected_players, ]
    assist_data$predicted_assists <- predicted_assists
    
    # Return the predicted data
    datatable(assist_data[, c("player_name", "club", "assists", "predicted_assists")])
  })
  
  # Player Details Tab - Goals Details Table
  output$goals_details_table <- renderDT({
    datatable(attempts_goals_merged, options = list(scrollX = TRUE))
  })
  
  # Player Details Tab - Assists Details Table
  output$assists_details_table <- renderDT({
    datatable(merged_data, options = list(scrollX = TRUE))
  })
  
  # Download Predicted Data (Goals and Assists)
  output$download_data <- downloadHandler(
    filename = function() {
      paste("predicted_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # Combine goals and assists prediction data for download
      selected_players_goals <- input$player_table_goals_rows_selected
      selected_players_assists <- input$player_table_assists_rows_selected
      if (length(selected_players_goals) == 0 && length(selected_players_assists) == 0) {
        return(NULL)
      }
      
      # Predict goals and assists for selected players
      predicted_goals <- predict(goal_model, attempts_goals_merged[selected_players_goals, ])
      goal_data <- attempts_goals_merged[selected_players_goals, ]
      goal_data$predicted_goals <- predicted_goals
      
      predicted_assists <- predict(assist_model, merged_data[selected_players_assists, ])
      assist_data <- merged_data[selected_players_assists, ]
      assist_data$predicted_assists <- predicted_assists
      
      # Combine the predicted data for goals and assists
      combined_data <- bind_rows(
        goal_data[, c("player_name", "club", "goals",    "predicted_goals")],
        assist_data[, c("player_name", "club", "assists", "predicted_assists")]
      )
      
      # Write to CSV
      write.csv(combined_data, file, row.names = FALSE)
    }
  )
}

# Run the app
shinyApp(ui , server)

