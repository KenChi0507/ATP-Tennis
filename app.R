library(shiny)
library(plotly)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(DT)
library(here)
library(magrittr)
library(shinydashboard)
library(RColorBrewer)

library(sparklyr)
sc <- spark_connect(master = "local")

ATP_Data <- spark_read_csv(sc, name = "my_data", path = "C:/Users/FI_0616/Desktop/Dataset/atp_matches.csv")
Player_Data  <- spark_read_csv(sc, name = "my_data", path = "C:/Users/FI_0616/Desktop/Dataset/atp_players.csv")

ATP_Inf <- list(
  "tourney_id" = "Tournament ID", "tourney_name" = "Tournament Name", "surface" = "Surface", "draw_size" = "Draw_Size", "tourney_level" = "Tournament Level", "tourney_date" = "Tourney Date", "match num" = "Match Number",
  "winner_id" = "Winner ID",	"winner_seed" = "Winner Seed",	"winner_entry" = "Winner Entry",	"winner_name" = "Winner Name",	"winner_hand" = "Winner Hand",	"winner_ht" = "Winner Height",	"winner_ioc" = "Winner Country",	"winner_age" = "Winner Age",	
  "loser_id" = "Loser ID",	"loser_seed" = "Loser Seed",	"loser_entry" = "Loser Entry", "loser_name" = "Loser Name",	"loser_hand" = "Loser Hand",	"loser_ht" = "Loser Height",	"loser_ioc" = "Loser Country",	"loser_age" = "Loser Age",
  "score" = "Score",  "best_of" = "Best of", "round" = "Round", "minutes" = "Minutes",
  
  "w_ace" = "Winner Ace",	"w_df" = "Winner Double Faults",	"w_svpt" = "Winner Serve Percent",	"w_1stIn" = "Winner First Serve In Percent",	"w_1stWon" = "Winner First Serve Winning Percent",	
  "w_2ndWon" = "Winner Second Serve Winning Percent",	"w_SvGms" = "Winner Number of Games Played on A Serve",	"w_bpSaved" = "Winner Breakpoints Saved",	"w_bpFaced" = "Winner Breakpoints Faced",	
  
  "l_ace" = "Loser Ace",	"l_df" = "Loser Double Faults",	"l_svpt" = "Loser Serve Percent",	"l_1stIn" = "Loser First Serve In Percent",	"l_1stWon" = "Loser First Serve Winning Percent",	
  "l_2ndWon" = "Loser Second Serve Winning Percent",	"l_SvGms" = "Loser Number of Games Played on A Serve",	"l_bpSaved" = "Loser Breakpoints Saved",	"l_bpFaced" = "Loser Breakpoints Faced",
  
  "winner_rank" = "Winner Rank",	"winner_rank_points" = "Winner Rank Points",	
  "loser_rank" = "Loser Rank",	"loser_rank_points" = "Loser Rank Points"
  )

Bar_list <- ATP_Data[,c(3,4,5,12,20,25,26)]
Advance_Bar_list <- list("1", "2", "3")
Pie_list <- ATP_Data[,c(3,4,5,12,20,25,26)]
Box_list <- ATP_Data[,c(13,15,21,23,26:49)]

Head <- dashboardHeader(title = span("ATP Tennis Match ", style = " font-size: 25px;font-weight: bold;"), titleWidth = 300)

Sidebar <- dashboardSidebar(
  width = 300,
  menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
  menuItem("Dataset", tabName = "data", icon = icon("database")),
  menuItem(text = "Visualization", tabName = "Viz",icon = icon("chart-line")),
  menuItem(text = "Information", tabName = "inf",icon = icon("circle-info")),
  
  conditionalPanel(condition="input.Data == 1",
                   checkboxGroupInput("show_vars", "Columns in ATP Match to show:",
                                      names(ATP_Data), selected = names(ATP_Data))),
  
  selectInput(inputId = 'BarData_1', label = "Select Bar 1 Data:",
              choices = names(Bar_list) ,selected = names(Bar_list)[[1]]),
  
  selectInput(inputId = 'BarData_2', label = "Select Bar 2 Data:",
              choices = names(Bar_list) ,selected = names(Bar_list)[[2]]),
  
  selectInput(inputId = 'PieData_1', label = "Select Pie 1 Data:",
              choices = names(Pie_list) ,selected = names(Pie_list)[[1]]),
  
  selectInput(inputId = 'PieData_2', label = "Select Pie 2 Data:",
              choices = names(Pie_list) ,selected = names(Pie_list)[[2]]),
  
  selectInput(inputId = 'BoxData_1', label = "Select Box 1 Data:",
              choices = names(Box_list) ,selected = names(Box_list)[[1]]),
  
  selectInput(inputId = 'BoxData_2', label = "Select Box 2 Data:",
              choices = names(Box_list) ,selected = names(Box_list)[[2]])

)

Body <- dashboardBody(
  tags$head( 
    tags$style(HTML(".main-sidebar { font-size: 20px; }")) 
  ),
  tabItems(
    tabItem(tabName = "dashboard",
            fluidRow(
              infoBox("Right Hand", paste0("27,525 Players"), paste0("88.83% Players"), icon=icon("fa-solid fa-tennis-ball")),
              infoBox("Total Matches", paste0("62,066 matches"), paste0("20 years"),icon=icon("fa-solid fa-tennis-ball")),
              infoBox("Player Age", paste0("14 years old - 46 years old"), paste0("15 years old - 44 years old"),icon=icon("fa-solid fa-tennis-ball"))
            ),
            fluidRow(
              valueBox(paste0("31,051 Players"),"ATP Tennis Match",icon=icon("user")),
              valueBox("USA","Most Tennis Players",icon=icon("flag")),
              valueBox("Hard Surface","Most Court Surface",icon=icon("fire"))
            ),
            fluidRow(
              box(title="Bar plot of Player in every country", status="primary",solidHeader=T,background="aqua",plotOutput("histogram")),
              box(title="Controls for Dashboard", status = "warning",solidHeader = T,background = "red",
                  sliderInput("bins","Top * Country",1,100,10)
                  )
              )
            ),
    
    tabItem(tabName = "data",
            tabBox(id = "Data", width=15,
                   tabPanel("Start", verbatimTextOutput("start"),value = 0),
                   tabPanel("Match Data", icon = icon("database"), dataTableOutput("dataM"),value = 1),
                   tabPanel("Structure", icon = icon("bars"), verbatimTextOutput("structure"),value = 0),
                   tabPanel("Summary", icon = icon("chart-pie"), verbatimTextOutput("summary"),value = 0)
            )
    ),
    
    tabItem(tabName = "Viz",
            tabBox(id = "Plot", width=15,
                   tabPanel(title = "Bar Plot", plotOutput("barplot")),
                   tabPanel(title = "Pie Plot",plotOutput("piechart")),
                   tabPanel(title = "Box Plot",plotOutput("boxchart"))
            )
    ),
    
    tabItem(tabName = "inf",
            h1("Data"),
            h3("The dataset is provided and downloaded from Kaggle through the link",
               a("ATP Tennis Match", href = "https://www.kaggle.com/datasets/sijovm/atpdata?select=atp_matches_till_2022.csv"),
               ", which includes every game from 1968 to 2022(188,162 rows * 49 columns). We only focus on those games from 2002 to 2022 (62067 rows * 49 columns)"),
            br(),
            h1("Business Questions"),
            h3("1. How can we leverage data analysis to identify the player who achieves the highest win rate, considering variables like seed, ranking, and performance across multiple rounds??"),
            h3("2. How can we develop a predictive model utilizing historical data from the dataset to accurately forecast match outcomes?"),
            h3("3. How can we conduct an analysis to understand the impact of surface type (e.g., grass, clay,hardcourt) on players' performance?"),
            h3("4. How can we evaluate the influence of player attributes such as height, age, handedness (right or left-handed), and rank on match outcomes in professional tennis?"),
            h3("5. How can we analyze the dataset to uncover trends and patterns in player performance over the years, focusing on factors like average match duration, number of aces, and service game success rate?"),
            h3("6. How can we leverage the dataset to determine the ideal periods during the year when playerperformance tends to reach its peak or decline?"),
            h3("7. How can we examine the performance of players from various countries to identify if certain nationalities exhibit a higher success rate or dominance in specific tournaments or surfaces?"),
    )
    
  )
)

ui <- dashboardPage(title="ALY6110 Final Project - ATP Tennis Match",skin="purple",
    Head,
    Sidebar,
    Body
)

server <- function(input,output){
  
output$histogram <- renderPlot({
    options(scipen = 999)
    
    filtered_df <- Player_Data %>%
      select(ioc, dob) %>%
      filter(dob > 19800000)
    
    country_counts <- filtered_df %>%
      group_by(ioc) %>%
      count() %>%
      ungroup() %>%
      arrange(desc(n)) %>%
      head(input$bins)
    
    ggplot(country_counts, aes(x = reorder(ioc, n), y = n, fill = ioc)) +
      geom_bar(stat = "identity") +
      labs(x = "Country (IOC)", y = "Count") +
      scale_fill_discrete() +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
})
  
  output$start <- renderPrint(
    cat("Data information of Campus Recruitment\n
[1]  tourney_id - tournament_id
[2]  tourney_name - tournament_name                                           
[3]  surface - surface in which the match is played
[4]  draw_size - the size of the draw
[5]  tourney_level - tournament level
      'G' = Grand Slams
      'M' = Masters 1000s (ATP 1000)
      'A' = other tour-level events (ATP 250, ATP 500)
      'C' = Challengers
      'S' = Satellites/ITFs (International Tennis Federation)
      'F' = Tour finals and other season-ending events
      'D' = Davis Cup      
[6]  tourney_date - starting date of the tournament                    
[7]  match num - match number in a certain tournament            
[8]  best_of - the maximum number of sets played                                       
[9]  round - the round in the tournament a match belongs to
[10] minutes - duration of the match in minutes 

Winner & Loser (double columns)
[11] id - player id  
[12] seed - the seed of the player in that tournament                    
[13] entry - How did the player enter the tournaments?
        WC - Wildcard
        Q - Qualifier
        LL - Lucky loser
        PR - Protected ranking
        SE - Special Exempt
        ALT - Alternate player           
[14] name - player name           
[15] hand - hand of the player, right or left
[16] ht - the height of the player
[17] IOC - the country of origin
[18] age - age of the player
[19] score - final score in the match
[20] Ace - number of aces in the match
[21] df - double faults
[22] svpt - serve percent
[23] 1stin - first serve in percent
[24] 1stWon - first serve winning percent
[25] 2ndWon - second serve winning percent
[26] SvGms - number of games played on a serve (The maximum difference between w_SvGms and l_SvGms will be 1)
[27] bpSaved - breakpoints saved
[28] bpFaced - breakpoints faced")
  )
  
  output$dataM <- renderDataTable({
    DT::datatable(
      ATP_Data[, input$show_vars, drop = FALSE],
      options = list(
        searching = TRUE,  
        lengthChange = TRUE,  
        pageLength = 10  
      )
    )
  })
  
  output$structure <- renderPrint(
    str(ATP_Data)
  )
  
  output$summary <- renderPrint(
    summary(ATP_Data)
  )
  
  output$barplot <- renderPlot({
    
    ATP <- ATP_Data %>%
      filter(tourney_date > 20020000)
    par(mfcol=c(1,2),mai=c(0,0,0,0), mar=c(7,4,5,4))
    plotData_1 <- data.frame(ATP[,input$BarData_1]) 
    plotData_1 <- table(unlist(plotData_1))
    title <- paste("Barplot of",ATP_Inf[input$BarData_1])
    Barplot <- barplot(plotData_1, ylab = as.character(ATP_Inf[input$BarData_1]), main = title, horiz = T, col = brewer.pal(12,"Set3"),
                       xlim = c(0,70000))
    text(plotData_1,Barplot,plotData_1,cex = 0.8, pos = 4)
    
    plotData_2 <- data.frame(ATP[,input$BarData_2]) 
    plotData_2 <- table(unlist(plotData_2))
    title <- paste("Barplot of",ATP_Inf[input$BarData_2])
    Barplot <- barplot(plotData_2, ylab = as.character(ATP_Inf[input$BarData_2]), main = title, horiz = T, col = brewer.pal(12,"Set3"),
                       xlim = c(0,70000))
    text(plotData_2,Barplot,plotData_2,cex = 0.8, pos = 4)
  })
  
  output$piechart <- renderPlot({
    
    ATP <- ATP_Data %>%
      filter(tourney_date > 20020000)
    par(mfcol=c(1,2),mai=c(0,0,0,0), mar=c(7,4,5,4))
    plotData_1 <- data.frame(ATP[,input$PieData_1]) 
    plotData_1 <- table(unlist(plotData_1))
    PER <- (plotData_1/sum(plotData_1))*100
    
    legend = paste(unique(sort(unlist(data.frame(ATP_Data[,input$PieData_1])))))
    title <- paste("Pie Plot of",ATP_Inf[input$PieData_1])
    
    pie(plotData_1, col = brewer.pal(12,"Set3"), label = paste(legend, "-", round(PER,2),"%"), main = title)
    legend("bottomleft",legend = legend, "", fill = brewer.pal(12,"Set3"))
    
    plotData_2 <- data.frame(ATP[,input$PieData_2]) 
    plotData_2 <- table(unlist(plotData_2))
    PER <- (plotData_2/sum(plotData_2))*100
    
    legend = paste(unique(sort(unlist(data.frame(ATP_Data[,input$PieData_2])))))
    title <- paste("Pie Plot of",ATP_Inf[input$PieData_2])
    
    pie(plotData_2, col = brewer.pal(12,"Set3"), label = paste(legend, "-", round(PER,2),"%"), main = title)
    legend("bottomleft",legend = legend, "", fill = brewer.pal(12,"Set3"))
    
  })
  
  output$boxchart <- renderPlot({
    
    ATP <- ATP_Data %>%
      filter(tourney_date > 20020000)
    par(mfcol=c(1,2),mai=c(0,0,0,0), mar=c(7,4,5,4))
    
    plotData_1 <- data.frame(ATP[,input$BoxData_1]) 
    # plotData_1 <- table(unlist(plotData_1))
    
    plotData_2 <- data.frame(ATP[,input$BoxData_2]) 
    # plotData_2 <- table(unlist(plotData_2))
    
    title_1 <- paste("Box Plot of",ATP_Inf[input$BoxData_1])
    xlab_1 <- paste(ATP_Inf[input$BoxData_1])
    
    title_2 <- paste("Box Plot of",ATP_Inf[input$BoxData_2])
    xlab_2 <- paste(ATP_Inf[input$BoxData_2])
    
    boxplot(plotData_1  ,las = 2, xlab = xlab_1, cex.names = 0.5, horizontal = F, col = brewer.pal(12,"Set3"), border = "red", main = title_1)
    boxplot(plotData_2  ,las = 2, xlab = xlab_2, cex.names = 0.5, horizontal = F, col = terrain.colors(5), border = "red", main = title_2)
    
  })
}

shinyApp(ui = ui, server = server)
