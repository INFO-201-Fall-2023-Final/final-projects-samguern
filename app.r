#library(shiny)
library(plotly)
library(dplyr)
library(stringr)
library(ggplot2)
filtered_Spotify <- read.csv("filtered_Spotify.csv")
filtered_spofityData <- read.csv("filtered_spofityData")
age_group_spotify_summary <- read.csv("age_group_spotify_summary")

ui <- fluidPage(
  tags$style(HTML("
                h2 {
                        background-color: #ccd5d8;
                        color: Black;
                        }")),
  titlePanel("My app"),
      
    tabsetPanel(type = "tabs",
                tabPanel("Intro", uiOutput("introTable"), tableOutput("fulldata"), uiOutput("intro")),
                  # for boxed text tabPanel("Intro", verbatimTextOutput("intro")),
                
                tabPanel("Age group and Hours per Day", 
                         sidebarLayout(sidebarPanel( h4("Filter Hours spent listening to music by Age group"),
                                          selectInput("age_grp", "Choose a Age Group", choices = filtered_Spotify$Age.group)), 
                                        mainPanel(plotlyOutput("agegroupMental")) ),
                         uiOutput("AgeHrsStory")
                         ),
                tabPanel("Disorder and Hours", 
                         sidebarLayout(sidebarPanel( h4("Filter by disorder"),
                                          selectInput("disorder", "Choose a disorder", choices = c("Anxiety", "Depression", "Insomnia", "OCD"))), 
                                        mainPanel(plotlyOutput("disorderHours")) ),
                         uiOutput("DisorderHrsStory")
                         ),
                tabPanel("Time and Device",
                         sidebarLayout(sidebarPanel( h4("Filter by time"),
                                                     selectInput("time", "Choose a time", choices = filtered_spofityData$music_time_slot)), 
                                       mainPanel(tableOutput("timeDevice")) ),
                         uiOutput("TimeDeviceStory")
                         ),
                tabPanel("Summary and About",
                         uiOutput("conclusions"))
                )
                
)
      



server <- function(input, output){
  output$introTable <- renderUI({
    h2("Summarized Dataset")
  })
  
  output$fulldata <- renderTable(age_group_spotify_summary)
  
  output$intro <- renderUI({
    tagList(
      p("This dataset is a summary of various statistics (music listening habits & personal mental health),",
        "categorized by age group and filtered to Spotify users."),
      ("Combined from two larger datasets (two different surveyed groups of people), this dataset was filtered down 
       to easily determine various averaged characteristics of Spotify listeners, all grouped by age categories."),
      ("Our summarized dataset finds:"),
      br(),
      ("-Average Anxiety, Depression, and OCD"),
      br(),
      ("-Average hours listened to music per day & what time of day"),
      br(),
      ("-Listening device"),
      br(),
      ("-Number of responses"),
      br(),
      p("-Number that listen to music while working"),
      br(),
      
      a("What is Spotify?", 
        href = "https://support.spotify.com/us/article/what-is-spotify/", 
        target = "_blank"), # need to open in browser to click on link
      
      h2("Why this is important:"),
      p("Our data provides crucial information about mental health patterns and music listening habits, as categorized by age."),
      ("It can be used in future implementation of medical treatments,"),
      ("or developing apps aimed towards specific age categories"),
      ("For example, according to our data, the age group 35-60 listens"),
      ("to music the least amount of hours in a day. Therefore, this type"),
      ("of user research shows how it would be least beneficial to market"),
      ("a music app to this category. Or, it could potentially be beneficial"),
      ("to know this information in aiming to target a new age category."),
      #includeHTML((src = "~Desktop/open-graph-default.png"), width = 600)
      #tags$img(src = "~Desktop/open-graph-default.png", width = 600),
    )
  })
  
  output$agegroupMental <- renderPlotly({
    p <- ggplot( data = filter(filtered_Spotify, Age.group == input$age_grp), mapping = aes(x = Age , y = Hours.per.day)) +
      geom_bar(stat = "identity") +
      coord_flip()
    p <- ggplotly(p, tooltip = "text")
    return(p)
  })
  
  output$AgeHrsStory <- renderUI({
    br()
    #Write something in here ig
    p("This graph...")
  })
  
  output$disorderHours <- renderPlotly({
    p <- ggplot( data = filtered_Spotify, mapping = aes(x = get(input$disorder), y = Hours.per.day, fill = Age.group)) +
      geom_point() +
      coord_flip() +
      labs(x = "Severity") 
    p <- ggplotly(p, tooltip = "text")
    return(p)
  })
  
  output$DisorderHrsStory <- renderUI({
    br()
    #Write something in here ig
    p("This graph...")
  })
  
  output$timeDevice <- renderTable({
    tempPlot <- filter(filtered_spofityData, music_time_slot == input$time) %>%
                group_by(spotify_listening_device) %>%
                summarise(count = n())
    return(tempPlot)
  })
  
  output$TimeDeviceStory <- renderUI({
    br()
    #Write something in here ig
    p("This graph...")
  })
  
  output$conclusions <- renderUI({
    tagList(
      h2("Conclusions"),
      #Write something in here ig
      p("Write conclusion"),
    
      h2("Sources for datasets")
      #link the sources here 
    )
    
  })
  
}

shinyApp(ui = ui, server = server)