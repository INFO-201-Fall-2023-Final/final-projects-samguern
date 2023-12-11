#https://er-luo.shinyapps.io/final-projects-samguern/
library(shiny)
library(plotly)
library(dplyr)
library(stringr)
library(ggplot2)
filtered_Spotify <- read.csv("filtered_Spotify.csv")
filtered_spofityData <- read.csv("filtered_spofityData.csv")
age_group_spotify_summary <- read.csv("age_group_spotify_summary.csv")

ui <- fluidPage(
  tags$style(HTML("
                h2 {
                        background-color: #ccd5d8;
                        color: Black;
                        }")),
  titlePanel("Music and Mental Health"),
  
  tabsetPanel(type = "tabs",
              tabPanel("Intro", uiOutput("introTable"), tableOutput("fulldata"), uiOutput("intro")),
              
              tabPanel("Age Group and Hours per Day", 
                       sidebarLayout(sidebarPanel( h4("Filter Hours spent listening to music by Age Group"),
                                                   selectInput("age_grp", "Choose an Age Group", choices = filtered_Spotify$Age.group)), 
                                     mainPanel(plotlyOutput("agegroupMental")) ),
                       uiOutput("AgeHrsStory")
              ),
              tabPanel("Time and Device",
                       sidebarLayout(sidebarPanel( h4("Filter by time"),
                                                   selectInput("time", "Choose a time", choices = filtered_spofityData$music_time_slot)), 
                                     mainPanel(tableOutput("timeDevice")) ),
                       uiOutput("TimeDeviceStory")
              ),
              tabPanel("Disorder and Hours", 
                       sidebarLayout(sidebarPanel( h4("Filter by disorder"),
                                                   selectInput("disorder", "Choose a disorder", choices = c("Anxiety", "Depression", "Insomnia", "OCD"))), 
                                     mainPanel(plotlyOutput("disorderHours")) ),
                       uiOutput("DisorderHrsStory")
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
      
      a("What is Spotify?", 
        href = "https://support.spotify.com/us/article/what-is-spotify/", 
        target = "_blank"),
      
      h2("Why this is important:"),
      paste("Our data provides crucial information about mental health patterns and music listening habits, as categorized by age."),
      ("It can be used in future implementation of medical treatments,"),
      ("or developing apps aimed towards specific age categories."),
      br(),
      ("For example, according to our data, the age group 35-60 listens"),
      ("to music the least amount of hours in a day. Therefore, this type"),
      ("of user research shows how it would be least beneficial to market"),
      ("a music app to this category. It is also potentially beneficial"),
      ("to know this information in aiming to target a new age category."),
      br(),
      ("Furthermore, our data checks for a correlation between time spent listening to music and mental health disorder severity."),
      ("This information, as well as our entire dataset, can be helpful in treating and understanding disorders, as it allows"),
      ("viewers to see how age and listening habits are compared to one another."),
      ("For example, knowing that 19 year olds listen to the most music is helpful in understanding how music"),
      ("might influence a 15 versus 19 year old looking to be treated for Anxiety, or impact their life in some way."),
    br(),
    h2("Sources for datasets"),
    a("Spotify User Behavior Dataset", 
      href = "https://www.kaggle.com/datasets/meeraajayakumar/spotify-user-behavior-dataset", 
      target = "_blank"), 
    br(),
    a("Music & Mental Health Survey Results", 
      href = "https://www.kaggle.com/datasets/catherinerasgaitis/mxmh-survey-results/", 
      target = "_blank")
  )
     })
  
  output$agegroupMental <- renderPlotly({
    p <- ggplot( data = filter(filtered_Spotify, Age.group == input$age_grp), mapping = aes(x = Age , y = Hours.per.day)) +
      geom_bar(stat = "identity") +
      coord_flip()
    p <- ggplotly(p, tooltip = "text")
    return(p)
  })
  
  selected_age_group <- reactive({
    input$age_grp
  })
  output$AgeHrsStory <- renderUI({
    br()
    if(selected_age_group() == ""){
      p(paste(
        "This graph shows the amount of comparative time each specific age listens to music.",
        "There are 4 age categories, as found by the original dataset: 12-20, 20-35, 35-60, and 60+.",
        "Different ages within each age group listen to music at different rates."
    ))} else if (selected_age_group() == "12-20"){
      p(paste(
        "For this graph, 19 year olds comparatively listened to the most music."
      ))
    } else if (selected_age_group() == "20-35"){
      p(paste(
        "For this graph, 21 year olds comparatively listened to the most music."
      ))
    } else if (selected_age_group() == "35-60"){
    p(paste(
      "For this graph, 35 year olds comparatively listened to the most music."
    ))
    } else if (selected_age_group() == "60+"){
      p(paste(
        "For this graph, 90 year olds comparatively listened to the most music."
      ))
    }
    
      
})
  
  
  output$disorderHours <- renderPlotly({
    p <- ggplot( data = filtered_Spotify, mapping = aes(x = get(input$disorder), y = Hours.per.day, fill = Age.group)) +
      geom_point() +
      coord_flip() +
      labs(x = "Severity") 
    p <- ggplotly(p, tooltip = "text")
    return(p)
  })
  
  selected_disorder <- reactive({
    input$disorder
  })
  output$DisorderHrsStory <- renderUI({
    br()
    if(selected_disorder() == ""){
      p(paste(
        "This graph plots the Severity of a disorder versus the amount of time one listens to music.
        There seems to be no clear correlation between hours listening to music and severity of disorder."
      ))
    } else if(selected_disorder() == "Anxiety"){
        p("Those who listened to music the most had Anxiety levels of 0, 5, and 7.4.")
    } else if(selected_disorder() == "Depression"){
        p("Those who listened to music the most had Depression levels of 0, 1.25, and 5.0.")
    } else if(selected_disorder() == "Insomnia"){
        p("Those who listened to music the most had Insomnia levels of 0 and 9.6")
    } else if(selected_disorder() == "OCD"){
        p("Those who listened to music the most had OCD levels of 0, 1.25, and 3")
    }
  })
  
  output$timeDevice <- renderTable({
    tempPlot <- filter(filtered_spofityData, music_time_slot == input$time) %>%
      group_by(spotify_listening_device) %>%
      summarise(count = n())
    return(tempPlot)
  })
  
  selected_time_device <- reactive({
    input$time
  })
  output$TimeDeviceStory <- renderUI({
    br()
    if(selected_time_device() == ""){
      p(paste(
        "This graph shows the amount of responders who listened on a particular device, as categorized by Night, Afternoon, and Morning."
      ))} else if(selected_time_device() == "Night"){
        p(paste(
          "Most Night listeners listened by Smartphone."
        ))
      } else if(selected_time_device() == "Afternoon"){
        p("Most Afternoon listeners listened by Smartphone, almost tying with Computer.
          They are most likely to use Smart Speakers or Voice Assistants.")
      } else if(selected_time_device() == "Morning"){
        p(paste(
          "Most Morning listeners listened by Smartphone."
        ))
      }
  })
  
  output$conclusions <- renderUI({
    tagList(
      h2("Conclusions"),
      p("In conclusion, we now have an in-depth categorization of music listening habits. 
        We are able to determine which age groups listen to how much music, 
        what devices listeners use at a certain point in the day, as well as
        view the severity of mental health disorder as compared to the amount
        of time one listens to music."),
      br(),
      p("19 year olds, 21 year olds, 35 year olds, and 90 year olds listen to the most music."),
      br(),
      p("Most of all listeners do so through Smartphone. Users in the Afternoon are most prone
        to using Smart Speakers or Voice Assistants."),
      br(),
      p("The severity of one's mental health disorder cannot be determined by the hours per day music is listened to."),
      
      h2("Sources for datasets"),
      a("Spotify User Behavior Dataset", 
        href = "https://www.kaggle.com/datasets/meeraajayakumar/spotify-user-behavior-dataset", 
        target = "_blank"), 
      br(),
    a("Music & Mental Health Survey Results", 
      href = "https://www.kaggle.com/datasets/catherinerasgaitis/mxmh-survey-results/", 
      target = "_blank")
    )
    
  })
  
}

shinyApp(ui = ui, server = server)