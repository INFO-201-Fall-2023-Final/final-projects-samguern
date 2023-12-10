library(shiny)

ui <- fluidPage(
  
  titlePanel("My app"),
      
    tabsetPanel(type = "tabs",
                tabPanel("Intro", uiOutput("intro")),
                  # for boxed text tabPanel("Intro", verbatimTextOutput("intro")),
                tabPanel("Story 1", plotOutput("story1")),
                tabPanel("Story 2", textOutput("story2")),
                tabPanel("Story 3", tableOutput("story3"))
      )
      
  )



server <- function(input, output){
  output$intro <- renderUI({
    tagList(
      h2("Add image"),
      p("This dataset is a summary of various statistics (music listening habits & personal mental health),",
        "categorized by age group and filtered to Spotify users."),
      ("Combined from two larger datasets (two different surveyed groups of people), this dataset was filtered down 
       to easily determine various averaged characteristics of Spotify listeners, all grouped by age categories."),
      ("Our summarized dataset finds:"),
      br(),
      ("Average Anxiety, Depression, and OCD"),
      br(),
      ("Average hours listened to music per day & what time of day"),
      br(),
      ("Listening device"),
      br(),
      ("Number of responses"),
      br(),
      p("Number that listen to music while working"),
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
      ("to know this information in aiming to target a new age category.")
      #includeHTML((src = "~Desktop/open-graph-default.png"), width = 600)
      #tags$img(src = "~Desktop/open-graph-default.png", width = 600),
    )
  })
  
  
}

shinyApp(ui = ui, server = server)
