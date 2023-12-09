library(shiny)

ui <- fluidPage(
  
  # App title ----
  titlePanel("My app"),
  
  # Sidebar layout with input and output definitions ----
 # navbarPage("",
    
    # Sidebar panel for inputs ----
    #sidebarPanel(),
    
    # Main panel for displaying outputs ----
    #mainPanel(
      
      # Output: Tabset w/ plot, summary, and table ----
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
      ("Number that listen to music while working")
      #includeHTML((src = "~Desktop/open-graph-default.png"), width = 600)
      #tags$img(src = "open-graph-default.png", width = 600),
    )
  })
  
  
}

shinyApp(ui = ui, server = server)
