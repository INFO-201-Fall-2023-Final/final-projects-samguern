library(shiny)

ui <- fluidPage(
  
  titlePanel("My app"),
  
 # navbarPage("",
    
    #sidebarPanel(),
    
    #mainPanel(
      
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
      #tags$img(src = "open-graph-default.png", width = 600)
      #includeHTML((src = "~Desktop/open-graph-default.png"), width = 600)
    )
  })
  
  
}

shinyApp(ui = ui, server = server)
