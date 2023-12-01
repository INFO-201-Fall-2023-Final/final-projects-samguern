library(shiny)

ui <- fluidPage(
  
  # App title ----
  titlePanel("My app"),
  
  # Sidebar layout with input and output definitions ----
  navbarPage("",
    
    # Sidebar panel for inputs ----
    #sidebarPanel(),
    
    # Main panel for displaying outputs ----
    #mainPanel(
      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Intro", textOutput("intro")),
                  # for boxed text tabPanel("Intro", verbatimTextOutput("intro")),
                  tabPanel("Story 1", plotOutput("story1")),
                  tabPanel("Story 2", textOutput("story2")),
                  tabPanel("Story 3", tableOutput("story3"))
      )
      
  )
)

server <- function(input, output){
  
  output$intro <- renderText({
    "Intro"
  })
  
}

shinyApp(ui = ui, server = server)