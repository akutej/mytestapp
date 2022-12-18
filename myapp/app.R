#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    
    titlePanel( title=span(img(src = "risk.jpg"),br(),br(),"Dashboard NEW")),
    
    
    
    sidebarLayout(position = "right",
      sidebarPanel(("sidebar panel"),
      inputPanel("Paramtersetup"),
      selectInput("projectgroup", "Projektgruppe:",
                  c("Projektgruppe 1" = "pgroup1",
                    "Projektgruppe 2" = "pgroup2",
                    "Projektgruppe 3" = "pgroup3",
                    "Projektgruppe 4" = "pgroup4",
                    "Projektgruppe 5" = "pgroup5",
                    "Alle" = "alle"),selected = "alle"),
      
      selectInput("Scenario", "Szenarien:",
                  c("Szenario 1" = "szenario1",
                    "Szenario 2" = "szenario2",
                    "Szenario 3" = "szenario3",
                    "Szenario 4" = "szenario4",
                    "Szenario 5" = "szenario5",
                    "Szenario 6" = "szenario6",
                    "Szenario 7" = "szenario7",
                    "Szenario 8" = "szenario8",
                    "Alle" = "alle"),selected = "alle"),
                   
                   
                   ),
      mainPanel(
        # img (src='risk.jpg'),
        # p("Auswahlmenü"),
      )
    )
    
    

    # Sidebar with a slider input for number of bins 
 
        # Show a plot of the generated distribution
 
)    
# Define server logic required to draw a histogram
server <- function(input, output) {

 
    
}

# Run the application 
shinyApp(ui = ui, server = server)
