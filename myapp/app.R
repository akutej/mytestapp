#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinytitle)
library(ggplot2)
library(shinythemes)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = shinytheme("paper"),
    title = "RiskAssessment",
    use_shiny_title(),
    #span(img(src = "risk.jpg"),br(),br(),"Dashboard NEW")),
    headerPanel(div(img(src = "risk.jpg"),style="text-align: center;")),
    
    sidebarLayout(position = "right",
      
      sidebarPanel(
      tags$h2("Benutzermenü"),
      #inputPanel
      tags$h3("Parametereinstellungen"),
      selectInput("projectgroup",label = h4("Projektgruppe:"),
                  c("Projektgruppe 1" = "pgroup1",
                    "Projektgruppe 2" = "pgroup2",
                    "Projektgruppe 3" = "pgroup3",
                    "Projektgruppe 4" = "pgroup4",
                    "Projektgruppe 5" = "pgroup5",
                    "Alle" = "alle"),selected = "alle"),
      
      selectInput("Scenario",label = h4("Szenarien:"),
                  c("Szenario 1" = "szenario1",
                    "Szenario 2" = "szenario2",
                    "Szenario 3" = "szenario3",
                    "Szenario 4" = "szenario4",
                    "Szenario 5" = "szenario5",
                    "Szenario 6" = "szenario6",
                    "Szenario 7" = "szenario7",
                    "Szenario 8" = "szenario8",
                    "Alle" = "alle"),selected = "alle"),
      
      #tags$h5("Benutzergruppen"),
      checkboxGroupInput("checkGroup", label = h4("Benutzergruppen"), 
                         choices = list("Projektteam" = 1, "Mitarbeiter" = 2),
                          ),
      tags$br(),
      tags$h3("Links"),
      tags$br(),
      tags$a(href="https://rstudio.risikobewertung.com","RStudio Server", target="_blank"),
      tags$br(),
      tags$br(),
      
      
      ),
      
      mainPanel(align = "center",
        #hr(),
        #fluidRow(column(3, verbatimTextOutput("value"))),
        tabsetPanel(type = "tabs",
                    tabPanel("Home",
                      tags$h1("Welcome Page"), 
                    ),
                    tabPanel("Plot",
                             tags$span("Folgende Gruppen wurden gewählt"), textOutput("value"),
                             tags$br(),
                             plotOutput("plot", width = "600px", height = "600px"), #,inline = FALSE         
                            ),
                    tabPanel("Summary",
                             tags$label("Anzahl der Datensätze: "), textOutput("rowsum",inline = T),
                             
                             ),
                    tabPanel("Table",
                             fluidRow(
                               column(12,
                                      tableOutput('table')
                             )
                             )
                             ),            
                    tabPanel("Datamanagement",
                             ),
                    tabPanel("Survey Summary",
                             
                             plotOutput("bar", width = "600px", height = "600px"),
                             
                    )            
                    
                    
                             
        ),
        
       
      
        
        )
      )
    
    

    # Sidebar with a slider input for number of bins 
 
        # Show a plot of the generated distribution
  )    
# Define server logic required to draw a histogram
server <- function(input, output) {

  
  
    # You can access the values of the widget (as a vector)
    # with input$checkGroup, e.g.
     #output$value <- renderPrint({ input$checkGroup })
      #message("The value of input$count is ", input$checkGroup)    
     
  
      output$plot <- renderPlot(plot(1:3), res = 96)
     
      #paste("ausgewählte Gruppen: ",input$checkGroup)})
      #input$checkGroup 
  
     #output$plot <- renderPlot({
       #Produce scatter plot
       
      #    ggplot()},
       #   res = 96)
     testtable <- read.table("Data/data.csv", header=TRUE, sep=";", dec=".")
     output$table <- renderTable(testtable)
     
     accounttable <- read.table("Data/account.csv", header=TRUE, sep=";", dec=".")
     newtest <- aggregate(accounttable, by=list(Category="ACC2SURV_RATEGUI"), FUN=sum)
     
     #accountoutput$table <- renderTable(accounttable)
     
     output$bar <- renderPlot({
       
       color <- c("blue", "red")
       
       barplot(colSums(newtest[,c("ACC2SURV_RATEGUI")]),
               ylab="Total",
               xlab="Census Year",
               names.arg = c("rated"),
               col = color)
     })
     
     
     countedtesttable <- nrow(testtable)
     output$rowsum <- renderText(countedtesttable)
}

# Run the application 
shinyApp(ui = ui, server = server)
