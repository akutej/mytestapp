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
library(dplyr)
library(RColorBrewer)
library(tidyverse)
library(reactable)

  # Define UI for application that draws a histogram
ui <- fluidPage( theme = shinytheme("paper"),
                list(tags$head(HTML('<link rel="icon", href="favicon-6.png", type="image/png" />'))),
                div(style="padding: 1px 0px; width: '100%'",
                titlePanel(
                              title="", windowTitle="Risk Assessment"
                          )
                ),
      navbarPage( theme = shinytheme("paper"),
                            title=div(img(src="risk.jpg", height=30 ),""),
                            tabPanel("Home","",
                            tags$h1("Welcome Page"), 
                ),
                
                tabPanel("Plot","",
                         
                      fluidPage(
                           
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
                                                   
                                                   tags$span("Folgende Gruppen wurden gewählt"), textOutput("value"),
                                                   tags$br(),
                                                   plotOutput("plot", width = "600px", height = "600px"), #,inline = FALSE         
                                                   
                                                   ),
                         
                           ),
                            ),
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
                             
                             plotOutput("bar", width = "400px", height = "400px"),
                             
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
     accframe=as.data.frame.matrix(accounttable)
     newtest <- aggregate(accframe, by=list(accounttable$ACC2SURV_RATEGUI), FUN=length )
     #reduced <- subset(newtest, select=c("Acc_ID"))
     colnames(newtest)
     names(newtest)[2] <- "User"
     newacctable <- newtest[2]
     
     #newtest <- aggregate(accounttable, by=list(Category="ACC2SURV_RATEGUI"), FUN=sum)
     
     #accountoutput$table <- renderTable(accounttable)
     
     output$bar <- renderPlot({
       
       
       counts <- newacctable$User  
       coul <- brewer.pal(5, "Set2") 
       barplot(counts, main="Which method do you find better?",
               #xlab="methods",
               ylab="number of ratings",
               names.arg = c("classical","graphical","equal"),
               col = coul,#rgb(0.2,0.4,0.6)
               ylim=c(0,30)
               )
                
       
       
       #barplot(colSums(newacctable[,c("User")]),
       #         ylab="Total",
       #        xlab="Census Year",
       #         names.arg = c("rated"),
       #         col = color)
     })
     
     #gibt die Tabelle aus
     countedtesttable <- nrow(testtable)
     output$rowsum <- renderText(countedtesttable)
}

# Run the application 
shinyApp(ui = ui, server = server)
