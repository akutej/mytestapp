library(shiny)
library(shinytitle)
library(ggplot2)
library(shinythemes)
library(dplyr)
library(RColorBrewer)
library(tidyverse)
library(reactable)
library(scales)

  # Define UI for application that draws a histogram
ui <- fluidPage( theme = shinytheme("united"),  #"paper""spacelab"flatly*cosmo
                list(tags$head(HTML('<link rel="icon", href="favicon-6.png", type="image/png" />'))),
                div(style="padding: 5px 0px; width: '100%'",
                titlePanel(   title="",
                              windowTitle="Risk Assessment"
                          ),
                          div(img(src="risk.jpg", height=40 ),""),
                          ),
                
      navbarPage( theme = shinytheme("flatly"),
                            title="",
                            tabPanel("Home","",
                            tags$h1("Welcome Page"), 
                ),
                
                tabPanel("Plot","",
                         
                      fluidPage(
                           
                           sidebarLayout(position = "right",
                                         
                                         sidebarPanel(
                                           tags$h2("Benutzermen├╝"),
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
                                                   
                                                   tags$span("Folgende Gruppen wurden gew├Ąhlt"), textOutput("value"),
                                                   tags$br(),
                                                   plotOutput("plot", width = "600px", height = "600px"), #,inline = FALSE         
                                                   
                                                   ),
                         
                           ),
                            ),
                ),
                            
                
                tabPanel("Summary",
                             tags$label("Anzahl der Datens├Ątze: "), textOutput("rowsum",inline = T),
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
                             tags$br(),
                             plotOutput("detailbar", width = "500px", height = "500px"),
                   )            
  )   
  )    

# Define server logic required to draw a histogram
server <- function(input, output) {
    # You can access the values of the widget (as a vector)
    # with input$checkGroup, e.g.
     #output$value <- renderPrint({ input$checkGroup })
      #message("The value of input$count is ", input$checkGroup)    
     
      output$plot <- renderPlot(plot(1:3), res = 96)
     
      #paste("ausgew├Ąhlte Gruppen: ",input$checkGroup)})
      #input$checkGroup 
  
     #output$plot <- renderPlot({
       #Produce scatter plot
       
      #    ggplot()},
       #   res = 96)
     testtable <- read.table("Data/data.csv", header=TRUE, sep=";", dec=".")
     output$table <- renderTable(testtable)
     
     #ACCOUNT Auswertungen
     accounttable <- read.table("Data/account.csv", header=TRUE, sep=";", dec=".")
     accframe=as.data.frame.matrix(accounttable)
  
     
     output$detailbar <- renderPlot({
       
       accframe2 <- cbind(accframe,AGEGroup=NA)
       accframe3 <- accframe2 %>% filter(!is.na(ACC2SURV_RATEGUI))
       accframe3 <- accframe3 %>% filter(!is.na(ACC2SURV_INFOAGE))
       accframe4 <- accframe3 %>% 
         mutate (AGEGroup = case_when(
           #ACC2SURV_INFOAGE == "53" ~ 'ALT',
           between(ACC2SURV_INFOAGE,21,30) ~ "age 21-30",
           between(ACC2SURV_INFOAGE,31,40) ~ "age 31-40",
           between(ACC2SURV_INFOAGE,41,50) ~ "age 41-50",
           between(ACC2SURV_INFOAGE,51,60) ~ "age 51-60",
           between(ACC2SURV_INFOAGE,61,70) ~ "age 61-70"
         )) 
       
       accframe4 <- accframe4 %>% 
         mutate (ACC2SURV_RATEGUI = case_when(
           ACC2SURV_RATEGUI == "1" ~ 'classic',
           ACC2SURV_RATEGUI == "2" ~ 'graphic',
           ACC2SURV_RATEGUI == "3" ~ 'equal'
         ))
       accframe4 <- accframe4 %>% select(ACC2SURV_RATEGUI,ACC2SURV_INFOAGE, AGEGroup)
       newtest2 <- aggregate(accframe4, by=list(accframe4$AGEGroup,accframe4$ACC2SURV_RATEGUI), FUN=length)
       ggp <- ggplot(newtest2, aes(x = reorder(Group.2, -ACC2SURV_INFOAGE), y = ACC2SURV_INFOAGE,  fill = Group.1, label = ACC2SURV_INFOAGE)) +  # Create stacked bar chart
       geom_bar(stat = "identity")
       ggp + geom_text(size = 3, position = position_stack(vjust = 0.8)) + labs(title = "user voting") + labs(x = "method")+ labs(y = "persons")+scale_fill_discrete(name = "groups")
       
       
     })
     #gibt die Tabelle aus
     countedtesttable <- nrow(testtable)
     output$rowsum <- renderText(countedtesttable)

     
}

# Run the application 
shinyApp(ui = ui, server = server)
