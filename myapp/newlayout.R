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
                           
                           sidebarLayout(position = "left",
                                         
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
                    tabPanel("Survey Analysis",
                             
                             #plotOutput("bar", width = "400px", height = "400px"),
                             #tags$br(),
                             
                             fluidPage(
                              splitLayout(cellWidths = c("33%", "33%", "34%"),
                             
                               plotOutput("detailbar1", width = "600px", height = "500px"),
                               plotOutput("detailbar2", width = "600px", height = "500px"),
                               tags$label(("Der Altersdurchschnitt der Befragten: "),textOutput("MeanAge",inline = T)),
                               
                              )
                             )
                             ),
                tabPanel("Survey Summary",
                         fluidPage(
                           splitLayout(cellWidths = c("33%", "33%", "34%"),
                                       
                                       fluidPage(
                                         tags$label("Details zu den Befragten: "),
                                         ),
                                       fluidPage(
                                       tags$label("Details zu den Antworten: "),
                                       ),
                                       fluidPage(
                                       tags$label("Details zu den Fragen"),
                                       tags$br(),
                                       tags$br(),
                                       tags$label(("Anzahl der Risiko- und Chancen-Szenarien: "),textOutput("questiondetail1",inline = T)),
                                       tags$br(),
                                       tags$label(("Anzahl der Fragenkategorien: "),textOutput("questiondetail2",inline = T)),
                                       tags$br(),
                                       tableOutput('tableCategories'),
                                       
                                       ),
                                    
                            
                                       )
                           )
                                       
                ),
  )   
  )    


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
     testtable <- read.table("Data/questions.csv", header=TRUE, sep=";", dec=".")
     output$table <- renderTable(testtable)
     
     #ACCOUNT Auswertungen
     accounttable <- read.table("Data/accounts.csv", header=TRUE, sep=";", dec=".")
     accframe=as.data.frame.matrix(accounttable)
     CALCMeanAGE <- mean(accframe$PERS_ALTER, trim = 0, na.rm = TRUE)  
     
     questions <- read.csv(file = 'Data/questions.csv', header=TRUE, sep=";", dec=".", encoding="auto")
     qframe=as.data.frame.matrix(questions)
     newcount <- table(qframe$QUES_CATEGORY)
     questcount <- nrow (qframe[duplicated(qframe$QUES_ID), ]) #zählt die Anzahl ohne Berücksichtigung der Duplikate
     Categorycount <- nrow (newcount)
     df2 <- qframe %>% group_by(QUES_CATEGORY,QUES_TYP) %>%    #weißt dem neuen dataframe2 den gefiterten dataframe zu
       summarise(total_count=n(),.groups = 'drop') %>%
       as.data.frame()
     colnames(df2) = c("Kategorie", "Szenarientyp", "Anzahl")
     
     output$questiondetail1 <- renderText(questcount)
     output$questiondetail2 <- renderText(Categorycount)
     output$tableCategories <- renderTable(df2)
     
     
     output$detailbar1 <- renderPlot({
       
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
       ggp + geom_text(size = 3, position = position_stack(vjust = 0.8)) + labs(title = "Which method do you like better?") + labs(x = "method")+ labs(y = "persons")+scale_fill_discrete(name = "groups")
        
       
     })
     
     output$detailbar2 <- renderPlot({
       
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
       ggp + geom_text(size = 3, position = position_stack(vjust = 0.8)) + labs(title = "Which method do you like better?") + labs(x = "method")+ labs(y = "persons")+scale_fill_discrete(name = "groups")
       
       
     })
     
     
     
     
     
     
     output$MeanAge <- renderText(CALCMeanAGE)
     
     #gibt die Tabelle aus
     countedtesttable <- nrow(testtable)
     output$rowsum <- renderText(countedtesttable)

     
}

# Run the application 
shinyApp(ui = ui, server = server)
