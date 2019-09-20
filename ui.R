#ShinyDashboard
library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)

#wordcloud
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)

#ggplot
library(ggplot2)
library(ggthemes)
library(plotly)
library(animation)

#clustering
library(cluster)
library(factoextra)

library(scales) #crime regression
library(DT) #dataTable
library(tidyr) #arrange
library(tidyverse)
library(dplyr) #filter
library(reshape2) #aggregate functions

#spatial Database
library(sp)  
library(stringr)
library(rgeos)
library(maptools)


ui <- dashboardPage(
         
          dashboardHeader(title = "Crime Analysis"),
  
              dashboardSidebar(
                    sidebarMenu(
                        menuItem("Browse Data", tabName = "Browsedata", icon = icon("dashboard")),
                       
                        menuItem("District-wise Crime  ", tabName= "plot2", icon = icon("bar-chart")),
                      
                        
                        menuItem("Average Crime Rate",tabName = "average",icon = icon("line-chart"),
                                  menuSubItem("Across India", tabName = "avg_india",icon = icon("angle-double-right")),
                                  menuSubItem("Across States and UT", tabName = "avg_states",icon = icon("angle-double-right"))),
                        menuItem("Crime Rate per lakh women  ", tabName= "plot3", icon = icon("signal")),
                        menuItem("Literacy Rate and Crime", tabName = "literacy",icon = icon("pie-chart")),
                        menuItem("Clustering ", tabName= "plot4",icon = icon("object-group"),
                                 menuSubItem("K-means", tabName = "kmeans",icon = icon("angle-double-right")),
                                 menuSubItem("Hierarchical", tabName = "dendo",icon = icon("angle-double-right"))),
                        menuItem("Linear Regression",tabName = "Linear",icon = icon("expand")),
                        menuItem("Map Representation ", tabName= "mapplot", icon = icon("map-marker")),
                       menuItem("Conclusion ", tabName= "conclusion", icon = icon("check-circle"))
                      )
              ),
          
          dashboardBody(
            tags$head(tags$style(HTML(''))),
            tabItems(
              
              tabItem(
                tabName = "Browsedata",
                box(title= " Browse File ",status = "primary",collapsible = TRUE,solidHeader = T,fileInput('file1', 'Choose CSV File',
                          accept=c('text/csv', 
                                   'text/comma-separated-values,text/plain', 
                                   '.csv'))),
                 fluidRow( plotOutput('wordplot',height = "600px"))

              ),
             
              #district
              tabItem(
                tabName = "plot2",
                headerPanel('District-wise crime'),
                
                fluidRow(box(title="Select Input", collapsible = TRUE,status = "primary",solidHeader = T,column(4,selectInput('year', 'Select Year', "")),
                
                column(6,selectInput('state', 'Select State/UT', "")))),
                
                 tabBox(width = "500px",
                  tabPanel(title = 'Rape',valueBoxOutput("RapeBox",width = 3),
                           fluidRow(column(width = 12,h3(''),plotOutput('View_rape')))
                  ),
                  tabPanel(title = 'Dowry Deaths', valueBoxOutput("DowryBox",width = 3),
                           fluidRow(column(width = 12, h3(''),plotOutput('View_dowry')))
                  ),
                  tabPanel(title = 'Kidnapping',valueBoxOutput("KidBox",width = 3),
                           fluidRow(column(width = 12, h3(''),plotOutput('View_kidnapping')))
                  ),
                  tabPanel(title = 'Assault',valueBoxOutput("AssaultBox",width = 3),
                           fluidRow(column(width = 12, h3(''),plotOutput('View_Assault')))
                  ),
                  tabPanel(title = 'Insult',valueBoxOutput("InsultBox",width = 3),
                           fluidRow(column(width = 12, h3(''),plotOutput('View_insult')))
                  ),
                  tabPanel(title = 'Cruelty',valueBoxOutput("CrueltyBox",width = 3),
                           fluidRow(column(width = 12, h3(''),plotOutput('View_cruelty')))
                  ),
                  tabPanel(title = 'Importation',valueBoxOutput("ImportBox",width = 3),
                           fluidRow(column(width = 12, h3(''),plotOutput('View_import')))
                  )
                ),style='width: 1000px; height: 1000px'
              ),
              
              
              #average
              tabItem(
                tabName = "avg_india",
                headerPanel("Average Crimes Across India (2001 - 2012) "),
                fluidRow(box(title="Select Input", collapsible = TRUE,status = "primary",solidHeader = T,selectInput('agg_crimes', 'Select Crime ',choices =""))),
                fluidRow(plotOutput('averageplot'))
               # plotOutput("tp")
              ),
              tabItem(
                tabName = "avg_states",
                headerPanel("Average Crimes Across States and UT "),
                fluidRow(box(title="Select Input", collapsible = TRUE,status = "primary",solidHeader = T,selectInput('agg_state', 'Select State ',choices =""))),
                
                tabBox(width = "500px",
                
                  tabPanel(title = "plot1",fluidRow(column(12,box(plotlyOutput('averageplot2'),width = 12,status = "primary",solidHeader = T)))),
                tabPanel(title = "plot2",fluidRow(column(12,box(plotlyOutput('averageplot3'),width = 12,status = "primary",solidHeader = T))))
              )
              ),
              
              #PerLakh
              tabItem(
                tabName = "plot3",
                headerPanel('Crime Rate Per Lakh Women'),
                br(),
                fluidRow(column(4,h4(radioButtons("radiolakh", label = h3("According to Census"),
                                                  choices = list("2001" = "2001",
                                                                 "2011" = "2011"),
                                                  selected = "2001",inline = T))),
                         column(4,verbatimTextOutput("perlakhtext")),
                         column(4,verbatimTextOutput("perlakhtext2"))),
                fluidRow(column(12,box(plotlyOutput('pop',height = '800px'),width = 12,status = "primary",solidHeader = T)))
                
                
              ),
              
              #Literacy
              tabItem(
                tabName = "literacy",
                headerPanel('Literacy Rate on Crime'),
                fluidRow(column(4,h4(radioButtons("radioliteracy", label = h3("According to Census"),
                                                  choices = list("2001" = "2001",
                                                                 "2011" = "2011"),
                                                  selected = "2001",inline = T)))),
                br(),
                
                tabBox(width = "500px",
                       tabPanel(title= "Highest Crime",fluidRow(column(7,plotOutput('ggbar',height = '500px')),
                                                                column(5,plotlyOutput('literacy1',height = "500px")))),
                       tabPanel(title= "Lowest Crime",fluidRow(column(7,plotOutput('ggbarlow',height = '500px')),
                                                               column(5,plotlyOutput('literacy1low',height = "500px"))))
                       
                       
                ),style='width: 1100px; height: 1000px'
                
              ),
              
              #clustering
              tabItem(
                tabName = "kmeans",
                headerPanel('Cluster'),
                
                fluidRow(box(title="Select Input",status = "primary", collapsible = TRUE,solidHeader = T,selectInput('state_cluster', 'Select STATE ', ""))),
                
                fluidRow(column(12,box(plotOutput('cluster2'),width = 12,status = "primary",solidHeader = T))),
                br(),
                fluidRow(box(title = "Clusters", width = 5,status = "primary", height = "575",solidHeader = T, dataTableOutput("Table")))
                
              ),
              
              tabItem(
                tabName = "dendo",
                headerPanel('Heirarchal Clustering'),
                fluidRow(box(title="Select Input", collapsible = TRUE,status = "primary",solidHeader = T,width = 8,column(6,sliderInput("dendo_rate", "Crime Rate",min = 0, max = 1500, value = 100)),
                         column(6,selectInput('dendo_year', 'Select Year', "")))
                         
                ),
                fluidRow(column(12,box(plotOutput('dendogram',height = "600px"),width = 12,status = "primary",solidHeader = T)))
              ),
              
              #linear Regression
              tabItem(
                tabName = "Linear",
                headerPanel("Linear Regression "),
                selectInput('linearstate', 'Select State ',choices =""),
                fluidRow(column(3,radioButtons("radio", label = h3("Crime"),
                             choices = list("RAPE" = "Rape",
                                            "DOWRY DEATH" = "DowryDeaths", 
                                            "ASSAULT ON WOMEN" = "Assault",
                                            "CRUELTY BY HUSBAND OR RELATIVES" = "Cruelty",
                                            "INSULT TO THE MODESTY OF WOMEN"="Insult",
                                            "KIDNAPPING & ABDUCTION" = "Kidnapping",
                                            "IMPORTATION OF GIRLS" = "Importation",
                                            "TOTAL CRIMES AGAINST WOMEN" = "TOTAL"), 
                             selected = "Rape")),
                         column(9,
                                box(
                                  title = "Summary",
                                  solidHeader = TRUE, collapsible = TRUE,
                                  width = 12,
                                  verbatimTextOutput("summary_linear")
                                
                           ))),
                fluidRow(column(12,box(plotOutput('linearplot'),width = 12,status = "primary",solidHeader = T))),
                br(),br(),
                fluidRow(column(4,box(
                  title = "Predicted Value",
                  solidHeader = TRUE, collapsible = TRUE,
                  width = 12,
                  verbatimTextOutput("PredictOutput")
                  
                )),
                column(3,box(
                  title = "Actual Value",
                  solidHeader = TRUE, collapsible = TRUE,
                  width = 12,
                  verbatimTextOutput("ActualOutput")
                ))
                )
                
              ),
              
              #crime Map
              tabItem(
                tabName = "mapplot",
                headerPanel("Map"),
                titlePanel("Crime Map of India"),
                
                fluidRow(
                  column(4,
                        
                         selectizeInput(
                           'id', label = "Year", choices = NULL,multiple=FALSE,selected="X2012",
                           options = list(create = TRUE,placeholder = 'Choose the year'))
                           
                         ,
                         radioButtons("radiomap", label = h3("Crime"),
                                      choices = list("RAPE" = "RAPE",
                                                     "DOWRY DEATH" = "DOWRY DEATH", 
                                                     "ASSAULT ON WOMEN" = "ASSAULT ON WOMEN WITH INTENT TO OUTRAGE HER MODESTY",
                                                     "CRUELTY BY HUSBAND OR RELATIVES" = "CRUELTY BY HUSBAND OR RELATIVES",
                                                     "INSULT TO THE MODESTY OF WOMEN"="INSULT TO THE MODESTY OF WOMEN",
                                                     "KIDNAPPING & ABDUCTION" = "KIDNAPPING & ABDUCTION",
                                                     "TOTAL CRIMES AGAINST WOMEN" = 'TOTAL CRIMES AGAINST WOMEN'), 
                                      selected = "RAPE")
                  ),
                  
                  # Show the choropleth map                              
                  column(8,
                         box(plotOutput("distPlot",height = "600px"),width = 12,status = "primary",solidHeader = T))
                )
              ),
              tabItem(
                tabName = "conclusion",
                headerPanel("Conclusion : Women Safety"),
                br(),br(),br(),br(),
                fluidRow(column(12,box(plotlyOutput("conclude"),title="Total Crime Against Women VS Year(2001 - 2012)",width = 12,status = "primary", collapsible = TRUE,solidHeader = T))),
                br(),
                fluidRow(column(12,box(plotOutput("wordcloudstate"),title="States of Concern",width = 12,status = "primary", collapsible = TRUE,solidHeader = T)))
              )
            ),
            mainPanel(

            )
            
          )
          
                    
      )
                    