library(shiny)
library(shinydashboard)
library(leaflet)
#library(tidyverse)
library(leaflet.extras)
library(lubridate)
#library(rgdal)
library(shinyBS)
library(shinyjs)
library(shinyWidgets)
library(plotly)

# Define UI for application that explores farm survey data
shinyUI(
  dashboardPage(skin = "blue",
  title = "Malawi Farm Survey",
  dashboardHeader(title = "Malawi Farm Survey", titleWidth = 420),
  
  # interactive sidebar with menu and widgets
  dashboardSidebar(width = 420,
                   tags$div(
                     tags$blockquote("Data Exploration Tools"),
                     style = "padding: 10px;"
                     
                   ),
                   useShinyjs(),
                   tags$head(
                     tags$style(HTML("
                                     .multicol { 
                                     height: 18em;
                                     -webkit-column-count: 2; /* Chrome, Safari, Opera */ 
                                     -moz-column-count: 2;    /* Firefox */ 
                                     column-count: 2; 
                                     -moz-column-fill: auto;
                                     -column-fill: auto;
                                     width: 19em;
                                     color: black;
                                     }
                                     div.checkbox {margin-top: 0px;}
                                     "))
                   ),
                   tags$head(
                     tags$style(type = "text/css", 
                                ".shiny-input-container {padding-top: 0px !important;}
                                 .title {margin: auto; width: 200px}")
                   ),
                   
                   bsCollapse(id = "colps_side",
                              
                              bsCollapsePanel(title = HTML('<font style="color: white;">File</font>'),
                                         value="fileCollapse",
                                         style="primary",
                                         tags$div(align = 'left',
                                                  style = "color:black;",
                                                  fileInput("file", label = h3("File input"),
                                                            multiple = FALSE,
                                                            accept = c(".csv", "text/csv")))),
                              
                              bsCollapsePanel(title = HTML('<font style="color: white;">Locations</font>'), 
                                              value="locCollapse", 
                                              style = "primary",
                                              fluidRow(
                                                
                                                column(6, span(style = "color:black;",
                                                               h4("Choose Region"),
                                                               checkboxGroupInput(inputId = "region", label = NULL,
                                                                                  choices = list("Northern" = "Northern", "Central" = "Central"),
                                                                                  selected = c("Northern", "Central")))),
                                                
                                                column(6, span(style = "color: black;",
                                                               materialSwitch(inputId = "distSwitch",
                                                                              label = "Select by District",
                                                                              status = "primary",
                                                                              right = TRUE),
                                                               materialSwitch(inputId = "epaSwitch",
                                                                              label = "Select by EPA",
                                                                              status = "primary",
                                                                              right = TRUE),
                                                               uiOutput("altitude")))
                                              ),
                                              br(),
                                              hidden(uiOutput("districtPanel")),
                                              br(),
                                              hidden(uiOutput("epaHeading")),
                                              hidden(uiOutput("epaPanel"))
                                              
                              ),
                              
                              bsCollapsePanel(title = HTML('<font style="color: white;">Sustainable Agriculture</font>'),
                                              value="sustCollapse",
                                              style = "primary",
                                              
                                              tags$div(align = 'left',
                                                       style = "color:black;",
                                                       fluidRow(
                                                         column(4,
                                                                radioButtons(inputId = "sust_log",
                                                                             label = h4("Uses Sustainable Agriculture?"),
                                                                             choices = list("All" = 1,
                                                                                            "Yes" = 2,
                                                                                            "No" = 3),
                                                                             selected = 1)),
                                                         column(4,
                                                                radioButtons(inputId = "sust_andor",
                                                                             label = h4("Combination of Sustainable types"),
                                                                             choices = list("AND" = " & ",
                                                                                            "OR" = " | "),
                                                                             selected = " | ")),
                                                       
                                                         column(2,
                                                                dropdownButton(
                                                                  tags$h3("Chart of Sustainable Agriculture Types"),
                                                                  plotlyOutput("sustbar"),
                                                                  circle = TRUE, 
                                                                  status = "info", 
                                                                  icon = icon("bar-chart-o"), 
                                                                  width = "700px",
                                                                  tooltip = tooltipOptions(title = "Click for graph of sustainable agriculture types.")))
                                                       ),
                                                       h4 ("Select Sustainable Activities"),
                                                       uiOutput("sust_inputs")
                                              )
                              ),
                              
                              bsCollapsePanel(title = HTML('<font style="color: white;">Crops</font>'),
                                              value="sustCollapse",
                                              style = "primary",
                                              
                                              tags$div(
                                                align = 'left',
                                                style = "color:black;",
                                                h4("Combination of Crops"),
                                                fluidRow(
                                                  column(6,
                                                         radioButtons(inputId = "crop_andor",
                                                                      label = "",
                                                                      choices = list("AND" = " & ",
                                                                                     "OR" = " | "),
                                                                      inline = TRUE,
                                                                      selected = " | ")),
                                                  column(6,
                                                         dropdownButton(
                                                           tags$h3("Chart of Crops"),
                                                           plotlyOutput("cropsbar"),
                                                           circle = TRUE, 
                                                           status = "info", 
                                                           icon = icon("bar-chart-o"), 
                                                           width = "700px",
                                                           tooltip = tooltipOptions(title = "Click for graph of crops.")))
                                                ),
                                                h4 ("Select Crops"),
                                                fluidRow(uiOutput("crop_inputs")),
                                                h4("Additional Agriculture"),
                                                fluidRow(
                                                  column(3,
                                                         prettySwitch(inputId = "Vegetables.On",
                                                                      label = "Vegetables", 
                                                                      status = "primary",
                                                                      slim = TRUE)),
                                                  column(3, uiOutput("crop_ext_Veg_chk")),
                                                  column(3,
                                                         prettySwitch(inputId = "Fruit_trees.On",
                                                                      label = "Fruit Trees", 
                                                                      status = "primary",
                                                                      slim = TRUE)),
                                                  column(3, uiOutput("crop_ext_Fruit_chk"))
                                                )
                                              )
                              ),
                              
                              bsCollapsePanel(title = HTML('<font style="color: white;">Livestock</font>'),
                                              value="sustCollapse",
                                              style = "primary",
                                              
                                              tags$div(align = 'left',
                                                       style = "color:black;",
                                                       fluidRow(
                                                         column(4,
                                                                radioButtons(inputId = "livstk_log",
                                                                             label = h4("Rears Livestock?"),
                                                                             choices = list("All" = 1,
                                                                                            "Yes" = 2,
                                                                                            "No" = 3),
                                                                             selected = 1)),
                                                         column(4,
                                                                radioButtons(inputId = "livstk_andor",
                                                                             label = h4("Combination of Livestock types"),
                                                                             choices = list("AND" = " & ",
                                                                                            "OR" = " | "),
                                                                             selected = " | ")),
                                                         
                                                         column(2,
                                                                dropdownButton(
                                                                  tags$h3("Chart of Livestock Types"),
                                                                  plotlyOutput("livstkbar"),
                                                                  circle = TRUE, 
                                                                  status = "info", 
                                                                  icon = icon("bar-chart-o"), 
                                                                  width = "700px",
                                                                  tooltip = tooltipOptions(title = "Click for graph of livestock types.")))
                                                       ),
                                                       h4 ("Select Livcestock Types"),
                                                       uiOutput("livstk_inputs")
                                              )
                              ),
                              
                              bsCollapsePanel(title = HTML('<font style="color: white;">Respondant</font>'),
                                              value="trnCollapse",
                                              style = "primary",
                                              
                                              tags$div(align = 'left',
                                                       style = "color:black;",
                                                       fluidRow(
                                                         column(5,
                                                                checkboxGroupInput(inputId = "resp_Gender",
                                                                    label = h4("Gender"),
                                                                    choices = list("Male" = "Male","Female" = "Female"),
                                                                    selected = c("Male","Female"))),
                                                         column(7,
                                                                uiOutput ("LiteracyPanel"))),
                                                       uiOutput ("Followers"),
                                                       uiOutput ("FarmSize"),
                                                       uiOutput("Training"),
                                                       sliderInput("bins",
                                                                   "Number of bins:",
                                                                   min = 1,
                                                                   max = 50,
                                                                   value = 30))
                              ) 
                   ),           
                   # Cemac credit tag
                   div(style = "padding: 10px;",
                       helpText("Dashboard by", a("CEMAC, University of Leeds", 
                                                  href = "https://www.cemac.leeds.ac.uk", 
                                                  target = "_blank")))
                   
                     ),
  
  # Main panel for displaying outputs ----
  dashboardBody(
    
    bsCollapse(id = "colps_main", open = "mapCollapse",
               bsCollapsePanel(title = HTML('<font style="color : white">Map View</font>'), value="mapCollapse", style = "primary",
                               
                               tags$head(tags$style("#myMap{height:75vh !important;}")),
                               
                               leafletOutput("myMap")
                               
               ),
               bsCollapsePanel(title = HTML('<font style="color : white">Plot View</font>'), value="plotCollapse", style = "primary",
                               
                               tags$head(tags$style("#trnHist{height:35vh !important;}")),
                               fluidRow(
                                        tags$div(class="title", h3("Training Date")),
                                 column(6,
                                        plotOutput("trnHist")),
                                 column(4, offset = 1,
                                        tableOutput("trnSum"))),
                               fluidRow(
                                 tags$div(class="title", h3("Farm Size")),
                                 column(6,
                                        plotOutput("farmHist", height="300px")),
                                 column(4, offset = 1,
                                        tableOutput("farmSum")))
               ),
               bsCollapsePanel(title = HTML('<font style="color : white">Table View</font>'), value="tableCollapse", style = "primary",
                               downloadButton("downloadData", "Export CSV"),
                               box(
                                 title = "Selected subset of data", width = NULL, status = "primary",
                                 div(style = 'overflow-x: scroll', dataTableOutput('myDataTable')))
               )
    )
    
  ))
)