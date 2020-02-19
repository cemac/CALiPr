library(shiny)
library(shinydashboard)
library(leaflet)
library(tidyverse)
library(leaflet.extras)
library(lubridate)
library(rgdal)
library(raster)
library(shinyBS)
library(shinyjs)
library(shinyWidgets)
library(plotly)
library(DT)

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
                                                       fileInput("file1", "Choose CSV File",
                                                                 multiple = TRUE,
                                                                 accept = c("text/csv",
                                                                            "text/comma-separated-values,text/plain",
                                                                            ".csv")),
                                                       
                                                       # Horizontal line ----
                                                       tags$hr(),
                                                       
                                                       # Input: Checkbox if file has header ----
                                                       checkboxInput("header", "Header", TRUE),
                                                       
                                                       # Input: Select separator ----
                                                       radioButtons("sep", "Separator",
                                                                    choices = c(Comma = ",",
                                                                                Semicolon = ";",
                                                                                Tab = "\t"),
                                                                    selected = ","),
                                                       
                                                       # Input: Select quotes ----
                                                       radioButtons("quote", "Quote",
                                                                    choices = c(None = "",
                                                                                "Double Quote" = '"',
                                                                                "Single Quote" = "'"),
                                                                    selected = '"'))),

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
                                              value="cropCollapse",
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
                                              value="livstkCollapse",
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
                                                       uiOutput("Training"))
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
    
    tags$style(HTML("
    .tabbable > .nav > li > a                  {background-color: #428bca;  color:white}
    .tabbable > .nav > li[class=active]    > a {background-color: #244e73; color:white}
    ")),

    tabsetPanel(id = "tab_main",
               tabPanel(title = HTML('Map View'), value="mapTab", style = "primary",

                        tags$head(tags$style("#myMap{height:80vh !important;}")),
                        br(),
                        box(status = "primary",
                            width = 12,
                            leafletOutput("myMap")
                        )

               ),
               tabPanel(title = HTML('Plot View'), value="plotTab", style = "primary",

                               tags$head(tags$style("#trnHist{height:32vh !important;}")),
                               tags$head(tags$style("#farmHist{height:32vh !important;}")),
                               br(),
                              
                               fluidRow(
                                 box(title="Training Date", 
                                     status = "primary",
                                     width=12,
                                     solidHeader = T,
                                     column(6,
                                            plotlyOutput("trnHist")),
                                     column(4, offset = 1,
                                            fluidRow(
                                              tableOutput("trnSum"),
                                              sliderInput("trnBins",
                                                          "Number of bins:",
                                                          min = 1,
                                                          max = 100,
                                                          value = 50))))),
                               fluidRow(
                                 box(title="Farm Size",
                                     status = "primary",
                                     width=12,
                                     height=400,
                                     solidHeader = T,    
                                     column(6,
                                            plotlyOutput("farmHist")),
                                     column(4, offset = 1,
                                            fluidRow(
                                              tableOutput("farmSum"),
                                              sliderInput("frmBins",
                                                          "Number of bins:",
                                                          min = 1,
                                                          max = 50,
                                                          value = 25)))))
               ),
               tabPanel(title = HTML('Table View'), value="tableTab", style = "primary",
                               br(),
                               disabled(downloadButton("downloadData", "Export CSV")),
                               box(
                                 title = "Selected subset of data", width = NULL, status = "primary",
                                 div(style = 'overflow-x: scroll', DT::dataTableOutput('myDataTable')))
               )
    )

  ))
)
