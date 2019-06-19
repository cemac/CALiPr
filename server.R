cropnames <- list("Crops_Maize" = "Maize",
                  "Crops_Millet" = "Millet",
                  "Crops_Sorghum" = "Sorghum",
                  "Crops_Cassava" = "Cassava",
                  "Crops_Sweet_Potato" = "Sweet Potato",
                  "Crops_European_Potato" = "European Potato",
                  "Crops_Soya" = "Soya",
                  "Crops_Groundnuts" = "Groundnuts",
                  "Crops_Beans" = "Beans",
                  "Crops_Pigeon_Pea" = "Pigeon Pea",
                  "Crops_Cowpea" = "Cowpea",
                  "Crops_Sugarcane" = "Sugarcane",
                  "Crops_Other" = "Other")
sust_ag <- list("Full_CA" = "Conservation Agriculture (a combination of Maximum Cover, Minimum Tillage, Rotations/Mixtures)",
                "Partial_CA" = "Any of: Maximum Cover; Minimum Tillage; Rotations/Mixtures",
                "Agroforestry" = "Agroforestry",
                "Manure_making" = "Manure Making",
                "Pit_planting" = "Pit Planting",
                "Sust_Other" = "Other")
livstk <- list("Livestock_Cattle" = "Cattle",
                  "Livestock_Goats_Sheep" = "Goats/Sheep",
                  "Livestock_Pigs" = "Pigs",
                  "Livestock_Chickens" = "Chickens",
                  "Livestock_Guinea_Fowl" = "Guinea Fowl",
                  "Livestock_Ducks" = "Ducks",
                  "Livestock_Rabbits" = "Rabbits",
                  "Livestock_Other" = "Other")

# Define server logic required for data exploration
shinyServer(function(input, output,session) {
  
  borders <- readOGR(dsn = ".", layer = "MAA-level_1")
  
  FarmSurvey <- read.csv("FarmSurveyClean.csv", header = TRUE, sep = ",")

  #-----------------------------------------------------------------------------------------------------#
  #              Location Panel                                                                         #
  #-----------------------------------------------------------------------------------------------------#
  
  regdat <- reactive({
      subset(FarmSurvey, Lat != 0 & Region %in% input$region)
  })
  
  observeEvent(input$distSwitch, {
    shinyjs::toggle(id = "districtPanel", condition = input$distSwitch)
  })
  
  observeEvent(input$epaSwitch, {
    shinyjs::toggle(id = "epaHeading", condition = input$epaSwitch)
    shinyjs::toggle(id = "epaPanel", condition = input$epaSwitch)
  })
  
  output$altitude <- renderUI({
    tags$div(align = 'left',
             style = "color:black;",
             h4("Select Altitude Range", style = "color:black;"),
             sliderInput(inputId = "alt_slide",
                         label = NULL,
                         min=signif(min(regdat()$Altitude, na.rm=T), digits=2),
                         max=signif(max(regdat()$Altitude, na.rm=T), digits=2),
                         value=c(min(regdat()$Altitude, na.rm=T),max(regdat()$Altitude, na.rm=T))
             )
    )
  })
  
  altdat <- reactive({
    if (! is.null(input$alt_slide)){
      subset(regdat(), Altitude >= input$alt_slide[[1]] &  Altitude <= input$alt_slide[[2]])
    } else { 
      regdat() }
  })
  
  output$districtPanel <- renderUI({ 
    tags$div(align = 'left',
             style = "color:black;",
             h4("Choose District(s)", style = "color:black;"),
             checkboxGroupInput(inputId = "district",
                                label = NULL,
                                choices = as.list(as.vector(unique(altdat()$District[altdat()$District != ""]))) %>%
                                          setNames(.,.)%>%
                                          .[order(names(.))],
                                inline = FALSE,
                                selected = unique(altdat()$District[altdat()$District != ""]))

    ) 
  })
  
  distdat<-reactive({ 
    if ( is.null(input$district) | input$distSwitch == F ){
      altdat()
    } else {
      subset(altdat(), District %in% input$district )#| District2 %in% input$district)
    }
  })
  
  output$epaHeading <-renderUI({
    h4("Choose EPA(s)", style = "color: black;")
  })
  
  output$epaPanel <- renderUI({
    h4("Choose EPA(s)", style = "color: black;")
    tags$div(
      align='left',
      style="color:black;",
      class = "multicol",
      checkboxGroupInput(inputId = "epa",
                         label = NULL,
                         choices = as.list(as.vector(unique(distdat()$EPA[distdat()$EPA !=""]))) %>%
                           setNames(.,.)%>%
                           .[order(names(.))],
                         inline = FALSE,
                         width = "4em",
                         selected = unique(distdat()$EPA[distdat()$EPA !=""])
      )
    )
  })
  
  epadat<-reactive({
    if (is.null(input$epa) | input$epaSwitch == F){
      distdat()
    } else {
      subset(distdat(), EPA %in% input$epa )
    }
  })
  
  #-------------------------------------------------------------------------------------------------------#
  #         Sustainability Agriculture Panel                                                              #
  #-------------------------------------------------------------------------------------------------------#
  
  rvsust <- reactiveValues()
  
  makeSustInputs = function(n=1){
    tags$div(
      align = 'left',
      style = "color:black;",
      column(8,
             materialSwitch(inputId = paste0(names(sust_ag)[[n]],".On"),
                            label = sust_ag[[n]], 
                            status = "primary",
                            right = TRUE)
      ),
      column(1,
             hidden(prettyToggle(inputId = names(sust_ag)[[n]],
                            label_on = "Present",
                            label_off = "Absent",
                            icon_on = icon("check"),
                            icon_off = icon("remove"),
                            status_on = "primary",
                            status_off = "warning",
                            shape = "curve",
                            value = TRUE))
      )
    )
  }
  
  observe({
    for (n in 1:length(sust_ag)) {
      if (! is.null(input[[paste0(names(sust_ag)[[n]],".On")]])){
        if (input[[paste0(names(sust_ag)[[n]],".On")]] == TRUE) {
          show(id = paste0(names(sust_ag)[[n]]))
        } else {hide(id = paste0(names(sust_ag)[[n]]))}
      }
      if (! is.null(input[[paste0(names(sust_ag)[[n]],".On")]]) ){
        if ( input[[paste0(names(sust_ag)[[n]],".On")]] ) {
          if (! is.null(input[[paste0(names(sust_ag)[[n]])]])) {
            if( input[[paste0(names(sust_ag)[[n]])]] == TRUE ){
              rvsust[[paste0(names(sust_ag)[[n]])]] <- paste0(names(sust_ag)[[n]]," == 1")
            } else {
              rvsust[[paste0(names(sust_ag)[[n]])]] <- paste0(names(sust_ag)[[n]]," == 0")}
          } else {rvsust[[paste0(names(sust_ag)[[n]])]] <- NULL}
        } else {rvsust[[paste0(names(sust_ag)[[n]])]] <- NULL}
      } else {rvsust[[paste0(names(sust_ag)[[n]])]] <- NULL}
    }  
  })
  
  sustVector=reactive({lapply(X = 1:length(sust_ag), FUN = makeSustInputs)})
  
  output$sust_inputs <- renderUI({
    tagList(sustVector())})

  sustdat <- reactive({
    sustlist1 <- reactiveValuesToList(rvsust)
    if (TRUE %in% sapply(sustlist1, is.null)) {
      sustlist2 = sustlist1[-which(sapply(sustlist1, is.null))]
    } else {sustlist2 = sustlist1}
    if (input$sust_log == 3) {
      subset(epadat(), Sust_Ag_Activities == "No")
    } else if (length(sustlist2)!=0){
      if (input$sust_log == 2) {
        subset(epadat(), eval(parse(text=paste(sustlist2, collapse = input$sust_andor))))
      } else if (input$sust_log == 1) {
        subset(epadat(), (eval(parse(text=paste(sustlist2, collapse = input$sust_andor)))) | Sust_Ag_Activities == "No")}
    } else if (input$sust_log == 2) {
      subset(epadat(), Sust_Ag_Activities == "Yes")
    } else {epadat()}
  })
  
  output$sustbar <- renderPlotly ({
    north <- subset(FarmSurvey, Lat != 0 & Region == "Northern")
    central <- subset(FarmSurvey, Lat != 0 & Region == "Central")
    northvals = numeric(0)
    centvals = numeric(0)
    for (n in 1:length(sust_ag)) {
      northvals[[n]] <- sum(north[paste0(names(sust_ag)[[n]])], na.rm=T)
      centvals[[n]] <- sum(central[paste0(names(sust_ag)[[n]])], na.rm=T)
    }
    
    data <- data.frame(sust_ag, northvals, centvals)
    
    plot_ly(data, x = ~northvals, y = ~names(sust_ag), type = 'bar', orientation = 'h', name = 'Northern',
            marker = list(color = 'rgba(246, 78, 139, 0.6)',
                          line = list(color = 'rgba(246, 78, 139, 1.0)',
                                      width = 3))) %>%
      add_trace(x = ~centvals, name = 'Central',
                marker = list(color = 'rgba(58, 71, 80, 0.6)',
                              line = list(color = 'rgba(58, 71, 80, 1.0)',
                                          width = 3))) %>%
      layout(barmode = 'stack',
             xaxis = list(title = "Number of Farms"),
             yaxis = list(title ="",
                          categoryorder = "array",
                          categoryarray = ~rev(names(sust_ag))),
             showlegend=FALSE,
             hovermode = 'compare',
             autosize = F, 
             width = 600, 
             height = 400, 
             margin = list(l = 5,
                           r = 20,
                           b = 10,
                           t = 50,
                           pad = 4)
      )
    
  })
  
  #---------------------------------------------------------------------------------------------------------------------#
  #         Crops Panel                                                                                                 #
  #---------------------------------------------------------------------------------------------------------------------#
  
  output$crop_ext_Veg_chk <- renderUI ({     
    if (input$Vegetables.On == TRUE) {
      prettyToggle(
        inputId = "Vegetables",
        label_on = "Present", 
        icon_on = icon("check"),
        status_on = "info",
        status_off = "default", 
        label_off = "Absent",
        icon_off = icon("remove"),
        value = TRUE
      )
    } 
  })
  output$crop_ext_Fruit_chk <- renderUI ({     
    if (input$Fruit_trees.On == TRUE) {
      prettyToggle(
        inputId = "Fruit_trees",
        label_on = "Present", 
        icon_on = icon("check"),
        status_on = "info",
        status_off = "default", 
        label_off = "Absent",
        icon_off = icon("remove"),
        value = TRUE
      )
    } 
  })
  
  rvcrop <- reactiveValues()
  
  makeCropInputs = function(n=1){
    tags$div(
      align = 'left',
      style = "color:black;",
      column(7,
             materialSwitch(inputId = paste0(names(cropnames)[[n]],".On"),
                          label = cropnames[[n]], 
                          status = "primary",
                          right = TRUE)
      ),
      column(5,
             hidden(prettyToggle(inputId = names(cropnames)[[n]],
                                 label_on = "Present",
                                 label_off = "Absent",
                                 icon_on = icon("check"),
                                 icon_off = icon("remove"),
                                 status_on = "primary",
                                 status_off = "warning",
                                 shape = "curve",
                                 value = TRUE))
      )
    )
  }
  
  observe({
    for (n in 1:length(cropnames)) {
      if (! is.null(input[[paste0(names(cropnames)[[n]],".On")]])){
        if (input[[paste0(names(cropnames)[[n]],".On")]] == TRUE) {
          show(id = paste0(names(cropnames)[[n]]))
        } else {hide(id = paste0(names(cropnames)[[n]]))}
      }
      if (! is.null(input[[paste0(names(cropnames)[[n]],".On")]]) ){
        if ( input[[paste0(names(cropnames)[[n]],".On")]] ) {
          if (! is.null(input[[paste0(names(cropnames)[[n]])]])) {
            if( input[[paste0(names(cropnames)[[n]])]] == TRUE ){
              rvcrop[[paste0(names(cropnames)[[n]])]] <- paste0(names(cropnames)[[n]]," == 1")
            } else {
              rvcrop[[paste0(names(cropnames)[[n]])]] <- paste0(names(cropnames)[[n]]," == 0")}
          } else {rvcrop[[paste0(names(cropnames)[[n]])]] <- NULL}
        } else {rvcrop[[paste0(names(cropnames)[[n]])]] <- NULL}
      } else {rvcrop[[paste0(names(cropnames)[[n]])]] <- NULL}
    }  
  })
    
  cropVector=reactive({lapply(X = 1:length(cropnames), FUN = makeCropInputs)})
  
  output$crop_inputs <- renderUI({
    tagList(cropVector())})
  
  observe({
    if ( input$Vegetables.On ) {
      if (! is.null(input$Vegetables)) {
        if( input$Vegetables == TRUE ){
          rvcrop$Vegetables <- "Vegetables == 'Yes'"
        } else {
          rvcrop$Vegetables <- "Vegetables == 'No'"}
      } else {rvcrop$Vegetables <- NULL}
    } else {rvcrop$Vegetables <- NULL}
    if ( input$Fruit_trees.On ) {
      if (! is.null(input$Fruit_trees)) {
        if( input$Fruit_trees == TRUE ){
          rvcrop$Fruit_trees <- "Fruit_trees == 'Yes'"
        } else {
          rvcrop$Fruit_trees <- "Fruit_trees == 'No'"}
      } else {rvcrop$Fruit_trees <- NULL}
    } else {rvcrop$Fruit_trees <- NULL}
  })
  
  cropdat <- reactive({
    croplist1 <-reactiveValuesToList(rvcrop)
    if (TRUE %in% sapply(croplist1, is.null)) {
      croplist2 = croplist1[-which(sapply(croplist1, is.null))]
    } else {croplist2 = croplist1}
    if (length(croplist2) != 0) {
      subset(sustdat(), eval(parse(text=paste(croplist2, collapse = input$crop_andor))))
    } else { 
      sustdat() 
    }
  })
  
  output$cropsbar <- renderPlotly ({
    north <- subset(FarmSurvey, Lat != 0 & Region == "Northern")
    central <- subset(FarmSurvey, Lat != 0 & Region == "Central")
    northvals = numeric(0)
    centvals = numeric(0)
    for (n in 1:length(cropnames)) {
      northvals[[n]] <- sum(north[names(cropnames)[[n]]], na.rm=T)
      centvals[[n]] <- sum(central[names(cropnames)[[n]]], na.rm=T)
    }
    
    data <- data.frame(cropnames, northvals, centvals)
    
    plot_ly(data, x = ~northvals, y = ~names(cropnames), type = 'bar', orientation = 'h', name = 'Northern',
            marker = list(color = 'rgba(246, 78, 139, 0.6)',
                          line = list(color = 'rgba(246, 78, 139, 1.0)',
                                      width = 3))) %>%
      add_trace(x = ~centvals, name = 'Central',
                marker = list(color = 'rgba(58, 71, 80, 0.6)',
                              line = list(color = 'rgba(58, 71, 80, 1.0)',
                                          width = 3))) %>%
      layout(barmode = 'stack',
             xaxis = list(title = "Number of Farms"),
             yaxis = list(title ="",
                          categoryorder = "array",
                          categoryarray = ~rev(names(cropnames))),
             showlegend=FALSE,
             hovermode = 'compare',
             autosize = F, 
             width = 600, 
             height = 400, 
             margin = list(l = 5,
                           r = 20,
                           b = 10,
                           t = 50,
                           pad = 4)
             )
    
  })
  
  #----------------------------------------------------------------------------------------------------------#
  #           Livestock Panel                                                                                #
  #----------------------------------------------------------------------------------------------------------#

  rvlivstk <- reactiveValues()
  
  makeLivstkInputs = function(n=1){
    tags$div(
      align = 'left',
      style = "color:black;",
      column(7,
             materialSwitch(inputId = paste0(names(livstk)[[n]],".On"),
                            label = livstk[[n]], 
                            status = "primary",
                            right = TRUE)
      ),
      column(1,
             hidden(prettyToggle(inputId = names(livstk)[[n]],
                                 label_on = "Present",
                                 label_off = "Absent",
                                 icon_on = icon("check"),
                                 icon_off = icon("remove"),
                                 status_on = "primary",
                                 status_off = "warning",
                                 shape = "curve",
                                 value = TRUE))
             
      )
    )
  }
  
  observe({
    for (n in 1:length(livstk)) {
      if (! is.null(input[[paste0(names(livstk)[[n]],".On")]])){
        if (input[[paste0(names(livstk)[[n]],".On")]] == TRUE) {
          show(id = paste0(names(livstk)[[n]]))
        } else {hide(id = paste0(names(livstk)[[n]]))}
      }
      if (! is.null(input[[paste0(names(livstk)[[n]],".On")]]) ){
        if ( input[[paste0(names(livstk)[[n]],".On")]] ) {
          if (! is.null(input[[paste0(names(livstk)[[n]])]])) {
            if( input[[paste0(names(livstk)[[n]])]] == TRUE ){
              rvlivstk[[paste0(names(livstk)[[n]])]] <- paste0(names(livstk)[[n]]," == 1")
            } else {
              rvlivstk[[paste0(names(livstk)[[n]])]] <- paste0(names(livstk)[[n]]," == 0")}
          } else {rvlivstk[[paste0(names(livstk)[[n]])]] <- NULL}
        } else {rvlivstk[[paste0(names(livstk)[[n]])]] <- NULL}
      } else {rvlivstk[[paste0(names(livstk)[[n]])]] <- NULL}
    }  
  })
  
  livstkVector=reactive({lapply(X = 1:length(livstk), FUN = makeLivstkInputs)})
  
  output$livstk_inputs <- renderUI({
    tagList(livstkVector())})
  
  livstkdat <- reactive({
    livstklist1 <- reactiveValuesToList(rvlivstk)
    if (TRUE %in% sapply(livstklist1, is.null)) {
      livstklist2 = livstklist1[-which(sapply(livstklist1, is.null))]
    } else {livstklist2 = livstklist1}
    if (input$livstk_log == 3) {
      subset(cropdat(), Livestock == "No")
    } else if (length(livstklist2)!=0){
      if (input$livstk_log == 2) {
        subset(cropdat(), eval(parse(text=paste(livstklist2, collapse = input$livstk_andor))))
      } else if (input$livstk_log == 1) {
        subset(cropdat(), (eval(parse(text=paste(livstklist2, collapse = input$livstk_andor)))) | Livestock == "No")}
    } else if (input$livstk_log == 2) {
      subset(cropdat(), Livestock == "Yes")
    } else {cropdat()}
  })
  
  output$livstkbar <- renderPlotly ({
    north <- subset(FarmSurvey, Lat != 0 & Region == "Northern")
    central <- subset(FarmSurvey, Lat != 0 & Region == "Central")
    northvals = numeric(0)
    centvals = numeric(0)
    for (n in 1:length(livstk)) {
      northvals[[n]] <- sum(north[paste0(names(livstk)[[n]])], na.rm=T)
      centvals[[n]] <- sum(central[paste0(names(livstk)[[n]])], na.rm=T)
    }
    
    data <- data.frame(livstk, northvals, centvals)
    
    plot_ly(data, x = ~northvals, y = ~names(livstk), type = 'bar', orientation = 'h', name = 'Northern',
            marker = list(color = 'rgba(246, 78, 139, 0.6)',
                          line = list(color = 'rgba(246, 78, 139, 1.0)',
                                      width = 3))) %>%
      add_trace(x = ~centvals, name = 'Central',
                marker = list(color = 'rgba(58, 71, 80, 0.6)',
                              line = list(color = 'rgba(58, 71, 80, 1.0)',
                                          width = 3))) %>%
      layout(barmode = 'stack',
             xaxis = list(title = "Number of Farms"),
             yaxis = list(title ="",
                          categoryorder = "array",
                          categoryarray = ~rev(names(livstk))),
             showlegend=FALSE,
             hovermode = 'compare',
             autosize = F, 
             width = 600, 
             height = 400, 
             margin = list(l = 5,
                           r = 20,
                           b = 10,
                           t = 50,
                           pad = 4)
      )
    
  })  
  
  #---------------------------------------------------------------------------------------------------------#
  #          Respondant Panel                                                                               #
  #---------------------------------------------------------------------------------------------------------#
  
  output$LiteracyPanel <- renderUI({ 
    checkboxGroupInput(inputId = "resp_lit",
                       label = h4("Select Literacy levels"),
                       choices = as.list(with (livstkdat(), union(Literacy, Literacy)))%>%
                         setNames(.,.)%>%
                         .[order(names(.))],
                       inline = FALSE,
                       selected = unique(livstkdat()$Literacy)
    ) 
  })
  
  litdat<-reactive({
    if (! is.null(input$resp_lit)){
      subset(livstkdat(), Literacy %in% input$resp_lit )
    } else { 
    livstkdat() }
  })
  
  output$Followers <- renderUI({
    sliderInput(inputId = "resp_foll",
                "Number of Followers",
                min=min(litdat()$Number_followers, na.rm=T),
                max=max(litdat()$Number_followers, na.rm=T),
                value=c(min(litdat()$Number_followers, na.rm=T),max(litdat()$Number_followers, na.rm=T)))
  })
  
  foldat <- reactive({
    if (! is.null(input$resp_foll)){
      subset(litdat(), Number_followers >= input$resp_foll[[1]] &  Number_followers <= input$resp_foll[[2]])
    } else { 
      litdat() }
  })
  
  output$FarmSize <- renderUI({
    sliderInput(inputId = "resp_fsize",
                "Farm Size",
                min=min(foldat()$Farm_size, na.rm=T),
                max=max(foldat()$Farm_size, na.rm=T),
                value=c(min(foldat()$Farm_size, na.rm=T),max(foldat()$Farm_size, na.rm=T)))
  })
  
  fsizedat <- reactive({
    if (! is.null(input$resp_foll)){
      subset(foldat(), Farm_size >= input$resp_fsize[[1]] &  Farm_size <= input$resp_fsize[[2]])
    } else { 
      foldat() }
  })
  
  output$Training <- renderUI ({
    sliderInput("trnrng",
               "Training Date Range:",
                min=min(ymd(fsizedat()$Date_trained), na.rm=T),
                max=max(ymd(fsizedat()$Date_trained), na.rm=T),
                value=c(date("2012-06-01"), as_date(now())))
  
  })
  
  trndat <- reactive({
    if ( ! is.null(input$trnrng)){
      subset(fsizedat(), date(Date_trained) >= input$trnrng[[1]] & date(Date_trained) <= input$trnrng[[2]])
    } else {fsizedat()}
  })
  
  #---------------------------------------------------------------------------------------------------------#
  #          Mapping Panel                                                                                  #
  #---------------------------------------------------------------------------------------------------------#
  
  myData<-reactive({trndat()})
  
  output$myMap <- renderLeaflet({
    
    leaflet() %>% 
      addProviderTiles(providers$CartoDB.Positron, group = "Default Maptile") %>% 
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite Maptile") %>%
      setView(34.3015,-13.2512,zoom=7) %>% 
      addLayersControl(
        baseGroups = c("Default Maptile", "Satellite Maptile"),
        options = layersControlOptions(collapsed = FALSE)) %>%
      addPolygons(
        data = borders,
        stroke = TRUE, fillOpacity = 0, smoothFactor = 0,
        color = "black", opacity = 1, weight = 1)
  })
  
  observe({
    
    leafletProxy("myMap", data = myData()) %>%
      clearMarkerClusters() %>% 
      clearHeatmap() %>%
      addHeatmap(lng = ~Lon, lat = ~Lat, group = "HeatMap", blur = 15, max = 0.01, radius = 15) %>%
      addMarkers(data = myData(), ~Lon, ~Lat, clusterOptions = markerClusterOptions(), 
                 group = "Points") %>% 
      addLayersControl(baseGroups = c("Default Maptile", "Satellite Maptile"),
                       overlayGroups = c("HeatMap", "Points"),
                       options = layersControlOptions(collapsed = FALSE))
    
  })
  
  #--------------------------------------------------------------------------------------------------------#
  #         Plotting Panel                                                                                 #
  #--------------------------------------------------------------------------------------------------------#
  
  output$trnHist <- renderPlot({
    validate(
      need((nrow(myData()) >= 1), "No data in dataset. Subsetting too restrictive")
    )
    x <- ymd(myData()$Date_trained)
    # generate bins based on input$bins and range in input$trnrng from ui.R
    if (! is.null(input$trnrng) ) {

      min_bin = input$trnrng[[1]]
      max_bin = input$trnrng[[2]]
      binnum = input$bins + 1
    } else {
      min_bin = min(ymd(myData()$Date_trained), na.rm=T)
      max_bin = max(ymd(myData()$Date_trained), na.rm=T)
      binnum = 31
    }
    bins <- seq(min_bin, max_bin, length.out = binnum)
    
    # draw the histogram with the specified number of bins and within specified range
    hist(x[x >= min_bin & x <= max_bin], 
         breaks = unique(bins), 
         col = 'darkgray',
         main = NULL,
         xlab = "Date")
  })
  
  output$trnSum <- renderTable({
    x <- as.array(summary(date(myData()$Date_trained)))
    format(x, '%Y-%m-%d')
  }, colnames = FALSE, rownames = TRUE)
  
  output$farmHist <- renderPlot({
    validate(
      need((nrow(myData()) >= 1), "No data in dataset. Subsetting too restrictive")
    )
    x <- myData()$Farm_size
    # generate bins based on input$bins and range in input$trnrng from ui.R
    if (! is.null(input$resp_fsize) ) {
      min_bin = input$resp_fsize[[1]]
      max_bin = input$resp_fsize[[2]]
      binnum = input$bins + 1
    } else {
      min_bin = min(myData()$Farm_size, na.rm=T)
      max_bin = max(myData()$Farm_size, na.rm=T)
      binnum = 31
    }
    bins <- seq(min_bin, max_bin, length.out = binnum)

    # draw the histogram with the specified number of bins and within specified range
    hist(x[x >= min_bin & x <= max_bin], 
         breaks = unique(bins), 
         col = 'darkgray',
         main = NULL,
         xlab = "Farm size")
  })
  
  output$farmSum <- renderTable({
    x <- as.array(summary(myData()$Farm_size))
  }, colnames = FALSE, rownames = TRUE)
  
  
  #-----------------------------------------------------------------------------------------------------#
  #         Table Panel                                                                                 #
  #-----------------------------------------------------------------------------------------------------#
  
  output$myDataTable <- renderDataTable({myData()}, 
                                        options = list(pageLength = 5,
                                                       lengthMenu = c(5, 10, 15, 20)
                                                       )
                                        )
  
  output$downloadData <- downloadHandler(filename = function() {"FarmSurvey_Subset.csv"},
                                         content = function(file) {write.csv(myData(), 
                                                                             file, 
                                                                             row.names = FALSE)
                                                                  }
                                         )
  
  #-------------------------------------------------------------------------------------------------------#
  #         Inter-Panel Control                                                                           #
  #-------------------------------------------------------------------------------------------------------#
  
  observeEvent(input$colps_side, ({
    if (input$colps_side == "trnCollapse") {
      updateCollapse(session, "colps_main", open="plotCollapse")
    }
  }))
  
  observeEvent(input$colps_side, ({
    if (input$colps_side != "trnCollapse") {
      updateCollapse(session, "colps_main", open="mapCollapse")
    }
  }))
  
})
