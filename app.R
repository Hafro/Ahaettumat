# alt + o : Code folded


# Libraries ---------------------------------
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(DT)
library(leaflet)
library(RColorBrewer)
library(ggplot2)
library(XLConnect)
library(reshape)
library(aws.s3)
load("inputs.RData")
riverdata <<- riverdata

inputs <<- inputs



# Choices ---------------------------------
# Enumerated list of the names of farm sites along with Total as nr. 1
Choices = setNames(c(1:(length(rownames(farmsites))+1)),c('Total',farmsites$SiteName))


# header ---------------------------------
header <- dashboardHeaderPlus(
  enable_rightsidebar = TRUE,
  rightSidebarIcon = "fish",
  title = "Risk assessment"
)

# sidebar ---------------------------------
sidebar <- dashboardSidebar(sidebarMenu(
  ## sidebar Menu Map ---------------------------------
  menuItem("Map", tabName = "Map", icon = icon("map-marked")),
  ## sidebar Menu Plot ---------------------------------
  menuItem("Plot", tabName = "Plot", icon = icon("chart-bar")),
  ## sidebar Menu Table ---------------------------------
  menuItem("Table", tabName = "Table", icon = icon("table")),
  ## sidebar Menu Weibull ---------------------------------
  menuItem("Weibull", tabName = "Weibull", icon = icon("chart-line")),
  ## sidebar Menu Parameters ---------------------------------
  menuItem("Parameters", tabName = "Parameters", icon = icon("sliders-h"))
))

# body ---------------------------------
body <- dashboardBody(  
  tags$head(
  tags$style(
    "body {overflow-y: hidden;}"
    )
  ),
  tabItems(
  ## body Map tab ---------------------------------
  tabItem(tabName = "Map",
          h2("Map"),
          leafletOutput("mymap")),
  ## body Plot tab ---------------------------------
  tabItem(
    tabName = "Plot",
    h2("Plot"),
    plotOutput("myPlot1"),
    selectInput(
      "Graph",
      label = "Pick a farm site",
      choices = Choices,
      selected = 1
    ),
    fluidRow(
      column(4,radioButtons(
        "radio",
        label = "",
        choices = list("Amount" = 1, "Percentage" = 2),
        selected = 2
      )),
      column(4,radioButtons(
        "all",
        label = "",
        choices = list("Show only affected rivers" = 1, "Show all rivers" = 2),
        selected = 1
      ))
    )
  ),
  ## body Table tab ---------------------------------
  tabItem(
    tabName = "Table",
    h2("Table"),
    DT::dataTableOutput("table")
  ),
  ## body Weibull tab ---------------------------------
  tabItem(
    tabName = "Weibull",
    h2("Weibull Plot"),
    plotOutput("myPlot2"),
    selectInput(
      "Graph2",
      label = "Pick a farm site",
      choices = Choices,
      selected = 1
    )
  ),
  ## body Parameter tab ---------------------------------
  tabItem(
    tabName = "Parameters",
    fluidRow(
      column(
        3,
        h4(""),
        helpText(
          "Here you can change the values of the parameters which affect the model. "
        )
      ),
      column(
        3,
        h4("Weibull coefficients for early escapees:"),
        sliderInput(
          "beta",
          label = "beta",
          0,
          min = 1,
          max = 10,
          step = 0.1,
          value = inputs$beta
        ),
        sliderInput(
          "eta",
          label = "eta",
          0,
          min = 100,
          max = 1000,
          step = 10,
          value = inputs$eta
        )
      ),
      
      column(
        3,
        h4("Proportion of early escapees that return to rivers:"),
        sliderInput(
          "surv",
          label = NULL,
          0,
          min = 0,
          max = 0.05,
          step = 0.0005,
          value = inputs$surv
        )
      ),
      
      column(
        3,
        h4("Amount of escapees per ton of maximum biomass:"),
        sliderInput(
          "escS",
          label = "Early escapees:",
          0,
          min = 0,
          max = 3,
          step = 0.01,
          value = inputs$escS
        ),
        sliderInput(
          "esc",
          label = "Late escapees:",
          0,
          min = 0,
          max = 3,
          step = 0.01,
          value = inputs$esc
        )
      )
      
      
    ),
    
    fluidRow(
      column(
        3,
        h4("Homing parameter:"),
        sliderInput(
          "home",
          label = "Early escapees:",
          0,
          min = 0,
          max = 5,
          step = 0.05,
          value = inputs$home
        ),
        sliderInput(
          "home2",
          label = "Late escapees:",
          0,
          min = 0,
          max = 5,
          step = 0.05,
          value = inputs$home2
        )
      ),
      
      column(
        3,
        h4("Weibull coefficients for late escapees:"),
        sliderInput(
          "beta2",
          label = "beta",
          0,
          min = 1,
          max = 10,
          step = 0.1,
          value = inputs$beta2
        ),
        sliderInput(
          "eta2",
          label = "eta",
          0,
          min = 100,
          max = 1000,
          step = 10,
          value = inputs$eta2
        )
      ),
      
      column(
        3,
        h4(
          "Proportion of late escapees that sexually mature and go up into rivers:"
        ),
        sliderInput(
          "sex",
          label = NULL,
          0,
          min = 0,
          max = 1,
          step = 0.01,
          value = inputs$sex
        )
      ),
      
      column(
        3,
        h4("Length of rearing period at sea for a farmed salmon:"),
        sliderInput(
          "cTime",
          label = NULL,
          0,
          min = 10,
          max = 20,
          step = 0.5,
          value = inputs$cTime
        ),
        h4("Dangerperiod in months:"),
        sliderInput(
          "critPer",
          label = NULL,
          0,
          min = 2,
          max = 12,
          step = 0.5,
          value = inputs$critPer
        )
      )
    )
  )
),
tags$head(
  tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
))

# right sidebar ---------------------------------
rightsidebar = rightSidebar(
  ## rightsidebar Production ---------------------------------
  rightSidebarTabContent(
    id = 1,
    active = TRUE,
    title = "Maximum biomass of farmed salmon in thousands of tons in: ",
    icon = "circle",
    uiOutput("selectionsTons")
  )
)

# UI ---------------------------------
ui <- dashboardPagePlus(title = 'Risk assessment',
                        header,
                        sidebar,
                        body,
                        rightsidebar,
                        skin = 'blue'
)



# Server ---------------------------------
server <- function(input, output) {
  ## constants ---------------------------------
  
  amountOfRivers <- nrow(riverdata)
  amountOfFarmed <- nrow(farmsites)
  
  defZoom = 5
  mybins=c(0,4,10,100)
  mypalette = colorBin( palette=c("red3", "gold", "darkgreen"), domain = c(0:100), na.color="transparent", bins=mybins,reverse = T)
  
  ## Output UI controls ---------------------------------
  output$selectionsTons <- renderUI({
    tagList(
      lapply(c(1:length(rownames(farmsites))),
             function(x)
             {
               h5(farmsites[x,1])
               sliderInput(
                 paste("tonn",x,sep=""),
                 label = farmsites[x,1],
                 0,
                 min = 0,
                 max = farmsites[x,6],
                 step = 0.5,
                 value = eval(parse(text=paste('inputs$tonn',x,sep="")))
               )
             }
      )
    )
  })

  
  ## dataZoom ---------------------------------
  dataZoom <-
    reactive({
      # Takes in the zoom level to adjust marker size
      zoom <- input$mymap_zoom
      if (is.null(zoom)) {
        zoom = defZoom
      }
      zoom
    })
  
  ## dataNetpenAdult ---------------------------------
  # Function that puts the data into the Distribution function and gathers the output
  dataNetpenAdult <- reactive({
    NetpenAdult <- data.frame(
      lapply(rownames(farmsites),
             function(x)
             {
               amount = eval(parse(text=paste('input$tonn',x,sep="")))
               if (is.null(amount)){
                 amount=0
               }
               amount = amount*1000 
               Distribution(
                 farmsites[x, "position"],
                 input$beta2,
                 input$eta2,
                 amount,
                 input$esc,
                 input$sex,
                 input$cTime,
                 input$critPer,
                 input$home2
               )
             }
      )
    )
    NetpenAdult
  })
  ## dataNetpenSmolt ---------------------------------
  dataNetpenSmolt <- reactive({
    NetpenSmolt <- data.frame(
      lapply(rownames(farmsites),
             function(x)
             {
               amount = eval(parse(text=paste('input$tonn',x,sep="")))
               if (is.null(amount)){
                 amount=0
               }
               amount = amount*1000
               smoltDistribution(
                 farmsites[x, "position"],
                 input$beta,
                 input$eta,
                 amount,
                 input$escS,
                 input$surv,
                 input$home
                 #if (x == 12) {
                #   4.9
                 #} else {
                  # input$home
                 #}
               )
             }
      )
    )
    NetpenSmolt
  })
  ## dataNetpenTotal ---------------------------------
  dataNetpenTotal <- reactive({
    NetpenTotal <- round(dataNetpenAdult() + dataNetpenSmolt())
    NetpenTotal
  })
  
  ## dataGraph ---------------------------------
  # Takes the input which chooses what farmsite to show
  dataGraph <- reactive({
    Graph <- input$Graph
    if (Graph == 1) {
      Graph = amountOfFarmed+2
    }
    Graph
  })
  
  ## dataGraph2 ---------------------------------
  dataGraph2 <- reactive({
    Graph2 <- input$Graph2
    if (Graph2 == 1) {
      Graph2 = amountOfFarmed+2
    }
    Graph2
  })
  
  
  ## dataNetpen ---------------------------------
  #Function that gathers all the data per river
  dataNetpen <- reactive({
    Netpen <- data.frame(riverdata$Stock.Size,
                      dataNetpenTotal(),
                      row.names =  as.character(riverdata$RiverName))

    Netpen$"Farmed salmons" <- rowSums(Netpen[, 2:(amountOfFarmed+1)])
    colnames(Netpen) <-
      c(
        "Wild salmons",
        lapply(farmsites$SiteName,function(x) paste('Farmed salmons from',x)),
        "Farmed salmons"
      )
    Netpen$"Percentage of total" = round(Netpen$"Farmed salmons" / (Netpen$"Wild salmons"+Netpen$"Farmed salmons"),
                                   digits = 4) * 100
    Total <- colSums(Netpen)

    Total[amountOfFarmed+3] = round(sum(Netpen$"Farmed salmons") / (
      sum(Netpen$"Farmed salmons") + sum(Netpen$"Wild salmons")
    ), digits = 4)*100
    Netpen <- rbind(Netpen, Total)
    row.names(Netpen) = c(as.character(riverdata$RiverName), "Total")
    Netpen
  })
  
  ## dataNetpenAmount ---------------------------------
  
  # Function that takes in the amount per farmsite
  dataNetpenAmount <- reactive({
    NetpenAmount <- c(lapply(
      rownames(farmsites),
      function(x){
        if(is.null(eval(parse(text = paste('input$tonn',x,sep = ""))))){
          0
        } else{ 
          eval(parse(text = paste('input$tonn',x,sep = "")))
        }
      }
    ))
    NetpenAmount
  })
  
  
  ## Output map ---------------------------------
  # The map set up
  output$mymap <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 1, maxZoom = 15)) %>%
      addProviderTiles(providers$Hydda.Base) %>%
      setView(-18.830566,64.960766, zoom = 6) %>%
      addLegend( pal=mypalette, values=c(0,4,10,100), opacity=0.9, title = "Percent", position = "bottomright" )
    
    
  })
  ## Output map rivers ---------------------------------
  # Rivers draw on the map
  observe({
    j <- 1
    leafletProxy("mymap") %>%
      clearGroup("river")
    for (i in riverdata$SizeCategory ) {
      if (i != 0) {
        leafletProxy("mymap") %>%
          addCircleMarkers(
            lng = riverdata[j, 2],
            lat = riverdata[j, 3],
            fillColor = mypalette(dataNetpen()$'Percentage of total'[j]),
            fillOpacity = 1,
            stroke = TRUE,
            color = 'black',
            weight = 1,
            opacity = 1,
            radius = as.numeric(i)/2,
            group = "river",
            label = paste("River: ", riverdata$RiverName[j], "<br/>", "Stock size: ", round(dataNetpen()$'Wild salmons'[j],digits = 2), "<br/>", "Farmed salmons: ", round(dataNetpen()$'Farmed salmons'[j],digits = 2), sep="") %>%
              lapply(htmltools::HTML),
            labelOptions = labelOptions( style = list("font-weight" = "normal",  padding = "3px 8px"), textsize = "13px", direction = "auto")
          ) 
      }
      j <- j + 1
    }
  })
  
  ## Output map circles ---------------------------------
  # Draws the blue farmsite circles
  observe({
    j <- 1
    leafletProxy("mymap") %>%
      clearGroup("farmsites")
    for (i in dataNetpenAmount()) {
      if (i != 0) {
        leafletProxy("mymap") %>%
          addCircleMarkers(
            lng = farmsites[j, 3],
            lat = farmsites[j, 4],
            stroke = TRUE,
            fillColor = 'blue',
            fillOpacity = 1,
            color = 'black',
            weight = 1,
            radius = sqrt(i)*2,
            group = "farmsites",
            label = farmsites[j,1],
            popup = paste(as.character(i), "thousand tons", sep = " ")
          )
      }
      j <- j + 1
    }
  })
  
  ## Output plot ---------------------------------
  # Plots the bar plot filtering out uninfluenced rivers
  output$myPlot1 <- renderPlot({
    yCoord <- dataNetpen()[1:amountOfRivers, as.integer(dataGraph())]
    yCoord <- yCoord[order(riverdata$"position")]
    xNames <- riverdata$RiverName[order(riverdata$"position")]
    xNames <- factor(xNames, levels = unique(xNames))
    if (input$all == 1) {
      influence <- which(yCoord != 0, arr.ind = T)
    }
    else {
      influence <- which(yCoord > -1, arr.ind = T)
    }
    if (length(influence) == 0) {
      influence = 0
    }
    if (input$radio == 1) {
      ggplot(data = dataNetpen()[influence, ], aes(x = xNames[influence], y = yCoord[influence])) + geom_bar(stat =
                                                                                              "identity") + xlab("Salmon river") + ylab("Amount of farmed salmons in river") + theme(axis.text.x = element_text(angle = 60, hjust = 1,size = rel(1.2)))
    }
    else{
      amountOfWild <- dataNetpen()[order(riverdata$"position"), 1]
      prosentur <- yCoord / (amountOfWild + yCoord) * 100
      ggplot(data = dataNetpen()[influence, ], aes(x = xNames[influence], y = prosentur[influence])) + geom_bar(stat =
                                                                                                  "identity", aes(fill = ifelse(prosentur[influence] < 4, "darkgreen", ifelse(prosentur[influence] < 10, "gold", "red3")))) + xlab("Salmon river") + ylab("Percentage of farmed salmons in river") + theme(axis.text.x = element_text(angle = 60, hjust = 1,size = rel(1.2))) + geom_hline(yintercept =  4, color = "gold") + geom_hline(yintercept =  10, color = "red3") +scale_fill_manual(values = c('darkgreen', 'gold','red3'))  + theme(legend.position =
                                                                                                                                                                                                                                                                                                                                                                                              "none")
    }
  })
  
  ## Output Weibull ---------------------------------
  # Plots the weibull plots using the weib and weibL functions
  output$myPlot2 <- renderPlot({
    if (as.integer(dataGraph2()) == amountOfFarmed+2) {
      weibbS <- rep.int(0, amountOfRivers)
      weibbB <- rep.int(0, amountOfRivers)
      weibbSL <- rep.int(0, 2431)
      weibbBL <- rep.int(0, 2431)
      nums <- data.frame(c(-1215:1215))
      colnames(nums) = c('nums')
      for (i in c(1:amountOfFarmed)) {
        weibbS = weibbS + weib(farmsites[i, 2], input$beta, input$eta)
        weibbB = weibbB + weib(farmsites[i, 2], input$beta2, input$eta2)
        weibbSL = weibbSL + weibL(farmsites[i, 2], input$beta, input$eta)
        weibbBL = weibbBL + weibL(farmsites[i, 2], input$beta2, input$eta2)
      }
      ggplot() + geom_point(data = riverdata,
                            aes(
                              x = position,
                              y = weibbS,
                              colour = "Early escapees"
                            )) +
        geom_line(data = nums, aes(
          x = nums,
          y = weibbSL,
          colour = "Early escapees"
        )) +
        geom_point(data = riverdata,
                   aes(
                     x = position,
                     y = weibbB,
                     colour = "Late escapees"
                   )) +
        geom_line(data = nums, aes(
          x = nums,
          y = weibbBL,
          colour = "Late escapees"
        )) +
        geom_point(data = riverdata,
                   aes(
                     x = position,
                     y = weibbB + weibbS,
                     colour = "Total escapees"
                   )) +
        geom_line(data = nums,
                  aes(
                    x = nums,
                    y = weibbBL + weibbSL,
                    colour = "Total escapees"
                  )) +
        xlab("River") + ylab("Percentage") + scale_x_discrete(breaks =
                                                                NULL)
    } else{
      position <- farmsites[as.integer(dataGraph2()) - 1, 2]
      nums <- data.frame(c(-1215:1215))
      colnames(nums) = c('nums')
      ggplot() + geom_point(data = riverdata,
                            aes(
                              x = position,
                              y = weib(position, input$beta, input$eta),
                              colour = "Early escapees"
                            )) +
        geom_line(data = nums, aes(
          x = nums,
          y = weibL(position, input$beta, input$eta),
          colour = "Early escapees"
        )) +
        geom_point(data = riverdata, aes(
          x = position,
          y = weib(position, input$beta2, input$eta2),
          colour = "Late escapees"
        )) +
        geom_line(data = nums, aes(
          x = nums,
          y = weibL(position, input$beta2, input$eta2),
          colour = "Late escapees"
        )) +
        geom_point(data = riverdata,
                   aes(
                     x = position,
                     y = weib(position, input$beta2, input$eta2) + weib(position, input$beta, input$eta),
                     colour = "Total escapees"
                   )) +
        geom_line(data = nums,
                  aes(
                    x = nums,
                    y = weibL(position, input$beta2, input$eta2) + weibL(position, input$beta, input$eta),
                    colour = "Total escapees"
                  )) +
        xlab("River") + ylab("Percentage") + scale_x_discrete(breaks =
                                                                NULL)
    }
  })
  
  ## Output table ---------------------------------
  # Creates the table
  output$table <- DT::renderDataTable({
    yCoord <- dataNetpen()[1:(amountOfRivers+1), "Farmed salmons"]
    xCoord <- dataNetpen()[(amountOfRivers+1), ]
    influenceX <- which(xCoord != 0, arr.ind = T)[, 2]
    if (sum(dataNetpen()[1:amountOfRivers, "Farmed salmons"]) != 0) {
      influenceY <- which(yCoord != 0, arr.ind = T)
      df <- dataNetpen()[influenceY, influenceX]
    } else{
      influenceY <- c(1:amountOfRivers)
      df  = data.frame(dataNetpen()$"Wild salmons"[1:amountOfRivers], row.names = riverdata$RiverName)
      colnames(df) = "Wild salmons"
    }
    datatable(df,
              rownames = TRUE,
              filter = "none",
              width = "100%",
              options = list(scrollX = TRUE))%>%
              formatRound(TRUE, 1)
  })
  
 
  ## Distribution function ---------------------------------
  # Function that calculates the distribution of adult fish based on position of farmsite arg1
  Distribution <- function(arg1, beta, eta, annualProd, escapesPerTon, sexMat, seatime, crit, home){
    
    
    nums <- c(1:4200)
    maximum <- which.max((beta/eta)*((nums/eta)^(beta-1))*exp(-1*((nums/eta)^beta)))
    
    distance <- riverdata$position-arg1
    for(i in c(1:amountOfRivers)){
      if (distance[i] < -1215){
        distance[i] = 2430 + distance[i]
      }
      else if (distance[i] > 1215){
        distance[i] = - 2430 + distance[i]
      }
    }
    amendedDistance <- distance  + maximum
    amendedDistance[amountOfRivers+1] = maximum
    
    weibull <- ifelse(amendedDistance > 0, (beta/eta)*((amendedDistance/eta)^(beta-1))*exp(-1*((amendedDistance/eta)^beta)),0)
    totalProbability <- sum(weibull)
    amountInRiver <- rep(0,amountOfRivers+1)
    for(i in c(1:amountOfRivers)){
      amountInRiver[i] = riverdata[i,"Stock.Size"]
    }
    amountInRiver[amountOfRivers+1] = annualProd*home
    weibullxStock <- weibull*amountInRiver/totalProbability
    
    weibullxStockNormalized <- weibullxStock/sum(weibullxStock)
    
    FarmedFishInRiver <- annualProd*escapesPerTon*weibullxStockNormalized*sexMat*crit/seatime
    
    
    return(FarmedFishInRiver[1:amountOfRivers])
  }
  
  ## smoltDistribution function ---------------------------------
  # Function that calculates the distribution of smolts based on position of farmsite arg1
  smoltDistribution <- function(arg1, beta, eta, annualProd, escapesPerTon,survival,home){
    
    nums <- c(1:4200)
    maximum <- which.max((beta/eta)*((nums/eta)^(beta-1))*exp(-1*((nums/eta)^beta)))
    
    distance <- riverdata$position-arg1
    for(i in c(1:amountOfRivers)){
      if (distance[i] < -1215){
        distance[i] = 2430 + distance[i]
      }
      else if (distance[i] > 1215){
        distance[i] = - 2430 + distance[i]
      }
    }
    amendedDistance <- distance  + maximum
    amendedDistance[amountOfRivers+1] = maximum
    
    weibull <- ifelse(amendedDistance > 0, (beta/eta)*((amendedDistance/eta)^(beta-1))*exp(-1*((amendedDistance/eta)^beta)),0)
    
    totalProbability <- sum(weibull)
    
    amountInRiver <- rep(0,amountOfRivers+1)
    for(i in c(1:amountOfRivers)){
      amountInRiver[i] = riverdata[i,"Stock.Size"]
    }
    amountInRiver[amountOfRivers+1] = annualProd*home
    
    weibullxStock <- weibull*amountInRiver/totalProbability
    
    weibullxStockNormalized <- weibullxStock/sum(weibullxStock)
    
    FarmedFishInRiver <- annualProd*escapesPerTon*weibullxStockNormalized*survival
    
    
    return(FarmedFishInRiver[1:amountOfRivers])
  }
  
  ## weib function ---------------------------------
  # function to plot where points fall on a weibull plot
  weib<-function(arg1, beta, eta){
    
    nums <- c(1:4200)
    maximum <- which.max((beta/eta)*((nums/eta)^(beta-1))*exp(-1*((nums/eta)^beta)))
    
    
    distance <- riverdata$position-arg1
    for(i in c(1:amountOfRivers)){
      if (distance[i] < -1215){
        distance[i] = 2430 + distance[i]
      }
      else if (distance[i] > 1215){
        distance[i] = - 2430 + distance[i]
      }
    }
    
    amendedDistance <- distance  + maximum
    
    weibull <- ifelse(amendedDistance > 0, (beta/eta)*((amendedDistance/eta)^(beta-1))*exp(-1*((amendedDistance/eta)^beta)),0)
    return(weibull)
  }
  
  
  ## weib line function ---------------------------------
  # function to plot a weibull distribution
  weibL <- function(arg1, beta, eta){
    
    nums <- c(1:4200)
    numsL <- c(-1215:1215)
    maximum <- which.max((beta/eta)*((nums/eta)^(beta-1))*exp(-1*((nums/eta)^beta)))
    
    
    distance <- numsL-arg1
    
    for(i in c(1:2431)){
      if (distance[i] < -1215){
        distance[i] = 2430 + distance[i]
      }
      else if (distance[i] > 1215){
        distance[i] = - 2430 + distance[i]
      }
    }
    
    amendedDistance <- distance  + maximum
    
    weibull <- ifelse(amendedDistance > 0, (beta/eta)*((amendedDistance/eta)^(beta-1))*exp(-1*((amendedDistance/eta)^beta)),0)
    return(weibull)
  }

  
}

# runApp ---------------------------------
shinyApp(ui = ui, server = server)
