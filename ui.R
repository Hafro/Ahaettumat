library(shiny)
library(DT)
library(leaflet)
library(RColorBrewer)
library(XLConnect)
library(reshape)
library(shinythemes)
load("laxadata.RData")
uppl <<- uppl

shinyUI(shinyUI(navbarPage("",
 tabPanel("Dreifing",
  

  fluidPage(theme = shinytheme("cerulean"),
  
  tags$head(tags$style( 
    type = 'text/css',
    'form.well { max-height: 600px; overflow-y: auto; }'
  )),
  
  
  titlePanel("Áhættumat erfðablöndunar"),
    
  sidebarPanel(#Panellinn fyrir eldisstaðina
      h4("Patreksfjörður"), 
      helpText("Ársframleiðsla af eldislaxi í þúsundum tonna"),
      sliderInput("tonnSPat", label = NULL, 0, min = 0, max = 50, value = 20),
      helpText("Hlutfall snemmbúinna stroka"),
      sliderInput("tonnPat", label = NULL, 0, min = 0, max = 1, value = 0.5, step=0.1),
      
      h4("Tálknafjörður"),
      helpText("Ársframleiðsla af eldislaxi í þúsundum tonna"),
      sliderInput("tonnSTal", label = NULL, 0, min = 0, max = 50),
      helpText("Hlutfall snemmbúinna stroka"),
      sliderInput("tonnTal", label = NULL, 0, min = 0, max = 1, value = 0.5, step=0.1),
      
      h4("Arnarfjörður"),
      helpText("Ársframleiðsla af eldislaxi í þúsundum tonna"),
      sliderInput("tonnSArn", label = NULL, 0, min = 0, max = 50, value = 20),
      helpText("Hlutfall snemmbúinna stroka"),
      sliderInput("tonnArn", label = NULL, 0, min = 0, max = 1, value = 0.5, step=0.1),
      
      h4("Dýrafjörður"),
      helpText("Ársframleiðsla af eldislaxi í þúsundum tonna"),
      sliderInput("tonnSDyr", label = NULL, 0, min = 0, max = 50, value = 10),
      helpText("Hlutfall snemmbúinna stroka"),
      sliderInput("tonnDyr", label = NULL, 0, min = 0, max = 1, value = 0.5, step=0.1),
      
      h4("Önundarfjörður"),
      helpText("Ársframleiðsla af eldislaxi í þúsundum tonna"),
      sliderInput("tonnSOnu", label = NULL, 0, min = 0, max = 50),
      helpText("Hlutfall snemmbúinna stroka"),
      sliderInput("tonnOnu", label = NULL, 0, min = 0, max = 1, value = 0.5, step=0.1),
      
      h4("Ísafjarðardjúp"),
      helpText("Ársframleiðsla af eldislaxi í þúsundum tonna"),
      sliderInput("tonnSIsa", label = NULL, 0, min = 0, max = 50, value = 0),
      helpText("Hlutfall snemmbúinna stroka"),
      sliderInput("tonnIsa", label = NULL, 0, min = 0, max = 1, value = 0.5, step=0.1),
      
      h4("Eyjafjörður"),
      helpText("Ársframleiðsla af eldislaxi í þúsundum tonna"),
      sliderInput("tonnSEyj", label = NULL, 0, min = 0, max = 50, value = 0),
      helpText("Hlutfall snemmbúinna stroka"),
      sliderInput("tonnEyj", label = NULL, 0, min = 0, max = 1, value = 0.5, step=0.1),
      
      h4("Mjóifjörður / Norðfjarðarflói"),
      helpText("Ársframleiðsla af eldislaxi í þúsundum tonna"),
      sliderInput("tonnSMjo", label = NULL, 0, min = 0, max = 50),
      helpText("Hlutfall snemmbúinna stroka"),
      sliderInput("tonnMjo", label = NULL, 0, min = 0, max = 1, value = 0.5, step=0.1),
    
      h4("Reyðarfjörður"),
      helpText("Ársframleiðsla af eldislaxi í þúsundum tonna"),
      sliderInput("tonnSRey", label = NULL, 0, min = 0, max = 50, value = 10),
      helpText("Hlutfall snemmbúinna stroka"),
      sliderInput("tonnRey", label = NULL, 0, min = 0, max = 1, value = 0.5, step=0.1),
      
      h4("Fáskrúðsfjörður"),
      helpText("Ársframleiðsla af eldislaxi í þúsundum tonna"),
      sliderInput("tonnSFas", label = NULL, 0, min = 0, max = 50, value = 5),
      helpText("Hlutfall snemmbúinna stroka"),
      sliderInput("tonnFas", label = NULL, 0, min = 0, max = 1, value = 0.5, step=0.1),
      
      h4("Stöðvarfjörður"),
      helpText("Ársframleiðsla af eldislaxi í þúsundum tonna"),
      sliderInput("tonnSSto", label = NULL, 0, min = 0, max = 50, value = 0),
      helpText("Hlutfall snemmbúinna stroka"),
      sliderInput("tonnSto", label = NULL, 0, min = 0, max = 1, value = 0.5, step=0.1),
      
      h4("Berufjörður"),
      helpText("Ársframleiðsla af eldislaxi í þúsundum tonna"),
      sliderInput("tonnSBer", label = NULL, 0, min = 0, max = 50, value = 6),
      helpText("Hlutfall snemmbúinna stroka"),
      sliderInput("tonnBer", label = NULL, 0, min = 0, max = 1, value = 0.5, step=0.1),
      
      width = 3
  ),
  
  mainPanel(  
      tabsetPanel(
        tabPanel("Kort", leafletOutput("mymap")),             
        
        tabPanel("Graf", plotOutput("myPlot1"), 
                 selectInput("graf", label = "Veldu áhrif hvaða eldisstaðar skal sýna", choices = list("Heild" = 1,"Patreksfjörður" = 2, "Tálknafjörður" = 3, "Arnarfjörður" = 4, "Dýrafjörður" = 5, "Önundarfjörður" = 6, "Ísafjarðardjúp" = 7, "Eyjafjörður" = 8,"Mjóifjörður / Norðfjarðarflói" = 9, "Reyðarfjörður" = 10, "Fáskrúðsfjörður" = 11 , "Stöðvarfjörður" = 12, "Berufjörður" = 13), selected = 1), 
                 radioButtons("radio", label = "",choices = list("Fjöldi" = 1, "Prósentur" = 2),selected = 2)), 
        
        tabPanel("Tafla", DT::dataTableOutput("table"), downloadLink("downloadData", "Sækja Excel skjal")),
        
        
        tabPanel("Weibull", 
                 plotOutput("myPlot2"),
                 selectInput("graf2", label = "Veldu áhrif hvaða eldisstaðar skal sýna", choices = list("Heild" = 1,"Patreksfjörður" = 2, "Tálknafjörður" = 3, "Arnarfjörður" = 4, "Dýrafjörður" = 5, "Önundarfjörður" = 6, "Ísafjarðardjúp" = 7, "Eyjafjörður" = 8, "Mjóifjörður / Norðfjarðarflói" = 9, "Reyðarfjörður" = 10, "Fáskrúðsfjörður" = 11 , "Stöðvarfjörður" = 12, "Berufjörður" = 13), selected = 1))
  ))
  )),

 
  tabPanel("Breytur",  # Tab fyrir breytu stillingar
           fluidPage(
            titlePanel("Áhættumat erfðablöndunar"),
            tabsetPanel(
             tabPanel("Dreifingarstuðlar",
             fluidRow(
               column(3, 
                      h4(""),
                      helpText("Hér er hægt að breyta breytum og stuðlum sem hafa áhrif á módelið. ")),
               column(3,
                      h4("Weibull stuðlar fyrir snemmbúið strok:"),
                      sliderInput("beta", label = "beta", 0, min = 1, max = 10,step=0.1, value = 2.5),
                      sliderInput("eta", label = "eta", 0, min = 100, max = 1000, step=10,value = 120)),
               
               column(3,
                      h4("Líkur á að sjógönguseiði lifi af dvölina í sjó:"),
                      sliderInput("surv", label = NULL, 0, min = 0, max = 1,step=0.01,value = 0.05),
                      h4("Hlutfallsleg lífshæfni eldis-sjógönguseiða gagnvart villtum:"),
                      sliderInput("surv2", label = NULL, 0, min = 0, max = 1,step=0.01,value = 0.37)),
               
               column(3,
                      h4("Fjöldi laxa sem sleppur fyrir hvert tonn sem er framleitt"),
                      sliderInput("escS", label = "snemmbúin strok:", 0, min = 0, max = 10,step=0.1,value = 0.8),
                      sliderInput("esc", label = "síðbúin strok:", 0, min = 0, max = 10,step=0.1,value = 0.8))
                
                
             ),
             
             fluidRow(
               column(3,
                      h4("Heimsæknistuðull:"),
                      sliderInput("home", label = "snemmbúin strok:", 0, min = 0, max = 5,step=0.05,value = 0.2),
                      sliderInput("home2", label = "síðbúin strok:", 0, min = 0, max = 5,step=0.05,value = 0)),
                      
               column(3,
                      h4("Weibull stuðlar fyrir síðbúið strok:"),
                      sliderInput("beta2", label = "beta", 0, min = 1, max = 10,step=0.1, value = 2),
                      sliderInput("eta2", label = "eta", 0, min = 100, max = 1000,step=10, value = 1200)),
              
               column(3,
                      h4("Hlutfall síðbúinna stroka sem kynþroskast og leitar upp í á:"),
                      sliderInput("sex", label = NULL, 0, min = 0, max = 1,step=0.01,value = 0.15)),
               
               column(3,
                      h4("Tími sem laxinn er alinn í sjó í mánuðum:"),
                      sliderInput("cTime", label = NULL, 0, min = 10, max = 20,step=0.5,value = 18),
                      h4("Hættutími í mánuðum:"),
                      sliderInput("critPer", label = NULL, 0, min = 2, max = 5,step=0.5, value = 4))
            ))
            
            ))
           )
        )
  
     )
)
