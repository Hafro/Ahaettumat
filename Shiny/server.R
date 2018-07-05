library(shiny)
library(DT)
library(leaflet)
library(RColorBrewer)
library(leaflet.minicharts)
library(ggplot2)
library(XLConnect)
library(reshape)
load("laxadata.RData")
uppl <<- uppl
colnames(uppl) <- c("nafn", "V", "N", "fjarlaegd", "Medalfjoldi", "fjoldi.flokk")


shinyServer(function(input, output) {
  dataZoom <- reactive({         # Tekur inn zoom-level til að stilla af stærðir
  zoom <- input$mymap_zoom
  zoom})

  # Föll sem taka inn upplýsingarnar um eldisstaðina og reikna út dreifinguna á fiskunum
  dataKviAdult <- reactive({
    kviAdult <- data.frame(Dreifing(eldisstadir["Patreksfjörður","staðsetning"], input$beta2,input$eta2, input$tonnSPat*1000*(1-input$tonnPat), input$tonnPat*input$tonnSPat*1000, input$esc, input$sex, input$cTime, input$critPer, input$home2),
                            Dreifing(eldisstadir["Tálknafjörður","staðsetning"],input$beta2,input$eta2,input$tonnSTal*1000*(1-input$tonnTal),input$tonnTal*input$tonnSTal*1000,input$esc,input$sex, input$cTime, input$critPer, input$home2),
                            Dreifing(eldisstadir["Arnarfjörður","staðsetning"],input$beta2,input$eta2,input$tonnSArn*1000*(1-input$tonnArn),input$tonnArn*input$tonnSArn*1000,input$esc,input$sex, input$cTime, input$critPer, input$home2),
                            Dreifing(eldisstadir["Dýrafjörður","staðsetning"],input$beta2,input$eta2,input$tonnSDyr*1000*(1-input$tonnDyr),input$tonnDyr*input$tonnSDyr*1000,input$esc,input$sex, input$cTime, input$critPer, input$home2),
                            Dreifing(eldisstadir["Önundarfjörður","staðsetning"],input$beta2,input$eta2,input$tonnSOnu*1000*(1-input$tonnOnu),input$tonnOnu*input$tonnSOnu*1000,input$esc,input$sex, input$cTime, input$critPer, input$home2),
                            Dreifing(eldisstadir["Ísafjarðardjúp","staðsetning"],input$beta2,input$eta2,input$tonnSIsa*1000*(1-input$tonnIsa),input$tonnIsa*input$tonnSIsa*1000,input$esc,input$sex, input$cTime, input$critPer, input$home2),
                            Dreifing(eldisstadir["Eyjafjörður","staðsetning"],input$beta2,input$eta2,input$tonnSEyj*1000*(1-input$tonnEyj),input$tonnEyj*input$tonnSEyj*1000,input$esc,input$sex, input$cTime, input$critPer, input$home2),
                            Dreifing(eldisstadir["Mjóifjörður / Norðfjarðarflói","staðsetning"],input$beta2,input$eta2,input$tonnSMjo*1000*(1-input$tonnMjo),input$tonnMjo*input$tonnSMjo*1000,input$esc,input$sex, input$cTime, input$critPer, input$home2),
                            Dreifing(eldisstadir["Reyðarfjörður","staðsetning"],input$beta2,input$eta2,input$tonnSRey*1000*(1-input$tonnRey),input$tonnRey*input$tonnSRey*1000,input$esc,input$sex, input$cTime, input$critPer, input$home2),
                            Dreifing(eldisstadir["Fáskrúðsfjörður","staðsetning"],input$beta2,input$eta2,input$tonnSFas*1000*(1-input$tonnFas),input$tonnFas*input$tonnSFas*1000,input$esc,input$sex, input$cTime, input$critPer, input$home2),
                            Dreifing(eldisstadir["Stöðvarfjörður","staðsetning"],input$beta2,input$eta2,input$tonnSSto*1000*(1-input$tonnSto),input$tonnSto*input$tonnSSto*1000,input$esc,input$sex, input$cTime, input$critPer, input$home2),
                            Dreifing(eldisstadir["Berufjörður","staðsetning"],input$beta2,input$eta2,input$tonnSBer*1000*(1-input$tonnBer),input$tonnBer*input$tonnSBer*1000,input$esc,input$sex, input$cTime, input$critPer, input$home2)
                         )
    kviAdult})
  
  dataKviSmolt <- reactive({
    if (input$tonnSBer < 6.1){
      heim = 1.3
    } else {
      heim = input$home 
    }
    kviSmolt <- data.frame(smoltDreifing(eldisstadir["Patreksfjörður","staðsetning"],input$beta,input$eta,input$tonnSPat*1000*(1-input$tonnPat),input$tonnPat*input$tonnSPat*1000,input$escS,input$surv*input$surv2, input$home),
                           smoltDreifing(eldisstadir["Tálknafjörður","staðsetning"],input$beta,input$eta,input$tonnSTal*1000*(1-input$tonnTal),input$tonnTal*input$tonnSTal*1000,input$escS,input$surv*input$surv2, input$home),
                           smoltDreifing(eldisstadir["Arnarfjörður","staðsetning"],input$beta,input$eta,input$tonnSArn*1000*(1-input$tonnArn),input$tonnArn*input$tonnSArn*1000,input$escS,input$surv*input$surv2, input$home),
                           smoltDreifing(eldisstadir["Dýrafjörður","staðsetning"],input$beta,input$eta,input$tonnSDyr*1000*(1-input$tonnDyr),input$tonnDyr*input$tonnSDyr*1000,input$escS,input$surv*input$surv2, input$home),
                           smoltDreifing(eldisstadir["Önundarfjörður","staðsetning"],input$beta,input$eta,input$tonnSOnu*1000*(1-input$tonnOnu),input$tonnOnu*input$tonnSOnu*1000,input$escS,input$surv*input$surv2, input$home),
                           smoltDreifing(eldisstadir["Ísafjarðardjúp","staðsetning"],input$beta,input$eta,input$tonnSIsa*1000*(1-input$tonnIsa),input$tonnIsa*input$tonnSIsa*1000,input$escS,input$surv*input$surv2, input$home),
                           smoltDreifing(eldisstadir["Eyjafjörður","staðsetning"],input$beta,input$eta,input$tonnSEyj*1000*(1-input$tonnEyj),input$tonnEyj*input$tonnSEyj*1000,input$escS,input$surv*input$surv2, input$home),
                           smoltDreifing(eldisstadir["Mjóifjörður / Norðfjarðarflói","staðsetning"],input$beta,input$eta,input$tonnSMjo*1000*(1-input$tonnMjo),input$tonnMjo*input$tonnSMjo*1000,input$escS,input$surv*input$surv2, input$home),
                           smoltDreifing(eldisstadir["Reyðarfjörður","staðsetning"],input$beta,input$eta,input$tonnSRey*1000*(1-input$tonnRey),input$tonnRey*input$tonnSRey*1000,input$escS,input$surv*input$surv2, input$home),
                           smoltDreifing(eldisstadir["Fáskrúðsfjörður","staðsetning"],input$beta,input$eta,input$tonnSFas*1000*(1-input$tonnFas),input$tonnFas*input$tonnSFas*1000,input$escS,input$surv*input$surv2, input$home),
                           smoltDreifing(eldisstadir["Stöðvarfjörður","staðsetning"],input$beta,input$eta,input$tonnSSto*1000*(1-input$tonnSto),input$tonnSto*input$tonnSSto*1000,input$escS,input$surv*input$surv2, input$home),
                           smoltDreifing(eldisstadir["Berufjörður","staðsetning"],input$beta,input$eta,input$tonnSBer*1000*(1-input$tonnBer),input$tonnBer*input$tonnSBer*1000,input$escS,input$surv*input$surv2, heim)
                          )
    kviSmolt})
  
  dataKviSamtals <- reactive({
    kviSamtals <- dataKviAdult() + dataKviSmolt()
  kviSamtals})
  

  # Tekur inn upplýsingar um hvað gröfin eiga að sýna
  dataGraf <- reactive({
    graf <- input$graf
    if(graf == 1){
      graf = 14
    }
  graf})
  
  dataGraf2 <- reactive({
    graf2 <- input$graf2
    if(graf2 == 1){
      graf2 = 14
    }
    graf2})
  

  #Fall sem setur upp data.frameið sem inniheldur allar upplýsingar, t.d. dreifingar, stock size o.fl
  dataKvi <- reactive({
  kvi <- data.frame(uppl[,5],
                   dataKviSamtals(),
                   row.names =  as.character(uppl$nafn))
  kvi$Eldislaxar <- rowSums(kvi[,2:13])
  colnames(kvi) <- c("Villtir laxar","Eldislaxar frá Patreksfirði","Eldislaxar frá Tálknafirði","Eldislaxar frá Arnarfirði","Eldislaxar frá Dýrafirði","Eldislaxar frá Önundarfirði","Eldislaxar frá Ísafjarðardjúpi","Eldislaxar frá Eyjafirði","Eldislaxar frá Mjóafirði / Norðfjarðarflóa","Eldislaxar frá Reyðarfirði","Eldislaxar frá Fáskrúðsfirði","Eldislaxar frá Stöðvarfirði","Eldislaxar frá Berufirði", "Eldislaxar")
  kvi$"Prósent af heild" = round(kvi$Eldislaxar / (kvi$"Villtir laxar" + kvi$Eldislaxar),digits = 4)*100
  Samtals <- c(sum(kvi$"Villtir laxar"),sum(kvi$"Eldislaxar frá Patreksfirði"),sum(kvi$"Eldislaxar frá Tálknafirði"),sum(kvi$"Eldislaxar frá Arnarfirði"),sum(kvi$"Eldislaxar frá Dýrafirði"),sum(kvi$"Eldislaxar frá Önundarfirði"),sum(kvi$"Eldislaxar frá Ísafjarðardjúpi"),sum(kvi$"Eldislaxar frá Eyjafirði"),sum(kvi$"Eldislaxar frá Mjóafirði / Norðfjarðarflóa"),sum(kvi$"Eldislaxar frá Reyðarfirði"),sum(kvi$"Eldislaxar frá Fáskrúðsfirði"),sum(kvi$"Eldislaxar frá Stöðvarfirði"),sum(kvi$"Eldislaxar frá Berufirði"),sum(kvi$"Eldislaxar"),round(sum(kvi$"Eldislaxar")/(sum(kvi$"Eldislaxar")+sum(kvi$"Villtir laxar")),digits=1)+100)
  kvi <- rbind(kvi, Samtals)
  row.names(kvi) = c(as.character(uppl$nafn),"Samtals")
  kvi})
  
  
  # fall sem skilar vigri sem inniheldur magnið í öllum fjörðum
  dataKviMagn <- reactive({
    kviMagn <- c(input$tonnSPat,input$tonnSTal,input$tonnSArn,input$tonnSDyr,input$tonnSOnu,input$tonnSIsa,input$tonnSEyj,input$tonnSMjo,input$tonnSRey,input$tonnSFas,input$tonnSSto,input$tonnSBer)
    kviMagn})
  
  
  
  # Kortið teiknað upp og stillt´á Ísland
  output$mymap <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 6, maxZoom = 15)) %>%
      addProviderTiles(providers$Hydda.Base,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      setView(-19.020835000000034,64.963051, zoom = 5)  
     
  })
  
  
  # Kökuritin teiknuð inn, proxy notað til að kortið sjálft endurstillist ekki einungis kökurnar
  observe({
    if (!is.null(dataZoom())){
      leafletProxy("mymap") %>%
        addMinicharts(
          lng = uppl$V, lat = uppl$N,
          type = "pie",
          transitionTime = 0,
          chartdata = dataKvi()[1:78,c("Villtir laxar","Eldislaxar")], 
          colorPalette = c("green","red"), 
          width = as.numeric(uppl$fjoldi.flokk)/75*(dataZoom()^3), # aðlagar stærð að zoom-leveli
          popup = popupArgs(showTitle = TRUE, showValues = TRUE, labels = NULL,
                            supValues <- data.frame(round(dataKvi()[1:78,"Eldislaxar"]/(dataKvi()[1:78,"Eldislaxar"]+dataKvi()[1:78,"Villtir laxar"]),digits = 4)*100,uppl$nafn), supLabels = c("Prósenta","Á"))
       )  
   }
  })
  
  # Teiknar upp rauðu hringina fyrir eldin
  observe({
    j <- 1 
    leafletProxy("mymap") %>%
      clearGroup("eldi")
   for (i in dataKviMagn()){
     if (i != 0){
      leafletProxy("mymap") %>%
        addCircleMarkers(
          lng = eldisstadir[j,2], lat = eldisstadir[j,3],
          color = "red", 
          radius = i/15,
          group = "eldi",
          label = row.names(eldisstadir)[j],
          popup = paste(as.character(i),"þúsund tonn", sep = " ")
        )
     }
     j <- j+1
   }
  })
  
    # Teiknar upp súluritið, síar út tóm eldi og ár sem verða ekki fyrir áhrifum
    output$myPlot1 <- renderPlot({
      yHnit <- dataKvi()[1:78,as.integer(dataGraf())]
      yHnit <- yHnit[order(uppl$"fjarlaegd")]
      xNofn <- uppl$nafn[order(uppl$"fjarlaegd")]
      xNofn <- factor(xNofn, levels=unique(xNofn))
      ahrif <- which(yHnit != 0, arr.ind = T)
      if(length(ahrif)==0){ahrif = 0}
      if(input$radio == 1){
        ggplot(data=dataKvi()[ahrif,], aes(x=xNofn[ahrif],y=yHnit[ahrif])) + geom_bar(stat="identity") + xlab("Laxá") + ylab("Fjöldi eldisfiska í á") + theme(axis.text.x = element_text(angle = 60, hjust = 1))
      }
      else{
        fjoldiVilltra <- dataKvi()[order(uppl$"fjarlaegd"),1]
        prosentur <- yHnit/(fjoldiVilltra+yHnit) *100
        ggplot(data=dataKvi()[ahrif,], aes(x=xNofn[ahrif],y=prosentur[ahrif])) + geom_bar(stat="identity", aes(fill = prosentur[ahrif] > 4 )) + xlab("Laxá") + ylab("Prósenta af eldisfisk í á") + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_hline(yintercept =  4, color = "red") + scale_fill_manual(values = c('dark grey', 'red') )  + theme(legend.position="none")
      }
    })
  
    
    # Teiknar weibull dreifingar grafið og notar til þess weib föllin
  output$myPlot2 <- renderPlot({
    if(as.integer(dataGraf2())==14){
      weibbS <- rep.int(0,78)
      weibbB <- rep.int(0,78)
      weibbSL <- rep.int(0,2431)
      weibbBL <- rep.int(0,2431)
      tolur <- data.frame(c(-1215:1215))
      for(i in c(1:12)){
        weibbS = weibbS + weib(eldisstadir[i,1],input$beta,input$eta)
        weibbB = weibbB + weib(eldisstadir[i,1],input$beta2,input$eta2)
        weibbSL = weibbSL + weibL(eldisstadir[i,1],input$beta,input$eta)
        weibbBL = weibbBL + weibL(eldisstadir[i,1],input$beta2,input$eta2)
      }
      ggplot() + geom_point(data=uppl, aes(x=fjarlaegd,y=weibbS, colour = "Snemmbúin strok")) +
        geom_line(data=tolur, aes(x=tolur,y=weibbSL, colour = "Snemmbúin strok")) + 
        geom_point(data=uppl, aes(x=fjarlaegd,y=weibbB,colour = "Síðbúin strok")) + 
        geom_line(data=tolur, aes(x=tolur,y=weibbBL,colour = "Síðbúin strok")) +
        geom_point(data=uppl, aes(x=fjarlaegd,y=weibbB+weibbS,colour = "Heildarstrok")) +
        geom_line(data=tolur, aes(x=tolur,y=weibbBL+weibbSL,colour = "Heildarstrok")) +
        xlab("Ár") + ylab("Prósentur")+scale_x_discrete(breaks=NULL)
    } else{
      stadsetning <- eldisstadir[as.integer(dataGraf2())-1,1]
      tolur <- data.frame(c(-1215:1215))
      ggplot() + geom_point(data=uppl, aes(x=fjarlaegd,y=weib(stadsetning,input$beta,input$eta), colour = "Snemmbúin strok")) +
        geom_line(data=tolur, aes(x=tolur,y=weibL(stadsetning,input$beta,input$eta),colour = "Snemmbúin strok")) + 
        geom_point(data=uppl, aes(x=fjarlaegd,y=weib(stadsetning,input$beta2,input$eta2),colour = "Síðbúin strok")) + 
        geom_line(data=tolur, aes(x=tolur,y=weibL(stadsetning,input$beta2,input$eta2),colour = "Síðbúin strok")) +
        geom_point(data=uppl, aes(x=fjarlaegd,y=weib(stadsetning,input$beta2,input$eta2)+weib(stadsetning,input$beta,input$eta),colour = "Heildarstrok")) +
        geom_line(data=tolur, aes(x=tolur,y=weibL(stadsetning,input$beta2,input$eta2)+weibL(stadsetning,input$beta,input$eta),colour = "Heildarstrok")) +
        xlab("Ár") + ylab("Prósentur")+scale_x_discrete(breaks=NULL) 
    }
  })
  
  # skrifar út töfluna
  output$table <- DT::renderDataTable({
    yHnit <- dataKvi()[1:79,"Eldislaxar"]
    xHnit <- dataKvi()[79,]
    ahrifX <- which(xHnit != 0, arr.ind = T)[,2]
    if (sum(dataKvi()[1:78,"Eldislaxar"])!=0){
      ahrifY <- which(yHnit != 0, arr.ind = T)
      df <- dataKvi()[ahrifY,ahrifX]
    } else{
      ahrifY <- c(1:78)
      df  = data.frame(dataKvi()$"Villtir laxar"[1:78], row.names = uppl$nafn)
      colnames(df)="Villtir laxar"
    }
    datatable(df, rownames = TRUE, filter = "none", width = "100%")
    
  })
  
  # Vistar merkilegu upplýsingarnar í excel skjali
  save_results = reactive({
    yHnit <- dataKvi()[1:79,"Eldislaxar"]
    xHnit <- dataKvi()[79,]
    ahrifX <- which(xHnit != 0, arr.ind = T)[,2]
    if (sum(dataKvi()[1:78,"Eldislaxar"])!=0){
      ahrifY <- which(yHnit != 0, arr.ind = T)
      df <- dataKvi()[ahrifY,ahrifX]
    } else{
      ahrifY <- c(1:78)
      df  = data.frame(dataKvi()$"Villtir laxar"[1:78], row.names = uppl$nafn)
      colnames(df)="Villtir laxar"
    }
    wb <- loadWorkbook("results.xlsx", create = TRUE)
    createSheet(wb, name = "Ahrif")
    createSheet(wb, name = "Breytur")
    writeWorksheet(wb,df , sheet = "Ahrif", rownames = row.names(df))
    setColumnWidth(wb, sheet = "Ahrif", column= c(1:30), width = 6000)
    setRowHeight(wb, sheet = "Ahrif", row = 1, height = 25)
    clearRange(wb, sheet = "Ahrif", coords= c(1,1,1,1))
    
    eldi <- c(row.names(eldisstadir)," ")
    magnEldis <- c(input$tonnSPat, input$tonnSTal, input$tonnSArn, input$tonnSDyr, input$tonnSOnu, input$tonnSIsa,input$tonnSEyj  ,input$tonnSMjo, input$tonnSRey, input$tonnSFas, input$tonnSSto, input$tonnSBer," ")
    hlutfSmolts <- c(input$tonnPat, input$tonnTal, input$tonnArn, input$tonnDyr, input$tonnOnu, input$tonnIsa, input$tonnEyj,input$tonnMjo, input$tonnRey, input$tonnFas, input$tonnSto, input$tonnBer," ")
    bil <- c(" ", " "," "," "," "," "," "," "," "," "," "," "," ")
    nofnabrey <- c("Fjöldi síðbúinna stroka fyrir hvert tonn", "Fjöldi snemmbúinna stroka fyrir hvert tonn", "Tími sem lax er alinn í sjó í mánuðum", "Hættutími í mánuðum", "Hlutfall síðbúinna stroka sem kynþroskast og leitar upp í á", "Líkur á að sjógönguseiði lifi af dvölina í sjó", "Hlutfallsleg lífshæfni eldis-sjógönguseiða gagnvart villtum", "Heimsæknistuðull fyrir síðbúin strok", "Heimsæknistuðull fyrir snemmbúin strok", "Weibull beta fyrir síðbúið strok", "Weibull eta fyrir síðbúið strok", "Weibull beta fyrir snemmbúið strok", "Weibull eta fyrir snemmbúið strok")
    gildibrey <- c(input$esc, input$escS,input$cTime,input$critPer,input$sex,input$surv,input$surv2,input$home2,input$home,input$beta2,input$eta2,input$beta,input$eta)
    df2 = data.frame(eldi,magnEldis,hlutfSmolts,bil,nofnabrey,gildibrey)
    colnames(df2) = c("Eldisstaður", "Fjöldi tonna í þúsundum", "Hlutfall snemmbúinna stroka", " ", "Breytur", "Gildi á breytum")
    setColumnWidth(wb, sheet = "Breytur", column= c(1:3,5:6), width = 6000)
    setColumnWidth(wb, sheet = "Breytur", column= 5, width =13000)
    setRowHeight(wb, sheet = "Breytur", row = 1, height = 25)
    writeWorksheet(wb,df2, sheet = "Breytur")
    wb  
  })
  
  
  # skilar downloaduðu excel skjali
  output$downloadData <- downloadHandler(
    filename = function() { 'results.xlsx' },
    content = function(file) {
      filename = paste(file,"xlsx",sep=".")
      wb <- save_results()
      saveWorkbook(wb)
      file.rename("results.xlsx",file)
    }   
  ) 
  
  
})