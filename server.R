library(shiny)
library(leaflet)
library(RColorBrewer)
library(dplyr)
library(knitr)
library(vcd)
library(grid)
library(plotly)
#library(ggplot2)
#library(googleVis)
#library(igraph)
library(reshape)
library(vegan)
library(googleVis)
library(mxmaps)
library(rpivotTable)


#library(rCharts)
#runGist(4642963)

#library(ggplot2movies)

#shiny::runApp(system.file("shiny/", package = "googleVis"))

# Define server logic for slider examples
shinyServer(function(input, output) {
  
  

  #Ventana 1
  #### For the map in leaflet
  points <- reactive({
    #input$update
    #TableL <- TableL()
    if (input$Estados != "All") {
      TableL <- TableL[TableL$Estado %in% input$Estados,]
    }else TableL <- TableL
    
    if (input$Sistemas != "All") {
      TableL <- TableL[TableL$Sistema %in% input$Sistemas,]
    }else TableL <- TableL
    
    if (input$Géneros != "All") {
      TableL <- TableL[TableL$Género %in% input$Géneros,]
    }else TableL <- TableL
    
  })
  
  

  #P el mapa en leaflet
  output$mymap <- renderLeaflet({
    TTT <- c(brewer.pal(8,"Dark2"))
    
    Goldberg <- points()
    leaflet(data = Goldberg) %>%
      clearShapes() %>%
      addTiles() %>%
      clearBounds() %>%  
      addCircleMarkers(lng = ~lng, lat = ~lat, 
                       weight = 3, radius = 4, fillOpacity = 0.5, color = TTT,
                       clusterOptions = markerClusterOptions(showCoverageOnHover = T, 
                                                             spiderfyOnMaxZoom = T,
                                                             zoomToBoundsOnClick = T,
                                                             spiderfyDistanceMultiplier = 2), 
                       popup = paste(sep = " ","Sistema:",Goldberg$Sistema,"<br/>","Municipio:",Goldberg$Municipio,"<br/>","Localidad:",Goldberg$Localidad,"<br/>","Especie:",Goldberg$NomSci)) %>%
      #addMeasure(primaryLengthUnit = "kilometers", primaryAreaUnit = "hectares",activeColor = '#FF00FF') %>%
      addProviderTiles("OpenStreetMap.DE")
  })
  ## display a palettes simultanoeusly
 
#############################################
  #Para ventana 2 Imagenes y Grafico cleveland Plot

  
  Ceratti1 <- reactive({
    if (input$NomSci1 != "All") {

      TableL10 <- TableL101[TableL101$NomSci %in% input$NomSci1,]
      TableL10 <- droplevels(TableL10)
    }else TableL10 <- TableL101
  })

  output$Ceratti2 <- renderPrint({
    summary(Ceratti1())
  })  
  
#  output$Ceratti3 <- renderTable({
#    head(Ceratti1(), n = input$obs)
#  })

  #Para hacer un PivotTable
  
    output$Ceratti3 <- renderRpivotTable({
      rpivotTable(TableL101)
    })
  

    Narbat <- reactive({
        TableL1 <- TableL1[TableL1$Estado %in% input$Estado & TableL1$Sistema %in% input$Sistema,]
      
    })  
    
    
    #  
  
  #####################
  ######Hacer el Sankey
  
  #####################
  
  #Ventana 2.1 Sankey plot
  
  Richard1 <- reactive({
        TableH <- Narbat()
        #TableH <- TableL1
        
  attach(TableH)
        head(TableH)
  Glenn2 <- aggregate(Val1 ~ Estado + Municipio , FUN = sum, na.rm = T)
  #Glenn3 <- aggregate(Val1 ~ Municipio + NomSci , FUN = sum, na.rm = T)
  Glenn3 <- aggregate(Val1 ~ Municipio + Sistema , FUN = sum, na.rm = T)
  Glenn4 <- aggregate(Val1 ~ Sistema + Género , FUN = sum, na.rm = T)
  Glenn5 <- aggregate(Val1 ~ Género + NomSci , FUN = sum, na.rm = T)
  
  #head(Glenn2)
  
  names(Glenn2)[1:3] <- c("origin","visit","Val1")
  names(Glenn3)[1:3] <- c("origin","visit","Val1")
  names(Glenn4)[1:3] <- c("origin","visit","Val1")
  names(Glenn5)[1:3] <- c("origin","visit","Val1")
  detach(TableH)
  
  
  Katcha <- rbind(Glenn2, Glenn3, Glenn4, Glenn5)
  
  })
  
  
  #P hacer la figura
  output$Sankeyplot1 <- renderGvis({
#    
    
    
    Feynmann1 <- Richard1()
    #Feynmann1 <- Katcha
    LL34 <- gvisSankey(Feynmann1, from = "origin", to = "visit", weight = "Val1",
                       options = list(height = 850, width = 950,
                                      sankey = "{
                                      link:{color:{fill: 'red', fillOpacity: 0.9}},
                                      node:{nodePadding: 1, 
                                      label:{fontSize: 10}, 
                                      interactivity: true, width: 40},}"
                                      ), chartid = "Sankey")
    
    
    return(LL34)
    
    })
  
  
  ####################################
  # Ventana de Diversidad
  Narbat1 <- reactive({
    
    TableL4 <- data.frame(TableL4[,input$Columna], TableL4[,-c(1:4)])
      
  })
  # 
  
  
  output$Rarefraction <- renderPlot({
    Narbat2 <- Narbat1()
    TableL5 <- aggregate(Narbat2[,-1],list(Narbat2[,1]), FUN = sum, na.rm = T)
    
    
    LL35 <- RarefraccionCC(TableL5[,-c(1)], TableL5[,1])
    return(LL35)
  })
  
  
  output$Renyi1 <- renderPlotly({
    Narbat2 <- Narbat1()
    TableL5 <- aggregate(Narbat2[,-1],list(Narbat2[,1]), FUN = sum, na.rm = T)
    
    
    LL35 <- RenyiCC(TableL5[,-c(1)], TableL5[,1])
    #return(LL35)
    gg <- ggplotly(LL35)
    gg
  })

  
})
