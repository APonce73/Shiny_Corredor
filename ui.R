library(shiny)
library(leaflet)
library(knitr)
library(plotly)
library(ggplot2)
library(rpivotTable)


#runApp(system.file("shiny/", package = "googleVis"))
#library(ggplot2movies)

shinyUI(navbarPage(
  title = "Diversidad de Regionales",
   tabPanel('Introducción',
            div(img(src = "CONABIO_LOGO_13.JPG", width = "250"), style = "text-align: center;"),
            br(),
            bootstrapPage(h4("Datos de las Regionales de ", tags$a(href = "http://www.conabio.gob.mx/", "Conabio"  ) ,"en Oaxaca, Chiapas
                            Tabasco, Campeche, Yucatán y Quintana Roo."),
                          h4("Información adicional sobre el corredor biológico mesoamericano", tags$a(href="http://www.biodiversidad.gob.mx/corredor/corredor.html", "aqui" )),
                           h5("A continuación se explican cada una de las pestañas del análisis de los datos:") ),
            br(),
            br(),
            column(5, wellPanel(h3("Mapa "),
                                h5("En el mapa, se visualizan cada uno de los registros de las especies de acuerdo
                                   a distintos tipos de sistemas productivos donde las regionales de Conabio, tiene seguimiento
                                   de parcelas, comunidades o proyectos en distintas sistemas productivos"),
                                div(img(src = "Mapa.png", style = "display:block; margin-left: auto; margin-right: auto;",width = "400", height = "300")),
                                div(img(src = "Mapa1.png", style = "display:block; margin-left: auto; margin-right: auto;",width = "300", height = "300"))
                                )),
            
            column(5, wellPanel(h3("Especies. Tabla Dinámicas"),
                                
                                h5("2) Tabla pivotante: La tabla pivotante nos permite escoger las distintas 
                                   variables para poder verlas ya sea en tablas o gráficas. Al mismo tiempo nos permite
                                   hacer operaciones sencillas como sumatorias o porcientos de los resultados visualizados"),
                                div(img(src = "PivotTable.png", style = "display:block; margin-left: auto; margin-right: auto;",width = "300", height = "300"))
                                
                                )),
            
            column(5, wellPanel(h3("Especies. Datos por especie"),
                                h5("Se presenta la información general de las especies en dos formas:" ),
                                h5("1) Resumen por especie: Se puede escoger cada especie y nos da un breve resumen de sus
                                   valores de acuerdo a lo resportado por cada regional"),
                                div(img(src = "SummaryData.png", style = "display:block; margin-left: auto; margin-right: auto;", width = "300", height = "300"))
                               )),
            column(5, wellPanel(h3("Visualización"),
                                h5("Se realizó una diagrama de Sankey (DS). Los", tags$a(href = "https://es.wikipedia.org/wiki/Diagrama_de_Sankey", "DS"), "son un tipo específico de diagrama
                                   de flujo, en el que la anchura de las flechas se muestra la proporcional a la cantidad de flujo"),
                                div(img(src = "SankeyPlot.png", style = "display:block; margin-left: auto; margin-right: auto;",width = "400", height = "400"))
                                
                                )),
            column(5, wellPanel(h3("Diversidad"),
                                h5("Rarefracción"),
                                div(img(src = "Rarefraction.png", style = "display:block; margin-left: auto; margin-right: auto;",width = "300", height = "300")),
                                h5("Las curvas de Renyi"),
                                  div(img(src = "Renyi.png", style = "display:block; margin-left: auto; margin-right: auto;",width = "300", height = "300"))
                                
                                
                                )),
            column(9, wellPanel(h3("Información adicional sobre la visualización"),
                    h5("Toda el código fue elaborado en",tags$a(href = "http://cran.r-project.org", "R"  ) , "y se puede encontrarn en el ", tags$a(href = "https://github.com/APonce73", "Github"  )),
                    h5("Dudas y reportes de errores con Alejandro Ponce-M a aponce@conabio.gob.mx")
            ))
            
            
     
   ),

    tabPanel('Mapa', 
           # Define UI for slider demo application
           shinyUI(fluidPage(
             #Application title
             titlePanel("Especies en Regionales de Conabio"),
             h4("Razas de maíces"),
             
             sidebarLayout(
               sidebarPanel(
                 
                 #Estado
                 selectInput(inputId = "Estados",
                             label = h6("Estado:"),
                             c("All", levels(TableL$Estado))),
                 #c("All", unique(as.character(TableL$Raza_primaria)))),
                 
                 #Sistema
                 selectInput(inputId = "Sistemas",
                             label = h6("Sistema:"),
                             c("All", levels(TableL$Sistema))),
                 
                 #Por Genero
                 selectInput(inputId = "Géneros",
                             label = h6("Género:"),
                             c("All", levels(TableL$Género))), width = 2),
              fluidRow(
                 column(8,leafletOutput("mymap", width = "1000", height = "800"))
             )
            )
          ))
         ),

##Ventana 2 Especies
  tabPanel('Especies. Tabla Dinámica',
           # Define UI for slider demo application
           shinyUI(
             
             fluidPage(
             #Application title
             
            titlePanel("Información General de las Especies"),
             h4("En la",tags$a(href = "https://es.wikipedia.org/wiki/Tabla_din%C3%A1mica", "Tabla dinámica") , "selecciona y arrastra el elemento que deseas ver en columna y en renglón"),
             h4("Selecciona tabla o gráfica para ver y analizar tus datos"),
            
             
             mainPanel(
               
               rpivotTableOutput("Ceratti3")
               
               #fluidRow(
              #   column(9, 
              #          h4("Observaciones"),
              #          rpivotTableOutput("Ceratti3")
              #        
              #   ))
              

               , width = 8)
             
             )
            )
           ),

##Ventana 3 Especies
tabPanel('Especies. Datos por especie',
         # Define UI for slider demo application
         shinyUI(
           
           fluidPage(
             #Application title
             
             titlePanel("Información General de las Especies"),
             h4("Especies"),
            
               sidebarPanel(
                 
                 
                 # #Raza Primaria
                 selectInput(inputId = "NomSci1",
                             label = h6("Especie:"),
                             c("All", TableL101$NomSci))
                 
                 #numericInput("obs", "Numero de observaciones para ver:", 10)
                 
                 # selectInput(inputId = "Especie",
                 #             label = h6("Especie:"),
                 #             levels(TableL101$NomSci))
               ),
               
               fluidRow(
                 column(9, 
                        # h4("Observaciones"),
                        #rpivotTableOutput("Ceratti3"),
                        
                        h4("Resumen"),
                        verbatimTextOutput("Ceratti2" )
                 ))
              )
             
           )
         
),


##Ventana 2.1 Sankeyplot

tabPanel('Visualización', 
         # Define UI for slider demo application
         shinyUI(fluidPage(
           #Application title
           titlePanel("Gráfica de Sankey"),
           h4("Razas de maíces"),
           
           sidebarLayout(
             sidebarPanel(
               
               #Por Estado
               selectInput(inputId = "Estado",
                           label = h6("Estado:"),
                           choice = levels(TableL1$Estado),
                           selected = levels(TableL1$Estado),
                            multiple = T),
               br(),
               selectInput(inputId = "Sistema",
                           label = h6("Sistema:"),
                           choices = levels(TableL1$Sistema),
                           selected = levels(TableL1$Sistema),
                            multiple = T),
               br(),
               
               
 # selectInput(inputId = "Columna", label = h4("Variables"),
#       choices = names(TableL4)[2:4], selected = names(TableL4)[2], multiple = F),
               
              # selectInput(inputId = "Municipio",
              #             label = h6("Sistema:"),
              #             c("All", levels(TableL1$Municipio))),
               
               #h6("Visualización de la presencia de maíces en distintos estados"),
               br()
               
              # selectInput(inputId = "Complejo_racials",
              #             label = h6("Grupo racial:"),
              #             levels(TableLL$Complejo_racial)),
               
               
               
             , width = 2),  
                
                    column(9, htmlOutput("Sankeyplot1", width = "1000", height = "600"))
             )

         ))
     ),

tabPanel('Diversidad',
         #div(img(src = "CONABIO_LOGO_13.JPG", width = "250", height = "250"), style = "text-align: center;"),
         #br(),
         bootstrapPage( 

h4("Para evaluar la eficacia  de los muestreos se realizaran curvas de rarefracción, que
    nos ayudan a ver la eficacia del muestreo y la comparabilidad de los sitios de estudio o
    sistemas. Al mismo tiempo nos da una riqueza de especies.
    Los análisis estan basados en la formulación de Hulbert’s (1971) y 
    un error estándar de Heck et al. (1975)."),
                        br(),
                        br(),
                        sidebarLayout(
                          sidebarPanel(
                            
                            #actionButton("selectall", label="Select/Deselect all"),
                            selectInput(inputId = "Columna", label = h4("Variables"),
                                               choices = names(TableL4)[2:4], selected = names(TableL4)[2], multiple = F),
                            
                            #Por Estado
                   #         selectInput(inputId = "Estado1",
                    #                    label = h6("Estado:"),
                    #                    c("All", levels(TableL4$Estado))),
                    #        br(),
                    #        selectInput(inputId = "Sistema1",
                    #                    label = h6("Sistema:"),
                    #                    c("All", levels(TableL4$Sistema))),
                    #        br(),
                    #        selectInput(inputId = "Municipio1",
                    #                    label = h6("Municipio:"),
                    #                    c("All", TableL4$Municipio)),
                    #        #h6("Visualización de la presencia de maíces en distintos estados"),
                            br()
                            
                            # selectInput(inputId = "Complejo_racials",
                            #             label = h6("Grupo racial:"),
                            #             levels(TableLL$Complejo_racial)),
                            
                            
                            
                            , width = 2),  
                          fluidRow(column(8,
                                          plotOutput("Rarefraction", width = "1000", height = "600"))
                                          )
                            
                        
                            )
                          
                          
                        ),
         br(),
         br(),
         br(),
         br(),
         br(),
         
          h4(" Las curvas de diversidad de Renyi son líneas que 
          dan información de riqueza, equidad y permite comparar entre 
             distintos sitios o parcelas de forma gráfica, ordenando de 
             mayor a menor, lo que facilita su interpretación 
             (Kindt y Coe, 2005). Si una curva esta por encima de otra, 
             significa que tiene un mayor “perfil” con respecto a la 
             diversidad. Si las curvas se cruzan entre si, nos indica que 
             no es posible comparar esos dos ecosistemas en particular 
             (Kindt y Coe, 2005; Tóthmérész, 1995). El valor en el eje de las 'x' igual a 0, 1 y 2, 
             representan la riqueza, diversidad de shannon y simpson respectivamente; para obtener 
             En diversidad Rényi las curvas represetan: Riqueza (exp(x=0)), diversidad Shannon (x=1), 
             Inversa de simpson (exp (x=2)). Finalmente, la pendiente o la forma de la curva nos dice la equidad"),
         br(),
         fluidRow(column(9,
                         plotlyOutput("Renyi1", width = "1000", height = "600"))
         )
         
         
         )    
                             

  ))
