# ---
# Titulo: "Analisis de accidentalidad en el municipio de Medellin."
# Autores:"ALEJANDRA GRACIANO"
#         "JULIAN QUINTANA"
#         "SERGIO VALENCIA"
# Fecha: "12/10/2019"
# ---

library(shiny)
library(leaflet)
library(sf)

# Leer el archivo con la base de datos despues de hacer la limpieza de los datos.
Accidentalidad <- read.csv("Accidentalidad_Total_3.csv",header=TRUE, sep=",") 

annos <- Accidentalidad$f_accidente
annos <- as.Date(annos, "%Y")
annos <- format(annos,"%Y")
annos <- unique(annos)

# Define UI for application
ui <- fluidPage(
    
    # Indicar el encabezado de la aplicacion.
    h1(span("Análisis de la accidentalidad en el Municipio de Medellín", style = "font-weight: 600"), 
       style = "font-family: 'Source Sans Pro';
        color: #fff; text-align: center;
        background-color:#4682B4;
        padding: 35px"),
    br(),
    
    # Establecer una barra de navegacion en donde se encuentran todos las acciones de entrada para el analisis de los datos.
    sidebarLayout(
        column(width = 3,
               wellPanel(style = "background: #F0F8FF",
                         # Ingresar los botones de control para navegar por la información base y el aplicativo
                         selectInput("Comuna_Seleccionada","Comuna a analizar:",c("Seleccione una comuna:",unique(as.character(Accidentalidad$COMUNA)))),
                         selectInput("Barrio_Seleccionado","Barrio a analizar:",c("Seleccione un barrio:",unique(as.character(Accidentalidad$BARRIO)))),
                         selectInput("Mes_Analizado","Mes a analizar:",c("Seleccione un mes:",unique(as.character(unique(Accidentalidad$MES))))),
                         selectInput("Anno_Analizado","Año a analizar:",c("Seleccione un año:",unique(as.character(annos)))),
                         dateRangeInput('Fechas_Seleccionadas',label = 'Filtrar entre fecha de accidentes.',start = min(as.Date(Accidentalidad$f_accidente)) , end = max(as.Date(Accidentalidad$f_accidente))),
                         sliderInput("Horas_Seleccionada", "Horas Accidentes", 0, 24, value = c(0, 24),sep = "")
               ),
               # Establecer los botones de control para los analisis especiales del aplicativo
               wellPanel(style = "background: #F0F8FF",
                         radioButtons("Condicion","Seleccione la condicion especial a analizar:",
                                      choices = c("Ninguna." = "N", "Analizar por mes." = "M","No considerar festivos." = "F","Vacaciones final año (15 Dic - 15 Ene)." = "VF","Vacaciones mitad año (15 Jun - 15 Jul)." = "VM"),selected = "N")
               )
        ),
        
        # Establecer la barra de navegacion en donde se encuentran todas las acciones de salida de los analisis realizados
        # con base en los filtros seleccionados y las consideraciones especificadas.
        mainPanel(
          # Barra de navegacion con distintas pestañas para el analisis de la informacion.
          navbarPage("Navegacion",
            # Indicacion de la tabla de navegacion donde se visualiza la informacion agrupada por numero de accidentes, comunas y años de analisis.
            tabPanel("Accidentalidad Agrupada.",
              column(width = 12,
                fluidRow( 
                  wellPanel(style = "background: #F0F8FF",
                    h5(span("Análisis de la accidentalidad en el Municipio de Medellín", style = "font-weight: 600")),
                    helpText("Descripción: En esta pestaña se puede visualizar el número de accidentes ocurridos en todo el periodo de análisis, donde se realiza una separación de estos por comunas y una diferenciación según el año en el que ocurren los accidentes. Se pueden considerar condiciones especiales como: 1. Analizar la información agrupando por mes, 2. No considerar los días festivos en el horizonte de análisis, 3. No considerar las vacaciones de mitad o fin de año en el horizonte de análisis.")
                  ),
                  wellPanel(style = "background: #F0F8FF",
                    plotOutput(outputId = "Grafico4", height = "400px")
                  )
                )
              )
            ),
            # Pestaña del analisis semanal y la ubicacion espacial de los accidentes
            tabPanel("Analisis Semanal - Ubicacion Espacial Accidentes",
              column(width = 12,
                fluidRow(
                  wellPanel(style = "background: #F0F8FF",
                    h5(span("Análisis de la accidentalidad en el Municipio de Medellín", style = "font-weight: 600")),
                    helpText("Descripción: En esta pestaña se puede analizar la accidentalidad en la ciudad de Medellín según el día de la semana, en los diferentes años de análisis. Se pueden considerar condiciones especiales como: 1. Analizar la información agrupando por mes, 2. No considerar los días festivos en el horizonte de análisis, 3. No considerar las vacaciones de mitad o fin de año en el horizonte de análisis. Además, se puede observar la ubicación espacial donde han ocurrido los accidentes según la comuna y barrio escogido para el análisis.")
                  )
                ),
                wellPanel(style = "background: #F0F8FF",
                  fluidRow(
                    column(width = 6, plotOutput(outputId = "Grafico", height = "400px")
                   ),
                   column(width = 6, leafletOutput("Mapa",height = "400px")
                   )
                  )
                )
              )
            ),
            # Pestaña de las caracteristicas de la accidentalidad
            tabPanel("Caracteristicas de la Accidentalidad",
              column(width = 12,
                fluidRow(
                 wellPanel(style = "background: #F0F8FF",
                   h5(span("Análisis de la accidentalidad en el Municipio de Medellín", style = "font-weight: 600")),
                   helpText("Descripción: En esta pestaña se puede observar el tipo y la gravedad de los accidentes que han ocurrido en todo el periodo de análisis. Se pueden considerar condiciones especiales como: 1. Analizar la información agrupando por mes, 2. No considerar los días festivos en el horizonte de análisis, 3. No considerar las vacaciones de mitad o fin de año en el horizonte de análisis.")
                 )
                ),
                wellPanel(style = "background: #F0F8FF",
                  fluidRow(
                   column(width = 6, plotOutput(outputId = "Grafico2", height = "400px")
                   ),
                   column(width = 6, plotOutput(outputId = "Grafico3", height = "400px")
                   )
                  )
                )
              )
            ),
            # Pestaña del resumen de datos.
            tabPanel("Resumen Tabla de Datos",
                column(width = 12,
                 fluidRow(
                     wellPanel(style = "background: #F0F8FF",
                         h5(span("Análisis de la accidentalidad en el Municipio de Medellín", style = "font-weight: 600")),
                         helpText("Descripción: En esta pestaña se puede visualizar la información gráfica de las pestañas precedentes de forma tabulada, según los filtros aplicados. Se pueden considerar condiciones especiales como: 1. Analizar la información agrupando por mes, 2. No considerar los días festivos en el horizonte de análisis, 3. No considerar las vacaciones de mitad o fin de año en el horizonte de análisis.")
                     )
                 ),
                 wellPanel(style = "background: #F0F8FF",
                     fluidRow(
                         column(width = 12,
                             DT::dataTableOutput("table")
                         )
                     )
                 )
                )
            )
          )
        )
    ),
    
    # Indicar la base de datos de donde se toma la informacion inicial, y las especificaciones del aplicativo.
    h3(span("La base de datos de la accidentalidad se toma de acuerdo con los datos de: https://geomedellin-m-medellin.opendata.arcgis.com/search?tags=movilidad", style = "font-weight: 600"), 
       style = "font-family: 'Source Sans Pro';
        color: #000000; text-align: center;
        background-color:#ADD8E6;
        padding: 35px"),
    
    h4(span("Realizado por: Alejandra Graciano, Julián Muñoz, Sergio Valencia", style = "font-weight: 200"), 
       style = "font-family: 'Source Sans Pro';
        color: #000000; text-align: center;
        background-color:#87CEFA;
        padding: 35px"),
    br()
)

# Define server logic required
server <- function(input, output, session) {
    
    # Filtrar informacion con base en los filtros de manera dinamica, cuando se actualicen los filtros la informacion mostrada se actualiza automaticamente
    observe({
        
        # Renombrar la bases de datos temporal para el manejo.
        data <- Accidentalidad
        
        # Indicar si se ha seleccionado un mes de analizar, y filtrar la BD.
        if (input$Mes_Analizado != "Seleccione un mes:") {
            data <- data[data$MES == input$Mes_Analizado,]
        }
        
        # Indicar si se ha seleccionado un mes de analisis y filtrar el horizonte de analisis.
        if (input$Anno_Analizado != "Seleccione un año:") {
            data <- data[data$PERIODO == input$Anno_Analizado,]
            
            updateDateRangeInput(session, "Fechas_Seleccionadas", 
                                 label = "Filtrar entre fecha de accidentes.", 
                                 start = min(as.Date(data$f_accidente)),
                                 end = max(as.Date(data$f_accidente)))
        }
        
        # Indicar si no existe los filtros principales, generar el filtro por rango
        if (input$Anno_Analizado == "Seleccione un año:" & input$Mes_Analizado == "Seleccione un mes:") {
            dateRangeInput('Fechas_Seleccionadas',
                           label = 'Filtrar entre fecha de accidentes.',
                           start = min(as.Date(Accidentalidad$f_accidente)) , end = max(as.Date(Accidentalidad$f_accidente)))
        }
        
        
        # indicar los barrios de la comuna seleccionada.
        if (input$Comuna_Seleccionada != "Seleccione una comuna:" & input$Barrio_Seleccionado == "Seleccione un barrio:") {
            
            data <- data[data$COMUNA == input$Comuna_Seleccionada,]
            
            updateSelectInput(session,"Barrio_Seleccionado",
                              "Barrio a analizar:",
                              c("Seleccione un barrio:", unique(as.character(data$BARRIO))),
                              selected = NULL)
            
        # Volver a las condiciones iniciales de la BD cuando no se tiene ni una columna seleccionada ni un barrio seleccionado
        } else if (input$Comuna_Seleccionada == "Seleccione una comuna:" & input$Barrio_Seleccionado == "Seleccione un barrio:") {
            
            updateSelectInput(session,"Barrio_Seleccionado",
                              "Barrio a analizar:",
                              c("Seleccione un barrio:", unique(as.character(Accidentalidad$BARRIO))),
                              selected = NULL)
        
        # Cuando no se selecciona una comuna, indicar todos los barrios que se pueden analizar
        } else if (input$Comuna_Seleccionada == "Seleccione una comuna:" & input$Barrio_Seleccionado != "Seleccione un barrio:") {
            
            updateSelectInput(session,"Barrio_Seleccionado",
                              "Barrio a analizar:",
                              c(input$Barrio_Seleccionado, unique(as.character(Accidentalidad$BARRIO))),
                              selected = NULL)
        }
        
    })
    
    
    # Funcion para indicar la informacion de la base de datos en representacion de tabla con base en los filtros analizados, en la pestaña tabal de datos
    output$table <- DT::renderDataTable(DT::datatable({
        
        # Renombrar la bases de datos temporal para el manejo.
        data <- Accidentalidad
        
        # Analisis y filtrado de la BD con base en las condiciones especiales
        # Si se tiene selecionada ninguna como condicion especial no filtrar la BD
        if (input$Condicion == "N") {
        
        # Si se tiene seleccionada la condicion especial festivo, filtrar la BD
        } else if (input$Condicion == "F") {
            data <- data[(data$Festivo == "No Festivo"),]
         
        # Si se tiene seleccionada la condicion especial vacaciones fin de año, filtrar la BD
        } else if (input$Condicion == "VF") {
            data <- data[(data$MES == 12 & data$DIA >= 15 & data$DIA <= 31 | data$MES == 1 & data$DIA >= 1 & data$DIA <= 15),]
        
        # Si se tiene seleccionada la condicion especial vacaciones mitad de año, filtrar la BD    
        } else if (input$Condicion == "VM") {
            data <- data[(data$MES == 6 & data$DIA >= 15 & data$DIA <= 31 | data$MES == 7 & data$DIA >= 1 & data$DIA <= 15),]
            
        }
        
        # Filtrar la BD si se tiene una columna seleccionada
        if (input$Comuna_Seleccionada != "Seleccione una comuna:") {
            data <- data[data$COMUNA == input$Comuna_Seleccionada,]
        }
        
        if (input$Barrio_Seleccionado != "Seleccione un barrio:") {
            data <- data[data$BARRIO == input$Barrio_Seleccionado,]
        }
        
        # Indicar el rango inicial de fechas de la BD total
        Fecha_Inicial <- as.Date(min(input$Fechas_Seleccionadas), origin = "1970-01-01")
        Fecha_Final <- as.Date(max(input$Fechas_Seleccionadas), origin = "1970-01-01")
        
        # Si no se tiene seleccionado la condiciones especial de analizar por mes, filtrar la BD
        if (input$Condicion != "M") {
            data <- data[(as.Date(data$f_accidente) >= Fecha_Inicial & as.Date(data$f_accidente) <= Fecha_Final),]
        
        # Si se tiene seleccionado la condicion especial de analizar por mes, verificar si se tiene seleccionado un año o no, para filtrar la BD.
        } else if (input$Condicion == "M") {
            # Si se tiene seleccionado un año de analisis especifico
            if (input$Anno_Analizado != "Seleccione un año:") {
                data <- data[(data$PERIODO == input$Anno_Analizado),]
            } else {
                
            }
            
            # Si se tiene seleccionado un mes de analisis especifico
            if (input$Mes_Analizado != "Seleccione un mes:") {
                data <- data[(data$MES == input$Mes_Analizado),]
            } else {
                
            }
            
        }
        
        # Indicar las horas seleccionadas y filtrar la BD
        Hora_Inicial <- input$Horas_Seleccionada[1]
        Hora_Final <- input$Horas_Seleccionada[2]
        data <- data[(as.numeric(data$HORA2) >= Hora_Inicial & as.numeric(data$HORA2) <= Hora_Final),]
        
        # mejorar la informacion que se va a visualizar en la pestaña de tabla de datos.
        data$Longitud <- substr(data$Longitud, 1, 10)
        data$Latitud <- substr(data$Latitud, 1, 10)
        data <- subset(data,select = c("f_accidente","BARRIO","COMUNA","DIRECCION","CLASE","GRAVEDAD","DIA_NOMBRE","HORA","Indicacion","Latitud","Longitud"))
        names(data) <- c("FECHA","BARRIO","COMUNA","DIRECCION","CLASE","GRAVEDAD","DIA","HORA","AM/PM","LATITUD","LONGITUD")
        
        data
    }))
    
    
    # Dibujar el grafico de barra de la pestaña informacion agrupada.
    output$Grafico <- renderPlot({
        
        # Renombrar la bases de datos temporal para el manejo.
        data <- Accidentalidad
        
        # Analisis y filtrado de la BD con base en las condiciones especiales
        # Si se tiene selecionada ninguna como condicion especial no filtrar la BD
        if (input$Condicion == "N") {

        # Si se tiene seleccionada la condicion especial festivo, filtrar la BD
        } else if (input$Condicion == "F") {
            data <- data[(data$Festivo == "No Festivo"),]
            
        # Si se tiene seleccionada la condicion especial vacaciones fin de año, filtrar la BD
        } else if (input$Condicion == "VF") {
            data <- data[(data$MES == 12 & data$DIA >= 15 & data$DIA <= 31 | data$MES == 1 & data$DIA >= 1 & data$DIA <= 15),]
            
        # Si se tiene seleccionada la condicion especial vacaciones mitad de año, filtrar la BD
        } else if (input$Condicion == "VM") {
            data <- data[(data$MES == 6 & data$DIA >= 15 & data$DIA <= 31 | data$MES == 7 & data$DIA >= 1 & data$DIA <= 15),]
            
        }
        
        # Filtrar la BD si se tiene una columna seleccionada
        if (input$Comuna_Seleccionada != "Seleccione una comuna:") {
            data <- data[data$COMUNA == input$Comuna_Seleccionada,]
        }
        
        # Filtrar la BD si se tiene un barrio seleccionado
        if (input$Barrio_Seleccionado != "Seleccione un barrio:") {
            data <- data[data$BARRIO == input$Barrio_Seleccionado,]
        }
        
        # Indicar el rango inicial de fechas de la BD total
        Fecha_Inicial <- as.Date(min(input$Fechas_Seleccionadas), origin = "1970-01-01")
        Fecha_Final <- as.Date(max(input$Fechas_Seleccionadas), origin = "1970-01-01")
        
        # Si se tiene seleccionado la condicion especial de analizar por mes, verificar si se tiene seleccionado un año o no, para filtrar la BD.
        if (input$Condicion != "M") {
            data <- data[(as.Date(data$f_accidente) >= Fecha_Inicial & as.Date(data$f_accidente) <= Fecha_Final),]
            
        } else if (input$Condicion == "M") {
            # Si se tiene seleccionado un año de analisis especifico
            if (input$Anno_Analizado != "Seleccione un año:") {
                data <- data[(data$PERIODO == input$Anno_Analizado),]
            } else {
                
            }
            
            # Si se tiene seleccionado un mes de analisis especifico
            if (input$Mes_Analizado != "Seleccione un mes:") {
                data <- data[(data$MES == input$Mes_Analizado),]
            } else {
                
            }
            
        }
        
        
        # Filtrar la informacion con base en el rango de horas seleccionadas.
        Hora_Inicial <- input$Horas_Seleccionada[1]
        Hora_Final <- input$Horas_Seleccionada[2]
        data <- data[(as.numeric(data$HORA2) >= Hora_Inicial & as.numeric(data$HORA2) <= Hora_Final),]
        
        # Realizar el grafico con base en los filtros seleccionados, ya se en el periodo mensual o anual.
        # Si se tiene seleccionado la condicion especial de analizar por mes.
        if (input$Condicion == "M") {
            Conteo <- aggregate(X~PERIODO+MES,data=data,FUN="length")
            Conteo2 <- table(data$MES,data$PERIODO)
            limite_y = max(Conteo$X)
            barplot(Conteo2,beside=TRUE, las = 1, main = "Accidentalidad Semanal vs Periodo Analizado.",
                    xlab = "Periodo anual.", ylab = "Numero de accidentes.", ylim = c(0,limite_y*1.1),
                    legend.text = unique(Conteo$MES), args.legend=list(x = "topright", bty="n"))
        # si no se tiene seleccionado la condicion especial de analizar por mes.
        } else {
            Conteo <- aggregate(X~PERIODO+DIA_NOMBRE,data=data,FUN="length")
            Conteo2 <- table(data$DIA_NOMBRE,data$PERIODO)
            limite_y = max(Conteo$X)
            barplot(Conteo2,beside=TRUE, las = 1, main = "Accidentalidad Semanal vs Periodo Analizado.",
                    xlab = "Periodo anual.", ylab = "Numero de accidentes.", ylim = c(0,limite_y*1.1),
                    legend.text = unique(Conteo$DIA_NOMBRE), args.legend=list(x = "topright", bty="n"))
        }
        
        
        
    })
    
    # Dibujar el grafico de barra de la pestaña discriminacion semanal - ubicacion espacial
    output$Grafico2 <- renderPlot({
        
        # Renombrar la bases de datos temporal para el manejo.
        data <- Accidentalidad
        
        # Analisis y filtrado de la BD con base en las condiciones especiales
        # Si se tiene selecionada ninguna como condicion especial no filtrar la BD
        if (input$Condicion == "N") {
        
        # Si se tiene seleccionada la condicion especial festivo, filtrar la BD
        } else if (input$Condicion == "F") {
            data <- data[(data$Festivo == "No Festivo"),]
        
        # Si se tiene seleccionada la condicion especial vacaciones fin de año, filtrar la BD    
        } else if (input$Condicion == "VF") {
            data <- data[(data$MES == 12 & data$DIA >= 15 & data$DIA <= 31 | data$MES == 1 & data$DIA >= 1 & data$DIA <= 15),]
            
        # Si se tiene seleccionada la condicion especial vacaciones mitad de año, filtrar la BD
        } else if (input$Condicion == "VM") {
            data <- data[(data$MES == 6 & data$DIA >= 15 & data$DIA <= 31 | data$MES == 7 & data$DIA >= 1 & data$DIA <= 15),]
            
        }
        
        # Filtrar la BD si se tiene una columna seleccionada
        if (input$Comuna_Seleccionada != "Seleccione una comuna:") {
            data <- data[data$COMUNA == input$Comuna_Seleccionada,]
        }
        
        # Filtrar la BD si se tiene un barrio seleccionado
        if (input$Barrio_Seleccionado != "Seleccione un barrio:") {
            data <- data[data$BARRIO == input$Barrio_Seleccionado,]
        }
        
        # Indicar el rango de fechas seleccionadas.
        Fecha_Inicial <- as.Date(min(input$Fechas_Seleccionadas), origin = "1970-01-01")
        Fecha_Final <- as.Date(max(input$Fechas_Seleccionadas), origin = "1970-01-01")
        
        # Si se tiene seleccionado la condicion especial de analizar por mes, verificar si se tiene seleccionado un año o no, para filtrar la BD.
        if (input$Condicion != "M") {
            data <- data[(as.Date(data$f_accidente) >= Fecha_Inicial & as.Date(data$f_accidente) <= Fecha_Final),]
            
        } else if (input$Condicion == "M") {
            # Si se tiene seleccionado un año de analisis especifico
            if (input$Anno_Analizado != "Seleccione un año:") {
                data <- data[(data$PERIODO == input$Anno_Analizado),]
            } else {
                
            }
            
            # Si no se tiene seleccionado un mes de analisis especifico
            if (input$Mes_Analizado != "Seleccione un mes:") {
                data <- data[(data$MES == input$Mes_Analizado),]
            } else {
                
            }
            
        }
        
        # Filtrar la informacion con base en el rango de horas seleccionado
        Hora_Inicial <- input$Horas_Seleccionada[1]
        Hora_Final <- input$Horas_Seleccionada[2]
        data <- data[(as.numeric(data$HORA2) >= Hora_Inicial & as.numeric(data$HORA2) <= Hora_Final),]
        
        # Realizar el grafico con base en los filtros seleccionados, ya se en el periodo mensual o anual.
        # Si se tiene seleccionado la condicion especial de analizar por mes.
        if (input$Condicion == "M") {
            Conteo <- aggregate(X~MES+CLASE,data=data,FUN="length")
            Conteo2 <- table(data$CLASE,data$MES)
            limite_y = max(Conteo$X)
            barplot(Conteo2,beside=TRUE, las = 1, main = "Accidentalidad por Tipo vs Periodo Analizado.",
                    xlab = "Periodo mensual.", ylab = "Numero de accidentes.", ylim = c(0,limite_y*1.1),
                    legend.text = unique(Conteo$CLASE), args.legend=list(x = "topright", bty="n"))
        
        # si no se tiene seleccionado la condicion especial de analizar por mes.
        } else {
            Conteo <- aggregate(X~PERIODO+CLASE,data=data,FUN="length")
            Conteo2 <- table(data$CLASE,data$PERIODO)
            limite_y = max(Conteo$X)
            barplot(Conteo2,beside=TRUE, las = 1, main = "Accidentalidad por Tipo vs Periodo Analizado.",
                    xlab = "Periodo anual.", ylab = "Numero de accidentes.", ylim = c(0,limite_y*1.1),
                    legend.text = unique(Conteo$CLASE), args.legend=list(x = "topright", bty="n"))
        }
        
    })
    
    # Dibujar el grafico de barra del tipo de accidente de la pestaña caracteristicas de los accidentes
    output$Grafico3 <- renderPlot({
        
        # Renombrar la bases de datos temporal para el manejo.
        data <- Accidentalidad
        
        # Analisis y filtrado de la BD con base en las condiciones especiales
        # Si se tiene selecionada ninguna como condicion especial no filtrar la BD
        if (input$Condicion == "N") {
            
        # Si se tiene seleccionada la condicion especial festivo, filtrar la BD
        } else if (input$Condicion == "F") {
            data <- data[(data$Festivo == "No Festivo"),]
            
        # Si se tiene seleccionada la condicion especial vacaciones fin de año, filtrar la BD
        } else if (input$Condicion == "VF") {
            data <- data[(data$MES == 12 & data$DIA >= 15 & data$DIA <= 31 | data$MES == 1 & data$DIA >= 1 & data$DIA <= 15),]
            
        # Si se tiene seleccionada la condicion especial vacaciones mitad de año, filtrar la BD
        } else if (input$Condicion == "VM") {
            data <- data[(data$MES == 6 & data$DIA >= 15 & data$DIA <= 31 | data$MES == 7 & data$DIA >= 1 & data$DIA <= 15),]
            
        }
        
        # Filtrar la BD si se tiene una columna seleccionada
        if (input$Comuna_Seleccionada != "Seleccione una comuna:") {
            data <- data[data$COMUNA == input$Comuna_Seleccionada,]
        }
        
        # Filtrar la BD si se tiene un barrio seleccionado
        if (input$Barrio_Seleccionado != "Seleccione un barrio:") {
            data <- data[data$BARRIO == input$Barrio_Seleccionado,]
        }
        
        # Filtrar la informacion con base en el rango de fechas seleccionadas
        Fecha_Inicial <- as.Date(min(input$Fechas_Seleccionadas), origin = "1970-01-01")
        Fecha_Final <- as.Date(max(input$Fechas_Seleccionadas), origin = "1970-01-01")
        
        # Si se tiene seleccionado la condicion especial de analizar por mes, verificar si se tiene seleccionado un año o no, para filtrar la BD.
        if (input$Condicion != "M") {
            data <- data[(as.Date(data$f_accidente) >= Fecha_Inicial & as.Date(data$f_accidente) <= Fecha_Final),]
            
        } else if (input$Condicion == "M") {
            # Si se tiene seleccionado un año de analisis especifico
            if (input$Anno_Analizado != "Seleccione un año:") {
                data <- data[(data$PERIODO == input$Anno_Analizado),]
            } else {
                
            }
            
            # Si se tiene seleccionado un mes de analisis especifico
            if (input$Mes_Analizado != "Seleccione un mes:") {
                data <- data[(data$MES == input$Mes_Analizado),]
            } else {
                
            }
            
        }
        
        # Filtrar la infomracion con base en el rango de horas seleccionadas
        Hora_Inicial <- input$Horas_Seleccionada[1]
        Hora_Final <- input$Horas_Seleccionada[2]
        data <- data[(as.numeric(data$HORA2) >= Hora_Inicial & as.numeric(data$HORA2) <= Hora_Final),]
        
        # Realizar el grafico con base en los filtros seleccionados, ya se en el periodo mensual o anual.
        # Si se tiene seleccionado la condicion especial de analizar por mes.
        if (input$Condicion == "M") {
            Conteo <- aggregate(X~MES+GRAVEDAD,data=data,FUN="length")
            Conteo2 <- table(data$GRAVEDAD,data$MES)
            limite_y = max(Conteo$X)
            barplot(Conteo2,beside=TRUE, las = 1, main = "Gravedad del Accidente vs Periodo Analizado.",
                    xlab = "Periodo mensual.", ylab = "Numero de accidentes.", ylim = c(0,limite_y*1.1),
                    legend.text = unique(Conteo$GRAVEDAD), args.legend=list(x = "topright", bty="n"))
          
        # si no se tiene seleccionado la condicion especial de analizar por mes.  
        } else {
            Conteo <- aggregate(X~PERIODO+GRAVEDAD,data=data,FUN="length")
            Conteo2 <- table(data$GRAVEDAD,data$PERIODO)
            limite_y = max(Conteo$X)
            barplot(Conteo2,beside=TRUE, las = 1, main = "Gravedad del Accidente vs Periodo Analizado.",
                    xlab = "Periodo anual.", ylab = "Numero de accidentes.", ylim = c(0,limite_y*1.1),
                    legend.text = unique(Conteo$GRAVEDAD), args.legend=list(x = "topright", bty="n"))
        }
        
    })
    
    # Dibujar el grafico de barra de la gravedad del accidente de la pestaña caracteristicas de los accidentes
    output$Grafico4 <- renderPlot({
        
        # Renombrar la bases de datos temporal para el manejo.
        data <- Accidentalidad
        
        # Analisis y filtrado de la BD con base en las condiciones especiales
        # Si se tiene selecionada ninguna como condicion especial no filtrar la BD
        if (input$Condicion == "N") {
            
        # Si se tiene seleccionada la condicion especial festivo, filtrar la BD
        } else if (input$Condicion == "F") {
            data <- data[(data$Festivo == "No Festivo"),]
            
        # Si se tiene seleccionada la condicion especial vacaciones fin de año, filtrar la BD
        } else if (input$Condicion == "VF") {
            data <- data[(data$MES == 12 & data$DIA >= 15 & data$DIA <= 31 | data$MES == 1 & data$DIA >= 1 & data$DIA <= 15),]
            
        # Si se tiene seleccionada la condicion especial vacaciones mitad de año, filtrar la BD
        } else if (input$Condicion == "VM") {
            data <- data[(data$MES == 6 & data$DIA >= 15 & data$DIA <= 31 | data$MES == 7 & data$DIA >= 1 & data$DIA <= 15),]
            
        }
        
        # Filtrar la informacion con base en el rango de fechas seleccionado
        Fecha_Inicial <- as.Date(min(input$Fechas_Seleccionadas), origin = "1970-01-01")
        Fecha_Final <- as.Date(max(input$Fechas_Seleccionadas), origin = "1970-01-01")
        
        # Si se tiene seleccionado la condicion especial de analizar por mes, verificar si se tiene seleccionado un año o no, para filtrar la BD.
        if (input$Condicion != "M") {
            data <- data[(as.Date(data$f_accidente) >= Fecha_Inicial & as.Date(data$f_accidente) <= Fecha_Final),]
            
        } else if (input$Condicion == "M") {
            # Si se tiene seleccionado un año de analisis especifico
            if (input$Anno_Analizado != "Seleccione un año:") {
                data <- data[(data$PERIODO == input$Anno_Analizado),]
            } else {
                
            }
            
            # Si se tiene seleccionado un mes de analisis especifico
            if (input$Mes_Analizado != "Seleccione un mes:") {
                data <- data[(data$MES == input$Mes_Analizado),]
            } else {
                
            }
            
        }
        
        # Filtrar la informacion con base en el rango de fechas seleccionado
        Hora_Inicial <- input$Horas_Seleccionada[1]
        Hora_Final <- input$Horas_Seleccionada[2]
        data <- data[(as.numeric(data$HORA2) >= Hora_Inicial & as.numeric(data$HORA2) <= Hora_Final),]
        
        # Realizar el grafico con base en los filtros seleccionados, ya se en el periodo mensual o anual.
        # Si se tiene seleccionado la condicion especial de analizar por mes.
        if (input$Condicion == "M") {
            Conteo <- aggregate(X~COMUNA,data=data,FUN="length")
            Conteo3 <- aggregate(X~COMUNA+MES,data=data,FUN="length")
            Conteo2 <- table(data$MES,data$COMUNA)
            limite_y = max(Conteo$X)
            par(mar=c(10, 6, 3, 1))
            barplot(Conteo2, las = 2, main = "Numero de accidentes vs Comunas.",
                    ylab = "Numero de accidentes.", ylim = c(0,limite_y*1.1),
                    legend.text = row.names(Conteo2), args.legend=list(x = "topright", bty="n"))
            
        # si no se tiene seleccionado la condicion especial de analizar por mes. 
        } else {
            Conteo <- aggregate(X~COMUNA,data=data,FUN="length")
            Conteo3 <- aggregate(X~COMUNA+PERIODO,data=data,FUN="length")
            Conteo2 <- table(data$PERIODO,data$COMUNA)
            limite_y = max(Conteo$X)
            par(mar=c(10, 6, 3, 1))
            barplot(Conteo2, las = 2, main = "Numero de accidentes vs Comunas.",
                    ylab = "Numero de accidentes.", ylim = c(0,limite_y*1.1),
                    legend.text = row.names(Conteo2), args.legend=list(x = "topright", bty="n"))
        }
        
    })
    
    
    # Dibujar el mapa de la ubicacion del accidente de la pestaña discriminacion semanal - ubicacion espacial
    output$Mapa <- renderLeaflet({
        
        # Renombrar la bases de datos temporal para el manejo.
        data <- Accidentalidad
        
        # Analisis y filtrado de la BD con base en las condiciones especiales
        # Si se tiene selecionada ninguna como condicion especial no filtrar la BD
        if (input$Condicion == "N") {
            
        # Si se tiene seleccionada la condicion especial festivo, filtrar la BD
        } else if (input$Condicion == "F") {
            data <- data[(data$Festivo == "No Festivo"),]
            
        # Si se tiene seleccionada la condicion especial vacaciones fin de año, filtrar la BD
        } else if (input$Condicion == "VF") {
            data <- data[(data$MES == 12 & data$DIA >= 15 & data$DIA <= 31 | data$MES == 1 & data$DIA >= 1 & data$DIA <= 15),]
            
        # Si se tiene seleccionada la condicion especial vacaciones mitad de año, filtrar la BD
        } else if (input$Condicion == "VM") {
            data <- data[(data$MES == 6 & data$DIA >= 15 & data$DIA <= 31 | data$MES == 7 & data$DIA >= 1 & data$DIA <= 15),]
            
        }
        
        # Filtrar la informacion con base en el rango de fechas seleccionados
        Fecha_Inicial <- as.Date(min(input$Fechas_Seleccionadas), origin = "1970-01-01")
        Fecha_Final <- as.Date(max(input$Fechas_Seleccionadas), origin = "1970-01-01")
        data <- data[(as.Date(data$f_accidente) >= Fecha_Inicial & as.Date(data$f_accidente) <= Fecha_Final),]
        
        # Filtrar la informacion con base en el rango de horas seleccionados
        Hora_Inicial <- input$Horas_Seleccionada[1]
        Hora_Final <- input$Horas_Seleccionada[2]
        data <- data[(as.numeric(data$HORA2) >= Hora_Inicial & as.numeric(data$HORA2) <= Hora_Final),]
        
        # Grafico de los accidentes teniendo en cuenta el barrio seleccionado
        if (input$Barrio_Seleccionado != "Seleccione un barrio:") {
            
            data <- data[data$BARRIO == input$Barrio_Seleccionado,]
            
            # Encontrar los valores extremos para graficar
            Longitud <- data$Longitud
            Latitud <- data$Latitud
            lng1 = min(Longitud)
            lat1 = min(Latitud)
            lng2 = max(Longitud)
            lat2 = max(Latitud)
            
            map <- leaflet()
            map <- addProviderTiles(map,provider = "OpenStreetMap.Mapnik")
            map <- fitBounds(map, 
                             lng1=lng1, 
                             lat1=lat1, 
                             lng2=lng2, 
                             lat2=lat2)
            map <- addMarkers(map,
                              lat=Latitud,
                              lng=Longitud,
                              popup=data$DIRECCION)
            map
            
        # Grafico de los accidentes si no hay nada seleccionado
        # Se evidencia el mapa con una agrupacion de los accidentes por zona para evitar la saturacion visual
        } else if (input$Comuna_Seleccionada != "Seleccione una comuna:") {
            
            data <- data[data$COMUNA == input$Comuna_Seleccionada,]
            
            # Encontrar los valores extremos para graficar
            Longitud <- data$Longitud
            Latitud <- data$Latitud
            lng1 = min(Longitud)
            lat1 = min(Latitud)
            lng2 = max(Longitud)
            lat2 = max(Latitud)
            
            map <- leaflet()
            map <- addProviderTiles(map,provider = "OpenStreetMap.Mapnik")
            map <- fitBounds(map, 
                             lng1=lng1, 
                             lat1=lat1, 
                             lng2=lng2, 
                             lat2=lat2)
            map <- addMarkers(map,
                              lat=Latitud,
                              lng=Longitud,
                              popup=data$DIRECCION,
                              clusterOptions=markerClusterOptions())
            map
            
        # Grafico de los accidentes si no hay nada seleccionado
        # Se evidencia el mapa con una agrupacion de los accidentes por zona para evitar la saturacion visual
        } else if (input$Comuna_Seleccionada == "Seleccione una comuna:" & input$Barrio_Seleccionado == "Seleccione un barrio:") {
            
            data <- data
            
            # Encontrar los valores extremos para graficar
            Longitud <- data$Longitud
            Latitud <- data$Latitud
            lng1 = min(Longitud)
            lat1 = min(Latitud)
            lng2 = max(Longitud)
            lat2 = max(Latitud)
            
            map <- leaflet()
            map <- addProviderTiles(map,provider = "OpenStreetMap.Mapnik")
            map <- fitBounds(map, 
                             lng1=lng1, 
                             lat1=lat1, 
                             lng2=lng2, 
                             lat2=lat2)
            map <- addMarkers(map,
                              lat=Latitud,
                              lng=Longitud,
                              popup=data$DIRECCION,
                              clusterOptions=markerClusterOptions())
            map
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
