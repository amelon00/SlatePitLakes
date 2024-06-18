library(readxl)
library(shiny)
library(leaflet)
library(DT)
library(shinythemes)
library(leaflet.extras)
Lagunas_pizarraNW_2024_junio <- read_excel("C:/R/Slate_pit_lakes_R/Slate_pit_lakes_2024//R_Lagunas_pizarraNW_2024_junio.xlsx")
Lagunas_pizarraNW_2024_junio$imagen <- paste0("C:/R/Slate_pit_lakes_R/Slate_pit_lakes_2024/www/", (Lagunas_pizarraNW_2024_junio$ID), ".jpg")
Lagunas_pizarraNW_2024_junio$imagen


ui <- fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel("Slate pit lakes - Northwest Spain"),
  fluidRow(
    column(12, leafletOutput("mapa", height = "500px")),
    column(12, DTOutput("tabla_datos"))
  ),
  fluidRow(
    column(12, p("Source: GEOPAT (Grupo de Investigación Geomorfología, Paisaje y Territorio, Universidad de León). Corresponding author: José María Redondo-Vega (jmredv@unileon.es)"))
  )
)



server2 <- function(input, output, session) {
  
  # Creación del mapa
  output$mapa <- renderLeaflet({
    leaflet(data = Lagunas_pizarraNW_2024_junio) %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satélite") %>%
      addProviderTiles(providers$Esri.WorldTopoMap, group = "Topográfico") %>%
      addLayersControl(
        baseGroups = c("Satélite", "Topográfico"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      addMiniMap(tiles = providers$Esri.WorldStreetMap, toggleDisplay = TRUE) %>%
      addMeasure(primaryLengthUnit = "meters", primaryAreaUnit = "sqmeters") %>%
      addScaleBar(position = "bottomleft") %>%
      addCircleMarkers(lng = ~Longitude, lat = ~Latitude,
                       popup = ~paste("ID:", ID, "<br>",
                                      "", Place, "<br>"),
                                      #"<img src='", imagen, "' width='100'>"),
                       color = "red", fillColor = "red", radius = 6,
                       layerId = ~ID)  # Añadir layerId para identificar los marcadores
  })
  
  # Creación de la tabla dinámica
  output$tabla_datos <- renderDT({
    datatable(Lagunas_pizarraNW_2024_junio,
              selection = 'single',  # Permitir selección de una sola fila
              options = list(pageLength = 27, autoWidth = TRUE))
  })
  
  # Observador para detectar clics en los marcadores del mapa
  observeEvent(input$mapa_marker_click, {
    click <- input$mapa_marker_click
    selected_code <- click$id
    
    # Encontrar el índice de la fila correspondiente en la tabla
    selected_row <- which(Lagunas_pizarraNW_2024_junio$ID == selected_code)
    
    # Seleccionar la fila correspondiente en la tabla
    proxy <- dataTableProxy("tabla_datos")
    selectRows(proxy, selected_row)
  })
}



# Ejecutar la aplicación
shinyApp(ui = ui, server = server2)

