## prueba de commit 777
#### 7777777777
library(shiny)
library(ggplot2)
library(dplyr)
library(scales)

# Carga tu dataframe
##datos <- cruce_todos_v3

datos <- read.csv("cruce_todos_v3.csv", fileEncoding = "UTF-8")

##datos <- read.csv("https://mapfrecorp.sharepoint.com/:x:/r/sites/GO365EECOMPUSGLTSPE-CUMPLIMIENTO/Documentos%20compartidos/INFORMACION%20EN%20USO/JOSEPH/cruce_todos_v2.csv?d=w94978c6cd64841aa8e94d215d13d01f8&csf=1&download=1")

# En app.R:

# Limpia nombres de columnas
names(datos) <- trimws(names(datos))

# Convierte tipos
datos$ramo <- as.character(datos$ramo)
datos$numero_polizas <- as.factor(datos$numero_polizas)

ui <- fluidPage(
  titlePanel("Análisis de Primas por Filtros"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("ramo", "Selecciona el ramo:", choices = sort(unique(datos$ramo))),
      selectInput("tipo_cliente", "Selecciona el tipo de cliente:", choices = sort(unique(datos$TIPO_CLIENTE))),
      selectInput("num_polizas", "Selecciona número de pólizas:", choices = sort(unique(datos$numero_polizas)))
    ),
    
    mainPanel(
      plotOutput("boxplot"),
      br(),
      textOutput("resumen")
    )
  )
)

server <- function(input, output, session) {
  
  # Actualiza dinámicamente las opciones de número de pólizas según ramo y tipo de cliente
  observeEvent(c(input$ramo, input$tipo_cliente), {
    polizas_filtradas <- datos %>%
      filter(ramo == input$ramo, TIPO_CLIENTE == input$tipo_cliente) %>%
      pull(numero_polizas) %>%
      unique() %>%
      sort()
    
    updateSelectInput(session, "num_polizas", choices = polizas_filtradas)
  })
  
  # Datos filtrados
  data_filtrada <- reactive({
    datos %>%
      filter(ramo == input$ramo,
             TIPO_CLIENTE == input$tipo_cliente,
             numero_polizas == input$num_polizas)
  })
  
  # Renderiza el gráfico
  output$boxplot <- renderPlot({
    validate(
      need(nrow(data_filtrada()) > 0, "No hay datos para esta combinación de filtros."),
      need("PRIMA_ANUAL_SOLES" %in% names(data_filtrada()), 
           paste("Columnas disponibles:", paste(names(data_filtrada()), collapse = ", ")))
    )
    
    # Calcula Q1, Q3 e IQR
    primas <- as.numeric(data_filtrada()$PRIMA_ANUAL_SOLES)
    Q1 <- quantile(primas, 0.25, na.rm = TRUE)
    Q3 <- quantile(primas, 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    limite_superior <- Q3 + 1.5 * IQR
    
    ggplot(data_filtrada(), aes(x = numero_polizas, y = primas)) +
      geom_boxplot(fill = "steelblue", color = "black") +
      geom_hline(yintercept = limite_superior, color = "red", linetype = "dashed", size = 1) +
      annotate("text", x = 1, y = limite_superior, 
               label = paste("Límite:", scales::comma(round(limite_superior, 2))), 
               vjust = -0.5, color = "red", size = 4, fontface = "bold") +
      labs(title = "Distribución de Primas",
           subtitle = paste("Límite superior:", scales::comma(round(limite_superior, 2))),
           x = "Número de pólizas", y = "Prima Anual (Soles)") +
      scale_y_continuous(labels = scales::comma) +
      theme_minimal()
  })
  
  # Texto con totales y porcentaje
  output$resumen <- renderText({
    primas <- as.numeric(data_filtrada()$PRIMA_ANUAL_SOLES)
    Q1 <- quantile(primas, 0.25, na.rm = TRUE)
    Q3 <- quantile(primas, 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    limite_superior <- Q3 + 1.5 * IQR
    
    total_clientes <- nrow(data_filtrada())
    clientes_superan <- sum(primas > limite_superior, na.rm = TRUE)
    porcentaje <- ifelse(total_clientes > 0, round((clientes_superan / total_clientes) * 100, 2), 0)
    
    paste0("Total clientes: ", scales::comma(total_clientes),
           " | Clientes sobre el límite: ", scales::comma(clientes_superan),
           " (", porcentaje, "%)")
  })
}

shinyApp(ui = ui, server = server)

## comn
##rsconnect::deployApp(normalizePath("D:/Descargas/Pedidos Hugo/ANALISIS DE PRIMAS/10. OCTUBRE 2025/Shiny_analisis_polizas"))
###fhfhfhhfgj
