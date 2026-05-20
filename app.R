# =========================================================
# DASHBOARD POBREZA GLOBAL - TODO EN UN SOLO ARCHIVO
# app.R
# =========================================================

# =========================
# INSTALAR PAQUETES
# =========================
# Ejecutar SOLO la primera vez

install.packages("shiny")
install.packages("shinydashboard")
install.packages("shinyWidgets")
install.packages("plotly")
install.packages("DT")
install.packages("dplyr")
install.packages("leaflet")
install.packages("ggplot2")
install.packages("rsconnect")

installed <- packages %in% installed.packages()

if(any(!installed)){
  install.packages(packages[!installed])
}

# =========================
# LIBRERÍAS
# =========================

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(DT)
library(dplyr)
library(leaflet)
library(ggplot2)
library(rsconnect)

# =========================
# DATOS
# =========================

pobreza_data <- data.frame(
  pais = c(
    "China",
    "Nigeria",
    "India",
    "Brasil",
    "Perú",
    "Sudáfrica",
    "Indonesia"
  ),
  
  pib = c(
    13000,
    2400,
    2600,
    9800,
    7200,
    6500,
    4800
  ),
  
  pobreza = c(
    0.1,
    42,
    12,
    5,
    20,
    27,
    11
  ),
  
  poblacion = c(
    1400,
    223,
    1420,
    215,
    34,
    60,
    280
  ),
  
  region = c(
    "Asia",
    "África",
    "Asia",
    "América Latina",
    "América Latina",
    "África",
    "Asia"
  )
)

# =========================
# UI
# =========================

ui <- dashboardPage(
  
  skin = "blue",
  
  dashboardHeader(
    title = "Pobreza Global"
  ),
  
  dashboardSidebar(
    
    sidebarMenu(
      
      menuItem(
        "Resumen",
        tabName = "resumen",
        icon = icon("dashboard")
      ),
      
      menuItem(
        "Tendencias",
        tabName = "tendencias",
        icon = icon("chart-line")
      ),
      
      menuItem(
        "Mapas",
        tabName = "mapas",
        icon = icon("globe")
      ),
      
      menuItem(
        "Impacto",
        tabName = "impacto",
        icon = icon("chart-area")
      )
    ),
    
    br(),
    
    pickerInput(
      inputId = "region",
      label = "Selecciona Región:",
      choices = c("Mundo", unique(pobreza_data$region)),
      selected = "Mundo"
    ),
    
    sliderInput(
      inputId = "year",
      label = "Año:",
      min = 1800,
      max = 2024,
      value = 2024,
      sep = ""
    )
  ),
  
  dashboardBody(
    
    tags$head(
      tags$style(HTML("
      
        .content-wrapper,
        .right-side {
          background-color: #0b1326;
        }

        .main-header .logo {
          background-color: #111827 !important;
          color: white !important;
          font-weight: bold;
        }

        .main-header .navbar {
          background-color: #111827 !important;
        }

        .main-sidebar {
          background-color: #111827 !important;
        }

        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
          background-color: #2563eb !important;
        }

        .box {
          border-radius: 14px;
        }

        .small-box {
          border-radius: 14px;
        }

        body {
          font-family: 'Inter', sans-serif;
        }

      "))
    ),
    
    tabItems(
      
      # =================================================
      # TAB RESUMEN
      # =================================================
      
      tabItem(
        tabName = "resumen",
        
        fluidRow(
          
          valueBox(
            value = "712.4M",
            subtitle = "Población en pobreza extrema",
            icon = icon("users"),
            color = "light-blue",
            width = 4
          ),
          
          valueBox(
            value = "8.54%",
            subtitle = "Tasa global",
            icon = icon("percent"),
            color = "green",
            width = 4
          ),
          
          valueBox(
            value = "$2.15/día",
            subtitle = "Línea internacional",
            icon = icon("wallet"),
            color = "yellow",
            width = 4
          )
        ),
        
        fluidRow(
          
          box(
            title = "PIB vs Pobreza Extrema",
            width = 8,
            status = "primary",
            solidHeader = TRUE,
            
            plotlyOutput("bubblePlot", height = 500)
          ),
          
          box(
            title = "Mapa Mundial",
            width = 4,
            status = "danger",
            solidHeader = TRUE,
            
            leafletOutput("mapa", height = 500)
          )
        ),
        
        fluidRow(
          
          box(
            title = "Base de Datos",
            width = 12,
            status = "info",
            solidHeader = TRUE,
            
            DTOutput("tabla")
          )
        )
      ),
      
      # =================================================
      # TAB TENDENCIAS
      # =================================================
      
      tabItem(
        tabName = "tendencias",
        
        fluidRow(
          
          box(
            title = "Tendencia Global",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            
            plotOutput("linePlot", height = 550)
          )
        )
      ),
      
      # =================================================
      # TAB MAPAS
      # =================================================
      
      tabItem(
        tabName = "mapas",
        
        fluidRow(
          
          box(
            title = "Mapa Interactivo",
            width = 12,
            status = "success",
            solidHeader = TRUE,
            
            leafletOutput("worldMap", height = 650)
          )
        )
      ),
      
      # =================================================
      # TAB IMPACTO
      # =================================================
      
      tabItem(
        tabName = "impacto",
        
        fluidRow(
          
          box(
            title = "Estrategias de Mitigación",
            width = 12,
            status = "warning",
            solidHeader = TRUE,
            
            br(),
            
            h3("Acciones Globales"),
            
            br(),
            
            tags$ul(
              
              tags$li(
                h4("Seguridad alimentaria")
              ),
              
              tags$li(
                h4("Inversión en educación")
              ),
              
              tags$li(
                h4("Infraestructura social")
              ),
              
              tags$li(
                h4("Programas de ayuda económica")
              ),
              
              tags$li(
                h4("Desarrollo rural")
              )
            )
          )
        )
      )
    )
  )
)

# =========================
# SERVER
# =========================

server <- function(input, output) {
  
  # -----------------------
  # FILTRAR DATOS
  # -----------------------
  
  filtered_data <- reactive({
    
    if(input$region == "Mundo"){
      pobreza_data
    } else {
      pobreza_data %>%
        filter(region == input$region)
    }
  })
  
  # -----------------------
  # BUBBLE CHART
  # -----------------------
  
  output$bubblePlot <- renderPlotly({
    
    df <- filtered_data()
    
    plot_ly(
      data = df,
      
      x = ~pib,
      y = ~pobreza,
      
      size = ~poblacion,
      color = ~region,
      
      text = ~paste(
        "<b>País:</b>", pais,
        "<br><b>PIB:</b>", pib,
        "<br><b>Pobreza:</b>", pobreza, "%",
        "<br><b>Población:</b>", poblacion, "M"
      ),
      
      hoverinfo = "text",
      type = "scatter",
      mode = "markers"
      
    ) %>%
      
      layout(
        title = "Relación entre PIB y pobreza extrema",
        
        xaxis = list(
          title = "PIB per cápita"
        ),
        
        yaxis = list(
          title = "Pobreza extrema (%)"
        )
      )
  })
  
  # -----------------------
  # MAPA PEQUEÑO
  # -----------------------
  
  output$mapa <- renderLeaflet({
    
    leaflet() %>%
      
      addProviderTiles(
        providers$CartoDB.DarkMatter
      ) %>%
      
      setView(
        lng = 0,
        lat = 20,
        zoom = 2
      )
  })
  
  # -----------------------
  # TABLA
  # -----------------------
  
  output$tabla <- renderDT({
    
    datatable(
      filtered_data(),
      
      options = list(
        pageLength = 5,
        autoWidth = TRUE
      )
    )
  })
  
  # -----------------------
  # TENDENCIA
  # -----------------------
  
  output$linePlot <- renderPlot({
    
    years <- 2000:2024
    
    pobreza_values <- c(
      35,34,33,32,31,30,29,28,27,26,
      25,24,23,22,21,20,19,18,17,16,
      15,14,12,10,8
    )
    
    df <- data.frame(
      year = years,
      pobreza = pobreza_values
    )
    
    ggplot(
      df,
      aes(
        x = year,
        y = pobreza
      )
    ) +
      
      geom_line(
        linewidth = 1.5,
        color = "blue"
      ) +
      
      geom_point(
        size = 3
      ) +
      
      labs(
        title = "Reducción Global de la Pobreza",
        x = "Año",
        y = "Porcentaje (%)"
      ) +
      
      theme_minimal(base_size = 15)
  })
  
  # -----------------------
  # MAPA GRANDE
  # -----------------------
  
  output$worldMap <- renderLeaflet({
    
    leaflet() %>%
      
      addProviderTiles(
        providers$CartoDB.DarkMatter
      ) %>%
      
      setView(
        lng = 0,
        lat = 20,
        zoom = 2
      ) %>%
      
      addMarkers(
        lng = c(8, 78, -51, -75),
        lat = c(9, 21, -10, -9),
        
        popup = c(
          "Nigeria: Alta pobreza",
          "India: Media pobreza",
          "Brasil: Baja pobreza",
          "Perú: Pobreza moderada"
        )
      )
  })
}

# =========================
# EJECUTAR APP
# =========================

shinyApp(ui, server)

# =========================================================
# SUBIR A SHINYAPPS.IO
# =========================================================

# 1. Crear cuenta:
# https://www.shinyapps.io

# 2. Obtener TOKEN y SECRET

# 3. Ejecutar:

# rsconnect::setAccountInfo(
#   name='TU_USUARIO',
#   token='TU_TOKEN',
#   secret='TU_SECRET'
# )

# 4. Publicar aplicación

# rsconnect::deployApp()

# =========================================================
# GUARDAR COMO:
# app.R
# =========================================================