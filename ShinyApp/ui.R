# ui.R

shinyUI(navbarPage(title=img(src="dit.png",height = 24, width = 24),
                   
  # Pestaña "Clics geolocalizados"                 
  tabPanel("Clics geolocalizados",
    sidebarLayout(
      sidebarPanel(
        selectInput("mapClics", 
          label = "Elige una web:",
          choices = c("Total", "www.nasa.gov", "pld.dpi.wi.gov", "www.fbi.gov"),
          selected = "Total"
        ),
        sliderInput("mapNiveles", label = "Número de niveles:",
                    min = 1, max = 9, value = 1)
      ),
      mainPanel(
        plotOutput("map")
      )
    )
  ),
  
  # Pestaña "Reglas de asociación (Tabla)"
  tabPanel("Reglas de asociación (Tabla)",
    dataTableOutput("rules_table")
  ), #, añadir más pestañas
  
  # Pestaña "Reglas de asociación (Gráfico)"
  tabPanel("Reglas de asociación (Gráfico)",
    sidebarLayout(
      sidebarPanel(
        radioButtons("rules_mode",
          label = "Selecciona el tipo de gráfico:",
          choices = list("2D", "3D"),
          selected = "2D"
        ),
        uiOutput("ui")
      ),
      mainPanel(
        # Quitar errores
        tags$style(type="text/css",
        ".shiny-output-error { visibility: hidden; }",
        ".shiny-output-error:before { visibility: hidden; }"),
                   
        plotOutput("rules_plot")
      )
    )          
  )#, añadir más pestañas
  
))
