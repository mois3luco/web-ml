# ui.R

shinyUI(navbarPage(title=img(src="dit.png",height = 24, width = 24), windowTitle="Dashboard",
                   
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
  
  # Pestaña "Perfiles de acceso"
  tabPanel("Perfiles de acceso",
           sidebarLayout(
             sidebarPanel(
               selectInput("uaClics", 
                           label = "Elige una web:",
                           choices = c("Total", "www.nasa.gov", "pld.dpi.wi.gov", "www.fbi.gov"),
                           selected = "pld.dpi.wi.gov"
               )
             ),
             mainPanel(
               plotOutput("ua_plot")
             )
           )
  ),
  
  # Pestaña "Reglas de asociación (Tabla)"
  tabPanel("Reglas de asociación (Tabla)",
    dataTableOutput("rules_table")
  ), 
  
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
  ),
  # Pestaña "Random Forests"
  tabPanel("Random Forests",
    sidebarLayout(
      sidebarPanel(
        radioButtons("tree",
          label = "Selecciona un árbol:",
          choices = list("known_user + midweek + ua_profiles", "country_code + time + ua_profiles",
                         "known_user + midweek + time", "country_code + referring_url + time"),
          selected = "known_user + midweek + ua_profiles"
        )

      ),
      mainPanel(
        plotOutput("tree_plot")
      )
    )          
  )
  
))
