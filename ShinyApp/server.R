# server.R

require(choroplethr)
require(ggplot2)
require(arulesViz)
require(rattle)
require(rpart.plot)
require(RColorBrewer)
load(".//data//map_data.RData")
load(".//data//ua_data.RData")
load(".//data//rules_data.RData")
load(".//data//trees_data.RData")

shinyServer(function(input, output, session) {
  
  # Pestaña "Clics geolocalizados"
  output$map <- renderPlot({
    
    map_data <- switch(input$mapClics, 
      "Total" = total_clics,
      "www.nasa.gov" = nasa_clics,
      "pld.dpi.wi.gov" = pld_clics,
      "www.fbi.gov" = fbi_clics
    )
    
    map_title <- switch(input$mapClics, 
      "Total" = "Clics totales",
      "www.nasa.gov" = "Clics www.nasa.gov",
      "pld.dpi.wi.gov" = "Clics pld.dpi.wi.gov",
      "www.fbi.gov" = "Clics www.fbi.gov"
    )
    
    state_choropleth(map_data, title=paste(map_title, "(01/01/13 a 17/05/13)", sep = " "),
    legend = "Nº total de clics", num_colors = input$mapNiveles)
    
  })
  
  # Pestaña "Perfiles de acceso"
  output$ua_plot <- renderPlot({
    
    ua_data <- switch(input$uaClics, 
                       "Total" = ua_total,
                       "www.nasa.gov" = ua_nasa,
                       "pld.dpi.wi.gov" = ua_pld,
                       "www.fbi.gov" = ua_fbi
    )
    
    ua_title <- switch(input$uaClics, 
                       "Total" = "totales",
                       "www.nasa.gov" = "www.nasa.gov",
                       "pld.dpi.wi.gov" = "pld.dpi.wi.gov",
                       "www.fbi.gov" = "www.fbi.gov"
    )
    
    ggplot(ua_data)+geom_bar(aes(x=long_url,fill=ua_profiles),color="white",position ="fill")+
    facet_grid(midweek~time)+ggtitle(paste("Perfiles de acceso", ua_title, sep = " "))+
    labs(x="",y="Entre semana")
    
  })
  
  # Pestaña "Reglas de asociación (Tabla)"
  output$rules_table <- renderDataTable({
    rules_df
  }, options=list(pageLength=10))
  
  # Pestaña "Reglas de asociación (Graph)"
  
  output$ui <- renderUI({
    if (is.null(input$rules_mode))
      return()
    
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    switch(input$rules_mode,
      "2D" = list(selectInput("rules_xaxis", 
             label = "Variable Eje X:",
             choices = c("support", "confidence", "lift"),
             selected = "support"),
             
             selectInput("rules_yaxis", 
             label = "Variable Eje Y:",
             choices = c("support", "confidence", "lift"),
             selected = "confidence")),
      
      "3D" = selectInput("rules_zaxis", 
             label = "Variable:",
             choices = c("support", "confidence", "lift"),
             selected = "confidence")
    )
  })
  
  output$rules_plot <- renderPlot({
    
    if(input$rules_mode == "2D"){
      plot(rules, method="matrix", measure=c(input$rules_yaxis, input$rules_xaxis))
    }else{
      plot(rules, method="matrix3d", measure = input$rules_zaxis)
    }

  })
  
  output$tree_plot <- renderPlot({
    
    t <- switch(input$tree, 
      "known_user + midweek + ua_profiles" = t1,
      "country_code + time + ua_profiles" = t2,
      "known_user + midweek + time" = t3,
      "country_code + referring_url + time" = t4
    )

    fancyRpartPlot(t, sub="")
    
  })
  
})