#saber19un: Datos limpios UN
#saber19icfes
#saber19icfesun

#### Boxplot
grafica_boxplot <- function(datos, x, y, colorby, highcharter = F, violin = F, leyendav = T){
  
  colnames(datos)[colnames(datos) == x] <- "xG"
  colnames(datos)[colnames(datos) == y] <- "yG"
  colnames(datos)[colnames(datos) == colorby] <- "colorbyG"
  
  datospuntos <- data.frame(datos[c("xG","yG","colorbyG")])
  
  #  
  if(!highcharter)
  {
    if(leyendav){
      ori = "v"
    }
    else{
      ori = "h"
    }
    
    if(!violin){
      bigote <- plot_ly(datos, x = ~xG, y = ~yG, color = ~colorbyG, type = "box")
      bigote <- bigote %>% layout(
        boxmode = "group",
        title = str_c("Resultados ", str_to_lower(y), " por ", str_to_lower(colorby)),
        xaxis = list(
          #type = 'category',
          title = x
        ),
        yaxis = list(
          title = y
        ),
        legend = list(orientation = ori)
      )
      
      bigote
    }
    else{
      violin <- plot_ly(datos, x = ~xG, y = ~yG, color = ~colorbyG, type = "violin", box = list(visible = T))
      violin <- violin %>% layout(
        violinmode = "group",
        title = str_c("Resultados ", str_to_lower(y), " por ", str_to_lower(colorby)),
        xaxis = list(
          #type = 'category',
          title = x
        ),
        yaxis = list(
          title = y
        ),
        legend = list(orientation = ori)
      )
      
      violin
    }
  }
  
  #  
  else
  {
    dat <- data_to_boxplot(datospuntos, yG, xG, colorbyG, add_outliers = F)
    
    highchart() %>%
      hc_xAxis(type = "category") %>%
      hc_add_series_list(dat) %>%
      hc_title(text = str_c("Resultados ", str_to_lower(y), " por ", str_to_lower(x))) %>%
      hc_yAxis(title = list(text = y)) 
  }
  
  
}

grafica_boxplot(saber19un, x = "YEAR_SP", y = "PUNTAJE_GLOBAL", colorby = "SEDE_NOMBRE_MAT")
grafica_boxplot(saber19un, x = "YEAR_SP", y = "PUNTAJE_GLOBAL", colorby = "FACULTAD")
grafica_boxplot(saber19un, x = "ESTRATO_ORIG", y = "PUNTAJE_GLOBAL", colorby = "SEDE_NOMBRE_MAT", leyendav = F )
grafica_boxplot(saber19un, x = "SEDE_NOMBRE_MAT", y = "PUNTAJE_GLOBAL", colorby = "ESTRATO_ORIG", leyendav = F )
grafica_boxplot(saber19un, x = "ESTRATO_ORIG", y = "PUNTAJE_GLOBAL", colorby = "SEDE_NOMBRE_MAT", highcharter = T)

#### Barras
grafica_barras <- function(datos, componente, clase, highcharter = F, apilada = T, vertical = T, leyendav = TRUE){
  
  data2 <- datos
  
  colnames(data2)[colnames(data2) == clase] <- "claseG"
  colnames(data2)[colnames(data2) == componente] <- "componenteG"
  
  if(leyendav){
    ori = "v"
  }
  else{
    ori = "h"
  }
  
  
  datosperfil <- data.frame(data2 %>%
                              select(claseG, componenteG) %>%
                              group_by(claseG, componenteG) %>% 
                              summarize( n = n())) 
  datosperfil2 <- data.frame(data2 %>%
                               select(claseG, componenteG) %>%
                               group_by(claseG) %>%  
                               summarize( n2 = n()))
  
  datosperfil3  <- inner_join(datosperfil, datosperfil2, by = "claseG")
  
  
  if(highcharter){
    
    if(vertical)
    {
      tipo = "column"
    }
    else {
      tipo = "bar"
    }
    
    if(apilada){
      
      hc <- datosperfil3 %>% 
        hchart(tipo, hcaes(x = 'claseG', y = round(100*(n/n2), 2), group = 'componenteG'), stacking = "normal") %>%
        hc_title(text = str_c("Resultados ", str_to_lower(componente), " por ", str_to_lower(clase))) %>%
        hc_yAxis(title = list(text = "% de individuos"),
                 labels = list(format = "{value}%"),
                 max = 100) %>%
        hc_xAxis(title = list(text = clase))
      
      hc
      
    }
    else{
      
      hc <- datosperfil3 %>% 
        hchart(tipo, hcaes(x = 'claseG', y = round(100*(n/n2), 2), group = 'componenteG')) %>%
        hc_title(text = str_c("Resultados ", str_to_lower(componente), " por ", str_to_lower(clase))) %>%
        hc_yAxis(title = list(text = "% de individuos"),
                 labels = list(format = "{value}%")) %>%
        hc_xAxis(title = list(text = clase))
      
      hc
      
    }
    
  }
  #
  else{
    if(apilada){
      
      if(vertical){
        perfil <- plot_ly(data = datosperfil3, x =~ datosperfil3[,1], y = ~round(100*(n/n2), 2), color =~ datosperfil3[,2], type = "bar"
        ) 
        perfil <- perfil %>% layout(title=str_c("Distribución ",str_to_lower(componente), " por ", str_to_lower(clase)), #Titulo
                                    xaxis = list(title = clase ,zeroline = FALSE), #Etiqueta en x
                                    yaxis = list(title = " % de individuos",zeroline = FALSE, ticksuffix = "%"),
                                    barmode = 'stack',
                                    legend = list(orientation = ori))
      }
      else{
        perfil <- plot_ly(data = datosperfil3, y =~ datosperfil3[,1], x = ~round(100*(n/n2), 2), color =~ datosperfil3[,2], type = "bar"
        )
        perfil <- perfil %>% layout(title=str_c("Distribución ",str_to_lower(componente), " por ", str_to_lower(clase)), #Titulo
                                    yaxis = list(title = clase ,zeroline = FALSE), #Etiqueta en x
                                    xaxis = list(title = " % de individuos",zeroline = FALSE, ticksuffix = "%"),
                                    barmode = 'stack',
                                    legend = list(orientation = ori))
      }
      
      perfil
      
    }
    
    else{
      
      if(vertical){
        barra <- plot_ly(data = datosperfil3, x =~ datosperfil3[,1], y = ~round((100*n/n2), 2), color =~ datosperfil3[,2], type = "bar")
        
        barra <- barra %>% layout(title=str_c("Distribución ",str_to_lower(componente), " por ", str_to_lower(clase)), #Titulo
                                  xaxis = list(title = clase ,zeroline = FALSE), #Etiqueta en x
                                  yaxis = list(title = " % de individuos",zeroline = FALSE, ticksuffix = "%"))
        
        barra
      }
      else{
        barra <- plot_ly(data = datosperfil3, y =~ datosperfil3[,1], x = ~round((100*n/n2), 2), color =~ datosperfil3[,2], type = "bar")
        
        barra <- barra %>% layout(title=str_c("Distribución ",str_to_lower(componente), " por ", str_to_lower(clase)), #Titulo
                                  yaxis = list(title = clase ,zeroline = FALSE), #Etiqueta en x
                                  xaxis = list(title = " % de individuos",zeroline = FALSE, ticksuffix = "%"))
        
        barra
      }
      
    }
  }
  
  
}

#datos: dataframe. Sin default.
#componente: Prueba especifica/global. Sin default.
#clase: Variable a segregar. Sin default.
#apilada: True barras apiladas
#         False barras normales. Default = True.
#vertical: True orientación de las gráficas vertical.
#          False orientación de las gráficas horizontal. Default = True.
#leyendav: True orientación de la leyenda vertical.
#          False orientación de la leyenda horizontal. Default = True.

grafica_barras(saber19un, componente = "NIVEL INGLÉS", clase = "ESTRATO_ORIG", vertical = F)
grafica_barras(saber19un, componente = "NIVEL INGLÉS", clase = "SEDE_NOMBRE_MAT", apilada = F, vertical = F)
grafica_barras(saber19un, componente = "NIVEL INGLÉS", clase = "SEDE_NOMBRE_MAT")

grafica_barras(saber19un, componente = "NIVEL INGLÉS", clase = "ESTRATO_ORIG", vertical = F, highcharter = T)
grafica_barras(saber19un, componente = "NIVEL INGLÉS", clase = "SEDE_NOMBRE_MAT", apilada = F, vertical = F, highcharter = T)
grafica_barras(saber19un, componente = "NIVEL INGLÉS", clase = "SEDE_NOMBRE_MAT", highcharter = T)

#### Burbujas
grafica_puntos <- function(datos, x, y, id, colorby, highcharter = F, opacity = 0.5, mini = 10, maxi = 50, leyendav = T){
  
  colnames(datos)[colnames(datos) == x] <- "xG"
  colnames(datos)[colnames(datos) == y] <- "yG"
  colnames(datos)[colnames(datos) == id] <- "idG"
  colnames(datos)[colnames(datos) == colorby] <- "colorbyG"
  
  datospuntos <- data.frame(group_by(datos[c("xG","yG","idG","colorbyG")],
                                     colorbyG, idG) %>% mutate (n = n()) %>%
                              summarise_all(c(mean), na.rm = T))
  
  if(!highcharter){
    
    if(leyendav){
      ori = "v"
    }
    else{
      ori = "h"
    }
    
    
    puntos <- plot_ly(datospuntos, x = ~xG, y = ~yG, text = ~idG, type = 'scatter', mode = 'markers', size = ~n, color = ~colorbyG,
                      marker = list(opacity = opacity, sizemode = 'diameter'), sizes = c(mini, maxi))
    
    puntos <- puntos %>% layout(title=str_c("Por ", str_to_lower(id), " y ", str_to_lower(colorby)), #Titulo
                                yaxis = list(title = y ,zeroline = FALSE), #Etiqueta en x
                                xaxis = list(title = x ,zeroline = FALSE),
                                legend = list(orientation = ori))
    
    puntos
  }
  else{
    hc <- hchart(datospuntos, "scatter", hcaes(x = round(xG, 2), y = round(yG, 2), size = n, group = colorbyG), maxSize = "15%")
    
    hc <- hc %>% hc_title(text = str_c("Por ", str_to_lower(id), " y ", str_to_lower(colorby))) %>%
      hc_yAxis(title = list(text = y)) %>%
      hc_xAxis(title = list(text = x))
    hc
  }
  
}

grafica_puntos(saber19un, x="PBM_ORIG", y="RAZONAMIENTO CUANTITATIVO", id="FACULTAD_S", colorby="SEDE_NOMBRE_MAT")
grafica_puntos(saber19un, x="PBM_ORIG", y="RAZONAMIENTO CUANTITATIVO", id="FACULTAD_S", colorby="SEDE_NOMBRE_MAT", highcharter = T)

grafica_puntos(saber19un, x="PBM_ORIG", y="RAZONAMIENTO CUANTITATIVO", id="PROGRAMA_S", colorby="SEDE_NOMBRE_MAT")
grafica_puntos(saber19un, x="PUNTAJE_GLOBAL", y="RAZONAMIENTO CUANTITATIVO", id="ESTRATO", colorby="SEDE_NOMBRE_MAT")
grafica_puntos(saber19un, x="PUNTAJE_GLOBAL", y="RAZONAMIENTO CUANTITATIVO", id="SEXO", colorby="ESTRATO_ORIG")

#### Radar plotly
grafica_radar <- function(datos, clase, mini = 100, maxi=300, orden = "normal", opacity = 1, llenar = T)
{
  pruebaspunUN <- c("PUNTAJE_GLOBAL",
                    "COMPETENCIAS CIUDADANAS", 
                    "COMUNICACIÓN ESCRITA",    
                    "INGLÉS",                  
                    "LECTURA CRÍTICA",         
                    "RAZONAMIENTO CUANTITATIVO")
  
  if(llenar == T){
    fillG = "tonext"
  }
  else{
    fillG = "none"
  }
  
  colnames(datos)[colnames(datos) == clase] <- "claseG"
  
  datosgrafica <- data.frame(group_by(datos[c("claseG",pruebaspunUN)],
                                      claseG) %>% mutate (n = n()) %>%  #Cambiar aquí
                               summarise_all(c(mean), na.rm = T))
  grafica <- plot_ly(
    type = 'scatterpolar',
    fill = fillG,
    opacity = opacity,
    mode = "lines+markers"
  )
  
  if(orden == "normal"){
    ordenG = c(1:nrow(datosgrafica))
    ordenleyendas = "normal"
  }
  else{
    ordenG = c(nrow(datosgrafica):1)
    ordenleyendas = "reversed"
  }
  
  for(i in ordenG){
    
    grafica <- grafica %>%
      add_trace(
        r = matrix(datosgrafica[i,-c(1, ncol(datosgrafica))]),
        theta = pruebaspunUN,
        name = datosgrafica[i,1],
        opacity = opacity
      )
  }
  
  grafica <- grafica %>%
    layout(
      polar = list(
        radialaxis = list(
          visible = T,
          range = c(mini,maxi)
        )
      ),
      title = list(
        text = "",
        x = 0.98,
        y = 0.8
      ),
      legend = list(title = list(text = str_c("Puntajes por ", str_to_lower(clase)), font = list(size = 15)), traceorder = ordenleyendas)
    )
  
  grafica
}

#Datos: dataframe.  Sin Default.
#Clase: Variable a segregar.  Sin Default.
#mini = valor a iniciar la escala (centro del circulo). Default = 100.
#maxi = valor máximo de la escala (borde del circulo).  Default = 300.
#order = "normal" si organiza las capas por orden alfabetico, Default = "normal"
#        "inverso" si organiza las capas por orden alfabetico inverso.
#opacity = transparencia de las figuras. Default = 1.
#llenar = True si colorea la figura,
#         False si no colorea la figura. 

grafica_radar(saber19un, clase = "SEXO")
grafica_radar(saber19un, clase = "ESTRATO_ORIG", maxi = 250)
grafica_radar(saber19un, clase = "NACIONALIDAD", maxi = 250)
grafica_radar(saber19un, clase = "PBM", maxi = 250)
grafica_radar(saber19un, clase = "SEDE_NOMBRE_MAT", orden = "inverso", opacity = 1, llenar = T, maxi = 200)

#### Radar JSchart
grafica_radarJS <- function(datos, clase, llenar = 0, mini = 100, maxi=300){
  
  pruebaspunUN <- c("PUNTAJE_GLOBAL",
                    "COMPETENCIAS CIUDADANAS", 
                    "COMUNICACIÓN ESCRITA",    
                    "INGLÉS",                  
                    "LECTURA CRÍTICA",         
                    "RAZONAMIENTO CUANTITATIVO")
  
  colnames(datos)[colnames(datos) == clase] <- "claseG"
  
  datosgrafica <- data.frame(group_by(datos[c("claseG",pruebaspunUN)],
                                      claseG) %>% mutate (n = n()) %>%  #Cambiar aquí
                               summarise_all(c(mean), na.rm = T))
  
  
  datosgraficafinal <- as.data.frame(t(datosgrafica[-1]))
  colnames(datosgraficafinal) <- c(levels(datosgrafica$claseG))
  
  ch <- chartJSRadar(scores = round(datosgraficafinal[-c(nrow(datosgraficafinal)),], 2), labs = rownames(datosgraficafinal[-c(nrow(datosgraficafinal)),]),  scaleStartValue = mini, maxScale = maxi, polyAlpha = llenar)
  
  ch
}

grafica_radarJS(saber19un, clase = "SEDE_NOMBRE_MAT", maxi = 250)

#### Radar Highcharter
grafica_radarHC <- function(tipo, llenar, mini = 100, maxi=300){
  
  pruebaspunUN <- c("PUNTAJE_GLOBAL",
                    "COMPETENCIAS CIUDADANAS", 
                    "COMUNICACIÓN ESCRITA",    
                    "INGLÉS",                  
                    "LECTURA CRÍTICA",         
                    "RAZONAMIENTO CUANTITATIVO")
  
  datos <- saber19un
  
  colnames(datos)[colnames(datos) == "SEDE_NOMBRE_MAT"] <- "claseG"
  
  datosgrafica <- data.frame(group_by(datos[c("claseG",pruebaspunUN)],
                                      claseG) %>% mutate (n = n()) %>%  
                               summarise_all(c(mean), na.rm = T))
  
  if(tipo == "polygon"){
    linea = 0
  }else{
    linea = 1
  }
  
  hc <- highchart() %>%
    hc_chart(polar = TRUE) %>% 
    hc_title(text = "Resultados por Sede UN") %>% 
    hc_xAxis(categories = c(colnames(datosgrafica)[-c(1, ncol(datosgrafica))]),
             tickmarkPlacement = "on",
             lineWidth = linea) %>%
    hc_yAxis(gridLineInterpolation = tipo,
             lineWidth = 0,
             min = mini,
             max = maxi) %>%
    hc_series(
      list(
        name = datosgrafica[1,1],
        data = as.numeric(datosgrafica[1,-c(1, ncol(datosgrafica))]),
        pointPlacement = "on",
        type = llenar,
        showInLegend = T
      ),
      list(
        name = datosgrafica[2,1],
        data = as.numeric(datosgrafica[2,-c(1, ncol(datosgrafica))]),
        pointPlacement = "on",
        type = llenar,
        showInLegend = T
      ),
      list(
        name = datosgrafica[3,1],
        data = as.numeric(datosgrafica[3,-c(1, ncol(datosgrafica))]),
        pointPlacement = "on",
        type = llenar,
        showInLegend = T
      ),
      list(
        name = datosgrafica[4,1],
        data = as.numeric(datosgrafica[4,-c(1, ncol(datosgrafica))]),
        pointPlacement = "on",
        type = llenar,
        showInLegend = T
      )
    )
  
  hc
}

grafica_radarHC(tipo= "circle", llenar= "bar")


